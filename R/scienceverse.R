# scienceverse.R — Build a searchable SQLite archive from metacheck exports.
#
# `add_to_scienceverse()` is a sibling to `convert_psychds()`: run it right after
# a paper's Psych-DS collection has been written to disk, and it shreds that
# collection's exports (collection.json, each study-*/dataset_description.json,
# scales/*.osd, logs/*.manifest.json, logs/*.checks.json, plus the checks .rds
# and the manuscript full text) into a set of typed, queryable tables in one
# portable SQLite file. The same call, pointed at any existing collection root,
# backfills the archive — live use and backfill are one code path.
#
# The design is "shred for query, keep a blob for retrieval": everything you
# would filter or search on (per-variable stats, per-finding statistics, files,
# checks, paper text) becomes rows/columns and FTS5 indexes, while the lossless
# R-native checks result is stored as a BLOB for exact round-trip in R.
#
# Findings (the flattened per-text-unit module results in checks.json) are routed
# by module into one of five tables so each table stays tidy for its domain:
#   stat_findings  — stat_check, stat_p_exact, stat_p_nonsig, stat_effect_size, marginal
#   code_findings  — code_check
#   data_findings  — codebook_check, data_validate
#   excel_findings — spreadsheet_check (and legacy excel_check)
#   other_findings — everything else (coi/ethics/funding/open_practices/power/
#                    prereg + any unrecognised module), so no finding is dropped.

# `%||%` and `plural()` are defined elsewhere in the package namespace.

# Default archive location: getOption("metacheck.scienceverse.db") when set
# (e.g. in .Rprofile, so a chosen archive — such as one kept in a synced folder
# instead of the rappdirs cache — does not need to be retyped every session),
# else the canonical scienceverse.sqlite in the same rappdirs data dir the rest
# of the package uses (import-papers, db-*).
.sv_default_db <- function() {
  getOption("metacheck.scienceverse.db") %||%
    file.path(rappdirs::user_data_dir("metacheck", "scienceverse"),
              "scienceverse.sqlite")
}

# Guard the suggested database packages: this feature is optional, so DBI and
# RSQLite live in Suggests and are checked at call time rather than imported.
.sv_require <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("add_to_scienceverse() needs the package",
         plural(length(missing)), " ",
         paste(sprintf("'%s'", missing), collapse = ", "),
         ". Install with install.packages(c(",
         paste(sprintf('"%s"', missing), collapse = ", "), ")).",
         call. = FALSE)
  }
}

# ── module -> findings-table routing ────────────────────────────────────────
.sv_stat_modules  <- c("stat_check", "stat_p_exact", "stat_p_nonsig",
                       "stat_effect_size", "marginal")
.sv_code_modules  <- c("code_check")
.sv_data_modules  <- c("codebook_check", "data_validate")
# The findings TABLE is still called excel_findings: it is a persisted SQLite
# table name, so renaming it would break every existing archive. Only the module
# name changed (excel_check -> spreadsheet_check); the old name is kept so
# archives written before the rename still route to the same table.
.sv_excel_modules <- c("spreadsheet_check", "excel_check")

# Which findings table a given module's rows belong in.
.sv_findings_table <- function(module) {
  if (module %in% .sv_stat_modules)  return("stat_findings")
  if (module %in% .sv_code_modules)  return("code_findings")
  if (module %in% .sv_data_modules)  return("data_findings")
  if (module %in% .sv_excel_modules) return("excel_findings")
  "other_findings"
}

# Columns kept per findings table (besides doi + module). Any finding field not
# listed for its table is left behind in the .rds blob, not silently claimed by
# the wrong domain. Location columns are shared across all findings tables.
.sv_loc_cols <- c("text", "text_id", "paragraph_id", "section_id",
                  "page_number", "section_type", "header")

.sv_findings_cols <- list(
  stat_findings = c(.sv_loc_cols,
    "p_value", "p_comp", "significance", "imprecise", "zero",
    "test", "test_text", "es", "d_reported", "d_reported_text",
    "t_value", "df", "f_reported", "f_reported_text", "df1", "df2",
    "eta_implied_partial", "omega_implied_partial",
    "eta_coherence", "d_coherence"),
  code_findings = c("text", "repo_url", "file_name", "file_url", "file_size",
    "file_type", "repo_name", "language", "checked", "parse_error",
    "parse_error_msg", "code_abs_path", "absolute_paths", "library_lines",
    "library_max_between", "comment_lines", "code_lines", "percentage_comment",
    "loaded_files_missing", "loaded_files_missing_names"),
  data_findings = c("text", "source_file", "column_name", "column", "label",
    "detail", "label_status", "scale", "scale_confidence", "utf8_repaired",
    "demographic", "check"),
  excel_findings = c("File", "Sheet", "Issue", "Detail"),
  other_findings = c(.sv_loc_cols, "formatted",
    "data", "code", "materials", "prereg", "on_request", "ethics", "live_data")
)

# ── small readers/coercers ──────────────────────────────────────────────────

# Read a JSON file, returning NULL on any failure so one malformed file cannot
# abort a whole ingest.
.sv_read_json <- function(path) {
  # .sv_first() returns NULL when no file matches (e.g. a single-study dataset
  # with no logs/, or no statistical_output.json), so guard against a NULL / NA /
  # empty path before file.exists(), which errors on those.
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
      !file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
           error = function(e) NULL)
}

# Pull a scalar out of a parsed-JSON list as character; NA if absent/empty.
.sv_chr <- function(x, ...) {
  for (k in c(...)) {
    if (!is.null(x[[k]])) {
      v <- x[[k]]
      if (length(v) == 0) return(NA_character_)
      # Nested objects/arrays are not scalars; skip to the next key.
      if (is.list(v) && is.null(names(v)) && length(v) > 1) next
      return(as.character(v[[1]]))
    }
  }
  NA_character_
}

.sv_num <- function(x, ...) suppressWarnings(as.numeric(.sv_chr(x, ...)))

# Coerce a single findings field (which may be a scalar, NULL, or a length>1
# list) into one storable cell: scalars pass through; anything vector-like is
# JSON-encoded so the value survives without exploding the schema.
.sv_cell <- function(v) {
  if (is.null(v)) return(NA)
  if (length(v) == 0) return(NA)
  if (length(v) == 1 && !is.list(v)) return(v[[1]])
  # list / multi-element: store as compact JSON text.
  tryCatch(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null"),
           error = function(e) NA)
}

.sv_authors <- function(x) {
  au <- x[["author"]]
  if (is.null(au) || length(au) == 0) return(NA_character_)
  nms <- vapply(au, function(a) {
    if (is.list(a)) as.character(a[["name"]] %||% NA_character_) else as.character(a)
  }, character(1))
  nms <- nms[!is.na(nms) & nzchar(nms)]
  if (!length(nms)) return(NA_character_)
  paste(nms, collapse = ", ")
}

.sv_keywords <- function(x) {
  kw <- x[["keywords"]]
  if (is.null(kw) || length(kw) == 0) return(NA_character_)
  paste(unlist(kw, use.names = FALSE), collapse = "; ")
}

# ── per-collection extraction (parsed -> list of data frames) ────────────────

# Build every table's rows for one collection root. Returns a named list of
# data frames (possibly 0-row) plus the raw bytes of the checks .rds.
.sv_extract_collection <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  coll <- .sv_read_json(file.path(root, "collection.json")) %||% list()

  # The DOI is the collection's own identifier; fall back to the folder name
  # only when the identifier is absent (older exports / hand-built roots).
  doi <- .sv_chr(coll, "identifier")
  if (is.na(doi) || !nzchar(doi)) doi <- basename(root)

  # locate logs/ artefacts (named <doi>.*, but glob to be robust)
  logs_dir  <- file.path(root, "logs")
  manifest  <- .sv_read_json(.sv_first(logs_dir, "\\.manifest\\.json$"))
  checks    <- .sv_read_json(.sv_first(logs_dir, "\\.checks\\.json$"))
  stat_out  <- .sv_read_json(.sv_first(logs_dir, "\\.statistical_output\\.json$"))
  rds_path  <- .sv_first(logs_dir, "\\.rds$")
  rds_blob  <- if (!is.null(rds_path) && file.exists(rds_path))
    list(readBin(rds_path, "raw", n = file.info(rds_path)$size)) else list(NULL)
  # Also recover the checks OBJECT: its per-paper summary_table and the
  # repo_check module table are not in checks.json, only here.
  rds_obj <- if (!is.null(rds_path) && file.exists(rds_path))
    tryCatch(readRDS(rds_path), error = function(e) NULL) else NULL

  # manuscript full text (documentation/*_fulltext.txt or fulltext.txt)
  fulltext <- .sv_read_fulltext(root)

  prov <- manifest[["provenance"]] %||% list()
  llm  <- prov[["llm"]] %||% list()
  soft <- prov[["software"]] %||% list()

  study_dirs <- .sv_study_dirs(root)

  papers <- data.frame(
    doi              = doi,
    title            = .sv_chr(coll, "name"),
    description      = .sv_chr(coll, "description"),
    authors          = .sv_authors(coll),
    keywords         = .sv_keywords(coll),
    date             = .sv_chr(coll, "dateCreated", "metacheck:generated"),
    n_studies        = length(study_dirs),
    metacheck_version = .sv_chr(soft, "version"),
    r_version        = .sv_chr(prov, "r_version"),
    platform         = .sv_chr(prov, "platform"),
    prod_date        = .sv_chr(prov, "prod_date"),
    llm_used         = as.integer(isTRUE(llm[["used"]])),
    llm_model        = .sv_chr(llm, "model"),
    manifest_kind    = .sv_chr(prov, "manifest_kind"),
    n_files          = .sv_num(manifest, "n_files"),
    n_downloaded     = .sv_num(manifest, "n_downloaded"),
    fulltext         = fulltext %||% NA_character_,
    collection_json  = tryCatch(jsonlite::toJSON(coll, auto_unbox = TRUE),
                                error = function(e) NA_character_),
    root_path        = root
  )
  papers$checks_rds <- rds_blob   # list column of raw vectors (BLOB)

  studies     <- .sv_studies(doi, study_dirs)
  variables   <- .sv_variables(doi, study_dirs)
  scales      <- .sv_scales(doi, root)
  scale_items <- .sv_scale_items(doi, root)
  files       <- .sv_files(doi, manifest)
  checks_df   <- .sv_checks(doi, checks)
  findings    <- .sv_findings(doi, checks)
  summaries    <- if (!is.null(rds_obj)) .sv_summary(doi, rds_obj) else .sv_empty("summaries")
  repo_files   <- if (!is.null(rds_obj)) .sv_repo_files(doi, rds_obj) else .sv_empty("repo_files")
  module_tables <- if (!is.null(rds_obj)) .sv_module_tables(doi, rds_obj) else .sv_empty("module_tables")

  statistical_results <- .sv_statistical_results(doi, stat_out)

  c(list(papers = papers, studies = studies, variables = variables,
         scales = scales, scale_items = scale_items, files = files,
         checks = checks_df, summaries = summaries, repo_files = repo_files,
         module_tables = module_tables,
         statistical_results = statistical_results),
    findings)
}

# Flatten a statistical-output document (from reproducibility_check's JASP/
# jamovi/R extraction, written by stat_output_write()) into queryable rows for
# the scienceverse DB: one row per extracted statistic, carrying its ontology
# type. The document is metacheck's native schema (R/stat-output.R's
# stat_output_json()): analyses[] -> results[] -> values{}, where each value is
# keyed by the statistic's own short name and holds value/stato_label/stato_iri.
# The stato_* columns carry whichever vocabulary typed the statistic — a STATO
# class or a metacheck-minted term (see R/stato-map.R) — or NA when neither did.
# Accepts one such document or a list of them.
.sv_statistical_results <- function(doi, stat_out) {
  empty <- data.frame(doi = character(0), analysis = character(0),
                      result = character(0), statistic = character(0),
                      stato_label = character(0), stato_iri = character(0),
                      value = character(0))
  if (is.null(stat_out) || !length(stat_out)) return(empty)
  # Normalise to a list of documents: a single document is recognised by
  # carrying `analyses` directly.
  docs <- if (!is.null(stat_out[["analyses"]])) list(stat_out) else stat_out

  rows <- list()
  for (doc in docs) {
    if (is.null(doc[["analyses"]])) next
    for (an in doc[["analyses"]]) {
      analysis <- .sv_chr(an, "analysis")
      for (res in an[["results"]] %||% list()) {
        # `result` identifies the specific result ROW: prefer its result_id
        # (which also traces back to the code line / table that produced it),
        # falling back to the row label when absent.
        rid <- .sv_chr(res, "result_id")
        rlab <- .sv_chr(res, "row_label")
        result <- if (!is.na(rid) && nzchar(rid)) rid else rlab
        values <- res[["values"]] %||% list()
        for (stat_name in names(values)) {
          v <- values[[stat_name]]
          rows[[length(rows) + 1L]] <- data.frame(
            doi = doi,
            analysis = analysis,
            result = result,
            statistic = stat_name,
            stato_label = .sv_chr(v, "stato_label"),
            stato_iri = .sv_chr(v, "stato_iri"),
            # value is stored as TEXT: it holds numbers AND reported strings
            # ("< .001", "Inf"), so a single character column is the only type
            # that fits every cell.
            value = as.character(.sv_cell(v[["value"]])))
        }
      }
    }
  }
  if (!length(rows)) empty else dplyr::bind_rows(rows)
}

# First file in `dir` matching `pattern`, or NULL.
.sv_first <- function(dir, pattern) {
  if (!dir.exists(dir)) return(NULL)
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(f)) f[[1]] else NULL
}

.sv_read_fulltext <- function(root) {
  doc <- file.path(root, "documentation")
  if (!dir.exists(doc)) return(NULL)
  f <- list.files(doc, pattern = "fulltext\\.txt$", full.names = TRUE)
  if (!length(f)) return(NULL)
  txt <- tryCatch(readLines(f[[1]], warn = FALSE, encoding = "UTF-8"),
                  error = function(e) character(0))
  if (!length(txt)) return(NULL)
  paste(txt, collapse = "\n")
}

.sv_study_dirs <- function(root) {
  d <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  d <- d[grepl("(^|/)study-", d)]
  d[file.exists(file.path(d, "dataset_description.json"))]
}

.sv_studies <- function(doi, study_dirs) {
  if (!length(study_dirs)) return(.sv_empty("studies"))
  rows <- lapply(study_dirs, function(sd) {
    dd <- .sv_read_json(file.path(sd, "dataset_description.json"))
    if (is.null(dd)) return(NULL)
    vm <- dd[["variableMeasured"]]
    data.frame(
      doi            = doi,
      study_group    = sub("^study-", "", basename(sd)),
      study_dir      = basename(sd),
      title          = .sv_chr(dd, "name"),
      description    = .sv_chr(dd, "description"),
      schema_version = .sv_chr(dd, "schemaVersion"),
      n_variables    = if (is.null(vm)) 0L else length(vm)
    )
  })
  dplyr::bind_rows(rows)
}

.sv_variables <- function(doi, study_dirs) {
  rows <- list()
  for (sd in study_dirs) {
    dd <- .sv_read_json(file.path(sd, "dataset_description.json"))
    if (is.null(dd)) next
    grp <- sub("^study-", "", basename(sd))
    vm  <- dd[["variableMeasured"]]
    if (is.null(vm)) next
    for (v in vm) {
      st    <- v[["metacheck:statistics"]] %||% list()
      scale <- v[["metacheck:scale"]] %||% list()
      rows[[length(rows) + 1L]] <- data.frame(
        doi            = doi,
        study_group    = grp,
        name           = .sv_chr(v, "name"),
        label          = .sv_chr(v, "description"),
        concept        = .sv_chr(v, "metacheck:concept"),
        level          = .sv_chr(v, "metacheck:measurementLevel"),
        role           = .sv_chr(v, "metacheck:role"),
        representation = .sv_chr(v, "metacheck:representation"),
        source_file    = .sv_chr(v, "metacheck:sourceFile"),
        scale          = .sv_chr(scale, "name"),
        scale_code     = .sv_chr(scale, "code"),
        technique      = .sv_chr(v, "measurementTechnique"),
        value_pattern  = .sv_chr(v, "valuePattern"),
        n         = .sv_num(st, "n"),
        n_missing = .sv_num(st, "n_missing"),
        mean      = .sv_num(st, "mean"),
        sd        = .sv_num(st, "sd"),
        se        = .sv_num(st, "se"),
        median    = .sv_num(st, "median"),
        p25       = .sv_num(st, "p25"),
        p75       = .sv_num(st, "p75"),
        iqr       = .sv_num(st, "iqr"),
        skewness  = .sv_num(st, "skewness"),
        kurtosis  = .sv_num(st, "kurtosis"),
        min_value = .sv_num(v, "minValue"),
        max_value = .sv_num(v, "maxValue")
      )
    }
  }
  if (!length(rows)) return(.sv_empty("variables"))
  dplyr::bind_rows(rows)
}

.sv_scales <- function(doi, root) {
  osd_dir <- file.path(root, "scales")
  if (!dir.exists(osd_dir)) return(.sv_empty("scales"))
  rows <- lapply(list.files(osd_dir, pattern = "\\.osd$", full.names = TRUE),
    function(f) {
      osd <- .sv_read_json(f)
      if (is.null(osd)) return(NULL)
      def   <- osd[["definition"]] %||% list()
      info  <- def[["scale_info"]] %||% list()
      mc    <- def[["metacheck"]] %||% list()
      items <- def[["items"]]
      lik   <- def[["likert_options"]] %||% list()
      data.frame(
        doi           = doi,
        scale         = .sv_chr(info, "name"),
        code          = .sv_chr(info, "code"),
        abbreviation  = .sv_chr(info, "abbreviation"),
        n_items       = if (is.null(items)) NA_integer_ else length(items),
        likert_points = suppressWarnings(as.integer(.sv_chr(lik, "points"))),
        source        = .sv_chr(mc, "scale_source"),
        confidence    = .sv_chr(mc, "confidence"),
        osd_file      = basename(f)
      )
    })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(.sv_empty("scales"))
  dplyr::bind_rows(rows)
}

# One row per item of each scale: the item id, its type, and its question text.
# The wording lives in the OSD's top-level `translations$en` map (id -> text);
# fall back to the id when no translation exists (e.g. demographic blocks whose
# "items" are bare column codes).
.sv_scale_items <- function(doi, root) {
  osd_dir <- file.path(root, "scales")
  if (!dir.exists(osd_dir)) return(.sv_empty("scale_items"))
  rows <- lapply(list.files(osd_dir, pattern = "\\.osd$", full.names = TRUE),
    function(f) {
      osd <- .sv_read_json(f)
      if (is.null(osd)) return(NULL)
      def   <- osd[["definition"]] %||% list()
      info  <- def[["scale_info"]] %||% list()
      items <- def[["items"]]
      if (is.null(items) || !length(items)) return(NULL)
      en    <- (osd[["translations"]] %||% list())[["en"]] %||% list()
      code  <- .sv_chr(info, "code")
      scale <- .sv_chr(info, "name")
      do.call(rbind, lapply(seq_along(items), function(i) {
        it  <- items[[i]]
        id  <- .sv_chr(it, "id")
        txt <- if (!is.null(en[[id]])) as.character(en[[id]][[1]]) else NA_character_
        data.frame(
          doi        = doi,
          scale_code = code,
          scale      = scale,
          position   = i,
          item_id    = id,
          item_type  = .sv_chr(it, "type"),
          text       = txt %||% NA_character_
        )
      }))
    })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(.sv_empty("scale_items"))
  dplyr::bind_rows(rows)
}

.sv_files <- function(doi, manifest) {
  fl <- manifest[["files"]]
  if (is.null(fl) || !length(fl)) return(.sv_empty("files"))
  rows <- lapply(fl, function(f) data.frame(
    doi         = doi,
    file_name   = .sv_chr(f, "file_name"),
    file_path   = .sv_chr(f, "file_path"),
    repo_url    = .sv_chr(f, "repo_url"),
    file_url    = .sv_chr(f, "file_url"),
    file_size   = .sv_num(f, "file_size"),
    data_type   = .sv_chr(f, "data_type"),
    data_format = .sv_chr(f, "data_format"),
    downloaded  = as.integer(isTRUE(f[["downloaded"]])),
    status      = .sv_chr(f, "status")
  ))
  dplyr::bind_rows(rows)
}

.sv_checks <- function(doi, checks) {
  ch <- checks[["checks"]]
  if (is.null(ch) || !length(ch)) return(.sv_empty("checks"))
  rows <- lapply(ch, function(c1) {
    counts <- c1[["counts"]]
    counts_json <- if (is.null(counts)) NA_character_
      else if (is.character(counts) && length(counts) == 1) counts   # already JSON text
      else tryCatch(jsonlite::toJSON(counts, auto_unbox = TRUE, null = "null"),
                    error = function(e) NA_character_)
    data.frame(
      doi           = doi,
      module        = .sv_chr(c1, "module"),
      traffic_light = .sv_chr(c1, "traffic_light"),
      summary_text  = .sv_chr(c1, "summary_text"),
      counts_json   = as.character(counts_json)
    )
  })
  dplyr::bind_rows(rows)
}

# The per-paper summary_table (one wide row of counts across every module) lives
# only in the checks object, not in checks.json. Store it as one row keyed by
# doi; its columns vary a little by paper, so bind_rows fills gaps with NA.
.sv_summary <- function(doi, rds_obj) {
  st <- rds_obj[["summary_table"]]
  if (is.null(st) || !is.data.frame(st) || !nrow(st)) return(.sv_empty("summaries"))
  st <- st[1, !names(st) %in% c("paper_id", "doi"), drop = FALSE]
  # Store as JSON, not wide columns: the count columns differ from paper to
  # paper, and per-paper appends into one wide table would clash on schema. A
  # JSON map (name -> value) is schema-stable and displays as a tidy 2-col table.
  json <- tryCatch(jsonlite::toJSON(as.list(st), auto_unbox = TRUE, null = "null"),
                   error = function(e) NA_character_)
  data.frame(doi = doi, summary_json = as.character(json))
}

# repo_check's table is the only module table absent from checks.json (the module
# emits no findings). Capture it as a small table so that check has content too.
.sv_repo_files <- function(doi, rds_obj) {
  rc <- rds_obj[["modules"]][["repo_check"]][["table"]]
  if (is.null(rc) || !is.data.frame(rc) || !nrow(rc)) return(.sv_empty("repo_files"))
  keep <- intersect(c("repo_url", "file_name", "file_path", "file_url",
                      "file_location", "file_size", "file_type", "repo_name"),
                    names(rc))
  out <- rc[, keep, drop = FALSE]
  cbind(data.frame(doi = doi), out)
}

# Store every module's COMPLETE result table, one row per module, as JSON. This
# is the faithful "table the report shows for this module" — it keeps the rich
# per-module classification columns (e.g. power's sample_size/alpha/power/
# effect_size/complete, prereg registration details, full variable stats) that
# the fixed-schema findings tables deliberately drop. Columns that are list-cols
# are flattened to a character summary so toJSON stays simple and stable.
.sv_module_tables <- function(doi, rds_obj) {
  mods <- rds_obj[["modules"]]
  if (is.null(mods) || !length(mods)) return(.sv_empty("module_tables"))
  rows <- list()
  for (nm in names(mods)) {
    tb <- mods[[nm]][["table"]]
    if (is.null(tb) || !is.data.frame(tb) || !nrow(tb)) next
    # Flatten any column that is not a plain atomic vector — list-columns AND S3
    # objects such as ellmer's `ellmer_output` (the raw LLM answer) — to
    # character, so jsonlite::toJSON always has a method for every column.
    flat <- as.data.frame(lapply(tb, function(col) {
      if (is.atomic(col) && !is.object(col)) return(col)
      if (is.list(col))
        return(vapply(col, function(x)
          paste(as.character(unlist(x)), collapse = "; "), character(1)))
      # S3/other: coerce elementwise to character (length-preserving).
      vapply(seq_along(col), function(i)
        paste(as.character(col[[i]]), collapse = "; "), character(1))
    }), stringsAsFactors = FALSE, check.names = FALSE)
    json <- tryCatch(
      jsonlite::toJSON(flat, dataframe = "rows", na = "null"),
      error = function(e) NA_character_)
    rows[[length(rows) + 1L]] <- data.frame(
      doi = doi, module = nm, n_rows = nrow(tb),
      table_json = as.character(json), stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(.sv_empty("module_tables"))
  dplyr::bind_rows(rows)
}

# Route each finding to its table, keeping only that table's columns.
.sv_findings <- function(doi, checks) {
  out <- list(stat_findings = list(), code_findings = list(),
              data_findings = list(), excel_findings = list(),
              other_findings = list())
  fd <- checks[["findings"]]
  if (!is.null(fd) && length(fd)) {
    for (f in fd) {
      module <- .sv_chr(f, "module")
      tbl    <- .sv_findings_table(module)
      cols   <- .sv_findings_cols[[tbl]]
      row    <- c(list(doi = doi, module = module),
                  lapply(stats::setNames(cols, cols),
                         function(k) .sv_cell(f[[k]])))
      out[[tbl]][[length(out[[tbl]]) + 1L]] <- as.data.frame(row,
                                                             stringsAsFactors = FALSE)
    }
  }
  lapply(names(out), function(nm) {
    if (length(out[[nm]])) dplyr::bind_rows(out[[nm]])
    else .sv_empty(nm)
  }) |> stats::setNames(names(out))
}

# Empty-frame templates keyed by table name, so a table always has its columns
# even when a collection contributes no rows of that kind.
.sv_empty <- function(which) {
  switch(which,
    studies = data.frame(doi = character(), study_group = character(),
      study_dir = character(), title = character(), description = character(),
      schema_version = character(), n_variables = integer()),
    variables = data.frame(doi = character(), study_group = character(),
      name = character(), label = character(), concept = character(),
      level = character(), role = character(), representation = character(),
      source_file = character(), scale = character(), scale_code = character(),
      technique = character(), value_pattern = character(), n = numeric(),
      n_missing = numeric(), mean = numeric(), sd = numeric(), se = numeric(),
      median = numeric(), p25 = numeric(), p75 = numeric(), iqr = numeric(),
      skewness = numeric(), kurtosis = numeric(), min_value = numeric(),
      max_value = numeric()),
    scales = data.frame(doi = character(), scale = character(),
      code = character(), abbreviation = character(), n_items = integer(),
      likert_points = integer(), source = character(), confidence = character(),
      osd_file = character()),
    scale_items = data.frame(doi = character(), scale_code = character(),
      scale = character(), position = integer(), item_id = character(),
      item_type = character(), text = character()),
    files = data.frame(doi = character(), file_name = character(),
      file_path = character(), repo_url = character(), file_url = character(),
      file_size = numeric(), data_type = character(), data_format = character(),
      downloaded = integer(), status = character()),
    checks = data.frame(doi = character(), module = character(),
      traffic_light = character(), summary_text = character(),
      counts_json = character()),
    summaries = data.frame(doi = character(), summary_json = character()),
    module_tables = data.frame(doi = character(), module = character(),
      n_rows = integer(), table_json = character()),
    repo_files = data.frame(doi = character(), repo_url = character(),
      file_name = character(), file_path = character(), file_url = character(),
      file_location = character(), file_size = numeric(),
      file_type = character(), repo_name = character()),
    # findings tables: doi + module + that table's cols, all character/NA
    {
      cols <- .sv_findings_cols[[which]]
      df <- data.frame(doi = character(), module = character())
      for (cn in cols) df[[cn]] <- logical(0)  # typed on first insert
      df
    })
}

# ── SQLite writing ──────────────────────────────────────────────────────────

# Delete every row for `doi` across all tables, so a re-add is idempotent.
.sv_delete_doi <- function(con, doi) {
  tbls <- c("papers", "studies", "variables", "scales", "scale_items",
            "files", "checks", "summaries", "repo_files", "module_tables",
            "stat_findings", "code_findings", "data_findings",
            "excel_findings", "other_findings")
  for (t in tbls) {
    if (DBI::dbExistsTable(con, t)) {
      DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE doi = ?", t),
                     params = list(doi))
    }
  }
}

# Append a data frame to a table, creating it on first write. `field.types`
# lets us force the BLOB column type for the checks_rds list-of-raw column.
.sv_write <- function(con, name, df) {
  # Force the checks_rds column to SQLite BLOB. field.types can only be given at
  # CREATE time (not with append), so create the table explicitly the first time.
  ft <- if ("checks_rds" %in% names(df)) c(checks_rds = "BLOB") else NULL
  if (!DBI::dbExistsTable(con, name)) {
    if (ncol(df) == 0) return(invisible())
    DBI::dbCreateTable(con, name, df, field.types = ft)
  }
  if (is.null(df) || nrow(df) == 0) return(invisible())
  DBI::dbWriteTable(con, name, df, append = TRUE)
  invisible()
}

# Build the FTS5 virtual tables + triggers-free content mirror. We use
# "external content"-free FTS (a standalone FTS table we repopulate), which is
# simplest and robust for a rebuild-on-ingest archive.
.sv_build_fts <- function(con) {
  defs <- list(
    papers_fts    = c(tbl = "papers",    cols = "doi, title, authors, keywords, description, fulltext"),
    findings_fts  = NULL,  # populated from the union below
    variables_fts = c(tbl = "variables", cols = "doi, name, label, concept, scale"),
    scales_fts    = c(tbl = "scales",    cols = "doi, scale, abbreviation, code")
  )

  # papers_fts
  DBI::dbExecute(con, "DROP TABLE IF EXISTS papers_fts")
  DBI::dbExecute(con, "CREATE VIRTUAL TABLE papers_fts USING fts5(doi, title, authors, keywords, description, fulltext)")
  DBI::dbExecute(con, "INSERT INTO papers_fts SELECT doi, title, authors, keywords, description, fulltext FROM papers")

  # variables_fts
  DBI::dbExecute(con, "DROP TABLE IF EXISTS variables_fts")
  DBI::dbExecute(con, "CREATE VIRTUAL TABLE variables_fts USING fts5(doi, name, label, concept, scale)")
  DBI::dbExecute(con, "INSERT INTO variables_fts SELECT doi, name, label, concept, scale FROM variables")

  # scales_fts
  DBI::dbExecute(con, "DROP TABLE IF EXISTS scales_fts")
  DBI::dbExecute(con, "CREATE VIRTUAL TABLE scales_fts USING fts5(doi, scale, abbreviation, code)")
  DBI::dbExecute(con, "INSERT INTO scales_fts SELECT doi, scale, abbreviation, code FROM scales")

  # findings_fts: union the `text` column of every findings table that has one.
  DBI::dbExecute(con, "DROP TABLE IF EXISTS findings_fts")
  DBI::dbExecute(con, "CREATE VIRTUAL TABLE findings_fts USING fts5(doi, module, text)")
  for (t in c("stat_findings", "code_findings", "data_findings", "other_findings")) {
    if (DBI::dbExistsTable(con, t) &&
        "text" %in% DBI::dbListFields(con, t)) {
      DBI::dbExecute(con, sprintf(
        "INSERT INTO findings_fts SELECT doi, module, text FROM %s WHERE text IS NOT NULL", t))
    }
  }
  invisible()
}

#' Recover the checks result stored in a scienceverse archive
#'
#' The `papers.checks_rds` column holds each paper's checks result as the exact
#' bytes of its `logs/<doi>.rds` file (gzip-compressed, as [saveRDS()] writes).
#' This helper pulls that BLOB for one paper and reconstructs the original R
#' object, so callers do not have to know it is compressed.
#'
#' @param con a DBI connection to a scienceverse SQLite archive.
#' @param doi the paper identifier (the `doi` column / collection folder name).
#'
#' @returns the checks result (a list of module outputs), or `NULL` if the
#'   paper has no stored blob.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSQLite::SQLite(), "scienceverse.sqlite")
#' res <- scienceverse_checks(con, "collabra.165")
#' res$modules$stat_check$table
#' }
scienceverse_checks <- function(con, doi) {
  .sv_require(c("DBI", "RSQLite"))
  raw <- DBI::dbGetQuery(con,
    "SELECT checks_rds FROM papers WHERE doi = ?", params = list(doi))$checks_rds
  if (!length(raw) || is.null(raw[[1]]) || !length(raw[[1]])) return(NULL)
  bytes <- raw[[1]]
  # Stored blob is the .rds file verbatim (gzip). Decompress defensively: if it
  # is ever stored uncompressed, unserialize the bytes as-is.
  obj <- tryCatch(unserialize(memDecompress(bytes, type = "gzip")),
                  error = function(e) tryCatch(unserialize(bytes),
                                               error = function(e2) NULL))
  obj
}

#' Add a metacheck collection to a scienceverse SQLite archive
#'
#' Shred a metacheck Psych-DS collection (as written by [convert_psychds()])
#' into a searchable SQLite database. Run it right after `convert_psychds()` on
#' the collection root it produced, or over any existing collection root to
#' backfill the archive. Adding the same paper again replaces its rows, so the
#' operation is idempotent.
#'
#' The archive stores structural tables (`papers`, `studies`, `variables`,
#' `scales`, `files`, `checks`), five findings tables split by module domain
#' (`stat_findings`, `code_findings`, `data_findings`, `excel_findings`,
#' `other_findings`), the lossless checks result as a BLOB in `papers.checks_rds`,
#' the manuscript full text in `papers.fulltext`, and FTS5 full-text indexes
#' (`papers_fts`, `findings_fts`, `variables_fts`, `scales_fts`).
#'
#' Requires the suggested packages \pkg{DBI} and \pkg{RSQLite}.
#'
#' @param collection_root path to a metacheck output root — a folder containing
#'   `collection.json` (a multi-study collection) or `dataset_description.json`
#'   (a single-study Psych-DS dataset), as written by [convert_psychds()]. A
#'   character vector adds several in one call.
#' @param db_path path to the SQLite file to create or append to. Created (with
#'   parent directories) if it does not exist. Defaults to
#'   `getOption("metacheck.scienceverse.db")` when set, else a canonical
#'   `scienceverse.sqlite` in the package's rappdirs data directory
#'   (`rappdirs::user_data_dir("metacheck", "scienceverse")`), so repeated calls
#'   accumulate into one shared archive.
#' @param rebuild_fts whether to rebuild the FTS5 indexes after writing. `TRUE`
#'   (default) keeps search current; set `FALSE` when adding many roots in a
#'   loop and call the function once more (or `add_to_scienceverse()` on the last
#'   root) to rebuild at the end.
#' @param quiet suppress progress messages.
#'
#' @returns invisibly, `db_path`.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- convert_psychds(paper, output_dir = "psychds/mypaper")
#' add_to_scienceverse(res$output_dir, "scienceverse.sqlite")
#'
#' # backfill an existing corpus
#' roots <- list.dirs("collabra", recursive = FALSE)
#' add_to_scienceverse(roots, "scienceverse.sqlite")
#'
#' # query it
#' con <- DBI::dbConnect(RSQLite::SQLite(), "scienceverse.sqlite")
#' DBI::dbGetQuery(con, "
#'   SELECT DISTINCT p.doi, p.title
#'   FROM papers p
#'   JOIN papers_fts f ON f.doi = p.doi
#'   JOIN stat_findings s ON s.doi = p.doi
#'   WHERE papers_fts MATCH 'stress' AND s.f_reported > 5")
#' DBI::dbDisconnect(con)
#' }
add_to_scienceverse <- function(collection_root, db_path = .sv_default_db(),
                                rebuild_fts = TRUE, quiet = FALSE) {
  .sv_require(c("DBI", "RSQLite"))
  if (is.null(db_path) || !nzchar(db_path))
    stop("`db_path` must be a non-empty path to the SQLite archive to write.")
  if (!quiet && identical(db_path, .sv_default_db()))
    message("Writing to the default scienceverse archive: ", db_path)

  roots <- collection_root
  # accept a single root or a vector; keep only real metacheck output roots. A
  # MULTI-study paper has a `collection.json`; a SINGLE-study paper is a flat
  # Psych-DS dataset with only `dataset_description.json` (convert_psychds writes
  # no collection.json for it, by design). A paper with NO shareable data files
  # gets a METADATA-ONLY root instead (manuscript text + logs only, see
  # convert_psychds()'s "No data files to convert" branch in R/psychds-convert.R)
  # — it carries neither of the above, only its own `ro-crate-metadata.json`.
  # Accept ALL THREE — .sv_extract_collection() already tolerates a missing
  # collection.json (falls back to the folder name as the DOI) and an empty
  # study_dirs (every studies/variables table already handles zero rows), so a
  # metadata-only root ingests fine; it just contributes empty studies/
  # variables/scales tables alongside its (real) checks, manifest and fulltext.
  is_root <- vapply(roots, function(r)
    dir.exists(r) && (file.exists(file.path(r, "collection.json")) ||
                        file.exists(file.path(r, "dataset_description.json")) ||
                        file.exists(file.path(r, "ro-crate-metadata.json"))),
    logical(1))
  if (!any(is_root)) {
    stop("No metacheck output roots found (a folder with a collection.json, ",
         "dataset_description.json, or ro-crate-metadata.json): ",
         paste(roots, collapse = ", "))
  }
  if (!all(is_root) && !quiet) {
    message("Skipping ", sum(!is_root), " path",
            plural(sum(!is_root)),
            " without a collection.json / dataset_description.json.")
  }
  roots <- roots[is_root]

  dir.create(dirname(normalizePath(db_path, winslash = "/", mustWork = FALSE)),
             recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (root in roots) {
    ex <- tryCatch(.sv_extract_collection(root), error = function(e) {
      if (!quiet) message("  skip ", basename(root), ": ", conditionMessage(e))
      NULL
    })
    if (is.null(ex)) next

    doi <- ex$papers$doi[[1]]
    DBI::dbWithTransaction(con, {
      .sv_delete_doi(con, doi)
      for (nm in names(ex)) .sv_write(con, nm, ex[[nm]])
    })
    if (!quiet) message("  added ", doi,
                        " (", nrow(ex$variables), " variables, ",
                        sum(vapply(ex[grep("_findings$", names(ex))], nrow, 0L)),
                        " findings)")
  }

  if (rebuild_fts) .sv_build_fts(con)
  invisible(db_path)
}

# ── Query layer (used by scienceverse_app(); testable without Shiny) ──────────
#
# These build parameterised SQL from a small set of inputs. User-supplied values
# always travel as bound `params` (never pasted into SQL); only column and table
# names — validated against a fixed allow-list — are interpolated. This keeps
# the browser safe against injection while letting the app compose a text search
# (Google-style field:value) with numeric filter controls.

# Escape a user term for an FTS5 MATCH: wrap in double quotes, doubling any
# internal quote, so punctuation in the term cannot break the query.
.sv_fts_quote <- function(term) {
  paste0('"', gsub('"', '""', term), '"')
}

# Run a SELECT, passing bound params only when there are any: RSQLite errors on
# `params = list()` for a query that has no placeholders.
.sv_get <- function(con, sql, params) {
  if (length(params)) DBI::dbGetQuery(con, sql, params = params)
  else DBI::dbGetQuery(con, sql)
}

#' Open a connection to a scienceverse archive
#'
#' Convenience wrapper that checks for the optional database packages and opens
#' a DBI connection to the SQLite archive. Call [DBI::dbDisconnect()] when done.
#'
#' @param db_path path to the SQLite archive. Defaults to the same canonical
#'   location [add_to_scienceverse()] writes to.
#'
#' @returns a DBI connection.
#' @export
scienceverse_connect <- function(db_path = .sv_default_db()) {
  .sv_require(c("DBI", "RSQLite"))
  if (!file.exists(db_path)) {
    stop("No scienceverse archive at: ", db_path,
         "\nBuild one first with add_to_scienceverse().")
  }
  DBI::dbConnect(RSQLite::SQLite(), db_path)
}

# Corpus-level counts for the app's stat tiles. Returns a named integer vector.
.sv_counts <- function(con) {
  one <- function(sql) tryCatch(DBI::dbGetQuery(con, sql)$n, error = function(e) 0L)
  ftabs <- c("stat_findings", "code_findings", "data_findings",
             "excel_findings", "other_findings")
  c(papers    = one("SELECT COUNT(*) n FROM papers"),
    studies   = one("SELECT COUNT(*) n FROM studies"),
    variables = one("SELECT COUNT(*) n FROM variables"),
    scales    = one("SELECT COUNT(DISTINCT code) n FROM scales"),
    findings  = sum(vapply(ftabs, function(t)
                  one(sprintf("SELECT COUNT(*) n FROM %s", t)), integer(1))),
    files     = one("SELECT COUNT(*) n FROM files"))
}

# Append LIKE-per-term conditions for a free-text string across one or more
# columns, returning updated (where, params). Each whitespace-separated term is
# AND-combined; a term matches if it appears in ANY of `cols`.
.sv_like_terms <- function(text, cols, where, params) {
  terms <- strsplit(trimws(text), "\\s+")[[1]]
  for (term in terms) {
    if (!nzchar(term)) next
    clause <- paste(sprintf("%s LIKE ?", cols), collapse = " OR ")
    where <- c(where, sprintf("(%s)", clause))
    params <- c(params, rep(list(paste0("%", term, "%")), length(cols)))
  }
  list(where = where, params = params)
}

#' Search papers in a scienceverse archive
#'
#' Bare terms search the manuscript full text and metadata via FTS5;
#' `field:value` terms (Google-style) restrict to a metadata column.
#'
#' @param con a scienceverse connection (see [scienceverse_connect()]).
#' @param query a search string. Bare terms search title/authors/keywords/
#'   fulltext; `field:value` restricts (fields: title, authors, keywords, doi).
#' @param limit maximum rows to return.
#'
#' @returns a data frame of matching papers with per-paper counts.
#' @export
scienceverse_papers <- function(con, query = "", limit = 500) {
  .sv_require("DBI")
  query <- if (is.null(query)) "" else trimws(query)
  base <- paste(
    "SELECT p.doi, p.title, p.authors, p.keywords, p.date, p.n_studies,",
    "(SELECT COUNT(*) FROM variables v WHERE v.doi = p.doi) n_variables,",
    "(SELECT COUNT(DISTINCT s.code) FROM scales s WHERE s.doi = p.doi) n_scales",
    "FROM papers p")
  where <- character(0); params <- list()

  if (nzchar(query)) {
    parsed <- trove_parse_query(query,
                fields = c("title", "authors", "keywords", "doi"))
    for (fld in names(parsed$field_terms))
      for (term in parsed$field_terms[[fld]]) {
        where <- c(where, sprintf("p.%s LIKE ?", fld))
        params <- c(params, list(paste0("%", term, "%")))
      }
    if (length(parsed$free_terms)) {
      match <- paste(vapply(parsed$free_terms, .sv_fts_quote, character(1)),
                     collapse = " AND ")
      where <- c(where,
        "p.doi IN (SELECT doi FROM papers_fts WHERE papers_fts MATCH ?)")
      params <- c(params, list(match))
    }
  }

  sql <- base
  if (length(where)) sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  sql <- paste(sql, "ORDER BY p.date DESC LIMIT", as.integer(limit))
  .sv_get(con, sql, params)
}

# The findings tables and, for each, the numeric columns a user may range-filter.
.sv_findings_numeric <- list(
  stat_findings  = c("f_reported", "t_value", "p_value", "df1", "df2", "es"),
  code_findings  = c("code_lines", "comment_lines", "percentage_comment",
                     "file_size"),
  data_findings  = character(0),
  excel_findings = character(0),
  other_findings = character(0)
)

#' Search findings in a scienceverse archive
#'
#' Query one findings table, optionally with a free-text match on the finding
#' text and numeric range filters on that table's numeric columns. This is the
#' engine behind the app's Findings tab (e.g. stat findings with `f_reported`
#' greater than 5).
#'
#' @param con a scienceverse connection.
#' @param table one of `stat_findings`, `code_findings`, `data_findings`,
#'   `excel_findings`, `other_findings`.
#' @param text free-text matched against the finding `text` (per-term, AND).
#' @param ranges a named list of `c(min, max)` (either may be `NA`) for numeric
#'   columns of `table`; unknown columns are ignored.
#' @param doi optional DOI to restrict to one paper.
#' @param limit maximum rows to return.
#'
#' @returns a data frame of matching findings.
#' @export
scienceverse_findings <- function(con, table = "stat_findings", text = "",
                                  ranges = list(), doi = NULL, limit = 1000) {
  .sv_require("DBI")
  valid <- names(.sv_findings_numeric)
  if (!table %in% valid) {
    stop("`table` must be one of: ", paste(valid, collapse = ", "))
  }
  where <- character(0); params <- list()

  text <- if (is.null(text)) "" else trimws(text)
  if (nzchar(text) && "text" %in% DBI::dbListFields(con, table)) {
    lt <- .sv_like_terms(text, "text", where, params)
    where <- lt$where; params <- lt$params
  }

  numeric_cols <- .sv_findings_numeric[[table]]
  for (col in names(ranges)) {
    if (!col %in% numeric_cols) next
    rng <- ranges[[col]]
    lo <- suppressWarnings(as.numeric(rng[[1]]))
    hi <- suppressWarnings(as.numeric(rng[[2]]))
    if (!is.na(lo)) {
      where <- c(where, sprintf("%s >= ?", col)); params <- c(params, list(lo))
    }
    if (!is.na(hi)) {
      where <- c(where, sprintf("%s <= ?", col)); params <- c(params, list(hi))
    }
  }

  if (!is.null(doi) && nzchar(doi)) {
    where <- c(where, "doi = ?"); params <- c(params, list(doi))
  }

  sql <- sprintf("SELECT * FROM %s", table)
  if (length(where)) sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  sql <- paste(sql, "LIMIT", as.integer(limit))
  .sv_get(con, sql, params)
}

#' Search scales in a scienceverse archive
#'
#' @param con a scienceverse connection.
#' @param query bare terms search scale/abbreviation/code/source; `field:value`
#'   restricts (fields: scale, code, source, confidence).
#' @param limit maximum rows.
#' @returns a data frame of scales with a per-scale paper count.
#' @export
scienceverse_scales <- function(con, query = "", limit = 1000) {
  .sv_require("DBI")
  query <- if (is.null(query)) "" else trimws(query)
  # Exclude unnamed scales: rows with no scale name, or detected item blocks
  # that never received a name (source 'unnamed_block').
  base <- paste(
    "SELECT scale, code, COUNT(DISTINCT doi) n_papers, MAX(n_items) n_items,",
    "GROUP_CONCAT(DISTINCT source) source,",
    "GROUP_CONCAT(DISTINCT confidence) confidence",
    "FROM scales",
    "WHERE scale IS NOT NULL AND TRIM(scale) <> ''",
    "AND (source IS NULL OR source <> 'unnamed_block')")
  where <- character(0); params <- list()
  if (nzchar(query)) {
    parsed <- trove_parse_query(query,
                fields = c("scale", "code", "source", "confidence"))
    for (fld in names(parsed$field_terms))
      for (term in parsed$field_terms[[fld]]) {
        where <- c(where, sprintf("%s LIKE ?", fld))
        params <- c(params, list(paste0("%", term, "%")))
      }
    for (term in parsed$free_terms) {
      lt <- .sv_like_terms(term, c("scale", "abbreviation", "code", "source"),
                           where, params)
      where <- lt$where; params <- lt$params
    }
  }
  sql <- base
  if (length(where)) sql <- paste(sql, "AND", paste(where, collapse = " AND "))
  sql <- paste(sql, "GROUP BY code ORDER BY n_papers DESC, scale LIMIT",
               as.integer(limit))
  .sv_get(con, sql, params)
}

#' Get the items of one scale
#'
#' Return the individual items (questions) of a scale, with their wording where
#' the OSD carried a translation. Backs the app's "items in this scale" table.
#'
#' @param con a scienceverse connection.
#' @param code the scale `code` (as shown in [scienceverse_scales()]).
#' @param doi optional DOI to restrict to one paper's copy of the scale.
#'
#' @returns a data frame with `item_id`, `item_type`, and `text` (the question),
#'   ordered by item position. De-duplicated across papers by item id + text.
#' @export
scienceverse_scale_items <- function(con, code, doi = NULL) {
  .sv_require("DBI")
  if (!DBI::dbExistsTable(con, "scale_items")) {
    return(data.frame(position = integer(), item_id = character(),
                      item_type = character(), text = character()))
  }
  where <- "scale_code = ?"; params <- list(code)
  if (!is.null(doi) && nzchar(doi)) {
    where <- paste(where, "AND doi = ?"); params <- c(params, list(doi))
  }
  sql <- paste(
    "SELECT MIN(position) position, item_id, item_type, text",
    "FROM scale_items WHERE", where,
    "GROUP BY item_id, text ORDER BY position")
  .sv_get(con, sql, params)
}

#' Get a paper's summary table
#'
#' Return the per-paper summary_table (the wide row of per-module counts) as a
#' tidy two-column data frame (`metric`, `value`), for display on the paper
#' detail. Empty when the paper has no stored summary.
#'
#' @param con a scienceverse connection.
#' @param doi the paper identifier.
#' @returns a data frame with columns `metric` and `value`.
#' @export
scienceverse_summary <- function(con, doi) {
  .sv_require("DBI")
  if (!DBI::dbExistsTable(con, "summaries"))
    return(data.frame(metric = character(), value = character()))
  j <- DBI::dbGetQuery(con, "SELECT summary_json FROM summaries WHERE doi = ?",
                       params = list(doi))$summary_json
  if (!length(j) || is.na(j[[1]])) return(data.frame(metric = character(),
                                                     value = character()))
  lst <- tryCatch(jsonlite::fromJSON(j[[1]]), error = function(e) NULL)
  if (is.null(lst) || !length(lst)) return(data.frame(metric = character(),
                                                     value = character()))
  data.frame(metric = names(lst),
             value = vapply(lst, function(x) paste(as.character(x),
                                                   collapse = "; "), character(1)),
             row.names = NULL)
}

# Which findings table (if any) holds a given module's rows. Mirrors the ingest
# routing so the app can pull "the full table for this check" from the DB.
.sv_module_findings_table <- function(module) {
  tbl <- .sv_findings_table(module)  # stat/code/data/excel/other_findings
  tbl
}

#' Get the full result table for one check (module) of one paper
#'
#' The row-level result table each check produced is already stored as findings
#' rows; this returns them for one paper+module. `repo_check` (the one module
#' that emits no findings) is served from the `repo_files` table instead.
#'
#' @param con a scienceverse connection.
#' @param doi the paper identifier.
#' @param module the check module name (as in [scienceverse_checks_of()] / the
#'   `checks` table).
#' @param limit maximum rows.
#' @returns a data frame of that check's rows (all-NA columns dropped), or a
#'   0-row frame if the check produced no table.
#' @export
scienceverse_check_table <- function(con, doi, module, limit = 5000) {
  .sv_require("DBI")
  df <- NULL

  # Preferred source: the module's COMPLETE table, stored as JSON in
  # module_tables. This carries the rich per-module classification (power
  # aspects, prereg details, full variable stats) the report shows.
  if (DBI::dbExistsTable(con, "module_tables")) {
    j <- DBI::dbGetQuery(con,
      "SELECT table_json FROM module_tables WHERE doi = ? AND module = ?",
      params = list(doi, module))$table_json
    if (length(j) && !is.na(j[[1]]) && nzchar(j[[1]])) {
      df <- tryCatch(jsonlite::fromJSON(j[[1]]), error = function(e) NULL)
      if (!is.null(df) && !is.data.frame(df)) df <- as.data.frame(df)
    }
  }

  # Fallbacks for archives built before module_tables existed.
  if (is.null(df)) {
    if (identical(module, "repo_check") && DBI::dbExistsTable(con, "repo_files")) {
      df <- .sv_get(con, paste("SELECT * FROM repo_files WHERE doi = ? LIMIT",
                               as.integer(limit)), list(doi))
    } else {
      tbl <- .sv_module_findings_table(module)
      if (DBI::dbExistsTable(con, tbl))
        df <- .sv_get(con, sprintf(
          "SELECT * FROM %s WHERE doi = ? AND module = ? LIMIT %d",
          tbl, as.integer(limit)), list(doi, module))
    }
  }

  if (is.null(df) || !nrow(df)) return(data.frame())
  if (nrow(df) > limit) df <- df[seq_len(limit), , drop = FALSE]

  # Trim for a clean human display: drop keys, the raw LLM answer, internal id /
  # layout columns, and per-module duplicates of `text` (e.g. `text.power`). The
  # full table is still stored in module_tables; this only affects what's shown.
  # `_id$` catches metacheck's internal row indices (text_id, power_id, ...); a
  # bare `id` is kept (it can be meaningful, e.g. a prereg registration id).
  noise <- c("doi", "module", "paper_id", "answer", "formatted",
             grep("_id$", names(df), value = TRUE),
             grep("^text\\.", names(df), value = TRUE))
  df <- df[, !names(df) %in% noise, drop = FALSE]
  # Drop any column that is entirely empty/NA.
  df <- df[, vapply(df, function(c) any(!is.na(c) & nzchar(as.character(c))),
                    logical(1)), drop = FALSE]
  df
}

#' List the checks recorded for one paper
#'
#' @param con a scienceverse connection.
#' @param doi the paper identifier.
#' @returns a data frame of the paper's checks (module, traffic_light,
#'   summary_text).
#' @export
scienceverse_checks_of <- function(con, doi) {
  .sv_require("DBI")
  .sv_get(con, paste("SELECT module, traffic_light, summary_text FROM checks",
                     "WHERE doi = ? ORDER BY module"), list(doi))
}

#' Search files (the manifest) in a scienceverse archive
#'
#' @param con a scienceverse connection.
#' @param query bare terms search file_name/repo_url/status; `field:value`
#'   restricts (fields: file_name, data_type, data_format, repo_url, status).
#' @param data_type optional exact data_type filter (code/data/codebook/...).
#' @param limit maximum rows.
#' @returns a data frame of files.
#' @export
scienceverse_files <- function(con, query = "", data_type = NULL, limit = 1000) {
  .sv_require("DBI")
  query <- if (is.null(query)) "" else trimws(query)
  where <- character(0); params <- list()
  if (nzchar(query)) {
    parsed <- trove_parse_query(query,
                fields = c("file_name", "data_type", "data_format",
                           "repo_url", "status"))
    for (fld in names(parsed$field_terms))
      for (term in parsed$field_terms[[fld]]) {
        where <- c(where, sprintf("%s LIKE ?", fld))
        params <- c(params, list(paste0("%", term, "%")))
      }
    for (term in parsed$free_terms) {
      lt <- .sv_like_terms(term, c("file_name", "repo_url", "status"),
                           where, params)
      where <- lt$where; params <- lt$params
    }
  }
  if (!is.null(data_type) && nzchar(data_type)) {
    where <- c(where, "data_type = ?"); params <- c(params, list(data_type))
  }
  sql <- paste("SELECT doi, file_name, data_type, data_format, file_size,",
               "downloaded, status, file_url, repo_url FROM files")
  if (length(where)) sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  sql <- paste(sql, "LIMIT", as.integer(limit))
  .sv_get(con, sql, params)
}
