# Generate a Psych-DS-compliant copy of a repository. This is the file-writing
# counterpart to the `psychds_check` module: the check reports the compliance
# gap, this function fixes it on disk. Ported from datacheck's
# 3_psychds_convert.R, but driven by metacheck's data_check / codebook_check
# outputs and the paper object (no GROBID XML / CrossRef enrichment).

# Representation that receives a numeric statistics block in variableMeasured.
.psychds_numeric_reps <- c("numeric")
# Measurement levels that receive a valuePattern (their distinct values matter).
.psychds_categorical_levels <- c("nominal", "ordinal")
# Quality states that hold no measured content. These columns still get a
# variableMeasured entry (Psych-DS requires every CSV column to be described, so
# omitting them makes the dataset fail validation with CsvColumnMissingFromMetadata),
# but the entry is a minimal stub marked empty rather than a full facet block —
# an all-NA column has no statistics, value pattern, or measurement level to report.
.psychds_empty_quality <- c("empty")

# Build the variableMeasured list for one set of columns. `cols` is a subset of
# data_check's columns table; `labels` is codebook_check's labels table (or
# NULL). Returns a list of PropertyValue objects.
.psychds_variable_measured <- function(cols, labels = NULL) {
  if (is.null(cols) || nrow(cols) == 0) return(list())

  # Scale dictionary for the OSD code cross-reference (loaded once).
  .osd_dict <- tryCatch(get("scales", envir = asNamespace("metacheck")),
                        error = function(e) NULL)
  if (is.null(.osd_dict))
    .osd_dict <- data.frame(name = character(), acronym = character(),
                            code = character(), source = character())

  # Attach labels by source_file + column_name when available.
  if (!is.null(labels) && nrow(labels) > 0 &&
      all(c("source_file", "column_name") %in% names(labels))) {
    keep <- c("source_file", "column_name", "label", "label_status",
              "label_source", "label_method", "codebook_variable",
              "scale", "scale_confidence", "scale_source",
              "value_labels", "missing_values", "question", "universe")
    labels <- labels[, intersect(keep, names(labels)), drop = FALSE]
    cols <- merge(cols, labels, by = c("source_file", "column_name"),
                  all.x = TRUE, suffixes = c("", ".lbl"))
  }

  # Every column is described (Psych-DS requires each CSV column to appear in
  # variableMeasured). An empty (all-NA) column gets a minimal stub entry — it
  # has no content to describe beyond "this column exists and holds no data" —
  # while the rest get the full facet schema
  # (representation/quality/measurement_level/concept/unit).
  qual <- tolower(cols$quality %||% "")
  if (nrow(cols) == 0) return(list())

  lapply(seq_len(nrow(cols)), function(i) {
    row <- cols[i, ]

    # Empty column: a minimal PropertyValue naming it and flagging it empty, so
    # the CSV column is documented without inventing statistics for all-NA data.
    if (qual[i] %in% .psychds_empty_quality) {
      return(Filter(Negate(is.null), list(
        `@type` = "PropertyValue",
        name    = row$column_name,
        description = "Empty column (no observed values in the shared data).",
        `metacheck:quality` = "empty",
        `metacheck:source_file` = if (!is.na(row$source_file)) row$source_file else NULL
      )))
    }

    rep_ <- row$representation %||% NA_character_
    lvl  <- row$measurement_level %||% NA_character_
    pv  <- list(`@type` = "PropertyValue", name = row$column_name)

    # Variable description from the codebook, when documented.
    if ("label_status" %in% names(row) && !is.na(row$label_status) &&
        row$label_status %in% c("labelled", "llm") &&
        !is.na(row$label) && nzchar(row$label)) {
      pv[["description"]]                 <- row$label
      pv[["metacheck:label_source"]]      <- row$label_source
      pv[["metacheck:codebook_variable"]] <- row$codebook_variable
    }

    # Psychometric scale membership (from codebook_check's LLM scale
    # identification). The scale NAME goes in schema.org's native
    # `measurementTechnique` (its intended use — the instrument a variable was
    # measured with); the grouping/confidence go in a namespaced extension, as
    # schema.org has no compositional grouping property. This mirrors DDI's
    # `analysis` variable-group concept ("variables combined into the same
    # index") within Psych-DS's JSON-LD.
    if ("scale" %in% names(row) && !is.na(row$scale) && nzchar(row$scale)) {
      pv[["measurementTechnique"]] <- row$scale
      # Cross-reference to the exported OSD scale definition: the same code the
      # .osd file is written under (scales/{code}.osd), so a reader can jump from
      # a variable to its instrument definition. scale_source also marks how the
      # scale was identified (dictionary/manuscript/generated).
      ssrc <- if ("scale_source" %in% names(row) && !is.na(row$scale_source))
        row$scale_source else NA_character_
      cp <- .osd_code_and_provenance(row$scale, row$column_name, ssrc, .osd_dict)
      pv[["metacheck:scale"]] <- Filter(Negate(is.null), list(
        name       = row$scale,
        code       = cp$code,
        source     = cp$source,
        confidence = if ("scale_confidence" %in% names(row) &&
                         !is.na(row$scale_confidence)) row$scale_confidence else NULL
      ))
    }

    # Measurement level → schema.org has no native property, so record it in a
    # namespaced extension (DDI @classificationLevel). Unit → schema.org
    # `unitText`. Concept → namespaced extension (DDI Variable→Concept).
    if (!is.na(lvl)) pv[["metacheck:measurementLevel"]] <- lvl
    concept <- if ("concept" %in% names(row)) row$concept %||% NA_character_ else NA_character_
    if (!is.na(concept) && nzchar(concept)) pv[["metacheck:concept"]] <- concept
    unit <- if ("unit" %in% names(row)) row$unit %||% NA_character_ else NA_character_
    if (!is.na(unit) && nzchar(unit)) pv[["unitText"]] <- unit
    role <- if ("role" %in% names(row)) row$role %||% NA_character_ else NA_character_
    if (!is.na(role) && nzchar(role)) pv[["metacheck:role"]] <- role

    # Question text + universe/filter (DDI QuestionText, Universe).
    question <- if ("question" %in% names(row)) row$question %||% NA_character_ else NA_character_
    if (!is.na(question) && nzchar(question)) pv[["metacheck:question"]] <- question
    universe <- if ("universe" %in% names(row)) row$universe %||% NA_character_ else NA_character_
    if (!is.na(universe) && nzchar(universe)) pv[["metacheck:universe"]] <- universe

    # Value labels / code list (DDI CodeList): emit as schema.org PropertyValue
    # children so each code->label pair is machine-readable. The raw JSON is also
    # kept in a namespaced field for round-tripping.
    vl_json <- if ("value_labels" %in% names(row)) row$value_labels %||% NA_character_ else NA_character_
    vl <- .decode_value_labels(vl_json)
    if (!is.null(vl) && length(vl) > 0) {
      pv[["metacheck:valueLabels"]] <- vl_json
      pv[["metacheck:codeList"]] <- unname(lapply(seq_along(vl), function(k)
        list(`@type` = "PropertyValue", value = names(vl)[k], name = unname(vl)[k])))
    }

    # Missing-value scheme (DDI MissingValues): which codes denote missingness,
    # distinguishing sentinels from real values.
    mv_json <- if ("missing_values" %in% names(row)) row$missing_values %||% NA_character_ else NA_character_
    if (!is.na(mv_json) && nzchar(mv_json))
      pv[["metacheck:missingValues"]] <- mv_json

    # Numeric statistics block (numeric representation).
    if (identical(rep_, "numeric")) {
      if ("min" %in% names(row) && !is.na(row$min)) pv[["minValue"]] <- row$min
      if ("max" %in% names(row) && !is.na(row$max)) pv[["maxValue"]] <- row$max
      stat_fields <- c("n", "n_missing", "mean", "sd", "se", "median",
                       "p25", "p75", "iqr", "skewness", "kurtosis")
      stat_block <- lapply(stat_fields, function(f)
        if (f %in% names(row) && !is.na(row[[f]])) row[[f]] else NULL)
      names(stat_block) <- stat_fields
      stat_block <- Filter(Negate(is.null), stat_block)
      if (length(stat_block) > 0) pv[["metacheck:statistics"]] <- stat_block
    }

    # Value pattern for categorical (nominal/ordinal, non-numeric) columns.
    if (!is.na(lvl) && lvl %in% .psychds_categorical_levels &&
        !identical(rep_, "numeric") &&
        "sample_values" %in% names(row) && !is.na(row$sample_values) &&
        nzchar(row$sample_values)) {
      vals <- unique(trimws(strsplit(row$sample_values, "\\|")[[1]]))
      vals <- vals[nzchar(vals)]
      if (length(vals) > 0) pv[["valuePattern"]] <- paste(vals, collapse = "|")
    }

    if (!is.na(rep_)) pv[["metacheck:representation"]] <- rep_
    if (!is.na(row$source_file)) pv[["metacheck:source_file"]] <- row$source_file
    Filter(Negate(is.null), pv)
  })
}

# Normalise a keywords value into a flat list of scalar strings for JSON. On a
# scivrs_paper, paper$info$keywords is an AsIs list-column of length 1 whose one
# element is the character vector of keywords — so a naive as.list() keeps the
# outer wrapper and serialises as a nested array [["a","b"]] instead of the
# schema.org-correct ["a","b"]. Unwrap a single list/AsIs cell to its contents,
# then split into scalars. Returns NULL when there are no keywords.
.psychds_keywords <- function(kw) {
  if (is.null(kw) || length(kw) == 0) return(NULL)
  # Unwrap an AsIs / list cell (length-1 holding the real vector).
  if (is.list(kw) && length(kw) == 1) kw <- kw[[1]]
  kw <- unlist(kw, use.names = FALSE)
  kw <- as.character(kw)
  kw <- kw[!is.na(kw) & nzchar(kw)]
  if (length(kw) == 0) return(NULL)
  as.list(kw)
}

# Resolve the online download URL for a source file from data_check's structure
# table. Prefer the direct file URI (file_url, e.g. an OSF download link); fall
# back to the repository URL (repo_url) when there is no direct link (e.g.
# ResearchBox, which exposes no per-file download URL). Returns NA when neither
# is known. `structure_df` is data_check's per-file structure table.
.psychds_source_url <- function(file_name, structure_df) {
  if (is.null(structure_df) || !nrow(structure_df) ||
      !"file_name" %in% names(structure_df)) return(NA_character_)
  # Basename match. file_name is NOT unique across a paper's repositories (see
  # the copy loop's warning), so a duplicated basename resolves to the FIRST
  # matching row — acceptable for a "jump to the source online" link.
  i <- match(file_name, structure_df$file_name)
  if (is.na(i)) return(NA_character_)
  fu <- if ("file_url" %in% names(structure_df)) structure_df$file_url[i] else NA_character_
  if (!is.na(fu) && nzchar(fu)) return(fu)
  ru <- if ("repo_url" %in% names(structure_df)) structure_df$repo_url[i] else NA_character_
  if (!is.na(ru) && nzchar(ru)) return(ru)
  NA_character_
}

# MIME type for a source file's extension, for a DataDownload's encodingFormat.
# Returns NULL for an unknown extension, so the field is simply omitted.
.psychds_encoding_format <- function(file_name) {
  ext <- tolower(tools::file_ext(file_name))
  mimes <- c(csv = "text/csv", tsv = "text/tab-separated-values",
             txt = "text/plain", json = "application/json",
             xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
             xls = "application/vnd.ms-excel",
             sav = "application/x-spss-sav", dta = "application/x-stata-dta",
             rds = "application/x-r-rds", rdata = "application/x-r-data",
             sas7bdat = "application/x-sas-data", por = "application/x-spss-por",
             jasp = "application/x-jasp")
  if (ext %in% names(mimes)) unname(mimes[[ext]]) else NULL
}

# Build the schema.org `distribution` array (a list of DataDownload objects) for
# a study root: one entry per source data file that has a resolvable online URL.
# `data_files` are the source file names (basenames) landing in this root's
# data/; `structure_df` supplies their URLs. Returns NULL when none resolve, so
# `distribution` is dropped rather than emitted empty.
.psychds_distribution <- function(data_files, structure_df) {
  data_files <- unique(data_files[!is.na(data_files) & nzchar(data_files)])
  if (!length(data_files)) return(NULL)
  entries <- lapply(data_files, function(fn) {
    url <- .psychds_source_url(fn, structure_df)
    if (is.na(url)) return(NULL)
    Filter(Negate(is.null), list(
      `@type`        = "DataDownload",
      name           = fn,
      contentUrl     = url,
      encodingFormat = .psychds_encoding_format(fn)))
  })
  entries <- Filter(Negate(is.null), entries)
  if (!length(entries)) NULL else entries
}

# Build the dataset_description.json object for one study. `distribution` is the
# optional schema.org DataDownload list (built by .psychds_distribution) linking
# each source data file to its online download URL; NULL omits the field.
.psychds_dataset_description <- function(paper, study_label, property_values,
                                         distribution = NULL) {
  info <- paper$info %||% list()
  # paper$info may be a tibble; `$` on a missing tibble column warns, so look up
  # fields by name and return NULL when absent (or empty, e.g. a stub paper).
  ival <- function(field) {
    v <- if (field %in% names(info)) info[[field]] else NULL
    if (length(v) == 0) NULL else v
  }

  title <- ival("title")
  name <- if (!is.null(title) && nzchar(title))
    paste0(title, if (nzchar(study_label)) paste0(" — ", study_label) else "")
  else paste0("Dataset", if (nzchar(study_label)) paste0(" — ", study_label) else "")

  desc <- list(
    `@context`       = "https://schema.org/",
    `@type`          = "Dataset",
    name             = name,
    description      = paste0("Psych-DS dataset generated by metacheck",
                             if (nzchar(study_label)) paste0(" for ", study_label) else "",
                             "."),
    variableMeasured = property_values,
    schemaVersion    = "Psych-DS 1.5.1"
  )

  # Where the source data files can be downloaded online (schema.org). One
  # DataDownload per source file with a resolvable URL; omitted when none.
  if (!is.null(distribution) && length(distribution))
    desc[["distribution"]] <- distribution

  # Authors from the paper object.
  if (!is.null(paper$author) && nrow(paper$author) > 0) {
    nm <- trimws(paste(paper$author$given %||% "", paper$author$family %||% ""))
    nm <- nm[nzchar(nm)]
    if (length(nm) > 0)
      desc[["author"]] <- lapply(nm, function(x) list(`@type` = "Person", name = x))
  }
  doi <- ival("doi") %||% NA_character_
  if (!is.na(doi) && nzchar(doi))
    desc[["identifier"]] <- paste0("https://doi.org/", sub("^https?://doi.org/", "", doi))
  kw <- .psychds_keywords(ival("keywords"))
  if (!is.null(kw)) desc[["keywords"]] <- kw

  desc[["metacheck:generated"]] <- format(Sys.Date(), "%Y-%m-%d")
  Filter(Negate(is.null), desc)
}

# Build and write the multi-study collection metadata as `collection.json` at the
# output root. It is schema.org JSON-LD with @type "Collection" (a schema.org
# type), so any schema.org reader — and Google Dataset Search — can consume it,
# and it uses the same vocabulary Psych-DS itself speaks. It is deliberately NOT
# named dataset_description.json, so the Psych-DS validator (which only opens a
# file of that exact name) never validates it, and the root stays a non-dataset
# collection (Option A).
#
# hasPart indexes every part that exists: each study-<group>/ dataset (with its
# variable count), each root-level shared file, the paper full text, and the
# logs. `study_roots` are the "study-<group>" prefixes; `shared_files` the plan
# target paths that carry no study prefix; `fulltext_rel`/`logs_rel` the
# root-relative paths of those generated artifacts. Returns the written path.
.psychds_collection_json <- function(paper, output_dir, pid, study_roots,
                                     columns_df = NULL, labels_df = NULL,
                                     shared_files = character(0),
                                     fulltext_rel = character(0),
                                     logs_rel = character(0),
                                     paradata = list()) {
  info <- paper$info %||% list()
  ival <- function(field) {
    v <- if (field %in% names(info)) info[[field]] else NULL
    if (length(v) == 0) NULL else v
  }

  title <- ival("title")
  name <- if (!is.null(title) && nzchar(title)) title else
    paste0("Data collection (", pid, ")")

  # hasPart: one Dataset entry per study root, carrying its variable count.
  parts <- lapply(study_roots, function(sr) {
    grp <- sub("^study-", "", sr)
    n_vars <- if (!is.null(columns_df) && "group" %in% names(columns_df))
      sum(!is.na(columns_df$group) & columns_df$group == grp) else NA_integer_
    Filter(Negate(is.null), list(
      `@type`   = "Dataset",
      name      = paste("Study", toupper(grp)),
      # A relative reference to the sub-dataset's own metadata file.
      identifier = paste0(sr, "/"),
      `metacheck:datasetDescription` = paste0(sr, "/dataset_description.json"),
      `metacheck:variableCount` = if (!is.na(n_vars)) n_vars else NULL))
  })

  # hasPart: root-level shared files (codebooks, materials, documentation), the
  # paper full text, and the provenance logs — as CreativeWork references.
  ref_parts <- function(paths, type, note = NULL) {
    paths <- unique(paths[!is.na(paths) & nzchar(paths)])
    lapply(paths, function(p) Filter(Negate(is.null), list(
      `@type` = type, name = basename(p),
      `metacheck:path` = p,
      `metacheck:role` = note)))
  }
  parts <- c(parts,
             ref_parts(shared_files, "CreativeWork", "shared across studies"),
             ref_parts(fulltext_rel, "CreativeWork", "paper full text"),
             ref_parts(logs_rel, "CreativeWork", "metacheck provenance log"))

  # hasPart: one Dataset entry per Behaverse paradata file — the trial-level
  # (response time / stimulus / option) data for an instrument, cross-referenced
  # to the matching scale (OSD) on the canonical instrument id.
  parts <- c(parts, lapply(paradata, function(pd) Filter(Negate(is.null), list(
    `@type`   = "Dataset",
    name      = paste("Paradata:", pd$instrument_id),
    identifier = pd$path,
    `metacheck:instrument_id` = pd$instrument_id,
    `metacheck:sourceFormat`  = if (nzchar(pd$format %||% "")) pd$format else NULL,
    `metacheck:responseCount` = pd$n_responses,
    `metacheck:scale`         = if (!is.na(pd$osd_link)) pd$osd_link else NULL))))

  coll <- list(
    `@context`  = "https://schema.org/",
    `@type`     = "Collection",
    name        = name,
    description = paste0(
      "A collection of ", length(study_roots), " Psych-DS datasets (one per ",
      "study) generated by metacheck. Each study-*/ part is an independently ",
      "valid Psych-DS dataset; this collection root is intentionally not itself ",
      "a Psych-DS dataset."),
    hasPart     = parts)

  # Authors, DOI, keywords — same extraction as the per-study description.
  if (!is.null(paper$author) && nrow(paper$author) > 0) {
    nm <- trimws(paste(paper$author$given %||% "", paper$author$family %||% ""))
    nm <- nm[nzchar(nm)]
    if (length(nm) > 0)
      coll[["author"]] <- lapply(nm, function(x) list(`@type` = "Person", name = x))
  }
  doi <- ival("doi") %||% NA_character_
  if (!is.na(doi) && nzchar(doi))
    coll[["identifier"]] <- paste0("https://doi.org/",
                                   sub("^https?://doi.org/", "", doi))
  kw <- .psychds_keywords(ival("keywords"))
  if (!is.null(kw)) coll[["keywords"]] <- kw

  coll[["dateCreated"]]        <- format(Sys.Date(), "%Y-%m-%d")
  coll[["metacheck:generated"]] <- format(Sys.Date(), "%Y-%m-%d")
  coll <- Filter(Negate(is.null), coll)

  path <- file.path(output_dir, "collection.json")
  .psychds_write_json(coll, path)
  invisible(path)
}

# Resolve the module outputs a converter needs, reusing anything already
# computed and running only what is missing. This is what lets a converter be
# called on a captured report()/report_module_run() result (a chain object) and
# skip re-running the modules — the same way modules reuse each other's outputs
# within a chain.
#
# `paper` may be a plain paper object OR a captured chain result (the list
# returned by report()/report_module_run(), or a single metacheck_module_output).
# `needed` is the module vector.
#
# report()/report_module_run() attach the paper to their result as a "paper"
# attribute, so a captured result carries everything: its module outputs are
# reused and the real paper (title/authors/DOI) is recovered from the attribute.
# When no paper is recoverable at all, the paper_id is taken from the module
# tables and a minimal stub paper stands in for the metadata builders.
#
# Returns list(ops = <named module outputs>, paper = <paper>, pid = <id string>).
.converter_resolve <- function(paper, needed,
                               local_path = NULL, local_only = FALSE,
                               model = llm_model(), params = list()) {
  # Coerce whatever we were handed into (a) a named list of module outputs we
  # can reuse, and (b) the underlying paper object (when available).
  chain <- paper

  if (inherits(chain, "metacheck_module_output")) {
    # report_module_run() returns the LAST module's output, with every EARLIER
    # module nested in $prev_outputs. Reuse them ALL — not just the last one —
    # so the converter uses the data_check result the chain already produced
    # (with its download="all" file_locations) instead of re-running data_check
    # with default args (download="data"), which would leave code/supplemental
    # files with file_location = NA and make the converter drop them as "not
    # downloaded". This was the cause of code/supplemental files (e.g. .Rmd
    # analysis scripts) missing from the Psych-DS output.
    reuse <- chain$prev_outputs %||% list()
    this <- chain
    this$prev_outputs <- NULL
    reuse[[chain$module %||% "unknown"]] <- this
  } else if (is.list(chain) && !inherits(chain, "scivrs_paper") &&
             any(needed %in% names(chain))) {
    reuse <- chain           # already a named list of module outputs
  } else {
    reuse <- list()          # a plain paper: nothing to reuse
  }

  # Recover the underlying paper: an explicit paper arg, else the "paper"
  # attribute report()/report_module_run() attaches to its result, else a
  # module output that still carries $paper. Otherwise NULL.
  real_paper <- if (inherits(paper, "scivrs_paper")) paper else NULL
  if (is.null(real_paper)) {
    att <- attr(chain, "paper")
    if (inherits(att, "scivrs_paper")) real_paper <- att
  }
  if (is.null(real_paper) && length(reuse) > 0) {
    for (mo in reuse) {
      if (!is.null(mo$paper) && inherits(mo$paper, "scivrs_paper")) {
        real_paper <- mo$paper; break
      }
    }
  }

  # Reuse is all-or-nothing: the modules chain through get_prev_outputs, which
  # only sees outputs produced within the same report_module_run() call, so a
  # reused output cannot feed a freshly-run one. If every needed module is
  # already present we reuse them wholesale; otherwise we re-run the full chain
  # from the paper (which wires the modules together correctly). This still
  # skips all work when a complete captured result is handed in.
  if (all(needed %in% names(reuse))) {
    ops <- reuse[needed]
  } else {
    if (is.null(real_paper))
      stop("Cannot run the module(s) ",
           paste(setdiff(needed, names(reuse)), collapse = ", "),
           " because no paper object is available. Pass the paper object (not ",
           "only a partial captured report result) as `paper`.", call. = FALSE)
    ops <- report_module_run(
      real_paper, needed,
      args = list(data_check = list(local_path = local_path,
                                    local_only = local_only,
                                    model = model, params = params)))
  }

  # Surface an upstream module failure with its real error, rather than letting
  # the converter fall over later with a cryptic "empty plan" / "no columns"
  # message. A failed module carries traffic_light == "fail" and its error text
  # in $report (see report_module_run()).
  for (m in needed) {
    mo <- ops[[m]]
    if (!is.null(mo) && identical(mo$traffic_light, "fail")) {
      msg <- paste(mo$report %||% "unknown error", collapse = " ")
      hint <- ""
      if (grepl("maximum number of calls", msg)) {
        # Suggest a limit above the number of calls the run actually needed.
        needed_calls <- suppressWarnings(as.integer(
          sub(".*would make ([0-9]+) calls.*", "\\1", msg)))
        suggest <- if (!is.na(needed_calls)) needed_calls + 10L else 200L
        hint <- sprintf(
          "\n\nRaise the limit above what the run needs, e.g. llm_max_calls(%d).",
          suggest)
      }
      stop("The '", m, "' module failed, so there is nothing to convert:\n  ",
           paste(mo$report %||% "unknown error", collapse = "\n  "), hint,
           call. = FALSE)
    }
  }

  # paper_id: from the real paper, else recovered from any module table.
  pid <- if (!is.null(real_paper)) paper_id(real_paper) else character(0)
  if (length(pid) == 0) {
    for (mo in ops) {
      for (tbl in list(mo$table, mo$summary_table, mo$structure)) {
        if (!is.null(tbl) && "paper_id" %in% names(tbl)) {
          got <- unique(tbl$paper_id[!is.na(tbl$paper_id)])
          if (length(got) > 0) { pid <- got; break }
        }
      }
      if (length(pid) > 0) break
    }
  }
  pid <- if (length(pid) == 0) "dataset" else as.character(pid[[1]])

  # When the underlying paper is unrecoverable (only a captured result was
  # given), stand in a minimal paper carrying just the recovered id, so the
  # dataset-metadata builders have a valid paper object (empty title/authors).
  if (is.null(real_paper)) real_paper <- paper(id = pid)

  # `ops` is the subset the converter needs (data_check/codebook_check/
  # psychds_check). `all_ops` is EVERY captured module output when a full chain
  # was handed in (reuse holds them all) — used for the provenance logs so the
  # logs/ RDS + checks JSON record the whole run, not just the converter's three
  # modules. When modules were re-run from a bare paper (no reuse), the full set
  # is just `ops`.
  all_ops <- if (length(reuse)) reuse else ops

  list(ops = ops, all_ops = all_ops, paper = real_paper, pid = pid)
}

# Build the "why is there nothing to convert" explanation. When a paper linked
# a repository that was found but could not be *listed* (a GitHub repo over the
# size gate, a private OSF component), the bare "no repository" / "empty plan"
# message is misleading — the repository exists, it was just skipped. This spells
# out which repo, why, and how to include it: raise the caps to fetch it, or
# download it manually and point data_check at the local copy. `ops` is the
# resolved module-output list; returns "" when no repo was gated.
.converter_gated_hint <- function(ops) {
  g <- ops[["data_check"]]$gated_repos %||% ops[["repo_check"]]$gated_repos
  if (is.null(g) || nrow(g) == 0) return("")
  # One paper often links the same repo by several deep URLs (…/tree/master/x,
  # …/tree/master/y); collapse to the repo root so each is reported once.
  g$root <- sub("/(tree|blob)/.*$", "", g$repo_url)
  g <- g[!duplicated(paste(g$root, g$repo_error)), , drop = FALSE]
  lines <- vapply(seq_len(nrow(g)), function(i) {
    sprintf("  - %s: %s", g$root[i],
            g$repo_error[i] %||% "could not be listed")
  }, character(1))
  paste0(
    "\n\nA linked repository was found but not downloaded, so there were no ",
    "files to process:\n",
    paste(lines, collapse = "\n"),
    "\n\nTo include it, either raise the size limits (e.g. ",
    "`max_download_size` / `max_file_size`, or `github_gate = FALSE` for a ",
    "GitHub repo) and re-run, or download the repository manually and pass its ",
    "folder to data_check as a local repository: ",
    "`data_check(paper, local_path = \"/path/to/downloaded/repo\")`.")
}

.psychds_write_json <- function(obj, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, na = "null")
  writeLines(json, path, useBytes = TRUE)
  invisible(path)
}

# Write the paper's extracted full text as a plain UTF-8 text file under the
# dataset root's `documentation/` folder, so the release carries the manuscript
# prose the checks were run against (not just the data). The per-unit text table
# is read via paper_table(paper, "text") — the same accessor search_text() uses —
# and joined to paper_table(paper, "section") for section headers; we reconstruct
# readable text by ordering on text_id and emitting a "# <header>" line whenever
# the section changes. Returns the written path, or NULL when the paper carries no
# full text (e.g. a stub paper recovered from a captured result).
#
# `documentation/` always sits at the true output root. In a flat (single-study)
# dataset that folder is unambiguous, so the file is `fulltext.txt`. In a
# multi-study dataset the studies live in `study-*/` subtrees and the root
# `documentation/` is shared context, so the file is pid-qualified
# (`<pid>_fulltext.txt`) to keep it identifiable.
.psychds_write_paper_text <- function(paper, output_dir, pid, multi_study) {
  if (!.is_paper(paper)) return(NULL)
  ft <- tryCatch(paper_table(paper, "text"), error = function(e) NULL)
  if (is.null(ft) || !is.data.frame(ft) || nrow(ft) == 0) return(NULL)
  if (!"text" %in% names(ft)) return(NULL)

  # Attach section headers the same way search_text() does, when available.
  sections <- tryCatch(paper_table(paper, "section"), error = function(e) NULL)
  scols <- c("section_id", "paper_id", "header", "section_type")
  if (!is.null(sections) && is.data.frame(sections) &&
      all(scols %in% names(sections)) && "section_id" %in% names(ft)) {
    ft <- dplyr::left_join(ft, sections[, scols],
                           by = intersect(c("section_id", "paper_id"), names(ft)),
                           relationship = "many-to-many")
  }

  # Order by reading order when available; otherwise keep the given order.
  if ("text_id" %in% names(ft)) ft <- ft[order(ft$text_id), , drop = FALSE]

  header <- if ("header" %in% names(ft)) ft$header else rep(NA_character_, nrow(ft))
  lines <- character(0)
  last_header <- NA_character_
  for (i in seq_len(nrow(ft))) {
    txt <- ft$text[i]
    if (is.na(txt) || !nzchar(trimws(txt))) next
    h <- header[i]
    # Emit a section header line when the header changes (and is real text that
    # is not just a repeat of the paragraph itself).
    if (!is.na(h) && nzchar(trimws(h)) &&
        !identical(h, last_header) && !identical(trimws(h), trimws(txt))) {
      if (length(lines) > 0) lines <- c(lines, "")
      lines <- c(lines, paste0("# ", trimws(h)), "")
      last_header <- h
    }
    lines <- c(lines, trimws(txt))
  }
  if (length(lines) == 0) return(NULL)

  doc_dir <- file.path(output_dir, "documentation")
  dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)
  fname <- if (isTRUE(multi_study)) paste0(pid, "_fulltext.txt") else "fulltext.txt"
  path <- file.path(doc_dir, fname)
  # UTF-8, BOM-free (writeLines with a UTF-8 connection adds no BOM).
  con <- file(path, open = "wb", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con, useBytes = FALSE)
  invisible(path)
}

# Build a REDUCED file manifest from data_check's structure table, for when no
# full manifest was written at run time (data_check's `manifest` arg unset). The
# full manifest (.data_check_write_manifest) needs run-time state the module
# output does not carry — the download gate tables, per-file skip reasons, HEAD
# size probes — so this fallback records only what the finished output knows: the
# file inventory with each file's downloaded/not-downloaded status, plus the same
# `provenance` block. It writes the same top-level shape (files[] + provenance)
# so write_file_manifest() can still read it. Returns the written path.
.psychds_reduced_manifest <- function(structure_df, pid, path) {
  cols <- function(nm) if (!is.null(structure_df[[nm]])) structure_df[[nm]] else
    rep(NA_character_, nrow(structure_df))
  loc  <- cols("file_location")
  downloaded <- !is.na(loc) & nzchar(loc) & file.exists(loc %||% "")

  entries <- lapply(seq_len(nrow(structure_df)), function(i) {
    sz <- suppressWarnings(as.numeric(cols("file_size")[i]))
    if (isTRUE(downloaded[i])) {
      on_disk <- suppressWarnings(file.size(loc[i]))
      if (!is.na(on_disk)) sz <- on_disk
    }
    Filter(Negate(is.null), list(
      file_name   = cols("file_name")[i],
      file_path   = cols("file_path")[i] %||% cols("file_name")[i],
      repo_url    = cols("repo_url")[i],
      file_url    = cols("file_url")[i],
      file_size   = if (!is.na(sz)) sz else NULL,
      data_type   = cols("data_type")[i],
      data_format = cols("data_format")[i],
      downloaded  = downloaded[i],
      status      = if (isTRUE(downloaded[i])) "downloaded" else "not_downloaded"
    ))
  })

  provenance <- list(
    software  = list(name = "metacheck", version = tryCatch(
      as.character(utils::packageVersion("metacheck")),
      error = function(e) NA_character_)),
    r_version = R.version.string,
    platform  = R.version$platform,
    prod_date = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    llm       = if (isTRUE(llm_use()))
      list(used = TRUE, model = llm_model()) else list(used = FALSE),
    # This manifest is the reduced form: it lacks the full manifest's per-file
    # skip reasons and gate details (those need data_check's run-time state).
    manifest_kind = "reduced"
  )

  doc <- Filter(Negate(is.null), list(
    paper_id     = pid,
    generated    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    provenance   = provenance,
    n_files      = nrow(structure_df),
    n_downloaded = sum(downloaded),
    files        = entries
  ))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(doc, auto_unbox = TRUE, pretty = TRUE, na = "null")
  writeLines(json, path, useBytes = TRUE)
  invisible(path)
}

# Write the provenance `logs/` folder for a converted dataset and return a named
# character vector of the files written (name = one-line description, for the
# README). Folds the three previously-separate artifacts into the conversion:
#   * <pid>.manifest.json — the file inventory. The FULL manifest is reused when
#     data_check wrote one this run (ops$data_check$manifest_path exists on disk);
#     otherwise a reduced manifest is built from the structure table.
#   * <pid>.checks.json    — per-paper check summaries + findings, when any check
#     module ran (capture_check_results on the module-output list).
#   * <pid>.rds            — full lossless module tables, when a module produced a
#     non-empty table (capture_module_tables).
# `ops` is convert_psychds's resolved named list of module outputs (a chain the
# capture_* functions accept directly). logs/ always sits at the dataset root.
.psychds_write_logs <- function(ops, structure_df, pid, output_dir,
                                all_ops = NULL) {
  logs_dir <- file.path(output_dir, "logs")
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  written <- character(0)
  # The checks JSON and module-tables RDS record the WHOLE run, so use every
  # captured module (all_ops) when available; the converter's `ops` is only the
  # 3 modules it needs to build the archive. The manifest below stays on `ops`
  # (it is data_check-specific). Falls back to `ops` when no full set was passed.
  log_ops <- if (!is.null(all_ops) && length(all_ops)) all_ops else ops

  # 1. Manifest — always. Reuse the full one if present, else reduced.
  manifest_dest <- file.path(logs_dir, paste0(pid, ".manifest.json"))
  full <- ops[["data_check"]]$manifest_path
  if (!is.null(full) && length(full) && file.exists(full[[1]])) {
    file.copy(full[[1]], manifest_dest, overwrite = TRUE)
    written[manifest_dest] <-
      "File inventory recording every repository file and whether it was downloaded (full manifest, with per-file skip reasons)."
  } else if (!is.null(structure_df) && is.data.frame(structure_df) &&
             nrow(structure_df) > 0) {
    .psychds_reduced_manifest(structure_df, pid, manifest_dest)
    written[manifest_dest] <-
      "File inventory recording every repository file and whether it was downloaded (reduced manifest)."
  }

  # 2. Checks JSON — when any module output is present (the checks that ran).
  has_modules <- any(vapply(log_ops, function(mo)
    inherits(mo, "metacheck_module_output") || is.list(mo), logical(1)))
  if (length(log_ops) > 0 && has_modules) {
    checks_path <- tryCatch(
      capture_check_results(log_ops, logs_dir, paper_id = pid),
      error = function(e) NULL)
    if (!is.null(checks_path) && file.exists(checks_path))
      written[checks_path] <-
        "Per-paper results of the metacheck checks that were run: each module's outcome plus its individual findings."
  }

  # 3. Tables RDS — when at least one module produced a non-empty table.
  has_table <- any(vapply(log_ops, function(mo)
    is.data.frame(mo$table) && nrow(mo$table) > 0, logical(1)))
  if (has_table) {
    rds_path <- tryCatch(
      capture_module_tables(log_ops, logs_dir, paper_id = pid),
      error = function(e) NULL)
    if (!is.null(rds_path) && file.exists(rds_path))
      written[rds_path] <-
        "Full, lossless module output tables (R data file) for later reloading and analysis without re-running the checks."
  }

  written
}

# Append (or create) a "Provenance logs" section to the dataset-root README,
# listing each file written into logs/ and what it holds. `logs_written` is the
# named vector from .psychds_write_logs() (name = path, value = description).
.psychds_write_logs_readme <- function(logs_written, output_dir, readme_path) {
  if (!length(logs_written)) return(invisible())
  # logs/ always sits at the dataset root, so the README-relative path of each
  # file is just "logs/<basename>".
  rel <- file.path("logs", basename(names(logs_written)))
  lines <- c(
    "",
    "## Provenance logs",
    "",
    paste0("The `logs/` folder holds the metacheck records behind this ",
           "conversion — the provenance of what was checked and packaged:"),
    "",
    vapply(seq_along(logs_written), function(i)
      sprintf("- **`%s`** — %s", rel[i], unname(logs_written)[i]), character(1)),
    "")
  con_lines <- if (file.exists(readme_path))
    readLines(readme_path, warn = FALSE) else character(0)
  writeLines(c(con_lines, lines), readme_path, useBytes = TRUE)
  invisible()
}

# Copy a file, dropping a leading UTF-8 BOM (EF BB BF) if present. Byte-level so
# it works for any encoding/content. Returns TRUE on success.
.psychds_copy_no_bom <- function(src, dest) {
  tryCatch({
    n <- file.info(src)$size
    if (is.na(n) || n == 0) return(file.copy(src, dest, overwrite = TRUE))
    con <- file(src, "rb"); on.exit(close(con), add = TRUE)
    bytes <- readBin(con, "raw", n)
    if (length(bytes) >= 3 &&
        bytes[1] == as.raw(0xEF) && bytes[2] == as.raw(0xBB) &&
        bytes[3] == as.raw(0xBF)) {
      bytes <- bytes[-(1:3)]
    }
    out <- file(dest, "wb"); on.exit(close(out), add = TRUE)
    writeBin(bytes, out)
    TRUE
  }, error = function(e) FALSE)
}

# Robustly coerce a plan column to logical (it may arrive as logical, or as
# character "TRUE"/"FALSE" after a JSON round-trip).
isTRUE_vec <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(as.character(x)) %in% c("true", "1")
}

# Write a genuine, Psych-DS-clean CSV from a non-CSV data source (xlsx, sav,
# dta, ...). Reads the file in full via data_read_head() — the same reader
# data_check uses, so the columns match variableMeasured — and writes UTF-8,
# BOM-free, no row names. Returns FALSE if the source cannot be read as a table.
.psychds_write_data_csv <- function(src, dest) {
  tryCatch({
    df <- data_read_head(src, n_rows = Inf)
    if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(FALSE)
    # write.csv adds no BOM; force UTF-8 for non-ASCII headers/values.
    con <- file(dest, open = "wb", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    utils::write.csv(df, con, row.names = FALSE, na = "")
    TRUE
  }, error = function(e) FALSE)
}

# Recover the analyses from a .jasp as a readable code artifact: a numbered,
# human-readable summary (one line per analysis) followed by the verbatim
# analyses.json for completeness. Returns TRUE if written. The analyses are the
# closest thing a .jasp has to "code" — a structured record of the tests run.
.psychds_write_jasp_code <- function(src, dest) {
  tryCatch({
    j <- read_jasp(src)
    summary <- .jasp_analyses_summary(j$analyses)
    if (!length(summary) && is.null(j$analyses)) return(FALSE)
    raw <- tryCatch(jsonlite::toJSON(j$analyses, auto_unbox = TRUE, pretty = TRUE),
                    error = function(e) NULL)
    lines <- c(
      paste0("# Analyses recovered from ", basename(src)),
      "# These are the JASP analyses stored in the file (not runnable R code).",
      "",
      if (length(summary)) summary else "(no analyses recorded)",
      "",
      "# --- Raw analyses.json ---",
      if (!is.null(raw)) raw else "(unavailable)")
    con <- file(dest, open = "wb", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(lines, con, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

#' Generate a Psych-DS-compliant copy of a repository
#'
#' Writes a [Psych-DS](https://psych-ds.github.io/) dataset directory for a
#' paper's data repository: files are copied to their standard locations, a
#' `dataset_description.json` is generated (with `variableMeasured` built from
#' the extracted columns and codebook labels), and `README` / `CHANGES` stubs
#' are added when missing. This is the generator counterpart of the
#' `psychds_check` module — run the check first to preview the gap.
#'
#' Multi-study repositories (when `data_check` assigned study groups under
#' `llm_use(TRUE)`) are written as `study-<group>/` subdirectories, each a
#' self-contained, independently-valid Psych-DS dataset. Files that belong to no
#' single study (a whole-repo README/codebook, shared materials) sit at the
#' collection root beside the study folders (following BIDS, which places shared
#' content at the root rather than in a pseudo-subject). The root is then a
#' *collection* of datasets, not itself a Psych-DS dataset, so it carries a
#' machine-readable `collection.json` (schema.org JSON-LD, `@type` `Collection`)
#' instead of a root `dataset_description.json` — validate each `study-*/` folder.
#' Original files whose contents cannot be read (no local copy) are skipped with
#' a note.
#'
#' @param paper a paper object (see [read()]), **or** a captured result of
#'   `report(paper, ...)` / `report_module_run(paper, ...)`. When a captured
#'   result containing all of `data_check` / `codebook_check` / `psychds_check`
#'   is passed, those outputs are reused (with the paper recovered from the
#'   result) instead of re-running; otherwise the modules are run.
#' @param output_dir directory to write the Psych-DS dataset into; created if
#'   needed. Defaults to `"psychds/<paper_id>"` under the working directory.
#' @param refresh_osf whether to fetch a fresh OSF file listing even if one was
#'   already retrieved this session. The default (`FALSE`) reuses the cached
#'   listing, so building the dataset right after running the checks does not
#'   re-query (and risk being throttled by) the OSF API. Set `TRUE` to force a
#'   fresh listing. Downloaded file contents are cached on disk regardless.
#' @param local_path,local_only passed to `data_check` when its output is not
#'   already available (see `data_check()`)
#' @param model,params passed to the underlying modules when `llm_use(TRUE)`
#' @param overwrite whether to overwrite an existing `output_dir`. When `FALSE`
#'   (the default) and `output_dir` already exists, the function messages and
#'   skips rather than erroring (the returned list has `existed = TRUE`).
#'
#' @returns (invisibly) a list with `output_dir`, `n_files_copied`,
#'   `n_studies`, `descriptions` (paths of written dataset_description.json
#'   files), `collection` (path of the root `collection.json` for a multi-study
#'   collection, else empty), `fulltext` (path of the paper's full-text file
#'   under `documentation/`, if written), and `logs` (paths written into
#'   `logs/`: the file manifest, and the check results / module tables when those
#'   modules ran). When an existing `output_dir` was skipped, the list
#'   additionally contains `existed = TRUE` and the counts are zero.
#' @export
#' @examples
#' \dontrun{
#' # Capture the checks, then build the folder — the modules are reused, not
#' # re-run, and the OSF listing is not fetched again:
#' res <- report(paper, c("data_check", "codebook_check", "psychds_check"))
#' convert_psychds(res)
#'
#' # Or call directly on the paper; the modules are run as needed:
#' convert_psychds(paper)
#' }
convert_psychds <- function(paper, output_dir = NULL,
                            refresh_osf = FALSE,
                            local_path = NULL, local_only = FALSE,
                            model = llm_model(), params = list(),
                            overwrite = FALSE) {
  # ── Gather the placement plan + data behind it ──────────────────────────────
  # Reuse the session's cached OSF listing unless a fresh one is requested, so
  # building right after the checks doesn't re-query (and risk throttling) OSF.
  old_osf_cache <- getOption("metacheck.osf.cache", TRUE)
  options(metacheck.osf.cache = !isTRUE(refresh_osf))
  on.exit(options(metacheck.osf.cache = old_osf_cache), add = TRUE)

  # Reuse whatever the checks already computed (when `paper` is a captured
  # report()/report_module_run() result); otherwise run the chain, so
  # codebook_check / psychds_check consume data_check's result via
  # get_prev_outputs.
  needed <- c("data_check", "codebook_check", "psychds_check")
  resolved <- .converter_resolve(paper, needed,
                                 local_path = local_path, local_only = local_only,
                                 model = model, params = params)
  ops     <- resolved$ops
  all_ops <- resolved$all_ops %||% ops   # full run for the provenance logs
  paper <- resolved$paper
  dc <- ops[["data_check"]]
  structure_df <- dc$structure
  columns_df   <- dc$table
  labels_df <- ops[["codebook_check"]]$table
  scales_osd <- ops[["codebook_check"]]$scales_osd
  plan      <- ops[["psychds_check"]]$table
  pid <- resolved$pid
  if (is.null(output_dir)) output_dir <- file.path("psychds", pid)

  if (is.null(plan) || nrow(plan) == 0) {
    message("No files to convert: psychds_check returned an empty plan.",
            .converter_gated_hint(ops))
    return(invisible(list(
      output_dir = output_dir,
      n_files_copied = 0L,
      n_studies = 0L,
      descriptions = character(0),
      skipped = character(0),
      copy_failed = character(0),
      empty_plan = TRUE
    )))
  }

  if (dir.exists(output_dir) && !overwrite) {
    message("Psych-DS output already exists, skipping: ", output_dir,
            ". Set overwrite = TRUE to replace it.")
    return(invisible(list(
      output_dir = output_dir, n_files_copied = 0L, n_studies = 0L,
      descriptions = character(0), skipped = character(0), existed = TRUE
    )))
  }
  if (dir.exists(output_dir) && overwrite) unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # A local copy is needed to copy the file's bytes. Resolve each plan row to its
  # source POSITIONALLY: psychds_check builds its table straight from
  # structure_df (one row per file, same order), so plan row i is structure row i.
  #
  # A name-keyed lookup (setNames(file_location, file_name)) is WRONG here and
  # silently corrupts the output: `file_name` is a basename and is NOT unique
  # across a paper's repositories — several OSF components each ship their own
  # `demographics.csv` / `learning_2.csv`. `loc[[name]]` returns only the FIRST
  # match, so every study's copy of a duplicated name got the same (first)
  # component's bytes: study-ex2/data/demographics.csv contained study 1's
  # participants. No column is unique enough to key on (in one real paper: 138
  # rows, 68 distinct file_name, 102 distinct file_path, 118 distinct
  # file_location), so the row index is the only correct identifier.
  #
  # Fall back to the name map only if the plan is NOT row-aligned with
  # structure_df (a caller passing a filtered/re-ordered plan), where positional
  # lookup would be worse than an imperfect name match.
  row_aligned <- nrow(plan) == nrow(structure_df) &&
    identical(as.character(plan$file_name), as.character(structure_df$file_name))
  loc_fallback <- stats::setNames(structure_df$file_location,
                                  structure_df$file_name)
  src_of <- function(i) {
    if (row_aligned) structure_df$file_location[i]
    else loc_fallback[[plan$file_name[i]]]
  }
  if (!row_aligned)
    warning("The conversion plan is not row-aligned with the file structure; ",
            "falling back to a file-name lookup, which cannot tell apart ",
            "same-named files from different repositories.", call. = FALSE)

  # ── Copy files to their target locations ────────────────────────────────────
  n_copied    <- 0L
  skipped     <- character(0) # files not on disk (never downloaded)
  skipped_i   <- integer(0)   # their plan-row indices, to group by type below
  copied_i    <- integer(0)   # plan rows actually written into the dataset
  copy_failed <- character(0) # files on disk that failed to copy (I/O error)
  # Does the plan mark this file for CSV conversion (a non-CSV data source)?
  plan_convert <- if ("convert" %in% names(plan)) isTRUE_vec(plan$convert) else
    rep(FALSE, nrow(plan))
  plan_orig_target <- if ("original_target" %in% names(plan))
    plan$original_target else rep(NA_character_, nrow(plan))

  for (i in seq_len(nrow(plan))) {
    src <- src_of(i)
    if (is.null(src) || is.na(src) || !nzchar(src) || !file.exists(src)) {
      skipped   <- c(skipped, plan$file_name[i])
      skipped_i <- c(skipped_i, i)
      next
    }
    dest <- file.path(output_dir, plan$target_path[i])
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

    if (isTRUE(plan_convert[i])) {
      # Non-CSV data source (xlsx/sav/dta/...): write a REAL CSV at the _data.csv
      # target from the fully-read data (not a byte copy of the original, which
      # would be an invalid CSV), and keep the untouched original beside it so
      # the release retains the authored artifact.
      wrote_csv <- .psychds_write_data_csv(src, dest)
      if (wrote_csv) {
        n_copied <- n_copied + 1L
        copied_i <- c(copied_i, i)
      } else copy_failed <- c(copy_failed, plan$file_name[i])
      # A .jasp also bundles the ANALYSES that were run: recover them as a
      # readable code artifact beside the data CSV, so the release carries both
      # the data (CSV) and the analysis provenance (the "code").
      if (grepl("\\.jasp$", src, ignore.case = TRUE))
        .psychds_write_jasp_code(src, sub("_data\\.csv$", "_jasp_code.txt", dest))
      # Copy the original alongside (original extension).
      if (!is.na(plan_orig_target[i]) && nzchar(plan_orig_target[i])) {
        odest <- file.path(output_dir, plan_orig_target[i])
        dir.create(dirname(odest), recursive = TRUE, showWarnings = FALSE)
        if (file.copy(src, odest, overwrite = TRUE)) {
          n_copied <- n_copied + 1L
          if (!wrote_csv) copied_i <- c(copied_i, i)
        } else copy_failed <- c(copy_failed, plan$file_name[i])
      }
    } else if (grepl("\\.csv$", dest, ignore.case = TRUE) &&
        grepl("^(study-[^/]+/)?data/", plan$target_path[i])) {
      # Data CSVs must be BOM-free: a UTF-8 BOM makes the first column header
      # read as "﻿id", which then mismatches variableMeasured (a Psych-DS
      # error).
      ok <- .psychds_copy_no_bom(src, dest)
      if (ok) {
        n_copied <- n_copied + 1L
        copied_i <- c(copied_i, i)
      } else copy_failed <- c(copy_failed, plan$file_name[i])
    } else if (file.copy(src, dest, overwrite = TRUE)) {
      n_copied <- n_copied + 1L
      copied_i <- c(copied_i, i)
    } else {
      copy_failed <- c(copy_failed, plan$file_name[i])
    }
  }

  # ── Study roots: derive from the target paths' study-<group>/ prefixes ───────
  # Only from plan rows whose file actually landed on disk: a study group whose
  # every file was skipped (never downloaded) must not become a directory
  # holding nothing but a generated dataset_description.json with an empty
  # variableMeasured — that is noise, not a dataset.
  planned_dirs <- unique(sub("^(study-[^/]+)/.*$", "\\1",
                             grep("^study-", plan$target_path, value = TRUE)))
  study_dirs <- unique(sub("^(study-[^/]+)/.*$", "\\1",
                           grep("^study-", plan$target_path[copied_i],
                                value = TRUE)))
  empty_study_dirs <- setdiff(planned_dirs, study_dirs)
  if (length(empty_study_dirs) > 0)
    message(sprintf(paste0(
      "%d planned study director%s omitted because none of %s files were ",
      "available on disk: %s."),
      length(empty_study_dirs),
      if (length(empty_study_dirs) == 1) "y was" else "ies were",
      if (length(empty_study_dirs) == 1) "its" else "their",
      paste(empty_study_dirs, collapse = ", ")))
  study_roots <- if (length(planned_dirs) > 0) study_dirs else ""

  # ── Write dataset_description.json per study root ───────────────────────────
  descriptions <- character(0)
  for (sr in study_roots) {
    # Columns belonging to this study root (by group), else all when flat.
    if (nzchar(sr)) {
      grp <- sub("^study-", "", sr)
      root_cols <- if (!is.null(columns_df) && "group" %in% names(columns_df))
        columns_df[!is.na(columns_df$group) & columns_df$group == grp, ,
                   drop = FALSE] else columns_df[0, , drop = FALSE]
      study_label <- paste("Study", toupper(grp))
    } else {
      root_cols <- columns_df
      study_label <- ""
    }
    pv <- .psychds_variable_measured(root_cols, labels_df)
    # Source data files landing in this root's data/ folder, resolved to their
    # online download URLs. The plan is row-aligned with structure_df (see the
    # copy loop), so a data-target plan row i maps to structure row i, whose
    # file_name is the source basename that carries the URL.
    data_prefix <- paste0(if (nzchar(sr)) paste0(sr, "/") else "", "data/")
    is_root_data <- startsWith(plan$target_path, data_prefix)
    root_data_files <- if (row_aligned)
      structure_df$file_name[is_root_data] else plan$file_name[is_root_data]
    distribution <- .psychds_distribution(root_data_files, structure_df)
    desc <- .psychds_dataset_description(paper, study_label, pv, distribution)
    dest <- file.path(output_dir, sr, "dataset_description.json")
    .psychds_write_json(desc, dest)
    descriptions <- c(descriptions, dest)

    # README / CHANGES stubs when missing at this root.
    root_has <- function(pat) any(grepl(pat,
      basename(plan$target_path[startsWith(plan$target_path,
        if (nzchar(sr)) paste0(sr, "/") else "")]), ignore.case = TRUE))
    # Psych-DS expects README and CHANGES to carry a .md/.txt extension.
    readme_dest  <- file.path(output_dir, sr, "README.md")
    changes_dest <- file.path(output_dir, sr, "CHANGES.md")
    if (!root_has("^README") && !file.exists(readme_dest))
      writeLines(c(paste0("# ", desc$name),
                   "", "Psych-DS dataset generated by metacheck.",
                   "See dataset_description.json for machine-readable metadata."),
                 readme_dest)
    if (!root_has("^CHANGES") && !file.exists(changes_dest))
      writeLines(c(paste0(
        format(Sys.Date(), "%Y-%m-%d"),
        " - Repository converted by ",
        "[Metacheck](https://www.scienceverse.org/metacheck/). CHANGES.md ",
        "document added, which can be used to log the version history of the ",
        "dataset (describing changes, updates and corrections).")),
        changes_dest)
  }

  # ── Multi-study collection root ─────────────────────────────────────────────
  # In a multi-study repository the output root is a COLLECTION of datasets (each
  # study-<group>/), not itself a Psych-DS dataset — so it gets NO root
  # dataset_description.json (Option A: a Psych-DS validator run on the root
  # correctly reports it is not a dataset; validate each study-<group>/ instead).
  # It does get BIDS-style root README/CHANGES, and a machine-readable
  # collection.json (schema.org JSON-LD, @type Collection). collection.json is
  # deliberately NOT named dataset_description.json, so the Psych-DS validator —
  # which only ever opens a file of that exact name — never sees it.
  multi_study <- length(planned_dirs) > 0
  if (multi_study) {
    root_has_root <- function(pat) any(grepl(pat,
      basename(plan$target_path[!grepl("^study-", plan$target_path)]),
      ignore.case = TRUE))
    readme_dest  <- file.path(output_dir, "README.md")
    changes_dest <- file.path(output_dir, "CHANGES.md")
    if (!root_has_root("^README") && !file.exists(readme_dest))
      writeLines(c(
        "# Multi-study data collection",
        "",
        paste0("This folder is a collection of ", length(study_roots),
               " Psych-DS datasets, one per study, generated by metacheck. Each ",
               "`study-*/` subfolder is a complete, independently-valid Psych-DS ",
               "dataset with its own `dataset_description.json`. Files shared ",
               "across studies (this README, cross-study codebooks and ",
               "materials) live here at the collection root."),
        "",
        paste0("See `collection.json` for a machine-readable description of the ",
               "collection and its parts."),
        "",
        paste0("**Note:** the collection root is intentionally *not* itself a ",
               "Psych-DS dataset (it has no root `dataset_description.json`). To ",
               "validate, point the validator at each `study-*/` folder.")),
        readme_dest)
    if (!root_has_root("^CHANGES") && !file.exists(changes_dest))
      writeLines(c(paste0(
        format(Sys.Date(), "%Y-%m-%d"),
        " - Collection assembled by ",
        "[Metacheck](https://www.scienceverse.org/metacheck/). CHANGES.md ",
        "added to log the version history of the collection.")),
        changes_dest)
  }
  # collection.json itself is written near the end, once the full text and logs
  # exist, so it can index them as parts too (see below).

  # ── Write identified scales as OpenScales OSD files ─────────────────────────
  # One .osd per NAMED scale, flat at scales/{code}.osd, plus a section in the
  # dataset-root README explaining the provenance markers. Unnamed detections are
  # skipped (they are not scales). See .scales_to_osd() / .osd_write_scales().
  #
  # Trial-level PARADATA (response times, trial/stimulus channels from Behaverse /
  # Inquisit / E-Prime / jsPsych source files) is normalised to Behaverse `trial`
  # documents at paradata/<instrument>.json (see R/behaverse-convert.R) — one file
  # per instrument, the full response data, nothing deleted. The OSD and the
  # paradata file cross-reference each other on the canonical instrument id, so we
  # pre-scan the paradata instrument keys, write the OSDs (which embed the link),
  # then write the paradata (linking back to the matching OSDs).
  paradata_keys <- .bh_paradata_keys(output_dir)
  osd_codes     <- .osd_write_scales(scales_osd, output_dir, structure_df,
                                     paradata_keys = paradata_keys)
  n_scales_written <- attr(osd_codes, "n_written") %||% 0L
  paradata_index <- .osd_write_paradata(output_dir, osd_codes = as.character(osd_codes),
                                        study_name = pid)

  # ── Write the paper's full text into documentation/ (always at the root) ─────
  # The release should carry the manuscript prose the checks read, not just the
  # data. multi_study is true when the plan produced study-<group>/ subtrees, in
  # which case the shared root documentation/ file is pid-qualified.
  fulltext_path <- .psychds_write_paper_text(
    paper, output_dir, pid, multi_study = length(planned_dirs) > 0)

  # ── Write the provenance logs/ folder (manifest, checks, tables) ─────────────
  # Fold the previously-separate manifest / checks / tables artifacts into the
  # conversion: the manifest is always written (full one reused when data_check
  # produced it, else reduced), the checks/tables are written when the modules
  # that generate them ran. Each written file is explained in the root README.
  logs_written <- .psychds_write_logs(ops, structure_df, pid, output_dir,
                                      all_ops = all_ops)
  .psychds_write_logs_readme(logs_written, output_dir,
                             file.path(output_dir, "README.md"))

  # ── Multi-study collection metadata (collection.json) ───────────────────────
  # Written last so it can index every part that exists: each study dataset, the
  # root-level shared files (codebooks/materials/documentation), the paper full
  # text, and the logs. Not named dataset_description.json, so the Psych-DS
  # validator ignores it. Single-study (flat) datasets get no collection.json —
  # their root dataset_description.json already describes them.
  collection_path <- NULL
  if (length(planned_dirs) > 0) {
    # Paths of the extra generated artifacts, relative to the output root.
    root_norm <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    to_rel <- function(p) {
      if (is.null(p) || !length(p)) return(character(0))
      pn <- normalizePath(p, winslash = "/", mustWork = FALSE)
      ifelse(startsWith(pn, paste0(root_norm, "/")),
             substring(pn, nchar(root_norm) + 2L), pn)
    }
    collection_path <- .psychds_collection_json(
      paper, output_dir, pid, study_roots,
      columns_df = columns_df, labels_df = labels_df,
      shared_files = grep("^study-", plan$target_path, value = TRUE,
                          invert = TRUE),
      fulltext_rel = to_rel(fulltext_path),
      logs_rel     = to_rel(names(logs_written)),
      paradata     = paradata_index)
  }

  # Explain the files that were NOT placed in the dataset. A file is not added
  # when it was not downloaded — which can be intentional (an asset the release
  # links to rather than mirrors, via `skip_types`), or because `download` was
  # not `"all"`, or a size cap skipped it. The converter only sees "not on disk",
  # so it states the fact and points to the manifest for the per-file reason,
  # rather than prescribing a single (possibly wrong) fix. The breakdown by type
  # usually makes the cause obvious (e.g. all assets => a deliberate skip_types).
  if (length(skipped) > 0) {
    n_total <- nrow(plan)
    types   <- tolower(plan$data_type[skipped_i] %||% "other")
    types[is.na(types) | !nzchar(types)] <- "other"
    by_type <- sort(table(types), decreasing = TRUE)
    breakdown <- paste(sprintf("%d %s", by_type, names(by_type)), collapse = ", ")
    only_assets <- length(by_type) == 1 && names(by_type)[1] == "asset"
    hint <- if (only_assets)
      paste0("These are assets (stimuli/media); if you excluded them on purpose ",
             "(skip_types = \"asset\") the release should link to them instead of ",
             "hosting them.")
    else
      paste0("Files are only added when downloaded. Run data_check with ",
             "download = \"all\" (and without skip_types) to include more; a ",
             "per-file reason is recorded in the data_check manifest.")
    message(
      sprintf(paste0(
        "%d of %d repository file%s %s not added to the Psych-DS dataset ",
        "(not downloaded; by type: %s).\n  %s"),
        length(skipped), n_total, plural(length(skipped)),
        if (length(skipped) == 1) "was" else "were", breakdown, hint))
  }

  if (length(copy_failed) > 0)
    message(
      sprintf("%d file%s could not be copied despite being downloaded ",
              length(copy_failed), plural(length(copy_failed))),
      "(a read/write error): ",
      paste(utils::head(copy_failed, 5), collapse = ", "),
      if (length(copy_failed) > 5) ", ..." else "")

  message("Wrote Psych-DS ",
          if (!is.null(collection_path)) "collection" else "dataset",
          " to ", normalizePath(output_dir, mustWork = FALSE),
          " (", n_copied, " file(s), ", length(descriptions),
          " dataset description(s)",
          if (!is.null(collection_path)) ", collection.json" else "",
          if (n_scales_written > 0) paste0(", ", n_scales_written, " scale(s)") else "",
          if (length(paradata_index) > 0)
            paste0(", ", length(paradata_index), " paradata file(s)") else "",
          if (!is.null(fulltext_path)) ", paper full text" else "",
          if (length(logs_written) > 0)
            paste0(", ", length(logs_written), " log file(s)") else "",
          ").\n")
  if (!is.null(collection_path))
    message("  The collection root is not itself a Psych-DS dataset; ",
            "validate each study-*/ folder (see collection.json / README).")

  invisible(list(
    output_dir     = output_dir,
    n_files_copied = n_copied,
    n_studies      = length(study_roots),
    descriptions   = descriptions,
    collection     = collection_path %||% character(0),
    n_scales       = n_scales_written,
    n_paradata     = length(paradata_index),
    fulltext       = fulltext_path %||% character(0),
    logs           = names(logs_written),
    skipped        = skipped,
    copy_failed    = copy_failed
  ))
}

# Write the identified scales as OpenScales OSD files under output_dir/scales/,
# one file per named scale, flat at scales/{code}.osd, and append a
# provenance-explaining section to the dataset-root README.md. Objects flagged
# `write = FALSE` (unnamed detections) are skipped. Returns the number written.
.osd_write_scales <- function(scales_osd, output_dir, structure_df = NULL,
                              paradata_keys = character(0)) {
  if (is.null(scales_osd) || !length(scales_osd))
    return(structure(character(0), n_written = 0L))
  writeable <- Filter(function(o) isTRUE(attr(o, "write")), scales_osd)
  if (!length(writeable)) return(structure(character(0), n_written = 0L))

  index <- list()   # rows for the README: code, name, source, provenance
  used <- character(0)   # codes already written, to disambiguate collisions
  for (osd in writeable) {
    code <- attr(osd, "code") %||% "SCALE"
    # Disambiguate a repeated code so a later scale does not overwrite an earlier
    # one at scales/{code}.osd. This is common for unnamed blocks, where several
    # non-adjacent same-prefix runs (e.g. three "response" blocks) all slug to the
    # same name. Suffix -2, -3, ... on collision, and keep the OSD's own
    # scale_info$code in sync with the path it is written to.
    if (code %in% used) {
      i <- 2L
      while (paste0(code, "-", i) %in% used) i <- i + 1L
      code <- paste0(code, "-", i)
      osd$definition$scale_info$code <- code
    }
    used <- c(used, code)
    # Attach the online download URL of each source data file next to the scale's
    # existing source_files list, so a reader of the .osd can jump to the data the
    # scale was extracted from. Same file_url-then-repo_url rule as the dataset
    # distribution. Parallel to source_files; omitted when no URL resolves.
    src_files <- osd$definition$metacheck$source_files
    if (!is.null(src_files) && length(src_files)) {
      urls <- vapply(unlist(src_files, use.names = FALSE),
                     function(fn) .psychds_source_url(fn, structure_df) %||% NA_character_,
                     character(1))
      urls <- urls[!is.na(urls) & nzchar(urls)]
      if (length(urls))
        osd$definition$metacheck$source_urls <- as.list(unname(urls))
    }
    # Files live flat, next to each other: scales/<code>.osd (no per-scale
    # subfolder). Guard against over-long paths (OneDrive/Windows' ~260-char
    # limit); .safe_write_path shortens + warns.
    osd_path <- .safe_write_path(file.path(output_dir, "scales",
                                           paste0(code, ".osd")))
    # Cross-reference to the instrument's Behaverse paradata file, when trial-
    # level paradata for this instrument was (or will be) written. The join key is
    # the canonical instrument id; the OSD points at ../paradata/<key>.json and the
    # Behaverse Instrument.link points back here. Emitted as namespaced keys the
    # OSD spec ignores (same mechanism as metacheck:reference_item).
    key <- .bh_instrument_key(code)
    if (key %in% paradata_keys) {
      osd$definition$scale_info[["metacheck:behaverse_instrument_id"]] <- key
      osd$definition$scale_info[["metacheck:paradata"]] <-
        paste0("../paradata/", key, ".json")
    }

    dir.create(dirname(osd_path), recursive = TRUE, showWarnings = FALSE)
    json <- jsonlite::toJSON(osd, auto_unbox = TRUE, pretty = TRUE, null = "null")
    writeLines(json, osd_path, useBytes = TRUE)
    mc <- osd$definition$metacheck %||% list()
    index[[length(index) + 1L]] <- list(
      code = code,
      name = osd$definition$scale_info$name %||% "",
      source = mc$scale_source %||% "",
      provenance = mc$provenance %||% "")
  }

  .osd_write_readme_section(index, file.path(output_dir, "README.md"))
  # The canonical instrument keys of the scales written, so paradata linking uses
  # exactly the same key set.
  attr(used, "n_written") <- length(writeable)
  used
}

# Append (or create) a "Psychometric scales" section to the dataset-root README,
# listing each written scale and explaining the three provenance markers so the
# archive is not mistaken for an authoritative registry.
.osd_write_readme_section <- function(index, readme_path) {
  if (!length(index)) return(invisible())
  lines <- c(
    "",
    "## Psychometric scales",
    "",
    paste0("metacheck identified ", length(index), " psychometric scale",
           plural(length(index)),
           " in this dataset and exported each as an OpenScales OSD file under ",
           "`scales/{code}.osd`. This is an archive of what metacheck *found*, ",
           "not an authoritative scale registry. The **Provenance** column below ",
           "marks how confidently each scale was identified:"),
    "",
    "- **dictionary match** — matched a known instrument in metacheck's scale dictionary (OpenScales-derived or curated).",
    "- **named in manuscript** — a named instrument identified from the manuscript text, not in the dictionary.",
    "- **metacheck-generated label** — a construct label *generated by metacheck* from the item wording. This is **not** a recognised named instrument, only metacheck's inference of what the items measure. Treat it as a starting point, not a definition.",
    "- **detected block, unnamed** — a coherent block of same-prefix rating columns that metacheck could **not** name. Recorded for its structure (its items and response scale) only.",
    "",
    "| Code | Scale | Provenance |",
    "|------|-------|------------|")
  rows <- vapply(index, function(r) sprintf("| `%s` | %s | %s |",
    r$code, if (nzchar(r$name)) r$name else "(unnamed)",
    switch(r$source,
      dictionary = "dictionary match",
      manuscript = "named in manuscript",
      self_generated = "metacheck-generated label",
      unnamed_block = "detected block, unnamed",
      r$source)), character(1))

  con_lines <- if (file.exists(readme_path))
    readLines(readme_path, warn = FALSE) else character(0)
  writeLines(c(con_lines, lines, rows, ""), readme_path, useBytes = TRUE)
  invisible()
}
