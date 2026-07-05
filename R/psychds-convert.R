# Generate a Psych-DS-compliant copy of a repository. This is the file-writing
# counterpart to the `psychds_check` module: the check reports the compliance
# gap, this function fixes it on disk. Ported from datacheck's
# 3_psychds_convert.R, but driven by metacheck's data_check / codebook_check
# outputs and the paper object (no GROBID XML / CrossRef enrichment).

# Representation that receives a numeric statistics block in variableMeasured.
.psychds_numeric_reps <- c("numeric")
# Measurement levels that receive a valuePattern (their distinct values matter).
.psychds_categorical_levels <- c("nominal", "ordinal")
# Quality states that get no variableMeasured entry at all (no measured content).
.psychds_excluded_quality <- c("empty")

# Build the variableMeasured list for one set of columns. `cols` is a subset of
# data_check's columns table; `labels` is codebook_check's labels table (or
# NULL). Returns a list of PropertyValue objects.
.psychds_variable_measured <- function(cols, labels = NULL) {
  if (is.null(cols) || nrow(cols) == 0) return(list())

  # Attach labels by source_file + column_name when available.
  if (!is.null(labels) && nrow(labels) > 0 &&
      all(c("source_file", "column_name") %in% names(labels))) {
    keep <- c("source_file", "column_name", "label", "label_status",
              "label_source", "label_method", "codebook_variable",
              "scale", "scale_confidence",
              "value_labels", "missing_values", "question", "universe")
    labels <- labels[, intersect(keep, names(labels)), drop = FALSE]
    cols <- merge(cols, labels, by = c("source_file", "column_name"),
                  all.x = TRUE, suffixes = c("", ".lbl"))
  }

  # Drop columns with no measured content (empty). The facet schema
  # (representation/quality/measurement_level/concept/unit) is expected.
  qual <- tolower(cols$quality %||% "")
  cols <- cols[!(qual %in% .psychds_excluded_quality), , drop = FALSE]
  if (nrow(cols) == 0) return(list())

  lapply(seq_len(nrow(cols)), function(i) {
    row <- cols[i, ]
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
      pv[["metacheck:scale"]] <- Filter(Negate(is.null), list(
        name       = row$scale,
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

# Build the dataset_description.json object for one study.
.psychds_dataset_description <- function(paper, study_label, property_values) {
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
  kw <- ival("keywords")
  if (!is.null(kw) && length(kw) > 0)
    desc[["keywords"]] <- as.list(kw)

  desc[["metacheck:generated"]] <- format(Sys.Date(), "%Y-%m-%d")
  Filter(Negate(is.null), desc)
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
    reuse <- stats::setNames(list(chain), chain$module %||% "unknown")
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

  list(ops = ops, paper = real_paper, pid = pid)
}

.psychds_write_json <- function(obj, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, na = "null")
  writeLines(json, path, useBytes = TRUE)
  invisible(path)
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
#' self-contained Psych-DS dataset, plus a `study-shared/` directory. Original
#' files whose contents cannot be read (no local copy) are skipped with a note.
#'
#' @param paper a paper object (see [read_paper()]), **or** a captured result of
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
#'   already available (see [data_check()])
#' @param model,params passed to the underlying modules when `llm_use(TRUE)`
#' @param overwrite whether to overwrite an existing `output_dir`. When `FALSE`
#'   (the default) and `output_dir` already exists, the function messages and
#'   skips rather than erroring (the returned list has `existed = TRUE`).
#'
#' @returns (invisibly) a list with `output_dir`, `n_files_copied`,
#'   `n_studies`, and `descriptions` (paths of written dataset_description.json
#'   files). When an existing `output_dir` was skipped, the list additionally
#'   contains `existed = TRUE` and the counts are zero.
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
  ops   <- resolved$ops
  paper <- resolved$paper
  dc <- ops[["data_check"]]
  structure_df <- dc$structure
  columns_df   <- dc$table
  labels_df <- ops[["codebook_check"]]$table
  plan      <- ops[["psychds_check"]]$table

  if (is.null(plan) || nrow(plan) == 0)
    stop("No files to convert: psychds_check returned an empty plan.", call. = FALSE)

  pid <- resolved$pid
  if (is.null(output_dir)) output_dir <- file.path("psychds", pid)

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

  # A local copy is needed to copy the file's bytes.
  loc <- setNames(structure_df$file_location, structure_df$file_name)

  # ── Copy files to their target locations ────────────────────────────────────
  n_copied    <- 0L
  skipped     <- character(0) # files not on disk (never downloaded)
  skipped_i   <- integer(0)   # their plan-row indices, to group by type below
  copy_failed <- character(0) # files on disk that failed to copy (I/O error)
  for (i in seq_len(nrow(plan))) {
    src <- loc[[plan$file_name[i]]]
    if (is.null(src) || is.na(src) || !nzchar(src) || !file.exists(src)) {
      skipped   <- c(skipped, plan$file_name[i])
      skipped_i <- c(skipped_i, i)
      next
    }
    dest <- file.path(output_dir, plan$target_path[i])
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    # Data CSVs must be BOM-free: a UTF-8 BOM makes the first column header read
    # as "﻿id", which then mismatches variableMeasured (a Psych-DS error).
    if (grepl("\\.csv$", dest, ignore.case = TRUE) &&
        grepl("^(study-[^/]+/)?data/", plan$target_path[i])) {
      ok <- .psychds_copy_no_bom(src, dest)
      if (ok) n_copied <- n_copied + 1L
      else copy_failed <- c(copy_failed, plan$file_name[i])
    } else if (file.copy(src, dest, overwrite = TRUE)) {
      n_copied <- n_copied + 1L
    } else {
      copy_failed <- c(copy_failed, plan$file_name[i])
    }
  }

  # ── Study roots: derive from the target paths' study-<group>/ prefixes ───────
  study_dirs <- unique(sub("^(study-[^/]+)/.*$", "\\1",
                           grep("^study-", plan$target_path, value = TRUE)))
  study_roots <- if (length(study_dirs) > 0) study_dirs else ""

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
    desc <- .psychds_dataset_description(paper, study_label, pv)
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

  message("Wrote Psych-DS dataset to ", normalizePath(output_dir, mustWork = FALSE),
          " (", n_copied, " file(s), ", length(descriptions),
          " dataset description(s)).\n")

  invisible(list(
    output_dir     = output_dir,
    n_files_copied = n_copied,
    n_studies      = length(study_roots),
    descriptions   = descriptions,
    skipped        = skipped,
    copy_failed    = copy_failed
  ))
}
