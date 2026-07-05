# Generate the inputs for a human-readable codebook. This is a companion to
# `convert_psychds`: where that function writes a machine-readable Psych-DS
# dataset (dataset_description.json + variableMeasured), this one prepares a
# fully-*labelled* data frame — columns carry variable labels and value labels
# as attributes, and the data frame carries dataset-level metadata — so the user
# can, as an optional next step, render a rich codebook with the `codebook`
# package (https://github.com/rubenarslan/codebook) without metacheck taking on
# rmarkdown/pandoc/codebook as hard dependencies.
#
# The labelled data frame is built from the same data_check / codebook_check
# outputs convert_psychds uses, so the two stay consistent. We always write the
# .rds + a ready-to-run .Rmd + a JSON-LD metadata file (base R + our own
# helpers); we additionally render an HTML codebook when the `codebook` package
# and pandoc are both available.

# Attach variable/value labels and dataset-level metadata onto a data frame so
# the `codebook` package can consume it. `df` is the (real or zero-row) data,
# `cols` the matching subset of data_check's columns table, `labels` the
# codebook_check labels table (or NULL), `meta` the dataset-level metadata list.
.codebook_label_df <- function(df, cols, labels = NULL, meta = NULL) {
  # Attach labels by source_file + column_name when available.
  if (!is.null(labels) && nrow(labels) > 0 &&
      all(c("source_file", "column_name") %in% names(labels))) {
    keep <- c("source_file", "column_name", "label", "label_status",
              "scale", "scale_confidence")
    labels <- labels[, intersect(keep, names(labels)), drop = FALSE]
    cols <- merge(cols, labels, by = c("source_file", "column_name"),
                  all.x = TRUE, suffixes = c("", ".lbl"))
  }

  # Index the column metadata by name for per-column lookup.
  cols_by_name <- if (!is.null(cols) && nrow(cols) > 0)
    split(cols, cols$column_name) else list()

  for (nm in names(df)) {
    row <- cols_by_name[[nm]]
    if (is.null(row) || nrow(row) == 0) next
    row <- row[1, , drop = FALSE]

    # Variable label: only when codebook_check produced a usable one.
    if ("label_status" %in% names(row) && !is.na(row$label_status) &&
        row$label_status %in% c("labelled", "llm") &&
        !is.na(row$label) && nzchar(row$label)) {
      attr(df[[nm]], "label") <- as.character(row$label)
    }

    # Scale membership (from codebook_check's LLM scale identification): keep it
    # on the column so it is preserved in the saved labelled data frame and can
    # be reported / reused. The scale name is the item's measurement instrument.
    if ("scale" %in% names(row) && !is.na(row$scale) && nzchar(row$scale)) {
      attr(df[[nm]], "scale") <- as.character(row$scale)
      if ("scale_confidence" %in% names(row) && !is.na(row$scale_confidence))
        attr(df[[nm]], "scale_confidence") <- as.character(row$scale_confidence)
    }

    # We deliberately do NOT synthesise value labels from `sample_values`: those
    # are the raw observed values, so a label built from them equals the code
    # (e.g. 0 -> "0"), carrying no information. Worse, they are always character,
    # so on a numeric column the mismatch between a character `labels` attribute
    # and numeric data breaks the codebook package's plotting ("Discrete value
    # supplied to a continuous scale"). Genuine value labels already embedded in
    # the data (e.g. haven labels on an SPSS/Stata column) are correctly typed
    # and are preserved untouched below.
    #
    # Value labels codebook_check DECODED from a codebook (a real code->label
    # map, DDI CodeList) are different: they carry information. Attach them as a
    # haven-style `labels` attribute, typing the codes to match the column so the
    # codebook package renders them without the numeric/character clash above.
    if ("value_labels" %in% names(row) && !is.na(row$value_labels) &&
        nzchar(row$value_labels) && is.null(attr(df[[nm]], "labels"))) {
      vl <- .decode_value_labels(row$value_labels)   # names = codes, vals = labels
      if (!is.null(vl) && length(vl) > 0) {
        codes <- names(vl)
        if (is.numeric(df[[nm]])) {
          num <- suppressWarnings(as.numeric(codes))
          if (!any(is.na(num))) {
            lab_vec <- stats::setNames(num, unname(vl))
            attr(df[[nm]], "labels") <- lab_vec
          }
        } else {
          attr(df[[nm]], "labels") <- stats::setNames(codes, unname(vl))
        }
      }
    }
  }

  # Any embedded haven value labels already on real columns (reading a .sav/.dta
  # via haven leaves attr(x, "labels") in place) are left untouched.
  if (!is.null(meta)) attr(df, "metadata") <- meta
  df
}

# Build the dataset-level metadata list the `codebook` package reads from
# attr(df, "metadata"). Mirrors the schema.org fields .psychds_dataset_description
# emits, so the codebook metadata and the Psych-DS descriptor agree.
.codebook_metadata <- function(paper, study_label, var_names) {
  info <- paper$info %||% list()
  ival <- function(field) {
    v <- if (field %in% names(info)) info[[field]] else NULL
    if (length(v) == 0) NULL else v
  }

  title <- ival("title")
  name <- if (!is.null(title) && nzchar(title))
    paste0(title, if (nzchar(study_label)) paste0(" — ", study_label) else "")
  else paste0("Dataset", if (nzchar(study_label)) paste0(" — ", study_label) else "")

  meta <- list(
    name        = name,
    description = paste0("Codebook prepared by metacheck",
                        if (nzchar(study_label)) paste0(" for ", study_label) else "",
                        "."),
    datePublished = format(Sys.Date(), "%Y-%m-%d")
  )

  if (!is.null(paper$author) && nrow(paper$author) > 0) {
    nm <- trimws(paste(paper$author$given %||% "", paper$author$family %||% ""))
    nm <- nm[nzchar(nm)]
    if (length(nm) > 0)
      meta[["creator"]] <- lapply(nm, function(x) list(`@type` = "Person", name = x))
  }
  doi <- ival("doi") %||% NA_character_
  if (!is.na(doi) && nzchar(doi))
    meta[["url"]] <- paste0("https://doi.org/", sub("^https?://doi.org/", "", doi))
  kw <- ival("keywords")
  if (!is.null(kw) && length(kw) > 0)
    meta[["keywords"]] <- as.list(kw)

  Filter(Negate(is.null), meta)
}

# Build a dataset-level list of identified scales for the codebook metadata,
# from codebook_check's labels table restricted to the columns in this study.
# One entry per scale: its name and the member variables (mirroring DDI's
# `varGrp` membership; see the manual's discussion of scale representation).
# Returns NULL when no scale was identified.
.codebook_scales <- function(labels, var_names) {
  if (is.null(labels) || nrow(labels) == 0 ||
      !all(c("column_name", "scale") %in% names(labels))) return(NULL)
  keep <- labels[labels$column_name %in% var_names &
                   !is.na(labels$scale) & nzchar(labels$scale), , drop = FALSE]
  if (nrow(keep) == 0) return(NULL)
  scales <- unique(keep$scale)
  lapply(scales, function(s) {
    vars <- unique(keep$column_name[keep$scale == s])
    conf <- keep$scale_confidence[keep$scale == s][1]
    Filter(Negate(is.null), list(
      name       = s,
      confidence = if (!is.null(conf) && !is.na(conf)) conf else NULL,
      variables  = as.list(vars)
    ))
  })
}

# Serialize a metadata list + column names as a schema.org Dataset JSON-LD, so a
# machine-readable codebook exists even without the `codebook` package. When a
# variable was matched to an identified scale, its name goes in schema.org's
# native `measurementTechnique` (as in the Psych-DS output). `scale_of` maps a
# variable name to its scale name (or returns NA).
.codebook_write_jsonld <- function(meta, var_names, path, scale_of = NULL) {
  obj <- c(
    list(`@context` = "https://schema.org/", `@type` = "Dataset"),
    meta,
    list(variableMeasured = lapply(var_names, function(v) {
      pv <- list(`@type` = "PropertyValue", name = v)
      sc <- if (is.function(scale_of)) scale_of(v) else NA_character_
      if (!is.null(sc) && !is.na(sc) && nzchar(sc)) pv[["measurementTechnique"]] <- sc
      pv
    }))
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, na = "null")
  writeLines(json, path, useBytes = TRUE)
  invisible(path)
}

# The one-line-to-run codebook Rmd template. Loads the saved labelled data frame
# and calls codebook::codebook(). Kept minimal so a user can edit it freely.
.codebook_rmd_template <- function(rds_name, title) {
  c(
    "---",
    sprintf('title: "%s"', gsub('"', "'", title)),
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    # Column names that differ only in special characters (e.g. "t'" vs a
    # t-with-diacritic) sanitize to the same section id, and pandoc then warns
    # "Duplicate identifier" once per section — hundreds of lines on a wide
    # dataset. The ids still work (links go to the first occurrence);
    # data_validate flags the colliding names themselves, so the render need
    # not repeat the warning.
    '    pandoc_args: ["--quiet"]',
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(warning = FALSE, message = FALSE)",
    "library(codebook)",
    "```",
    "",
    "```{r codebook, results='asis'}",
    "# Labelled data frame prepared by metacheck::convert_codebook(): variable",
    "# labels and dataset metadata are attached as attributes, which codebook()",
    "# reads directly.",
    sprintf('codebook_data <- readRDS("%s")', rds_name),
    "codebook(codebook_data)",
    "```",
    "",
    "---",
    "",
    "This codebook was generated with the",
    "[**codebook**](https://rubenarslan.github.io/codebook/) R package. If you",
    "use it, please cite:",
    "",
    "> Arslan, R. C. (2019). How to automatically document data with the codebook",
    "> package to facilitate data re-use. *Advances in Methods and Practices in",
    "> Psychological Science*, *2*(2), 169–187.",
    "> <https://doi.org/10.1177/2515245919838783>",
    "",
    "The labelled data frame was prepared by",
    "[metacheck](https://scienceverse.github.io/metacheck/)."
  )
}

#' Prepare a human-readable codebook for a repository
#'
#' Builds the inputs for a rich, human-readable codebook of a paper's data
#' repository and writes them to a side folder, so the codebook can be dropped
#' into the repository (or published) without changing the data itself. This is
#' the companion of [convert_psychds()]: that function writes the *machine*
#' readable Psych-DS dataset; this one writes a fully **labelled** data frame
#' plus a ready-to-run R Markdown document for the
#' [codebook](https://github.com/rubenarslan/codebook) package.
#'
#' For each study the function assembles a data frame whose columns carry the
#' variable labels matched by [codebook_check()] (plus any value labels already
#' embedded in the data, e.g. from SPSS/Stata files), and whose dataset-level
#' `metadata` attribute holds the title, description, authors, DOI and keywords
#' from the paper. It writes:
#'
#' * `codebook_data.rds` — the labelled data frame (real data rows when a local
#'   copy of the file is available, otherwise a zero-row frame carrying only the
#'   attributes),
#' * `codebook.Rmd` — a one-line-to-run template that loads the `.rds` and calls
#'   `codebook::codebook()`,
#' * `codebook_metadata.json` — schema.org Dataset JSON-LD (written with base
#'   tools, so it exists even without the `codebook` package).
#'
#' When the `codebook` package **and** pandoc are both installed, the `.Rmd` is
#' additionally rendered to `codebook.html`. Otherwise that step is skipped with
#' a message; the `.rds` + `.Rmd` let the user render it later. metacheck itself
#' does not depend on `codebook` or rmarkdown — they are only needed for the
#' optional HTML render.
#'
#' Note this does not produce a Psych-DS `dataset_description.json`; the JSON-LD
#' here is discoverability metadata for the codebook, not a Psych-DS descriptor.
#' Use [convert_psychds()] for a validation-passing dataset.
#'
#' @param paper a paper object (see [read_paper()]), **or** a captured result of
#'   `report(paper, ...)` / `report_module_run(paper, ...)`. When a captured
#'   result containing both `data_check` and `codebook_check` is passed, those
#'   outputs are reused (with the paper recovered from the result) instead of
#'   re-running; otherwise the modules are run.
#' @param output_dir directory to write the codebook inputs into; created if
#'   needed. Defaults to `"codebook/<paper_id>"` under the working directory.
#' @param render whether to render `codebook.html` when the `codebook` package
#'   and pandoc are available (`TRUE`, the default). When they are not, the
#'   render is skipped regardless.
#' @param refresh_osf whether to fetch a fresh OSF file listing (see
#'   [convert_psychds()]); the default (`FALSE`) reuses the session's listing.
#' @param local_path,local_only passed to `data_check` when its output is not
#'   already available (see [data_check()])
#' @param model,params passed to the underlying modules when `llm_use(TRUE)`
#' @param overwrite whether to overwrite an existing `output_dir`. When `FALSE`
#'   (the default) and `output_dir` already exists, the function messages and
#'   skips rather than erroring (the returned list has `existed = TRUE`).
#'
#' @returns (invisibly) a list with `output_dir`, `n_studies`, `rds_files`,
#'   `rmd_files`, `metadata_files`, `html_files` (the paths written), and
#'   `rendered` (whether any HTML was produced). When an existing `output_dir`
#'   was skipped, the list additionally contains `existed = TRUE`.
#' @seealso [convert_psychds()] for the machine-readable Psych-DS dataset.
#' @export
#' @examples
#' \dontrun{
#' # Capture the checks, then build the codebook (modules reused, not re-run):
#' res <- report(paper, c("data_check", "codebook_check"))
#' convert_codebook(res)
#'
#' # Or call directly on the paper:
#' convert_codebook(paper)
#'
#' # then, if not auto-rendered:
#' rmarkdown::render("codebook/<paper_id>/codebook.Rmd")
#' }
convert_codebook <- function(paper, output_dir = NULL, render = TRUE,
                             refresh_osf = FALSE,
                             local_path = NULL, local_only = FALSE,
                             model = llm_model(), params = list(),
                             overwrite = FALSE) {
  # Reuse the session's cached OSF listing unless a fresh one is requested.
  old_osf_cache <- getOption("metacheck.osf.cache", TRUE)
  options(metacheck.osf.cache = !isTRUE(refresh_osf))
  on.exit(options(metacheck.osf.cache = old_osf_cache), add = TRUE)

  # Reuse whatever the checks already computed (when `paper` is a captured
  # report()/report_module_run() result); otherwise run the chain.
  needed <- c("data_check", "codebook_check")
  resolved <- .converter_resolve(paper, needed,
                                 local_path = local_path, local_only = local_only,
                                 model = model, params = params)
  ops   <- resolved$ops
  paper <- resolved$paper
  dc <- ops[["data_check"]]
  columns_df <- dc$table
  structure_df <- dc$structure
  previews <- dc$previews %||% list()
  labels_df <- ops[["codebook_check"]]$table

  if (is.null(columns_df) || nrow(columns_df) == 0)
    stop("No extracted data columns to build a codebook from. Run data_check first.",
         call. = FALSE)

  pid <- resolved$pid
  if (is.null(output_dir)) output_dir <- file.path("codebook", pid)

  if (dir.exists(output_dir) && !overwrite) {
    message("Codebook output already exists, skipping: ", output_dir,
            ". Set overwrite = TRUE to replace it.")
    return(invisible(list(
      output_dir = output_dir, n_studies = 0L, rds_files = character(0),
      rmd_files = character(0), metadata_files = character(0),
      html_files = character(0), rendered = FALSE, existed = TRUE
    )))
  }
  if (dir.exists(output_dir) && overwrite) unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Local file paths, for reading real data rows when previews are unavailable.
  loc <- if (!is.null(structure_df))
    stats::setNames(structure_df$file_location, structure_df$file_name) else list()

  # Read the real rows for one source file: prefer data_check's cached preview
  # (already a full read), fall back to reading the cached copy, else NULL.
  read_rows <- function(source_file) {
    if (!is.null(previews[[source_file]])) return(previews[[source_file]])
    p <- loc[[source_file]]
    if (is.null(p) || is.na(p) || !nzchar(p) || !file.exists(p)) return(NULL)
    tryCatch(data_read_head(p, n_rows = Inf), error = function(e) NULL)
  }

  # ── One labelled data frame per study group (flat when no groups) ────────────
  groups <- if ("group" %in% names(columns_df)) columns_df$group else
    rep(NA_character_, nrow(columns_df))
  study_groups <- unique(groups[!is.na(groups) & groups != "shared"])
  multi_study  <- length(study_groups) > 1
  roots <- if (multi_study) study_groups else ""

  rds_files <- rmd_files <- meta_files <- html_files <- character(0)
  rendered <- FALSE

  can_render <- isTRUE(render) &&
    requireNamespace("codebook", quietly = TRUE) &&
    requireNamespace("rmarkdown", quietly = TRUE) &&
    rmarkdown::pandoc_available()

  for (grp in roots) {
    if (nzchar(grp)) {
      root_cols <- columns_df[!is.na(groups) & groups == grp, , drop = FALSE]
      sub_dir <- file.path(output_dir, paste0("study-", grp))
      study_label <- paste("Study", toupper(grp))
    } else {
      root_cols <- columns_df
      sub_dir <- output_dir
      study_label <- ""
    }
    if (nrow(root_cols) == 0) next
    dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)

    # Assemble the data frame: real rows per source file, aligned by column name.
    # Columns from different files sit side by side; unequal lengths are padded
    # to the longest so a single data frame holds every documented column.
    src_files <- unique(root_cols$source_file)
    col_data <- list()
    for (sf in src_files) {
      df <- read_rows(sf)
      sf_cols <- root_cols$column_name[root_cols$source_file == sf]
      for (cn in sf_cols) {
        col_data[[cn]] <- if (!is.null(df) && cn %in% names(df)) df[[cn]] else
          logical(0)
      }
    }
    n_max <- max(c(0L, vapply(col_data, length, integer(1))))
    if (n_max > 0) {
      col_data <- lapply(col_data, function(x)
        if (length(x) < n_max) c(x, rep(NA, n_max - length(x))) else x)
    }
    cb_df <- if (length(col_data) > 0)
      as.data.frame(col_data, check.names = FALSE, optional = TRUE) else
      data.frame()

    # Drop columns with no observed values (all NA). These arise from blank
    # spreadsheet headers or columns absent from a merged file; they carry no
    # codebook information and the codebook package cannot summarise them
    # (median(table(x)) on an empty table is NA -> "missing value where
    # TRUE/FALSE needed").
    if (ncol(cb_df) > 0) {
      keep_col <- vapply(cb_df, function(x) any(!is.na(x)), logical(1))
      cb_df <- cb_df[, keep_col, drop = FALSE]
    }

    # Drop columns whose names would break the codebook's figure files. The
    # codebook package writes one figure per variable to
    # <sub_dir>/codebook_files/figure-html/cb_codebook_data_<name>_distribution-<n>-1.png,
    # where <name> is the column name with every non-alphanumeric character
    # replaced by "_" (codebook:::safe_name, via rmdpartials::partial). That
    # sanitization is one-to-one, so file-illegal characters never reach png()
    # — but the name's LENGTH survives, and when the absolute figure path
    # passes Windows' 260-character limit png() cannot create the file and the
    # whole render aborts. The room left for a name depends on how deep the
    # output dir sits, so the budget is computed from the actual path rather
    # than a fixed constant. We deliberately skip rather than truncate —
    # data_validate flags these names ("Problematic column name") so the
    # researcher is warned to rename them in the source data.
    if (ncol(cb_df) > 0) {
      fig_dir <- file.path(normalizePath(sub_dir, mustWork = FALSE),
                           "codebook_files", "figure-html")
      name_budget <- 259L - nchar(fig_dir, type = "bytes") -
        nchar("/cb_codebook_data__distribution-9999-1.png")
      bad <- vapply(names(cb_df), function(nm)
        nchar(nm, type = "bytes") > name_budget, logical(1))
      if (any(bad)) {
        shown <- utils::head(names(cb_df)[bad], 3)
        shown <- sprintf('"%s%s"', substr(shown, 1, 40),
                         ifelse(nchar(shown) > 40, "...", ""))
        message(sum(bad), " column", plural(sum(bad)),
                " excluded from the codebook: the name", plural(sum(bad)),
                " (over ", name_budget, " characters) would push the",
                " codebook's figure file paths past Windows' 260-character",
                " limit (", paste(shown, collapse = ", "),
                "). See data_validate's 'Problematic column name' check;",
                " rename the column", plural(sum(bad)), " in the source data.")
        cb_df <- cb_df[, !bad, drop = FALSE]
      }
    }

    var_names <- names(cb_df)
    meta <- .codebook_metadata(paper, study_label, var_names)
    scales <- .codebook_scales(labels_df, var_names)
    if (!is.null(scales)) meta[["metacheck:scales"]] <- scales
    cb_df <- .codebook_label_df(cb_df, root_cols, labels_df, meta)

    # Write the artefacts for this study root.
    rds_path <- file.path(sub_dir, "codebook_data.rds")
    saveRDS(cb_df, rds_path)
    rds_files <- c(rds_files, rds_path)

    rmd_path <- file.path(sub_dir, "codebook.Rmd")
    writeLines(.codebook_rmd_template("codebook_data.rds", meta$name), rmd_path)
    rmd_files <- c(rmd_files, rmd_path)

    json_path <- file.path(sub_dir, "codebook_metadata.json")
    scale_of <- local({
      ld <- labels_df
      function(v) {
        if (is.null(ld) || !all(c("column_name", "scale") %in% names(ld)))
          return(NA_character_)
        s <- ld$scale[ld$column_name == v & !is.na(ld$scale)]
        if (length(s) > 0) s[[1]] else NA_character_
      }
    })
    .codebook_write_jsonld(meta, var_names, json_path, scale_of = scale_of)
    meta_files <- c(meta_files, json_path)

    if (can_render) {
      # The codebook package computes a distribution/plot for every variable, so
      # a wide dataset can take minutes with no console output. Announce the start
      # (with the variable count, which drives the time) and report elapsed time
      # on completion, so a long silent render is not mistaken for a hang.
      grp_label <- if (length(roots) > 1) paste0(" [", grp, "]") else ""
      message("Rendering codebook HTML", grp_label, " (", length(var_names),
              " variables) - this can take a few minutes ...")
      t0 <- Sys.time()
      html_out <- tryCatch(
        rmarkdown::render(rmd_path, output_file = "codebook.html",
                          quiet = TRUE, envir = new.env()),
        error = function(e) {message("Codebook render failed: ", conditionMessage(e)); NULL})
      if (!is.null(html_out) && file.exists(html_out)) {
        html_files <- c(html_files, html_out)
        rendered <- TRUE
        message("  done in ",
                format(round(difftime(Sys.time(), t0, units = "secs"), 1)), ".")
      }
    }
  }

  if (!can_render && isTRUE(render))
    message("Skipped HTML render: install the 'codebook' package and pandoc, ",
            "then run rmarkdown::render() on the generated codebook.Rmd.")

  message("Wrote codebook inputs to ", normalizePath(output_dir, mustWork = FALSE),
          " (", length(rds_files), " labelled data frame(s)",
          if (rendered) paste0(", ", length(html_files), " HTML codebook(s)") else "",
          ").\n")

  invisible(list(
    output_dir     = output_dir,
    n_studies      = length(rds_files),
    rds_files      = rds_files,
    rmd_files      = rmd_files,
    metadata_files = meta_files,
    html_files     = html_files,
    rendered       = rendered
  ))
}
