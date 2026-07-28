# Serialise extracted JASP/jamovi result tables (from read_stat_tables()) two
# ways, from ONE extraction:
#   * stat_results_long()  -> a flat, queryable data.frame (one row per cell:
#     analysis / row label / statistic / STATO iri / value) for the scienceverse
#     SQLite DB, matching how other module output is flattened into it;
#   * stat_output_isa()    -> a complete, VALIDATED ISA-JSON document (one file =
#     one Sample; each analysis a Process; each result row a Material whose
#     characteristics are STATO-typed values) for the run logs.
# Column -> STATO typing is via stato_type_column() (R/stato-map.R): STATO IRI
# where a class exists, else the header text as a nominal label (never dropped).
#
# Every result table also gets a stable result_id, via .stat_result_ids(): the
# CODENAME (source_file) that produced it plus a locator —
#   * an executed R script: the source LINE the statement started on
#     (`#L<line>`), from read_r_output()'s echo-based line attribution;
#   * a JASP/jamovi extraction: the analysis heading (`#<analysis>`), since
#     there is no source line for a GUI-driven analysis.
# When several results share the same locator (a loop producing several
# results on one line; a heading repeated across tables), `_1`/`_2`/... is
# appended in table order so every id stays unique within its source file.

# One result_id per element of `tables` (read_stat_tables()/read_r_output()'s
# return shape), stamped in the SAME order those tables are walked elsewhere in
# this file (stat_results_long, stat_output_isa) so ids line up across both
# serialisations of one extraction.
.stat_result_ids <- function(tables) {
  locator <- vapply(tables, function(tb) {
    if (!is.null(tb$line) && !is.na(tb$line)) paste0("L", tb$line)
    else if (!is.null(tb$analysis) && !is.na(tb$analysis) && nzchar(tb$analysis))
      tb$analysis
    else "result"
  }, character(1))
  ave(locator, locator, FUN = function(x)
    if (length(x) == 1) x else paste0(x, "_", seq_along(x)))
}

# Which columns of a result table are STATISTICS (vs row-label / structural
# columns). JASP/jamovi lay tables out differently PER TEST — the label
# column(s) that key each row (test name, model, predictor, group, effect-size
# NAME) can be at the front, in the middle, or have an empty header, and there
# can be several. So classify by CONTENT, not header or position: a column is a
# row-LABEL when its non-empty cells are mostly non-numeric text; a statistic
# column holds (mostly) numbers OR has a header that maps to a known statistic.
# This adapts to every test automatically because it reads the actual column.
.stat_is_label_col <- function(header, values) {
  h <- tolower(trimws(header %||% ""))
  vals <- trimws(as.character(values %||% character(0)))
  vals <- vals[nzchar(vals)]
  # A header that types to a known statistic is a statistic column regardless of
  # content (e.g. a p column with "< .001" strings).
  if (nzchar(h) && !identical(stato_type_column(h)$termSource, ""))
    return(FALSE)
  # Content test: a value is "numeric-ish" if it parses as a number, is a
  # reported comparison (< .001), or an infinity — the forms statistics take.
  if (length(vals) == 0) return(TRUE)   # empty column -> treat as label/spacer
  numlike <- grepl("^[<>=]?\\s*[-+]?[0-9.]+([eE][-+]?[0-9]+)?$", vals) |
             grepl("(?i)^[-+]?inf$", vals, perl = TRUE) |
             grepl("^[<>]\\s*[.0-9]", vals)
  # Label column when fewer than half its cells look numeric.
  mean(numlike) < 0.5
}

#' Flatten extracted result tables into one queryable long data frame
#'
#' Turns the nested output of [read_stat_tables()] into a tidy long table: one
#' row per (analysis, table, result-row, statistic) cell, carrying the STATO
#' typing. This is the form stored in the scienceverse SQLite database, and the
#' basis for later matching of extracted results against the manuscript.
#'
#' @param tables the list returned by [read_stat_tables()] or [read_r_output()]
#' @param paper_id optional paper id / DOI to stamp on every row
#' @param source_file optional name of the file the tables were extracted from
#'   (e.g. the `.jasp`/`.omv` basename, or the script name for run-R output),
#'   recorded on every row as provenance for downstream matching, and as the
#'   `result_id` prefix (see below)
#'
#' @returns a data.frame with columns `paper_id`, `source_file`, `result_id`,
#'   `analysis`, `table_title`, `row_label`, `statistic`, `stato_label`,
#'   `stato_iri`, and `value`. `result_id` identifies the CODE that produced a
#'   result: `<source_file>#L<line>` for an executed R script (from
#'   [read_r_output()]'s echo-based line attribution), or
#'   `<source_file>#<analysis heading>` for a JASP/jamovi extraction (no source
#'   line exists there); `_1`/`_2`/... is appended when several results share
#'   one locator (e.g. a loop, or a repeated heading). Empty frame (same
#'   columns) when there is nothing to flatten.
#' @export
stat_results_long <- function(tables, paper_id = NA_character_,
                              source_file = NA_character_) {
  empty <- data.frame(paper_id = character(0), source_file = character(0),
                      result_id = character(0), analysis = character(0),
                      table_title = character(0), row_label = character(0),
                      statistic = character(0), stato_label = character(0),
                      stato_iri = character(0), value = character(0))
  if (is.null(tables) || length(tables) == 0) return(empty)

  result_ids <- .stat_result_ids(tables)
  full_ids <- if (!is.na(source_file) && nzchar(source_file))
    paste0(source_file, "#", result_ids) else result_ids

  rows <- lapply(seq_along(tables), function(ti) {
    tb <- tables[[ti]]
    df <- tb$data
    if (is.null(df) || !nrow(df) || !ncol(df)) return(NULL)
    headers <- names(df)
    # Identify label columns (row keys) vs statistic columns.
    is_label <- vapply(seq_along(headers), function(c)
      .stat_is_label_col(headers[[c]], df[[c]]), logical(1))
    label_cols <- which(is_label); stat_cols <- which(!is_label)
    if (!length(stat_cols)) return(NULL)

    per_row <- lapply(seq_len(nrow(df)), function(ri) {
      row_label <- paste(trimws(as.character(df[ri, label_cols, drop = TRUE])),
                         collapse = " ")
      row_label <- trimws(gsub("\\s+", " ", row_label))
      cells <- lapply(stat_cols, function(ci) {
        val <- trimws(as.character(df[ri, ci]))
        if (!nzchar(val)) return(NULL)
        typ <- stato_type_column(headers[[ci]])
        data.frame(paper_id = paper_id,
                   source_file = source_file,
                   result_id = full_ids[[ti]],
                   analysis = tb$analysis %||% NA_character_,
                   table_title = tb$title %||% NA_character_,
                   row_label = row_label,
                   statistic = headers[[ci]],
                   stato_label = typ$annotationValue,
                   stato_iri = typ$termAccession,
                   value = val)
      })
      dplyr::bind_rows(Filter(Negate(is.null), cells))
    })
    dplyr::bind_rows(per_row)
  })
  out <- dplyr::bind_rows(Filter(Negate(is.null), rows))
  if (!nrow(out)) empty else out
}

#' Serialise extracted result tables as a validated ISA-JSON document
#'
#' Builds a complete ISA-JSON Investigation from the result tables of ONE
#' `.jasp`/`.omv` file, following the modelling validated for metacheck: one file
#' = one dataset = one Sample; each analysis heading is a Protocol/Process on that
#' shared Sample; each result row is a Material whose `characteristics` are the
#' STATO-typed statistic values (via [stato_type_column()]). The document
#' validates against the bundled ISA v1.0 schemas (`inst/schema/isa-json/`).
#'
#' @param tables the list returned by [read_stat_tables()] or [read_r_output()]
#' @param paper_id paper id / DOI, used in identifiers and the study title
#' @param source_file basename of the originating `.jasp`/`.omv`, or the R
#'   script name for run-R output, recorded as the assay's data file and
#'   technology platform, and as the `result_id` prefix (see below)
#'
#' @returns a list (ISA Investigation) ready to serialise with
#'   `jsonlite::toJSON(auto_unbox = TRUE)`. `NULL` when there are no tables. Each
#'   Material carries a `"result_id"` `Comment` identifying the CODE that
#'   produced its table — see [stat_results_long()] for the id format (shared
#'   between both serialisations of one extraction).
#' @export
stat_output_isa <- function(tables, paper_id = "metacheck",
                            source_file = NA_character_) {
  if (is.null(tables) || length(tables) == 0) return(NULL)
  platform <- if (grepl("\\.omv$", source_file %||% "", ignore.case = TRUE))
    "jamovi" else if (grepl("\\.jasp$", source_file %||% "", ignore.case = TRUE))
    "JASP" else "unknown"

  oa <- function(av, ts = "", ta = "")
    list(`@type` = "OntologyAnnotation", annotationValue = av,
         termSource = ts, termAccession = ta)

  result_ids <- .stat_result_ids(tables)
  full_ids <- if (!is.na(source_file) && nzchar(source_file))
    paste0(source_file, "#", result_ids) else result_ids

  # One Material per result row, characteristics = STATO-typed statistic cells.
  materials <- list(); processes <- list(); mat_refs <- list(); proc_i <- 0L
  for (ti in seq_along(tables)) {
    tb <- tables[[ti]]; df <- tb$data
    if (is.null(df) || !nrow(df) || !ncol(df)) next
    headers <- names(df)
    is_label <- vapply(seq_along(headers), function(c)
      .stat_is_label_col(headers[[c]], df[[c]]), logical(1))
    stat_cols <- which(!is_label); label_cols <- which(is_label)
    if (!length(stat_cols)) next

    row_mat_ids <- character(0)
    for (ri in seq_len(nrow(df))) {
      chars <- lapply(stat_cols, function(ci) {
        val <- trimws(as.character(df[ri, ci]))
        if (!nzchar(val)) return(NULL)
        typ <- stato_type_column(headers[[ci]])
        num <- suppressWarnings(as.numeric(val))
        # Keep the number when finite; otherwise keep the string as written.
        # JSON has no Inf/NaN, and JASP/jamovi emit "Inf", "< .001", "NaN" etc.
        # as display strings — those stay strings (the schema's value allows it).
        list(`@type` = "MaterialAttributeValue",
             category = list(`@type` = "MaterialAttribute",
                             characteristicType = oa(typ$annotationValue,
                                                     typ$termSource,
                                                     typ$termAccession)),
             value = if (is.na(num) || !is.finite(num)) val else num)
      })
      chars <- Filter(Negate(is.null), chars)
      if (!length(chars)) next
      row_label <- trimws(gsub("\\s+", " ",
        paste(as.character(df[ri, label_cols, drop = TRUE]), collapse = " ")))
      mid <- sprintf("#material/t%d_r%d", ti, ri)
      materials[[length(materials) + 1L]] <- list(
        `@id` = mid, `@type` = "Material",
        name = paste0(tb$analysis %||% "result",
                      if (nzchar(row_label)) paste0(": ", row_label) else ""),
        type = "Extract Name", characteristics = chars,
        comments = list(list(`@type` = "Comment", name = "result_id",
                             value = full_ids[[ti]])))
      row_mat_ids <- c(row_mat_ids, mid)
    }
    if (!length(row_mat_ids)) next
    proc_i <- proc_i + 1L
    processes[[length(processes) + 1L]] <- list(
      `@id` = sprintf("#process/analysis%d", ti),
      `@type` = "Process",
      name = tb$analysis %||% sprintf("analysis %d", ti),
      executesProtocol = list(`@id` = "#protocol/statistical_analysis"),
      parameterValues = list(),
      inputs = list(list(`@id` = "#sample/dataset")),
      outputs = lapply(row_mat_ids, function(x) list(`@id` = x)),
      comments = list())
    mat_refs <- c(mat_refs, lapply(row_mat_ids, function(x) list(`@id` = x)))
  }
  if (!length(materials)) return(NULL)

  study <- list(
    `@id` = "#study/stats", `@type` = "Study", filename = "s_study.json",
    identifier = paper_id, title = paste0("Statistical results: ", paper_id),
    description = paste0("Result tables extracted from ",
                        source_file %||% "a statistics file",
                        " and typed with the STATO ontology."),
    submissionDate = "", publicReleaseDate = "",
    publications = list(), people = list(), studyDesignDescriptors = list(),
    protocols = list(list(
      `@id` = "#protocol/statistical_analysis", `@type` = "Protocol",
      name = "statistical analysis", protocolType = oa("data transformation"),
      description = "", uri = "", version = "",
      parameters = list(), components = list())),
    materials = list(
      sources = list(list(`@id` = "#source/participants", `@type` = "Source",
                          name = paste0(source_file %||% paper_id, " dataset"),
                          characteristics = list(), comments = list())),
      samples = list(list(`@id` = "#sample/dataset", `@type` = "Sample",
                          name = paste0(source_file %||% paper_id, " analysed data"),
                          characteristics = list(), factorValues = list(),
                          derivesFrom = list(list(`@id` = "#source/participants")),
                          comments = list())),
      otherMaterials = materials),
    processSequence = processes,
    assays = list(list(
      `@id` = "#assay/results", `@type` = "Assay", filename = "a_assay.json",
      measurementType = oa("hypothesis testing"),
      technologyType = oa(""), technologyPlatform = platform,
      dataFiles = if (!is.na(source_file)) list(list(
        `@id` = "#data/source", `@type` = "Data",
        name = source_file, type = "Derived Data File", comments = list())) else list(),
      materials = list(samples = list(list(`@id` = "#sample/dataset")),
                       otherMaterials = mat_refs),
      characteristicCategories = list(), unitCategories = list(),
      processSequence = list(), comments = list())),
    factors = list(), characteristicCategories = list(),
    unitCategories = list(), comments = list())

  list(
    `@id` = paste0("#investigation/", paper_id), `@type` = "Investigation",
    filename = "i_investigation.json", identifier = paper_id,
    title = paste0("Statistical output: ", paper_id),
    description = "Statistical result tables extracted by metacheck, STATO-typed, as ISA-JSON.",
    submissionDate = "", publicReleaseDate = "",
    ontologySourceReferences = list(list(
      `@type` = "OntologySourceReference", name = "STATO",
      description = "Statistical Methods Ontology",
      file = "http://purl.obolibrary.org/obo/stato.owl", version = "latest_release")),
    publications = list(), people = list(), studies = list(study),
    comments = list())
}

#' Validate a statistical-output document against the bundled ISA-JSON schema
#'
#' Checks that a [stat_output_isa()] document (or any ISA Investigation) conforms
#' to the ISA model v1.0 JSON Schema. Uses the single self-contained bundled
#' schema (`inst/schema/isa-json/isa_bundled_schema.json`, all cross-file `$ref`s
#' inlined under `$defs`) so validation needs no reference list and no network.
#'
#' @param isa an ISA Investigation list (as from [stat_output_isa()]) or a JSON
#'   string / path to a `.json` file
#'
#' @returns `TRUE` when valid; otherwise `FALSE` with the validation errors in
#'   the `"errors"` attribute. Errors (rather than returning) if `jsonvalidate`
#'   is not installed.
#' @export
stat_output_validate <- function(isa) {
  if (!requireNamespace("jsonvalidate", quietly = TRUE))
    stop("validation needs the 'jsonvalidate' package.", call. = FALSE)
  schema <- system.file("schema", "isa-json", "isa_bundled_schema.json",
                        package = "metacheck")
  if (!nzchar(schema) || !file.exists(schema))
    stop("bundled ISA schema not found in the installed package.", call. = FALSE)
  json <- if (is.character(isa) && length(isa) == 1 &&
              (file.exists(isa) || grepl("^\\s*[\\[{]", isa)))
    (if (file.exists(isa)) paste(readLines(isa, warn = FALSE), collapse = "\n") else isa)
  else jsonlite::toJSON(isa, auto_unbox = TRUE, null = "null")
  v <- jsonvalidate::json_validator(schema, engine = "ajv")
  v(json, verbose = TRUE, greedy = TRUE)
}

#' Write extracted statistical output to a dedicated folder
#'
#' Writes `reproducibility_check`'s accumulated `stat_output` (one element per
#' source file: JASP/jamovi files via [read_stat_tables()], executed R scripts
#' via [read_r_output()]) to `<root>/statistical_output/`, a folder sibling to
#' the materialised `data/` — so the extracted statistics sit alongside the data
#' and code that produced them. Two views are written from the SAME extraction:
#' one combined `results_long.csv` (every source file's [stat_results_long()]
#' rows stacked, one row per extracted statistic, easiest to load and filter),
#' and one `<codename>.statistical_output.json` per source file (its
#' [stat_output_isa()] document). Called from inside `reproducibility_check`
#' itself (not from the later psychds/scienceverse conversion step), so the
#' folder exists at the same point in the pipeline the data/code files do.
#'
#' @param stat_output a list as accumulated by `reproducibility_check` — each
#'   element a list with `file` (source file name), `isa`
#'   ([stat_output_isa()]'s document), and `long` ([stat_results_long()]'s
#'   table)
#' @param root the materialised layout root (the same directory `data/` is
#'   copied into); `statistical_output/` is created under it
#'
#' @returns the path to `statistical_output/`, invisibly, or `NULL` (nothing
#'   written) when `stat_output` is empty or carries no rows/documents.
#' @export
stat_output_write <- function(stat_output, root) {
  if (is.null(stat_output) || !length(stat_output)) return(invisible(NULL))

  longs <- Filter(function(s) is.data.frame(s$long) && nrow(s$long) > 0,
                  stat_output)
  isas  <- Filter(function(s) !is.null(s$isa), stat_output)
  if (!length(longs) && !length(isas)) return(invisible(NULL))

  out_dir <- file.path(root, "statistical_output")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (length(longs)) {
    combined <- dplyr::bind_rows(lapply(longs, `[[`, "long"))
    utils::write.csv(combined, file.path(out_dir, "results_long.csv"),
                     row.names = FALSE, na = "")
  }

  for (s in isas) {
    fn <- sub("[.][^.]+$", "", basename(s$file %||% "result"))
    json_path <- file.path(out_dir, paste0(fn, ".statistical_output.json"))
    writeLines(jsonlite::toJSON(s$isa, auto_unbox = TRUE, pretty = TRUE,
                                null = "null"), json_path)
  }

  invisible(out_dir)
}
