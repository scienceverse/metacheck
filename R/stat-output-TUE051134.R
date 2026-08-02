# Serialise extracted JASP/jamovi result tables (from read_stat_tables()) two
# ways, from ONE extraction:
#   * stat_results_long()  -> a flat, queryable data.frame (one row per cell:
#     analysis / row label / statistic / STATO iri / value) for the scienceverse
#     SQLite DB, matching how other module output is flattened into it;
#   * stat_output_json()   -> a complete, structured statistical-output document
#     (schema/schema_version, paper_id, source_file, source_format, and an
#     analyses[] array, each with results[] carrying result_id/row_label/values)
#     for the run logs. This is a metacheck-NATIVE schema, in the character of
#     OSD/DDI-Lifecycle/Psych-DS: a flat, self-describing JSON document with a
#     small set of named sections, every field open. It replaced an earlier
#     ISA-JSON-modelled version (Investigation/Study/Material vocabulary
#     borrowed from the life-sciences ISA model) that validated against a real
#     external schema but forced statistics into containers (a t-test result as
#     a "Material") that vocabulary was never designed to hold — a repurposing
#     metacheck itself invented, not an established convention. Dropping ISA
#     removes that mismatch entirely; there is no external schema to conform to
#     here, by design.
# Column -> STATO typing is via stato_type_column() (R/stato-map.R): STATO IRI
# where a class exists, else the header text as a nominal label (never dropped).
#
# Every result ROW gets a stable result_id, via .stat_result_ids() (the TABLE's
# base id) plus a per-row suffix added by each caller. The base id is the
# CODENAME (source_file) that produced the table plus a POSITIONAL locator —
#   * an executed R script: the source LINE the statement started on, from
#     read_r_output()'s echo-based line attribution (`l<line>`), plus
#     `line_seq` (that result TABLE's 1-based position among results sharing
#     the SAME line — a loop calling t.test() each iteration prints several
#     result tables off one source line);
#   * a JASP/jamovi extraction: there is no source line for a GUI-driven
#     analysis, so `table_index` (the table's 1-based ordinal position in the
#     rendered document, from read_stat_tables()) plays the same role a line
#     number plays for R.
# Each caller then appends `_r<row>` (that row's 1-based position within the
# table) to turn the table's base id into a unique per-ROW id — the level a
# reader actually wants to trace ("which line/table produced THIS reported
# t-value"), since one result table commonly holds several rows (one per
# comparison/predictor/group).
# The whole id is sanitised to lower-case letters/digits/underscores only —
# every separator (the `#` that used to join source_file to the locator, `.`
# in a filename, spaces in an analysis heading) becomes `_` — so result_id is
# always a single safe token, usable as a filename or a column value without
# further escaping.
.stat_sanitize_id <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  x <- gsub("[^a-z0-9]+", "_", x)
  sub("^_|_$", "", x)
}

# One base (per-TABLE) result_id per element of `tables`
# (read_stat_tables()/read_r_output()'s return shape), stamped in the SAME
# order those tables are walked elsewhere in this file (stat_results_long,
# stat_output_json). Callers append `_r<row>` per row to get the final,
# per-result id. `source_file` is required (unlike before, where it was
# appended by each caller separately) so the whole id can be sanitised as ONE
# token here, in one place.
.stat_result_ids <- function(tables, source_file = NA_character_) {
  src <- .stat_sanitize_id(source_file %||% "result")
  locator <- vapply(tables, function(tb) {
    if (!is.null(tb$line) && !is.na(tb$line)) {
      seq_n <- tb$line_seq %||% 1L
      paste0("l", tb$line, "_", seq_n)
    } else if (!is.null(tb$table_index) && !is.na(tb$table_index)) {
      paste0("t", tb$table_index)
    } else if (!is.null(tb$analysis) && !is.na(tb$analysis) && nzchar(tb$analysis)) {
      tb$analysis
    } else "result"
  }, character(1))
  ids <- paste0(src, "_", locator)
  # A locator can still repeat across TABLES (e.g. the analysis-heading
  # fallback with no line/table_index, if a heading recurs) — disambiguate
  # with a trailing counter before row-suffixing, same intent as before.
  ave(ids, ids, FUN = function(x)
    if (length(x) == 1) x else paste0(x, "_", seq_along(x)))
}

# Is this cell a PLACEHOLDER rather than a value? JASP renders an empty cell in
# a result table as "." and jamovi as an em/en dash; a table for an analysis the
# user set up but never completed is placeholder in EVERY cell. Emitting those
# produces fully STATO-typed junk — a "p-value" whose value is "." — which is
# worse than omitting them, because a downstream matcher sees a p that exists
# but can never match anything. Treated exactly like the already-skipped empty
# cell: the key is omitted, and a result left with no values at all is dropped.
# Deliberately NARROW: only these exact markers (after trimming) count, so a
# real value is never discarded.
.STAT_PLACEHOLDERS <- c(".", "-", "—", "–", "−",
                        "na", "nan", "null", "n/a")
.stat_is_placeholder <- function(x) {
  x <- trimws(as.character(x %||% ""))
  !nzchar(x) || tolower(x) %in% .STAT_PLACEHOLDERS
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
  # Placeholders ("." in JASP, an em dash in jamovi) are not content: an
  # all-placeholder column carries no information either way. They must be
  # dropped BEFORE the numeric-content test below, because that test's regex
  # (`[0-9.]+`) matches a bare "." — so a column of JASP placeholders would
  # otherwise look 100% numeric and be misclassified as a statistic column,
  # producing a junk statistic keyed off an empty header.
  vals <- vals[!vapply(vals, .stat_is_placeholder, logical(1))]
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
#'   `stato_iri`, and `value`. `result_id` identifies the CODE ROW that
#'   produced a result, sanitised to lower-case letters/digits/underscores:
#'   `<source_file>_l<line>_<line_seq>_r<row>` for an executed R script (from
#'   [read_r_output()]'s echo-based line attribution), or
#'   `<source_file>_t<table_index>_r<row>` for a JASP/jamovi extraction (no
#'   source line exists there, so the table's ordinal position in the rendered
#'   document stands in for one); `row` is the 1-based position of this result
#'   within its table. Empty frame (same columns) when there is nothing to
#'   flatten.
#' @export
stat_results_long <- function(tables, paper_id = NA_character_,
                              source_file = NA_character_) {
  empty <- data.frame(paper_id = character(0), source_file = character(0),
                      result_id = character(0), analysis = character(0),
                      table_title = character(0), row_label = character(0),
                      statistic = character(0), stato_label = character(0),
                      stato_iri = character(0), value = character(0))
  if (is.null(tables) || length(tables) == 0) return(empty)

  base_ids <- .stat_result_ids(tables, source_file)

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
        if (.stat_is_placeholder(val)) return(NULL)
        typ <- stato_type_column(headers[[ci]])
        data.frame(paper_id = paper_id,
                   source_file = source_file,
                   result_id = .stat_sanitize_id(paste0(base_ids[[ti]], "_r", ri)),
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

#' Serialise extracted result tables as a structured statistical-output document
#'
#' Builds a complete statistical-output document from the result tables of ONE
#' `.jasp`/`.omv` file or executed R script: one document per source file, with
#' an `analyses` array (one element per analysis heading/table) each carrying a
#' `results` array (one element per result row) of STATO-typed statistic values
#' (via [stato_type_column()]). This is a metacheck-native schema — a flat,
#' self-describing document in the character of OSD/DDI-Lifecycle/Psych-DS —
#' not a repurposing of an unrelated external standard's vocabulary; there is
#' no external schema this validates against.
#'
#' @param tables the list returned by [read_stat_tables()] or [read_r_output()]
#' @param paper_id paper id / DOI, recorded on the document
#' @param source_file basename of the originating `.jasp`/`.omv`, or the R
#'   script name for run-R output, recorded as `source_file` and used to derive
#'   `source_format` and the `result_id` prefix (see below)
#'
#' @returns a list ready to serialise with `jsonlite::toJSON(auto_unbox =
#'   TRUE)`, with elements `schema`, `schema_version`, `paper_id`,
#'   `source_file`, `source_format`, and `analyses` (a list of `list(analysis,
#'   results)`, where each result is `list(result_id, row_label, values)` and
#'   `values` is a named list keyed by the statistic's own short name, e.g.
#'   `t`/`df`/`p`/`d`, each a `list(value, stato_label, stato_iri)` —
#'   `stato_label`/`stato_iri` omitted when [stato_type_column()] found no
#'   STATO class for that statistic). `NULL` when there are no tables or no
#'   table yields any typed value. `result_id` identifies the CODE ROW that
#'   produced it — see [stat_results_long()] for the id format (shared between
#'   both serialisations of one extraction).
#' @export
stat_output_json <- function(tables, paper_id = "metacheck",
                            source_file = NA_character_) {
  if (is.null(tables) || length(tables) == 0) return(NULL)
  source_format <- if (grepl("\\.omv$", source_file %||% "", ignore.case = TRUE))
    "jamovi" else if (grepl("\\.jasp$", source_file %||% "", ignore.case = TRUE))
    "JASP" else if (grepl("\\.[rR]$", source_file %||% "", ignore.case = TRUE))
    "R" else "unknown"

  base_ids <- .stat_result_ids(tables, source_file)

  analyses <- list()
  for (ti in seq_along(tables)) {
    tb <- tables[[ti]]; df <- tb$data
    if (is.null(df) || !nrow(df) || !ncol(df)) next
    headers <- names(df)
    is_label <- vapply(seq_along(headers), function(c)
      .stat_is_label_col(headers[[c]], df[[c]]), logical(1))
    stat_cols <- which(!is_label); label_cols <- which(is_label)
    if (!length(stat_cols)) next

    results <- list()
    for (ri in seq_len(nrow(df))) {
      values <- list()
      for (ci in stat_cols) {
        val <- trimws(as.character(df[ri, ci]))
        if (.stat_is_placeholder(val)) next
        typ <- stato_type_column(headers[[ci]])
        num <- suppressWarnings(as.numeric(val))
        # Keep the number when finite; otherwise keep the string as written.
        # JSON has no Inf/NaN, and JASP/jamovi emit "Inf", "< .001", "NaN" etc.
        # as display strings — those stay strings.
        entry <- list(value = if (is.na(num) || !is.finite(num)) val else num)
        if (nzchar(typ$termAccession)) {
          entry$stato_label <- typ$annotationValue
          entry$stato_iri <- typ$termAccession
        }
        # Key by the statistic's own header text (lower-cased, sanitised) so
        # every column type — even one with no STATO class — gets a stable,
        # readable key rather than being dropped.
        key <- .stat_sanitize_id(headers[[ci]])
        if (!nzchar(key)) key <- paste0("v", ci)
        values[[key]] <- entry
      }
      if (!length(values)) next
      row_label <- trimws(gsub("\\s+", " ",
        paste(as.character(df[ri, label_cols, drop = TRUE]), collapse = " ")))
      results[[length(results) + 1L]] <- list(
        result_id = .stat_sanitize_id(paste0(base_ids[[ti]], "_r", ri)),
        row_label = row_label,
        values = values)
    }
    if (!length(results)) next
    analyses[[length(analyses) + 1L]] <- list(
      analysis = tb$analysis %||% NA_character_,
      results = results)
  }
  if (!length(analyses)) return(NULL)

  list(
    schema = "metacheck-statistical-output",
    schema_version = "1.0",
    paper_id = paper_id,
    source_file = source_file,
    source_format = source_format,
    analyses = analyses)
}

#' Validate a statistical-output document's native shape
#'
#' Checks that a [stat_output_json()] document has the required top-level
#' fields, that `analyses` is a list of `list(analysis, results)`, and that
#' every result carries a `result_id` and a non-empty `values` object whose
#' entries each have a `value`. This is a native structural check, not an
#' executed external JSON Schema — there is no external standard this document
#' conforms to, by design (see the file header comment). Mirrors
#' [behaverse_validate()]'s and `psychds-validate.R`'s hand-rolled approach.
#'
#' @param doc a statistical-output document as an R list (as from
#'   [stat_output_json()]), or a length-1 character path / JSON string to parse
#'   first.
#'
#' @returns a list with `valid` (`TRUE` when no issues), `issues` (a character
#'   vector of problem descriptions), and `summary` (`n_errors`, `n_analyses`,
#'   `n_results`).
#' @export
stat_output_validate <- function(doc) {
  if (is.character(doc) && length(doc) == 1L) {
    src <- if (file.exists(doc)) doc else textConnection(doc)
    parsed <- tryCatch(jsonlite::fromJSON(src, simplifyVector = FALSE),
                       error = function(e) NULL)
    if (is.null(parsed))
      return(list(valid = FALSE, issues = "Input is not valid JSON.",
                  summary = list(n_errors = 1L, n_analyses = 0L, n_results = 0L)))
    doc <- parsed
  }

  issues <- character(0)
  add <- function(msg) issues <<- c(issues, msg)

  required_top <- c("schema", "schema_version", "paper_id", "source_file",
                    "source_format", "analyses")
  missing_top <- setdiff(required_top, names(doc %||% list()))
  if (length(missing_top))
    add(sprintf("Document missing top-level field%s: %s.",
                plural(length(missing_top)), paste(missing_top, collapse = ", ")))

  analyses <- doc$analyses %||% list()
  if (!is.list(analyses))
    add("`analyses` must be a list.")

  n_results <- 0L
  for (a in analyses) {
    if (is.null(a$analysis))
      add("An analysis entry is missing `analysis`.")
    results <- a$results %||% list()
    if (!is.list(results) || !length(results)) {
      add("An analysis entry has no `results`.")
      next
    }
    for (r in results) {
      n_results <- n_results + 1L
      if (is.null(r$result_id) || !nzchar(r$result_id %||% ""))
        add("A result is missing `result_id`.")
      values <- r$values %||% list()
      if (!is.list(values) || !length(values)) {
        add(sprintf("Result \"%s\" has no `values`.", r$result_id %||% "?"))
        next
      }
      for (vn in names(values)) {
        if (is.null(values[[vn]]$value))
          add(sprintf("Result \"%s\": value \"%s\" is missing `value`.",
                      r$result_id %||% "?", vn))
      }
    }
  }

  list(valid = !length(issues), issues = issues,
       summary = list(n_errors = length(issues), n_analyses = length(analyses),
                      n_results = n_results))
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
#' [stat_output_json()] document). Called from inside `reproducibility_check`
#' itself (not from the later psychds/scienceverse conversion step), so the
#' folder exists at the same point in the pipeline the data/code files do.
#'
#' @param stat_output a list as accumulated by `reproducibility_check` — each
#'   element a list with `file` (source file name), `json`
#'   ([stat_output_json()]'s document), and `long` ([stat_results_long()]'s
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
  jsons <- Filter(function(s) !is.null(s$json), stat_output)
  if (!length(longs) && !length(jsons)) return(invisible(NULL))

  out_dir <- file.path(root, "statistical_output")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (length(longs)) {
    combined <- dplyr::bind_rows(lapply(longs, `[[`, "long"))
    utils::write.csv(combined, file.path(out_dir, "results_long.csv"),
                     row.names = FALSE, na = "")
  }

  for (s in jsons) {
    fn <- sub("[.][^.]+$", "", basename(s$file %||% "result"))
    json_path <- file.path(out_dir, paste0(fn, ".statistical_output.json"))
    writeLines(jsonlite::toJSON(s$json, auto_unbox = TRUE, pretty = TRUE,
                                null = "null"), json_path)
  }

  invisible(out_dir)
}
