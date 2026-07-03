#' Excel Formatting Check
#'
#' @description
#' This module inspects the Excel (`.xlsx`) files in a repository for formatting
#' practices that hurt machine-readability and data reuse: cells that use fill
#' colour to encode information, merged cells, fully empty rows, and empty or
#' unnamed (blank-header) columns. None of these survive a plain CSV export, so
#' data documented only through them is effectively lost to anyone who reads the
#' file programmatically.
#'
#' @details
#' The module consumes the file classification produced by `data_check` (and,
#' transitively, `repo_check`), takes every `.xlsx` file with a local copy, and
#' reads it as a zip of XML parts (no extra dependency beyond `xml2`). For each
#' worksheet it flags:
#'
#' * **colour coding** — cells whose fill references a non-default colour, i.e.
#'   colour used to mark or group values;
#' * **merged cells** — ranges merged across rows/columns, which break the
#'   rectangular grid a table reader expects;
#' * **empty rows** — rows inside the used range that are entirely blank;
#' * **empty / unnamed columns** — columns with no values, or with a blank
#'   header (read by `readxl` as `...2`, `...8`, ...).
#'
#' Legacy `.xls` files cannot be inspected at the cell/style level here and are
#' reported as un-inspectable (converting them to `.xlsx` is recommended).
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object, or NULL to check local
#'   files only (see [test_paper()])
#' @param local_path optional path passed to `data_check` / `repo_check` when
#'   their output is not already available
#' @param local_only if TRUE, skip online repository lookups (see `repo_check`)
#' @param model,params passed to `data_check` when `llm_use(TRUE)`
#'
#' @returns a list
excel_check <- function(paper, local_path = NULL, local_only = FALSE,
                        model = llm_model(), params = list()) {

  .excel_exts <- c("xlsx", "xls")

  .pid <- function(...) {
    id <- paper_id(paper)
    for (df in list(...)) {
      if (length(id) > 0) break
      if (!is.null(df) && "paper_id" %in% names(df)) id <- unique(df$paper_id)
    }
    if (length(id) == 0) NA_character_ else id[[1]]
  }

  # ── 1. File classification from data_check ──────────────────────────────────
  structure_df <- get_prev_outputs("data_check", "structure")
  if (is.null(structure_df)) {
    mo <- if (!is.null(local_path)) {
      module_run(paper, "data_check", local_path = local_path,
                 local_only = local_only, model = model, params = params)
    } else {
      module_run(paper, "data_check", local_only = local_only,
                 model = model, params = params)
    }
    structure_df <- mo$structure
  }

  summary_zero <- c(file_n = 0, flagged_file_n = 0, color_n = 0, merge_n = 0,
                    empty_row_n = 0, empty_col_n = 0)
  empty <- function(text) {
    list(
      table = data.frame(),
      summary_table = data.frame(paper_id = .pid(structure_df),
                                 as.list(summary_zero)),
      na_replace = summary_zero,
      traffic_light = "na",
      summary_text = text
    )
  }

  # Excel files with a readable local copy.
  xl_rows <- if (!is.null(structure_df) && nrow(structure_df) > 0) {
    structure_df[
      tolower(tools::file_ext(structure_df$file_name)) %in% .excel_exts &
        !is.na(structure_df$file_location) &
        nzchar(structure_df$file_location) &
        file.exists(structure_df$file_location %||% ""),
      , drop = FALSE
    ]
  } else structure_df[0, , drop = FALSE]

  if (is.null(xl_rows) || nrow(xl_rows) == 0)
    return(empty("We found no Excel files to inspect."))

  n_files <- nrow(xl_rows)

  # ── 2. Inspect each file ─────────────────────────────────────────────────────
  findings <- list()   # one row per (file, sheet, issue)
  per_file <- list()   # per-file issue counts
  for (i in seq_len(n_files)) {
    fname <- xl_rows$file_name[i]
    path  <- xl_rows$file_location[i]
    ext   <- tolower(tools::file_ext(fname))

    if (ext == "xls") {
      findings[[length(findings) + 1L]] <- data.frame(
        File = fname, Sheet = NA_character_, Issue = "Un-inspectable (.xls)",
        Detail = "Legacy .xls format; convert to .xlsx for a full check.")
      next
    }

    insp <- tryCatch(.excel_inspect(path), error = function(e) NULL)
    if (is.null(insp)) {
      findings[[length(findings) + 1L]] <- data.frame(
        File = fname, Sheet = NA_character_, Issue = "Unreadable",
        Detail = "The file could not be parsed as an .xlsx workbook.")
      next
    }

    fc <- c(color = 0L, merge = 0L, empty_row = 0L, empty_col = 0L)
    for (s in insp$sheets) {
      if (s$color_cells > 0) {
        fc["color"] <- fc["color"] + s$color_cells
        findings[[length(findings) + 1L]] <- data.frame(
          File = fname, Sheet = s$name, Issue = "Colour coding",
          Detail = sprintf("%d cell%s use fill colour to encode information; colour is lost on CSV export.",
                           s$color_cells, plural(s$color_cells)))
      }
      if (length(s$merges) > 0) {
        fc["merge"] <- fc["merge"] + length(s$merges)
        findings[[length(findings) + 1L]] <- data.frame(
          File = fname, Sheet = s$name, Issue = "Merged cells",
          Detail = sprintf("%d merged range%s (%s) break the rectangular grid.",
                           length(s$merges), plural(length(s$merges)),
                           paste(utils::head(s$merges, 5), collapse = ", ")))
      }
      if (s$empty_rows > 0) {
        fc["empty_row"] <- fc["empty_row"] + s$empty_rows
        findings[[length(findings) + 1L]] <- data.frame(
          File = fname, Sheet = s$name, Issue = "Empty rows",
          Detail = sprintf("%d fully empty row%s inside the data range.",
                           s$empty_rows, plural(s$empty_rows)))
      }
      if (s$empty_cols > 0) {
        fc["empty_col"] <- fc["empty_col"] + s$empty_cols
        findings[[length(findings) + 1L]] <- data.frame(
          File = fname, Sheet = s$name, Issue = "Empty or unnamed columns",
          Detail = sprintf("%d column%s %s empty or have a blank header.",
                           s$empty_cols, plural(s$empty_cols),
                           if (s$empty_cols == 1) "is" else "are"))
      }
    }
    per_file[[fname]] <- fc
  }

  findings_df <- if (length(findings) > 0) dplyr::bind_rows(findings) else
    data.frame(File = character(0), Sheet = character(0),
               Issue = character(0), Detail = character(0))

  # ── 3. Tallies ───────────────────────────────────────────────────────────────
  totals <- Reduce(`+`, per_file, c(color = 0L, merge = 0L,
                                    empty_row = 0L, empty_col = 0L))
  n_flagged_files <- length(unique(findings_df$File))

  # ── 4. Traffic light ─────────────────────────────────────────────────────────
  tl <- if (nrow(findings_df) == 0) "green" else "yellow"

  # ── 5. Report ────────────────────────────────────────────────────────────────
  report <- c(
    "This module inspects Excel files for formatting that is not machine-readable (colour coding, merged cells, empty rows, empty/unnamed columns).",
    sprintf("We examined %d Excel file%s in the repository.",
            n_files, plural(n_files))
  )

  if (nrow(findings_df) == 0) {
    report <- c(report,
      "No machine-readability issues were found in the Excel files.")
  } else {
    report <- c(
      report,
      sprintf("%d of %d Excel file%s %s at least one formatting issue.",
              n_flagged_files, n_files, plural(n_files),
              if (n_flagged_files == 1) "has" else "have"),
      "#### Excel Formatting Issues",
      scroll_table(findings_df, maxrows = 20),
      "Excel formatting such as colour, merged cells, and blank rows/columns is not preserved when data are read programmatically or exported to CSV. Store data as a plain rectangular table (one header row, one column per variable, no colour-encoded meaning) so it is machine-readable."
    )
  }

  # ── 6. Summary text + table ──────────────────────────────────────────────────
  if (nrow(findings_df) == 0) {
    summary_text <- sprintf(
      "We examined %d Excel file%s and found no machine-readability issues.",
      n_files, plural(n_files))
  } else {
    parts <- c(
      if (totals["color"] > 0) sprintf("%d colour-coded cell%s",
        totals["color"], plural(totals["color"])),
      if (totals["merge"] > 0) sprintf("%d merged range%s",
        totals["merge"], plural(totals["merge"])),
      if (totals["empty_row"] > 0) sprintf("%d empty row%s",
        totals["empty_row"], plural(totals["empty_row"])),
      if (totals["empty_col"] > 0) sprintf("%d empty/unnamed column%s",
        totals["empty_col"], plural(totals["empty_col"]))
    )
    summary_text <- sprintf(
      "Across %d Excel file%s we found %s.",
      n_files, plural(n_files), paste(parts, collapse = ", "))
  }

  summary_table <- data.frame(
    paper_id       = .pid(structure_df),
    file_n         = n_files,
    flagged_file_n = n_flagged_files,
    color_n        = unname(totals["color"]),
    merge_n        = unname(totals["merge"]),
    empty_row_n    = unname(totals["empty_row"]),
    empty_col_n    = unname(totals["empty_col"])
  )

  list(
    table = findings_df,
    summary_table = summary_table,
    na_replace = summary_zero,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}

# ── Module-local helpers ──────────────────────────────────────────────────────

# Inspect one .xlsx file by reading it as a zip of XML parts. Returns a list with
# `sheets`, each a list(name, color_cells, merges, empty_rows, empty_cols), or
# NULL on failure. Uses only xml2 (no readxl/openxlsx dependency for the
# style-level checks); the empty-row/column checks read cell values from the
# sheet XML directly.
.excel_inspect <- function(path) {
  if (!file.exists(path)) return(NULL)
  tmp <- tempfile("xlsx_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ok <- tryCatch({ utils::unzip(path, exdir = tmp); TRUE },
                 error = function(e) FALSE)
  if (!ok) return(NULL)

  xl <- file.path(tmp, "xl")
  if (!dir.exists(xl)) return(NULL)

  # Which cell style indices (s=) reference a non-default fill colour?
  colored_styles <- .excel_colored_styles(file.path(xl, "styles.xml"))

  # Sheet name + file mapping (workbook order matches sheetN.xml order well
  # enough for reporting; fall back to the file stem).
  wb_path <- file.path(xl, "workbook.xml")
  sheet_names <- if (file.exists(wb_path)) {
    wb <- tryCatch(xml2::read_xml(wb_path), error = function(e) NULL)
    if (is.null(wb)) character(0) else {
      xml2::xml_attr(xml2::xml_find_all(wb, ".//*[local-name()='sheet']"), "name")
    }
  } else character(0)

  sheet_files <- sort(list.files(file.path(xl, "worksheets"),
                                 pattern = "^sheet[0-9]+\\.xml$",
                                 full.names = TRUE))
  sheets <- list()
  for (j in seq_along(sheet_files)) {
    nm <- if (j <= length(sheet_names)) sheet_names[[j]] else
      tools::file_path_sans_ext(basename(sheet_files[[j]]))
    sheets[[length(sheets) + 1L]] <-
      .excel_inspect_sheet(sheet_files[[j]], nm, colored_styles)
  }

  list(sheets = sheets)
}

# Return the 0-based cell-style indices (positions in cellXfs) whose fill is a
# non-default colour. Default fills are patternType none/gray125 or black/white
# fgColor; anything else counts as colour coding.
.excel_colored_styles <- function(styles_path) {
  if (!file.exists(styles_path)) return(integer(0))
  st <- tryCatch(xml2::read_xml(styles_path), error = function(e) NULL)
  if (is.null(st)) return(integer(0))

  fills <- xml2::xml_find_all(st, ".//*[local-name()='fills']/*[local-name()='fill']")
  fill_is_color <- vapply(fills, function(fl) {
    fg <- xml2::xml_find_first(fl, ".//*[local-name()='fgColor']")
    if (inherits(fg, "xml_missing")) return(FALSE)
    rgb <- xml2::xml_attr(fg, "rgb")
    # A themed colour (no rgb) or a plain black/white fill is not "colour coding".
    !is.na(rgb) && nzchar(rgb) &&
      !toupper(rgb) %in% c("FF000000", "FFFFFFFF", "00000000")
  }, logical(1))
  colored_fill_ids <- which(fill_is_color) - 1L    # 0-based fillId

  if (length(colored_fill_ids) == 0) return(integer(0))

  xfs <- xml2::xml_find_all(st, ".//*[local-name()='cellXfs']/*[local-name()='xf']")
  xf_fill <- suppressWarnings(as.integer(xml2::xml_attr(xfs, "fillId")))
  which(xf_fill %in% colored_fill_ids) - 1L         # 0-based style index (s=)
}

# Inspect one worksheet XML: count colour-coded cells, merged ranges, fully
# empty rows, and empty/unnamed columns. `colored_styles` is the set of 0-based
# style indices that use a colour fill.
#
# All cell-level facts are extracted with a handful of whole-document xml2 calls
# and then reduced with base-R vector ops. A previous version ran an XPath query
# per cell to test whether it held a value, which is O(rows x cols) XPath
# evaluations — minutes on a wide (e.g. 300 x 2000) Qualtrics export. Here the
# populated cells come from a single `.//c[v|is]` query, and row/column identity
# is parsed from the vector of cell references.
.excel_inspect_sheet <- function(sheet_path, name, colored_styles) {
  blank <- list(name = name, color_cells = 0L, merges = character(0),
                empty_rows = 0L, empty_cols = 0L)
  sh <- tryCatch(xml2::read_xml(sheet_path), error = function(e) NULL)
  if (is.null(sh)) return(blank)

  # Every cell, and its reference (e.g. "B12"). One query for all cells.
  cells    <- xml2::xml_find_all(sh, ".//*[local-name()='c']")
  cell_ref <- xml2::xml_attr(cells, "r")

  # Populated cells: those with a <v> (value) or <is> (inline string) child.
  # One query returns exactly the non-empty cells, avoiding per-cell XPath.
  val_cells <- xml2::xml_find_all(
    sh, ".//*[local-name()='c'][*[local-name()='v'] or *[local-name()='is']]")
  val_ref <- xml2::xml_attr(val_cells, "r")

  # Colour-coded cells: cells whose style index is in colored_styles.
  color_cells <- 0L
  if (length(colored_styles) > 0 && length(cells) > 0) {
    cell_s <- suppressWarnings(as.integer(xml2::xml_attr(cells, "s")))
    color_cells <- sum(cell_s %in% colored_styles, na.rm = TRUE)
  }

  # Merged ranges.
  merges <- xml2::xml_attr(
    xml2::xml_find_all(sh, ".//*[local-name()='mergeCell']"), "ref")
  merges <- merges[!is.na(merges)]

  # Split cell references into column letters and row numbers (vectorised).
  ref_col <- function(ref) sub("[0-9]+$", "", ref)
  ref_row <- function(ref) suppressWarnings(as.integer(sub("^[A-Za-z]+", "", ref)))
  val_col <- ref_col(val_ref)
  val_rownum <- ref_row(val_ref)

  # Empty rows: rows inside the populated range that carry no value. Only blanks
  # that fall between the first and last populated row are counted (trailing
  # blank <row> elements in the XML are rare and not meaningful).
  empty_rows <- 0L
  if (length(val_rownum) > 0) {
    populated <- sort(unique(val_rownum))
    if (length(populated) > 1)
      empty_rows <- (max(populated) - min(populated) + 1L) - length(populated)
  }

  # Empty / unnamed columns. The header is the first populated row; a column is
  # problematic if its header cell is blank, or it has a header but no value in
  # any row below. Column identity is the letter part of the cell reference.
  empty_cols <- 0L
  if (length(val_ref) > 0) {
    hdr_row  <- min(val_rownum)
    hdr_cols <- val_col[val_rownum == hdr_row]                 # cols with a header value
    all_cols <- unique(ref_col(cell_ref))                      # every column that appears
    body_cols <- unique(val_col[val_rownum > hdr_row])         # cols with a body value
    blank_header  <- setdiff(all_cols, hdr_cols)               # column present but no header
    header_no_body <- setdiff(hdr_cols, body_cols)             # header but empty below
    empty_cols <- length(unique(c(blank_header, header_no_body)))
  }

  list(name = name, color_cells = color_cells, merges = merges,
       empty_rows = empty_rows, empty_cols = empty_cols)
}
