#' Spreadsheet Formatting Check
#'
#' @description
#' This module inspects the spreadsheet files in a repository (`.xlsx`, `.xls`
#' and OpenDocument `.ods`/`.fods`) for formatting practices that hurt
#' machine-readability and data reuse: cells that use fill colour to encode
#' information, merged cells, fully empty rows, and empty or unnamed
#' (blank-header) columns. None of these survive a plain CSV export, so data
#' documented only through them is effectively lost to anyone who reads the file
#' programmatically.
#'
#' @details
#' The module consumes the file classification produced by `data_check` (and,
#' transitively, `repo_check`), takes every spreadsheet file with a local copy,
#' and reads it as a zip of XML parts (no extra dependency beyond `xml2`). For
#' each worksheet it flags:
#'
#' * **colour coding** — cells whose fill references a non-default colour, i.e.
#'   colour used to mark or group values;
#' * **merged cells** — ranges merged across rows/columns, which break the
#'   rectangular grid a table reader expects;
#' * **empty rows** — rows inside the used range that are entirely blank;
#' * **empty / unnamed columns** — columns with no values, or with a blank
#'   header (read by `readxl`/`readODS` as `...2`, `...8`, ...).
#'
#' `.xlsx` (OOXML) and `.ods` (OpenDocument) are both zipped XML and are
#' inspected at the cell/style level by format-specific parsers that return the
#' same per-sheet facts, so the two formats are reported identically. Legacy
#' `.xls` is a binary format with no XML to read: it is checked for an offset
#' header (which `readxl` can see) but reported as un-inspectable for the
#' style-level checks, with conversion to `.xlsx` recommended.
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
spreadsheet_check <- function(paper, local_path = NULL, local_only = FALSE,
                              model = llm_model(), params = list()) {

  # Formats inspected. .xlsx/.ods are zipped XML (full cell/style inspection);
  # .fods is the same ODF content in a single flat XML file (no zip); .xls is
  # binary and only supports the readxl-based offset-header check.
  .spreadsheet_exts <- c("xlsx", "xls", "ods", "fods")

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

  # Spreadsheet files with a readable local copy.
  xl_rows <- if (!is.null(structure_df) && nrow(structure_df) > 0) {
    structure_df[
      tolower(tools::file_ext(structure_df$file_name)) %in% .spreadsheet_exts &
        !is.na(structure_df$file_location) &
        nzchar(structure_df$file_location) &
        file.exists(structure_df$file_location %||% ""),
      , drop = FALSE
    ]
  } else structure_df[0, , drop = FALSE]

  if (is.null(xl_rows) || nrow(xl_rows) == 0)
    return(empty("We found no spreadsheet files to inspect."))

  n_files <- nrow(xl_rows)

  # ── 2. Inspect each file ─────────────────────────────────────────────────────
  findings <- list()   # one row per (file, sheet, issue)
  per_file <- list()   # per-file issue counts
  for (i in seq_len(n_files)) {
    fname <- xl_rows$file_name[i]
    path  <- xl_rows$file_location[i]
    ext   <- tolower(tools::file_ext(fname))

    # data_check flagged this file as not a usable rectangular dataset (a coding
    # worksheet: mostly free text and/or almost all empty). It is still inspected
    # for formatting below; here we add the structural note so the author knows
    # the file needs restructuring, not just reformatting.
    if (isFALSE(xl_rows$tabular_usable[i])) {
      reason <- xl_rows$non_tabular_reason[i] %||% NA_character_
      findings[[length(findings) + 1L]] <- data.frame(
        File = fname, Sheet = NA_character_, Issue = "Not a rectangular dataset",
        Detail = sprintf(
          "This file reads as a table but is not a usable dataset%s. Store the data as a plain rectangular table (one header row, one column per variable) with a codebook.",
          if (!is.na(reason)) sprintf(" (%s)", reason) else ""))
    }

    # Offset header: a banner / blank / units row above the real column header, so
    # the file does not read as a clean table. Reported so the AUTHOR removes the
    # junk row(s) at source; metacheck also repairs it in-memory for its own checks.
    #
    # This runs for EVERY format, including .xls: it reads the first rows through
    # readxl/readODS rather than the XML, so it does not need the zipped-XML
    # structure the style-level checks below require. (It previously sat after an
    # early `next` for .xls, so legacy files silently skipped a check they can
    # in fact support.)
    oh <- tryCatch(.spreadsheet_offset_header(path, ext), error = function(e) NULL)
    if (!is.null(oh)) {
      findings[[length(findings) + 1L]] <- data.frame(
        File = fname, Sheet = NA_character_, Issue = "Header not on first row",
        Detail = sprintf(
          "The column header is on row %d; above it is %s. Remove the row%s above the header so the first row of the sheet is the column header — otherwise the file reads with invented column names (…1, …4) and the data mis-types.",
          oh$header_row, oh$detail, plural(oh$n_above)))
    }

    # Style-level checks (colour, merges, empty rows/columns) need the document
    # XML. .xlsx and .ods/.fods both provide it; binary .xls does not.
    if (ext == "xls") {
      findings[[length(findings) + 1L]] <- data.frame(
        File = fname, Sheet = NA_character_, Issue = "Un-inspectable (.xls)",
        Detail = "Legacy .xls format: colour, merged cells and empty rows/columns cannot be inspected. Convert to .xlsx or .ods for a full check.")
      next
    }

    insp <- tryCatch(
      if (ext %in% c("ods", "fods")) .ods_inspect(path) else .excel_inspect(path),
      error = function(e) NULL)
    if (is.null(insp)) {
      findings[[length(findings) + 1L]] <- data.frame(
        File = fname, Sheet = NA_character_, Issue = "Unreadable",
        Detail = sprintf("The file could not be parsed as a %s workbook.",
                         if (ext %in% c("ods", "fods")) "OpenDocument" else ".xlsx"))
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
    "This module inspects spreadsheet files (.xlsx, .xls, .ods) for formatting that is not machine-readable (colour coding, merged cells, empty rows, empty/unnamed columns).",
    sprintf("We examined %d spreadsheet file%s in the repository.",
            n_files, plural(n_files))
  )

  if (nrow(findings_df) == 0) {
    report <- c(report,
      "No machine-readability issues were found in the spreadsheet files.")
  } else {
    report <- c(
      report,
      sprintf("%d of %d spreadsheet file%s %s at least one formatting issue.",
              n_flagged_files, n_files, plural(n_files),
              if (n_flagged_files == 1) "has" else "have"),
      "#### Spreadsheet Formatting Issues",
      scroll_table(findings_df, maxrows = 20),
      "Spreadsheet formatting such as colour, merged cells, and blank rows/columns is not preserved when data are read programmatically or exported to CSV. Store data as a plain rectangular table (one header row, one column per variable, no colour-encoded meaning) so it is machine-readable."
    )
  }

  # ── 6. Summary text + table ──────────────────────────────────────────────────
  if (nrow(findings_df) == 0) {
    summary_text <- sprintf(
      "We examined %d spreadsheet file%s and found no machine-readability issues.",
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
      "Across %d spreadsheet file%s we found %s.",
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

# Detect an OFFSET HEADER: a banner / blank / units / repeated-label row sitting
# ABOVE the real column header, so the file does not read as a clean rectangular
# table (the reader takes the junk row as the header and invents …N names, or
# spreads one label — CDA merged across 110 columns — into CDA…1 … CDA…110).
#
# Reuses the same detector as the read-time repair (data_promote_header_row /
# .detect_header_row) so the flag and the repair cannot disagree about where the
# header is. Returns NULL when the header is already row 1, else a one-line human
# description of what sits above it, for the researcher to remove at source.
.spreadsheet_offset_header <- function(path, ext = tolower(tools::file_ext(path))) {
  raw <- if (ext %in% c("ods", "fods")) {
    # readODS is in Suggests: without it an .ods simply skips this check (the
    # xml2-based style checks still run), rather than erroring.
    if (!requireNamespace("readODS", quietly = TRUE)) return(NULL)
    # No `range=`: a fixed range like "A1:Z6" silently TRUNCATES the sheet to 26
    # columns, which would hide the header on a wide export (a 30-column sheet
    # reads back as 26). Read the sheet and cap the rows in R instead.
    tryCatch({
      d <- as.data.frame(suppressWarnings(readODS::read_ods(
        path, col_names = FALSE, .name_repair = "minimal")))
      utils::head(d, 6L)
    }, error = function(e) NULL)
  } else {
    tryCatch(as.data.frame(suppressWarnings(readxl::read_excel(
      path, col_names = FALSE, n_max = 6L, col_types = "text",
      .name_repair = "minimal"))), error = function(e) NULL)
  }
  if (is.null(raw) || nrow(raw) < 2 || ncol(raw) < 2) return(NULL)
  rows <- lapply(seq_len(nrow(raw)), function(i) as.character(raw[i, , drop = TRUE]))
  det  <- .detect_header_row(rows)
  if (det$header_row <= 1L || length(det$stripped) == 0) return(NULL)

  # Describe each stripped row: a repeated banner ("CDA" × 110), a near-empty
  # spacer, or a stale placeholder header. Keep it short and concrete.
  describe <- function(v) {
    vals <- trimws(as.character(v)); nz <- vals[nzchar(vals) & !is.na(vals)]
    if (length(nz) == 0) return("an empty row")
    dup <- .row_duplication(v)
    if (dup >= 0.6 && length(unique(nz)) <= 3)
      return(sprintf("\"%s\" repeated across %d column%s",
                     paste(unique(nz), collapse = "\"/\""),
                     length(nz), plural(length(nz))))
    if (mean(.is_placeholder_name(v)) >= 0.5)
      return("a row of placeholder names from an earlier mis-read")
    sprintf("a partial label row (%s%s)",
            paste(utils::head(nz, 3), collapse = ", "),
            if (length(nz) > 3) ", …" else "")
  }
  descr <- vapply(det$stripped, describe, character(1))
  list(header_row = det$header_row, n_above = length(det$stripped),
       detail = paste(descr, collapse = "; then "))
}

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

# ── OpenDocument (.ods / .fods) ───────────────────────────────────────────────
#
# ODF stores the same facts as OOXML but in a structurally different way, so the
# .excel_* parsers above cannot simply be pointed at it:
#
#   * ALL sheets live in one content.xml (not one sheetN.xml per sheet);
#   * cells carry NO reference attribute (no r="B12") — position is IMPLICIT in
#     document order, so row/column indices must be reconstructed by counting;
#   * blank runs are COMPRESSED: `table:number-columns-repeated="3"` stands for
#     three cells and `table:number-rows-repeated="5"` for five rows. Counting
#     elements naively would report one empty row where there are five;
#   * colour is `fo:background-color` on a named cell style, not a fillId;
#   * merges are `table:number-columns-spanned`/`-rows-spanned` on the anchor
#     cell (followed by <table:covered-table-cell> placeholders), not a
#     ready-made "A1:B1" range string — so the range label is synthesised here
#     to match the xlsx report wording.
#
# Returns the SAME shape as .excel_inspect(): list(sheets = list(list(name,
# color_cells, merges, empty_rows, empty_cols))), so the module body treats the
# two formats identically.
.ods_inspect <- function(path) {
  if (!file.exists(path)) return(NULL)

  # .fods is a single flat XML file; .ods is a zip whose content.xml holds it.
  ext <- tolower(tools::file_ext(path))
  doc <- if (identical(ext, "fods")) {
    tryCatch(xml2::read_xml(path), error = function(e) NULL)
  } else {
    tmp <- tempfile("ods_")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    ok <- tryCatch({ utils::unzip(path, exdir = tmp); TRUE },
                   error = function(e) FALSE)
    if (!ok) return(NULL)
    cx <- file.path(tmp, "content.xml")
    if (!file.exists(cx)) return(NULL)
    tryCatch(xml2::read_xml(cx), error = function(e) NULL)
  }
  if (is.null(doc)) return(NULL)

  colored <- .ods_colored_styles(doc)

  # One <table:table> per sheet, in workbook order.
  tables <- xml2::xml_find_all(doc, ".//*[local-name()='table']")
  # Guard: nested tables (a table inside a cell) would double-count. Keep only
  # tables whose parent is the spreadsheet body.
  keep <- vapply(tables, function(tb) {
    p <- xml2::xml_parent(tb)
    identical(xml2::xml_name(p), "spreadsheet")
  }, logical(1))
  tables <- tables[keep]
  if (length(tables) == 0) return(list(sheets = list()))

  sheets <- lapply(seq_along(tables), function(j) {
    nm <- xml2::xml_attr(tables[[j]], "name")
    if (is.na(nm) || !nzchar(nm)) nm <- paste0("Sheet", j)
    .ods_inspect_sheet(tables[[j]], nm, colored)
  })
  list(sheets = sheets)
}

# Names of cell styles whose background is a real colour. Mirrors
# .excel_colored_styles(): a style counts only if it sets an explicit background
# that is not the default/neutral (transparent, white, black). Both the automatic
# styles (used by cells) and any common styles are scanned.
.ods_colored_styles <- function(doc) {
  sty <- xml2::xml_find_all(
    doc, ".//*[local-name()='style'][@*[local-name()='family']='table-cell']")
  if (length(sty) == 0) return(character(0))
  nm <- xml2::xml_attr(sty, "name")
  bg <- vapply(sty, function(s) {
    p <- xml2::xml_find_first(s, ".//*[local-name()='table-cell-properties']")
    if (inherits(p, "xml_missing")) return(NA_character_)
    xml2::xml_attr(p, "background-color")
  }, character(1))
  is_col <- !is.na(bg) & nzchar(bg) &
    !tolower(bg) %in% c("transparent", "none", "#ffffff", "#fff",
                        "#000000", "#000")
  nm[is_col & !is.na(nm)]
}

# Convert a 1-based column index to spreadsheet letters (1 -> A, 27 -> AA), so
# merged ranges can be reported as "A1:B1" exactly like the xlsx path.
.ods_col_letter <- function(i) {
  out <- character(length(i))
  for (k in seq_along(i)) {
    n <- i[[k]]; s <- ""
    while (n > 0) {
      r <- (n - 1L) %% 26L
      s <- paste0(LETTERS[r + 1L], s)
      n <- (n - 1L) %/% 26L
    }
    out[[k]] <- s
  }
  out
}

# Inspect one <table:table>. Walks rows/cells in document order, expanding the
# repeat counters, and records for each populated cell its (row, col) position —
# reconstructing the coordinates OOXML gives for free. Downstream logic then
# matches .excel_inspect_sheet() exactly.
.ods_inspect_sheet <- function(tbl, name, colored) {
  blank <- list(name = name, color_cells = 0L, merges = character(0),
                empty_rows = 0L, empty_cols = 0L)

  rows <- xml2::xml_find_all(tbl, "./*[local-name()='table-row']")
  if (length(rows) == 0) return(blank)

  int_attr <- function(node, a, default = 1L) {
    v <- suppressWarnings(as.integer(xml2::xml_attr(node, a)))
    if (is.na(v) || v < 1L) default else v
  }

  val_row <- integer(0); val_col <- integer(0)   # populated cell coordinates
  seen_col <- integer(0)                          # every column that appears
  color_cells <- 0L
  merges <- character(0)
  r <- 0L   # 1-based row cursor

  # A trailing run of empty rows is padding (LibreOffice writes rows out to the
  # sheet limit, e.g. number-rows-repeated="1048570"); it is not data structure,
  # and the empty-row count below only looks BETWEEN populated rows anyway.
  for (ri in seq_along(rows)) {
    row <- rows[[ri]]
    rep_r <- int_attr(row, "number-rows-repeated")
    cells <- xml2::xml_find_all(
      row, "./*[local-name()='table-cell' or local-name()='covered-table-cell']")

    cc <- 0L   # 1-based column cursor within this row
    for (ci in seq_along(cells)) {
      cell  <- cells[[ci]]
      rep_c <- int_attr(cell, "number-columns-repeated")
      # Populated = has a value type or any text content (matches the xlsx rule
      # of "<v> or <is> present").
      vt <- xml2::xml_attr(cell, "value-type")
      txt <- trimws(xml2::xml_text(cell))
      populated <- (!is.na(vt) && nzchar(vt)) || nzchar(txt)

      # A huge repeat count on an EMPTY cell is right-padding to the sheet limit;
      # it does not mean thousands of real columns. Only count padding runs when
      # the cell actually holds something.
      span_c <- if (populated) rep_c else min(rep_c, 1024L)

      idx <- cc + seq_len(span_c)
      seen_col <- c(seen_col, idx)

      if (populated) {
        # The same value repeated across `rep_c` columns occupies each of them.
        for (rr in seq_len(rep_r)) {
          val_row <- c(val_row, rep(r + rr, span_c))
          val_col <- c(val_col, idx)
        }
        sn <- xml2::xml_attr(cell, "style-name")
        if (!is.na(sn) && sn %in% colored)
          color_cells <- color_cells + (span_c * rep_r)
      } else {
        sn <- xml2::xml_attr(cell, "style-name")
        # A colour-filled but EMPTY cell still encodes information visually
        # (a shaded block marking a group), so it counts — but only when the
        # run is a plausible real range, not sheet-limit padding.
        if (!is.na(sn) && sn %in% colored)
          color_cells <- color_cells + (span_c * rep_r)
      }

      # Merge anchor: spans recorded on the cell that starts the range.
      sc <- suppressWarnings(as.integer(
        xml2::xml_attr(cell, "number-columns-spanned")))
      sr <- suppressWarnings(as.integer(
        xml2::xml_attr(cell, "number-rows-spanned")))
      sc <- if (is.na(sc)) 1L else sc
      sr <- if (is.na(sr)) 1L else sr
      if (sc > 1L || sr > 1L) {
        merges <- c(merges, sprintf(
          "%s%d:%s%d",
          .ods_col_letter(cc + 1L), r + 1L,
          .ods_col_letter(cc + sc), r + sr))
      }

      cc <- cc + span_c
    }
    r <- r + rep_r
  }

  # Empty rows: blank rows BETWEEN the first and last populated row (identical
  # rule to the xlsx path — trailing padding is not counted).
  empty_rows <- 0L
  if (length(val_row) > 0) {
    populated <- sort(unique(val_row))
    if (length(populated) > 1)
      empty_rows <- (max(populated) - min(populated) + 1L) - length(populated)
  }

  # Empty / unnamed columns: header is the first populated row; a column is
  # problematic if it has no header value, or a header but nothing below it.
  empty_cols <- 0L
  if (length(val_col) > 0) {
    hdr_row  <- min(val_row)
    hdr_cols <- unique(val_col[val_row == hdr_row])
    body_cols <- unique(val_col[val_row > hdr_row])
    # Only consider columns within the used range; padding beyond the last
    # populated column is not a missing column.
    all_cols <- unique(seen_col[seen_col <= max(val_col)])
    blank_header   <- setdiff(all_cols, hdr_cols)
    header_no_body <- setdiff(hdr_cols, body_cols)
    empty_cols <- length(unique(c(blank_header, header_no_body)))
  }

  list(name = name, color_cells = color_cells, merges = merges,
       empty_rows = empty_rows, empty_cols = empty_cols)
}
