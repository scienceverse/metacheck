# Tests for the spreadsheet_check module: flags non-machine-readable spreadsheet
# formatting (colour coding, merged cells, empty rows, empty/unnamed columns).
# Runs offline against fixture files built in tempdir(); no network, no LLM.
# Requires openxlsx to build the .xlsx fixtures.

skip_if_not_installed("openxlsx")

# Build a repository fixture with one "messy" and one "clean" Excel file.
make_excel_repo <- function() {
  d <- file.path(tempdir(), paste0("xl_fix_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)

  # Messy workbook: colour-coded cells, a merged range, an all-empty column.
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data")
  df <- data.frame(id = 1:4, grp = c("a", "b", "a", "b"),
                   empty = rep(NA, 4), val = c(10, 20, 30, 40))
  openxlsx::writeData(wb, "Data", df)
  openxlsx::addStyle(wb, "Data", openxlsx::createStyle(fgFill = "#FFCC00"),
                     rows = 2, cols = 4)
  openxlsx::addStyle(wb, "Data", openxlsx::createStyle(fgFill = "#00CCFF"),
                     rows = 3, cols = 4)
  openxlsx::mergeCells(wb, "Data", cols = 1:2, rows = 7)
  openxlsx::saveWorkbook(wb, file.path(d, "data", "messy.xlsx"), overwrite = TRUE)

  # Clean workbook: a plain rectangular table.
  openxlsx::write.xlsx(data.frame(id = 1:3, score = c(1.1, 2.2, 3.3)),
                       file.path(d, "data", "clean.xlsx"))
  d
}

test_that("spreadsheet_check flags colour, merges and empty columns", {
  llm_use(FALSE)
  d <- make_excel_repo()
  op <- module_run(test_paper("x"), "spreadsheet_check",
                   local_path = d, local_only = TRUE)

  expect_equal(op$traffic_light, "yellow")
  st <- op$summary_table
  expect_equal(st$file_n, 2)
  expect_equal(st$flagged_file_n, 1)          # only messy.xlsx has issues
  expect_gt(st$color_n, 0)
  expect_gt(st$merge_n, 0)
  expect_gt(st$empty_col_n, 0)

  # The findings table names each issue type for the messy file.
  issues <- op$table$Issue
  expect_true(any(grepl("Colour", issues)))
  expect_true(any(grepl("Merged", issues)))
  expect_true(any(grepl("Empty or unnamed", issues)))
  # Scope is always reported.
  expect_true(any(grepl("examined 2 spreadsheet files", op$report)))
})

test_that("spreadsheet_check is green when Excel files are clean", {
  llm_use(FALSE)
  d <- file.path(tempdir(), paste0("xl_clean_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  openxlsx::write.xlsx(data.frame(id = 1:3, score = c(1.1, 2.2, 3.3)),
                       file.path(d, "data", "clean.xlsx"))

  op <- module_run(test_paper("x"), "spreadsheet_check",
                   local_path = d, local_only = TRUE)
  expect_equal(op$traffic_light, "green")
  expect_equal(nrow(op$table), 0)
  expect_true(any(grepl("No machine-readability issues", op$report)))
})

test_that("spreadsheet_check returns na when there are no Excel files", {
  llm_use(FALSE)
  d <- file.path(tempdir(), paste0("xl_none_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(id = 1:3), file.path(d, "data", "x.csv"),
                   row.names = FALSE)

  op <- module_run(test_paper("x"), "spreadsheet_check",
                   local_path = d, local_only = TRUE)
  expect_equal(op$traffic_light, "na")
  expect_true(any(grepl("no spreadsheet files", op$summary_text)))
})

# ── OpenDocument (.ods) ───────────────────────────────────────────────────────
#
# The .ods fixtures are written as raw ODF XML rather than through a writer:
# readODS::write_ods() cannot produce cell fills or merged ranges, which are
# exactly the features under test. Writing the XML also pins the two structures
# that make ODS different from OOXML — implicit cell positions and the
# `number-rows-repeated` / `number-columns-repeated` counters that compress
# blank runs — so a regression in the counter expansion is caught here.
.write_ods_fixture <- function(path, content_xml) {
  build <- file.path(tempdir(), paste0("odsb_", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(build, "META-INF"), recursive = TRUE,
             showWarnings = FALSE)
  writeLines("application/vnd.oasis.opendocument.spreadsheet",
             file.path(build, "mimetype"), sep = "")
  writeLines(paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0" manifest:version="1.2">',
    '<manifest:file-entry manifest:full-path="/" manifest:media-type="application/vnd.oasis.opendocument.spreadsheet"/>',
    '<manifest:file-entry manifest:full-path="content.xml" manifest:media-type="text/xml"/>',
    '</manifest:manifest>'), file.path(build, "META-INF", "manifest.xml"))
  writeLines(content_xml, file.path(build, "content.xml"))

  wd <- setwd(build)
  on.exit(setwd(wd), add = TRUE)
  utils::zip(path, c("mimetype", "META-INF/manifest.xml", "content.xml"),
             flags = "-r9Xq")
  path
}

.ods_header <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<office:document-content',
  ' xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"',
  ' xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0"',
  ' xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"',
  ' xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"',
  ' xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"',
  ' office:version="1.2">')

test_that("spreadsheet_check flags colour and merges in .ods files", {
  llm_use(FALSE)
  skip_if_not_installed("readODS")

  d <- file.path(tempdir(), paste0("ods_fix_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)

  # A merged banner (A1:B1), one red cell, and three blank rows written as a
  # SINGLE row element with number-rows-repeated="3".
  messy <- paste0(.ods_header,
    '<office:automatic-styles>',
    '<style:style style:name="ceRed" style:family="table-cell">',
    '<style:table-cell-properties fo:background-color="#ff0000"/></style:style>',
    # white/transparent must NOT count as colour coding
    '<style:style style:name="ceWhite" style:family="table-cell">',
    '<style:table-cell-properties fo:background-color="#ffffff"/></style:style>',
    '</office:automatic-styles>',
    '<office:body><office:spreadsheet>',
    '<table:table table:name="Data">',
    '<table:table-row>',
    '<table:table-cell table:number-columns-spanned="2" table:number-rows-spanned="1" office:value-type="string"><text:p>banner</text:p></table:table-cell>',
    '<table:covered-table-cell/>',
    '<table:table-cell office:value-type="string"><text:p>val</text:p></table:table-cell>',
    '</table:table-row>',
    '<table:table-row>',
    '<table:table-cell table:style-name="ceRed" office:value-type="float" office:value="1"><text:p>1</text:p></table:table-cell>',
    '<table:table-cell table:style-name="ceWhite" office:value-type="float" office:value="2"><text:p>2</text:p></table:table-cell>',
    '<table:table-cell office:value-type="float" office:value="3"><text:p>3</text:p></table:table-cell>',
    '</table:table-row>',
    '<table:table-row table:number-rows-repeated="3"><table:table-cell table:number-columns-repeated="3"/></table:table-row>',
    '<table:table-row>',
    '<table:table-cell office:value-type="float" office:value="9"><text:p>9</text:p></table:table-cell>',
    '<table:table-cell table:number-columns-repeated="2"/>',
    '</table:table-row>',
    '</table:table></office:spreadsheet></office:body></office:document-content>')
  .write_ods_fixture(file.path(d, "data", "messy.ods"), messy)

  op <- module_run(test_paper("x"), "spreadsheet_check",
                   local_path = d, local_only = TRUE)

  expect_equal(op$traffic_light, "yellow")
  st <- op$summary_table
  expect_equal(st$file_n, 1)
  # Exactly one coloured cell: the red one. The white fill is neutral and the
  # unstyled cells must not be counted.
  expect_equal(st$color_n, 1)
  expect_equal(st$merge_n, 1)
  # The blank run is number-rows-repeated="3", so counting row ELEMENTS would
  # give 1. Correct expansion yields 3 empty rows between row 2 and row 6.
  expect_equal(st$empty_row_n, 3)

  issues <- op$table$Issue
  expect_true(any(grepl("Colour", issues)))
  expect_true(any(grepl("Merged", issues)))
  # The merge range is synthesised into the same A1:B1 form the .xlsx path uses.
  expect_true(any(grepl("A1:B1", op$table$Detail)))
  expect_true(any(grepl("examined 1 spreadsheet file\\b", op$report)))
})

test_that("spreadsheet_check is green for a clean .ods file", {
  llm_use(FALSE)
  skip_if_not_installed("readODS")

  d <- file.path(tempdir(), paste0("ods_clean_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)

  clean <- paste0(.ods_header,
    '<office:body><office:spreadsheet><table:table table:name="Data">',
    '<table:table-row>',
    '<table:table-cell office:value-type="string"><text:p>id</text:p></table:table-cell>',
    '<table:table-cell office:value-type="string"><text:p>score</text:p></table:table-cell>',
    '</table:table-row>',
    '<table:table-row>',
    '<table:table-cell office:value-type="float" office:value="1"><text:p>1</text:p></table:table-cell>',
    '<table:table-cell office:value-type="float" office:value="1.1"><text:p>1.1</text:p></table:table-cell>',
    '</table:table-row>',
    '<table:table-row>',
    '<table:table-cell office:value-type="float" office:value="2"><text:p>2</text:p></table:table-cell>',
    '<table:table-cell office:value-type="float" office:value="2.2"><text:p>2.2</text:p></table:table-cell>',
    '</table:table-row>',
    '</table:table></office:spreadsheet></office:body></office:document-content>')
  .write_ods_fixture(file.path(d, "data", "clean.ods"), clean)

  op <- module_run(test_paper("x"), "spreadsheet_check",
                   local_path = d, local_only = TRUE)
  expect_equal(op$traffic_light, "green")
  expect_equal(nrow(op$table), 0)
})

test_that("data_read_head reads .ods like .xlsx", {
  skip_if_not_installed("readODS")
  p <- file.path(tempdir(), paste0("rh_", as.integer(runif(1, 1, 1e6)), ".ods"))
  readODS::write_ods(data.frame(id = 1:4, grp = c("a", "b", "a", "b")), p)

  df <- data_read_head(p, n_rows = 3)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)
  expect_equal(names(df), c("id", "grp"))
})

test_that(".ods is treated as tabular data", {
  expect_equal(data_format("ods"), "tabular")
  expect_equal(data_format("fods"), "tabular")
})
