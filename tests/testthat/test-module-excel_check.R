# Tests for the excel_check module: flags non-machine-readable Excel formatting
# (colour coding, merged cells, empty rows, empty/unnamed columns). Runs offline
# against fixture .xlsx files built with openxlsx in tempdir(); no network, no
# LLM. Requires openxlsx to build the fixtures.

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

test_that("excel_check flags colour, merges and empty columns", {
  llm_use(FALSE)
  d <- make_excel_repo()
  op <- module_run(test_paper("x"), "excel_check",
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
  expect_true(any(grepl("examined 2 Excel files", op$report)))
})

test_that("excel_check is green when Excel files are clean", {
  llm_use(FALSE)
  d <- file.path(tempdir(), paste0("xl_clean_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  openxlsx::write.xlsx(data.frame(id = 1:3, score = c(1.1, 2.2, 3.3)),
                       file.path(d, "data", "clean.xlsx"))

  op <- module_run(test_paper("x"), "excel_check",
                   local_path = d, local_only = TRUE)
  expect_equal(op$traffic_light, "green")
  expect_equal(nrow(op$table), 0)
  expect_true(any(grepl("No machine-readability issues", op$report)))
})

test_that("excel_check returns na when there are no Excel files", {
  llm_use(FALSE)
  d <- file.path(tempdir(), paste0("xl_none_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(id = 1:3), file.path(d, "data", "x.csv"),
                   row.names = FALSE)

  op <- module_run(test_paper("x"), "excel_check",
                   local_path = d, local_only = TRUE)
  expect_equal(op$traffic_light, "na")
  expect_true(any(grepl("no Excel files", op$summary_text)))
})
