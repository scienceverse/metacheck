# Module tests for codebook_check and data_validate, run offline via a local
# fixture repo (test_paper + local_path). Rules-only (llm_use(FALSE)).

make_cb_fixture <- function() {
  d <- file.path(tempdir(), paste0("cb_fix_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(id = 1:6, dv = c(2.1, 3.4, 1.9, 4.0, 2.8, 30.0),
               binary = c(0, 1, 0, 1, 0, 1)),
    file.path(d, "data", "study1.csv"), row.names = FALSE)
  writeLines(c("varname,description",
               "dv,dependent variable",
               "binary,condition"),
             file.path(d, "codebook.csv"))
  writeLines("A readme.", file.path(d, "README.txt"))
  d
}

test_that("codebook_check reports documentation coverage", {
  llm_use(FALSE)
  d <- make_cb_fixture()
  ops <- report_module_run(
    test_paper("x"), c("data_check", "codebook_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  cc <- ops[["codebook_check"]]

  expect_true("codebook_check" %in% module_list()$name)
  expect_true(cc$traffic_light %in% c("green", "yellow", "red"))
  # dv + binary are documented; id is not (codebook has no id entry)
  expect_gt(cc$summary_table$matched_n, 0)
  expect_true("group" %in% names(cc$table))
  expect_match(cc$summary_text, "column")
})

test_that("codebook_check is red when no codebook documentation exists", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "cb_nocodebook"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(data.frame(a = 1:5, b = c(1.1, 2.2, 3.3, 4.4, 5.5)),
                   file.path(d, "data", "d.csv"), row.names = FALSE)
  ops <- report_module_run(
    test_paper("x"), c("data_check", "codebook_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  cc <- ops[["codebook_check"]]
  expect_equal(cc$traffic_light, "red")
  expect_equal(cc$summary_table$codebook_var_n, 0)
})

test_that("data_validate flags planted data-quality issues", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "dv_planted"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(data.frame(
    id    = 1:30,
    score = c(rnorm(29, 50, 5), 500),                 # outlier
    grp   = c(rep("Ctrl", 15), rep("ctrl", 14), "X"), # case + sparse
    flat  = rep(1, 30)                                 # constant
  ), file.path(d, "data", "study.csv"), row.names = FALSE)

  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  dv <- ops[["data_validate"]]

  expect_true("data_validate" %in% module_list()$name)
  checks <- dv$table$check
  expect_true("Outliers" %in% checks)
  expect_true("Case issues" %in% checks)
  expect_true("Constant" %in% checks)
  expect_equal(dv$traffic_light, "red")   # several columns flagged
})

test_that("data_validate reports outliers as a table and one combined figure", {
  skip_if_not_installed("ggplot2")
  llm_use(FALSE)
  d <- file.path(tempdir(), "dv_report"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  # Two numeric columns, each with a planted outlier -> both listed in the table
  # and drawn as facets in a single combined figure.
  utils::write.csv(data.frame(
    id = 1:40,
    a  = c(rnorm(39, 10, 1), 100),
    b  = c(rnorm(39, 0, 1), -50)
  ), file.path(d, "data", "study.csv"), row.names = FALSE)

  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  dv <- ops[["data_validate"]]
  report <- paste(dv$report, collapse = "\n")

  # An Outliers section with a per-column summary sentence.
  expect_true(any(grepl("#### Outliers", dv$report, fixed = TRUE)))
  expect_true(grepl("IQR fences", report))

  # A single combined distribution figure: exactly one embedded <img>, not one
  # per column (the old behaviour rendered a plot per numeric column).
  expect_true(any(grepl("#### Distributions", dv$report, fixed = TRUE)))
  n_imgs <- lengths(regmatches(report, gregexpr("<img ", report)))
  expect_equal(n_imgs, 1L)
  expect_true(grepl("data:image/png;base64", report))
})

test_that("data_validate distribution figure caps the number of facets", {
  skip_if_not_installed("ggplot2")
  llm_use(FALSE)
  # Many numeric columns (more than the internal facet cap of 40) -> the figure
  # is truncated and says so, rather than rendering one facet per column.
  d <- file.path(tempdir(), "dv_wide"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  n_cols <- 60L
  wide <- as.data.frame(matrix(stats::rnorm(30 * n_cols), nrow = 30))
  names(wide) <- paste0("v", seq_len(n_cols))
  utils::write.csv(wide, file.path(d, "data", "wide.csv"), row.names = FALSE)

  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  report <- paste(ops[["data_validate"]]$report, collapse = "\n")

  expect_true(grepl("Showing the first", report))
  expect_true(grepl(paste0("of ", n_cols, " numeric columns"), report))
  # Still a single combined figure.
  expect_equal(lengths(regmatches(report, gregexpr("<img ", report))), 1L)
})

test_that("data_validate is green on clean data", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "dv_clean"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  # Evenly spread values (uniform sequence) have no Tukey outliers, so a clean
  # column genuinely produces zero findings. (Random normal data at n=40 would
  # occasionally have a value just past 1.5*IQR — that is correct, not an error.)
  utils::write.csv(data.frame(
    id = 1:40,
    x  = seq(10, 30, length.out = 40),
    y  = seq(1, 5, length.out = 40)
  ), file.path(d, "data", "clean.csv"), row.names = FALSE)
  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  dv <- ops[["data_validate"]]
  expect_equal(dv$traffic_light, "green")
  expect_equal(nrow(dv$table), 0)
})
