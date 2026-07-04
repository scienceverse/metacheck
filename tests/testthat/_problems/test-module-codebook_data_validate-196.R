# Extracted from test-module-codebook_data_validate.R:196

# prequel ----------------------------------------------------------------------
.cb_env <- new.env()
sys.source(metacheck:::module_find("codebook_check"), envir = .cb_env)
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

# test -------------------------------------------------------------------------
cols <- data.frame(
    source_file = "s.csv", column_name = c("rt", "cond"),
    col_type = c("continuous", "binary"), min = c(200, 1), max = c(900, 2),
    n = 40, mean = c(550, NA), stringsAsFactors = FALSE)
vm <- metacheck:::.psychds_variable_measured(cols, NULL)
expect_equal(vm[[1]][["metacheck:representation"]], "numeric")
