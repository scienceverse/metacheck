# Extracted from test-module-codebook_data_validate.R:125

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
llm_use(TRUE)
llm_called <- FALSE
testthat::local_mocked_bindings(
    llm = function(...) { llm_called <<- TRUE; stop("llm should not be called") }
  )
d <- file.path(tempdir(), "cb_scale_gate")
unlink(d, recursive = TRUE)
dir.create(file.path(d, "data"), recursive = TRUE)
set.seed(3)
mk <- function(prefix, file) {
    items <- as.data.frame(matrix(sample(1:5, 40 * 8, replace = TRUE), nrow = 40))
    names(items) <- paste0(prefix, "_", 1:8)
    utils::write.csv(cbind(id = 1:40, items),
                     file.path(d, "data", file), row.names = FALSE)
  }
mk("panas", "a.csv")
mk("rosenberg", "b.csv")
mk("bigfive", "c.csv")
ops <- suppressWarnings(report_module_run(
    test_paper("x"), c("data_check", "codebook_check"),
    args = list(
      data_check     = list(local_path = d, local_only = TRUE),
      codebook_check = list(codebook_max_calls = 1))))
cb <- ops[["codebook_check"]]
expect_false(llm_called)
