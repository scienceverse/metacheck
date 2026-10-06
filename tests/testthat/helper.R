# always executed by load_all() and at the beginning of automated testing
# https://r-pkgs.org/testing-design.html#testthat-helper-files

# if TRUE, skip slow tests and those that need external connections
quick <- FALSE

testthat::set_max_fails(5)

email("metacheck@scienceverse.org")

httptest2::.mockPaths(NULL)
apis <- normalizePath("apis")
httptest2::.mockPaths(apis)

# We now ask OSF for page[size]=100 on every listing request (10x fewer calls).
# The recorded mocks predate that query param, and httptest2 hashes the query
# string into the mock file name (build_mock_url), so the added param would miss
# every fixture. httptest2 applies the current redactor to the request before
# computing the mock path (mock_request: build_mock_url(get_current_redactor()(req))),
# so a redactor that strips our page[size] makes the request resolve to the
# pre-existing recording. The live request still carries the param.
httptest2::set_redactor(function(req) {
  req$url <- gsub("[?&]page(%5[Bb]size%5[Dd]|\\[size\\])=100", "", req$url)
  req
})

# Load the ui/server objects from a shiny app file in inst/app/ without
# launching the app. The app files end in `shinyApp(ui, server)`; we source
# everything before that line (with the working directory set to the app dir,
# so their relative source() calls resolve) and return the environment holding
# `ui` and `server` for use with shiny::testServer().
load_app_env <- function(app_file) {
  appdir <- system.file("app", package = "metacheck")
  testthat::skip_if(appdir == "", "metacheck app dir not installed")
  path <- file.path(appdir, app_file)
  testthat::skip_if_not(file.exists(path), paste("missing app file:", app_file))

  old <- setwd(appdir)
  on.exit(setwd(old))

  code <- readLines(path)
  end  <- grep("^shinyApp", code)
  if (length(end) == 0) end <- length(code) + 1

  env <- new.env(parent = globalenv())
  with_mocked_bindings(
    llm_model_list = \(...) data.frame(),
    eval(parse(text = paste(code[seq_len(end - 1)], collapse = "\n")), envir = env)
  )
  env
}

# A fresh temp directory for a codebook_check fixture, removed when the
# calling test finishes. Lives here (not in test-codebook-helpers.R, where it
# was previously defined but never itself called) because
# test-module-codebook_check.R depends on it too: a helper shared across test
# files must live in helper.R, which testthat guarantees is sourced before
# any test file runs under every runner (devtools::test(), test_file() on a
# single file, test_dir()) -- unlike a definition left inside another test-*.R
# file, which only happens to be visible if that other file ran first in the
# same session.
cbc_dir <- function(name) {
  d <- file.path(tempdir(), paste0("cbc_", name, "_",
                                   as.integer(runif(1, 1, 1e6))))
  unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(d, recursive = TRUE), envir = parent.frame())
  d
}

# Run data_check then codebook_check over a local fixture directory. See
# cbc_dir()'s own comment for why this lives in helper.R.
cbc_run <- function(d, paper = test_paper("x"), ...) {
  report_module_run(
    paper, c("data_check", "codebook_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE),
                codebook_check = list(...)))[["codebook_check"]]
}

# mock function
test_that <- function(desc, code, mock = "none") {
  Sys.setenv("MOCK_CAPTURE" = "FALSE")
  if (mock == "mock") {
    httptest2::use_mock_api()
    on.exit(httptest2::stop_mocking())
  } else if (mock == "capture") {
    Sys.setenv("MOCK_CAPTURE" = "TRUE")
    httptest2::start_capturing()
    on.exit(httptest2::stop_capturing())
  }

  # message the test description on elapsed time > 4
  time <- system.time( testthat::test_that(desc, code) )
  s <- round(time[['elapsed']], 1)
  if (s > 4) message(s, ": ", desc)
}

# grobid_url <- "http://localhost:8070"
grobid_url <- "https://grobid.hti.ieis.tue.nl"
# grobid_url <- "https://grobidorg-grobid.hf.space/"
bibr_url <- "https://platform.metacheck.app"

# change fancy quotes to straight for text matching with crossref
fix_fancy <- function(x) {
  x |>
    gsub("[\u2018\u2019\u201A\u201B\u0060]", "'", x = _) |>
    gsub("[\u201C\u201D\u201E\u201F]", '"', x = _) |>
    gsub("–", "-", x = _)
}

# skip functions ----------------------------------------

skip_shiny <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyjs")
  skip_if_not_installed("shinydashboard")
  skip_if_not_installed("DT")
  skip_on_cran()
}

skip_api <- function(host = "google.com") {
  if (quick) skip("API")
  skip_on_cran()
  skip_on_covr()

  is_online <- tryCatch({
    res <- curl::curl_fetch_memory(host)
    res$status_code < 400
  }, error = \(e) return(FALSE))

  skip_if_not(is_online)
}

# adjust to run LLM tests where wanted
skip_llm <- function() {
  if (quick) skip("LLM")
  skip_on_cran()
  skip_on_covr()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("GROQ_API_KEY")), "No GROQ_API_KEY set")
}

# skip if requires OSF API
skip_osf <- function() {
  if (quick) skip("OSF")
  skip_on_cran()
  skip_on_covr()
  skip_if_offline("api.osf.io")
  skip_if_not(osf_api_check() == "ok", "OSF API unavailable")
}

# skip when running quick checks
skip_if_quick <- function() {
  if (quick) skip("Quick mode")
}

# skip if the psychsci test corpus couldn't be downloaded (see setup.R)
skip_no_psychsci <- function() {
  skip_if_not(exists("psychsci") && !is.null(get("psychsci")),
              "psychsci test corpus not available")
}

# codebook_check / data_check local-fixture helpers --------------------------
# These must live in THIS file, not in an ordinary test-*.R file: testthat
# sources every test-*.R file into its OWN private child environment
# (test_one_file()'s `env(env)`), so a top-level function assigned in one
# test file is never visible to another, however they are ordered -- only
# helper-*.R/helper.R (sourced directly into the shared environment by
# source_test_helpers()) is visible everywhere. Confirmed live: cbc_dir()
# used to be defined in test-codebook-helpers.R, where it worked for tests in
# THAT file (sourced together with its own definition) but threw "could not
# find function" for every test in test-module-codebook_check.R that called
# it, under this project's actual test runner (devtools::test()).

# A fresh temp directory, removed when the calling test finishes.
cbc_dir <- function(name) {
  d <- file.path(tempdir(), paste0("cbc_", name, "_",
                                   as.integer(runif(1, 1, 1e6))))
  unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(d, recursive = TRUE), envir = parent.frame())
  d
}

# Run data_check then codebook_check over a local fixture directory.
cbc_run <- function(d, paper = test_paper("x"), ...) {
  report_module_run(
    paper, c("data_check", "codebook_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE),
                codebook_check = list(...)))[["codebook_check"]]
}




