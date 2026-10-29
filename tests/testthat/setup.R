# code tailored for test execution in non-interactive environments
# not executed by load_all()
# https://r-pkgs.org/testing-design.html#testthat-setup-files

# metacheck's messages stay on: many tests check them (verbose(FALSE) here was
# undone straight away until 2026-10-29, so the tests were written with them on)

# Disable the session OSF listing cache during tests: it would let one test
# reuse another's listing and skip the httptest2-mocked request, breaking mock
# expectations. Production default stays TRUE (set in zzz.R).
options(metacheck.osf.cache = FALSE)

# Redirect BOTH on-disk caches to a throwaway temp dir for the whole test run.
# The production default is now a folder in the working directory, so without
# this a test could write .metacheck_repo_cache / .metacheck_llm_cache into the
# package source — or (with clearing) delete a real one. A single shared
# override (metacheck.cache.dir) moves both; the LLM env var is set too since it
# takes precedence over the option in .llm_cache_dir().
.mc_test_cache <- file.path(tempdir(), "metacheck-test-cache")
dir.create(.mc_test_cache, showWarnings = FALSE, recursive = TRUE)
options(metacheck.cache.dir = .mc_test_cache)
# metacheck's state folder (key problems, ResearchBox's count of requests for
# boxes it could not find) is shared by every R session of the user: tests
# write theirs to the throwaway folder too.
options(metacheck.state.dir = file.path(.mc_test_cache, "state"))
.mc_old_llm_env <- Sys.getenv("METACHECK_LLM_CACHE_DIR", unset = NA)
Sys.setenv(METACHECK_LLM_CACHE_DIR = file.path(.mc_test_cache, "llm"))

# teardown code, run when all tests have run. teardown_env() is needed: without
# it, each reset runs as soon as this file has run, before the first test, and
# the tests would write to the package source and the user's own state folder.
withr::defer(options(metacheck.osf.cache = TRUE), teardown_env())
withr::defer(options(metacheck.cache.dir = NULL), teardown_env())
withr::defer(options(metacheck.state.dir = NULL), teardown_env())
withr::defer({
  if (is.na(.mc_old_llm_env)) Sys.unsetenv("METACHECK_LLM_CACHE_DIR")
  else Sys.setenv(METACHECK_LLM_CACHE_DIR = .mc_old_llm_env)
}, teardown_env())
withr::defer(unlink(.mc_test_cache, recursive = TRUE), teardown_env())

# psychsci test corpus ---------------------------------------------------
# psychsci was removed from data/ (LazyData) to reduce package size and is
# now downloaded on demand from the GitHub release and cached in the same
# directory metacheck already uses for other downloaded data
# (see rappdirs::user_data_dir() usage in R/db-retractionwatch.R).
#
# If it can't be downloaded (e.g. no internet, as on CRAN), tests that need
# it are skipped via skip_if_no_psychsci(), not failed.

psychsci_cache_path <- rappdirs::user_data_dir("metacheck", "scienceverse") |>
  file.path("psychsci.rds")

if (!file.exists(psychsci_cache_path)) {
  dir.create(dirname(psychsci_cache_path), showWarnings = FALSE, recursive = TRUE)
  tryCatch(
    curl::curl_download(
      "https://github.com/scienceverse/papers/releases/download/psychsci/psychsci.rds",
      psychsci_cache_path
    ),
    error = function(e) NULL
  )
}

if (file.exists(psychsci_cache_path)) {
  psychsci <- tryCatch(readRDS(psychsci_cache_path), error = function(e) NULL)
} else {
  psychsci <- NULL
}

if (is.null(psychsci)) {
  unlink(psychsci_cache_path) # remove any partial/corrupt download
  message("psychsci test corpus not available (download failed); dependent tests will be skipped")
}
