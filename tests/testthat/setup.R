# code tailored for test execution in non-interactive environments
# not executed by load_all()
# https://r-pkgs.org/testing-design.html#testthat-setup-files

verbose(FALSE)

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
.mc_old_llm_env <- Sys.getenv("METACHECK_LLM_CACHE_DIR", unset = NA)
Sys.setenv(METACHECK_LLM_CACHE_DIR = file.path(.mc_test_cache, "llm"))

# teardown code
withr::defer(verbose(TRUE))
withr::defer(options(metacheck.osf.cache = TRUE))
withr::defer(options(metacheck.cache.dir = NULL))
withr::defer({
  if (is.na(.mc_old_llm_env)) Sys.unsetenv("METACHECK_LLM_CACHE_DIR")
  else Sys.setenv(METACHECK_LLM_CACHE_DIR = .mc_old_llm_env)
})
withr::defer(unlink(.mc_test_cache, recursive = TRUE))
