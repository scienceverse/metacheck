# always executed by load_all() and at the beginning of automated testing
# https://r-pkgs.org/testing-design.html#testthat-helper-files

testthat::set_max_fails(10)

email("metacheck@scienceverse.org")

httptest::.mockPaths(NULL)
apis <- normalizePath("apis")
httptest::.mockPaths(apis)

skip_api <- function(host = "google.com") {
  # skip("API")
  skip_on_cran()
  skip_on_covr()
  skip_if_offline(host)
}

# adjust to run LLM tests where wanted
skip_llm <- function() {
  # skip("LLM")
  skip_on_cran()
  skip_on_covr()
  skip_if_offline()
  skip_if(!nzchar(Sys.getenv("GROQ_API_KEY")), "No GROQ_API_KEY set")
}

# skip if requires OSF API
skip_osf <- function() {
  # skip("OSF")
  skip_on_cran()
  skip_on_covr()
  skip_if_offline()
  skip_if_not(osf_api_check() == "ok", "OSF API unavailable")
}

# skip when running quick checks
skip_if_quick <- function() {
  skip_if(
    identical(Sys.getenv("METACHECK_QUICK"), "true"),
    "Quick mode — skipping slow tests"
  )
}
