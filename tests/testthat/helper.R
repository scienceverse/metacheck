# always executed by load_all() and at the beginning of automated testing
# https://r-pkgs.org/testing-design.html#testthat-helper-files

testthat::set_max_fails(1)

email("metacheck@scienceverse.org")

httptest::.mockPaths(NULL)
apis <- normalizePath("apis")
httptest::.mockPaths(apis)

skip_api <- function(host = "google.com") {
  skip("Uses External API")
  skip_on_cran()
  skip_on_covr()
  skip_if_offline(host)
}

# adjust to run LLM tests where wanted
skip_llm <- function() {
  skip("LLM")

  # skips tests if contraindicated
  skip_on_cran()
  skip_on_covr()
  skip_if_offline()
}

# skip if requires OSF API
skip_osf <- function() {
  skip("Requires OSF") # skips all tests that require API

  # skips tests if contraindicated
  skip_if_offline()
  skip_on_cran()
  skip_on_covr()
  skip_if_not(osf_api_check() == "ok")
}

# skip when running quick checks
skip_if_quick <- function() {
  skip("Too long")
}
