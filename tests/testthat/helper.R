# always executed by load_all() and at the beginning of automated testing
# https://r-pkgs.org/testing-design.html#testthat-helper-files

email("metacheck@scienceverse.org")

httptest::.mockPaths(NULL)
apis <- normalizePath("apis")
httptest::.mockPaths(apis)

# adjust to run LLM tests where wanted
skip_llm <- function() {
  #skip("LLM")

  # skips tests if contraindicated
  skip_on_cran()
  skip_on_covr()
  skip_if_offline("api.groq.com")
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

# expect pattern in x
expect_grepl <- function(pattern, x,
                         ignore.case = TRUE,
                         perl = FALSE,
                         fixed = FALSE,
                         useBytes = FALSE) {
  obs <- grepl(pattern, x, ignore.case, perl, fixed, useBytes)
  expect_true(all(obs))
}
