# always executed by load_all() and at the beginning of automated testing
# https://r-pkgs.org/testing-design.html#testthat-helper-files

# if TRUE, skip slow tests and those that need external connections
quick <- FALSE

testthat::set_max_fails(1)

email("metacheck@scienceverse.org")

httptest::.mockPaths(NULL)
apis <- normalizePath("apis")
httptest::.mockPaths(apis)

# grobid_url <- "http://localhost:8070"
grobid_url <- "https://grobid.metacheck.app"
bibr_url <- "https://platform.metacheck.app"

# change fancy quotes to straight for text matching with crossref
fix_fancy <- function(x) {
  x |>
    gsub("[\u2018\u2019\u201A\u201B\u0060]", "'", x = _) |>
    gsub("[\u201C\u201D\u201E\u201F]", '"', x = _) |>
    gsub("–", "-", x = _)
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
