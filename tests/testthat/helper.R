# always executed by load_all() and at the beginning of automated testing
# https://r-pkgs.org/testing-design.html#testthat-helper-files

# if TRUE, skip slow tests and those that need external connections
quick <- TRUE

testthat::set_max_fails(5)

email("metacheck@scienceverse.org")

httptest2::.mockPaths(NULL)
apis <- normalizePath("apis")
httptest2::.mockPaths(apis)

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
