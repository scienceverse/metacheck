## set default options
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.pkg <- list(
    papercheck.verbose = TRUE,
    papercheck.llm_max_calls = 30L,
    papercheck.llm.model = "llama3-70b-8192"
  )
  # only set if not already set
  toset <- !(names(op.pkg) %in% names(op))
  if(any(toset)) options(op.pkg[toset])

  invisible()
}

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
