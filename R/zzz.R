## set default options
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.pkg <- list(
    scienceverse.verbose = TRUE,
    papercheck.gpt_max_calls = 100L
  )
  # only set if not already set
  toset <- !(names(op.pkg) %in% names(op))
  if(any(toset)) options(op.pkg[toset])

  invisible()
}
