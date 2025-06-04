## set default options
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.pkg <- list(
    papercheck.verbose = TRUE,
    papercheck.llm_max_calls = 30L,
    papercheck.llm.model = "llama-3.3-70b-versatile"
  )
  # only set if not already set
  toset <- !(names(op.pkg) %in% names(op))
  if(any(toset)) options(op.pkg[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  stripe <- paste0(
    "\033[31m***\033[39m",
    "\033[33m***\033[39m",
    "\033[32m***\033[39m",
    "\033[34m***\033[39m",
    "\033[35m***\033[39m"
  )
  paste(
    "\n",
    stripe,
    "Welcome to PaperCheck. For support and examples visit:",
    "https://scienceverse.github.io/papercheck/",
    "\\26A0 This software is an alpha version, so please check any results. ",
    "\\26A0 False positives and false negatives will occur at unknown rates.",
    stripe,
    sep = "\n"
  ) %>% packageStartupMessage()
}

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
