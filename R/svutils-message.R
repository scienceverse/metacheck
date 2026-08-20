#' Less scary green messages
#'
#' Metacheck's replacement for [base::message()]: prints in green when run
#' interactively (so it reads as informational rather than a warning), and
#' is silent entirely when [verbose()] is `FALSE`.
#'
#' @param ... message components (see \code{\link[base]{message}})
#' @param domain (see \code{\link[base]{message}})
#' @param appendLF append new line? (see \code{\link[base]{message}})
#'
#' @return TRUE
#' @export
#' @keywords internal
message <- function(..., domain = NULL, appendLF = TRUE) {
  if (verbose()) {
    if (interactive()) {
      # not in knitr environment
      base::message("\033[32m", ..., "\033[39m",
                    domain = domain, appendLF = appendLF
      )
    } else {
      base::message(..., domain = domain, appendLF = appendLF)
    }
  }
}
