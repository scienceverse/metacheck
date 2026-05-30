#' Less scary green messages
#'
#' @param ... message components (see \code{\link[base]{message}})
#' @param domain (see \code{\link[base]{message}})
#' @param appendLF append new line? (see \code{\link[base]{message}})
#'
#' @return TRUE
#' @export
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
