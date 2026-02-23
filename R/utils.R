#' Default value for `NULL`
#'
#' This infix function makes it easy to replace `NULL`s with a default value. It's inspired by the way that Ruby's or operation (`||`) works.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns `x`.
#' @export
#' @keywords internal
#' @name op-null-default
#' @examples
#' 1 %||% 2
#' NULL %||% 2
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Reexport from base on newer versions of R to avoid conflict messages
if (exists("%||%", envir = baseenv())) {
  `%||%` <- get("%||%", envir = baseenv())
}


#' Less scary green messages
#'
#' @param ... message components (see \code{\link[base]{message}})
#' @param domain (see \code{\link[base]{message}})
#' @param appendLF append new line? (see \code{\link[base]{message}})
#'
#' @return TRUE
#' @keywords internal
#'
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

#' Set or get metacheck verbosity
#'
#' @param verbose if logical, sets whether to show verbose output messages and progress bars
#'
#' @returns the current option value (logical)
#' @export
#'
#' @examples
#' verbose()
verbose <- function(verbose = NULL) {
  if (is.null(verbose)) {
    return(getOption("metacheck.verbose"))
  } else if (as.logical(verbose) %in% c(TRUE, FALSE)) {
    options(metacheck.verbose = as.logical(verbose))
    invisible(getOption("metacheck.verbose"))
  } else {
    stop("set verbose with TRUE or FALSE")
  }
}

#' Set or get email
#'
#' @param email if a string, sets the email
#'
#' @returns the current option value (character)
#' @export
#'
#' @examples
#' email()
email <- function(email = NULL) {
  if (is.null(email)) {
    email <- getOption("metacheck.email") %||% "metacheck@scienceverse.org"
    return(email)
  } else if (is.character(email) && grepl(".+@.+\\..+$", email)) {
    options(metacheck.email = email)
    invisible(getOption("metacheck.email"))
  } else {
    stop("Set email with a valid email address")
  }
}

#' Check if the host of a URL is online
#'
#' @param url a URL to check
#'
#' @returns boolean
#' @export
#'
#' @examples
#' online()
online <- function(url = "google.com") {
  host <- urltools::domain(url)
  !is.null(curl::nslookup(host, error = FALSE))
}



#' Psychological Science Open Access Paper Set
#'
#' 250 open access papers from Psychological Science.
#'
#' @format A list of 250 paper objects
#' @source \url{https://journals.sagepub.com/home/pss}
"psychsci"


#' Progress Bar
#'
#' @param total total number of ticks
#' @param format The format of the progress bar
#'
#' @returns a function
#' @export
pb <- function(total, format = "[:bar] :percent") {
  if (verbose()) {
    pb <- progress::progress_bar$new(
      total = total, clear = FALSE,
      format = format,
      show_after = 0
    )
    pb$tick(0)
    # Sys.sleep(0.2)
    # pb$tick(0)
  } else {
    # dummy functions so we don't have to call if (verbose())
    pb <- list(
      tick = function(...) {
        invisible()
      },
      message = function(...) {
        invisible()
      },
      terminate = function(...) {
        invisible()
      }
    )
  }

  return(pb)
}



