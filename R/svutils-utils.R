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

#' Replace If
#'
#' Replace values if NULL, NA, or specified value
#'
#' @param x For each `x[[i]]` if a value in `replace`, return `y[[i]]`; otherwise return `x[[i]]`.
#' @param y Replacement value(s); if not the same length as `x`, then values will be recycled
#' @param replace values in `x` to replace
#' @export
#' @keywords internal
#' @examples
#' rep_if(list(NULL, 1), 2)
#' rep_if(list(NULL, 0, NULL), 1:3)
rep_if <- function(x, y, replace = NULL) {
  y <- rep_len(y, length(x))
  for (i in seq_along(x)) {
    x[[i]] <- if (is.null(x[[i]]) || x[[i]] %in% replace) y[[i]] else x[[i]]
  }

  return(x)
}

#' Set or get verbosity
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
    v <- getOption("scienceverse.verbose") %||% TRUE
    return(v)
  } else if (as.logical(verbose) %in% c(TRUE, FALSE)) {
    options(scienceverse.verbose = as.logical(verbose))
    invisible(getOption("scienceverse.verbose"))
  } else {
    stop("set verbose with TRUE or FALSE")
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
  #host <- urltools::domain(url)
  url <- ifelse(grepl("^[a-zA-Z]+://", url), url, paste0("http://", url))
  host <- sub("^[a-zA-Z]+://([^/]+).*", "\\1", url)

  !is.null(curl::nslookup(host, error = FALSE))
}
