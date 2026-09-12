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

#' Default value for `NULL` or length-zero values
#'
#' Like [`%||%`], but also substitutes `y` when `x` has length zero (for
#' example an empty array in a parsed JSON API response), not only when `x`
#' is `NULL`. This matters when `x` feeds a single `data.frame()` column
#' alongside scalar values: a length-zero `x` (as opposed to `NULL`) passes
#' `%||%` unchanged and then throws "arguments imply differing number of
#' rows" (in `data.frame()`) or "replacement has 0 rows" (in `$<-`), which
#' typically aborts and silently drops the whole record it belongs to.
#'
#' @param x,y If `x` is `NULL` or has length 0, will return `y`; otherwise returns `x`.
#' @export
#' @keywords internal
#' @name op-empty-default
#' @examples
#' 1 %empty_or% 2
#' NULL %empty_or% 2
#' character(0) %empty_or% 2
`%empty_or%` <- function(x, y) {
  if (length(x) == 0) y else x
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
#' Get or set whether metacheck's own functions (e.g. [message()], [pb()])
#' print progress messages and progress bars. Call with no argument to read
#' the current value; call with `TRUE`/`FALSE` to set it for the rest of the
#' session. Defaults to `TRUE` when never set.
#'
#' @param verbose if logical, sets whether to show verbose output messages and progress bars
#'
#' @returns the current option value (logical)
#' @export
#' @keywords internal
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
#' Resolves the URL's host via DNS lookup to check whether it is reachable.
#' A scheme (`http://`/`https://`) is added automatically if `url` doesn't
#' have one. This only confirms the host resolves, not that the specific
#' page or API endpoint responds.
#'
#' A single DNS lookup can fail transiently (a brief resolver hiccup) even
#' when the host is fine, so this retries a few times with a short pause
#' before reporting the host as unreachable. Many archive modules use this
#' as a hard pre-flight gate (`stop()` if offline) before an entire batch of
#' otherwise-unrelated records, so one flaky lookup here previously aborted
#' every one of them.
#'
#' @param url a URL to check
#' @param tries number of DNS lookup attempts before giving up
#' @param wait seconds to pause between attempts
#'
#' @returns boolean
#' @export
#' @keywords internal
#'
#' @examples
#' online()
online <- function(url = "google.com", tries = 3, wait = 1) {
  #host <- urltools::domain(url)
  url <- ifelse(grepl("^[a-zA-Z]+://", url), url, paste0("http://", url))
  host <- sub("^[a-zA-Z]+://([^/]+).*", "\\1", url)

  for (i in seq_len(tries)) {
    if (!is.null(curl::nslookup(host, error = FALSE))) return(TRUE)
    if (i < tries) Sys.sleep(wait)
  }

  FALSE
}
