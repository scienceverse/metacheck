# manually add batch queries to mocking - req_perform_parallel doesn't add them
# httptest2::start_capturing()
# for (url in urls) {
#   httr2::request(url) |>
#     httr2::req_headers(Accept = "application/json") |>
#     httr2::req_perform()
# }
# httptest2::stop_capturing()


#' Batch query
#'
#' @param urls A vector of URLs
#' @param batch_size Size of each batch
#' @param msg Message to show in progress bar - set to NULL to omit progressbar
#' @param delay Courtesy delay between batches (in seconds)
#' @param accept The header type to accept
#'
#' @returns a list of responses
#' @keywords internal
.batch_query <- function(urls,
                         batch_size = 5,
                         msg = "Batch Query",
                         delay = 0.5,
                         accept = "application/json",
                         req_func = \(req) {req}) {
  if (length(urls) == 0) return(list())

  # set up requests from urls
  reqs <- lapply(urls, \(url) {
    tryCatch({
      httr2::request(url) |>
        httr2::req_headers(Accept = accept) |>
        req_func() |>
        #httr2::req_throttle(rate = 30 / 1) |>
        httr2::req_retry(max_tries = 3, is_transient = \(resp) {
          status <- httr2::resp_status(resp)
          status %in% c(429, 500, 502, 503)
        }) |>
        httr2::req_error(is_error = \(resp) FALSE)
    }, error = \(e) {
      warning("Bad URL: ", url, call. = FALSE)
      return(NULL)
    })
  })

  # batch to avoid rate limiting
  n <- length(reqs)
  resps <- vector("list", n)

  batches <- split(seq_len(n), ceiling(seq_len(n) / batch_size))

  if (!is.null(msg)) {
    format <- sprintf("%s [:bar] :current/:total", msg)
    pb <- pb(n, format = format)
  }

  if (TRUE) { # parallel
    for (b in seq_along(batches)) {
      idx <- batches[[b]]
      valid_idx <- !sapply(reqs[idx], is.null) # skip errors

      resps[idx][valid_idx] <- httr2::req_perform_parallel(
        reqs[idx][valid_idx],
        on_error = "continue",
        progress = FALSE
      )
      if (!is.null(msg)) { pb$tick(length(idx)) }

      # courtesy delay
      Sys.sleep(delay)
    }
  } else { # non-parallel for mocking - workaround
    for (idx in seq_along(reqs)) {
      if (is.null(reqs[[idx]])) break # skip errors

      resps[[idx]] <- httr2::req_perform(
        reqs[[idx]]
      )
      if (!is.null(msg)) { pb$tick(1) }

      # courtesy delay
      Sys.sleep(delay)
    }
  }

  resps
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


#' Psychological Science Open Access Paper Set
#'
#' 250 open access papers from Psychological Science.
#'
#' @format A list of 250 paper objects
#' @source \url{https://journals.sagepub.com/home/pss}
"psychsci"


# bind_rows <- function(..., .id = NULL) {
#   # try to bind and try to fix if error
#   df <- tryCatch(
#     dplyr::bind_rows(..., .id = .id),
#     error = \(e) {}
#   )
#
#   if (!is.null(df)) return(df)
#
#   dfs <- list(...)
#
#   if (length(dfs > 2)) stop("Data frames are imcompatible for binding.")
#   x <- dfs[[1]]
#   y <- dfs[[2]]
#
#   # find data types for all shared columns
#   x_types <- flatten(x) |> sapply(typeof)
#   y_types <- flatten(y) |> sapply(typeof)
#   share <- intersect(names(x_types), names(y_types))
#   mismatch <- x_types[share] != y_types[share]
#
#   find_var <- function(df, flat_name) {
#     path <- strsplit(flat_name, "\\.")[[1]]
#     Reduce(function(acc, name) acc[[name]], path, init = df)
#   }
#
#   change_type <- function(df, flat_name, type) {
#     fname <- paste0("as.", type)
#     if (!exists(fname, mode = "function")) {
#       warning(type)
#       return(df)
#     }
#     f <- get(fname, mode = "function")
#
#     new_var <- find_var(df, flat_name) |> f()
#
#     path <- strsplit(flat_name, "\\.")[[1]]
#
#     if (length(path) == 1) {
#       df[[path[[1]]]] <- new_var
#     } else if (length(path) == 2) {
#       df[[path[[1]]]][[path[[2]]]] <- new_var
#     } else if (length(path) == 3) {
#       df[[path[[1]]]][[path[[2]]]][[path[[3]]]] <- new_var
#     } else if (length(path) == 4) {
#       df[[path[[1]]]][[path[[2]]]][[path[[3]]]][[path[[4]]]] <- new_var
#     } else if (length(path) == 5) {
#       df[[path[[1]]]][[path[[2]]]][[path[[3]]]][[path[[4]]]][[path[[5]]]] <- new_var
#     } else {
#       warning("Path too long:", length(path))
#     }
#
#     df
#   }
#
#   for (flat_name in names(mismatch[mismatch])) {
#     #flat_name <- names(mismatch[mismatch])[[1]]
#     x_var <- find_var(x, flat_name)
#     y_var <- find_var(y, flat_name)
#     newtype <- typeof(c(x_var, y_var))
#
#     x <- change_type(x, flat_name, newtype)
#     y <- change_type(y, flat_name, newtype)
#   }
#
#   dplyr::bind_rows(x, y, .id = .id)
# }
