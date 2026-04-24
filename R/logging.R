#' Get log path
#'
#' Checks the most recent log file created that day and creates a new one if missing.
#'
#' @returns the log file path
#' @export
#'
#' @keywords internal
logpath <- function() {
  dir <- rappdirs::user_data_dir("metacheck/log", "scienceverse")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  # get most recent log
  pattern <- "^metacheck.log$"
  log <- list.files(dir, pattern, full.names = TRUE) |> utils::tail(1)

  # make a new log if missing
  if (length(log) == 0) { # || file.size(log) > 1024^2) {
    #dt <- Sys.time() |> format("%Y-%m-%d_%H-%M-%S")
    path <- paste0("metacheck", ".log") |> file.path(dir, x = _)
    jsonlite::write_json(list(), path)
  }

  return(log)
}


#' Log messages
#'
#' Adds a logging message to the log. Keeps the log as a maximum of 1000 rows.
#'
#' @param label a string with the context (e.g.,module name)
#' @param contents a named list of the log contents
#' @param logpath an optional file path to save the log in
#'
#' @returns called for side effects of writing to log, returns logpath
#' @export
#'
#' @examples
#' logpath <- tempfile(fileext = ".log")
#' logger("test", list(x = 1), logpath)
#' lastlog()
logger <- function(label = "", contents = list(), logpath = NULL) {
  if (!is.list(contents)) {
    contents <- list(error = contents)
  }

  # make sure character contents are UTF-8
  contents <- lapply(contents, \(v) {
    if (!is.character(v)) return(v)
    iconv(v, to = "UTF-8")
  })

  logpath <- logpath %||% logpath()
  if (!file.exists(logpath)) {
    jsonlite::write_json(list(), logpath)
  }
  prev_log <- tryCatch({
      jsonlite::read_json(logpath) |>
        utils::head(999) # don't let log get over 1000
    }, error = \(e) { return(list()) })
  log <- c(list(
    label = label,
    dt = Sys.time() |> format("%Y-%m-%d %H:%M:%S")
  ), contents)
  #log$stack <- capture.output(sys.calls())

  # prepend to log
  new_log <- c(list(log), prev_log)
  jsonlite::write_json(new_log, logpath, auto_unbox = TRUE, pretty = TRUE)

  invisible(logpath)
}


#' Get the last log
#'
#' @param i the indices to return
#' @param logpath an optional file path to read the log from
#'
#' @returns a list of the last log item, or a data frame of multiple items
#' @export
#'
#' @examples
#' # set up 2 log items
#' logger("test", list(msg = "hi"))
#' logger("test", list(msg = "hi again"))
#'
#' lastlog()
#' lastlog(2)
#' lastlog(1:2)
lastlog <- function(i = 1, logpath = NULL) {
  if (!is.numeric(i)) {
    stop("i must be a vector of indices", call. = FALSE)
  }

  logpath <- logpath %||% logpath()
  full_log <- jsonlite::read_json(logpath)
  if (length(full_log) == 0) return(NULL)
  log <- full_log[intersect(i, seq_along(full_log))]

  if (length(log) == 1) {
    log <- log[[1]]
  } else {
    log <- tryCatch(
      dplyr::bind_rows(log),
      error = \(e) { return(log)})
  }

  return(log)
}
