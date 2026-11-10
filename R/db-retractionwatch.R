#' RetractionWatch data
#'
#' DOIs and nature of statements from the RetractionWatch database. Use `rw_date()` to find the date it was downloaded, and `rw_update()` to update it.
#'
#' @format A data frame with 44784+ rows and 2 columns:
#' \describe{
#'   \item{doi}{Document ID}
#'   \item{retractionwatch}{Nature of note(s)}
#' }
#' @source \url{https://api.labs.crossref.org/data/retractionwatch}, or
#'   \url{https://gitlab.com/crossref/retraction-watch-data}
#'
#' @returns a data frame
#' @export
#'
#' @examples
#' retractionwatch() |> head()
retractionwatch <- function() {
  # a row with a blank DOI matched every reference without a DOI in a join on
  # doi (ref_retraction filtered it first, other joins did not) (U15)
  no_blank <- function(x) {
    date <- attr(x, "date")
    x$doi <- trimws(x$doi)
    x <- x[!is.na(x$doi) & nzchar(x$doi), , drop = FALSE]
    attr(x, "date") <- date
    x
  }
  int <- system.file("databases/retractionwatch.Rds", package = "metacheck")
  int_rw <- no_blank(readRDS(int))

  ext <- tools::R_user_dir("metacheck", "data") |>
    file.path("retractionwatch.Rds")

  if (file.exists(ext)) {
    # check dates
    ext_rw <- no_blank(readRDS(ext))
    ext_date <- attr(ext_rw, "date")
    int_date <- attr(int_rw, "date")
    if (ext_date > int_date) {
      return(ext_rw)
    }
  }

  return(int_rw)
}

#' @rdname retractionwatch
#' @export
rw <- retractionwatch

#' Get date retractionwatch was updated
#'
#' @returns the date
#' @export
#'
#' @examples
#' rw_date()
rw_date <- function() {
  attr(retractionwatch(), "date")
}

#' Update retractionwatch
#'
#' metacheck comes with a built-in data frame called `retractionwatch`. We update it regularly, but you can use this function to download the newest version. The download is >50MB, but this function will summarise the information into a smaller version (~0.5 MB) and delete the original file.
#'
#' The data come from the Crossref Labs API. Crossref is retiring that API;
#' when it cannot be reached, Crossref's copy on GitLab
#' (<https://gitlab.com/crossref/retraction-watch-data>) is used.
#'
#' @returns the path to the data frame (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # downloads the Retraction Watch database into tools::R_user_dir("metacheck", "data")
#' rw_update()
#' }
rw_update <- function() {
  OriginalPaperDOI <- RetractionNature <- doi <- NULL

  # download newest RW update
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout))
  options(timeout = 300)

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  # Crossref is retiring the Labs API, so its copy on GitLab is the fallback
  # (#305). On 10 November 2026 the Labs file was still the newer one: it had
  # 29 records (up to Record ID 74700) that GitLab's (up to 74620) had not.
  # A failed download is not read as the data: the Labs API answers a refused
  # request (e.g. one without an email address) with status 200 and an error
  # message in JSON, so the file must start with the Retraction Watch columns.
  urls <- c(
    paste0("https://api.labs.crossref.org/data/retractionwatch?", email()),
    "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv"
  )
  ok <- FALSE
  for (url in urls) {
    resp <- tryCatch(
      .mc_request(url) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform(path = tmp),
      error = \(e) NULL
    )
    if (!is.null(resp) && httr2::resp_status(resp) == 200) {
      header <- tryCatch(readLines(tmp, n = 1, warn = FALSE),
                         error = \(e) "")
      if (length(header) && grepl("OriginalPaperDOI", header[[1]], fixed = TRUE) &&
          grepl("RetractionNature", header[[1]], fixed = TRUE)) {
        ok <- TRUE
        break
      }
    }
  }
  if (!ok) {
    stop("Could not download the Retraction Watch data from ",
         paste(sub("\\?.*$", "", urls), collapse = " or "), call. = FALSE)
  }

  # decrease size
  retractionwatch <- utils::read.csv(tmp) |>
    dplyr::select(
      doi = OriginalPaperDOI,
      # pmid = OriginalPaperPubMedID,
      retractionwatch = RetractionNature
    ) |>
    dplyr::mutate(doi = trimws(doi)) |>
    dplyr::filter(doi != "unavailable", doi != "", retractionwatch != "") |>   # U15
    dplyr::summarise(retractionwatch = unique(retractionwatch) |>
                       paste(collapse = ";"), .by = doi)

  attr(retractionwatch, "date") <- Sys.Date()

  # how/where to save this file?
  # https://blog.r-hub.io/2020/03/12/user-preferences/
  dir <- tools::R_user_dir("metacheck", "data")
  dir.create(dir, FALSE, TRUE)
  path <- file.path(dir, "retractionwatch.Rds")
  saveRDS(retractionwatch, path, compress = "xz")

  invisible(path)
}
