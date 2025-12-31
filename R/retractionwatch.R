#' RetractionWatch data
#'
#' DOIs and nature of statements from the RetractionWatch database. Use `rw_date()` to find the date it was downloaded, and `rw_update()` to update it.
#'
#' @format A data frame with 44784+ rows and 2 columns:
#' \describe{
#'   \item{doi}{Document ID}
#'   \item{retractionwatch}{Nature of note(s)}
#' }
#' @source \url{https://api.labs.crossref.org/data/retractionwatch}
#'
#' @returns a data frame
#' @export
#'
#' @examples
#' retractionwatch()
retractionwatch <- function() {
  int <- system.file("databases/retractionwatch.Rds", package = "metacheck")
  int_rw <- readRDS(int)

  ext <- rappdirs::user_data_dir("metacheck", "scienceverse") |>
    file.path("retractionwatch.Rds")

  if (file.exists(ext)) {
    # check dates
    ext_rw <- readRDS(ext)
    ext_date <- attr(ext_rw, "date")
    int_date <- attr(int_rw, "date")
    if (ext_date > int_date) return(ext_rw)
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
#' @returns the path to the data frame (invisibly)
#' @export
#'
rw_update <- function() {
  OriginalPaperDOI <- RetractionNature <- doi <- NULL

  # download newest RW update
  old_timeout <- getOption("timeout")
  on.exit(options(timeout=old_timeout))
  options(timeout=300)

  tmp <- tempfile(fileext = ".csv")
  url <- paste0("https://api.labs.crossref.org/data/retractionwatch?", email())
  utils::download.file(url, destfile = tmp)
  on.exit(unlink(tmp))

  # decrease size
  retractionwatch <- utils::read.csv(tmp) |>
    dplyr::select(
      doi = OriginalPaperDOI,
      #pmid = OriginalPaperPubMedID,
      retractionwatch = RetractionNature) |>
    dplyr::filter(doi != "unavailable") |>
    dplyr::summarise(retractionwatch = unique(retractionwatch) |> paste(collapse = ";"), .by = doi)

  attr(retractionwatch, "date") <- Sys.Date()

  # how/where to save this file?
  # https://blog.r-hub.io/2020/03/12/user-preferences/
  dir <- rappdirs::user_data_dir("metacheck", "scienceverse")
  dir.create(dir, FALSE, TRUE)
  path <- file.path(dir, "retractionwatch.Rds")
  saveRDS(retractionwatch, path, compress = "xz")

  invisible(path)
}
