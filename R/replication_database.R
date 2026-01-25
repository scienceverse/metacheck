#' FORRT Replication Database
#'
#' FReD database containing DOIs of original studies and replications. Use `fred_date()` to find the date it was downloaded, and `fred_update()` to update it.
#'
#' @format A data frame with 2222 rows and 4 columns:
#' \describe{
#'   \item{ref_original}{reference of original study}
#'   \item{doi_original}{doi of original study}
#'   \item{ref_replication}{reference of replication study}
#'   \item{ref_replication}{doi of replication study}
#' }
#' @source \url{https://osf.io/9r62x/files/z5u9b}
#'
#' @returns a data frame
#' @export
#'
#' @examples
#' FReD()
FReD <- function() {
  int <- system.file("databases/FReD.Rds", package = "metacheck")
  int_FReD <- readRDS(int)

  ext <- rappdirs::user_data_dir("metacheck", "scienceverse") |>
    file.path("FReD.Rds")

  if (file.exists(ext)) {
    # check dates
    ext_FReD <- readRDS(ext)
    ext_date <- attr(ext_FReD, "date")
    int_date <- attr(int_FReD, "date")
    if (ext_date > int_date) {
      return(ext_FReD)
    }
  }

  return(int_FReD)
}

#' Get date FReD was updated
#'
#' @returns the date
#' @export
#'
#' @examples
#' FReD_date()
FReD_date <- function() {
  attr(FReD(), "date")
}

#' Update FReD
#'
#' metacheck comes with a built-in data frame called `FReD`. We update it regularly, but you can use this function to download the newest version. The download is >5MB, but this function will summarise the information into a smaller version and delete the original file.
#'
#' @returns the path to the data frame (invisibly)
#' @export
#'
FReD_update <- function() {
  # download newest FReD update
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout))
  options(timeout = 300)

  tmp <- tempfile()
  suppressMessages(osf_file_download(
    osf_id = "z5u9b",
    download_to = tmp,
    ignore_folder_structure = TRUE
  ))
  on.exit(unlink(tmp))

  file <- list.files(tmp, "\\.xls", full.names = TRUE)
  if (length(file) == 0) stop("The file at osf.io/z5u9b is missing")
  FReD <- suppressMessages(
    readxl::read_excel(file[[1]])
  )
  # Remove first 2 rows
  FReD <- FReD[-c(1, 2), ]
  # Remove all rows without doi for original study and replication and decrease size
  rows <- FReD$doi_original != "" &
    !is.na(FReD$doi_original) &
    FReD$doi_replication != "" &
    !is.na(FReD$doi_replication)
  cols <- c("ref_original", "doi_original", "ref_replication", "doi_replication")
  FReD <- unique(FReD[rows, cols])
  FReD <- FReD[FReD$doi_original != "" & !is.na(FReD$doi_original) &
    FReD$doi_replication != "" & !is.na(FReD$doi_replication), ]
  # decrease size
  cols <- c(
    "ref_original", "doi_original",
    "ref_replication", "doi_replication"
  )
  FReD <- dplyr::distinct(FReD[cols])

  attr(FReD, "date") <- Sys.Date()

  # how/where to save this file?
  # https://blog.r-hub.io/2020/03/12/user-preferences/
  dir <- rappdirs::user_data_dir("metacheck", "scienceverse")
  dir.create(dir, FALSE, TRUE)
  path <- file.path(dir, "FReD.Rds")
  saveRDS(FReD, path, compress = "xz")

  invisible(path)
}
