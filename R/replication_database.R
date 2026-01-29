#' FORRT Replication Database (FLoRA)
#'
#' FLoRA database containing DOIs of original studies and replications. Use `FLoRA_date()` to find the date it was downloaded, and `FLoRA_update()` to update it.
#'
#' @format A data frame with 8 columns:
#' \describe{
#'   \item{doi_o}{DOI of original study}
#'   \item{apa_ref_o}{APA reference of original study}
#'   \item{doi_r}{DOI of replication study (may be NA if url_r is provided)}
#'   \item{apa_ref_r}{APA reference of replication study}
#'   \item{url_r}{URL of replication study (used when DOI is not available)}
#'   \item{outcome}{replication outcome}
#'   \item{outcome_quote}{quote describing replication outcome}
#'   \item{type}{replication or reproduction}
#' }
#' @source \url{https://osf.io/9r62x/files/t4j8f}
#'
#' @returns a data frame
#' @export
#'
#' @examples
#' FLoRA()
FLoRA <- function() {
  int <- system.file("databases/FLoRA.Rds", package = "metacheck")
  int_FLoRA <- readRDS(int)

  ext <- rappdirs::user_data_dir("metacheck", "scienceverse") |>
    file.path("FLoRA.Rds")

  if (file.exists(ext)) {
    # check dates
    ext_FLoRA <- readRDS(ext)
    ext_date <- attr(ext_FLoRA, "date")
    int_date <- attr(int_FLoRA, "date")
    if (ext_date > int_date) {
      return(ext_FLoRA)
    }
  }

  return(int_FLoRA)
}

#' Get date FLoRA was updated
#'
#' @returns the date
#' @export
#'
#' @examples
#' FLoRA_date()
FLoRA_date <- function() {
  attr(FLoRA(), "date")
}

#' Update FLoRA
#'
#' metacheck comes with a built-in data frame called `FLoRA`. We update it regularly, but you can use this function to download the newest version. The download is >5MB, but this function will summarise the information into a smaller version and delete the original file.
#'
#' @returns the path to the data frame (invisibly)
#' @export
#'
FLoRA_update <- function() {
  # download newest FLoRA update
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout))
  options(timeout = 300)

  tmp <- tempfile()
  suppressMessages(osf_file_download(
    osf_id = "t4j8f",
    download_to = tmp,
    ignore_folder_structure = TRUE
  ))
  on.exit(unlink(tmp))

  file <- list.files(tmp, "\\.csv", full.names = TRUE)
  if (length(file) == 0) stop("The file at osf.io/t4j8f is missing")
  FLoRA <- suppressMessages(
    readr::read_csv(file[[1]], show_col_types = FALSE)
  )

  # Remove rows without doi for original study, and without doi or url for replication
  has_doi_o <- !is.na(FLoRA$doi_o) & FLoRA$doi_o != ""
  has_doi_r <- !is.na(FLoRA$doi_r) & FLoRA$doi_r != ""
  has_url_r <- !is.na(FLoRA$url_r) & FLoRA$url_r != ""
  rows <- has_doi_o & (has_doi_r | has_url_r)

  # Keep 8 columns
  cols <- c("doi_o", "apa_ref_o", "doi_r", "apa_ref_r", "url_r", "outcome", "outcome_quote", "type")
  FLoRA <- dplyr::distinct(FLoRA[rows, cols])

  attr(FLoRA, "date") <- Sys.Date()

  # how/where to save this file?
  # https://blog.r-hub.io/2020/03/12/user-preferences/
  dir <- rappdirs::user_data_dir("metacheck", "scienceverse")
  dir.create(dir, FALSE, TRUE)
  path <- file.path(dir, "FLoRA.Rds")
  saveRDS(FLoRA, path, compress = "xz")

  invisible(path)
}
