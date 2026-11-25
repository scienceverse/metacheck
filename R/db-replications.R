#' FORRT Library of Reproduction and Replication Attempts (FLoRA)
#'
#' FLoRA database containing DOIs of original studies and replications. Use `FLoRA_date()` to find the date of the FLoRA release it comes from, and `FLoRA_update()` to download the latest release. `FLoRA()` uses that download instead of the built-in data when it was published on the same day or later and has all the built-in columns. The release's version and DOI are in `attr(FLoRA(), "version")` and `attr(FLoRA(), "doi")`.
#'
#' @format A data frame with 10 columns:
#' \describe{
#'   \item{doi_o}{DOI of original study}
#'   \item{apa_ref_o}{APA reference of original study}
#'   \item{doi_r}{DOI of replication study (may be NA if url_r is provided)}
#'   \item{apa_ref_r}{APA reference of replication study}
#'   \item{url_r}{URL of replication study (used when DOI is not available)}
#'   \item{title_r}{title of replication study}
#'   \item{alt_identifier_r}{other identifiers of the replication study, separated by commas: a preprint, the individual report of a study in a project paper, or that project paper}
#'   \item{outcome}{replication outcome; for a reproduction, its computational reproducibility and robustness outcomes (e.g., "computationally reproducible, robustness not checked")}
#'   \item{outcome_quote}{quote describing replication outcome}
#'   \item{type}{replication or reproduction}
#' }
#' @source FLoRA releases on Zenodo, \doi{10.5281/zenodo.24461022} (all versions)
#'
#' @returns a data frame
#' @export
#'
#' @examples
#' FLoRA()
FLoRA <- function() {
  int <- system.file("databases/FLoRA.Rds", package = "metacheck")
  int_FLoRA <- readRDS(int)

  ext <- tools::R_user_dir("metacheck", "data") |>
    file.path("FLoRA.Rds")

  # a downloaded copy is used when it is from the same or a later release date
  # and has every built-in column (the former OSF download lacks some)
  if (file.exists(ext)) {
    ext_FLoRA <- readRDS(ext)
    if (all(names(int_FLoRA) %in% names(ext_FLoRA)) &&
        isTRUE(attr(ext_FLoRA, "date") >= attr(int_FLoRA, "date"))) {
      return(ext_FLoRA)
    }
  }

  return(int_FLoRA)
}

#' Get date of the FLoRA release
#'
#' @returns the publication date of the FLoRA release on Zenodo that `FLoRA()` uses
#' @export
#'
#' @examples
#' FLoRA_date()
FLoRA_date <- function() {
  attr(FLoRA(), "date")
}

#' Update FLoRA
#'
#' metacheck comes with a built-in data frame called `FLoRA`. We update it regularly, but you can use this function to download the newest version. It downloads `flora.csv` from the latest FLoRA release on Zenodo (\doi{10.5281/zenodo.24461022}), which is >5MB, summarises the information into a smaller version and deletes the original file.
#'
#' @returns the path to the data frame (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # downloads the FLoRA database into tools::R_user_dir("metacheck", "data")
#' FLoRA_update()
#' }
FLoRA_update <- function() {
  # the concept record (all versions) answers with the latest version's record
  concept <- "https://zenodo.org/api/records/24461022/versions/latest"
  record <- .mc_request(concept) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  csv <- Filter(\(f) identical(f$key, "flora.csv"), record$files)
  if (length(csv) == 0) {
    stop("The FLoRA release at https://doi.org/", record$doi,
         " has no flora.csv", call. = FALSE)
  }
  csv <- csv[[1]]

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  .mc_request(csv$links$self) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = tmp)
  md5 <- sub("^md5:", "", csv$checksum)
  if (!identical(unname(tools::md5sum(tmp)), md5)) {
    stop("The download of flora.csv from https://doi.org/", record$doi,
         " is incomplete; try again", call. = FALSE)
  }

  # every field is text (see the release's CODEBOOK.md)
  raw <- readr::read_csv(tmp, col_types = readr::cols(.default = "c"),
                         na = c("", "NA"), progress = FALSE)
  FLoRA <- .FLoRA_summarise(raw)
  attr(FLoRA, "date") <- as.Date(record$metadata$publication_date)
  attr(FLoRA, "version") <- record$metadata$version
  attr(FLoRA, "doi") <- record$doi

  # how/where to save this file?
  # https://blog.r-hub.io/2020/03/12/user-preferences/
  dir <- tools::R_user_dir("metacheck", "data")
  dir.create(dir, FALSE, TRUE)
  path <- file.path(dir, "FLoRA.Rds")
  saveRDS(FLoRA, path, compress = "xz")

  invisible(path)
}

#' Summarise the FLoRA release's flora.csv
#'
#' Keeps rows with a DOI for the original study and a DOI or URL for the
#' replication, and the 10 columns metacheck uses. The Zenodo release leaves
#' `outcome` and `outcome_quote` empty for a reproduction, which has a
#' computational reproducibility and a robustness outcome instead; these are
#' combined into `outcome` ("computationally reproducible, robustness not
#' checked") and `outcome_quote`.
#'
#' @param raw the data frame read from flora.csv
#'
#' @returns a data frame
#' @keywords internal
.FLoRA_summarise <- function(raw) {
  has <- \(x) !is.na(x) & x != ""
  # paste the non-missing values of each row, or NA when there are none
  paste_rows <- function(..., sep) {
    m <- cbind(...)
    m[!has(m)] <- NA
    apply(m, 1, \(x) if (all(is.na(x))) NA_character_ else
      paste(x[!is.na(x)], collapse = sep))
  }

  repro <- raw$type %in% "reproduction" & !has(raw$outcome)
  if (any(repro)) {
    r <- raw[repro, ]
    comp <- ifelse(r$outcome_computation %in% "not checked",
                   "computation not checked", r$outcome_computation)
    rob <- ifelse(r$outcome_robustness %in% "not checked",
                  "robustness not checked", r$outcome_robustness)
    raw$outcome[repro] <- paste_rows(comp, rob, sep = ", ")
    comp_q <- ifelse(has(r$outcome_computational_quote),
                     paste0("Computational reproducibility: ", r$outcome_computational_quote), NA)
    rob_q <- ifelse(has(r$outcome_robustness_quote),
                    paste0("Robustness: ", r$outcome_robustness_quote), NA)
    raw$outcome_quote[repro] <- paste_rows(comp_q, rob_q, sep = "\n\n")
  }

  # Remove rows without doi for original study, and without doi or url for replication
  rows <- has(raw$doi_o) & (has(raw$doi_r) | has(raw$url_r))

  # Keep 10 columns
  cols <- c("doi_o", "apa_ref_o", "doi_r", "apa_ref_r", "url_r", "title_r",
            "alt_identifier_r", "outcome", "outcome_quote", "type")
  dplyr::distinct(raw[rows, cols])
}
