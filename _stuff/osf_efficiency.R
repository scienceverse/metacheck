osf_id <- "rcu92"
osf_api_calls(0)
info <- osf_info(osf_id)
osf_api_calls() # 1


osf_url <- "pngda"
osf_url <- c("yt32c", "z3tr9")
id_col = 1
recursive = TRUE

osf_api_calls(0)
data <- osf_retrieve(osf_url, id_col, recursive)
osf_api_calls() # originally 39 w/ recursive (141 w find_project)
# now 35/35


osf_id <- c("pngda", "6nt4v", "6846ebe94ff8297687bf4917")



#' List Files in an OSF Component
#'
#' @param osf_id a vector of OSF IDs
#'
#' @returns a data frame with file info
#' @export
#' @keywords internal
osf_files <- function(osf_id) {
  osf_api <- getOption("metacheck.osf.api")
  node_id <- osf_check_id(osf_id)

  message("* Retrieving files for ",
          paste(node_id, collapse = ", "),
          "...")

  # OSF guids
  node_id_guid <- node_id[nchar(node_id) == 5]
  if (length(node_id_guid)) {
    url <- sprintf("%s/nodes/?filter[id]=%s&embed=files", osf_api,
                   paste(node_id_guid, collapse = ","))
    storage <- osf_get_all_pages(url, links = c("data", "embeds", "files"))
    f <- do.call(dplyr::bind_rows, storage$embeds$files$data %||% list())
    obj_guid <- osf_file_data(f)
  } else {
    obj_guid <- NULL
  }

  # waterbutler IDs
  node_id_wb <- setdiff(node_id, node_id_guid)
  if (length(node_id_wb)) {
    urls <- sprintf("%s/files/%s/?embed=files", osf_api, node_id_wb)
    f <- lapply(urls, \(url) {
      osf_get_all_pages(url, links = c("data", "embeds", "files"))
    }) |> do.call(dplyr::bind_rows, args = _)
    obj_wb <- osf_file_data(f)
  } else {
    obj_wb <- NULL
  }
  obj <- dplyr::bind_rows(obj_guid, obj_wb)

  return(obj)
}
