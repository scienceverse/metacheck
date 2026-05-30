#' List Local Files
#'
#' Lists all files in a local directory recursively and returns a data frame
#' compatible with the `repo_check` output table, for use with `code_check`.
#'
#' @param path path to a local directory
#'
#' @returns a data frame with columns `repo_url`, `file_name`, `file_url`,
#'   `file_location`, `file_size`, `file_type`
#' @export
#'
#' @examples
#' \dontrun{
#'   local_files("C:/my_project")
#' }
local_files <- function(path) {
  if (length(path) > 1) {
    return(dplyr::bind_rows(lapply(path, local_files)))
  }

  path <- normalizePath(path, mustWork = TRUE)

  if (!dir.exists(path)) {
    stop("'", path, "' is not a directory")
  }

  all_paths <- list.files(path, recursive = TRUE, full.names = TRUE)

  if (length(all_paths) == 0) {
    return(data.frame(
      repo_url = character(0),
      file_name = character(0),
      file_url = character(0),
      file_location = character(0),
      file_size = numeric(0),
      file_type = character(0)
    ))
  }

  file_names <- basename(all_paths)
  file_sizes <- file.size(all_paths)

  exts <- tolower(sub("^.*\\.", "", file_names))
  exts[!grepl("\\.", file_names)] <- NA_character_

  types <- metacheck::file_types$type[match(exts, metacheck::file_types$ext)]

  is_readme <- grepl("readme|read[_ ]me", file_names, ignore.case = TRUE)
  types[is_readme] <- "readme"

  data.frame(
    repo_url = path,
    file_name = file_names,
    file_url = NA_character_,
    file_location = all_paths,
    file_size = file_sizes,
    file_type = types
  )
}
