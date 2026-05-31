#' List Local Files
#'
#' Lists all files in a local directory recursively and returns a data frame
#' compatible with the `repo_check` output table, for use with `code_check`.
#'
#' @param path path to a local directory or file, or a vector of paths
#' @param recursive whether to search the files recursively
#'
#' @returns a data frame with columns `repo_url`, `file_name`, `file_url`,
#'   `file_location`, `file_size`, `file_type`
#' @export
#'
#' @examples
#' \dontrun{
#'   local_files("my_project")
#' }
local_files <- function(path, recursive = FALSE) {
  if (length(path) > 1) {
    df <- lapply(path, local_files) |>
      dplyr::bind_rows()
    return(df)
  }

  #path <- normalizePath(path, mustWork = TRUE)

  # handle files and directories
  if (dir.exists(path)) {
    all_paths <- list.files(path, full.names = TRUE, recursive = recursive)
    all_paths <- all_paths[!dir.exists(all_paths)]
  } else if (file.exists(path)) {
    all_paths <- path
  } else {
    all_paths <- character(0)
    warning("This path does not exist: ", path)
  }

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
  file_types <- file_category(file_names)$file_category

  data.frame(
    repo_url = path,
    file_name = file_names,
    file_url = NA_character_,
    file_location = normalizePath(all_paths),
    file_size = file_sizes,
    file_type = file_types
  )
}
