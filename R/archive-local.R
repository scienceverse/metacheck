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
#' # the demo repository bundled with metacheck
#' local_files(demofile("repo"), recursive = TRUE)
local_files <- function(path, recursive = FALSE) {
  if (length(path) > 1) {
    # `recursive` passed on: several paths were always listed without
    # their subfolders (U49)
    df <- lapply(path, local_files, recursive = recursive) |>
      dplyr::bind_rows()
    return(df)
  }

  #path <- normalizePath(path, mustWork = TRUE)

  # handle files and directories
  if (dir.exists(path)) {
    all_paths <- list.files(path, full.names = TRUE, recursive = recursive)
    # Hidden files are listed only when they do not belong in a repository
    # (.DS_Store, .Rhistory, .git/, .Rproj.user/, ...; see .useless_file_kind()),
    # so repo_check can report them; other hidden files stay out, as before.
    rel_visible <- list.files(path, recursive = recursive)
    rel_hidden <- setdiff(list.files(path, recursive = recursive,
                                     all.files = TRUE, no.. = TRUE),
                          rel_visible)
    rel_hidden <- rel_hidden[.is_useless_file(rel_hidden)]
    all_paths <- c(all_paths, file.path(path, rel_hidden))
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
  # a file file_category() cannot place takes its extension's type, as a
  # repository's listing does: a local .zip or .tar.gz was not counted as
  # an archive (U123)
  file_types <- file_category(file_names)$file_category
  ext_type <- metacheck::file_types$type[
    match(tolower(tools::file_ext(file_names)), metacheck::file_types$ext)]
  file_types <- ifelse(is.na(file_types), ext_type, file_types)

  data.frame(
    repo_url = path,
    file_name = file_names,
    file_url = NA_character_,
    file_location = normalizePath(all_paths, mustWork = FALSE),
    file_size = file_sizes,
    file_type = file_types
  )
}
