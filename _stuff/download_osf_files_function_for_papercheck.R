library(osfr)
library(httr)

download_osf_project_files <- function(osf_project_id, max_folder_length = 40, ignore_folder_structure = FALSE) {

  # If you set authentication, you can upload and download files to your repository. But strangely engough, the osfr package will say you do not have access to other public repositories. You have download, but not upload access. But we need to remove the authentication to prevent this error. Maybe there is a better way to authenticate that will fix this, with looking into.
  osf_auth("")

  # Retry logic for osf_retrieve
  max_attempts <- 5
  attempt <- 1
  info <- NULL

  while (is.null(info) && attempt <= max_attempts) {
    cat("Attempt", attempt, "to retrieve OSF project...\n")
    info <- tryCatch(
      papercheck::osf_retrieve(osf_project_id, recursive = TRUE, find_project = TRUE),
      error = function(e) {
        cat("Error:", e$message, "\n")
        Sys.sleep(2 ^ attempt)
        return(NULL)
      }
    )
    attempt <- attempt + 1
  }

  if (is.null(info)) {
    stop("Failed to retrieve OSF project after multiple attempts.")
  }

  # Sanitize function
  # Folder names allowed on the OSF can have characters not allowed when storing it on your computer. So, we fix some names.
  # This means the code does not perfectly mirror all path and files names.
  sanitize_name <- function(name) {
    name <- tolower(name)
    name <- gsub("&amp;", "and", name, fixed = TRUE)
    name <- gsub("&lt;|&gt;|&quot;|&#39;", "", name)
    name <- gsub("&nbsp;", "_", name)
    name <- gsub("&ndash;|&mdash;", "-", name)
    name <- gsub("&hellip;", "...", name)
    name <- gsub("\"", "", name)
    name <- gsub("[<>:/\\\\|?*]", "", name)
    name <- gsub("\\s+", "_", name)
    name <- gsub("\\.+$", "", name)
    name <- gsub("_+", "_", name)
    name <- trimws(name)
    return(name)
  }

  # Clean and prepare
  info$osf_id <- trimws(info$osf_id)
  info$parent <- trimws(info$parent)
  info$name <- trimws(info$name)

  # Build unique, truncated folder names
  # On the OSF you can nest folders and give long folder names, but windows has a 260 character folder name limit. We need to deal with this when downloading files, and do it by truncating long folder names.
  folder_names <- list()
  used_names <- list()

  for (i in seq_len(nrow(info))) {
    osf_id <- info$osf_id[i]
    raw_name <- sanitize_name(info$name[i])
    if (nchar(raw_name) > max_folder_length) {
      raw_name <- substr(raw_name, 1, max_folder_length)
    }
    base_name <- raw_name
    suffix <- 1
    while (raw_name %in% used_names) {
      raw_name <- paste0(base_name, "_", suffix)
      suffix <- suffix + 1
    }
    used_names[[length(used_names) + 1]] <- raw_name
    folder_names[[osf_id]] <- raw_name
  }

  id_to_name <- folder_names
  id_to_parent <- setNames(info$parent, info$osf_id)

  # Build full folder path
  # If all files are open, the top folder has no parent. But sometimes there is a higher level folder but it is closed. We check this.
  missing_parents <- character(0)

  get_full_path <- function(file_id) {
    if (ignore_folder_structure) return(top_level_name)
    path <- character()
    current_id <- id_to_parent[[file_id]]

    while (!is.na(current_id)) {
      if (!(current_id %in% names(id_to_name))) {
        missing_parents <<- unique(c(missing_parents, current_id))
        break
      }
      folder_name <- id_to_name[[current_id]]
      path <- c(folder_name, path)
      current_id <- id_to_parent[[current_id]]
    }

    return(paste(path, collapse = .Platform$file.sep))
  }

  # We use the top-level folder name from the OSF project
  if (ignore_folder_structure) {
    top_level_candidates <- info[is.na(info$parent) | !(info$parent %in% info$osf_id), ]
    top_level_name <- sanitize_name(top_level_candidates$name[1])
    if (!dir.exists(top_level_name)) {
      dir.create(top_level_name)
    }
  }

  # Track failures
  failures <- data.frame(osf_id = character(), reason = character(), stringsAsFactors = FALSE)

  # Download files
  for (file_id in info$osf_id) {
    if (file_id %in% info$parent) next

    cat("Processing:", file_id, "\n")

    folder_path <- get_full_path(file_id)

    tryCatch({
      if (!dir.exists(folder_path)) {
        dir.create(folder_path, recursive = TRUE)
        for (i in 1:10) {
          if (dir.exists(folder_path)) break
          Sys.sleep(0.2)
        }
      }
    }, error = function(e) {
      failures <- rbind(failures, data.frame(osf_id = file_id, reason = paste("Folder creation failed:", e$message)))
      next
    })

    file_on_osf <- tryCatch(osf_retrieve_file(file_id), error = function(e) NULL)
    if (is.null(file_on_osf)) {
      failures <- rbind(failures, data.frame(osf_id = file_id, reason = "Failed to retrieve file"))
      next
    }

    url <- tryCatch(file_on_osf[[3]][[1]][["links"]][["download"]], error = function(e) NULL)
    if (is.null(url)) {
      failures <- rbind(failures, data.frame(osf_id = file_id, reason = "No download URL"))
      next
    }

    temp_file <- tempfile()
    response <- tryCatch(GET(url, write_disk(temp_file, overwrite = TRUE)), error = function(e) NULL)
    if (is.null(response)) {
      failures <- rbind(failures, data.frame(osf_id = file_id, reason = "Download failed"))
      next
    }

    content_disp <- headers(response)$`content-disposition`
    filename <- sub('.*filename="([^"]+)".*', '\\1', content_disp)
    filename <- sanitize_name(filename)

    dest_path <- file.path(folder_path, filename)
    if (nchar(dest_path) > 240) {
      filename <- substr(filename, 1, 100)
      dest_path <- file.path(folder_path, filename)
      if (nchar(dest_path) > 260) {
        failures <- rbind(failures, data.frame(osf_id = file_id, reason = "Path too long"))
        next
      }
    }

    if (!dir.exists(folder_path)) {
      failures <- rbind(failures, data.frame(osf_id = file_id, reason = "Folder missing at rename"))
      next
    }

    success <- tryCatch(file.rename(temp_file, dest_path), error = function(e) FALSE)
    if (!success) {
      failures <- rbind(failures, data.frame(osf_id = file_id, reason = "File rename failed"))
      next
    }

    cat("Saved to:", dest_path, "\n")
    Sys.sleep(1)
  }

  # We print a warning if there are closed folders that we can't access
  if (length(missing_parents) > 0) {
    cat("\nNote: Some folders referenced a parent that was not included in the project metadata.\n")
    cat("This likely means those parent folders are private or inaccessible.\n")
    cat("Missing parent OSF IDs:\n")
    print(missing_parents)

    failures <- rbind(
      failures,
      data.frame(
        osf_id = missing_parents,
        reason = "Missing parent folder (possibly private)",
        stringsAsFactors = FALSE
      )
    )
  }

  # Return both info and failures silently
  return(list(info = info, failures = failures))
}

# Download files and mirror the folder structure. By default folder names are truncated at 40 characters.
download_results <- download_osf_project_files("pngda")
# Download files and ignore the folder structure. Store all files in one folder.
download_results <- download_osf_project_files("pngda", ignore_folder_structure = TRUE)
