#' Install modules from GitHub
#'
#' Downloads a public GitHub repository that holds one or more metacheck
#' modules and saves it in the metacheck user data directory, so its modules
#' can be used by name in [module_run()], [report()], [module_list()] and
#' [module_help()], like the built-in modules.
#'
#' A module is an .R file with a roxygen header, as made by
#' [module_template()]. Every such file in the top folder of the repository,
#' or in a folder called `modules/`, is installed as a module. Other files
#' (helper code, data, tests) are saved with the repository, but are not
#' listed as modules.
#'
#' A module can run other modules with [module_run()] and include their
#' results in its own report with [module_report()], so running one module
#' (e.g. `report(paper, modules = "my_check")`) gives one report for all of
#' its checks.
#'
#' A module can state the lowest metacheck version it needs with a line
#' `#' @metacheck 0.3.4` in its header; it then refuses to install or run with
#' an older metacheck. External modules should only use exported metacheck
#' functions: internal functions can change without notice.
#'
#' A module names the R packages it needs with `#' @import pkg` or
#' `#' @importFrom pkg fun`. A package that is not on CRAN is named with its
#' GitHub repository, `#' @import owner/pkg`: the package `pkg` is then
#' loaded, and when it is not installed, `module_install()` and
#' [module_run()] say to install it with `remotes::install_github("owner/pkg")`
#' instead of `install.packages()`.
#'
#' Nothing is saved when a module in the repository has the same name as a
#' built-in module, a module from another installed repository, or another
#' module in the same repository.
#'
#' Modules are saved in the metacheck user data folder, which belongs to the
#' user who installs them. To share installed modules between users (for
#' example with the user a Shiny Server runs apps as), set the environment
#' variable `METACHECK_MODULES_DIR` to a folder all of them can read, both
#' when installing and when using the modules.
#'
#' Installing a module means running its code whenever you use it: only
#' install modules from repositories you trust.
#'
#' @param repo GitHub repository in "owner/repo" format, or its URL
#' @param ref the branch, tag or commit to install (defaults to the
#'   repository's default branch)
#' @param overwrite whether to replace the repository if it is already
#'   installed
#'
#' @returns a data frame of the installed repository (see
#'   [module_installed()]), invisibly
#' @export
#' @seealso [module_update()], [module_remove()], [module_installed()]
#'
#' @examples
#' \dontrun{
#' module_install("owner/repo")
#' module_run(demopaper(), "module_name")
#' }
module_install <- function(repo, ref = NULL, overwrite = FALSE) {
  if (!is.character(repo) || length(repo) != 1 ||
      is.null(.github_repo_name(repo))) {
    stop("repo must be a GitHub repository in the format \"owner/repo\"",
         call. = FALSE)
  }

  # resolve the repository's name, branch and commit before downloading, so
  # an existing installation can be checked first
  src <- .module_resolve(repo, ref)
  installed <- module_installed()
  same_repo <- tolower(installed$repo) == tolower(src$repo)
  if (any(same_repo) && !overwrite) {
    stop("The repository ", src$repo, " is already installed. ",
         "Use module_update(\"", src$repo, "\") or overwrite = TRUE.",
         call. = FALSE)
  }
  if (any(same_repo) && installed$sha[same_repo][[1]] == src$sha) {
    message(src$repo, " is already up to date (commit ",
            substr(src$sha, 1, 7), ")")
    return(invisible(installed[same_repo, ]))
  }

  tmp <- tempfile("module_install")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  repo_dir <- .module_download(src, tmp)

  # find and check the modules before anything is saved
  files <- .module_repo_files(repo_dir)
  if (length(files) == 0) {
    stop("No modules found in ", src$repo, ". A module is an .R file with ",
         "a roxygen header, in the top folder or in a folder called modules/.",
         call. = FALSE)
  }
  infos <- lapply(file.path(repo_dir, files), module_info)
  mod_names <- basename(files) |> sub("\\.R$", "", x = _)
  for (i in seq_along(infos)) .module_check_version(infos[[i]], mod_names[[i]])
  .module_check_names(mod_names, src$repo, installed)

  # save the repository and a record of where it came from
  dest <- file.path(.modules_dir(), src$repo)
  unlink(dest, recursive = TRUE)
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  file.copy(list.files(repo_dir, full.names = TRUE, all.files = TRUE,
                       no.. = TRUE),
            dest, recursive = TRUE)
  record <- data.frame(
    repo = src$repo,
    ref = src$ref,
    sha = src$sha,
    installed = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    files = paste(files, collapse = ", ")
  )
  write.dcf(record, file.path(dest, .module_source_file), width = 10000)

  message("Installed ", length(mod_names), " module(s) from ", src$repo,
          " (commit ", substr(src$sha, 1, 7), "): ",
          paste(mod_names, collapse = ", "))

  # name packages the modules need that are not installed; "owner/pkg" is a
  # package only on GitHub (#223)
  pkgs <- lapply(infos, \(info) {
    c(unlist(info$import), sapply(info$importFrom, `[[`, 1))
  }) |> unlist() |> unique()
  missing <- pkgs[!vapply(.pkg_name(pkgs), \(p) nzchar(system.file(package = p)),
                          logical(1))]
  if (length(missing)) {
    on_github <- grepl("/", missing)
    how <- c(
      if (any(!on_github)) paste0("install.packages(c(\"",
        paste(missing[!on_github], collapse = "\", \""), "\"))"),
      if (any(on_github)) paste0("remotes::install_github(c(\"",
        paste(missing[on_github], collapse = "\", \""), "\"))")
    )
    warning("These modules need packages that are not installed: ",
            paste(unique(.pkg_name(missing)), collapse = ", "),
            ". Install them with ", paste(how, collapse = " and "),
            call. = FALSE)
  }

  installed <- module_installed()
  invisible(installed[installed$repo == src$repo, ])
}


#' Update installed modules
#'
#' Downloads the latest commit of the branch or tag each repository was
#' installed from (see [module_install()]). A repository that is already up to
#' date is not downloaded again.
#'
#' @param repo GitHub repository in "owner/repo" format, or NULL to update
#'   every installed repository
#'
#' @returns a data frame of the installed repositories (see
#'   [module_installed()]), invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' module_update()
#' }
module_update <- function(repo = NULL) {
  installed <- module_installed()
  if (!is.null(repo)) {
    installed <- installed[tolower(installed$repo) == tolower(repo), ]
    if (nrow(installed) == 0) {
      stop("The repository ", repo, " is not installed; ",
           "see module_installed()", call. = FALSE)
    }
  }

  for (i in seq_len(nrow(installed))) {
    module_install(installed$repo[[i]], installed$ref[[i]], overwrite = TRUE)
  }

  invisible(module_installed())
}


#' Remove installed modules
#'
#' Deletes a repository of modules installed with [module_install()].
#'
#' @param repo GitHub repository in "owner/repo" format
#'
#' @returns `TRUE` invisibly if removed, `FALSE` if not installed
#' @export
#'
#' @examples
#' \dontrun{
#' module_remove("owner/repo")
#' }
module_remove <- function(repo) {
  installed <- module_installed()
  match <- tolower(installed$repo) == tolower(repo)

  if (!any(match)) {
    message("The repository ", repo, " is not installed; nothing to remove.")
    return(invisible(FALSE))
  }

  dest <- file.path(.modules_dir(), installed$repo[match][[1]])
  unlink(dest, recursive = TRUE)
  # remove the owner's folder when it holds no other repository
  owner_dir <- dirname(dest)
  if (length(list.files(owner_dir, all.files = TRUE, no.. = TRUE)) == 0) {
    unlink(owner_dir, recursive = TRUE)
  }

  message("Removed modules from ", installed$repo[match][[1]])
  invisible(TRUE)
}


#' List installed module repositories
#'
#' Lists the GitHub repositories installed with [module_install()]. Use
#' [module_list()] to see their modules together with the built-in ones.
#'
#' @returns a data frame with one row per repository: `repo`, the `ref`
#'   (branch, tag or commit) it was installed from, the `sha` of the installed
#'   commit, when it was `installed`, its `modules`, and the local `path`
#' @export
#'
#' @examples
#' module_installed()
module_installed <- function() {
  repo_dirs <- list.dirs(.modules_dir(), recursive = FALSE) |>
    lapply(list.dirs, recursive = FALSE) |>
    unlist()
  records <- file.path(repo_dirs, .module_source_file)
  records <- records[file.exists(records)]

  rows <- lapply(records, \(f) {
    x <- read.dcf(f)
    files <- strsplit(x[1, "files"], ",")[[1]] |> trimws()
    data.frame(
      repo = x[1, "repo"],
      ref = x[1, "ref"],
      sha = x[1, "sha"],
      installed = x[1, "installed"],
      modules = basename(files) |> sub("\\.R$", "", x = _) |>
        paste(collapse = ", "),
      path = dirname(f)
    )
  })

  if (length(rows) == 0) {
    return(data.frame(repo = character(0), ref = character(0),
                      sha = character(0), installed = character(0),
                      modules = character(0), path = character(0)))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


# Lets a module state the lowest metacheck version it needs in its roxygen
# header (#' @metacheck 0.3.4). roxygen2 drops tags it does not know, with a
# warning, so the tag needs a parser for module_info() to see it.
#' @exportS3Method roxygen2::roxy_tag_parse
#' @noRd
roxy_tag_parse.roxy_tag_metacheck <- function(x) {
  roxygen2::tag_value(x)
}


# ----- internal helpers ------------------------------------------------------

# the file in each installed repository that records where it came from
.module_source_file <- "_metacheck_source.dcf"

# Where installed modules are kept: the metacheck user data folder, or the
# folder in the METACHECK_MODULES_DIR env var, so that several users (e.g. the
# shiny user of a Shiny Server and the user who installs) can share one.
#' @keywords internal
.modules_dir <- function() {
  override <- Sys.getenv("METACHECK_MODULES_DIR", "")
  if (nzchar(override)) return(override)

  tools::R_user_dir("metacheck", "data") |>
    file.path("modules")
}

# The paths of every installed module file, for module_find() and
# module_list().
.module_installed_files <- function() {
  installed <- module_installed()
  files <- lapply(installed$path, \(path) {
    record <- read.dcf(file.path(path, .module_source_file))
    file.path(path, strsplit(record[1, "files"], ",")[[1]] |> trimws())
  }) |> unlist() |> as.character()
  files[file.exists(files)]
}

# The record of the repository a module file was installed from (repo, ref,
# sha, installed, files), or NULL for a module that was not installed with
# module_install().
.module_source <- function(module_path) {
  mod_dir <- normalizePath(.modules_dir(), "/", mustWork = FALSE)
  path <- normalizePath(module_path, "/", mustWork = FALSE)
  if (!startsWith(tolower(path), tolower(paste0(mod_dir, "/")))) return(NULL)

  parts <- substring(path, nchar(mod_dir) + 2) |> strsplit("/")
  parts <- parts[[1]]
  if (length(parts) < 3) return(NULL)
  record <- file.path(mod_dir, parts[[1]], parts[[2]], .module_source_file)
  if (!file.exists(record)) return(NULL)

  as.list(read.dcf(record)[1, ])
}

# The module files of a repository, relative to its folder: the .R files in
# its top folder and in modules/ that have a roxygen header and a function.
.module_repo_files <- function(repo_dir) {
  files <- c(list.files(repo_dir, "\\.R$"),
             list.files(file.path(repo_dir, "modules"), "\\.R$") |>
               file.path("modules", x = _))
  is_module <- sapply(files, \(f) {
    info <- tryCatch(module_info(file.path(repo_dir, f)),
                     error = \(e) NULL)
    !is.null(info) && !is.na(info$func_name %||% NA)
  })
  files[unlist(is_module)]
}

# Stop when the lowest metacheck version a module states (@metacheck) is
# newer than the installed metacheck.
.module_check_version <- function(info, name) {
  if (is.null(info$metacheck)) return(invisible(TRUE))

  needed <- tryCatch(
    gsub("[^0-9.-]", "", info$metacheck) |> package_version(),
    error = \(e) stop("The module '", name, "' gives '", info$metacheck,
                      "' as its metacheck version, which is not a version ",
                      "number", call. = FALSE)
  )
  have <- utils::packageVersion("metacheck")
  if (have < needed) {
    stop("The module '", name, "' needs metacheck ", needed,
         " or later; you have ", have, ". Update metacheck first.",
         call. = FALSE)
  }

  invisible(TRUE)
}

# Stop when a module name of the repository being installed is used twice in
# that repository, by a built-in module, or by another installed repository.
.module_check_names <- function(names, repo, installed) {
  dupes <- unique(names[duplicated(names)])
  if (length(dupes)) {
    stop("Nothing was installed: ", repo, " has more than one module named ",
         paste(dupes, collapse = ", "), call. = FALSE)
  }

  builtin <- list.files(system.file("modules", package = "metacheck"),
                        "\\.R$", recursive = TRUE) |>
    basename() |> sub("\\.R$", "", x = _)
  others <- installed[tolower(installed$repo) != tolower(repo), ]
  other_names <- strsplit(others$modules, ", ")
  other_repo <- rep(others$repo, lengths(other_names))
  other_names <- unlist(other_names)

  clash <- c(
    sprintf("%s (built-in)", intersect(names, builtin)),
    sprintf("%s (from %s)", other_names[other_names %in% names],
            other_repo[other_names %in% names])
  )
  if (length(clash)) {
    stop("Nothing was installed: ", repo, " has modules with names that ",
         "are already used: ", paste(clash, collapse = ", "),
         ". Rename the module file(s) in the repository.", call. = FALSE)
  }

  invisible(TRUE)
}

# The repository's own name ("owner/repo" as GitHub spells it), the ref
# (default branch if NULL) and the commit that ref points to now.
.module_resolve <- function(repo, ref = NULL) {
  repo <- .github_repo_name(repo)
  meta_resp <- tryCatch(
    .mc_request(sprintf("https://api.github.com/repos/%s", repo)) |>
      .github_config() |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = \(e) stop("Could not reach GitHub: ", e$message, call. = FALSE)
  )
  if (httr2::resp_status(meta_resp) == 404) {
    stop("The GitHub repository ", repo, " does not exist or is private",
         call. = FALSE)
  }
  if (httr2::resp_status(meta_resp) != 200) {
    stop("GitHub API error (status ", httr2::resp_status(meta_resp), ") for ",
         repo, call. = FALSE)
  }
  meta <- httr2::resp_body_json(meta_resp)
  ref <- ref %||% meta$default_branch

  # 404 or 422: no branch, tag or commit with that name
  commit_resp <- tryCatch(
    .mc_request(sprintf("https://api.github.com/repos/%s/commits/%s",
                        meta$full_name, utils::URLencode(ref))) |>
      .github_config() |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = \(e) stop("Could not reach GitHub: ", e$message, call. = FALSE)
  )
  if (httr2::resp_status(commit_resp) %in% c(404, 422)) {
    stop("The repository ", meta$full_name, " has no branch, tag or commit ",
         "called '", ref, "'", call. = FALSE)
  }
  if (httr2::resp_status(commit_resp) != 200) {
    stop("GitHub API error (status ", httr2::resp_status(commit_resp),
         ") for ", meta$full_name, call. = FALSE)
  }

  list(repo = meta$full_name,
       ref = ref,
       sha = httr2::resp_body_json(commit_resp)$sha)
}

# Download and unpack the commit `src$sha` of `src$repo` into `dir`; returns
# the folder that holds the repository's files.
.module_download <- function(src, dir) {
  zip_path <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_path), add = TRUE)

  resp <- tryCatch(
    .mc_request(sprintf("https://api.github.com/repos/%s/zipball/%s",
                        src$repo, src$sha)) |>
      .github_config() |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(path = zip_path),
    error = \(e) stop("Download failed: ", e$message, call. = FALSE)
  )
  if (httr2::resp_status(resp) != 200) {
    stop("Download failed (status ", httr2::resp_status(resp), ") for ",
         src$repo, call. = FALSE)
  }

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(zip_path, exdir = dir)
  # GitHub puts every file in one top folder named owner-repo-sha
  list.dirs(dir, recursive = FALSE)[[1]]
}
