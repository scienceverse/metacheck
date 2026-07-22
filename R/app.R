#' Launch Report App
#'
#' Launch the Report app: upload a PDF and generate a report with one
#' click, with privacy options for what is sent to external servers.
#'
#' @param quiet whether to show debugging messages in the console
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
#' @returns NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' report_app()
#' }
#'
report_app <- function(quiet = FALSE, ...) {
  pckgs <- c("shiny", "shinydashboard", "shinyjs", "DT")
  names(pckgs) <- pckgs
  req_pckgs <- sapply(pckgs, requireNamespace, quietly = TRUE)

  if (all(req_pckgs)) {
    appdir <- system.file("app/report_app.R", package = "metacheck")
    shiny::runApp(appDir = appdir, quiet = quiet, ...) |> invisible()
  } else {
    warning(
      "You need to install the following packages to run the app: ",
      paste(names(req_pckgs[!req_pckgs]), collapse = ", ")
    )
  }
}

requireNamespace <- NULL # allows mocked bindings for test

#' Launch Trove App
#'
#' Launch the trove app: browse and search a corpus of metacheck-generated
#' Psych-DS collections. The app scans `root` for collection folders (each
#' containing a `collection.json` and one or more `study-*/` subfolders), builds
#' an index (see [trove_index()]), and lets you browse papers and search across
#' every variable and every identified scale/task in the corpus.
#'
#' @param root directory to scan for metacheck collection folders. Defaults to
#'   the current working directory.
#' @param quiet whether to show debugging messages in the console
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
#' @returns NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' trove_app("collabra")
#' }
#'
trove_app <- function(root = ".", quiet = FALSE, ...) {
  if (!dir.exists(root)) {
    stop("`root` is not an existing directory: ", root)
  }

  pckgs <- c("shiny", "bslib", "DT")
  names(pckgs) <- pckgs
  req_pckgs <- sapply(pckgs, requireNamespace, quietly = TRUE)

  if (all(req_pckgs)) {
    .GlobalEnv$.trove.root. <- normalizePath(root, winslash = "/", mustWork = TRUE)
    on.exit(rm(".trove.root.", envir = .GlobalEnv))
    appdir <- system.file("app/trove_app.R", package = "metacheck")
    shiny::runApp(appDir = appdir, quiet = quiet, ...) |> invisible()
  } else {
    warning(
      "You need to install the following packages to run the app: ",
      paste(names(req_pckgs[!req_pckgs]), collapse = ", ")
    )
  }
}

#' Launch the scienceverse archive browser
#'
#' Launch a Shiny app that searches a scienceverse SQLite archive built by
#' [add_to_scienceverse()]. Unlike [trove_app()] (which indexes a folder tree in
#' memory on every launch), this queries the database live, so it starts
#' instantly and can also search the extracted findings (statistics, code
#' metrics, data/Excel issues) and each paper's full text — surfaces the folder
#' index does not expose.
#'
#' Four tabs: Papers (metadata + full-text search, with per-paper studies,
#' checks and files), Findings (free-text plus numeric filters such as F greater
#' than 5 or p below .05), Scales & Tasks, and Files (the download manifest).
#' Text boxes use the same Google-style `field:value` grammar as [trove_app()].
#'
#' @param db_path path to the SQLite archive. Defaults to the canonical archive
#'   [add_to_scienceverse()] writes to.
#' @param quiet whether to show debugging messages in the console.
#' @param ... arguments passed to [shiny::runApp()].
#'
#' @export
#'
#' @returns NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' scienceverse_app()                       # the default archive
#' scienceverse_app("my_archive.sqlite")    # a specific one
#' }
#'
scienceverse_app <- function(db_path = .sv_default_db(), quiet = FALSE, ...) {
  pckgs <- c("shiny", "bslib", "DT", "DBI", "RSQLite")
  names(pckgs) <- pckgs
  req_pckgs <- sapply(pckgs, requireNamespace, quietly = TRUE)
  if (!all(req_pckgs)) {
    stop("You need to install the following packages to run the app: ",
         paste(names(req_pckgs[!req_pckgs]), collapse = ", "), call. = FALSE)
  }
  if (!file.exists(db_path)) {
    stop("No scienceverse archive at: ", db_path,
         "\nBuild one first with add_to_scienceverse().", call. = FALSE)
  }
  .GlobalEnv$.scienceverse.db. <- normalizePath(db_path, winslash = "/",
                                                mustWork = TRUE)
  on.exit(rm(".scienceverse.db.", envir = .GlobalEnv))
  appdir <- system.file("app/scienceverse_app.R", package = "metacheck")
  shiny::runApp(appDir = appdir, quiet = quiet, ...) |> invisible()
}

# #' Launch Demo App
# #'
# #' Launch the demo app with Upload/Demo, full module selection, and text search.
# #'
# #' @param paper optional paper or paperlist to load
# #' @param quiet whether to show debugging messages in the console
# #' @param ... arguments to pass to shiny::runApp
# #'
# #' @export
# #'
# #' @returns A paper object (invisibly)
# #'
# #' @examples
# #' \dontrun{
# #' metacheck_app()
# #' }
# #'
#
# metacheck_app <- function(paper = NULL, quiet = FALSE, ...) {
#   # check study
#   if (!is.null(paper) & !.is_paper(paper) & !.is_paper_list(paper)) {
#     stop("The first argument must be a paper object created by metacheck, or NULL")
#
#   }
#
#   pckgs <- c("shiny", "shinydashboard", "shinyjs", "DT")
#   names(pckgs) <- pckgs
#   req_pckgs <- sapply(pckgs, requireNamespace, quietly = TRUE)
#
#   if (all(req_pckgs)) {
#     .GlobalEnv$.app.paper. <- paper
#     on.exit(rm(".app.paper.", envir = .GlobalEnv))
#     appdir <- system.file("app/metacheck_app.R", package = "metacheck")
#     shiny::runApp(appDir = appdir, quiet = quiet, ...) |> invisible()
#   } else {
#     warning(
#       "You need to install the following packages to run the app: ",
#       paste(names(req_pckgs[!req_pckgs]), collapse = ", ")
#     )
#   }
# }
