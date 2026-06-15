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

requireNamespace <- NULL

#' Launch Demo App
#'
#' Launch the demo app with Upload/Demo, full module selection, and text search.
#'
#' @param paper optional paper or paperlist to load
#' @param quiet whether to show debugging messages in the console
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
#' @returns A paper object (invisibly)
#'
#' @examples
#' \dontrun{
#' metacheck_app()
#' }
#'

metacheck_app <- function(paper = NULL, quiet = FALSE, ...) {
  # check study
  if (!is.null(paper) & !.is_paper(paper) & !.is_paper_list(paper)) {
    stop("The first argument must be a paper object created by metacheck, or NULL")

  }

  pckgs <- c("shiny", "shinydashboard", "shinyjs", "DT")
  names(pckgs) <- pckgs
  req_pckgs <- sapply(pckgs, requireNamespace, quietly = TRUE)

  if (all(req_pckgs)) {
    .GlobalEnv$.app.paper. <- paper
    on.exit(rm(".app.paper.", envir = .GlobalEnv))
    appdir <- system.file("app/metacheck_app.R", package = "metacheck")
    shiny::runApp(appDir = appdir, quiet = quiet, ...) |> invisible()
  } else {
    warning(
      "You need to install the following packages to run the app: ",
      paste(names(req_pckgs[!req_pckgs]), collapse = ", ")
    )
  }
}
