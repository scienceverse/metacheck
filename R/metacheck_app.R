#' Launch Create Report App
#'
#' Launch the Create Report app: upload a PDF and generate a report with one
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
#' create_report_app()
#' }
#'
create_report_app <- function(quiet = FALSE, ...) {
  pckgs <- c("shiny", "shinydashboard", "shinyjs", "DT")
  names(pckgs) <- pckgs
  req_pckgs <- sapply(pckgs, requireNamespace, quietly = TRUE)

  if (all(req_pckgs)) {
    appdir <- system.file("app", package = "metacheck")
    shiny::runApp(appDir = appdir, appFile = "create_report_app.R", quiet = quiet, ...) |> invisible()
  } else {
    warning(
      "You need to install the following packages to run the app: ",
      paste(names(req_pckgs[!req_pckgs]), collapse = ", ")
    )
  }
}

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
#' demo_app()
#' }
#'
demo_app <- function(paper = NULL, quiet = FALSE, ...) {
  if (!is.null(paper) && !"scivrs_paper" %in% class(paper)) {
    stop("The first argument must be a paper object created by metacheck, or NULL.")
  }

  pckgs <- c("shiny", "shinydashboard", "shinyjs", "DT")
  names(pckgs) <- pckgs
  req_pckgs <- sapply(pckgs, requireNamespace, quietly = TRUE)

  if (all(req_pckgs)) {
    .GlobalEnv$.app.paper. <- paper
    on.exit(rm(".app.paper.", envir = .GlobalEnv))
    appdir <- system.file("app", package = "metacheck")
    shiny::runApp(appDir = appdir, appFile = "demo_app.R", quiet = quiet, ...) |> invisible()
  } else {
    warning(
      "You need to install the following packages to run the app: ",
      paste(names(req_pckgs[!req_pckgs]), collapse = ", ")
    )
  }
}
