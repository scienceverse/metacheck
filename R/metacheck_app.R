#' Launch Shiny App
#'
#' Create a meta-study file interactively in a shiny app that runs locally in RStudio or your web browser (recommended).
#'
#' @param paper optional paper or paperlist to load
#' @param quiet whether to show the debugging messages in the console
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
#' @returns A paper object created or edited by the app
#'
#' @examples
#' \dontrun{
#' s <- metacheck_app()
#' }
#'
metacheck_app <- function(paper = NULL, quiet = FALSE, ...) {
  # check study
  if (!is.null(paper) && !"scivrs_paper" %in% class(paper)) {
    stop("The first argument must be a paper object created by metacheck, or NULL to create it entirely in the app.")
  }

  # check required packages
  pckgs <- c(
    "shiny", "shinydashboard", "shinyjs",
    "shiny.i18n", "DT"
  )
  names(pckgs) <- pckgs
  req_pckgs <- sapply(pckgs, requireNamespace, quietly = TRUE)

  if (all(req_pckgs)) {
    .GlobalEnv$.app.study. <- paper
    on.exit(rm(".app.study.", envir = .GlobalEnv))

    shiny::runApp(appDir = system.file("app", package = "metacheck"), quiet = quiet, ...) |> invisible()
  } else {
    warning(
      "You need to install the following packages to run the app: ",
      paste(names(req_pckgs[!req_pckgs]), collapse = ", ")
    )
  }
}
