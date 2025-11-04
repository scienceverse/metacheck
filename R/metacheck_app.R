#' Launch Shiny App
#'
#' Create a meta-study file interactively in a shiny app that runs locally in RStudio or your web browser (recommended). It does not connect to the web at all, so your data are completely private.
#'
#' @param study optional study to load
#' @param quiet whether to show the debugging messages in the console
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
#' @returns A study object created or edited by the app
#'
#' @examples
#' \donttest{ s <- metacheck_app() }
#'
metacheck_app <- function(study = NULL, quiet = FALSE, ...) {
  # check study
  if (!is.null(study) && !"scivrs_paper" %in% class(study)) {
    stop("The argument study must be a paper object created by metacheck, or NULL to create it entirely in the app.")
  }

  # check required packages
  pckgs <- c("shiny", "shinydashboard", "shinyjs",
             "shiny.i18n", "DT", "waiter")
  names(pckgs) <- pckgs
  req_pckgs <- sapply(pckgs, requireNamespace, quietly = TRUE)

  if (all(req_pckgs)) {
    .GlobalEnv$.app.study. <- study
    on.exit(rm(".app.study.", envir=.GlobalEnv))

    shiny::runApp(appDir = system.file("app", package = "metacheck"), quiet = quiet, ...) |> invisible()
  } else {
    warning("You need to install the following packages to run the app: ",
            paste(names(req_pckgs[!req_pckgs]), collapse = ", "))
  }
}
