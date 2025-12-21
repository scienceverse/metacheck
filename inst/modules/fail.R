#' Meant to Fail
#'
#' @description
#' Test failure in a pipeline.
#'
#' @keywords general
#'
#' @author LD

#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list
#'
#' @examples
#' module_run(psychsci, "fail")
fail <- function(paper, ...) {
  stop("This module fails on purpose")
}
