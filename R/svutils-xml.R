#' Find and return info from XML by xpath
#'
#' @param xml the xml document, node, or nodeset
#' @param xpath a string containing an xpath expression
#' @param join optional string to join vectors
#'
#' @returns text
#' @export
xml_find <- function(xml, xpath, join = NULL) {
  text <- xml2::xml_find_all(xml, xpath) |>
    xml2::xml_text(trim = TRUE) |>
    gsub("\\s+", " ", x = _)

  if (!is.null(join)) text <- paste(text, collapse = join)

  if (length(text) == 0) text <- ""

  return(text)
}

#' Find and return first info from XML by xpath
#'
#' @param xml the xml document, node, or nodeset
#' @param xpath a string containing an xpath expression
#' @param join optional string to join vectors
#'
#' @returns text
#' @export
xml_find1 <- function(xml, xpath, join = NULL) {
  xml_find(xml, xpath, join)[[1]]
}
