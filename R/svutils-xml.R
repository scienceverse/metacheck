#' Find and return text from XML by xpath
#'
#' This function trims and replaces multiple spaces
#'
#' @param xml the xml document, node, or nodeset
#' @param xpath a string containing an xpath expression
#' @param join optional string to join vectors
#'
#' @returns text
#' @export
#' @keywords internal
.xml_find_text <- function(xml, xpath, join = NULL) {
  text <- xml2::xml_find_all(xml, xpath) |>
    xml2::xml_text(trim = TRUE) |>
    gsub(" +", " ", x = _)

  if (!is.null(join)) text <- paste(text, collapse = join)

  if (length(text) == 0) text <- ""

  return(text)
}

#' Find and return first text from XML by xpath
#'
#' @param xml the xml document, node, or nodeset
#' @param xpath a string containing an xpath expression
#'
#' @returns text
#' @export
#' @keywords internal
.xml_find1_text <- function(xml, xpath) {
  .xml_find_text(xml, xpath, join = NULL)[[1]]
}


#' Read in a grobid XML file
#'
#' Read in, strip TEI namespace, and fix common problems.
#'
#' @param path Path to a grobid/TEI XML file
#'
#' @returns a list with class "xml_document" for xml2
#' @export
#' @keywords internal
#'
#' @examples
#' path <- demofile("xml")
#' xml <- .xml_read_grobid(path)
.xml_read_grobid <- function(path) {
  if (!file.exists(path)) stop("The XML file does not exist.")

  operators <- c(
    "=", "<", ">", "~",
    "\u2248", # ~~
    "\u2260", # !=
    "\u2264", # <=
    "\u2265", # >=
    "\u226A", # <<
    "\u226B" # >>
  ) |> paste(collapse = "")
  num_pre_op <- sprintf("\\s+([\u00B20-9.]+\\s*[%s])", operators)

  xml_text <- readLines(path, warn = FALSE) |>
    paste(collapse = "\n") |>
    # gsub(' xmlns="http://www.tei-c.org/ns/1.0"', "",
    #      x = _, fixed = TRUE
    # ) |>
    # fix common mangled stats
    #gsub("\u00B2",  "2", x = _) |> # squared -> 2
    gsub(num_pre_op, "\\1", x = _) |>    # "XX  2 = ?" -> "XX2 = ?"
    gsub("r\\s*p\\s*2",                  "rp\u00B2", x = _) |>
    gsub("\u03C\\s*p\\s*2",              "\u03Cp\u00B2", x = _) |> # omega
    gsub("\u03B7\\s*p\\s*[2\u00B2]",     "\u03B7p\u00B2", x = _) |> # eta
    gsub("\u03B7\\s*G\\s*[2\u00B2]",     "\u03B7G\u00B2", x = _) |> # eta
    gsub("\u03B7\\s*[2\u00B2]",          "\u03B7\u00B2", x = _) |> # eta
    gsub("\u03C4\\s*[2\u00B2]",          "\u03C4\u00B2", x = _) |> # tau
    gsub("\\br\\s*[2\u00B2](\\s*[=><])", "r\u00B2\\1", x = _) |>
    gsub("\\bR\\s*[2\u00B2]\\s+M\\b",    "R\u00B2M", x = _) |>
    gsub("\\bR\\s*[2\u00B2]\\b",         "R\u00B2", x = _) |>
    gsub("\\bI\\s*[2\u00B2]\\b",         "I\u00B2", x = _) |>
    gsub("\u03A7\\s*[2\u00B2]",          "\u03A7\u00B2", x = _) |> # chi
    gsub("\\bf\\s*[2\u00B2](\\s*[=><])", "f\u00B2\\1", x = _) |>
    gsub("\u03A7[2\u00B2]\\s*\\((\\s*\\d+)\\s*\\)", "\u03A7\u00B2(\\1)", x = _) |> # chi
    gsub("\\br\\s*\\((\\s*\\d+)\\s*\\)", "r(\\1)", x = _) |>
    gsub("\\bd\\s+z\\b",                 "dz", x = _) |> # sz
    gsub("\\bd\\g+z\\b",                 "gz", x = _) |># gz
    gsub("\\bBF\\s+([10]{2})\\b",        "BF\\1", x = _) |> # BF 10; BF 01

    gsub("(https?://)\\s+", "\\1", x = _) |> # whitespace in url
    gsub("(\\d\\.)\\s+(\\d)", "\\1\\2", x = _) |> # #. #
    gsub("\\b[Ff]ig\\. (\\D?\\d)", "Fig \\1", x = _) |>
    gsub("\\b[Ff]igure\\. (\\d)", "Figure \\1", x = _) |>
    gsub("\\b[Tt]ab\\. (\\d)", "Tab \\1", x = _) |>
    gsub("\\b[Tt]able\\. (\\d)", "Table \\1", x = _) |>
    gsub("</ref><ref", "</ref> <ref", x = _, fixed = TRUE)

  xml2::read_xml(xml_text) |>
    xml2::xml_ns_strip() #strip tei namsapce to make find simpler
}
