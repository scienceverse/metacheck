#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end

# Suppress R CMD check note for packages used only in modules
#' @importFrom DT datatable
#' @importFrom oddpub open_data_search
NULL

#' @export
#' @importFrom svutils pb
svutils::pb

#' @export
#' @importFrom svutils verbose
svutils::verbose

#' @export
#' @importFrom svutils %||%
svutils::`%||%`

#' @export
#' @importFrom svutils message
svutils::message

#' @export
#' @importFrom svutils online
svutils::online

#' @export
#' @importFrom svutils xml_find
svutils::xml_find

#' @export
#' @importFrom svutils xml_find1
svutils::xml_find1
