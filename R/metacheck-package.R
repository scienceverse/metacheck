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
