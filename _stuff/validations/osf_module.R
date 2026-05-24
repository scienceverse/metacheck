#' OSF Module
#'
#' @param paper the metacheck object
#'
#' @returns list
osf_module <- function(paper) {
  links <- osf_links(paper)
  contents <- osf_info(links,
                           recursive = TRUE,
                           find_project = TRUE)
  summary <- file_category(contents)

  #return
  list(
    check = 1,
    summary = summary
  )
}
