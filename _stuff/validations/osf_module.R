#' OSF Module
#'
#' @param paper the papercheck object
#'
#' @returns list
osf_module <- function(paper) {
  links <- osf_links(paper)
  contents <- osf_retrieve(links,
                           recursive = TRUE,
                           find_project = TRUE)
  summary <- summarize_contents(contents)

  #return
  list(
    check = 1,
    summary = summary
  )
}
