#' Set or get email
#'
#' @param email if a string, sets the email
#'
#' @returns the current option value (character)
#' @export
#'
#' @examples
#' email()
email <- function(email = NULL) {
  if (is.null(email)) {
    email <- getOption("metacheck.email") %||% "metacheck@scienceverse.org"
    return(email)
  } else if (is.character(email) && grepl(".+@.+\\..+$", email)) {
    options(metacheck.email = email)
    invisible(getOption("metacheck.email"))
  } else {
    stop("Set email with a valid email address")
  }
}


#' Psychological Science Open Access Paper Set
#'
#' 250 open access papers from Psychological Science.
#'
#' @format A list of 250 paper objects
#' @source \url{https://journals.sagepub.com/home/pss}
"psychsci"
