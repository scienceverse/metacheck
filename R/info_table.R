#' Get paper information in a table
#'
#' @param paper a paper object or a list of paper objects
#' @param info a vector of columns to return
#'
#' @returns a data frame with each paper id and info columns
#' @export
#'
#' @examples
#' info_table(psychsci[1:5])
info_table <- function(paper,
                       info = c(
                         "title",
                         "doi"
                       )) {
  if (is_paper(paper)) {
    one_paper <- paper
    paper <- list(one_paper)
    names(paper) <- one_paper$id
  }

  df <- concat_tables(paper, "info")

  # add in any missing columns and reorder
  missing_cols <- setdiff(info, names(df))
  df[, missing_cols] <- NA

  if (!"id" %in% info) info <- c("id", info)
  df <- df[, info]

  return(df)
}


#' Get author information in a table
#'
#' @param paper a paper object or a list of paper objects
#'
#' @returns a data frame of author information
#' @export
#'
#' @examples
#' paper <- psychsci[1:2]
#' author_table(paper)
author_table <- function(paper) {
  if (is_paper(paper)) {
    paper <- list(paper)
    names(paper) <- paper[[1]]$id
  }

  dfs <- lapply(paper, function(p) {
    u <- lapply(p$authors, function(a) {
      a$affiliation <- sapply(a$affiliation, paste, collapse = ", ") |>
        paste(collapse = "; ")
      unlist(a)
    })
    df <- do.call(dplyr::bind_rows, u)
    df$id <- rep(p$id, nrow(df))
    df$n <- seq_along(df$id)

    df
  })

  do.call(dplyr::bind_rows, dfs)
}
