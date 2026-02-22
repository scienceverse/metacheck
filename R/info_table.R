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

  if (!"paper_id" %in% info) info <- c("paper_id", info)
  df <- df[, info]

  return(df)
}

