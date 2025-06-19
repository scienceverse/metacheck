#' Get paper information in a table
#'
#' @param paper a paper object or a list of paper objects
#' @param info a vector of columns to return
#' @param path whether to return absolute or relative path for the filename
#'
#' @returns a data frame with each paper id and info columns
#' @export
#'
#' @examples
#' paper <- demodir() |> read_grobid()
#' info_table(paper)
info_table <- function(paper,
                       info = c("filename",
                                "title",
                                "keywords",
                                "doi"),
                       path = c("relative", "absolute")
                       ) {
  if (is_paper(paper)) {
    one_paper <- paper
    paper <- list(one_paper)
    names(paper) <- one_paper$name
  }

  infos <- lapply(paper, function(p) {
    info_table <- list()
    info_table$id <- p$id

    for (item in info) {
      if (item == "id") break
      value <- p$info[[item]]

      if (length(value) > 1) {
        value <- unlist(value) |> paste(collapse = "; ")
      } else if (length(value) == 0) {
        value <- NULL
      }

      info_table[[item]] <- value
    }

    info_table
  })
  df <- do.call(dplyr::bind_rows, infos)

  # add in any missing columns and reorder
  missing_cols <- setdiff(info, names(df))
  df[, missing_cols] <- NA
  if (!"id" %in% info) info <- c("id", info)
  df <- df[, info]

  # set filename to absolute or relative path
  if ("filename" %in% info) {
    df$filename <- normalizePath(df$filename, mustWork = FALSE)
    if (match.arg(path) == "relative") {
      wd <- getwd() |> normalizePath() |> paste0("/")
      df$filename <- gsub(wd, "", df$filename, fixed = TRUE)
    }
  }

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
