#' Expand a JSON column
#'
#' It is useful to ask an LLM to return data in JSON structured format, but can be frustrating to extract the data, especially where the LLM makes syntax mistakes. This function tries to expand a column with a JSON-formatted response into columns and deals with it gracefully (sets an 'error' column to "parsing error") if there are errors. It also fixes column data types, if possible.
#'
#' @param table the table with a column to expand
#' @param col the name or index of the column to expand (defaults to "answer" or the first column)
#' @param suffix the suffix for the extracted columns if they conflict with names in the table
#'
#' @returns the table plus the expanded columns
#' @export
#'
#' @examples
#' table <- data.frame(
#'   id = 1:5,
#'   answer = c(
#'     '{"number": "1", "letter": "A", "bool": true}',
#'     '{"number": "2", "letter": "B", "bool": "FALSE"}',
#'     '{"number": "3", "letter": "", "bool": null}',
#'     'oh no, the LLM misunderstood',
#'     '{"number": "5", "letter": ["E", "F"], "bool": false}'
#'   )
#' )
#'
#' expanded <- json_expand(table, "answer")
#' expanded
json_expand <- function(table, col = "answer", suffix = c("", ".json")) {
  # handle non-table input
  if (is.vector(table)) table <- data.frame(json = table)
  if (is.null(table[[col]])) col <- 1

  table$.temp_id. <- seq_along(table[[1]])

  # expand JSON text
  to_expand <- table[[col]]
  expanded <- lapply(seq_along(to_expand), \(i) {
    tryCatch({
      json <- gsub('"null"', "null", to_expand[[i]])
      json <- gsub('.*```json\\s*\\n', "", json)
      json <- gsub('\\n\\s*```.*', "", json)
      j <- jsonlite::fromJSON(json)
      if (length(j) == 0) {
        j <- data.frame(error = NA_character_)
      }

      if (is.atomic(j) && is.null(names(j))) {
        j <- data.frame(error = "not a list")
      }

      # j <- lapply(j, \(x) I(list(x)))
      if (!is.data.frame(j)) {
        j <- lapply(j, paste, collapse = ";")
      }

      # make all character to avoid bind conflicts
      j[] <- lapply(j, as.character)
      j$.temp_id. <- i

      j
    }, error = \(e) {
      return(data.frame(.temp_id. = i, error = "parsing error"))
    })
  }) |>
    do.call(dplyr::bind_rows, args = _)


  # fix data types from making all char
  for (i in names(expanded)) {
    expanded[[i]] <- utils::type.convert(expanded[[i]], as.is = TRUE)
  }

  # get rid of error column if exists and no errors
  if (!is.null(expanded[["error"]]) && all(is.na(expanded$error))) {
    expanded$error <- NULL
  }

  # add expanded to table
  joined_tbl <- dplyr::left_join(table, expanded, by = ".temp_id.", suffix = suffix)
  joined_tbl$.temp_id. <- NULL

  return(joined_tbl)
}
