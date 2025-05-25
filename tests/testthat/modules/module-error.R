#' List All P-Values
#'
#' @description
#' List all p-values in the text, returning the matched text (e.g., 'p = 0.04')
#' and document location in a table.
#'
#' @author Lisa DeBruine
#'
#' @param paper a paper object or paperlist object
all_p_values <- function(paper)
  pattern <- "\\bp-?(value)?\\s*[<>=≤≥]{1,2}\\s*(n\\.?s\\.?|\\d?\\.\\d+)(e-\\d+)?"
  table <- search_text(paper, pattern, return = "match", "perl" = TRUE)

  list(
    table = table
  )
}
