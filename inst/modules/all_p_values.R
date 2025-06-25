#' List All P-Values
#'
#' @description
#' List all p-values in the text, returning the matched text (e.g., 'p = 0.04')
#' and document location in a table.
#'
#' @author Lisa DeBruine
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light
#'
#' @examples
#' module_run(psychsci, "all_p_values")
all_p_values <- function(paper, ...) {
  # set up pattern ----
  operators <- c("=", "<", ">", "~",
                 "\u2248", # ~~
                 "\u2260", # !=
                 "\u2264", # <=
                 "\u2265", # >=
                 "\u226A", # <<
                 "\u226B" # >>
  ) |> paste(collapse = "")

  pattern <- paste0(
    "\\bp-?(value)?\\s*", # ways to write p
    "[", operators, "]{1,2}\\s*", # 1-2 operators
    "(n\\.?s\\.?|\\d?\\.\\d+)(e-\\d+)?" # ns or valid numbers
  )

  # detailed table of results ----
  p <- search_text(paper, pattern, return = "match",
                       perl = TRUE, ignore.case = FALSE)

  # get operator
  pattern <- paste0("[", operators, "]{1,2}")
  matches <- gregexpr(pattern, p$text, perl = TRUE)
  p$p_comp <- regmatches(p$text, matches) |> sapply(`[[`, 1)

  # get value
  pattern <- paste0("(?<=[", operators, "]{1,2}).*$")
  matches <- gregexpr(pattern, p$text, perl = TRUE)
  p$p_value <- regmatches(p$text, matches) |> trimws()
  p$p_value <- suppressWarnings(as.numeric(p$p_value))

  # summary output for paperlists ----
  summary_table <- dplyr::count(p, id, name = "p_values")

  # determine the traffic light ----
  tl <- if (nrow(p)) "info" else "na"

  # return a list ----
  list(
    table = p,
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl
  )
}
