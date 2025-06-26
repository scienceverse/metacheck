#' List All P-Values
#'
#' @description
#' List all p-values in the text, returning the matched text (e.g., 'p = 0.04')
#' and document location in a table.
#'
#' Note that this will not catch p-values reported like "the p-value is 0.03" because that results in a ton of false positives when papers discuss p-value thresholds. If you need to detect text like that, use `search_text()` function and a custom pattern like "\\bp(-| )?values?\\s+.{1,20}\\s+[0-9\\.]+"
#'
#' This will catch most comparators like =<>~≈≠≤≥≪≫ and most versions of scientific notation like 5.0 x 10^-2 or 5.0e-2. If you find any formats that are not correctly handled by this function, please contact the author.
#'
#' @author Lisa DeBruine (debruine@gmail.com)
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
    "(n\\.?s\\.?|\\d?\\.\\d+)",# ns or valid numbers
    "\\s*(e\\s*-\\d+)?", # also match scientific notation
    "(\\s*[x\\*]\\s*10\\s*\\^\\s*-\\d+)?"
  )

  # detailed table of results ----
  p <- search_text(paper, pattern, return = "match",
                       perl = TRUE, ignore.case = FALSE)

  # get operator
  pattern <- paste0("[", operators, "]{1,2}")
  matches <- gregexpr(pattern, p$text, perl = TRUE)
  p$p_comp <- regmatches(p$text, matches) |> sapply(`[[`, 1)

  # get value
  s <- strsplit(p$text, paste0("\\s*[", operators, "]{1,2}\\s*"))
  pvals <- sapply(s, \(x) x[[2]]) |>
    gsub("\\s", "", x = _) |>
    gsub("[x*]10\\^", "e", x = _)
  p$p_value <- suppressWarnings(as.numeric(pvals))

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
