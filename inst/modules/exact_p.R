#' Exact P-Values
#'
#' @description
#' List any p-values reported with insufficient precision (e.g., p < .05 or p = n.s.)
#'
#' @author Lisa DeBruine
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light, and report text
#'
#' @examples
#' module_run(psychsci, "exact_p")
exact_p <- function(paper, ...) {
  # detailed table of results ----
  p <- module_run(paper, "all_p_values")$table

  operators <- c("=", "<", ">", "~",
                 "\u2248", # ~~
                 "\u2260", # !=
                 "\u2264", # <=
                 "\u2265", # >=
                 "\u226A", # <<
                 "\u226B" # >>
  ) |> paste(collapse = "")

  # get operator
  pattern <- paste0("[", operators, "]{1,2}")
  matches <- gregexpr(pattern, p$text, perl = TRUE)
  p$p_comp <- regmatches(p$text, matches) |> sapply(`[[`, 1)

  # get value
  pattern <- paste0("(?<=[", operators, "]{1,2}).*$")
  matches <- gregexpr(pattern, p$text, perl = TRUE)
  p$p_value <- regmatches(p$text, matches) |> trimws()
  p$p_value <- suppressWarnings(as.numeric(p$p_value))

  p$imprecise <- p$p_comp == "<" & p$p_value > .001
  p$imprecise <- p$imprecise | !p$p_comp %in% c("=", "<")
  p$imprecise <- p$imprecise | is.na(p$p_value)
  cols <- c("text", "p_comp", "p_value", "section", "header", "div", "p", "s", "id")
  table <- p[p$imprecise, cols]

  # summary output for paperlists ----
  summary_table <- dplyr::count(p, id, imprecise) |>
    dplyr::mutate(imprecise = factor(imprecise, c("FALSE", "TRUE"))) |>
    tidyr::pivot_wider(names_from = imprecise, values_from = n,
                       # in case no instances of T or F
                       names_expand = TRUE, values_fill = 0) |>
    dplyr::rename(exact_p = `FALSE`, imprecise_p = `TRUE`)

  # determine the traffic light ----
  tl <- dplyr::case_when(
    nrow(p) == 0 ~ "na",
    any(p$imprecise) ~ "red",
    !all(p$imprecise) ~ "green",
    .default = "yellow"
  )

  # report text for each possible traffic light ----
  report <- c(
    red = "You may have reported some imprecise p-values",
    yellow ="You may have reported some imprecise p-values",
    green = "All p-values were reported with standard precision",
    na = "No p-values were detected"
  )

  # return a list ----
  list(
    table = table,
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report[[tl]]
  )
}
