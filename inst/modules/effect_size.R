#' Missing Effect Sizes
#'
#' @description
#' Detect t-tests and F-tests with missing effect sizes
#'
#' @author Daniel Lakens
#' @author Lisa DeBruine
#'
#' @import dplyr
#' @import tidyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light, and report text
effect_size <- function(paper, ...) {
  # paper <- psychsci[[9]] # to test

  # Narrow down to sentences that could contain stats
  stat_sentences <- paper |>
    search_text("=") |> # sentences with an equal sign
    search_text("[0-9]") # sentences with numbers

  # t-tests ----

  ## detect tests ----
  test_regex <- paste0(
    "\\bt\\s*", # word border and t
    "(\\(\\s*\\d+(\\.\\d+)?\\s*\\))?", # optional df
    "\\s*=\\s*", # comparator
    "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?" # number
  )
  text_found_test <- stat_sentences |>
    search_text(test_regex, perl = TRUE, ignore.case = FALSE) |>
    dplyr::select(id, text, section, div, p, s)

  ## detect relevant effect sizes ----
  potentials <- c(
    "cohen('|\u2019)?s\\s+d",
    "cohen('|\u2019)?s\\s+d\\s*z",
    "d", "d\\s*z", "ds",
    "hedges?('|\u2019)?s?\\s+g",
    "g", "b", "r", "β"
  )

  es_regex <- paste0(
    "\\b", # word border
    "(", paste(potentials, collapse = "|"), ")",
    "\\b",
    "\\s*[=≈<>\u2264\u2265]{1,3}\\s*", # comparators
    "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?" # number
  )

  by <- c("id", "section", "div", "p", "s")
  text_found_es <- search_text(text_found_test, es_regex,
                               return = "match", perl = FALSE) |>
    dplyr::summarise(es = paste(text, collapse = "; "),
                     .by = dplyr::all_of(by))

  ## add exact text ----
  test_match <- search_text(text_found_test, test_regex, return = "match",
                            perl = TRUE, ignore.case = FALSE) |>
    dplyr::summarise(test_text = paste(text, collapse = "; "),
                     .by = dplyr::all_of(by))
  t_table <- text_found_test |>
    dplyr::left_join(text_found_es, by = by) |>
    dplyr::left_join(test_match,by = by)
  t_table$test <- "t-test"

  # F-tests -----

  ## detect tests ----
  test_regex <- paste0(
    "\\bF\\s*", # word border and F
    "\\(\\s*\\d+\\s*,\\s*\\d+\\s*\\)", # df (must be 2 integers)
    "\\s*=\\s*", # comparator
    "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?" # number
  )

  # sentences with a relevant test
  text_found_test <- stat_sentences |>
    search_text(test_regex, perl = TRUE, ignore.case = FALSE) |>
    dplyr::select(id, section, text, div, p, s)

  ## detect relevant effect sizes ----
  potentials <- c(
    "(C|c)ohen('|\u2019)?s\\s+f",
    "f\\s*(2|²)?",
    "η\\s*p*\\s*(2|²)",
    "(P|p)artial\\s+η\\s*(2|²)",
    "(O|o)mega\\s*(2|²)?",
    "ω\\s*(2|²)?",
    "(C|c)ohen('|\u2019)?s\\s+d",
    "d",
    "β",
    "η\\s*G\\s*(2|²)",
    "R\\s*(2|²)",
    "R", "r",
    "ω\\s*p\\s*(2|²)?",
    "BF\\s*\\d*"
  )

  es_regex <- paste0(
    "\\b", # word border
    "(", paste(potentials, collapse = "|"), ")",
    "\\s*[=≈<>\u2264\u2265]{1,3}\\s*", # comparators
    "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?" # number
  )

  text_found_es <- search_text(text_found_test, es_regex,
                               return = "match", perl = FALSE) |>
    dplyr::summarise(es = paste(text, collapse = "; "),
                     .by = dplyr::all_of(by))

  ## add exact text ----
  test_match <- search_text(text_found_test, test_regex, return = "match",
                            perl = TRUE, ignore.case = FALSE) |>
    dplyr::summarise(test_text = paste(text, collapse = "; "),
                     .by = dplyr::all_of(by))
  f_table <- text_found_test |>
    dplyr::left_join(text_found_es, by = by) |>
    dplyr::left_join(test_match,by = by)
  f_table$test <- "F-test"

  # combine tests ----
  table <- dplyr::bind_rows(t_table, f_table)

  ## summary table ----
  summary_table <- table |>
    dplyr::summarise(
      ttests_with_es = sum(test == "t-test" & !is.na(es)),
      ttests_without_es = sum(test == "t-test" & is.na(es)),
      Ftests_with_es = sum(test == "F-test" & !is.na(es)),
      Ftests_without_es = sum(test == "F-test" & is.na(es)),
      .by = dplyr::all_of(c("id")))

  # traffic light ----
  total_n <- nrow(table)
  noes_n <- is.na(table$es) |> sum()
  tl <- dplyr::case_when(
    total_n == 0 ~ "na",
    noes_n == 0 ~ "green",
    noes_n == total_n ~ "red",
    noes_n < total_n ~ "yellow",
    .default = "fail"
  )

  # report text ----
  report <- c(
    na = "No t-tests or F-tests were detected.",
    red = "No effect sizes were detected for any t-tests or F-tests. The Journal Article Reporting Standards state effect sizes should be reported.",
    yellow = "Effect sizes were detected for some, but not all t-tests or F-tests. The Journal Article Reporting Standards state effect sizes should be reported.",
    green = "All detected t-tests and F-tests had an effect size reported in the same sentence.",
    fail = "There was an error detecting effect sizes."
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
