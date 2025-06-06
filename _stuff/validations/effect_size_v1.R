#' Missing Effect Sizes (v1)
#'
#' @description
#' Detect t-tests and F-tests  with missing effect sizes
#'
#' @author Daniel Laken
#' @author Lisa DeBruine
#'
#' @import dplyr
#' @import tidyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light, and report text
detect_missing_effect_size_ttest <- function(paper, ...) {
  # t-tests ----
  test_regex <- paste0(
    "\\bt\\s*", # word border and t
    "(\\(\\s*\\d+(\\.\\d+)?\\s*\\))?", # optional df
    "\\s*=\\s*", # comparator
    "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?" # number
  )
  text_found_test <- paper |>
    search_text("=") |> # sentences with equal signs
    search_text("[0-9]") |> # sentences with numbers
    # sentences with a relevant test
    search_text(test_regex, perl = TRUE, ignore.case = FALSE)

  t_total_n <- nrow(text_found_test)

  # Regex to detect t-test relevant effect sizes
  potentials <- c(
    "cohen('|\u2019)?s\\s+d",
    "d", "dz", "ds",
    "hedges?('|\u2019)?s?\\s+g",
    "g", "b", "r"
  )

  es_regex <- paste0(
    "\\b", # word border
    "(", paste(potentials, collapse = "|"), ")",
    "\\s*[=≈<>\u2264\u2265]{1,3}\\s*", # comparators
    "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?" # number
  )
  text_found_es <- search_text(text_found_test, es_regex, perl = FALSE)

  # Identify t-tests without reported effect sizes
  es_not_reported <- dplyr::anti_join(
    text_found_test,
    text_found_es,
    by = "text"
  )

  es_not_reported$test <- "t-test"

  ## add exact text
  test_match <- search_text(text_found_test, test_regex, return = "match",
                            perl = TRUE, ignore.case = FALSE) |>
    dplyr::summarise(test_text = paste(text, collapse = "; "),
                     .by = c("div", "p", "s", "id"))
  t_table <- dplyr::left_join(es_not_reported, test_match,
                               by = c("div", "p", "s", "id"))

  ## summary table ----
  summary_not <- dplyr::count(es_not_reported, id,
                              name = "ttests_without_es")
  summary_test <- dplyr::count(text_found_test, id,
                               name = "ttests_n")
  summary_es <- dplyr::count(text_found_es, id,
                             name = "ttests_with_es")
  t_summary_table <- summary_test |>
    dplyr::left_join(summary_es, by = "id") |>
    dplyr::left_join(summary_not, by = "id") |>
    dplyr::mutate(
      ttests_with_es = tidyr::replace_na(ttests_with_es, 0),
      ttests_without_es = tidyr::replace_na(ttests_without_es, 0)
    )

  # F-tests -----
  test_regex <- paste0(
    "\\bF\\s*", # word border and F
    "\\(\\s*\\d+\\s*,\\s*\\d+\\s*\\)", # df (must be 2 integers)
    "\\s*=\\s*", # comparator
    "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?" # number
  )
  text_found_test <- paper |>
    search_text("=") |> # sentences with equal signs
    search_text("[0-9]") |> # sentences with numbers
    search_text(test_regex, perl = TRUE, ignore.case = FALSE) # sentences with a relevant test

  f_total_n <- nrow(text_found_test)

  # Regex to detect tests with reported effect sizes
  potentials <- c(
    "(C|c)ohen('|\u2019)?s\\s+f",
    "f\\s*(2|²)?",
    "η\\s*p*\\s*(2|²)",
    "(P|p)artial\\s+η\\s*(2|²)",
    "(O|o)mega\\s*(2|²)?",
    "ω\\s*(2|²)?"
  )

  es_regex <- paste0(
    "\\b", # word border
    "(", paste(potentials, collapse = "|"), ")",
    "\\s*[=≈<>\u2264\u2265]{1,3}\\s*", # comparators
    "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?" # number
  )

  text_found_es <- search_text(text_found_test, es_regex,
                               perl = FALSE)

  # Identify t-tests without reported effect sizes
  es_not_reported <- dplyr::anti_join(
    text_found_test,
    text_found_es,
    by = "text"
  )

  es_not_reported$test <- "F-test"

  ## add exact text
  test_match <- search_text(text_found_test, test_regex, return = "match",
                            perl = TRUE, ignore.case = FALSE) |>
    dplyr::summarise(test_text = paste(text, collapse = "; "),
                     .by = c("div", "p", "s", "id"))
  f_table <- dplyr::left_join(es_not_reported, test_match,
                              by = c("div", "p", "s", "id"))

  ## summary table ----
  summary_not <- dplyr::count(es_not_reported, id,
                              name = "Ftests_without_es")
  summary_test <- dplyr::count(text_found_test, id,
                               name = "Ftests_n")
  summary_es <- dplyr::count(text_found_es, id,
                             name = "Ftests_with_es")
  f_summary_table <- summary_test |>
    dplyr::left_join(summary_es, by = "id") |>
    dplyr::left_join(summary_not, by = "id") |>
    dplyr::mutate(
      Ftests_with_es = tidyr::replace_na(Ftests_with_es, 0),
      Ftests_without_es = tidyr::replace_na(Ftests_without_es, 0)
    )

  # combine tests ----
  table <- dplyr::bind_rows(t_table, f_table)
  summary_table <- dplyr::left_join(t_summary_table, f_summary_table, by = "id")

  # traffic light ----
  total_n <- t_total_n + f_total_n
  noes_n <- nrow(table)
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
