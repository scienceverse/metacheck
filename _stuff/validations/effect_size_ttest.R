#' Missing Effect Sizes (t-test)
#'
#' @description
#' Detect t-tests with missing effect sized
#'
#' @author Daniel Laken
#'
#' @import dplyr
#' @import tidyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light, and report text
detect_missing_effect_size_ttest <- function(paper, ...) {
  # Regex to detect all t-tests
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


  # Regex to detect effect sizes
  potentials <- c(
    "cohen('|\u2019)?s\\s+d",
    "d", "dz", "ds",
    "hedges?('|\u2019)?s?\\s+g",
    "g", "b", "r"
    # "cohen('|\u2019)?s\\s+f",
    # "f\\s*(2|²)?",
    # "omega\\s*(2|²)?",
    # "ω\\s*(2|²)?",
    # "η\\s*p*\\s*(2|²)",
    # "partial\\s+η\\s*(2|²)"
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

  summary_not <- dplyr::count(es_not_reported, id,
                              name = "ttests_without_es")
  summary_test <- dplyr::count(text_found_test, id,
                            name = "ttests_n")
  summary_es <- dplyr::count(text_found_es, id,
                            name = "ttests_with_es")
  summary_table <- summary_test |>
    dplyr::left_join(summary_es, by = "id") |>
    dplyr::left_join(summary_not, by = "id") |>
    dplyr::mutate(
      ttests_with_es = tidyr::replace_na(ttests_with_es, 0),
      ttests_without_es = tidyr::replace_na(ttests_without_es, 0)
    )

  total_n <- nrow(text_found_test)
  noes_n <- nrow(es_not_reported)
  tl <- dplyr::case_when(
    total_n == 0 ~ "na",
    noes_n == 0 ~ "green",
    noes_n == total_n ~ "red",
    noes_n < total_n ~ "yellow",
    .default = "fail"
  )

  report <- c(
    na = "No t-tests were detected.",
    red = "No effect sizes were detected for any t-tests. The Journal Article Reporting Standards state effect sizes should be reported.",
    yellow = "Effect sizes were detected for some, but not all t-tests. The Journal Article Reporting Standards state effect sizes should be reported.",
    green = "All detected t-tests had an effect size reported in the same sentence.",
    fail = "There was an error detecting t-tests."
  )

  # return a list ----
  list(
    table = es_not_reported,
    summary = summary_table,
    na_replace = list(ttests_n = 0),
    traffic_light = tl,
    report = report[[tl]]
  )
}
