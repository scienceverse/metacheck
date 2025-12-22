#' Effect Sizes in t-tests and F-tests
#'
#' @description
#' The Effect Size module checks for effect sizes in t-tests and F-tests.
#'
#' @details
#' The Effect Size check searches for regular expressions that match a predefined pattern. The module was validated on APA reported statistical tests, and might miss effect sizes that were reported in other reporting styles. It was validated by the Metacheck team on papers published in Psychological Science.
#'
#' If you want to extend the package to detect effect sizes for additional tests, reach out to the Metacheck development team.
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#' @import tidyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
stat_effect_size <- function(paper) {
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
  table_missing <- subset(table, is.na(es))

  ## summary table ----
  summary_table <- table |>
    dplyr::summarise(
      ttests_with_es = sum(test == "t-test" & !is.na(es)),
      ttests_without_es = sum(test == "t-test" & is.na(es)),
      Ftests_with_es = sum(test == "F-test" & !is.na(es)),
      Ftests_without_es = sum(test == "F-test" & is.na(es)),
      .by = dplyr::all_of(c("id")))

  # traffic light ----
  total_n <- nrow(table_missing)
  noes_n <- is.na(table_missing$es) |> sum()
  tl <- dplyr::case_when(
    total_n > 0 & noes_n == 0 ~ "green",
    total_n == 0 ~ "na",
    noes_n == total_n ~ "red",
    noes_n < total_n ~ "yellow",
    .default = "fail"
  )

  # report / summary_text ----
  if (tl == "na") {
    report <- "No t-tests or F-tests were detected."
    summary_text <- report
  } else if (tl == "green") {
    report <- "All detected t-tests and F-tests had an effect size reported in the same sentence."
    summary_text <- report
  } else {
    module_output <- sprintf(
      "We found %1$d t-test%2$s and/or F-test%2$s where effect sizes are not reported. We recommend checking the sentences below, and add any missing effect sizes.",
      nrow(table_missing),
      ifelse(nrow(table_missing) == 1, "", "s")
    )

    summary_text <- sprintf(
      "We found %1$d t-test%2$s and/or F-test%2$s where effect sizes are not reported.",
      nrow(table_missing),
      ifelse(nrow(table_missing) == 1, "", "s")
    )

    guidance <- c(
      "For metascientific articles demonstrating that effect sizes are often not reported:",
      "* Peng, C.-Y. J., Chen, L.-T., Chiang, H.-M., & Chiang, Y.-C. (2013). The Impact of APA and AERA Guidelines on Effect Size Reporting. Educational Psychology Review, 25(2), 157–209. doi:[10.1007/s10648-013-9218-2](https://doi.org/10.1007/s10648-013-9218-2).",
      "For educational material on reporting effect sizes:",
      "* [Guide to Effect Sizes and Confidence Intervals](https://matthewbjane.quarto.pub/guide-to-effect-sizes-and-confidence-intervals/)"
    )

    # select cols for the report table
    cols <- c("text", "section", "es", "test_text", "test")
    report_table <- table[, cols, drop=FALSE]
    colnames(report_table) <- c("Sentence", "Section", "Effect Size", "Reported Test", "Test Type")
    # wrap in a collapsible
    detail_table <- scroll_table(report_table) |>
      collapse_section("All detected and assessed stats")

    # structure the report in order
    report <- c(
      module_output,
      "The following sentences are missing effect sizes",
      scroll_table(table_missing$text),
      collapse_section(guidance),
      detail_table
    )
  }

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}
