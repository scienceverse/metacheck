#' Missing Effect Sizes in t-tests and F-tests
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
    total_n == 0 ~ "na",
    noes_n == 0 ~ "green",
    noes_n == total_n ~ "red",
    noes_n < total_n ~ "yellow",
    .default = "fail"
  )

  if (nrow(table_missing) == 0) {
    report <- "No t-tests or F-tests were detected, or all detected t-tests and F-tests had an effect size reported in the same sentence."
  } else {
    module_output <- sprintf(
      "We found %d t-tests and/or F-tests where effect sizes are not reported. We recommend checking the sentences below, and add any missing effect sizes.",
      nrow(table_missing)
    )
    issues_found <- paste(sprintf("**%s**", table_missing$text), collapse = "\n\n")
    if (nrow(table_missing) == 0) {
      report <- "No t-tests or F-tests were detected, or all detected t-tests and F-tests had an effect size reported in the same sentence."
    } else {
      module_output <- sprintf(
        "We found <strong><span style='color:#006400;'>%d</span></strong> t-tests and/or F-tests where effect sizes are not reported. We recommend checking the sentences below, and add any missing effect sizes.",
        nrow(table_missing)
      )
      issues_found <- paste(
        sprintf("<li style='border-bottom:1px solid #ddd; padding-bottom:6px; margin-bottom:6px;'>%s</li>", table_missing$text),
        collapse = "\n"
      )
      sentences_block <- paste0(
        "<div style='border:1px solid #ccc; padding:10px; ",
        "max-height:250px; overflow-y:auto; background-color:#f9f9f9; ",
        "margin-top:5px; margin-bottom:15px;'>",
        
        "<ul style='list-style-type: circle; padding-left:20px; margin:0;'>",
        issues_found,
        "</ul>",
        
        "</div>"
      )
      
      # I wrapped the guidance in a block that collapses + increased the size of the heading and made the color dark green
      # I also used bullets & BOLD labels so both guidance resources look clearly separated 
      guidance <- paste0(
        "<ul style='list-style-type: circle; padding-left: 20px;'>",
        "<li><strong>For metascientific articles demonstrating that effect sizes are often not reported</strong>:<br>",
        "Peng, C.-Y. J., Chen, L.-T., Chiang, H.-M., & Chiang, Y.-C. (2013). The Impact of APA and AERA Guidelines on Effect Size Reporting. Educational Psychology Review, 25(2), 157–209. ",
        "<a href='https://doi.org/10.1007/s10648-013-9218-2' target='_blank'>https://doi.org/10.1007/s10648-013-9218-2</a>",
        "</li><br>",
        "<li><strong>For educational material on reporting effect sizes</strong>:<br>",
        "<a href='https://matthewbjane.quarto.pub/guide-to-effect-sizes-and-confidence-intervals/' target='_blank'>https://matthewbjane.quarto.pub/guide-to-effect-sizes-and-confidence-intervals/</a>",
        "</li>",
        "</ul>"
      )

      guidance_block <- paste0(
        "<details style='display:inline-block;'>",
        "<summary style='cursor:pointer; margin:0; padding:0;'>",
        "<strong><span style='font-size:20px; color:#006400;'>Guidance</span></strong>",
        "</summary>",
        "<div style='margin-top:10px;'>",
        guidance,
        "</div>",
        "</details>"
      )
      
      ## I can use this color "#1BA57B" if I want the same green as the one on the side in the table of contents!! 
      # I made the detailed table here become expandable + chose the columns (can be changed) 
      table_subset <- table[, c("text", "section", "es", "test_text", "test"), drop=FALSE]
      colnames(table_subset) <- c("Sentence", "Section", "Effect Size", "Reported Test", "Test Type")
      
      table_headers <- paste0(
        "<tr>",
        paste(
          sprintf(
            "<th style='border:1px solid #ccc; padding:4px; background-color:#f0f0f0;'>%s</th>",
            names(table_subset)
          ),
          collapse = ""
        ),
        "</tr>"
      )
      
      table_rows <- apply(table_subset, 1, function(row) {
        paste0(
          "<tr>",
          paste(
            sprintf("<td style='border:1px solid #ccc; padding:4px;'>%s</td>", row),
            collapse = ""
          ),
          "</tr>"
        )
      })
      table_rows <- paste(table_rows, collapse = "\n")
      
      detailed_table_html <- paste0(
        "<table style='border-collapse:collapse; width:100%; font-size:90%;'>",
        "<thead>", table_headers, "</thead>",
        "<tbody>", table_rows, "</tbody>",
        "</table>"
      )
      
      detailed_table_block <- paste0(
        "<details style='display:block; margin-top:15px;'>",
        "<summary style='cursor:pointer; margin:0; padding:0;'>",
        "<strong><span style='font-size:20px; color:#006400;'>Detailed Table</span></strong>",
        "</summary>",
        "<div style='margin-top:10px; max-height:450px; overflow:auto; border:1px solid #999; padding:5px; background-color:#ffffff;'>",
        detailed_table_html,
        "</div>",
        "</details>"
      )
      
      # I structured the report so the main summary is followed by the missing sentences, guidance, and the detailed table (expandable) in one section!
      report <- sprintf(
        "%s\n\n<div><strong>The Following Sentences are Missing Effect Sizes:</strong></div>\n\n%s\n\n%s\n\n%s",
        module_output, sentences_block, guidance_block, detailed_table_block
      )
    }
  }

  # return a list ----
  list(
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report
  )
}
