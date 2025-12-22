#' Power
#'
#' @description
#' Find power analyses and return their components.
#'
#' @keywords method
#'
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Cristian Mesquida
#'
#' @import tidyr
#'
#' @param paper a paper object or paperlist object
#' @param seed a seed for the LLM
#' @param ... further arguments (not used)
#'
#' @returns a list
power <- function(paper, seed = 8675309) {
  # detailed table of results ----
  table <- paper |>
    # sentences containing the word power
    search_text("power", section = "method") |>
    # and containing at least one number
    search_text("[0-9]") |>
    expand_text(psychsci, "paragraph")

  table <- table[c("id", "section", "div", "p", "expanded")] |>
    unique()

  # optional LLM part
  if (nrow(table) > 0 && llm_use()) {

    query <- 'Does this sentence report a power analysis? If so, return the analysis type (apriori, post-hoc, or sensitivity); the statistical test; sample size; critical alpha criterion; power level; effect size; effect size metric; and software/method used, in JSON format like:

[{
  "type": "apriori",
  "test": "paired samples t-test",
  "sample": 20,
  "alpha": 0.05,
  "power": 0.8,
  "es": 0.4,
  "es_metric": "cohen\'s D",
  "software": "G*Power"
}]

If any parameters are not relevant or not found, return them as null.

If there is no power analysis, return only [{type: "none"}].

If there is more than one power analysis, return a list like [{...analysis1...}, {...analysis2...}]

Answer only in valid JSON format, starting and ending with [].'

    llm_power <- table |>
      llm(query, text_col = "expanded", seed = seed) |>
      json_expand() |>
      tidyr::separate_longer_delim(type:software, delim = "; ") |>
      dplyr::rowwise() |>
      dplyr::mutate(complete = dplyr::c_across(type:software) |> is.na() |> sum() == 0) |>
      dplyr::ungroup()

    table <- llm_power[llm_power$type != "none", ]

    # summary output for paperlists ----
    summary_table <- table |>
      dplyr::summarise(power.n = dplyr::n(),
                       power.complete = sum(complete),
                       dplyr::across(type:software,
                                     \(x) sum(!is.na(x)),
                                     .names = "power.{.col}"),

                       .by = id)
  } else {
    summary_table <- table |>
      dplyr::summarise(power.n = dplyr::n(),
                       power.complete = NA_integer_,
                       .by = id)
  }

  # determine the traffic light ----
  tl <- dplyr::case_when(
    nrow(table) == 0 ~ "na",
    isFALSE(llm_use()) ~ "yellow",
    all(summary_table$power.complete == summary_table$power.n) ~ "green",
    .default = "red"
  )

  summary_text <- c(
    na = "No power analyses were detected",
    red = "You included power analysis, but some essential reporting aspects appear missing.",
    yellow = "You included power analysis, but did not use an LLM to assess completeness, so should check manually.",
    green = "You included power analysis, and all essential reporting aspects appear present."
  )

  help_text <- "Power analyses need to contain the following information to be interpretable: the statistical test, sample size, critical alpha criterion, power level, effect size, and an effect size metric. For example:

> An a priori power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020) with Cohen's d = 0.5 and a critical alpha of p = 0.05, determined that 64 participants are required in each group for 80% power."


  report <- c(summary_text[[tl]],
              scroll_table(table$expanded, maxrows = 1),
              help_text) |>
    paste(collapse = "\n\n")

  list(
    table = table,
    summary_table = summary_table,
    na_replace = list(power.n = 0),
    traffic_light = tl,
    report = report,
    summary_text = summary_text[[tl]]
  )
}
