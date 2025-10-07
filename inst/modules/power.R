#' Power
#'
#' @description
#' Find power analyses and return their components.
#'
#' @author Daniel Lakens, Lisa DeBruine, Cristian Mesquida
#'
#' @import tidyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, traffic light, and report text
#'
#' @examples
#' module_run(psychsci, "power")
power <- function(paper, seed = 8675309) {
  # detailed table of results ----
  table <- paper |>
    # sentences containing the word power
    search_text("power", section = "method") |>
    # and containing at least one number
    search_text("[0-9]") |>
    expand_text(psychsci, "paragraph")

  # optional LLM part
  if (Sys.getenv("GROQ_API_KEY") != "") {

    query <- 'Does this sentence report a power analysis? If so, return the analysis type (apriori, post-hoc, or sensitivity); the statistical test; sample size; critical alpha criterion; power level; effect size; and effect size metric, in JSON format like:

{
  "type": "apriori",
  "test": "paired samples t-test",
  "sample": 20,
  "alpha": 0.05,
  "power": 0.8,
  "es": 0.4,
  "es_metric": "cohen\'s D"
}

If any parameters are not relevant or not found, return them as "NA". If there is no power analysis, return type as "none" and all other parameters as "NA".

If there is more than one power analysis, return a list like [{...analysis1...}, {...analysis2...}]

Answer only in valid JSON format, starting and ending with [].'

    llm_power <- table[c("id", "section", "div", "p", "expanded")] |>
      unique() |>
      llm(query, text_col = "expanded", seed = seed) |>
      json_expand() |>
      tidyr::separate_longer_delim(type:es_metric, delim = "; ")

    table <- llm_power[llm_power$type != "none", ]
  }

  # summary output for paperlists ----
  summary_table <- dplyr::count(table, id, name = "power")

  # determine the traffic light ----
  tl <- ifelse(nrow(table), "red", "green")

  report = c(
    red = "You did not include any power analyses.",
    green = "You included a power anlaysis."
  )

  list(
    table = table,
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report[[tl]]
  )
}
