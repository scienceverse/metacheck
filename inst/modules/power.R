#' Power Analysis Check
#'
#' @description
#' This module uses a large language module (LLM) to extract information reported
#' in a priori power analyses, including the statistical test, sample size, alpha
#' level, desired level of power,and magnitude and type of effect size.
#'
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#' @author Daniel Lakens (\email{d.lakens@tue.nl})
#' @author Cristian Mesquida (\email{c.mesquida.caldentey@tue.nl})
#'
#' @import dplyr
#' @import tidyr
#'
#' @param paper a paper object or paperlist object
#' @param seed a seed for the LLM
#'
#' @returns a list with table, summary, traffic light, and report text
#'
#' @details
#' The Power Analysis Check module uses regular expressions to identify sentences that contain a statistical power analysis. Without the use of an LMM, the module uses regular expressions to classify the power analysis as a-priori, sensitivity, or post-hoc. With the use of an LMM, it check if the power analysis is reported with all required information.
#'
#' The regular expressions can miss power analyses, or fail to classify them correctly. The type of power analysis is often difficult to classify, which can easily be solved by explicitly specifying the type of power analysis as 'a-priori', 'sensitivity', or 'post-hoc'. Note that 'post-hoc' or 'observed' power is rarely useful. The LMM can fail to identify information in the paper, and will not have access to information in other paragraphs in the paper than those that contain the word 'power'. This package was validated by the Metacheck team on articles in Psychological Science.
#'
#' @examples
#' module_run(psychsci, "power")
power <- function(paper, seed = 8675309) {
  # Build the word pattern to detect power analyses
  word_pattern <- paste(
    c(
      "a[- ]?priori",
      "G\\*Power",
      "sample[- ]size (estimation|calculation)",
      "a[- ]?posteriori",
      "post[- ]?hoc",
      "sensitivity"
    ),
    collapse = "|"
  )

  # Pattern for digits, but not years
  numeric_pattern <- "\\b(?!\\d{4}\\b)\\d+(?:[.,]\\d+)?\\b"

  # Step 1: Search for "power"
  table <- search_text(
    paper,
    pattern = "power",
    return = "paragraph"
  )

  # Step 3: Search for numeric content
  table <- search_text(
    table, # only keep paragraphs with a number
    pattern = numeric_pattern,
    return = "paragraph",
    perl = TRUE
  )

  # get full paragraph
  table <- expand_text(table, paper, "paragraph")

  guidance <- "Power analyses need to contain the following information to be interpretable: the type of power analysis, the statistical test, the software used, sample size, critical alpha criterion, power level, effect size, and an effect size metric. In addition, it is recommended to make sure the power analysis is reproducible (by sharing the code, or a screenshot, of the power analysis), and to provide good arguments for why the study was designed to detect and effect of this size. For an a-priori power analysis, where the sample size is determined, reporting all information would look like:
> An a priori power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that for a Cohen's d = 0.5, an alpha level of 0.05, and a desired power level of 80% required at least 64 participants in each group.
  For a sensitivity power analysis, this sentence would look like:
> A sensitivity power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that with 64 participants in each group, and an alpha level of 0.05, a desired power level of 80% was reached for an effect size of d = 0.5."

  # Step 4: Classify paragraphs uing LLM
  if (nrow(table) > 0 && llm_use()) {
    power_query <- r"(
An a priori power analysis is used to calculate the required sample size to achieve a desired level of statistical power given an effect size, statistical test, and alpha level.
A sensitivity analysis is used to estimate, given a sample size, which effect sizes a design has sufficient power (e.g., 80% or 90%) to detect, given a statistical test and alpha level.
A post hoc power analysis (also referred to as observed power, or retrospective power) uses an empirically observed effect size, and computes the achieved power for that empirically observed effect size, given a statistical test and alpha level. Note that post-hoc power is [rarely useful to report](https://lakens.github.io/statistical_inferences/08-samplesizejustification.html#sec-posthocpower).

If the paragraph DOES describe one or more power analyses, extract ONLY the following information and return it as JSON, use this exact schema:

Here is an example of input text:

An a priori power analysis was conducted to estimate the sample size required to achieve 80% power to detect a Cohen's d of 0.2 using an unpaired t-test at an alpha level of 0.05. This required a total sample size of 300 participants. A second a priori power analysis was conducted to estimate the required sample size for a secondary outcome. To achieve 80% power to detect a Cohen's f of 0.1 using a one-way ANOVA, a sample size of 350 was required. The a priori power analyses were conducted with G*Power.

Here is an example of the returned output:

[
 {
  "type_of_power_analysis": "apriori",
  "statistical_test": "unpaired t-test",
  "sample_size": 300,
  "alpha_level": 0.05,
  "power": 0.8,
  "effect_size": 0.2,
  "effect_size_metric": "Cohen's d",
  "software": "G*Power"
 },
 {
  "type_of_power_analysis": "apriori",
  "statistical_test": "one-way ANOVA",
  "sample_size": 350,
  "alpha_level": null,
  "power": 0.8,
  "effect_size": 0.1,
  "effect_size_metric": "Cohen's f",
  "software": "G*Power"
 }
]

Rules:
- If one or more power analyes, return an array of JSON objects.
- If there are no power analyses reported, return:

[
 {
  "type_of_power_analysis": "none",
  "statistical_test": "none",
  "sample_size": "none",
  "alpha_level": "none",
  "power": "none",
  "effect_size": "none",
  "effect_size_metric": "none",
  "software": "none"
 }
]

- Do NOT guess values, if any information is missing or unclear, return null (unquoted).
- Use only the exact labels listed below for "type_of_power_analysis", "statistical_test", "efect_size_metric" and "software". All labels are case-sensitive.
- For "type_of_power_analysis": Use ONLY these exact strings:
  - "apriori"
  - "sensitivity"
  - "posthoc"
- Do NOT classify "type_of_power_analysis" as a priori if the text only reports achieved power for an existing sample size.
- For "statistical_test": Use ONLY these exact strings. Choose the closest match or "null" if unclear/unsupported. Ignore one-sided vs. two-sided. If ANOVA is used, specify one-way, two-way, or three-way.
  - "paired t-test"
  - "unpaired t-test"
  - "one-sample t-test"
  - "one-way ANOVA"
  - "two-way ANOVA"
  - "MANOVA"
  - "regression"
  - "chi-square"
  - "correlation"
  - "other"
- For "effects_size_metric": Use ONLY these exact strings. Use "unstandardised" for raw/non-standardized effects (e.g., means, proportions).
  - "Cohen's d"
  - "Hedges' g"
  - "Cohen's f"
  - "partial eta squared"
  - "eta squared"
  - "unstandardised"
- For "software": Use ONLY these exact strings.
  - "G*Power"
  - "Superpower"
  - "Pangea"
  - "Morepower"
  - "PASS"
  - "pwr"
  - "simr"
  - "PowerUpR"
  - "simulation"
  - "InteractionPoweR"
  - "pwrss"
  - "other"
)"

    llm_power <- table |>
      llm(power_query, text_col = "expanded") |>
      json_expand() |>
      tidyr::separate_longer_delim(type_of_power_analysis:software, delim = "; ") |>
      dplyr::rowwise() |>
      dplyr::mutate(complete = dplyr::c_across(type_of_power_analysis:software) |> is.na() |> sum() == 0) |>
      dplyr::ungroup()

    table <- llm_power[llm_power$type_of_power_analysis != "none", ]

    # summary output for paperlists ----
    summary_table <- table |>
      dplyr::summarise(power.n = dplyr::n(),
                       power.complete = sum(complete),
                       dplyr::across(type_of_power_analysis:software,
                                     \(x) sum(!is.na(x)),
                                     .names = "power.{.col}"),
                       .by = id)

    if(all(summary_table$power.complete == summary_table$power.n)){
      tl <- "green"
      report_text <- "You included a power analysis, and all essential information could be detected."
      summary_text <- "You included a power analysis, and all essential information could be detected."
    } else {
      tl <- "red"
      has_na <- sapply(
        table[, c("type_of_power_analysis", "statistical_test", "sample_size", "alpha_level", "power", "effect_size", "effect_size_metric", "software"), drop = FALSE],
        function(x) any(is.na(x))
      )
      cols_with_na <- names(has_na)[has_na]
      summary_text <- "You included a power analysis, but some essential information could not be detected."
      report_text <- sprintf("You included a power analysis, but some essential information could not be detected. The following sentences were found: %s \n\nThe following information seems to be missing: %s",
                             paste(table$text, collapse = ", "),
                             paste(cols_with_na, collapse = ", "))
    }

    observed_power_text <- ifelse(table$type_of_power_analysis == "posthoc", "You reported a power analysis that has been classified as 'post-hoc'. Calculating observed power is [almost never useful](https://lakens.github.io/statistical_inferences/08-samplesizejustification.html#sec-posthocpower). If you actually performed a sensitivity power analysis, label it as such explicitly. \n\n", "")

    report <- c(report_text,
                observed_power_text,
                scroll_table(table[, c("type_of_power_analysis", "statistical_test", "sample_size", "alpha_level", "power", "effect_size", "effect_size_metric", "software"), drop = FALSE], colwidths = c(50,10,10,10,10,10,10,10,10)),
                collapse_section(guidance)) |>
      paste(collapse = "\n\n")


  } else if (nrow(table) > 0) {
    # If not using LMM
    table <- dplyr::mutate(
      table,
      type_of_power_analysis = dplyr::case_when(
        stringr::str_detect(tolower(text), "a[- ]?priori") ~ "apriori",
        stringr::str_detect(tolower(text), "sensitivity") ~ "sensitivity",
        stringr::str_detect(tolower(text), "compromise power") ~ "compromise",
        stringr::str_detect(tolower(text), "a[- ]?posteriori|post[- ]?hoc|retrospective") ~ "posthoc",
        TRUE ~ "unknown"
      )
    )

    summary_table <- table |>
      dplyr::summarise(power.n = dplyr::n(),
                       power.complete = NA_integer_,
                       .by = id)

    report_text <- sprintf("You included a power analysis, but did not use an LLM to assess if all information was reported. Check the power analysis manually: %s",
                           paste(table$text, collapse = ", "))

    observed_power_text <- ifelse(table$type_of_power_analysis == "posthoc", "You reported a power analysis that has been classified as 'post-hoc'. Calculating observed power is [almost never useful](https://lakens.github.io/statistical_inferences/08-samplesizejustification.html#sec-posthocpower). If you actually performed a sensitivity power analysis, label it as such explicitly.\n\n", "")
    summary_text <- "You included a power analysis. Check if all required information is reported."

    tl <- "yellow"

    report <- c(report_text,
                observed_power_text,
                scroll_table(table[,c("text", "type_of_power_analysis")]),
                collapse_section(guidance)) |>
      paste(collapse = "\n\n")

  }
  # Need this catch all, also for when LLM classifies sentences as 'none'.
  if (nrow(table) == 0 ||
      (exists("llm_power", inherits = TRUE) &&
       isTRUE(all(llm_power$type_of_power_analysis == "none", na.rm = TRUE)))
  ) {
    # no power detected
    tl <- "na"
    summary_text <- "No power analyses were detected."
    report_text <- "No power analyses were detected."
    report <- c(summary_text,
                collapse_section(guidance)) |>
      paste(collapse = "\n\n")

    summary_table <- table |>
      dplyr::summarise(power.n = dplyr::n(),
                       power.complete = NA_integer_,
                       .by = id)
  }

  # return a list ----
  list(
    table = table,
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}
