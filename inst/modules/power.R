#' Power Analysis Check
#'
#' @description
#' This module uses a large language module (LLM) to extract information reported in power analyses, including the statistical test, sample size, alpha level, desired level of power,and magnitude and type of effect size.
#'
#' If you have not set llm_use(TRUE) and supplied a groq API, the module will return paragraphs that potentially contain power analyses, based on a regular expression search.
#'
#' @details
#' The Power Analysis Check module uses regular expressions to identify sentences that contain a statistical power analysis. Without the use of an LMM, the module uses regular expressions to classify the power analysis as a-priori, sensitivity or post-hoc. With the use of an LMM, it checks if the power analysis is reported with all required information.
#'
#' The regular expressions can miss power analyses, or fail to classify them correctly. The type of power analysis is often difficult to classify, which can easily be solved by explicitly specifying the type of power analysis as 'a-priori', 'sensitivity', or 'post-hoc'. Note that 'post-hoc' or 'observed' power is rarely useful. The LMM can fail to identify information in the paper, and will not have access to information in paragraphs in the paper other than those that contain the word 'power'. This package was validated by the Metacheck team on articles in Psychological Science.
#'
#' @keywords method
#'
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#' @author Daniel Lakens (\email{d.lakens@tue.nl})
#' @author Cristian Mesquida (\email{c.mesquida.caldentey@tue.nl})
#'
#' @import dplyr
#' @import stringr
#'
#' @param paper a paper object or paperlist object
#' @param seed a seed for the LLM
#'
#' @returns a list
power <- function(paper, seed = 8675309) {
  # find potential power anlayses ----

  # select paragraphs with power/powers/powered
  # will not match e.g., powerful
  power_pattern <- "\\bpower(ed|s)?\\b"

  # phrases that should also be in the paragraph
  power_words <- c(
    "power analy",
    "effect size",
    "sized? effect",
    "sample[- ]size",
    "g[-* ]?power",
    "a[- ]?priori",
    "a[- ]?posteriori",
    "post[- ]?hoc",
    "sensitivity",
    "pwr",
    "statistical power",
    "to detect",
    "achieve",
    "(small|medium|large) effect",
    "observed power",
    "a power of",
    "%",
    "power\\s*="
  )

  # only keep paragraphs with a number
  # Pattern for digits, but not years
  numeric_pattern <- "\\b(?!\\d{4}\\b)\\d+(?:[.,]\\d+)?\\b"

  # search for paragraphs with "power" and at least one power word
  potential_power <- paper |>
    search_text(power_pattern, return = "paragraph") |>
    search_text(power_words, return = "paragraph") |>
    search_text(numeric_pattern, return = "paragraph", perl = TRUE)


  # classify paragraphs ----

  # columns to be defined by LLM
  llm_cols <- c(
    "power_type", # also defined by regex
    "statistical_test",
    "sample_size",
    "alpha_level",
    "power",
    "effect_size",
    "effect_size_metric",
    "software"
  )

  if (nrow(potential_power) > 0 && llm_use()) {
    ## use LLM ----

    # power_query defined below function
    llm_results <- llm(potential_power,
                       power_query,
                       text_col = "text",
                       seed = seed)

    table <- llm_results |>
      json_expand() |>
      dplyr::rowwise() |>
      dplyr::mutate(complete = !any(dplyr::across(dplyr::any_of(llm_cols), is.na))) |>
      dplyr::ungroup() |>
      dplyr::filter(power_type != "none")

    # check for NAs in LLM columns
    has_na <- dplyr::select(table, dplyr::any_of(llm_cols)) |>
      is.na() |> apply(2, any)

    if (nrow(table) == 0) {
      # do nothing -- handle later
    } else if (!any(has_na)) {
      # LLM found only complete power analyses
      tl <- "green"
      report_text <- "All essential information could be detected."
    } else {
      # LLM found incomplete power analyses
      tl <- "red"

      cols_with_na <- names(has_na)[has_na]

      report_text <- sprintf(
        "Some essential information could not be detected: %s",
        paste(cols_with_na, collapse = ", ")
      )
    }
  } else if (nrow(potential_power) > 0) {
    ## use regex ----

    table <- dplyr::mutate(
      potential_power,
      power_type = dplyr::case_when(
        stringr::str_detect(tolower(text), "a[- ]?priori") ~ "apriori",
        stringr::str_detect(tolower(text), "sensitivity") ~ "sensitivity",
        stringr::str_detect(tolower(text), "compromise power") ~ "compromise",
        stringr::str_detect(
          tolower(text),
          "a[- ]?posteriori|post[- ]?hoc|retrospective"
        ) ~ "posthoc",
        TRUE ~ "unknown"
      ),
      complete = NA
    )

    tl <- "yellow"

    report_text <- "You chose to not use an LLM to assess if all information was reported, so please check for all required information manually."
  } else {
    table <- potential_power
  }

  # generate report ----

  # general guidance to insert in report later
  guidance <- c(
    "Power analyses need to contain the following information to be interpretable: the type of power analysis, the statistical test, the software used, sample size, critical alpha criterion, power level, effect size, and an effect size metric. In addition, it is recommended to make sure the power analysis is reproducible (by sharing the code, or a screenshot, of the power analysis), and to provide good arguments for why the study was designed to detect an effect of this size.",
    "For an a-priori power analysis, where the sample size is determined, reporting all information would look like:",
    "> An a priori power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that for a Cohen's d = 0.5, an alpha level of 0.05, and a desired power level of 80% required at least 64 participants in each group.",
    "For a sensitivity power analysis, this sentence would look like:",
    "> A sensitivity power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that with 64 participants in each group, and an alpha level of 0.05, a desired power level of 80% was reached for an effect size of d = 0.5."
  )

  if (nrow(table) == 0) {
    ## no power detected ----
    tl <- "na"
    summary_text <- "No power analyses were detected."
    report <- c(summary_text, collapse_section(guidance))

    summary_table <- data.frame(id = NA_character_,
                                power_n = NA_integer_,
                                power_complete = NA_integer_)
  } else {
    ## power detected ----
    # check for observed power and add text/type
    observed_power_text <- ifelse(
      any(table$power_type == "posthoc"),
      observed_power_text <- "You reported a power analysis that has been classified as 'post-hoc'. Calculating observed power is [almost never useful](https://lakens.github.io/statistical_inferences/08-samplesizejustification.html#sec-posthocpower). If you actually performed a sensitivity power analysis, label it as such explicitly.",
      ""
    )

    # report tables
    table$power_id <- seq_along(table$text)

    info_table <- dplyr::select(table, power_id, dplyr::any_of(llm_cols))
    text_table <- table |>
      dplyr::summarise(
        power_id = paste(power_id, collapse = ";"),
        .by = text) |>
      dplyr::select(power_id, text)

    if (nrow(table) == 1) {
      # power_id not needed for a single power analysis
      info_table$power_id <- NULL
      text_table$power_id <- NULL
    }

    # highlight important terms in text
    highlighted_terms <- c(
      "power",
      "a[- ]?priori",
      "sensitivity",
      "a[- ]?posteriori",
      "post[- ]?hoc",
      "observed power",
      "retrospective power"
    )
    text_table$text <- highlighted_terms |>
      paste(collapse = "|") |>
      paste0("(", x = _, ")") |>
      gsub("<strong>\\1</strong>", text_table$text, ignore.case = TRUE)

    # summary_text ----
    summary_text <- sprintf(
      "We detected %d potential power %s.",
      nrow(table),
      plural(nrow(table), "analysis","analyses")
    )

    # summary_table ----
    summary_table <- dplyr::summarise(
      table,
      power_n = dplyr::n(),
      power_complete = sum(complete),
      # exclude power_type
      dplyr::across(dplyr::any_of(llm_cols[-1]),
                    \(x) sum(!is.na(x)), .names = "power_{.col}"),
      .by = id
    )

    # report ----
    report <- c(
      report_text,
      scroll_table(info_table, maxrows = 5),
      observed_power_text,
      scroll_table(text_table, maxrows = 1),
      collapse_section(guidance)
    )
  }

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = c(power_n = 0),
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}

# power query ----

power_query <- r"(
An a priori power analysis is used to calculate the required sample size to achieve a desired level of statistical power given an effect size, statistical test, and alpha level.
A sensitivity analysis is used to estimate, given a sample size, which effect sizes a design has sufficient power (e.g., 80% or 90%) to detect, given a statistical test and alpha level.
A post hoc power analysis (also referred to as observed power, or retrospective power) uses an empirically observed effect size, and computes the achieved power for that empirically observed effect size, given a statistical test and alpha level. Note that post-hoc power is [rarely useful to report](https://lakens.github.io/statistical_inferences/08-samplesizejustification.html#sec-posthocpower).

If the paragraph DOES describe one or more power analyses, extract ONLY the following information and return it as JSON, use this exact schema:

Here is an example of input text:

An a priori power analysis was conducted to estimate the sample size required to achieve 80% power to detect a Cohen's d of 0.2 using an unpaired t-test at an alpha level of 0.05. This required a total sample size of 300 participants. A second a priori power analysis was conducted to estimate the required sample size for a secondary outcome. To achieve 80% power to detect a Cohen's f of 0.1 using a one-way ANOVA, a sample size of 350 was required. The a priori power analyses were conducted with G*Power.

Here is an example of the returned output:

[ {
  "power_type": "apriori",
  "statistical_test": "unpaired t-test",
  "sample_size": 300,
  "alpha_level": 0.05,
  "power": 0.8,
  "effect_size": 0.2,
  "effect_size_metric": "Cohen's d",
  "software": "G*Power"
 },
 {
  "power_type": "apriori",
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

[{"power_type": "none"}]

- Do NOT guess values, if any information is missing or unclear, return null (unquoted).
- Use only the exact labels listed below for "power_type", "statistical_test", "efect_size_metric" and "software". All labels are case-sensitive.
- For "power_type": Use ONLY these exact strings:
  - "apriori"
  - "sensitivity"
  - "posthoc"
- Do NOT classify "power_type" as apriori if the text only reports achieved power for an existing sample size.
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
