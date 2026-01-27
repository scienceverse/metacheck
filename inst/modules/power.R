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

    # define system prompt from JSON schema
    preface <- "Identify and classify power analyses from exerpts of scientific manuscripts. Use null when information is missing, do not invent values. Only use 'other' if a value not in the enumerated options can be identified. There may be no power analysis in the text, or more than one. Return an array of objects as defined by the JSON schema below, bracketed by ```json and ```."
    # schema also defined below
    schema <- readLines("https://scienceverse.org/schema/power.json") |>
      paste(collapse = "\n")
    system_prompt <- paste(preface, schema, sep = "\n\n")

    llm_results <- llm(
      text = potential_power,
      system_prompt = system_prompt,
      text_col = "text",
      params = list(seed = seed)
    )

    table <- llm_results |>
      json_expand(suffix = c("", ".power")) |>
      dplyr::rowwise() |>
      dplyr::mutate(complete = !any(dplyr::across(dplyr::any_of(llm_cols), is.na))) |>
      dplyr::ungroup()

    if ("power_type" %in% names(table)) {
      table <- dplyr::filter(table, power_type != "none")
    }

    # check for NAs in LLM columns
    has_na <- dplyr::select(table, dplyr::any_of(llm_cols)) |>
      is.na() |>
      apply(2, any)

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

    summary_table <- data.frame(
      id = NA_character_,
      power_n = NA_integer_,
      power_complete = NA_integer_
    )
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
        .by = text
      ) |>
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
      plural(nrow(table), "analysis", "analyses")
    )

    # summary_table ----
    summary_table <- dplyr::summarise(
      table,
      power_n = dplyr::n(),
      power_complete = sum(complete),
      # exclude power_type
      dplyr::across(dplyr::any_of(llm_cols[-1]),
        \(x) sum(!is.na(x)),
        .names = "power_{.col}"
      ),
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

# schema ----

schema <- r"({
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://scienceverse.org/schema/power.json",
  "title": "Power Analyses",
  "description": "A power analysis.",
  "type": "object",
  "properties": {
    "text": {
      "description": "The specific text that contains all of the information used to determine this object's properties.",
      "type": ["string", "null"]
    },

    "power_type": {
      "description": "The type of power analysis. An 'apriori' power analysis is used to calculate the required sample size to achieve a desired level of statistical power given an effect size, statistical test, and alpha level. A 'sensitivity' analysis is used to estimate, given a sample size, which effect sizes a design has sufficient power (e.g., 80% or 90%) to detect, given a statistical test and alpha level. A 'posthoc' power analysis (also referred to as observed power, or retrospective power) uses an empirically observed effect size, and computes the achieved power for that empirically observed effect size, given a statistical test and alpha level.",
      "type": ["string", "null"],
      "enum": ["apriori", "sensitivity", "posthoc", null]
    },

    "statistical_test": {
      "description": "The statistical test used. Use null if unclear.",
      "type": ["string", "null"],
      "enum": [
        "paired t-test",
        "unpaired t-test",
        "one-sample t-test",
        "1-way ANOVA",
        "2-way ANOVA",
        "3-way ANOVA",
        "MANOVA",
        "regression",
        "chi-square",
        "correlation",
        "other",
        null
      ]
    },

    "statistical_test_other": {
      "description": "Free-text description if statistical_test is 'other', otherwise null.",
      "type": ["string", "null"]
    },

    "sample_size": {
      "description": "The sample size determined by or used in the power analysis. Give the total number if this is expressed as number per group.",
      "type": ["number", "null"],
      "minimum": 0
    },

    "alpha_level": {
      "description": "The alpha threshold used to determine significance.",
      "type": ["number", "null"],
      "exclusiveMinimum": 0,
      "maximum": 1
    },

    "power": {
      "description" : "The statistical power, expressed as a number between 0 and 1.",
      "type": ["number", "null"],
      "minimum": 0,
      "maximum": 1
    },

    "effect_size": {
      "description": "The numeric effect size used in or determined from the power analysis.",
      "type": ["number", "null"]
    },

    "effect_size_metric": {
      "description": "The effect size metric. Use 'unstandardised' for raw/non-standardized effects.",
      "type": ["string", "null"],
      "enum": [
        "Cohen's d",
        "Hedges' g",
        "Cohen's f",
        "partial eta squared",
        "eta squared",
        "unstandardised",
        "other",
        null
      ]
    },

    "effect_size_metric_other": {
      "description": "Free-text description if effect_size_metric is 'other', otherwise null.",
      "type": ["string", "null"]
    },

    "software": {
      "description": "The software used to conduct the power analysis.",
      "type": ["string", "null"],
      "enum": [
        "G*Power",
        "Superpower",
        "Pangea",
        "Morepower",
        "PASS",
        "pwr",
        "simr",
        "PowerUpR",
        "simulation",
        "InteractionPoweR",
        "pwrss",
        "other",
        null
      ]
    }
  },

  "required": [
    "power_type",
    "statistical_test",
    "statistical_test_other",
    "sample_size",
    "alpha_level",
    "power",
    "effect_size",
    "effect_size_metric",
    "effect_size_metric_other",
    "software"
  ],

  "additionalProperties": false
})"
