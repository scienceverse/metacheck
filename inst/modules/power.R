#' Power Analysis Check
#'
#' @description
#' This module uses uses regular expressions to identify sentences that contain a statistical power analysis. If specified by the user, it also uses a large language module (LLM) to extract information reported in power analyses, including the statistical test, sample size, alpha level, desired level of power, and magnitude and type of effect size.
#'
#' @details
#' The Power Analysis Check module uses regular expressions to identify sentences that contain a statistical power analysis. Without the use of an LMM, the module uses regular expressions to classify the power analysis as a-priori, sensitivity or post-hoc. With the use of an LMM, it checks if the power analysis is reported with all required information.
#'
#' The regular expressions can miss power analyses, or fail to classify them correctly. The type of power analysis is often difficult to classify, which can easily be solved by explicitly specifying the type of power analysis as 'a-priori', 'sensitivity', or 'post-hoc'. Note that 'post-hoc' or 'observed' power is rarely useful. The LMM can fail to identify information in the paper, and will not have access to information in paragraphs in the paper other than those that contain the word 'power'. This package was validated by the Metacheck team on articles in Psychological Science.
#'
#' <validation>In a sample of 128 papers with 246 instances of power statements, 203 were correctly detected (true positives), 22 were missed (false negatives) and 21 were incorrectly detected (false positives). Overall, among all instances flagged as power statements, 90.6% were correct (positive predictive value).</validation>
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
    text_search(power_pattern, return = "paragraph") |>
    text_search(power_words, return = "paragraph") |>
    text_search(numeric_pattern, return = "paragraph", perl = TRUE)


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

  llm_failed <- FALSE
  if (nrow(potential_power) > 0 && llm_use()) {
    ## use LLM ----

    extraction <- .power_llm_extract(potential_power, seed)
    llm_model_used <- extraction$model
    table <- extraction$table
    llm_failed <- isTRUE(extraction$failed)

    # set up report text
    report_text <- sprintf("We used the LLM model '%s' to check the contents of %d paragraph%s that contained words suggesting they might contain power analyses.",
                           llm_model_used,
                           nrow(potential_power),
                           plural(nrow(potential_power)))
    if (!extraction$structured) {
      report_text <- c(report_text,
        "(The provider does not support structured outputs for this schema; used prompt-based extraction instead.)")
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
      report_text <- c(report_text, "All essential information could be detected.")
    } else {
      # LLM found incomplete power analyses
      tl <- "red"

      cols_with_na <- names(has_na)[has_na]

      report_text <- c(
        report_text,
        sprintf(
          "Some essential information could not be detected: %s",
          paste(cols_with_na, collapse = ", ")
        )
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
    summary_text <- if (llm_failed) {
      "The LLM check for power analyses failed to run; no results could be extracted. This is not the same as finding no power analyses -- please check manually or re-run the check."
    } else {
      "No power analyses were detected."
    }
    report <- c(summary_text, collapse_section(guidance))

    summary_table <- data.frame(paper_id = paper_id(paper))
    summary_table$power_n <- 0
    summary_table$power_complete <- NA_integer_
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
      .by = paper_id
    )

    # report ----
    report <- c(
      report_text,
      scroll_table(info_table, maxrows = 5),
      observed_power_text,
      scroll_table(text_table, maxrows = 5),
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

# LLM extraction ----

# ellmer type spec for structured extraction, built by hand rather than via
# ellmer::type_from_schema(path = ".../power_array.json"): that shipped schema
# (like power.json below) uses union types ("type": ["string", "null"]) and
# null-inclusive enums, which some providers' structured-output validators
# reject outright (confirmed: Groq's openai/gpt-oss-20b returns HTTP 400 on
# power_array.json, Mistral accepts it -- see issue #323). ellmer's own
# mechanism for an optional field is `required = FALSE` (a plain type, no
# union, dropped from the JSON Schema `required` list) with no `null` in the
# enum's allowed values -- the exact fix issue #323 itself proposes ("replace
# [string,null] unions with a single type + required=FALSE, drop null from
# enums"). Building it this way (matching codebook_check.R's
# type_object()/type_array() pattern) means structured mode works on strict
# validators too, not just lenient ones, and the prompt-fence fallback below
# is only needed for a provider that rejects structured output outright
# (not this specific schema shape).
#
# Wrapped in a single-field object (power_analyses: array), not a bare
# top-level array: codebook_check.R's own type spec notes Groq's gpt-oss-20b
# 400s on a bare top-level array schema. llm()'s .unnest_result() unwraps a
# single-field wrapper object back into one row per array item automatically.
.power_type_spec <- function() {
  ellmer::type_object(
    power_analyses = ellmer::type_array(
      description = "Power analyses found in the text. Empty array if none.",
      ellmer::type_object(
        power_type = ellmer::type_enum(
          c("apriori", "sensitivity", "posthoc", "unknown"),
          description = "The type of power analysis. 'apriori' calculates the required sample size to achieve a desired power given an effect size, statistical test, and alpha level. 'sensitivity' estimates, given a sample size, which effect sizes a design has sufficient power to detect. 'posthoc' (observed/retrospective power) computes achieved power for an empirically observed effect size. Use 'unknown' if a power analysis is present but its type cannot be determined."
        ),
        statistical_test = ellmer::type_enum(
          c("paired t-test", "unpaired t-test", "one-sample t-test",
            "1-way ANOVA", "2-way ANOVA", "3-way ANOVA", "MANOVA",
            "regression", "chi-square", "correlation", "other"),
          description = "The statistical test used.", required = FALSE
        ),
        statistical_test_other = ellmer::type_string(
          "Free-text description if statistical_test is 'other'.",
          required = FALSE
        ),
        sample_size = ellmer::type_number(
          "The sample size determined by or used in the power analysis. Give the total number if this is expressed as number per group.",
          required = FALSE
        ),
        alpha_level = ellmer::type_number(
          "The alpha threshold used to determine significance.",
          required = FALSE
        ),
        power = ellmer::type_number(
          "The statistical power, expressed as a number between 0 and 1.",
          required = FALSE
        ),
        effect_size = ellmer::type_number(
          "The numeric effect size used in or determined from the power analysis.",
          required = FALSE
        ),
        effect_size_metric = ellmer::type_enum(
          c("Cohen's d", "Hedges' g", "Cohen's f", "partial eta squared",
            "eta squared", "unstandardised", "other"),
          description = "The effect size metric. Use 'unstandardised' for raw/non-standardized effects.",
          required = FALSE
        ),
        effect_size_metric_other = ellmer::type_string(
          "Free-text description if effect_size_metric is 'other'.",
          required = FALSE
        ),
        software = ellmer::type_enum(
          c("G*Power", "Superpower", "Pangea", "Morepower", "PASS", "pwr",
            "simr", "PowerUpR", "simulation", "InteractionPoweR", "pwrss",
            "other"),
          description = "The software used to conduct the power analysis.",
          required = FALSE
        )
      )
    )
  )
}

# Extract power-analysis fields via LLM, preferring provider-enforced
# structured output (the provider constrains generation to the type spec
# above) with automatic fallback to the original prompt-instructed +
# json_expand() approach when a provider rejects structured output outright
# (see issue #323). Returns list(table, model, structured): `table` always has
# one row per detected power analysis (rows with no power analysis found are
# already dropped, and `power_type` is never "none"/NA), with `complete`
# marking whether every llm_cols field was extracted.
.power_llm_extract <- function(potential_power, seed) {
  llm_cols <- c(
    "power_type", "statistical_test", "sample_size", "alpha_level",
    "power", "effect_size", "effect_size_metric", "software"
  )

  structured_prompt <- "Identify power analyses from excerpts of scientific manuscripts. Use null/omit a field when information is missing, do not invent values. Only use 'other' if a value not in the enumerated options can be identified. A paragraph may contain no power analysis, or more than one -- return one entry in power_analyses per power analysis actually described, and an empty array if the paragraph only references a power analysis presented elsewhere, or explicitly states that no power analysis was run."

  structured_result <- tryCatch(
    llm(
      text = potential_power,
      system_prompt = structured_prompt,
      type = .power_type_spec(),
      text_col = "text",
      model = llm_model(),
      params = list(seed = seed)
    ),
    error = function(e) NULL
  )
  # ellmer's chat_structured() returns the array field as an already-built
  # data frame/tibble, not a plain nested list -- confirmed live against
  # Groq, not just via a hand-built mock. .unnest_result()'s "unwrap a
  # single-field array" fast path only fires for is.list(inner) &&
  # !is.data.frame(inner), so a data-frame inner value falls through to
  # as.data.frame(result) instead, which R flattens into dotted
  # "power_analyses.field" names rather than unprefixed ones. Strip that
  # prefix the same way codebook_check.R does for its own wrapped arrays.
  structured_result <- .strip_llm_wrapper(structured_result, "power_analyses")

  # A systemic rejection (e.g. Groq 400ing the schema, or the call erroring
  # outright) means every row failed, not just an isolated flaky one -- llm()
  # already retries transient structured-JSON failures internally (see
  # .llm_json_retryable()), so a failure that survives that retry and hits
  # every row is the provider rejecting structured output for this call, not
  # noise. Anything less than "every row" is left as-is: rows come back with
  # power_type NA the same as a genuine empty array, which the caller already
  # treats as "no power analysis found" -- no different from a model that
  # legitimately found nothing there.
  all_failed <- is.null(structured_result) ||
    (".error" %in% names(structured_result) &&
       all(vapply(structured_result$.error, isTRUE, logical(1))))

  if (!all_failed) {
    table <- structured_result
    # ellmer::type_enum() fields come back as factors (confirmed live), which
    # would otherwise leak factor internals (structure(1L, levels = ...))
    # into the report's embedded table and could break string comparisons
    # anywhere downstream that assumes character -- coerce to plain character
    # to match the fallback path's json_expand()-produced columns exactly.
    enum_cols <- c("power_type", "statistical_test", "effect_size_metric", "software")
    for (col in intersect(enum_cols, names(table))) {
      table[[col]] <- as.character(table[[col]])
    }
    # Drop llm()'s own per-row error-tracking columns: a row that failed
    # structured extraction (but not every row -- see all_failed above) has
    # power_type NA and is dropped just below like any other empty result, so
    # .error/.error_msg carry no information a caller needs from here on.
    table$.error <- NULL
    table$.error_msg <- NULL
    # `text` collides between the input paragraph column and the schema's
    # extracted "text" field (dropped from the type spec above for exactly
    # this reason -- llm() would otherwise suffix one of them ".extracted");
    # `power_analyses` rows with nothing found come back as power_type = NA,
    # the structured equivalent of the fallback path's power_type == "none".
    if ("power_type" %in% names(table)) {
      table <- dplyr::filter(table, !is.na(power_type))
    } else {
      # No input row's power_analyses ever produced ANY object (every call
      # returned an empty array), so llm()'s join never added a power_type
      # column at all -- table is still one stub row per input paragraph.
      # Drop them all rather than assigning a 0-length column onto >0 rows.
      table <- table[0, , drop = FALSE]
      table$power_type <- character(0)
    }
    # A provider may omit an optional (required = FALSE) field's key entirely
    # rather than emitting it as null -- ellmer's schema only lists required
    # fields, so this is a legal response, not a malformed one. If EVERY
    # returned power_analyses object across every call omits the same key
    # (e.g. no paragraph ever got an alpha_level filled in), that column never
    # appears in `table` at all. dplyr::any_of() below silently skips a column
    # that does not exist rather than counting it as unextracted, which would
    # make `complete` wrongly TRUE. Guarantee every llm_cols column exists
    # (NA where absent) before computing `complete`.
    for (col in setdiff(llm_cols, names(table))) table[[col]] <- rep(NA, nrow(table))
    table <- table |>
      dplyr::rowwise() |>
      dplyr::mutate(complete = !any(dplyr::across(dplyr::all_of(llm_cols), is.na))) |>
      dplyr::ungroup()

    return(list(
      table = table,
      model = attr(structured_result, "llm")$model,
      structured = TRUE,
      failed = FALSE # reaching here means at least one row succeeded
    ))
  }

  ## fallback: prompt-instructed JSON + json_expand ----
  preface <- "Identify and classify power analyses from exerpts of scientific manuscripts. Use null when information is missing, do not invent values. Only use 'other' if a value not in the enumerated options can be identified. There may be no power analysis in the text, or more than one. If the paragraph only references a power analysis implied to be presented elsewhere in the paper, or explicitly states that no power analysis was run, classify power_type as 'none'. Return an array of objects, as defined by the JSON schema below, in the same order as in the paragraphs, bracketed by ```json and ```."
  schema_text <- readLines("https://scienceverse.org/schema/power.json") |>
    paste(collapse = "\n")
  system_prompt <- paste(preface, schema_text, sep = "\n\n")

  llm_results <- llm(
    text = potential_power,
    system_prompt = system_prompt,
    text_col = "text",
    model = llm_model(),
    params = list(seed = seed)
  )

  table <- llm_results |>
    json_expand(suffix = c("", ".power")) |>
    dplyr::rowwise() |>
    dplyr::mutate(complete = !any(dplyr::across(dplyr::any_of(llm_cols), is.na))) |>
    dplyr::ungroup()

  # A row whose call errored outright (llm() sets answer = NA) or whose
  # answer json_expand() could not parse gets its own error column set (e.g.
  # "parsing error") -- that is neither "found nothing" (power_type == "none")
  # nor a real extraction, so drop it the same way, instead of leaving a
  # phantom power_type == "unknown" row nothing downstream ever filters out.
  row_failed <- if ("error" %in% names(table)) !is.na(table$error) else FALSE

  if ("power_type" %in% names(table)) {
    table <- dplyr::filter(table, power_type != "none" & !row_failed)
  } else {
    table <- table[0, , drop = FALSE]
  }

  list(
    table = table,
    model = attr(llm_results, "llm")$model,
    structured = FALSE,
    # every row of this call failed (not just "nothing found") -- used by the
    # caller to tell a real check failure apart from a genuine negative result
    failed = nrow(potential_power) > 0 && nrow(table) == 0 && all(row_failed)
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
      "enum": ["apriori", "sensitivity", "posthoc", "unknown", "none", null]
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
