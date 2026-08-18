#' Preregistration Check
#'
#' @description
#' Retrieve information from preregistrations in a standardised way,
#' and make them easier to check.
#'
#' @details
#' The Preregistration Check module identifies preregistrations on the OSF and AsPredicted based on links in the manuscript, retrieves the preregistration text, and organizes the information into a template. The module then uses regular expressions to identify text from AsPredicted, and the API to retrieve text from the OSF. The information in the preregistration is returned.
#'
#' The module can’t extract information from non-structured preregistration templates (i.e., where the preregistration is uploaded in a single text field) and it can’t retrieve information in preregistrations that are stored as text documents on the OSF.
#'
#' If you want to extend the package to be able to download information from other preregistration sites, reach out to the Metacheck development team.

#'
#' @keywords method
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
prereg_check <- function(paper) {
  # paper <- psychsci[[218]] # to test
  # and paper <- xml[["09567976251396084"]] for multiple aspredicted
  # osf: paper <- xml[["09567976221114055"]]
  # osf: paper <- psychsci[31:40]

  # table ----
  links_ap <- aspredicted_links(paper)
  links_osf <- osf_links(paper)

  ## no links ----
  if (nrow(links_ap) == 0 && nrow(links_osf) == 0) {
    resp <- list(
      traffic_light = "na",
      summary_text = "No preregistration links were found.",
      summary_table = data.frame(
        paper_id = paper_id(paper),
        preregistration = 0
      )
    )
    return(resp)
  }

  ## AsPredicted preregs ----
  table_ap <- suppressMessages(
    aspredicted_info(links_ap$href)
  )
  ap_schema_table <- ap_schema(table_ap)

  ## OSF prereg ----
  osf_ids <- links_osf$href |>
    osf_check_id() |>
    unique()
  link_types <- osf_type(osf_ids)
  reg_ids <- osf_ids[link_types == "registrations" & !is.na(link_types)]

  ## no registrations ----
  if (length(reg_ids) == 0 & nrow(table_ap) == 0) {
    resp <- list(
      traffic_light = "na",
      summary_text = sprintf(
        "We found %d OSF link%s, but no registrations.",
        nrow(links_osf), nrow(links_osf) |> plural()
      ),
      na_replace = 0,
      summary_table = data.frame(
        paper_id = paper_id(paper),
        preregistration = 0
      )
    )
    return(resp)
  }

  ## get reg info from OSF ----
  urls <- sprintf(
    "https://api.osf.io/v2/registrations/%s",
    reg_ids #paste(reg_ids, collapse = ",")
  )

  # have to iterate, process then merge
  # because pagination > 10 usually returns unmergeable dfs
  ps <- lapply(urls, \(url) {
    reg_info <- osf_get_all_pages(url)

    if (length(reg_info) == 0) return(NULL)

    info <- reg_info
    osf_prereg_extract(info)
  })

  # make sure all items are not lists
  prereg_schemas <- c(ps, list(ap_schema_table)) |>
    lapply(\(x) lapply(x, paste, collapse = "\n\n"))
  prereg_info <- do.call(dplyr::bind_rows, prereg_schemas)

  if (nrow(prereg_info)) {
    paper_ids <- data.frame(
      paper_id = c(links_ap$paper_id, links_osf$paper_id),
      link = c(links_ap$href, links_osf$href)
    )
    paper_ids$link <- gsub("^(https://)?", "https://", paper_ids$link)

    prereg_info <- dplyr::left_join(prereg_info, paper_ids, by = "link")
  }

  # traffic light ----
  tl <- "info"

  # summary_text ----
  summary_text <- sprintf(
    "We found %d preregistration%s.",
    nrow(prereg_info), nrow(prereg_info) |> plural()
  )

  # report ----
  has_sample_size <- "sample_size" %in% names(prereg_info)
  report_text <- sprintf(
    "Meta-scientific research has shown that deviations from preregistrations are often not reported or checked, and that the most common deviations concern the sample size. We recommend manually checking the full preregistration at the link%s above%s.",
    nrow(prereg_info) |> plural(),
    ifelse(has_sample_size, ", and have provided the preregistered sample size", "")
  )

  prereg_link_table <- data.frame(
    id = link(prereg_info$link, prereg_info$id),
    title = prereg_info$title,
    template = prereg_info$template_name
  )

  if (has_sample_size) {
    samplesize_table <- prereg_info[, c("id", "sample_size")]
  } else {
    samplesize_table <- NULL
  }

  ## summary output for paperlists ----
  summary_table <- dplyr::count(prereg_info, paper_id,
    name = "preregistration",
    .drop = FALSE
  )

  ## prereg table ----
  # Remove columns where all values are NA
  prereg_table <- prereg_info[
    , colSums(!is.na(prereg_info)) > 0
  ] |>
    t() |>
    as.data.frame()

  # Add row names as a proper column (first column)
  prereg_table <- cbind(Field = rownames(prereg_table), prereg_table)

  # Rename columns "Preregistration 1", "Preregistration 2", ...
  n_prereg <- ncol(prereg_table) - 1 # subtract the 'Field' column
  colnames(prereg_table)[-1] <- paste0("Preregistration ", seq_len(n_prereg))

  ## guidance ----
  guidance <- c(
    "For metascientific articles demonstrating the rate of deviations from preregistrations, see:",
    format_ref(vandenAkker2024),
    "For educational material on how to report deviations from preregistrations, see:",
    format_ref(Lakens2024)
  )

  report <- c(
    summary_text,
    scroll_table(prereg_link_table),
    report_text,
    scroll_table(samplesize_table),
    collapse_section(
      scroll_table(prereg_table, maxrows = 5),
      "Full Preregistration"
    ),
    collapse_section(guidance)
  )

  # return a list ----
  list(
    table = prereg_info,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}

# references

vandenAkker2024 <- bibentry(
  bibtype = "Article",
  title = "The potential of preregistration in psychology: Assessing preregistration producibility and preregistration-study consistency",
  author = c(
    person("O. R.", "van den Akker"),
    person("M.", "Bakker"),
    person("M. A. L. M.", "van Assen"),
    person("C. R.", "Pennington"),
    person("L.", "Verweij"),
    person("M. M.", "Elsherif"),
    person("A.", "Claesen"),
    person("S. D. M.", "Gaillard"),
    person("S. K.", "Yeung"),
    person("J.-L.", "Frankenberger"),
    person("K.", "Krautter"),
    person("J. P.", "Cockcroft"),
    person("K. S.", "Kreuer"),
    person("T. R.", "Evans"),
    person("F. M.", "Heppel"),
    person("S. F.", "Schoch"),
    person("M.", "Korbmacher"),
    person("Y.", "Yamada"),
    person("N.", "Albayrak-Aydemir"),
    person("J. M.", "Wicherts")
  ),
  journal = "Psychological Methods",
  year = 2024,
  doi = "10.1037/met0000687"
)

Lakens2024 <- bibentry(
  bibtype = "Article",
  author  = "Lakens, Daniël",
  year    = 2024,
  title   = "When and How to Deviate From a Preregistration",
  journal = "Collabra: Psychology",
  volume  = 10,
  number  = 1,
  pages   = "117094",
  doi     = "10.1525/collabra.117094"
)

# helper functions ----

## OSF preregistration dispatch ----

# Identify a registration by its schema_id, then extract its responses. Each OSF
# registration template has a permanent schema_id
# (info$relationships$registration_schema$data$id). The schema *name*
# (registration_supplement) is not a reliable key because the OSF revises
# templates under the same name with incompatible response-key formats.
#
# Almost all templates are handled by one generic, schema-driven extractor
# (osf_pr_schema), which reads each field's human-readable label from the schema
# and maps it to a canonical field. This works for both schema formats:
# - "blocks" (modern): responses keyed by block position ("344-2"); label is the
#   question-label display_text.
# - "pages" (legacy): responses keyed by qid ("q19"); label is the question title.
# The only exceptions are schemas whose answers are nested too deeply to carry a
# usable label at the response-key level (see osf_special_handlers).
osf_prereg_extract <- function(info) {
  if (isTRUE(info$attributes$withdrawn)) {
    return(withdrawn(info))
  }

  schema_id <- info$relationships$registration_schema$data$id %||% NA_character_

  # A few schemas store answers under deeply nested, section-relative keys with
  # no usable field label at the response-key level (e.g. the original pages
  # version of the van 't Veer template). These need dedicated extractors.
  handler <- osf_special_handlers()[[schema_id]]
  if (!is.null(handler)) {
    return(handler(info))
  }

  # Everything else is read generically from the schema's field labels, which
  # works for both blocks-format and flat pages-format schemas (old and new).
  osf_pr_schema(info)
}

# map of schema_id -> dedicated extractor, for schemas the generic, label-driven
# extractor cannot handle (deeply nested, unlabelled-at-key-level). Wrapped in a
# function so it is evaluated at dispatch time, after the extractors are defined.
osf_special_handlers <- function() {
  list(
    # Pre-Registration in Social Psychology (van 't Veer & Giner-Sorolla, 2016)
    "5730e99a9ad5a102c5745a8a" = prsp  # original pages version (deeply nested)
  )
}

## Generic schema-driven extractor ----

# Extract responses from any registration whose schema carries human-readable
# field labels, for both schema formats:
# - blocks: response key "<grp>-<pos>"; label = the question-label display_text
#   preceding the input block at position <pos>.
# - pages (flat): response key is the question qid (possibly with a ".question"
#   /".uploader" sub-key); label = that question's title.
# Labels are mapped to canonical prereg_schema fields via osf_label_to_field().
# Deeply nested pages schemas (e.g. old van 't Veer) have no usable label at the
# response-key level and are handled by dedicated functions instead.
osf_pr_schema <- function(info) {
  common <- common_osf(info)

  responses <- info$attributes$registration_responses
  if (length(responses) == 0) {
    return(common)
  }

  # response key -> human-readable label, from the schema
  key_labels <- osf_schema_labels(info)
  if (length(key_labels) == 0) {
    return(common) # couldn't fetch schema; return common info only
  }

  keys <- names(responses)

  # reserved names set by common_osf() take precedence over schema fields, so a
  # schema field labelled e.g. "Title" cannot collide with the registration's
  # own title (which would create a duplicate column downstream).
  reserved <- names(common)

  extra <- list()
  for (i in seq_along(keys)) {
    # pages keys match directly; blocks keys ("<grp>-<pos>") match on position
    lookup <- function(k) if (k %in% names(key_labels)) key_labels[[k]] else NULL
    label <- lookup(keys[i])
    if (is.null(label)) {
      label <- lookup(sub("^.*-", "", keys[i]))
    }
    if (is.null(label) || is.na(label) || !nzchar(label)) next

    field <- osf_label_to_field(label)
    if (field %in% reserved) next

    value <- responses[[keys[i]]]
    value <- paste(unlist(value), collapse = " ") |> trimws()
    if (!nzchar(value)) next

    # if several labels map to the same canonical field, join them
    if (is.null(extra[[field]])) {
      extra[[field]] <- value
    } else {
      extra[[field]] <- paste(extra[[field]], value, sep = " ")
    }
  }

  c(common, extra)
}

# Fetch a registration's schema and return a named character vector mapping each
# possible response key to its human-readable field label. Handles both schema
# formats (blocks via display_text, pages via question titles).
osf_schema_labels <- function(info) {
  schema_url <- info$relationships$registration_schema$links$related$href
  if (is.null(schema_url)) return(character(0))

  schema <- tryCatch(
    httr2::request(schema_url) |>
      .osf_headers() |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_retry(
        max_tries = 3,
        is_transient = \(resp) httr2::resp_status(resp) == 429
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = FALSE),
    error = \(e) NULL
  )
  schema <- schema$data$attributes$schema

  if (!is.null(schema$blocks)) {
    return(osf_blocks_labels(schema$blocks))
  }
  if (!is.null(schema$pages)) {
    return(osf_pages_labels(schema$pages))
  }
  character(0)
}

# blocks format: key = "<grp>-<pos>" -> the question-label display_text that
# precedes the input block at 0-indexed position <pos>. Keyed by position
# (the "<grp>-" prefix is constant within a registration), with a duplicate
# entry under the bare position so either form resolves.
osf_blocks_labels <- function(blocks) {
  input_types <- c(
    "long-text-input", "short-text-input", "single-select-input",
    "multi-select-input", "file-input", "contributors-input"
  )

  labels <- character(0)
  last_label <- NA_character_
  for (i in seq_along(blocks)) {
    bt <- blocks[[i]]$block_type %||% NA_character_
    if (isTRUE(bt == "question-label")) {
      last_label <- blocks[[i]]$display_text %||% NA_character_
    }
    if (bt %in% input_types) {
      labels[[as.character(i - 1L)]] <- last_label
    }
  }
  labels
}

# pages format: key = question qid -> a usable field label. The schema title is
# preferred, but some templates (e.g. AsPredicted) use long full-sentence titles
# as prompts while giving the qid a clean semantic name ("sample", "analyses");
# in that case the qid is the better label. Object-type questions store answers
# under "<qid>.question"/"<qid>.uploader" sub-keys, so map those too.
osf_pages_labels <- function(pages) {
  labels <- character(0)
  for (p in pages) {
    for (q in p$questions) {
      qid <- q$qid %||% NA_character_
      if (is.na(qid)) next
      title <- q$title %||% NA_character_

      # prefer a short title; fall back to the qid when the title is missing or
      # is a long sentence (a prompt rather than a field name)
      title_is_label <- !is.na(title) && nzchar(title) &&
        nchar(title) <= 40 && !grepl("[?]", title)
      label <- if (title_is_label) title else qid

      labels[[qid]] <- label
      labels[[paste0(qid, ".question")]] <- label
      labels[[paste0(qid, ".uploader")]] <- label
    }
  }
  labels
}

# Map a schema field label (display_text) to a canonical prereg_schema field.
# Research-core labels (in their various OSF casings) map to shared canonical
# names so the same concept lines up across templates; unmapped labels fall
# back to a slugified version so no information is dropped.
osf_label_to_field <- function(label) {
  key <- tolower(trimws(label))
  if (key %in% names(osf_label_field)) {
    return(unname(osf_label_field[[key]]))
  }

  # slug fallback: lowercase, non-alphanumerics -> "_", trim repeats
  slug <- gsub("[^a-z0-9]+", "_", key)
  slug <- gsub("^_+|_+$", "", slug)
  if (!nzchar(slug)) "field" else slug
}

# Dictionary of research-core field labels -> canonical prereg_schema fields.
# Keys are lowercased display_text; multiple labels (and casings) intentionally
# collapse onto the same canonical field.
osf_label_field <- c(
  # research questions / hypotheses
  "research question"                  = "research_questions",
  "research questions"                 = "research_questions",
  "research question(s)"               = "research_questions",
  "primary research question(s)"       = "research_questions",
  "research questions or hypotheses"   = "research_questions",
  "research questions or hypothesis"   = "research_questions",
  "hypothesis"                         = "research_questions",
  "hypotheses"                         = "research_questions",
  "expectations / hypotheses"          = "research_questions",
  # description / background
  "description"                        = "description",
  "study description"                  = "description",
  "background"                         = "description",
  "summary"                            = "description",
  # study design / type
  "study design"                       = "study_design_overview",
  "study type"                         = "study_type",
  "number of conditions"               = "study_design_overview",
  "conditions"                         = "study_design_overview",
  # variables
  "manipulated variables"              = "manipulated_variables",
  "measured variables"                 = "measured_variables",
  "independent variables"              = "design_independent_variables",
  "dependent variables"                = "design_dependent_variables",
  "dependent variable"                 = "design_dependent_variables",
  "dependent"                          = "design_dependent_variables",
  "indices"                            = "indices",
  # blinding / randomisation
  "blinding of experimental treatments" = "blinding",
  "randomization"                      = "randomization",
  # data
  "existing data"                      = "existing_data",
  "explanation of existing data"       = "existing_data_explanation",
  "data collection procedures"         = "data_collection_procedures",
  "data collection"                    = "data_collection_started",
  "data"                               = "data_collection_started",
  # sample size
  "sample size"                        = "sample_size",
  "sample size rationale"              = "sample_size_rationale",
  "sampling and sample size"           = "sample_size",
  "my target sample size is"           = "sample_size",
  "the rationale for my sample size is" = "sample_size_rationale",
  "sample"                             = "sample_size",
  "stopping rule"                      = "stopping_rule",
  "stopping criteria"                  = "stopping_rule",
  "starting and stopping rules"        = "stopping_rule",
  # analysis
  "statistical models"                 = "statistical_tests",
  "statistical technique"              = "statistical_tests",
  "analyses"                           = "statistical_tests",
  "analyses2"                          = "additional_analyses",
  "transformations"                    = "transformations",
  "data transformations"               = "transformations",
  "planned data transformations"       = "transformations",
  "inference criteria"                 = "inference_criteria",
  "method of correction"               = "multiple_testing_correction",
  "reliability criteria"               = "reliability_criteria",
  "exploratory analysis"               = "exploratory_analyses",
  "other planned analysis"             = "exploratory_analyses",
  # exclusions / missing data / outliers
  "data exclusion"                     = "data_exclusion_criteria",
  "data inclusion and exclusion"       = "data_exclusion_criteria",
  "inclusion and exclusion criteria"   = "data_exclusion_criteria",
  "specific exclusion criteria"        = "data_exclusion_criteria",
  "outliers"                           = "outliers_and_exclusions",
  "outliers and exclusions"            = "outliers_and_exclusions",
  "missing data"                       = "missing_data_handling",
  # replication
  "replication importance"             = "replication_importance",
  # other
  "other"                              = "additional_comments",
  "additional information"             = "additional_comments",
  "context and additional information" = "additional_comments"
)

## AsPredicted Schema

ap_schema <- function(table_ap) {
  if (nrow(table_ap) == 0) {
    return(data.frame())
  }

  ap_id <- table_ap$ap_url |>
    sub("^https://aspredicted\\.org/", "", x = _) |>
    sub("\\.pdf.*", "", x = _) |>
    sub("blind\\.php\\?x\\=", "", x = _)

  data.frame(
    template_name = "AsPredicted",
    id = ap_id,
    link = table_ap$ap_url,
    title = table_ap$AP_title,
    date_created = table_ap$AP_created,
    existing_data_explanation = table_ap$AP_data,
    research_questions = table_ap$AP_hypotheses,
    design_dependent_variables = table_ap$AP_key_dv,
    study_design_overview = table_ap$AP_conditions,
    statistical_tests = table_ap$AP_analyses,
    outliers_and_exclusions = table_ap$AP_outliers,
    sample_size = table_ap$AP_sample_size,
    additional_comments = table_ap$AP_anything_else
  )
}

## Common OSF
common_osf <- function(info) {
  ra <- info$attributes

  list(
    template_name = ra$registration_supplement,
    title = ra$title,
    id = info$id,
    link = paste0("https://osf.io/", info$id),
    date_created = ra$date_created,
    date_modified = ra$date_modified,
    date_registered = ra$date_registered,
    embargo_end_date = ra$embargo_end_date,
    ia_url = ra$ia_url
  )
}


## Withdrawn ----
withdrawn <- function(info) {
  ra <- info$attributes

  common <- common_osf(info)
  extra <- list(
    description = "WITHDRAWN"
  )

  c(common, extra)
}

## Pre-Registration in Social Psychology (van 't Veer & Giner-Sorolla, 2016): Pre-Registration ----
prsp <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    research_questions =
      paste(c(
        prereg_answers$`description-hypothesis.question1a`,
        prereg_answers$`84-5`
      ), collapse = " "),
    hypotheses_interactions =
      paste(c(
        prereg_answers$`description-hypothesis.question2a`,
        prereg_answers$`84-7`
      ), collapse = " "),
    manipulation_checks =
      paste(c(
        prereg_answers$`description-hypothesis.question3a`,
        prereg_answers$`84-9`
      ), collapse = " "),
    theoretical_rationale =
      paste(c(
        prereg_answers$`recommended-hypothesis.question5a`,
        prereg_answers$`recommended-hypothesis.question6a`,
        prereg_answers$`84-14`, prereg_answers$`84-16`
      ), collapse = " "),
    design_independent_variables =
      paste(c(
        prereg_answers$`description-methods.design.question2a`,
        prereg_answers$`84-23`
      ), collapse = " "),
    design_dependent_variables =
      paste(c(
        prereg_answers$`description-methods.design.question2b`,
        prereg_answers$`84-25`
      ), collapse = " "),
    design_covariates_moderators =
      prereg_answers$`description-methods.design.question3b`,
    data_exclusion_criteria =
      paste(c(
        prereg_answers$`description-methods.planned-sample.question4b`,
        prereg_answers$`84-30`
      ), collapse = " "),
    data_collection_procedures =
      paste(
        c(
          prereg_answers$`description-methods.planned-sample.question5b`,
          prereg_answers$`description-methods.procedure.question10b`,
          prereg_answers$`84-32`, prereg_answers$`84-44`,
          prereg_answers$`84-47`, prereg_answers$`84-49`
        ),
        collapse = " "
      ),
    sample_size =
      paste(c(
        prereg_answers$`description-methods.planned-sample.question6b`,
        prereg_answers$`84-34`, prereg_answers$`84-36`
      ), collapse = " "),
    stopping_rule =
      paste(c(
        prereg_answers$`description-methods.planned-sample.question7b`,
        prereg_answers$`84-38`
      ), collapse = " "),
    outliers_and_exclusions =
      paste(c(
        prereg_answers$`description-methods.exclusion-criteria.question8b`,
        prereg_answers$`84-41`
      ), collapse = " "),
    fail_safe_exclusion_levels = prereg_answers$`recommended-methods.procedure.question9b`,
    indices =
      paste(c(
        prereg_answers$`confirmatory-analyses-first.first.question1c`,
        prereg_answers$`confirmatory-analyses-second.second.question1c`,
        prereg_answers$`confirmatory-analyses-third.third.question1c`,
        prereg_answers$`confirmatory-analyses-fourth.fourth.question1c`,
        prereg_answers$`confirmatory-analyses-further.further.question1c`,
        prereg_answers$`84-56`, prereg_answers$`84-68`,
        prereg_answers$`84-80`, prereg_answers$`84-92`
      ), collapse = " "),
    statistical_tests =
      paste(c(
        prereg_answers$`confirmatory-analyses-first.first.question2c`,
        prereg_answers$`confirmatory-analyses-second.second.question2c`,
        prereg_answers$`confirmatory-analyses-third.third.question2c`,
        prereg_answers$`confirmatory-analyses-fourth.fourth.question2c`,
        prereg_answers$`confirmatory-analyses-further.further.question2c`,
        prereg_answers$`84-58`, prereg_answers$`84-70`,
        prereg_answers$`84-82`, prereg_answers$`84-94`,
        prereg_answers$`84-126`
      ), collapse = " "),
    rationale_covariate =
      paste(c(
        prereg_answers$`confirmatory-analyses-first.first.question3c`,
        prereg_answers$`confirmatory-analyses-second.second.question3c`,
        prereg_answers$`confirmatory-analyses-third.third.question3c`,
        prereg_answers$`confirmatory-analyses-fourth.fourth.question3c`,
        prereg_answers$`confirmatory-analyses-further.further.question3c`,
        prereg_answers$`84-62`, prereg_answers$`84-74`,
        prereg_answers$`84-84`, prereg_answers$`84-96`
      ), collapse = " "),
    variables_roles_in_analyses =
      paste(c(
        prereg_answers$`confirmatory-analyses-first.first.question4c`,
        prereg_answers$`confirmatory-analyses-second.second.question4c`,
        prereg_answers$`confirmatory-analyses-third.third.question4c`,
        prereg_answers$`confirmatory-analyses-fourth.fourth.question4c`,
        prereg_answers$`confirmatory-analyses-further.further.question4c`,
        prereg_answers$`84-60`, prereg_answers$`84-72`,
        prereg_answers$`84-86`, prereg_answers$`84-98`
      ), collapse = " "),
    inference_criteria =
      paste(c(
        prereg_answers$`confirmatory-analyses-first.first.question5c`,
        prereg_answers$`confirmatory-analyses-second.second.question5c`,
        prereg_answers$`confirmatory-analyses-third.third.question5c`,
        prereg_answers$`confirmatory-analyses-fourth.fourth.question5c`,
        prereg_answers$`confirmatory-analyses-further.further.question5c`,
        prereg_answers$`84-64`, prereg_answers$`84-76`,
        prereg_answers$`84-88`, prereg_answers$`84-100`
      ), collapse = " "),
    multiple_testing_correction =
      paste(c(
        prereg_answers$`recommended-analysis.specify.question6c`,
        prereg_answers$`84-116`
      ), collapse = " "),
    missing_data_handling =
      paste(c(
        prereg_answers$`recommended-analysis.specify.question7c`,
        prereg_answers$`84-118`
      ), collapse = " "),
    reliability_criteria =
      paste(c(
        prereg_answers$`recommended-analysis.specify.question8c`,
        prereg_answers$`84-120`
      ), collapse = " "),
    transformations =
      paste(c(
        prereg_answers$`recommended-analysis.specify.question9c`,
        prereg_answers$`84-122`
      ), collapse = " "),
    assumptions_and_contingencies =
      paste(c(
        prereg_answers$`recommended-analysis.specify.question10c`,
        prereg_answers$`84-124`
      ), collapse = " "),
    data_collection_started =
      paste(c(
        prereg_answers$`datacompletion`,
        prereg_answers$`84-130`
      ), collapse = " "),
    data_looked =
      paste(c(
        prereg_answers$`looked`,
        prereg_answers$`84-134`
      ), collapse = " "),
    project_dates_start_end =
      paste(c(
        prereg_answers$`dataCollectionDates`,
        prereg_answers$`84-138`
      ), collapse = " "),
    additional_comments =
      paste(c(
        prereg_answers$`additionalComments`,
        prereg_answers$`84-140`
      ), collapse = " ")
  )

  c(common, extra)
}

# prereg schema ----
prereg_schema <- data.frame(
  id = NA_character_,
  date_created = NA_character_,
  template_name = NA_character_,
  registration_narrative_summary = NA_character_,
  title = NA_character_,
  authors = NA_character_,
  description = NA_character_,
  research_questions = NA_character_,
  hypotheses_main = NA_character_,
  hypotheses_interactions = NA_character_,
  manipulation_checks = NA_character_,
  theoretical_rationale = NA_character_,
  additional_comments = NA_character_,
  project_dates_start_end = NA_character_,
  study_type = NA_character_,
  study_design_overview = NA_character_,
  design_independent_variables = NA_character_,
  design_dependent_variables = NA_character_,
  design_covariates_moderators = NA_character_,
  blinding = NA_character_,
  randomization = NA_character_,
  manipulated_variables = NA_character_,
  measured_variables = NA_character_,
  indices = NA_character_,
  existing_data = NA_character_,
  existing_data_explanation = NA_character_,
  data_collection_procedures = NA_character_,
  data_collection_location = NA_character_,
  data_collection_started = NA_character_,
  data_looked = NA_character_,
  sample_size = NA_character_,
  sample_size_rationale = NA_character_,
  stopping_rule = NA_character_,
  fail_safe_exclusion_levels = NA_character_,
  statistical_tests = NA_character_,
  additional_analyses = NA_character_,
  transformations = NA_character_,
  inference_criteria = NA_character_,
  multiple_testing_correction = NA_character_,
  assumptions_and_contingencies = NA_character_,
  variables_roles_in_analyses = NA_character_,
  rationale_covariate = NA_character_,
  reliability_criteria = NA_character_,
  data_exclusion_criteria = NA_character_,
  outliers_and_exclusions = NA_character_,
  missing_data_handling = NA_character_,
  exploratory_analyses = NA_character_,
  replication_description = NA_character_,
  replication_importance = NA_character_,
  effect_size_original = NA_character_,
  confidence_interval_original = NA_character_,
  original_study_conducted = NA_character_,
  region = NA_character_,
  original_sample_size = NA_character_,
  original_population = NA_character_,
  original_data_collection = NA_character_,
  original_materials_available = NA_character_,
  instruction_similarities = NA_character_,
  measure_similarities = NA_character_,
  stimuli_similarities = NA_character_,
  procedure_similarities = NA_character_,
  location_similarities = NA_character_,
  remuneration_similarities = NA_character_,
  participant_similarities = NA_character_,
  differences_influencing_effects = NA_character_,
  date_modified = NA_character_,
  date_registered = NA_character_,
  embargo_end_date = NA_character_,
  ia_url = NA_character_
)

