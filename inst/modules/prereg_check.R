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
#' @import tidyr
#' @import httr
#' @import jsonlite
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
prereg_check <- function(paper, ...) {
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
        id = info_table(paper, c())$id,
        preregistration = 0
      )
    )
    return(resp)
  }

  ## AsPredicted preregs ----
  table_ap <- suppressMessages(
    aspredicted_retrieve(links_ap$text)
  )
  ap_schema <- ap_schema(table_ap)

  ## OSF prereg ----
  osf_ids <- links_osf$text |>
    osf_check_id() |>
    unique()
  link_types <- osf_type(osf_ids)
  reg_ids <- osf_ids[link_types == "registrations" & !is.na(link_types)]

  ## no registrations ----
  if (length(reg_ids) == 0 & nrow(table_ap) == 0) {
    resp <- list(
      traffic_light = "na",
      summary_text = sprintf("We found %d OSF link%s, but no registrations.",
                             nrow(links_osf), nrow(links_osf) |> plural()),
      na_replace = 0,
      summary_table = data.frame(
        id = info_table(paper, c())$id,
        preregistration = 0
      )
    )
    return(resp)
  }

  ## get reg info from OSF ----
  url <- sprintf("https://api.osf.io/v2/registrations/?filter[id]=%s",
                 paste(reg_ids, collapse = ","))
  reg_info <- osf_get_all_pages(url)

  osf_schemas <- lapply(seq_along(reg_info$id), \(i) {
    info <- reg_info[i, ]
    template <- info$attributes$registration_supplement
    osf31 <- info$attributes$registration_responses$q25

    if (info$attributes$withdrawn) {
      withdrawn(info)
    } else if (template == "OSF Preregistration" &&
               !is.null(osf31) && !is.na(osf31)) {
      osf_pr_31(info)
    } else if (template == "OSF Preregistration") {
      osf_pr_28(info)
    } else if (template == "Open-Ended Registration") {
      oer(info)
    } else if (template == "Prereg Challenge") {
      prc(info)
    } else if (template == "Preregistration Template from AsPredicted.org") {
      prap(info)
    } else if (template == "Pre-Registration in Social Psychology (van 't Veer & Giner-Sorolla, 2016): Pre-Registration") {
      prsp(info)
    } else if (template == "Replication Recipe (Brandt et al., 2013): Pre-Registration") {
      rrbrandt(info)
    } else if (template == "OSF-Standard Pre-Data Collection Registration") {
      osfpre(info)
    }
  })

  # make sure all items are not lists
  prereg_schemas <- c(osf_schemas, list(ap_schema))
  ps <- lapply(prereg_schemas, \(x) lapply(x, paste, collapse = "\n\n"))
  prereg_info <- do.call(dplyr::bind_rows, ps)

  # traffic light ----
  tl <- "info"

  # summary_text ----
  summary_text <- sprintf("We found %d preregistration%s.",
                          nrow(prereg_info), nrow(prereg_info) |> plural())

  # report ----
  report_text <- sprintf(
    "Meta-scientific research has shown that deviations from preregistrations are often not reported or checked, and that the most common deviations concern the sample size. We recommend manually checking the full preregistration at the link%s below, and have provided the preregistered sample size.",
    nrow(prereg_info) |> plural())

  prereg_link_table <- data.frame(
    id = link(prereg_info$link, prereg_info$id),
    title = prereg_info$title,
    template = prereg_info$template_name
  )

  samplesize_table <- prereg_info[, c("id", "sample_size")]

  ## summary output for paperlists ----
  summary_table <- dplyr::count(prereg_info, id,
                                name = "preregistration",
                                .drop = FALSE)

  ## prereg table ----
  # Remove columns where all values are NA
  prereg_table <- prereg_info[
    , colSums(!is.na(prereg_info)) > 0
  ] |> t() |> as.data.frame()

  # Add row names as a proper column (first column)
  prereg_table <- cbind(Field = rownames(prereg_table), prereg_table)

  # Rename columns "Preregistration 1", "Preregistration 2", ...
  n_prereg <- ncol(prereg_table) - 1  # subtract the 'Field' column
  colnames(prereg_table)[-1] <- paste0("Preregistration ", seq_len(n_prereg))

  ## guidance ----
  guidance <- c(
    "For metascientific articles demonstrating the rate of deviationsfrom preregistrations, see:",
    format_ref(vandenAkker2024),
    "For educational material on how to report deviations from preregistrations, see:",
    format_ref(Lakens2024)
  )

  report <- c(
    summary_text,
    scroll_table(prereg_link_table),
    report_text,
    scroll_table(samplesize_table),
    collapse_section(scroll_table(prereg_table, maxrows = 5),
                     "Full Preregistration"),
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

# referelnces

vandenAkker2024 <-  bibentry(
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

## AsPredicted Schema

ap_schema <- function(table_ap) {
  if (nrow(table_ap) == 0) return(data.frame())

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

## OSF-Standard Pre-Data Collection Registration ----
osfpre <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    data_collection_started = prereg_answers$datacompletion,
    data_looked = prereg_answers$looked,
    additional_comments = prereg_answers$comments
  )

  c(common, extra)
}

## Open-Ended Registration----
oer <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    description = prereg_answers$summary,
    sample_size = "The authors did not use a template. See the full preregistration for the sample size."
  )

  c(common, extra)
}

## Preregistration Template from AsPredicted.org ----
prap <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    data_collection_started = prereg_answers$data,
    research_questions = prereg_answers$hypothesis,
    design_dependent_variables = prereg_answers$dependent,
    study_design_overview = prereg_answers$conditions,
    statistical_tests = prereg_answers$analyses,
    outliers_and_exclusions = prereg_answers$outliers,
    sample_size = prereg_answers$sample,
    study_type = paste(c(prereg_answers$study_type,
                         prereg_answers$study_type_other), collapse = " "),
    additional_comments = prereg_answers$other
  )

  c(common, extra)
}


## OSF Preregistration 31 ----

osf_pr_31 <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    authors = prereg_answers$q2,
    description = prereg_answers$q3,
    research_questions = prereg_answers$q4,
    study_type = prereg_answers$q5,
    blinding = paste(c(prereg_answers$q6,
                       prereg_answers$q7), collapse = " "),
    study_design_overview = prereg_answers$q8.question,
    randomization = prereg_answers$q9,
    data_collection_started = prereg_answers$q10,
    existing_data_explanation = prereg_answers$q11,
    data_collection_procedures = prereg_answers$q12.question,
    sample_size = prereg_answers$q13,
    sample_size_rationale = prereg_answers$q14,
    stopping_rule = prereg_answers$q15,
    design_independent_variables = prereg_answers$q16.question,
    design_dependent_variables = prereg_answers$q17.question,
    indices = prereg_answers$q18.question,
    statistical_tests = prereg_answers$q19.question,
    transformations = prereg_answers$q20,
    inference_criteria = prereg_answers$q21,
    data_exclusion_criteria = prereg_answers$q22,
    outliers_and_exclusions = prereg_answers$q23,
    exploratory_analyses  = prereg_answers$q24,
    additional_comments = prereg_answers$q25
  )

  c(common, extra)
}

## OSF Preregistration 28 ----
osf_pr_28 <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    description = prereg_answers$q2,
    #research_questions = prereg_answers$q3,
    study_type = prereg_answers$q3,
    blinding = paste(c(prereg_answers$q4,
                       prereg_answers$q5), collapse = " "),
    study_design_overview = prereg_answers$q6.question,
    #randomization = prereg_answers$q9,
    data_collection_started = prereg_answers$q8,
    existing_data_explanation = prereg_answers$q9,
    data_collection_procedures = prereg_answers$q10.question,
    sample_size = prereg_answers$q11,
    sample_size_rationale = prereg_answers$q12,
    stopping_rule = prereg_answers$q13,
    design_independent_variables = prereg_answers$q14.question,
    design_dependent_variables = prereg_answers$q15.question,
    indices = prereg_answers$q16.question,
    statistical_tests = prereg_answers$q17.question,
    #statistical_tests = prereg_answers$q20,
    inference_criteria = prereg_answers$q19,
    data_exclusion_criteria = prereg_answers$q20,
    outliers_and_exclusions = prereg_answers$q21,
    exploratory_analyses  = prereg_answers$q22,
    additional_comments = prereg_answers$q23
  )

  c(common, extra)
}

## Prereg Challenge ----
prc <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  blinding <- {
    x <- unlist(prereg_answers$q15, use.names = FALSE);
    if (length(x)==0) NA_character_ else paste(x, collapse = " ")
  }

  common <- common_osf(info)

  extra <- list(
    description = prereg_answers$q3,
    research_questions = prereg_answers$q4,
    data_collection_started = prereg_answers$q5,
    existing_data_explanation = prereg_answers$q6,
    data_collection_procedures = prereg_answers$q7.question,
    sample_size = prereg_answers$q8,
    sample_size_rationale = prereg_answers$q9,
    stopping_rule = prereg_answers$q10,
    design_independent_variables = prereg_answers$q11.question,
    design_dependent_variables = prereg_answers$q12.question,
    indices = prereg_answers$q13.question,
    study_type = prereg_answers$q14,
    blinding = blinding,
    study_design_overview = prereg_answers$q16.question,
    randomization = prereg_answers$q17,
    statistical_tests = prereg_answers$q19.question,
    transformations = prereg_answers$q20,
    additional_analyses = prereg_answers$q21,
    inference_criteria = prereg_answers$q22,
    data_exclusion_criteria = prereg_answers$q23,
    outliers_and_exclusions = prereg_answers$q24,
    exploratory_analyses = prereg_answers$q25
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
      paste(c(prereg_answers$`description-hypothesis.question1a`,
              prereg_answers$`84-5`), collapse = " "),
    hypotheses_interactions =
      paste(c(prereg_answers$`description-hypothesis.question2a`,
              prereg_answers$`84-7`), collapse = " "),
    manipulation_checks =
      paste(c(prereg_answers$`description-hypothesis.question3a`,
              prereg_answers$`84-9`), collapse = " "),
    theoretical_rationale =
      paste(c(prereg_answers$`recommended-hypothesis.question5a`,
              prereg_answers$`recommended-hypothesis.question6a`,
              prereg_answers$`84-14`, prereg_answers$`84-16`), collapse = " "),
    design_independent_variables =
      paste(c(prereg_answers$`description-methods.design.question2a`,
              prereg_answers$`84-23`), collapse = " "),
    design_dependent_variables =
      paste(c(prereg_answers$`description-methods.design.question2b`,
              prereg_answers$`84-25`), collapse = " "),
    design_covariates_moderators =
      prereg_answers$`description-methods.design.question3b`,
    data_exclusion_criteria =
      paste(c(prereg_answers$`description-methods.planned-sample.question4b`,
              prereg_answers$`84-30`), collapse = " "),
    data_collection_procedures =
      paste(c(prereg_answers$`description-methods.planned-sample.question5b`,
              prereg_answers$`description-methods.procedure.question10b`,
              prereg_answers$`84-32`, prereg_answers$`84-44`,
              prereg_answers$`84-47`, prereg_answers$`84-49`),
              collapse = " "),
    sample_size =
      paste(c(prereg_answers$`description-methods.planned-sample.question6b`,
              prereg_answers$`84-34`, prereg_answers$`84-36`), collapse = " "),
    stopping_rule =
      paste(c(prereg_answers$`description-methods.planned-sample.question7b`,
              prereg_answers$`84-38`), collapse = " "),
    outliers_and_exclusions =
      paste(c(prereg_answers$`description-methods.exclusion-criteria.question8b`,
              prereg_answers$`84-41`), collapse = " "),
    fail_safe_exclusion_levels = prereg_answers$`recommended-methods.procedure.question9b`,
    indices =
      paste(c(prereg_answers$`confirmatory-analyses-first.first.question1c`,
              prereg_answers$`confirmatory-analyses-second.second.question1c`,
              prereg_answers$`confirmatory-analyses-third.third.question1c`,
              prereg_answers$`confirmatory-analyses-fourth.fourth.question1c`,
              prereg_answers$`confirmatory-analyses-further.further.question1c`,
              prereg_answers$`84-56`, prereg_answers$`84-68`,
              prereg_answers$`84-80`, prereg_answers$`84-92`), collapse = " "),
    statistical_tests =
      paste(c(prereg_answers$`confirmatory-analyses-first.first.question2c`,
              prereg_answers$`confirmatory-analyses-second.second.question2c`,
              prereg_answers$`confirmatory-analyses-third.third.question2c`,
              prereg_answers$`confirmatory-analyses-fourth.fourth.question2c`,
              prereg_answers$`confirmatory-analyses-further.further.question2c`,
              prereg_answers$`84-58`, prereg_answers$`84-70`,
              prereg_answers$`84-82`, prereg_answers$`84-94`,
              prereg_answers$`84-126`), collapse = " "),
    rationale_covariate =
      paste(c(prereg_answers$`confirmatory-analyses-first.first.question3c`,
              prereg_answers$`confirmatory-analyses-second.second.question3c`,
              prereg_answers$`confirmatory-analyses-third.third.question3c`,
              prereg_answers$`confirmatory-analyses-fourth.fourth.question3c`,
              prereg_answers$`confirmatory-analyses-further.further.question3c`,
              prereg_answers$`84-62`, prereg_answers$`84-74`,
              prereg_answers$`84-84`, prereg_answers$`84-96`), collapse = " "),
    variables_roles_in_analyses =
      paste(c(prereg_answers$`confirmatory-analyses-first.first.question4c`,
              prereg_answers$`confirmatory-analyses-second.second.question4c`,
              prereg_answers$`confirmatory-analyses-third.third.question4c`,
              prereg_answers$`confirmatory-analyses-fourth.fourth.question4c`,
              prereg_answers$`confirmatory-analyses-further.further.question4c`,
              prereg_answers$`84-60`, prereg_answers$`84-72`,
              prereg_answers$`84-86`, prereg_answers$`84-98`), collapse = " "),
    inference_criteria =
      paste(c(prereg_answers$`confirmatory-analyses-first.first.question5c`,
              prereg_answers$`confirmatory-analyses-second.second.question5c`,
              prereg_answers$`confirmatory-analyses-third.third.question5c`,
              prereg_answers$`confirmatory-analyses-fourth.fourth.question5c`,
              prereg_answers$`confirmatory-analyses-further.further.question5c`,
              prereg_answers$`84-64`, prereg_answers$`84-76`,
              prereg_answers$`84-88`, prereg_answers$`84-100`), collapse = " "),
    multiple_testing_correction =
      paste(c(prereg_answers$`recommended-analysis.specify.question6c`,
              prereg_answers$`84-116`), collapse = " "),
    missing_data_handling =
      paste(c(prereg_answers$`recommended-analysis.specify.question7c`,
              prereg_answers$`84-118`), collapse = " "),
    reliability_criteria =
      paste(c(prereg_answers$`recommended-analysis.specify.question8c`,
              prereg_answers$`84-120`), collapse = " "),
    transformations =
      paste(c(prereg_answers$`recommended-analysis.specify.question9c`,
              prereg_answers$`84-122`), collapse = " "),
    assumptions_and_contingencies =
      paste(c(prereg_answers$`recommended-analysis.specify.question10c`,
              prereg_answers$`84-124`), collapse = " "),
    data_collection_started =
      paste(c(prereg_answers$`datacompletion`,
              prereg_answers$`84-130`), collapse = " "),
    data_looked =
      paste(c(prereg_answers$`looked`,
              prereg_answers$`84-134`), collapse = " "),
    project_dates_start_end =
      paste(c(prereg_answers$`dataCollectionDates`,
              prereg_answers$`84-138`), collapse = " "),
    additional_comments =
      paste(c(prereg_answers$`additionalComments`,
              prereg_answers$`84-140`), collapse = " ")
  )

  c(common, extra)
}

## Replication Recipe (Brandt et al., 2013): Pre-Registration" ----
rrbrandt <- function(info) {
  ra <- info$attributes
  prereg_answers <- ra$registration_responses

  common <- common_osf(info)

  extra <- list(
    description = prereg_answers$item1,
    replication_importance = prereg_answers$item2,
    effect_size_original = prereg_answers$item3,
    confidence_interval_original = prereg_answers$item4,
    original_sample_size = prereg_answers$item5,
    original_study_conducted = prereg_answers$item6,
    region = prereg_answers$item7,
    original_population = prereg_answers$item8,
    original_data_collection = prereg_answers$item9,
    original_materials_available = prereg_answers$item10,
    assumptions_and_contingencies = prereg_answers$item11,
    data_collection_location = prereg_answers$item12,
    blinding = paste(prereg_answers$item13,
                     prereg_answers$item14),
    sample_size = prereg_answers$item15,
    sample_size_rationale = prereg_answers$item16,
    instruction_similarities = prereg_answers$item17,
    measure_similarities = prereg_answers$item18,
    stimuli_similarities = prereg_answers$item19,
    procedure_similarities = prereg_answers$item20,
    location_similarities = prereg_answers$item21,
    remuneration_similarities = prereg_answers$item22,
    participant_similarities = prereg_answers$item23,
    differences_influencing_effects = paste(prereg_answers$item24,
                                            prereg_answers$item25),
    data_exclusion_criteria  = prereg_answers$item26,
    statistical_tests = prereg_answers$item27,
    inference_criteria = prereg_answers$item28
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


