#' Preregistration Check
#'
#' @description
#' Retrieve information from preregistrations, and make them easier to check.
#'
#' @keywords method
#'
#' @author Daniel Lakens
#' @author Lisa DeBruine
#'
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import jsonlite
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list with table, summary, traffic light, and report text
prereg_check <- function(paper, ...) {
  # paper <- psychsci[[218]] # to test
  # and paper <- xml[["09567976251396084"]] for multiple aspredicted
  # osf: paper <- xml[["09567976221114055"]]
  # AsPredicted preregs
  links_ap <- aspredicted_links(paper)
  links_ap <- links_ap[!duplicated(links_ap$text), ]
  table <- aspredicted_retrieve(links_ap, id_col = 1)

  preregistration_information <- tibble()  # empty tibble to collect rows

  if (nrow(table) == 0) {
    report <- "We detected no links to preregistrations."
  } else {
    for (i in seq.int(1, nrow(table))) {
      prereg_schema <- tibble(
        id = NA_character_,
        link = NA_character_,
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

      prereg_schema$template_name <- "AsPredicted"
      prereg_schema$id <- sub("^https://aspredicted\\.org/([^\\.]+)\\.pdf.*$", "\\1", table$text[i])
      prereg_schema$id <- ifelse(grepl("\\.pdf", table$text[i]),
                                 sub("^https://aspredicted\\.org/([^\\.]+)\\.pdf.*$", "\\1", table$text[i]),
                                 sub("^https://aspredicted\\.org/(.*)$", "\\1", table$text[i]))
      prereg_schema$link <- table$text[i]
      prereg_schema$title <- table$AP_title[i]
      prereg_schema$date_created <- table$AP_created[i]
      prereg_schema$existing_data_explanation <- table$AP_data[i]
      prereg_schema$research_questions <- table$AP_hypotheses[i]
      prereg_schema$design_dependent_variables <- table$AP_key_dv[i]
      prereg_schema$study_design_overview <- table$AP_conditions[i]
      prereg_schema$statistical_tests <- table$AP_analyses[i]
      prereg_schema$outliers_and_exclusions <- table$AP_outliers[i]
      prereg_schema$sample_size <- table$AP_sample_size[i]
      prereg_schema$additional_comments <- table$AP_anything_else[i]

      preregistration_information <- bind_rows(preregistration_information, prereg_schema)   # append row
    }
  }

  # Find OSF links
  links_osf <- osf_links(paper)
  if (nrow(links_osf) > 0) {
    info_from_osf <- osf_retrieve(osf_check_id(links_osf$text))
    rows_to_loop <- info_from_osf %>% filter(osf_type == "registrations")
    if (nrow(rows_to_loop) > 0) {
      for (i in seq_len(nrow(rows_to_loop))) {
        registration <- GET(paste("https://api.osf.io/v2/registrations/?filter[id]=",rows_to_loop$osf_id[i], sep = ""))
        registration_content <- fromJSON(rawToChar(registration$content))
        template <- registration_content$data$attributes$registration_supplement
        prereg_questions <- registration_content$data$attributes$registered_meta
        prereg_answers <- registration_content$data$attributes$registration_responses

        prereg_schema <- tibble(
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
        if (registration_content$data$attributes$withdrawn == TRUE) {
          prereg_schema$template_name <- template
          prereg_schema$title <- registration_content$data$attributes$title
          prereg_schema$id <- registration_content$data$id
          prereg_schema$link <- paste0("https://osf.io/",registration_content$data$id)
          prereg_schema$date_created <- registration_content$data$attributes$date_created
          prereg_schema$date_modified <- registration_content$data$attributes$date_modified
          prereg_schema$date_registered <- registration_content$data$attributes$date_registered
          prereg_schema$embargo_end_date <- registration_content$data$attributes$embargo_end_date
          prereg_schema$ia_url <- registration_content$data$attributes$ia_url
          prereg_schema$description <- "WITHDRAWN"
        } else if (template == "Prereg Challenge") {
          prereg_schema$template_name <- template
          prereg_schema$title <- prereg_answers$q1
          prereg_schema$id <- registration_content$data$id
          prereg_schema$link <- paste0("https://osf.io/",registration_content$data$id)
          prereg_schema$date_created <- registration_content$data$attributes$date_created
          prereg_schema$date_modified <- registration_content$data$attributes$date_modified
          prereg_schema$date_registered <- registration_content$data$attributes$date_registered
          prereg_schema$embargo_end_date <- registration_content$data$attributes$embargo_end_date
          prereg_schema$ia_url <- registration_content$data$attributes$ia_url
          prereg_schema$description <- prereg_answers$q3
          prereg_schema$research_questions <- prereg_answers$q4
          prereg_schema$data_collection_started <- prereg_answers$q5
          prereg_schema$existing_data_explanation <- prereg_answers$q6
          prereg_schema$data_collection_procedures <- prereg_answers$q7.question
          prereg_schema$sample_size <- prereg_answers$q8
          prereg_schema$sample_size_rationale <- prereg_answers$q9
          prereg_schema$stopping_rule = prereg_answers$q10
          prereg_schema$design_independent_variables <- prereg_answers$q11.question
          prereg_schema$design_dependent_variables <- prereg_answers$q12.question
          prereg_schema$indices <- prereg_answers$q13.question
          prereg_schema$study_type <- prereg_answers$q14
          prereg_schema$blinding <- { x <- unlist(prereg_answers$q15, use.names = FALSE); if (length(x)==0) NA_character_ else paste(x, collapse = " ") }
          prereg_schema$study_design_overview <- prereg_answers$q16.question
          prereg_schema$randomization <- prereg_answers$q17
          prereg_schema$statistical_tests <- prereg_answers$q19.question
          prereg_schema$transformations <- prereg_answers$q20
          prereg_schema$additional_analyses <- prereg_answers$q21
          prereg_schema$inference_criteria <- prereg_answers$q22
          prereg_schema$data_exclusion_criteria <- prereg_answers$q23
          prereg_schema$outliers_and_exclusions <- prereg_answers$q24
          prereg_schema$exploratory_analyses <- prereg_answers$q25
        } else if (template == "Preregistration Template from AsPredicted.org") {
          prereg_schema$template_name <- template
          prereg_schema$title <- prereg_answers$name
          prereg_schema$id <- registration_content$data$id
          prereg_schema$link <- paste0("https://osf.io/",registration_content$data$id)
          prereg_schema$date_created <- registration_content$data$attributes$date_created
          prereg_schema$date_modified <- registration_content$data$attributes$date_modified
          prereg_schema$date_registered <- registration_content$data$attributes$date_registered
          prereg_schema$embargo_end_date <- registration_content$data$attributes$embargo_end_date
          prereg_schema$ia_url <- registration_content$data$attributes$ia_url
          prereg_schema$data_collection_started = prereg_answers$data
          prereg_schema$research_questions <- prereg_answers$hypothesis
          prereg_schema$design_dependent_variables <- prereg_answers$dependent
          prereg_schema$study_design_overview <- prereg_answers$conditions
          prereg_schema$statistical_tests <- prereg_answers$analyses
          prereg_schema$outliers_and_exclusions <- prereg_answers$outliers
          prereg_schema$sample_size <- prereg_answers$sample
          prereg_schema$study_type <- paste(c(prereg_answers$study_type, prereg_answers$study_type_other), collapse = " ")
          prereg_schema$additional_comments <- prereg_answers$other
        } else if (template == "Open-Ended Registration") {
          prereg_schema$template_name <- template
          prereg_schema$id <- registration_content$data$id
          prereg_schema$link <- paste0("https://osf.io/",registration_content$data$id)
          prereg_schema$date_created <- registration_content$data$attributes$date_created
          prereg_schema$date_modified <- registration_content$data$attributes$date_modified
          prereg_schema$date_registered <- registration_content$data$attributes$date_registered
          prereg_schema$embargo_end_date <- registration_content$data$attributes$embargo_end_date
          prereg_schema$ia_url <- registration_content$data$attributes$ia_url
          prereg_schema$title <- registration_content$data$attributes$title
          prereg_schema$description <- prereg_answers$summary
          prereg_schema$sample_size <- "The authors did not use a template. See the full preregistration for the sample size."
        } else if (template == "Pre-Registration in Social Psychology (van 't Veer & Giner-Sorolla, 2016): Pre-Registration") {
          prereg_schema$template_name <- template
          prereg_schema$title <- registration_content$data$attributes$title
          prereg_schema$id <- registration_content$data$id
          prereg_schema$link <- paste0("https://osf.io/",registration_content$data$id)
          prereg_schema$date_created <- registration_content$data$attributes$date_created
          prereg_schema$date_modified <- registration_content$data$attributes$date_modified
          prereg_schema$date_registered <- registration_content$data$attributes$date_registered
          prereg_schema$embargo_end_date <- registration_content$data$attributes$embargo_end_date
          prereg_schema$ia_url <- registration_content$data$attributes$ia_url
          prereg_schema$research_questions <- paste(c(prereg_answers$`description-hypothesis.question1a`, prereg_answers$`84-5`), collapse = " ")
          prereg_schema$hypotheses_interactions <- paste(c(prereg_answers$`description-hypothesis.question2a`, prereg_answers$`84-7`), collapse = " ")
          prereg_schema$manipulation_checks <- paste(c(prereg_answers$`description-hypothesis.question3a`, prereg_answers$`84-9`), collapse = " ")
          prereg_schema$theoretical_rationale <- paste(c(prereg_answers$`recommended-hypothesis.question5a`, prereg_answers$`recommended-hypothesis.question6a`, prereg_answers$`84-14`, prereg_answers$`84-16`), collapse = " ")
          prereg_schema$design_independent_variables <- paste(c(prereg_answers$`description-methods.design.question2a`, prereg_answers$`84-23`), collapse = " ")
          prereg_schema$design_dependent_variables <- paste(c(prereg_answers$`description-methods.design.question2b`, prereg_answers$`84-25`), collapse = " ")
          prereg_schema$design_covariates_moderators <- prereg_answers$`description-methods.design.question3b`
          prereg_schema$data_exclusion_criteria <- paste(c(prereg_answers$`description-methods.planned-sample.question4b`, prereg_answers$`84-30`), collapse = " ")
          prereg_schema$data_collection_procedures <- paste(c(prereg_answers$`description-methods.planned-sample.question5b`, prereg_answers$`description-methods.procedure.question10b`, prereg_answers$`84-32`, prereg_answers$`84-44`, prereg_answers$`84-47`, prereg_answers$`84-49`), collapse = " ")
          prereg_schema$sample_size <- paste(c(prereg_answers$`description-methods.planned-sample.question6b`, prereg_answers$`84-34`, prereg_answers$`84-36`), collapse = " ")
          prereg_schema$stopping_rule <- paste(c(prereg_answers$`description-methods.planned-sample.question7b`, prereg_answers$`84-38`), collapse = " ")
          prereg_schema$outliers_and_exclusions <- paste(c(prereg_answers$`description-methods.exclusion-criteria.question8b`, prereg_answers$`84-41`), collapse = " ")
          prereg_schema$fail_safe_exclusion_levels <- prereg_answers$`recommended-methods.procedure.question9b`
          prereg_schema$indices <- paste(c(prereg_answers$`confirmatory-analyses-first.first.question1c`, prereg_answers$`confirmatory-analyses-second.second.question1c`, prereg_answers$`confirmatory-analyses-third.third.question1c`, prereg_answers$`confirmatory-analyses-fourth.fourth.question1c`, prereg_answers$`confirmatory-analyses-further.further.question1c`, prereg_answers$`84-56`, prereg_answers$`84-68`, prereg_answers$`84-80`, prereg_answers$`84-92`), collapse = " ")
          prereg_schema$statistical_tests <- paste(c(prereg_answers$`confirmatory-analyses-first.first.question2c`, prereg_answers$`confirmatory-analyses-second.second.question2c`, prereg_answers$`confirmatory-analyses-third.third.question2c`, prereg_answers$`confirmatory-analyses-fourth.fourth.question2c`, prereg_answers$`confirmatory-analyses-further.further.question2c`, prereg_answers$`84-58`, prereg_answers$`84-70`, prereg_answers$`84-82`, prereg_answers$`84-94`, prereg_answers$`84-126`), collapse = " ")
          prereg_schema$rationale_covariate <- paste(c(prereg_answers$`confirmatory-analyses-first.first.question3c`, prereg_answers$`confirmatory-analyses-second.second.question3c`, prereg_answers$`confirmatory-analyses-third.third.question3c`, prereg_answers$`confirmatory-analyses-fourth.fourth.question3c`, prereg_answers$`confirmatory-analyses-further.further.question3c`, prereg_answers$`84-62`, prereg_answers$`84-74`, prereg_answers$`84-84`, prereg_answers$`84-96`), collapse = " ")
          prereg_schema$variables_roles_in_analyses <- paste(c(prereg_answers$`confirmatory-analyses-first.first.question4c`, prereg_answers$`confirmatory-analyses-second.second.question4c`, prereg_answers$`confirmatory-analyses-third.third.question4c`, prereg_answers$`confirmatory-analyses-fourth.fourth.question4c`, prereg_answers$`confirmatory-analyses-further.further.question4c`, prereg_answers$`84-60`, prereg_answers$`84-72`, prereg_answers$`84-86`, prereg_answers$`84-98`), collapse = " ")
          prereg_schema$inference_criteria <- paste(c(prereg_answers$`confirmatory-analyses-first.first.question5c`, prereg_answers$`confirmatory-analyses-second.second.question5c`, prereg_answers$`confirmatory-analyses-third.third.question5c`, prereg_answers$`confirmatory-analyses-fourth.fourth.question5c`, prereg_answers$`confirmatory-analyses-further.further.question5c`, prereg_answers$`84-64`, prereg_answers$`84-76`, prereg_answers$`84-88`, prereg_answers$`84-100`), collapse = " ")
          prereg_schema$multiple_testing_correction <- paste(c(prereg_answers$`recommended-analysis.specify.question6c`, prereg_answers$`84-116`), collapse = " ")
          prereg_schema$missing_data_handling <- paste(c(prereg_answers$`recommended-analysis.specify.question7c`, prereg_answers$`84-118`), collapse = " ")
          prereg_schema$reliability_criteria <- paste(c(prereg_answers$`recommended-analysis.specify.question8c`, prereg_answers$`84-120`), collapse = " ")
          prereg_schema$transformations <- paste(c(prereg_answers$`recommended-analysis.specify.question9c`, prereg_answers$`84-122`), collapse = " ")
          prereg_schema$assumptions_and_contingencies <- paste(c(prereg_answers$`recommended-analysis.specify.question10c`, prereg_answers$`84-124`), collapse = " ")
          prereg_schema$data_collection_started <- paste(c(prereg_answers$`datacompletion`, prereg_answers$`84-130`), collapse = " ")
          prereg_schema$data_looked <- paste(c(prereg_answers$`looked`, prereg_answers$`84-134`), collapse = " ")
          prereg_schema$project_dates_start_end <- paste(c(prereg_answers$`dataCollectionDates`, prereg_answers$`84-138`), collapse = " ")
          prereg_schema$additional_comments <- paste(c(prereg_answers$`additionalComments`, prereg_answers$`84-140`), collapse = " ")
        } else if (template == "OSF Preregistration" & ncol(registration_content$data$attributes$registration_responses) == 28) {
          prereg_schema$template_name <- template
          prereg_schema$id <- registration_content$data$id
          prereg_schema$link <- paste0("https://osf.io/",registration_content$data$id)
          prereg_schema$date_created <- registration_content$data$attributes$date_created
          prereg_schema$date_modified <- registration_content$data$attributes$date_modified
          prereg_schema$date_registered <- registration_content$data$attributes$date_registered
          prereg_schema$embargo_end_date <- registration_content$data$attributes$embargo_end_date
          prereg_schema$ia_url <- registration_content$data$attributes$ia_url
          prereg_schema$title <- registration_content$data$attributes$title
          prereg_schema$description <- prereg_answers$q2
          #prereg_schema$research_questions <- prereg_answers$q3
          prereg_schema$study_type <- prereg_answers$q3
          prereg_schema$blinding <- paste(c(prereg_answers$q4, prereg_answers$q5), collapse = " ")
          prereg_schema$study_design_overview <- prereg_answers$q6.question
          #prereg_schema$randomization <- prereg_answers$q9
          prereg_schema$data_collection_started <- prereg_answers$q8
          prereg_schema$existing_data_explanation <- prereg_answers$q9
          prereg_schema$data_collection_procedures <- prereg_answers$q10.question
          prereg_schema$sample_size <- prereg_answers$q11
          prereg_schema$sample_size_rationale <- prereg_answers$q12
          prereg_schema$stopping_rule <- prereg_answers$q13
          prereg_schema$design_independent_variables <- prereg_answers$q14.question
          prereg_schema$design_dependent_variables <- prereg_answers$q15.question
          prereg_schema$indices <- prereg_answers$q16.question
          prereg_schema$statistical_tests <- prereg_answers$q17.question
          #prereg_schema$statistical_tests <- prereg_answers$q20
          prereg_schema$inference_criteria <- prereg_answers$q19
          prereg_schema$data_exclusion_criteria <- prereg_answers$q20
          prereg_schema$outliers_and_exclusions <- prereg_answers$q21
          prereg_schema$exploratory_analyses  <- prereg_answers$q22
          prereg_schema$additional_comments <- prereg_answers$q23
        } else if (template == "OSF Preregistration"  & ncol(registration_content$data$attributes$registration_responses) == 31) {
          prereg_schema$template_name <- template
          prereg_schema$id <- registration_content$data$id
          prereg_schema$link <- paste0("https://osf.io/",registration_content$data$id)
          prereg_schema$date_created <- registration_content$data$attributes$date_created
          prereg_schema$date_modified <- registration_content$data$attributes$date_modified
          prereg_schema$date_registered <- registration_content$data$attributes$date_registered
          prereg_schema$embargo_end_date <- registration_content$data$attributes$embargo_end_date
          prereg_schema$ia_url <- registration_content$data$attributes$ia_url
          prereg_schema$title <- registration_content$data$attributes$title
          prereg_schema$authors <- prereg_answers$q2
          prereg_schema$description <- prereg_answers$q3
          prereg_schema$research_questions <- prereg_answers$q4
          prereg_schema$study_type <- prereg_answers$q5
          prereg_schema$blinding <- paste(c(prereg_answers$q6, prereg_answers$q7), collapse = " ")
          prereg_schema$study_design_overview <- prereg_answers$q8.question
          prereg_schema$randomization <- prereg_answers$q9
          prereg_schema$data_collection_started <- prereg_answers$q10
          prereg_schema$existing_data_explanation <- prereg_answers$q11
          prereg_schema$data_collection_procedures <- prereg_answers$q12.question
          prereg_schema$sample_size <- prereg_answers$q13
          prereg_schema$sample_size_rationale <- prereg_answers$q14
          prereg_schema$stopping_rule <- prereg_answers$q15
          prereg_schema$design_independent_variables <- prereg_answers$q16.question
          prereg_schema$design_dependent_variables <- prereg_answers$q17.question
          prereg_schema$indices <- prereg_answers$q18.question
          prereg_schema$statistical_tests <- prereg_answers$q19.question
          prereg_schema$transformations <- prereg_answers$q20
          prereg_schema$inference_criteria <- prereg_answers$q21
          prereg_schema$data_exclusion_criteria <- prereg_answers$q22
          prereg_schema$outliers_and_exclusions <- prereg_answers$q23
          prereg_schema$exploratory_analyses  <- prereg_answers$q24
          prereg_schema$additional_comments <- prereg_answers$q25
        } else if (template == "Replication Recipe (Brandt et al., 2013): Pre-Registration") {
          prereg_schema$template_name <- registration_content$data$attributes$registration_supplement
          prereg_schema$id <- registration_content$data$id
          prereg_schema$link <- paste0("https://osf.io/",registration_content$data$id)
          prereg_schema$date_created <- registration_content$data$attributes$date_created
          prereg_schema$date_modified <- registration_content$data$attributes$date_modified
          prereg_schema$date_registered <- registration_content$data$attributes$date_registered
          prereg_schema$embargo_end_date <- registration_content$data$attributes$embargo_end_date
          prereg_schema$ia_url <- registration_content$data$attributes$ia_url
          prereg_schema$title <- registration_content$data$attributes$title
          prereg_schema$description <- prereg_answers$item1
          prereg_schema$replication_importance <- prereg_answers$item2
          prereg_schema$effect_size_original <- prereg_answers$item3
          prereg_schema$confidence_interval_original <- prereg_answers$item4
          prereg_schema$original_sample_size <- prereg_answers$item5
          prereg_schema$original_study_conducted <- prereg_answers$item6
          prereg_schema$region <- prereg_answers$item7
          prereg_schema$original_population <- prereg_answers$item8
          prereg_schema$original_data_collection <- prereg_answers$item9
          prereg_schema$original_materials_available <- prereg_answers$item10
          prereg_schema$assumptions_and_contingencies <- prereg_answers$item11
          prereg_schema$data_collection_location <- prereg_answers$item12
          prereg_schema$blinding <- paste(prereg_answers$item13, prereg_answers$item14)
          prereg_schema$sample_size <- prereg_answers$item15
          prereg_schema$sample_size_rationale <- prereg_answers$item16
          prereg_schema$instruction_similarities <- prereg_answers$item17
          prereg_schema$measure_similarities <- prereg_answers$item18
          prereg_schema$stimuli_similarities <- prereg_answers$item19
          prereg_schema$procedure_similarities <- prereg_answers$item20
          prereg_schema$location_similarities <- prereg_answers$item21
          prereg_schema$remuneration_similarities <- prereg_answers$item22
          prereg_schema$participant_similarities <- prereg_answers$item23
          prereg_schema$differences_influencing_effects <- paste(prereg_answers$item24, prereg_answers$item25)
          prereg_schema$data_exclusion_criteria  <- prereg_answers$item26
          prereg_schema$statistical_tests <- prereg_answers$item27
          prereg_schema$inference_criteria <- prereg_answers$item28
        } else if (template == "OSF-Standard Pre-Data Collection Registration") {
          prereg_schema$template_name <- registration_content$data$attributes$registration_supplement
          prereg_schema$id <- registration_content$data$id
          prereg_schema$link <- paste0("https://osf.io/",registration_content$data$id)
          prereg_schema$date_created <- registration_content$data$attributes$date_created
          prereg_schema$date_modified <- registration_content$data$attributes$date_modified
          prereg_schema$date_registered <- registration_content$data$attributes$date_registered
          prereg_schema$embargo_end_date <- registration_content$data$attributes$embargo_end_date
          prereg_schema$ia_url <- registration_content$data$attributes$ia_url
          prereg_schema$title <- registration_content$data$attributes$title
          prereg_schema$data_collection_started <- prereg_answers$datacompletion
          prereg_schema$data_looked <- prereg_answers$looked
          prereg_schema$additional_comments <- prereg_answers$comments
        }
        preregistration_information <- bind_rows(preregistration_information, prereg_schema)   # append row
      }
    }
  }

  # traffic light ----
  tl <- "info"

  guidance <- paste0(
    "For metascientific articles demonstrating the rate of deviationsfrom preregistrations, see:<br><br>",
    "van den Akker, O. R., Bakker, M., van Assen, M. A. L. M., Pennington, C. R., Verweij, L., Elsherif, M. M., Claesen, A., Gaillard, S. D. M., Yeung, S. K., Frankenberger, J.-L., Krautter, K., Cockcroft, J. P., Kreuer, K. S., Evans, T. R., Heppel, F. M., Schoch, S. F., Korbmacher, M., Yamada, Y., Albayrak-Aydemir, N., … Wicherts, J. M. (2024). The potential of preregistration in psychology: Assessing preregistration producibility and preregistration-study consistency. Psychological Methods. ",
    "<a href='https://doi.org/10.1037/met0000687' target='_blank'>https://doi.org/10.1037/met0000687</a> <br>",
    "For educational material on how to report deviations from preregistrations, see:<br>",
    "Lakens, D. (2024). When and How to Deviate From a Preregistration. Collabra: Psychology, 10(1), 117094. ",
    "<a href='https://doi.org/10.1525/collabra.117094' target='_blank'>https://doi.org/10.1525/collabra.117094</a> <br><br> #### Full preregistration:"
  )

  if (nrow(preregistration_information) == 0) {
    tl <- "na"
    summary_text <- "We detected no links to preregistrations."
    # Create the data frame directly
    summary_table <- data.frame(
      id = paper$id,
      preregistration = 0L,
      stringsAsFactors = FALSE
    )
    report <- summary_text

  } else {
    tl <- "info"
    module_output <- sprintf(
      "We found %d preregistration(s).\n\nMeta-scientific research has shown that deviations from preregistrations are often not reported or checked, and that the most common deviations concern the sample size. We recommend manually checking the full preregistration at the link(s) below, and have provided the preregistered sample size.",
      nrow(preregistration_information))
    download_col <- sprintf('<a href="%s">link to preregistration</a>', preregistration_information$link)
    prereg_link_table <- data.frame(
      id = preregistration_information$id,
      download = download_col,
      stringsAsFactors = FALSE
    )
    summary_text <- sprintf(
      "We found %d preregistration(s).",
      nrow(preregistration_information)
    )
    issues_found <- scroll_table(preregistration_information$sample_size)
    # summary output for paperlists ----
    summary_table <- dplyr::count(preregistration_information, id, name = "preregistration", .drop = FALSE)
    # Remove columns where all values are NA
    prereg_table <- preregistration_information[
      , colSums(!is.na(preregistration_information)) > 0
    ]
    prereg_table <- t(prereg_table)
    prereg_table <- as.data.frame(prereg_table, stringsAsFactors = FALSE)

    # Add row names as a proper column (first column)
    prereg_table <- cbind(Field = rownames(prereg_table), prereg_table)

    # Rename columns "Preregistration 1", "Preregistration 2", ...
    n_prereg <- ncol(prereg_table) - 1  # subtract the 'Field' column
    colnames(prereg_table)[-1] <- paste0("Preregistration ", seq_len(n_prereg))

    prereg_table <- scroll_table(prereg_table)

    report <- c(module_output, scroll_table(prereg_link_table), "#### Preregistered Sample Size", issues_found, collapse_section(prereg_table, "Full Preregistration"), collapse_section(guidance))

  }

  # return a list ----
  list(
    table = preregistration_information,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}
