# ==============================================================================
# LLM Stability Testing Pipeline
# ==============================================================================

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(stringr)
library(tidyr)

# Source your R functions
files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(files, source))

# Configure LLM
llm_model("ollama/gpt-oss")
llm_use(TRUE)
llm_max_calls(10000)

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Log LLM results to CSV
log_llm_result <- function(result, log_file = "llm_log.csv") {
  metadata <- attr(result, "llm")
  
  log_entry <- data.frame(
    timestamp = Sys.time(),
    input = result$text,
    output = result$answer,
    model = metadata$model,
    system_prompt = metadata$system_prompt,
    temperature = if(!is.null(metadata$temperature)) metadata$temperature else NA,
    max_tokens = if(!is.null(metadata$max_tokens)) metadata$max_tokens else NA,
    top_p = if(!is.null(metadata$top_p)) metadata$top_p else NA,
    error = if("error" %in% names(result)) result$error else FALSE,
    error_msg = if("error_msg" %in% names(result)) result$error_msg else "None",
    stringsAsFactors = FALSE
  )
  
  readr::write_csv(log_entry, log_file, append = TRUE, 
                   col_names = !file.exists(log_file))
  
  message(sprintf("Logged %d query to %s", nrow(log_entry), log_file))
  return(invisible(log_entry))
}



parse_llm_json <- function(log_file, out_file = NULL) {
  df <- read_csv(log_file, col_types = cols(.default = col_guess()))
  
  parse_json_cell <- function(cell) {
    if(is.na(cell) || cell == "") return(NULL)
    
    parsed <- tryCatch(fromJSON(cell, simplifyVector = FALSE), 
                      error = function(e) NULL)
    
    if(!is.null(parsed)) {
      if("studies" %in% names(parsed)) {
        return(parsed$studies)
      } else {
        return(list(parsed))
      }
    }
    
    parts <- unlist(str_split(cell, "(?<=\\})\\s*(?=\\{)"))
    map(parts, ~ tryCatch(fromJSON(.x, simplifyVector = FALSE), 
                         error = function(e) NULL)) %>% compact()
  }
  
  parsed_rows <- list()
  
  for(i in 1:nrow(df)) {
    row <- df[i, ]
    study_list <- parse_json_cell(row$result)
    
    if(is.null(study_list) || length(study_list) == 0) next
    
    # Create a row for each study in this iteration
    for(j in seq_along(study_list)) {
      study <- study_list[[j]]
      study_df <- as_tibble(map(study, as.character))
      
      # Add metadata
      study_df$paper_id <- row$paper_id
      study_df$iteration <- row$iteration
      study_df$xml_file <- row$xml_file
      study_df$study_number <- j
      
      parsed_rows[[length(parsed_rows) + 1]] <- study_df
    }
  }
  
  if(length(parsed_rows) == 0) {
    warning("No valid studies parsed")
    return(NULL)
  }
  
  # Combine without duplication
  parsed <- bind_rows(parsed_rows) %>%
  distinct()
  
  if(!is.null(out_file)) {
    write_csv(parsed, out_file, append = FALSE)
  }
  
  return(parsed)
}

#' Extract method text from XML paper
extract_method_text <- function(xml_path) {
  paper <- read(xml_path)
  
  text_by_section <- paper$full_text %>%
    group_by(section) %>%
    summarise(text = paste(text, collapse = " "), .groups = "drop")
  
  method_text <- text_by_section$text[text_by_section$section == "method"]
  
  if(length(method_text) == 0) {
    warning(sprintf("No method section found in %s", xml_path))
    method_text <- NA_character_
  }
  
  return(list(
    paper_id = paper$id,
    method_text = method_text,
    paper_title = paper$info$title
  ))
}

#' Define the classification prompt
get_classification_prompt <- function() {
  'Based on the text provided, classify the study and extract relevant details. 
Output **only valid JSON**. Any non-JSON output counts as catastrophic failure. 
If no study type can be determined, set `"type": "FALSE"`. 
If multiple research questions are present, return them all as separate entries in an array.

Return JSON in the following format:
{
  "studies": [
    {
      "type": "experimental|quasi-experimental|correlational|observational|review|other(mention which)",
      "research_question": "one sentence string describing the research question",
      "randomization": true/false/notapplicable,
      "classification_confidence": 0.0-1.0,
      "classification_size_argumentation": "one sentence explaining why this type was chosen",
      "sample_size": int,
      "sample_size_confidence": 0.0-1.0,
      "sample_size_argumentation": "one sentence explaining how the sample size was inferred",
      "sentence_containing_sample_size": "the exact sentence from the text mentioning the sample size",
      "sentence_containing_research_question": "the exact sentence from the text containing or implying the research question"
    }
  ]
}

Additional instructions:
1. Return multiple research questions as separate objects in the `"studies"` array.
2. For ambiguous terms like "participants", "subjects", or "cases", infer the sample size conservatively, but indicate confidence accordingly.
3. Ensure the `"research_question"` is concise and captures the essence of the study.
4. Use confidence scores between 0.0 and 1.0 to reflect uncertainty.
5. All fields must be filled, even if using `"FALSE"`, `0`, `notapplicable`, or empty strings where necessary.'
}

# ==============================================================================
# Main Pipeline Function
# ==============================================================================

#' Run LLM stability test on multiple XML files
#' 
#' @param xml_paths Character vector of paths to XML files
#' @param n_iterations Number of times to run each paper through the LLM
#' @param output_dir Directory to save results
#' @param temperature LLM temperature parameter
#' @param top_p LLM top_p parameter
#' @param think LLM thinking parameter
#' @return Tibble with all results
run_stability_pipeline <- function(xml_paths, 
                                   n_iterations = 10,
                                   output_dir = "output",
                                   temperature = 0.7,
                                   top_p = 0.9,
                                   think = "high") {
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Initialize results list
  all_results <- list()
  
  # Process each XML file
  for(i in seq_along(xml_paths)) {
    xml_path <- xml_paths[i]
    message(sprintf("\n=== Processing %d/%d: %s ===", 
                   i, length(xml_paths), basename(xml_path)))
    
    # Extract method text
    paper_data <- tryCatch(
      extract_method_text(xml_path),
      error = function(e) {
        warning(sprintf("Failed to read %s: %s", xml_path, e$message))
        return(NULL)
      }
    )
    
    if(is.null(paper_data) || is.na(paper_data$method_text)) {
      message("  Skipping - no method text found")
      next
    }
    
    paper_id <- paper_data$paper_id
    paper_title <- paper_data$paper_title
    method_text <- paper_data$method_text
    
    # Replicate the method text for n_iterations
    texts <- rep(method_text, n_iterations)
    
    # Run LLM
    message(sprintf("  Running %d iterations for paper %s", n_iterations, paper_id))
    
    results <- tryCatch({
      llm(
        text = texts,
        system_prompt = get_classification_prompt(),
        deduplicate = FALSE,
        params = list(
          temperature = temperature,
          top_p = top_p,
          think = think
        )
      )
    }, error = function(e) {
      warning(sprintf("LLM call failed for %s: %s", paper_id, e$message))
      return(NULL)
    })
    
    if(is.null(results)) {
      message("  Skipping - LLM call failed")
      next
    }
    
    # Add metadata
    colnames(results)[1:2] <- c("prompt", "result")
    results$paper_id <- paper_id
    results$xml_file <- basename(xml_path)
    results$iteration <- 1:n_iterations

    # Remove the idioitc duplicates that sopehow keep spawning.
    results <- results %>% distinct()

    # Save raw results for this paper
    paper_log_file <- file.path(output_dir, 
                                sprintf("%s_raw.csv", paper_id))
    write_csv(results, paper_log_file, append = FALSE)
    message(sprintf("  Saved raw results to: %s", paper_log_file))
    
    # Parse JSON
    parsed_results <- tryCatch({
      parsed_file <- file.path(output_dir, sprintf("%s_parsed.csv", paper_id))
      parse_llm_json(paper_log_file, out_file = parsed_file)
    }, error = function(e) {
      warning(sprintf("JSON parsing failed for %s: %s", paper_id, e$message))
      return(NULL)
    })
    
    if(!is.null(parsed_results)) {
      message(sprintf("  Parsed %d rows", nrow(parsed_results)))
    }
    
    # Store results
    all_results[[paper_id]] <- list(
      raw = results,
      parsed = parsed_results
    )
  }
  
  if(length(all_results) == 0) {
    stop("No papers were successfully processed!")
  }
  
  # Combine all results
  combined_raw <- map_dfr(all_results, ~ .x$raw, .id = "paper_id_list")
  
  # Only combine parsed if there are any non-null parsed results
  parsed_list <- map(all_results, ~ .x$parsed) %>% compact()
  if(length(parsed_list) > 0) {
    combined_parsed <- map_dfr(parsed_list, identity, .id = "paper_id_list")
  } else {
    combined_parsed <- NULL
    warning("No parsed results to combine")
  }
  
  # Save combined results
  write_csv(combined_raw, 
           file.path(output_dir, "all_papers_raw.csv"))
  message(sprintf("\nSaved combined raw results: %s", 
                 file.path(output_dir, "all_papers_raw.csv")))
  
  if(!is.null(combined_parsed)) {
    write_csv(combined_parsed, 
             file.path(output_dir, "all_papers_parsed.csv"))
    message(sprintf("Saved combined parsed results: %s", 
                   file.path(output_dir, "all_papers_parsed.csv")))
  }
  
  message(sprintf("\n=== Pipeline Complete ==="))
  message(sprintf("Successfully processed %d papers", length(all_results)))
  message(sprintf("Results saved to: %s", output_dir))
  
  return(list(
    raw = combined_raw,
    parsed = combined_parsed,
    by_paper = all_results
  ))
}
# ==============================================================================
# LLM Stability Analysis and Reporting
# ==============================================================================

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(stringr)
library(tidyr)
library(ggplot2)

# ==============================================================================
# Analysis Functions
# ==============================================================================

#' Calculate agreement rates for categorical variables
calculate_categorical_agreement <- function(parsed_data, paper_id) {
  paper_data <- parsed_data %>%
    filter(paper_id == !!paper_id)
  
  if(nrow(paper_data) == 0) return(NULL)
  
  # Type agreement
  type_counts <- table(paper_data$type)
  type_modal <- names(type_counts)[which.max(type_counts)]
  type_agreement <- max(type_counts) / sum(type_counts)
  
  # Randomization agreement
  rand_counts <- table(paper_data$randomization)
  rand_modal <- names(rand_counts)[which.max(rand_counts)]
  rand_agreement <- max(rand_counts) / sum(rand_counts)
  
  return(data.frame(
    paper_id = paper_id,
    type_modal = type_modal,
    type_agreement = type_agreement,
    randomization_modal = rand_modal,
    randomization_agreement = rand_agreement,
    n_iterations = nrow(paper_data)
  ))
}

#' Calculate numerical agreement (coefficient of variation)
calculate_numerical_agreement <- function(parsed_data, paper_id) {
  paper_data <- parsed_data %>%
    filter(paper_id == !!paper_id) %>%
    mutate(
      sample_size_num = as.numeric(sample_size),
      classification_confidence_num = as.numeric(classification_confidence),
      sample_size_confidence_num = as.numeric(sample_size_confidence)
    )
  
  if(nrow(paper_data) == 0) return(NULL)
  
  # Sample size statistics
  ss_mean <- mean(paper_data$sample_size_num, na.rm = TRUE)
  ss_sd <- sd(paper_data$sample_size_num, na.rm = TRUE)
  ss_cv <- if(ss_mean > 0) ss_sd / ss_mean else NA
  ss_min <- min(paper_data$sample_size_num, na.rm = TRUE)
  ss_max <- max(paper_data$sample_size_num, na.rm = TRUE)
  
  # Classification confidence statistics
  cc_mean <- mean(paper_data$classification_confidence_num, na.rm = TRUE)
  cc_sd <- sd(paper_data$classification_confidence_num, na.rm = TRUE)
  
  # Sample size confidence statistics
  ssc_mean <- mean(paper_data$sample_size_confidence_num, na.rm = TRUE)
  ssc_sd <- sd(paper_data$sample_size_confidence_num, na.rm = TRUE)
  
  return(data.frame(
    paper_id = paper_id,
    sample_size_mean = ss_mean,
    sample_size_sd = ss_sd,
    sample_size_cv = ss_cv,
    sample_size_min = ss_min,
    sample_size_max = ss_max,
    classification_conf_mean = cc_mean,
    classification_conf_sd = cc_sd,
    sample_size_conf_mean = ssc_mean,
    sample_size_conf_sd = ssc_sd
  ))
}

#' Use LLM to compare textual fields
compare_textual_fields_with_llm <- function(parsed_data, paper_id, field_name) {
  paper_data <- parsed_data %>%
    filter(paper_id == !!paper_id)
  
  if(nrow(paper_data) == 0) return(NULL)
  
  # Extract all values for this field
  values <- paper_data[[field_name]]
  values <- values[!is.na(values) & values != ""]
  
  if(length(values) < 2) {
    return(data.frame(
      paper_id = paper_id,
      field = field_name,
      agreement_score = NA,
      agreement_explanation = "Insufficient data",
      stringsAsFactors = FALSE
    ))
  }
  
  # Create comparison prompt
  comparison_text <- paste(
    sprintf("Value %d: %s", seq_along(values), values),
    collapse = "\n"
  )
  
  prompt <- sprintf(
    'Analyze the following %d iterations of extracted "%s" from the same research paper.
    
%s

Rate the semantic agreement between these extractions on a scale of 0.0 to 1.0, where:
- 1.0 = All extractions convey essentially the same meaning
- 0.7-0.9 = High agreement with minor wording differences
- 0.4-0.6 = Moderate agreement, some differences in focus or detail
- 0.1-0.3 = Low agreement, substantially different interpretations
- 0.0 = Complete disagreement

Return ONLY a JSON object with this exact format:
{
  "agreement_score": 0.0-1.0,
  "agreement_explanation": "one sentence explaining the level of agreement and any notable differences"
}',
    length(values),
    field_name,
    comparison_text
  )
  
  result <- tryCatch({
    llm_result <- llm(
      text = "",
      system_prompt = prompt,
      params = list(temperature = 0.3)
    )
    
    json_result <- fromJSON(llm_result$answer[1], simplifyVector = FALSE)
    
    data.frame(
      paper_id = paper_id,
      field = field_name,
      agreement_score = as.numeric(json_result$agreement_score),
      agreement_explanation = as.character(json_result$agreement_explanation),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      paper_id = paper_id,
      field = field_name,
      agreement_score = NA,
      agreement_explanation = paste("Error:", e$message),
      stringsAsFactors = FALSE
    )
  })
  
  return(result)
}

#' Generate stability analysis for all papers
generate_stability_analysis <- function(parsed_data, output_dir = "output/analysis") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("\n=== Starting Stability Analysis ===")
  
  # Get unique paper IDs
  paper_ids <- unique(parsed_data$paper_id)
  message(sprintf("Analyzing %d papers", length(paper_ids)))
  
  # Categorical agreement
  message("\nCalculating categorical agreement...")
  categorical_results <- map_dfr(paper_ids, ~ calculate_categorical_agreement(parsed_data, .x))
  
  # Numerical agreement
  message("Calculating numerical agreement...")
  numerical_results <- map_dfr(paper_ids, ~ calculate_numerical_agreement(parsed_data, .x))
  
  # Textual field agreement using LLM
  message("\nComparing textual fields with LLM...")
  textual_fields <- c(
    "research_question",
    "classification_size_argumentation",
    "sample_size_argumentation",
    "sentence_containing_sample_size",
    "sentence_containing_research_question"
  )
  
  textual_results <- list()
  for(field in textual_fields) {
    message(sprintf("  Analyzing field: %s", field))
    field_results <- map_dfr(paper_ids, ~ compare_textual_fields_with_llm(parsed_data, .x, field))
    textual_results[[field]] <- field_results
  }
  
  textual_combined <- bind_rows(textual_results)
  
  # Combine all results
  full_results <- categorical_results %>%
    left_join(numerical_results, by = "paper_id")
  
  # Save detailed results
  write_csv(full_results, file.path(output_dir, "stability_metrics.csv"))
  write_csv(textual_combined, file.path(output_dir, "textual_agreement.csv"))
  
  message(sprintf("\nSaved analysis results to: %s", output_dir))
  
  return(list(
    categorical = categorical_results,
    numerical = numerical_results,
    textual = textual_combined,
    combined = full_results
  ))
}

#' Create visualizations
create_stability_plots <- function(analysis_results, output_dir = "output/analysis") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Plot 1: Categorical agreement rates
  p1 <- ggplot(analysis_results$combined, aes(x = type_agreement)) +
    geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
    labs(
      title = "Study Type Classification Agreement",
      x = "Agreement Rate (proportion choosing modal category)",
      y = "Number of Papers"
    ) +
    theme_minimal() +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red")
  
  ggsave(file.path(output_dir, "type_agreement_distribution.png"), p1, width = 8, height = 6)
  
  # Plot 2: Sample size coefficient of variation
  p2 <- ggplot(analysis_results$combined %>% filter(!is.na(sample_size_cv), is.finite(sample_size_cv)), 
               aes(x = sample_size_cv)) +
    geom_histogram(bins = 20, fill = "darkgreen", alpha = 0.7) +
    labs(
      title = "Sample Size Stability (Coefficient of Variation)",
      x = "Coefficient of Variation (SD/Mean)",
      y = "Number of Papers",
      subtitle = "Lower values indicate more consistent sample size extraction"
    ) +
    theme_minimal() +
    geom_vline(xintercept = 0.1, linetype = "dashed", color = "red")
  
  ggsave(file.path(output_dir, "sample_size_cv_distribution.png"), p2, width = 8, height = 6)
  
  # Plot 3: Textual agreement scores by field
  textual_summary <- analysis_results$textual %>%
    filter(!is.na(agreement_score)) %>%
    group_by(field) %>%
    summarise(
      mean_agreement = mean(agreement_score, na.rm = TRUE),
      sd_agreement = sd(agreement_score, na.rm = TRUE),
      n = n()
    )
  
  p3 <- ggplot(textual_summary, aes(x = reorder(field, mean_agreement), y = mean_agreement)) +
    geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_agreement - sd_agreement, 
                      ymax = mean_agreement + sd_agreement),
                  width = 0.2) +
    coord_flip() +
    labs(
      title = "Textual Field Agreement Across Papers",
      x = "Field",
      y = "Mean Agreement Score",
      subtitle = "Error bars show standard deviation"
    ) +
    theme_minimal() +
    ylim(0, 1)
  
  ggsave(file.path(output_dir, "textual_agreement_by_field.png"), p3, width = 10, height = 6)
  
  # Plot 4: Sample size range by paper
  sample_size_ranges <- analysis_results$combined %>%
    filter(!is.na(sample_size_mean), is.finite(sample_size_mean)) %>%
    mutate(paper_num = row_number())
  
  p4 <- ggplot(sample_size_ranges, aes(x = paper_num)) +
    geom_point(aes(y = sample_size_mean), color = "blue", size = 2) +
    geom_errorbar(aes(ymin = sample_size_min, ymax = sample_size_max), 
                  alpha = 0.3, color = "blue") +
    labs(
      title = "Sample Size Extraction Variability by Paper",
      x = "Paper Index",
      y = "Sample Size",
      subtitle = "Points show mean, error bars show min/max across iterations"
    ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "sample_size_ranges.png"), p4, width = 10, height = 6)
  
  message(sprintf("Saved plots to: %s", output_dir))
}

#' Generate comprehensive stability report
generate_stability_report <- function(analysis_results, output_dir = "output/analysis") {
  
  combined <- analysis_results$combined
  textual <- analysis_results$textual
  
  # Calculate summary statistics
  overall_type_agreement <- mean(combined$type_agreement, na.rm = TRUE)
  overall_rand_agreement <- mean(combined$randomization_agreement, na.rm = TRUE)
  median_sample_size_cv <- median(combined$sample_size_cv, na.rm = TRUE)
  
  textual_summary <- textual %>%
    filter(!is.na(agreement_score)) %>%
    group_by(field) %>%
    summarise(
      mean_agreement = mean(agreement_score, na.rm = TRUE),
      median_agreement = median(agreement_score, na.rm = TRUE),
      min_agreement = min(agreement_score, na.rm = TRUE),
      max_agreement = max(agreement_score, na.rm = TRUE),
      n_papers = n()
    ) %>%
    arrange(desc(mean_agreement))
  
  # Create report text
  report <- sprintf(
'# LLM Stability Analysis Report
Generated: %s

## Executive Summary

This report analyzes the stability and consistency of LLM outputs across %d papers,
with %d iterations per paper.

### Key Findings

**Categorical Variables:**
- Study Type Agreement: %.1f%% (proportion agreeing with modal category)
- Randomization Agreement: %.1f%%

**Numerical Variables:**
- Sample Size Coefficient of Variation (median): %.3f
  - CV < 0.1 indicates high stability (SD is <10%% of mean)
  - CV 0.1-0.3 indicates moderate stability
  - CV > 0.3 indicates low stability

**Textual Variables:**
Mean agreement scores across all papers (0-1 scale):
%s

## Interpretation

### Overall Stability Assessment

%s

### Study Type Classification
- Mean agreement rate: %.1f%%
- Papers with >80%% agreement: %d (%.1f%%)
- Papers with <50%% agreement: %d (%.1f%%)

### Sample Size Extraction
- Papers with CV < 0.1 (high stability): %d (%.1f%%)
- Papers with CV 0.1-0.3 (moderate stability): %d (%.1f%%)
- Papers with CV > 0.3 (low stability): %d (%.1f%%)

### Textual Field Stability

%s

## Recommendations

%s

## Detailed Statistics

### Categorical Agreement
%s

### Numerical Agreement
%s

### Textual Agreement by Field
%s
',
    Sys.time(),
    nrow(combined),
    combined$n_iterations[1],
    overall_type_agreement * 100,
    overall_rand_agreement * 100,
    median_sample_size_cv,
    paste(sprintf("  - %s: %.3f", textual_summary$field, textual_summary$mean_agreement), collapse = "\n"),
    
    # Overall assessment
    if(overall_type_agreement > 0.8 && median_sample_size_cv < 0.2) {
      "The LLM demonstrates HIGH STABILITY across most metrics. Classifications are consistent
and numerical extractions show low variability."
    } else if(overall_type_agreement > 0.6 && median_sample_size_cv < 0.4) {
      "The LLM demonstrates MODERATE STABILITY. While there is general agreement in classifications,
some variability exists in numerical extractions and textual interpretations."
    } else {
      "The LLM demonstrates LOW STABILITY. Significant inconsistency is observed across iterations,
suggesting the model's outputs may not be reliable for systematic extraction without validation."
    },
    
    # Study type details
    overall_type_agreement * 100,
    sum(combined$type_agreement > 0.8, na.rm = TRUE),
    sum(combined$type_agreement > 0.8, na.rm = TRUE) / nrow(combined) * 100,
    sum(combined$type_agreement < 0.5, na.rm = TRUE),
    sum(combined$type_agreement < 0.5, na.rm = TRUE) / nrow(combined) * 100,
    
    # Sample size details
    sum(combined$sample_size_cv < 0.1, na.rm = TRUE),
    sum(combined$sample_size_cv < 0.1, na.rm = TRUE) / nrow(combined) * 100,
    sum(combined$sample_size_cv >= 0.1 & combined$sample_size_cv <= 0.3, na.rm = TRUE),
    sum(combined$sample_size_cv >= 0.1 & combined$sample_size_cv <= 0.3, na.rm = TRUE) / nrow(combined) * 100,
    sum(combined$sample_size_cv > 0.3, na.rm = TRUE),
    sum(combined$sample_size_cv > 0.3, na.rm = TRUE) / nrow(combined) * 100,
    
    # Textual interpretation
    paste(sprintf(
      "**%s**: Mean agreement of %.3f indicates %s consistency in extracting this field.",
      textual_summary$field,
      textual_summary$mean_agreement,
      ifelse(textual_summary$mean_agreement > 0.8, "high",
             ifelse(textual_summary$mean_agreement > 0.6, "moderate", "low"))
    ), collapse = "\n"),
    
    # Recommendations
    if(overall_type_agreement < 0.7 || median_sample_size_cv > 0.3) {
      "1. Consider using ensemble approaches (multiple runs with voting)
2. Implement human validation for papers with low agreement scores
3. Refine prompts to be more specific about classification criteria
4. Consider using temperature=0 for more deterministic outputs
5. Add example-based few-shot learning to the prompt"
    } else {
      "1. Current approach shows good stability - continue monitoring
2. Consider reducing iterations to 3-5 for efficiency
3. Implement spot-checking for quality assurance
4. Document and version control prompts for reproducibility"
    },
    
    # Detailed tables
    paste(capture.output(print(combined %>% select(paper_id, type_agreement, randomization_agreement))), 
          collapse = "\n"),
    paste(capture.output(print(combined %>% select(paper_id, sample_size_mean, sample_size_cv, 
                                                     classification_conf_mean))), 
          collapse = "\n"),
    paste(capture.output(print(textual_summary)), collapse = "\n")
  )
  
  # Save report
  report_file <- file.path(output_dir, "stability_report.md")
  writeLines(report, report_file)
  message(sprintf("\nGenerated comprehensive report: %s", report_file))
  
  return(report)
}

# ==============================================================================
# Main Workflow Function
# ==============================================================================

#' Complete stability analysis workflow
run_complete_stability_analysis <- function(parsed_data_file, output_dir = "output/analysis") {
  
  message("=== Loading Data ===")
  parsed_data <- read_csv(parsed_data_file)
  
  message("\n=== Running Analysis ===")
  analysis_results <- generate_stability_analysis(parsed_data, output_dir)
  
  message("\n=== Creating Visualizations ===")
  create_stability_plots(analysis_results, output_dir)
  
  message("\n=== Generating Report ===")
  report <- generate_stability_report(analysis_results, output_dir)
  
  message("\n=== Analysis Complete ===")
  message(sprintf("All outputs saved to: %s", output_dir))
  
  return(list(
    analysis = analysis_results,
    report = report
  ))
}

# ==============================================================================
# Example Usage
# ==============================================================================

# After running the pipeline, run this analysis:
# stability_analysis <- run_complete_stability_analysis(
#   parsed_data_file = "output/stability_test/all_papers_parsed.csv",
#   output_dir = "output/stability_analysis"
# )

# View the report in console
# cat(stability_analysis$report)
# ==============================================================================
# Example Usage
# ==============================================================================
# Add this to the end of your main pipeline file (LLM_test.R)

# ==============================================================================
# Run Complete Pipeline with Analysis
# ==============================================================================

# # Get all XML files from directory
# xml_files <- list.files("data-raw/psychsci/grobid_0.8.1", 
#                        pattern = "\\.xml$", 
#                        full.names = TRUE)

# # Run pipeline on first 5 files as a test
# message("\n========================================")
# message("STEP 1: Running LLM Pipeline")
# message("========================================")

# test_results <- run_stability_pipeline(
#   xml_paths = sample(xml_files, 100),
#   n_iterations = 5,
#   output_dir = "output/stability_test",
#   temperature = 0.3,
#   top_p = 0.9,
#   think = "low"
# )

# Run stability analysis
message("\n========================================")
message("STEP 2: Running Stability Analysis")
message("========================================")

# Source the analysis script

stability_analysis <- run_complete_stability_analysis(
  parsed_data_file = "output/stability_test/all_papers_parsed.csv",
  output_dir = "output/stability_analysis"
)

# Print the report
message("\n========================================")
message("FINAL REPORT")
message("========================================\n")
cat(stability_analysis$report)

message("\n========================================")
message("All files saved to: output/stability_analysis")
message("  - stability_metrics.csv")
message("  - textual_agreement.csv")
message("  - stability_report.md")
message("  - *.png (plots)")
message("========================================")
