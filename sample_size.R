# ==============================================================================
# sample_size_extractor
# ==============================================================================

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(stringr)
library(tidyr)

# Source R functions for now
files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(files, source))

# Configure LLM
llm_model("ollama/gpt-oss:20b")
llm_use(TRUE)
llm_max_calls(10000)

# ==============================================================================
# Helper Functions
# ==============================================================================

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

#' Extract method text from paper
extract_method_text <- function(paper) {  
  text_by_section <- paper$full_text %>%
    group_by(section) %>%
    summarise(text = paste(text, collapse = " "), .groups = "drop")
  
  method_text <- text_by_section$text[text_by_section$section == "method"]
  
  if(length(method_text) == 0) {
    warning(sprintf("No method section found in paper %s", paper$id))
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
#' regex like implementation to extract sentences containing sample size information.
#' 

prepare_fsample_llm_input <- function(
  paper,
  max_sentences = 8,
  sections = c("method", "methods", "participants")
) {
  # --- configuration ---
  positive_anchors <- c(
    "aimed", "planned", "target", "recruit",
    "collected", "stopped", "until",
    "participants", "participant","subjects", "individuals", "N =", "n =", "N=","n=",
    "women", "men", "students", "adults", "children", "female", "male", "girls", "boys",
    "sample size", "sample", "group size", "condition size", "per condition", "per group",
    "excluded", "final sample", "after excluding", "after removing"
  )

  #numeric_pattern <- "\\b\\d{2,5}\\b"

  # --- search candidate sentences ---
  candidates <- search_text(
    paper,
    section = sections,
    pattern = str_c(positive_anchors, collapse = "|"),
    return = "sentence"
  )

  if (nrow(candidates) == 0) {
    return(list(
      has_candidates = FALSE,
      reason = "no_anchor_matches",
      sentences = NULL
    ))
  }
  # --- require numeric content ---
  # DISABLED -> Soemtimes people write "twenty participants" instead of "20 participants". Stupid but maybe there is a regex implementation for this.
  # candidates <- candidates %>%
  #   filter(str_detect(text, numeric_pattern))

  # if (nrow(candidates) == 0) {
  #   return(list(
  #     has_candidates = FALSE,
  #     reason = "no_numeric_sentences",
  #     sentences = NULL
  #   ))
  # }

  llm_text <- paste(candidates$text, collapse = " ")

  return(list(
    has_candidates = TRUE,
    n_sentences = nrow(candidates),
    llm_text = llm_text,
    debug = candidates
  ))
}

# ==============================================================================
# Main Pipeline Function
# ==============================================================================

# extract sample size goal.

# xml <- "/Users/levibaruch/dev/metacheck/data-raw/psychsci/grobid_0.8.2-full/0956797616631990.xml"
# paper <- read(xml)
# temperature <- 0.3
# top_p <- 0.9
# think <- "low"
# n_iterations <- 1
# output_dir <- "output/sample_size_llm"
# dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# # Extract method text
# paper_data <- tryCatch(
#   extract_method_text(paper),
#   error = function(e) {
#     warning(sprintf("Failed to read %s: %s", paper$id, e$message))
#     return(NULL)
#   }
# )
    
# if(is.null(paper_data) || is.na(paper_data$method_text)) {
#   message("  Skipping - no method text found")
#   next
# }

# # Define some variables    
# paper_id <- paper_data$paper_id
# paper_title <- paper_data$paper_title
# method_text <- paper_data$method_text
    
# # Replicate the method text for n_iterations to test stability. For now, we will not do this.
# #texts <- rep(method_text, n_iterations)
# texts <- method_text
# output_methods <- prepare_fsample_llm_input(paper)

# approx_token_length <- function(x) {
#   ceiling(nchar(x) / 4)
# }

# compare_lengths <- function(a, b) {
#   tibble::tibble(
#     string = c("a", "b"),
#     characters = c(nchar(a), nchar(b)),
#     approx_tokens = c(
#       approx_token_length(a),
#       approx_token_length(b)
#     )
#   )
# }

# differences <- compare_lengths(texts, output_methods$llm_text)
# print(differences)




# # Run LLM-
# message(sprintf("Running LLM for paper %s", paper_id))

# results <- tryCatch({
#   llm(
#     text = texts,
#     system_prompt = get_classification_prompt(),
#     deduplicate = FALSE,
#     params = list(
#       temperature = temperature,
#       top_p = top_p,
#       think = think
#     )
#   )
# }, error = function(e) {
#   warning(sprintf("LLM call failed for %s: %s", paper_id, e$message))
#   return(NULL)
# })
    
# if(is.null(results)) {
#   message("  LLM call failed, skipping paper")
#   next
# }
    
# # Add metadata to the results for reproducability
# colnames(results)[1:2] <- c("prompt", "result")
# results$paper_id <- paper_id
# results$iteration <- 1:n_iterations
# results$thinking_style <- think
# results$temperature <- temperature
# results$top_p <- top_p
# results$paper_title <- paper_title

# # Remove the idioitc duplicates that sopehow keep spawning.
# results <- results %>% distinct()

# # Save raw results for this paper
# paper_log_file <- file.path(output_dir, 
#                             sprintf("%s_%s_raw.csv", paper_title, paper_id))
# write_csv(results, paper_log_file, append = FALSE)
# message(sprintf("  Saved raw results to: %s", paper_log_file))

# # Parse JSON output
# parsed_results <- tryCatch({
#   parsed_file <- file.path(output_dir, sprintf("%s_parsed.csv", paper_id))
#   parse_llm_json(paper_log_file, out_file = parsed_file)
# }, error = function(e) {
#   warning(sprintf("JSON parsing failed for %s: %s", paper_id, e$message))
#   return(NULL)
# })

# if(!is.null(parsed_results)) {
#   message(sprintf("  Parsed %d rows", nrow(parsed_results)))
# }

# # Write parsed results

# paper_log_file_parsed <- file.path(output_dir, 
#                             sprintf("%s_%s_parsed.csv", paper_title, paper_id))
# write_csv(parsed_results, paper_log_file_parsed, append = FALSE)
# message(sprintf("  Saved parsed results to: %s", paper_log_file_parsed))