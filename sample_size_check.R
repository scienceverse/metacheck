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
source("sample_size.R")
verbose(verbose = TRUE)

# ==============================================================================
# Main Pipeline Function
# ==============================================================================
run_and_compare_sample_sizes <- function(paper, 
                                         output_dir = "output/sample_size_comparison",
                                         temperature = 0.3,
                                         top_p = 0.9,
                                         think = "low",
                                         raw_dir = "output/sample_size_comparison/raw") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_file <- file.path(output_dir, "comparison_log.csv")
  
  paper_id <- paper$id
  
  # --- 1. Regex-based LLM input ---
  regex_result <- prepare_fsample_llm_input(paper)
  
  if(regex_result$has_candidates) {
    regex_text <- regex_result$llm_text
  } else {
    regex_text <- ""
  }
  
  # --- 2. Full method text ---
  paper_data <- extract_method_text(paper)
  full_text <- if(!is.na(paper_data$method_text)) paper_data$method_text else ""
  
  # --- 3. Run LLM on both inputs ---
  run_llm_on_text <- function(text_input) {
    if(text_input == "" || is.na(text_input)) return(NULL)
    
    tryCatch({
      res <- llm(
        text = text_input,
        system_prompt = get_classification_prompt(),
        deduplicate = FALSE,
        params = list(
          temperature = temperature,
          top_p = top_p,
          think = think
        )
      )
      # Standardize column names
      colnames(res)[1:2] <- c("prompt", "result")
      return(res)
    }, error = function(e) {
      warning(sprintf("LLM call failed for paper %s: %s", paper_id, e$message))
      return(NULL)
    })
  }
  
  llm_regex <- run_llm_on_text(regex_text)
  llm_full   <- run_llm_on_text(full_text)

  #--- 3. save LLM outputs for debugging ---
  add_debug_details <- function(llm_df, type, paper_id, paper_title, think, temperature, top_p,output_dir) {
    llm_df$paper_id <- paper_id
    llm_df$thinking_style <- think
    llm_df$temperature <- temperature
    llm_df$top_p <- top_p
    llm_df$paper_title <- paper_title
    paper_log_file <- file.path(output_dir, 
                            sprintf("%s_%s_%s_raw.csv", paper_title, type, paper_id))
    write_csv(llm_df, paper_log_file, append = FALSE)
    message(sprintf("  Saved raw results to: %s", paper_log_file))
    return(llm_df)
  }

  llm_regex <- add_debug_details(llm_regex, "regex_", paper_id, paper_data$paper_title, think, temperature, top_p,raw_dir)
  llm_full   <- add_debug_details(llm_full, "full_", paper_id, paper_data$paper_title, think, temperature, top_p,raw_dir)

  # --- 4. Parse JSON output ---
  parse_llm_safe <- function(llm_df) {
    if(is.null(llm_df)) return(NULL)
    
    tmp_file <- tempfile(fileext = ".csv")
    write_csv(llm_df, tmp_file)
    parse_llm_json(tmp_file)
  }
  
  parsed_regex <- parse_llm_safe(llm_regex)
  parsed_full  <- parse_llm_safe(llm_full)
  
  # --- 5. Extract sample size and sentences ---
  get_sample_info <- function(parsed_df) {
    if(is.null(parsed_df) || nrow(parsed_df) == 0) return(tibble(
      sample_size = NA_integer_,
      sentence = NA_character_
    ))
    
    tibble(
      sample_size = as.integer(parsed_df$sample_size[1]),
      sentence = parsed_df$sentence_containing_sample_size[1]
    )
  }
  
  info_regex <- get_sample_info(parsed_regex)
  info_full  <- get_sample_info(parsed_full)
  
  # --- 6. Compare results ---
  comparison <- tibble(
    paper_id = paper_id,
    regex_sentence = info_regex$sentence,
    regex_size = info_regex$sample_size,
    full_sentence = info_full$sentence,
    full_size = info_full$sample_size,
    match = identical(info_regex$sample_size, info_full$sample_size)
  )
  #make agreement negative if both are NA
  comparison <- comparison %>% mutate(match = ifelse(is.na(regex_size) & is.na(full_size), FALSE, match))

  # --- 7. Log to CSV ---
  if(!file.exists(output_file)) {
    write_csv(comparison, output_file)
  } else {
    write_csv(comparison, output_file, append = TRUE)
  }
  
  return(comparison)
}

# extract sample size goal.
compare_sample_sizes_folder <- function(
  xml_folder,
  output_file = "output/sample_size_comparison/comparison_log.csv",
  temperature = 0.3,
  top_p = 0.9,
  think = "low",
  n_sample = 50
) {
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  
  xml_files <- list.files(xml_folder, pattern = "\\.xml$", full.names = TRUE)
  if(length(xml_files) == 0) stop("No XML files found in the folder")
  
  # --- Sample random subset if requested ---
  if(!is.null(n_sample) && n_sample < length(xml_files)) {
    set.seed(123)  # for reproducibility
    xml_files <- sample(xml_files, n_sample)
    message(sprintf("Randomly selected %d files to process", length(xml_files)))
  }
  
  all_results <- list()

  for(xml_file in xml_files) {
    message(sprintf("Processing file: %s", xml_file))
    
    # --- Read paper ---
    paper <- tryCatch(read(xml_file), error = function(e) {
      warning(sprintf("Failed to read %s: %s", xml_file, e$message))
      return(NULL)
    })
    
    if(is.null(paper)) next
    
    # --- Run the wrapper on this paper ---
    comparison <- tryCatch({
      run_and_compare_sample_sizes(
        paper = paper,
        output_dir = dirname(output_file),
        temperature = temperature,
        top_p = top_p,
        think = think
      )
    }, error = function(e) {
      warning(sprintf("Failed to process %s: %s", xml_file, e$message))
      return(NULL)
    })
    
    if(!is.null(comparison)) all_results[[length(all_results)+1]] <- comparison

  }
  
  if(length(all_results) == 0) {
    warning("No results to save")
    return(NULL)
  }
  
  # --- Combine all results ---
  
  return(final_df)
}
xml_folder <- "/Users/levibaruch/dev/metacheck/data-raw/psychsci/grobid_0.8.2-full/"
comparison_csv <- "output/sample_size_comparison/comparison_log.csv"

# Process 50 random XML files
results <- compare_sample_sizes_folder(
  xml_folder = xml_folder,
  output_file = comparison_csv,
  n_sample = 250
)
