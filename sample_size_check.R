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
run_and_compare_sample_sizes_batch <- function(papers, 
                                               output_dir = "output/sample_size_comparison",
                                               temperature = 0.3,
                                               top_p = 0.9,
                                               think = "low") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # --- 1. Prepare all inputs ---
  regex_inputs <- character(length(papers))
  full_inputs <- character(length(papers))
  paper_ids <- character(length(papers))
  
  for(i in seq_along(papers)) {
    paper <- papers[[i]]
    paper_ids[i] <- paper$id
    
    # Regex-based input
    regex_result <- prepare_fsample_llm_input(paper)
    regex_inputs[i] <- if(regex_result$has_candidates) regex_result$llm_text else ""
    
    # Full method text
    paper_data <- extract_method_text(paper)
    full_inputs[i] <- if(!is.na(paper_data$method_text)) paper_data$method_text else ""
  }
  
  # --- 2. Run LLM in batch ---
  # Filter to only non-empty inputs, but keep original indices
  regex_valid <- regex_inputs != "" & !is.na(regex_inputs)
  full_valid <- full_inputs != "" & !is.na(full_inputs)
  
  # Initialize result lists
  llm_regex_results <- vector("list", length(papers))
  llm_full_results <- vector("list", length(papers))
  
  # Run regex batch
  if(any(regex_valid)) {
    tryCatch({
      res_regex <- llm(
        text = regex_inputs[regex_valid],  # Pass as vector
        system_prompt = get_classification_prompt(),
        deduplicate = FALSE,
        params = list(
          temperature = temperature,
          top_p = top_p,
          think = think
        )
      )
      
      # Map results back to original indices
      valid_idx <- which(regex_valid)
      for(j in seq_along(valid_idx)) {
        llm_regex_results[[valid_idx[j]]] <- res_regex[j, ]
      }
    }, error = function(e) {
      warning(sprintf("Regex batch LLM call failed: %s", e$message))
    })
  }
  
  # Run full text batch
  if(any(full_valid)) {
    tryCatch({
      res_full <- llm(
        text = full_inputs[full_valid],  # Pass as vector
        system_prompt = get_classification_prompt(),
        deduplicate = FALSE,
        params = list(
          temperature = temperature,
          top_p = top_p,
          think = think
        )
      )
      
      # Map results back to original indices
      valid_idx <- which(full_valid)
      for(j in seq_along(valid_idx)) {
        llm_full_results[[valid_idx[j]]] <- res_full[j, ]
      }
    }, error = function(e) {
      warning(sprintf("Full text batch LLM call failed: %s", e$message))
    })
  }
  
  # --- 3. Parse and extract for each paper ---
  all_comparisons <- list()
  
  for(i in seq_along(papers)) {
    # Parse JSON
    parse_single <- function(llm_row) {
      if(is.null(llm_row) || is.null(nrow(llm_row)) || nrow(llm_row) == 0) return(NULL)
      
      tmp_file <- tempfile(fileext = ".csv")
      write_csv(llm_row, tmp_file)
      parse_llm_json(tmp_file)
    }
    
    parsed_regex <- parse_single(llm_regex_results[[i]])
    parsed_full <- parse_single(llm_full_results[[i]])
    
    # Extract sample info
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
    info_full <- get_sample_info(parsed_full)
    
    # Compare
    all_comparisons[[i]] <- tibble(
      paper_id = paper_ids[i],
      regex_sentence = info_regex$sentence,
      regex_size = info_regex$sample_size,
      full_sentence = info_full$sentence,
      full_size = info_full$sample_size,
      match = identical(info_regex$sample_size, info_full$sample_size)
    )
  }
  
  return(bind_rows(all_comparisons))
}

# Main function with batching
compare_sample_sizes_folder <- function(
  xml_folder,
  output_file = "output/sample_size_comparison/comparison_log.csv",
  temperature = 0.3,
  top_p = 0.9,
  think = "low",
  n_sample = 50,
  batch_size = 50
) {
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  
  xml_files <- list.files(xml_folder, pattern = "\\.xml$", full.names = TRUE)
  if(length(xml_files) == 0) stop("No XML files found in the folder")
  
  # --- Sample random subset if requested ---
  if(!is.null(n_sample) && n_sample < length(xml_files)) {
    set.seed(123)
    xml_files <- sample(xml_files, n_sample)
    message(sprintf("Randomly selected %d files to process", length(xml_files)))
  }
  
  # --- Read all papers first ---
  message("Reading papers...")
  papers <- list()
  for(xml_file in xml_files) {
    paper <- tryCatch(read(xml_file), error = function(e) {
      warning(sprintf("Failed to read %s: %s", xml_file, e$message))
      return(NULL)
    })
    if(!is.null(paper)) papers[[length(papers) + 1]] <- paper
  }
  
  message(sprintf("Successfully read %d papers", length(papers)))
  
  # --- Process in batches ---
  all_results <- list()
  n_batches <- ceiling(length(papers) / batch_size)
  
  for(batch_idx in seq_len(n_batches)) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, length(papers))
    
    message(sprintf("Processing batch %d/%d (papers %d-%d)", 
                    batch_idx, n_batches, start_idx, end_idx))
    
    batch_papers <- papers[start_idx:end_idx]
    
    batch_results <- tryCatch({
      run_and_compare_sample_sizes_batch(
        papers = batch_papers,
        output_dir = dirname(output_file),
        temperature = temperature,
        top_p = top_p,
        think = think
      )
    }, error = function(e) {
      warning(sprintf("Batch %d failed: %s", batch_idx, e$message))
      return(NULL)
    })
    
    if(!is.null(batch_results)) {
      all_results[[batch_idx]] <- batch_results
      
      # Save intermediate results
      if(file.exists(output_file)) {
        write_csv(batch_results, output_file, append = TRUE)
      } else {
        write_csv(batch_results, output_file)
      }
      
      message(sprintf("  Completed %d papers in this batch", nrow(batch_results)))
    }
  }
  
  if(length(all_results) == 0) {
    warning("No results to return")
    return(NULL)
  }
  
  final_df <- bind_rows(all_results)
  message(sprintf("Completed! Processed %d papers total", nrow(final_df)))
  
  return(final_df)
}

# Run it
xml_folder <- "/Users/levibaruch/dev/metacheck/data-raw/psychsci/grobid_0.8.2-full/"
comparison_csv <- "output/sample_size_comparison/comparison_log.csv"

results <- compare_sample_sizes_folder(
  xml_folder = xml_folder,
  output_file = comparison_csv,
  n_sample = 2000,
  batch_size = 50
)

# Run it
xml_folder <- "/Users/levibaruch/dev/metacheck/data-raw/psychsci/grobid_0.8.2-full/"
comparison_csv <- "output/sample_size_comparison/comparison_log.csv"

results <- compare_sample_sizes_folder(
  xml_folder = xml_folder,
  output_file = comparison_csv,
  n_sample = 2000,
  batch_size = 10
)