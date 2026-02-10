# ==============================================================================
# token difference
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
verbose(verbose = TRUE)

# Configure LLM
llm_model("ollama/gpt-oss:20b")
llm_use(FALSE)
llm_max_calls(10000)
source("sample_size.R")


approx_token_length <- function(x) {
  ceiling(nchar(x) / 4)
}

compare_lengths <- function(a, b) {
  tibble::tibble(
    string = c("a", "b"),
    characters = c(nchar(a), nchar(b)),
    approx_tokens = c(
      approx_token_length(a),
      approx_token_length(b)
    )
  )
}

# ==============================================================================
# Main Pipeline Function
# ==============================================================================

# extract sample size goal.
compare_token_length <- function(
  xml_folder,
  output_file = "output/token_comparison/comparison_log.csv"
) {
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  
  xml_files <- list.files(xml_folder, pattern = "\\.xml$", full.names = TRUE)
  if(length(xml_files) == 0) stop("No XML files found in the folder")
  
  for(xml_file in xml_files) {
      paper <- tryCatch(read(xml_file), error = function(e) NULL)
      if(is.null(paper)) {
        message("Skipping invalid XML: ", xml_file)
        next
      }
      paper_data <- extract_method_text(paper)
      regex_result <- prepare_fsample_llm_input(paper)

      full_text <- paper_data$method_text
      if(regex_result$has_candidates) {
        regex_text <- regex_result$llm_text
      }
      else {
        regex_text <- ""
      }
      differences <- compare_lengths(full_text, regex_text)
      #log the results per paper ID and Paper name
      log_entry <- tibble(
        paper_id = paper$id,
        paper_title = paper_data$paper_title,
        full_text_approx_tokens = differences$approx_tokens[differences$string == "a"],
        regex_text_approx_tokens = differences$approx_tokens[differences$string == "b"],
        token_difference = differences$approx_tokens[differences$string == "a"] - differences$approx_tokens[differences$string == "b"],
        percentage_difference = ifelse(differences$approx_tokens[differences$string == "a"] > 0, 
                                   (differences$approx_tokens[differences$string == "a"] - differences$approx_tokens[differences$string == "b"]) / differences$approx_tokens[differences$string == "a"] * 100, 
                                   NA)
      )
    if(file.exists(output_file)) {
      write_csv(log_entry, output_file, append = TRUE)
    } else {
      write_csv(log_entry, output_file)
    }
    }
}
xml_folder <- "/Users/levibaruch/dev/metacheck/data-raw/psychsci/grobid_0.8.2-full/"
comparison_csv <- "output/token_comparison/comparison_log.csv"

results <- compare_token_length(
  xml_folder = xml_folder,
  output_file = comparison_csv
)
