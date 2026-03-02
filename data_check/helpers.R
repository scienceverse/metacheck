library(haven)

### READING IN THE FULL TEXT FROM A PAPER
extract_full_text <- function(paper) {
text_by_section <- paper$full_text %>%
    summarise(text = paste(text, collapse = " "), .groups = "drop")
  text <- text_by_section$text
  full_text <- text[1]
  return(full_text)
}



### LOADING ANY TYPE OF DATA AS TIBBLE
# load CSV as tibble
load_csv_as_tibble <- function(file_path) {
    if(!file.exists(file_path)) {
        warning(sprintf("File not found: %s", file_path))
        return(NULL)
    }
    tryCatch({
        data <- read_delim(file_path, show_col_types = FALSE, delim = NULL, guess_max = 10000)
        return(data)
    }, error = function(e) {
        warning(sprintf("Failed to read CSV: %s", e$message))
        return(NULL)
    })
}

# load Excel as tibble
load_excel_as_tibble <- function(file_path, sheet = 1) {
    if(!file.exists(file_path)) {
        warning(sprintf("File not found: %s", file_path))
        return(NULL)
    }   
    tryCatch({
        data <- read_excel(file_path, sheet = sheet)
        return(as_tibble(data))
    }, error = function(e) {
        warning(sprintf("Failed to read Excel: %s", e$message))
        return(NULL)
    })
}

# Load Rdata as tibble
load_rdata_as_tibble <- function(file_path) {
    if(!file.exists(file_path)) {
        warning(sprintf("File not found: %s", file_path))
        return(NULL)
    }
    tryCatch({
        load(file_path)
        data <- get(ls()[1])  # Assuming the RData file contains one object
        if(!is.data.frame(data)) {
            warning("Loaded object is not a data frame")
            return(NULL)
        }
        return(as_tibble(data))
    }, error = function(e) {
        warning(sprintf("Failed to read RData: %s", e$message))
        return(NULL)
    })
}
load_sav_as_tibble <- function(file_path) {
    if(!file.exists(file_path)) {
        warning(sprintf("File not found: %s", file_path))
        return(NULL)
    }
    tryCatch({
        data <- haven::read_sav(file_path)
        return(as_tibble(data))
    }, error = function(e) {
        warning(sprintf("Failed to read SPSS file: %s", e$message))
        return(NULL)
    })
}

read_rtf_as_text <- function(path) {
  raw <- readLines(path, warn = FALSE, encoding = "UTF-8")
  text <- gsub("\\\\[a-z]+[0-9]* ?", "", raw)   # strip RTF control words
  text <- gsub("[{}]", "", text)
  paste(text, collapse = " ")
}


read_codebook_as_text <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  text <- switch(
    ext,
    "pdf"  = pdftools::pdf_text(path),
    "docx" = docxtractr::read_docx(path) |> docxtractr::docx_extract_all_text(),
    "txt"  = readLines(path, warn = FALSE),
    "md"   = readLines(path, warn = FALSE),
    "rtf"  = read_rtf_as_text(path),
    stop("Unsupported file type: ", ext)
  )
  
  if (length(text) > 1) {
    text <- paste(text, collapse = "\n")
  }
  
  text
}

### EXTRACTING INFORMATION FROM TIBBLE

#laod collumn names
get_column_info <- function(tibble_data) {
    if(is.null(tibble_data)) {
        warning("Input tibble is NULL")
        return(NULL)
    }
    return(colnames(tibble_data))
}

# Count amount of rows in tibble
count_rows <- function(tibble_data) {
    if(is.null(tibble_data)) {
        warning("Input tibble is NULL")
        return(NULL)
    }
    return(nrow(tibble_data))
}

# count amount of columns in tibble
count_columns <- function(tibble_data) {
    if(is.null(tibble_data)) {
        warning("Input tibble is NULL")
        return(NULL)
    }
    return(ncol(tibble_data))
}

# generate descriptive statistics for numeric collumns in tibble and generate basic summary for non-numeric collumns
generate_descriptive_stats <- function(tibble_data) {
  
  if (is.null(tibble_data)) {
    warning("Input tibble is NULL")
    return(NULL)
  }
  
  stats_list <- lapply(names(tibble_data), function(col_name) {
    
    column <- tibble_data[[col_name]]
    is_num <- is.numeric(column)
    
    list(
      type = if (is_num) "numeric" else "non_numeric",
      N = sum(!is.na(column)),
      missing = sum(is.na(column)),
      mean = if (is_num) mean(column, na.rm = TRUE) else NULL,
      sd = if (is_num) sd(column, na.rm = TRUE) else NULL,
      min = if (is_num) min(column, na.rm = TRUE) else NULL,
      max = if (is_num) max(column, na.rm = TRUE) else NULL,
      n_unique = length(unique(column))
    )
    
  })
  
  names(stats_list) <- names(tibble_data)
  
  # Convert to tibble similar to JSON structure
  stats_tibble <- tibble::enframe(stats_list, name = "data_type", value = "stats") %>%
    tidyr::unnest_wider(stats)
  
  return(stats_tibble)
}



compare_table <- function(data_stats, paper_stats, tol = 0.01) {
    # Convert relevant columns to numeric
  numeric_cols <- c("N", "mean", "sd", "n_unique", "min", "max", "n_unique")
  
  paper_stats <- paper_stats %>%
    mutate(across(all_of(numeric_cols), ~ as.numeric(.)))
  
  data_stats <- data_stats %>%
    mutate(across(all_of(numeric_cols), ~ as.numeric(.)))

  # Join on variable name
  tbl <- full_join(paper_stats, data_stats, by = "variable", suffix = c("_paper", "_data"))
  
  # Determine which variables are numeric in both datasets
  numeric_vars <- tbl %>%
    filter(!is.na(type_paper) & !is.na(type_data)) %>%
    filter(type_paper %in% c("numeric", "integer") & type_data %in% c("numeric", "integer")) %>%
    pull(variable)
  
  tbl <- tbl %>%
    mutate(
      # Only calculate mean and sd comparisons for numeric variables
      mean_difference = ifelse(variable %in% numeric_vars, abs(mean_paper - mean_data), NA_real_),
      mean_comparison = case_when(
        !(variable %in% numeric_vars) ~ NA_character_,
        is.na(mean_paper) & !is.na(mean_data) ~ "missing in paper",
        !is.na(mean_paper) & is.na(mean_data) ~ "missing in data",
        is.na(mean_paper) & is.na(mean_data) ~ "missing both",
        abs(mean_paper - mean_data) <= tol * abs(mean_paper) ~ "match",
        TRUE ~ "mismatch"
      ),
      
      sd_difference = ifelse(variable %in% numeric_vars, abs(sd_paper - sd_data), NA_real_),
      sd_comparison = case_when(
        !(variable %in% numeric_vars) ~ NA_character_,
        is.na(sd_paper) & !is.na(sd_data) ~ "missing in paper",
        !is.na(sd_paper) & is.na(sd_data) ~ "missing in data",
        is.na(sd_paper) & is.na(sd_data) ~ "missing both",
        abs(sd_paper - sd_data) <= tol * abs(sd_paper) ~ "match",
        TRUE ~ "mismatch"
      ),

      min_difference = ifelse(variable %in% numeric_vars, abs(min_paper - min_data), NA_real_),
      min_comparison = case_when(
        !(variable %in% numeric_vars) ~ NA_character_,
        is.na(min_paper) & !is.na(min_data) ~ "missing in paper",
        !is.na(min_paper) & is.na(sd_data) ~ "missing in data",
        is.na(min_paper) & is.na(sd_data) ~ "missing both",
        abs(min_paper - min_data) <= tol * abs(min_paper) ~ "match",
        TRUE ~ "mismatch"
      ),
      
      max_difference = ifelse(variable %in% numeric_vars, abs(max_paper - max_data), NA_real_),
      max_comparison = case_when(
        !(variable %in% numeric_vars) ~ NA_character_,
        is.na(max_paper) & !is.na(max_data) ~ "missing in paper",
        !is.na(max_paper) & is.na(sd_data) ~ "missing in data",
        is.na(max_paper) & is.na(sd_data) ~ "missing both",
        abs(max_paper - max_data) <= tol * abs(max_paper) ~ "match",
        TRUE ~ "mismatch"
      ),

      # Type comparison
      type_comparison = case_when(
        is.na(type_paper) & !is.na(type_data) ~ "missing in paper",
        !is.na(type_paper) & is.na(type_data) ~ "missing in data",
        is.na(type_paper) & is.na(type_data) ~ "missing both",
        type_paper == type_data ~ "match",
        TRUE ~ "mismatch"
      ),
      
      # N difference
      N_difference = abs(N_paper - N_data),
      N_comparison = case_when(
        is.na(N_paper) & !is.na(N_data) ~ "missing in paper",
        !is.na(N_paper) & is.na(N_data) ~ "missing in data",
        is.na(N_paper) & is.na(N_data) ~ "missing both",
        N_paper == N_data ~ "match",
        TRUE ~ "mismatch"
      ),
      
      n_unique_difference = abs(n_unique_paper - n_unique_data),
      n_unique_comparison = case_when(
        is.na(n_unique_paper) & !is.na(n_unique_data) ~ "missing in paper",
        !is.na(n_unique_paper) & is.na(n_unique_data) ~ "missing in data",
        is.na(n_unique_paper) & is.na(n_unique_data) ~ "missing both",
        n_unique_paper == n_unique_data ~ "match",
        TRUE ~ "mismatch"
      ),
      
      # Percentage differences for numeric checks
      mean_percentage_difference = ifelse(!is.na(mean_difference) & mean_paper != 0,
                                          abs(mean_difference / mean_paper * 100), NA_real_),
      sd_percentage_difference = ifelse(!is.na(sd_difference) & sd_paper != 0,
                                        abs(sd_difference / sd_paper * 100), NA_real_),
      min_percentage_difference = ifelse(!is.na(min_difference) & min_paper != 0,
                                        abs(min_difference / min_paper * 100), NA_real_),
      max_percentage_difference = ifelse(!is.na(max_difference) & max_paper != 0,
                                        abs(max_difference / max_paper * 100), NA_real_),
      n_unique_percentage_difference = ifelse(n_unique_paper != 0,
                                              abs(n_unique_difference / n_unique_paper * 100), NA_real_)
    ) %>%
    
    # Select and reorder columns
    select(
      variable,
      type_paper, type_data, type_comparison,
      N_paper, N_data, N_difference, N_comparison,
      mean_paper, mean_data, mean_difference, mean_percentage_difference, mean_comparison,
      sd_paper, sd_data, sd_difference, sd_percentage_difference, sd_comparison,
      min_paper, min_data, min_difference, min_percentage_difference,
      max_paper, max_data, max_difference, max_percentage_difference, 
      n_unique_paper, n_unique_data, n_unique_difference, n_unique_percentage_difference, n_unique_comparison
    )
  
  return(tbl)
}

generate_md_report <- function(comp_tbl, perc_tol = 5, report_title = "Data Comparison Report") {
  # Helper for NA handling
  `%||%` <- function(a, b) ifelse(is.na(a), b, a)
  
  # Summary counts
  summary_counts <- list(
    type_match = sum(comp_tbl$type_comparison == "match", na.rm = TRUE),
    type_mismatch = sum(comp_tbl$type_comparison == "mismatch", na.rm = TRUE),
    N_match = sum(comp_tbl$N_comparison == "match", na.rm = TRUE),
    N_mismatch = sum(comp_tbl$N_comparison == "mismatch", na.rm = TRUE),
    mean_match = sum(comp_tbl$mean_comparison == "match", na.rm = TRUE),
    mean_mismatch = sum(comp_tbl$mean_comparison == "mismatch", na.rm = TRUE),
    sd_match = sum(comp_tbl$sd_comparison == "match", na.rm = TRUE),
    sd_mismatch = sum(comp_tbl$sd_comparison == "mismatch", na.rm = TRUE),
    n_unique_match = sum(comp_tbl$n_unique_comparison == "match", na.rm = TRUE),
    n_unique_mismatch = sum(comp_tbl$n_unique_comparison == "mismatch", na.rm = TRUE)
  )
  
  # Variables with large differences
  large_diff <- comp_tbl %>%
    filter(
      (mean_percentage_difference > perc_tol) |
      (sd_percentage_difference > perc_tol) |
      (n_unique_percentage_difference > perc_tol)
    )
  
  # Start Markdown
  md <- c()
  
  md <- c(md, paste0("# ", report_title, "\n"))
  
  # Summary
  md <- c(md, "## Summary\n")
  md <- c(md, sprintf("- Type matches: %d, mismatches: %d", summary_counts$type_match, summary_counts$type_mismatch))
  md <- c(md, sprintf("- N matches: %d, mismatches: %d", summary_counts$N_match, summary_counts$N_mismatch))
  md <- c(md, sprintf("- Mean matches: %d, mismatches: %d", summary_counts$mean_match, summary_counts$mean_mismatch))
  md <- c(md, sprintf("- SD matches: %d, mismatches: %d", summary_counts$sd_match, summary_counts$sd_mismatch))
  md <- c(md, sprintf("- Unique count matches: %d, mismatches: %d", summary_counts$n_unique_match, summary_counts$n_unique_mismatch))
  
  # Large differences
  if(nrow(large_diff) > 0){
    md <- c(md, "\n## Variables with large percentage differences\n")
    for(i in 1:nrow(large_diff)) {
      row <- large_diff[i, ]
      md <- c(md, sprintf("- **%s**: mean diff = %.2f%%, sd diff = %.2f%%, unique diff = %.2f%%",
                          row$variable,
                          row$mean_percentage_difference %||% 0,
                          row$sd_percentage_difference %||% 0,
                          row$n_unique_percentage_difference %||% 0))
    }
  } else {
    md <- c(md, "\nNo variables exceed the percentage difference threshold.\n")
  }
  
  # Per-variable table
  md <- c(md, "\n## Detailed Comparison Table\n")
  
  # Table header
  md <- c(md, paste("| Variable | Type Paper | Type Data | Type Comp | N Paper | N Data | N Comp | Mean Paper | Mean Data | Mean Comp | SD Paper | SD Data | SD Comp | Unique Paper | Unique Data | Unique Comp |", 
                    collapse = ""))
  md <- c(md, paste("|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|", collapse = ""))
  
  # Table rows
  for(i in 1:nrow(comp_tbl)) {
    row <- comp_tbl[i, ]
    md <- c(md, paste(
      "|", row$variable,
      "|", row$type_paper %||% "NA",
      "|", row$type_data %||% "NA",
      "|", row$type_comparison %||% "NA",
      "|", row$N_paper %||% "NA",
      "|", row$N_data %||% "NA",
      "|", row$N_comparison %||% "NA",
      "|", row$mean_paper %||% "NA",
      "|", row$mean_data %||% "NA",
      "|", row$mean_comparison %||% "NA",
      "|", row$sd_paper %||% "NA",
      "|", row$sd_data %||% "NA",
      "|", row$sd_comparison %||% "NA",
      "|", row$n_unique_paper %||% "NA",
      "|", row$n_unique_data %||% "NA",
      "|", row$n_unique_comparison %||% "NA",
      "|"
    ))
  }
  
  return(paste(md, collapse = "\n"))
}

parse_descriptive_stats <- function(json_string) {
  stats_list <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
  
  stats_tibble <- tibble::enframe(stats_list, name = "data_type", value = "stats") %>%
    tidyr::unnest_wider(stats)
  
  return(stats_tibble)
}
