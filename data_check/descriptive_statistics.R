library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(stringr)
library(tidyr)
library(metacheck)

# Source R functions for now
source("./data_check/helpers.R")
# Configure LLM
llm_model("ollama/gpt-oss:20b")
llm_use(TRUE)
llm_max_calls(10000)
verbose(verbose = TRUE)

# SHOULD BE PART OF A MAJOR FUNCTION LATER. NOW HARDCODED FOR TESTING
#Reading helper$
extract_full_text <- function(paper) {
text_by_section <- paper$full_text %>%
    summarise(text = paste(text, collapse = " "), .groups = "drop")
  
  text <- text_by_section$text

full_text <- text[1]
return(full_text)
}


# Download OSF files from paper


# reading in paper. Only hardcoded part now!
filename = "/Users/levibaruch/dev/metacheck/data-raw/psychsci/grobid_0.8.1/0956797615620784.xml"
#filename = "/Users/levibaruch/dev/metacheck/data-raw/psychsci/grobid_0.8.1/0956797620929302.xml"
paper <- read(filename)
full_text <- extract_full_text(paper)
links <- osf_links(paper)

# # Download the data from all the osf links
# # Commented to not overload API
# for (link in links$text) {
#   if (link == "tvyxz") {
#     #skip the badges to open practises repo always
#     next
#   }
#   osf_file_download(link, download_to = sprintf("./data_check/data/%s/%s", paper$id, link), max_download_size = Inf,max_file_size = Inf)
# }

### Analyse the types in the data.

# list of data file types (csv, xlsx, sav, etc.)
data_types <- c("csv", "xlsx", "sav", "dta")

# list of codebook file types (pdf, docx, txt, etc.)
codebook_types <- c("pdf", "docx", "txt", "md", "rtf", "xlsx")

# list of code files (R, py, etc.)
code_types <- c("R", "r", "rmd", "RMD", "py", "ipynb", "do", "sas")

# detect the files.
# these things can be gigantic. We need to first let an LLM make a subselection of which files are 
# Datafiles, codebooks, code files, others and then we can read in the datafiles and codebooks and extract the relevant information. 
files <- list.files(sprintf("./data_check/data/%s", paper$id), full.names = TRUE, recursive = TRUE)

## Check for a repocheck approach

file_list_prompt <- paste(
  "You are a helpful assistant for categorizing files in a research project.",
  "Here is a list of files in the project:",
  paste(files, collapse = "\n"),
  "Categorize each file into one of the following categories: data, codebook, code, other.",
  "Data files are files that contain raw or processed data (e.g., .csv, .xlsx, .sav, .dta).",
  "Codebooks are files that describe the variables in the dataset (e.g., .pdf, .docx, .txt).",
  "Code files are files that contain code for data analysis (e.g., .R, .py).",
  "Other files are files that do not fit into the above categories.",
  "Return a JSON object where the keys are the file names and the values are the categories.",
  
  "Rules: do not include formatting or newlines in output. The JSON must be a single line with no newlines or extra formatting. Do not include any explanation or extra text. The output must start with { and end with }.",
  "Sometimes, multieple reseraches exist in one folder. In this case, number the data and files with the same name with _1, _2, etc. For example, if there are two data files called data.csv, name them data_1.csv and data_2.csv in the output JSON. Even when there is only one study, use _1 in the name to be consistent with the case when there are multiple studies.",
  "Here is an example",
  '{
    "folder/data.csv": "data_1",
    "folder2/codebook.pdf": "codebook_2",
    "folder/analysis.R": "code_1",
    "folder/readme.txt": "code_1"
  }'
  )

json_file_list <- llm(
  system_prompt = file_list_prompt,
  text = ""
)

parsed_file_list <- jsonlite::fromJSON(json_file_list$answer, simplifyVector = TRUE)

#now, only check the files that are categorized as data, codebook, and code. 
# I only check the first reserach in each, since I cannot do more right now.
data_files <- names(parsed_file_list)[parsed_file_list == "data_1"]
code_files <- names(parsed_file_list)[parsed_file_list == "code_1"]
codebook_files <- names(parsed_file_list)[parsed_file_list == "codebook_1"]

# Read the file based on the file type. For now, we take the heuristic of the largest file, but this will likely need to be more complex in practice.
if (length(data_files) == 0) {
  warning("No data files found")
  tibble_data <- NULL
} else {
  data_file <- data_files[which.max(file.size(data_files))]
  data_tibble <- switch(
    tools::file_ext(data_file),
    "csv" = load_csv_as_tibble(data_file),
    "xlsx" = load_excel_as_tibble(data_file),
    "sav" = load_sav_as_tibble(data_file),
    "dta" = load_dta_as_tibble(data_file),
    {
      warning("Unsupported file type: ", tools::file_ext(data_file))
      NULL
    }
  )
}

# Generate the Descriptive statistics from the data. This will be used to compare against the descriptive statistics extracted from the paper.
desc_stats <- generate_descriptive_stats(data_tibble)

# Lets try to understand the descriptive statistics from the codebook

if (length(codebook_files) > 0) {
  codebook_file <- codebook_files[which.max(file.size(codebook_files))]
  codebook_text <- read_codebook_as_text(codebook_file)
}

# Make a R codebook from the codebook text to use. Use both the row names in the codebook and the descriptive statistics to match the variables in the data to the variables in the paper. This will likely need to be more complex in practice, but for now we can just use the row names in the codebook and the variable names in the descriptive statistics to match the variables.

collumns_names_data <- get_column_info(data_tibble)
llm_prompt <- paste(
  "You are a assistant who parses codebooks to match their variables to variables in a dataset. Dataset variables:",
  paste(collumns_names_data, collapse = ", "),
  "Codebook text:",
  codebook_text,
  "Extract the variable names from the codebook text and match them to the variables in the dataset. Return a JSON object where the keys are the variable names in the dataset",
  "and the values are the variable names and explanation in the codebook. Example output:",
  '{
  "age": {
    "codebook_variable": "AGE_YRS",
    "explanation": "Age of the respondent in completed years at time of survey."
  },
  "gender": {
    "codebook_variable": "SEX",
    "explanation": "Self-reported gender of the participant (1 = Male, 2 = Female, 3 = Other)."
  }
}',
'Rules: do not include formatting or newlines in output'
)

variable_mapping <- llm(
  system_prompt = llm_prompt,
  text = ""
)

# Parse the variable mapping JSON as a tibble
fixed_json <- gsub("\\\\'92", "\u2019", variable_mapping$answer)
variable_mapping_tibble <- jsonlite::fromJSON(fixed_json, simplifyVector = TRUE) %>%
  tibble::enframe(name = "data_variable", value = "codebook_info") %>%
  tidyr::unnest_wider(codebook_info)
print (variable_mapping_tibble)

### DESCRIPTIVE STATISTICS FROM PAPER ###
# define prompts for descriptive statistics
features <- list(
  list(
    name = "sample_size",
    prompt = "What is the sample size of the study? Please provide only a number in your answer."
  ),
  list(
    name = "num_conditions",
    prompt = "How many experimental conditions are in the study? Please provide only a number in your answer."
  ),
  list(
    name = "is_within_subjects",
    prompt = "Is the study a within subjects or between-subjects design? Please answer with 'between' or 'within' or 'neither"
  ),
    list(
        name = "descriptive_statistics",
        prompt = paste(
    "You are a data assistant.",
    "Extract descriptive statistics for each variable described in 'text'.",
    "",
    "For every variable return:",
    "- type: numeric or non_numeric",
    "- N: number of non-missing observations",
    "- missing: number of missing values",
    "- mean: numeric variables only (otherwise null)",
    "- sd: numeric variables only (otherwise null)",
    "- min: numeric variables only (otherwise null)",
    "- max: numeric variables only (otherwise null)",
    "- n_unique: number of unique values",
    "",
    "Return ONLY valid JSON.",
    "Rules:",
    "- Use as many decimals as are described in the paper",
    "- Use double quotes for all JSON keys and strings.",
    "- Use null (not NA or None) for missing values.",
    "- Do not include explanations or markdown.",
    "- The output must start with { and end with }.",
    "",
    "Example structure for numeric:",
    "{",
    "  \"reaction_time_ms\": {",
    "    \"type\": \"numeric\",",
    "    \"N\": 100,",
    "    \"missing\": 2,",
    "    \"mean\": 5.2,",
    "    \"sd\": 1.3,",
    "    \"min\": 1,",
    "    \"max\": 10,",
    "    \"n_unique\": 95",
    "  }",
    "}",
    "",
    "Example structure for non-numeric:",
    "{",
    "  \"condition\": {",
    "    \"type\": \"non_numeric\",",
    "    \"N\": 100,",
    "    \"missing\": 0,",
    "    \"mean\": null,",
    "    \"sd\": null,",
    "    \"min\": null,",
    "    \"max\": null,",
    "    \"n_unique\": 3",
    "  }",
    "}"
  )
    )
)

# extract all these features from paper.
answers <- map(features, function(feature) {
  answer <- llm(
    system_prompt = feature$prompt,
    text = full_text
  )
  list(name = feature$name, answer = answer)
})

paper_sample_size <- answers[[1]]$answer
paper_num_conditions <- answers[[2]]$answer
paper_is_within_subjects <- answers[[3]]$answer
descriptive_statistics_llm <- answers[[4]]$answer
parse_descriptive_stats <- function(json_string) {
  stats_list <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
  
  stats_tibble <- tibble::enframe(stats_list, name = "data_type", value = "stats") %>%
    tidyr::unnest_wider(stats)
  
  return(stats_tibble)}

paper_descriptivestatistics <- parse_descriptive_stats(descriptive_statistics_llm$answer)

# OFten, the variable names are not the same between the paper and the data, so we need to do some cleaning and matching before we can compare the statistics. This is a simple example of how we can do that, but it will likely need to be more complex in practice.
# For this, if the codebook exists we try to match variable names. Lets do this using an llm prompt.


matching_prompt <- paste(
  "You are a helpful assistant for matching variable names between a paper and a dataset.",
  "Here are the variable names from the dataset:",
  paste(variable_mapping_tibble$data_variable),
  "Here are explanations for the variables from the codebook:",
  paste(variable_mapping_tibble$explanation, collapse = ", "),
  "Here are the variable names and explanations from the paper:",
  paste(paper_descriptivestatistics$data_type, collapse = ", "),
  "Match the variables from the dataset to the variables in the paper using the explanations.",
  "Return a JSON object where all dataset variables and all paper variables appear exactly once.",
  "Keys should be dataset variable names, values should be paper variable names.",
  "If a variable cannot be matched, use null as the value.",
  "If a variable matches multiple variables, choose the best match",
  "The JSON must be a single line with no newlines or extra formatting. Do not include any explanation or extra text.",
  "Example output:",
    '{
      "age": "age_years",
      "gender": "sex at birth"
      }'
)


matching_json <- llm(
  system_prompt = matching_prompt,
  text = ""
) 

# Parse the matching JSON
matching_mapping <- jsonlite::fromJSON(matching_json$answer, simplifyVector = TRUE)
matching_mapping_tibble <- tibble::enframe(
  matching_mapping,
  name = "data_variable",
  value = "paper_variable"
) %>%
  mutate(paper_variable = map_chr(paper_variable, ~ if (is.null(.x)) NA_character_ else .x))



# rename the paper_descriptivestatistics data_type column to paper_variable to match with the matching_mapping_tibble
paper_descriptivestatistics <- paper_descriptivestatistics %>%
  rename(paper_variable = data_type)

combined_tibble <- matching_mapping_tibble %>%
  left_join(paper_descriptivestatistics, by = "paper_variable")
  #drop the paper_variable column since it is the same as the data_variable column in the matching_mapping_tibble
  combined_tibble <- combined_tibble %>%
  select(-paper_variable)


# make all columns lowercase in combined_tibble and desc_stats and rename variable collumn to "variable"
paper_desc_stats <- combined_tibble %>%
  mutate(across(everything(), ~ tolower(as.character(.)))) %>%
  rename(variable = data_variable)

data_desc_stats <- desc_stats %>%
  mutate(across(everything(), ~ tolower(as.character(.)))) %>%
    rename(variable = data_type)

paper_desc_stats
data_desc_stats

# # do a grim check on the paper stats

#make numerical where needed
library(scrutiny)

paper_desc_stats <- paper_desc_stats %>%
  mutate(
    N = as.numeric(N),
    missing = as.numeric(missing),
    mean = as.numeric(mean),
    sd = as.numeric(sd),
    min = as.numeric(min),
    max = as.numeric(max),
    n_unique = as.numeric(n_unique)
  )

library(scrutiny)

for (mean in paper_desc_stats$mean){

result <- grim(x = mean, n=80)
print(result)
}

for (mean in data_desc_stats$mean){

result <- grim(x = mean, n=80)
print(result)
}

source("./data_check/helpers.R") 
results <- compare_table(data_desc_stats, paper_desc_stats) 
print(results)



writeLines(generate_md_report(results, report_title = sprintf("Data Comparison Report for paper %s", paper$id)), sprintf("./data_check/comparison_results/%s.md", paper$id))
#save tibble as csv

dir.create("./data_check/comparison_results/", showWarnings = FALSE)
write_csv(results, sprintf("./data_check/comparison_results/%s.csv", paper$id))


