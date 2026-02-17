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


#####

# 0 LOADING AND DEFINING VARIABLES

#####

# reading in paper. Only hardcoded path now!
filename = "/Users/levibaruch/dev/metacheck-datacheck/data-raw/psychsci/grobid_0.8.2/0956797615620784.xml" # This one works for sure! Ideal 
#filename = "/Users/levibaruch/dev/metacheck/data-raw/psychsci/grobid_0.8.1/0956797620929302.xml"

# Define variables
paper <- read(filename)
full_text <- extract_full_text(paper)
links <- osf_links(paper)
target_directory <- sprintf("./data_check/data/%s", paper$id)


# Download the data from all the osf links only if the data is not already on disk 
# (Simple check for the folder right now, assuming that if folder exitst, the data does aswell.)
# TO DO: Use the list of files from the other OSF functions to check data downloaded.
if (!dir.exists(target_directory)){
  for (link in links$text) {
    if (link == "tvyxz" | link == "osf.io/tvyxz/" | link == "osf.io/tvyxz") {
      print("Open Practise Badge repo detected; skipping")
      next
    }
    osf_file_download(link, download_to = target_directory, max_download_size = Inf,max_file_size = NULL)
  }
}

#####

# 1 DEFINING DATA TYPES FOUND IN THE OSF REPO

#####

# Since many OSF repos have multiple experiments, the amount of files is large
# We need to first find a way to distinguish between the different
# Datafiles, codebooks, code files. This is non trivial, since sometimes the same extention
# Is used for multiple things (e.g. an excel file for both a codebook and a datapoint)
# Multiple experiments also need to be identified and assigned a number.
# For now, this is done using an LLM. Perhaps a REGEX or folder based system would be better.

# Make a file list from the downloaded OSF fiels
files <- list.files(target_directory, full.names = TRUE, recursive = TRUE)

# This prompt is super verbose and ineffient token wise, but it works for now.

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
    "folder/key.rtf": "code_1"
  }'
  )
json_file_list <- llm(
  system_prompt = file_list_prompt,
  text = ""
)
parsed_file_list <- jsonlite::fromJSON(json_file_list$answer, simplifyVector = TRUE)

# After this, the process can be looped over the multiple experiments
# For now, we keep it simple and only go for the first detected experiment

data_files <- names(parsed_file_list)[parsed_file_list == "data_1"]
code_files <- names(parsed_file_list)[parsed_file_list == "code_1"]
codebook_files <- names(parsed_file_list)[parsed_file_list == "codebook_1"]

#####

# 2 Tidying up the data and reading in the codebook

#####


# Read the file based on the file type. 
# For now, we take the heuristic of the largest file per type being the right one (in case of multiple)
# since I want the "rawest" data. but this will likely need to be more complex in practice.

if (length(data_files) == 0) {
  warning("No data files found")
  data_tibble <- NULL
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

# Generate the Descriptive statistics from the data file. 
# This will be used to compare against the descriptive statistics extracted from the paper.
desc_stats <- generate_descriptive_stats(data_tibble)

# If a codebook exists and is detected we  want to use this find out which variable
# Has a certain meaning. Because 1: We can use this information to match the paper variables mentioned to the
# data, and 2: we can use this to identify the origin/source of the variable perhaps.

if (length(codebook_files) > 0) {
  codebook_file <- codebook_files[which.max(file.size(codebook_files))]
  # We parse it to simple text so it can be fed to an LLM. 
  codebook_text <- read_codebook_as_text(codebook_file)
}

# Now, we generate a generic codebook to use later. 
# We use both the row names in the codebook and the descriptive statistics 
# to match the variables in the data to the variables in the paper. 
#This will likely need to be more complex in practice, but for now we can just use the 
# row names in the codebook and the variable names in the descriptive statistics to match the variables.

# THIS is a very lazy llm implementation and can probably be better done otherways.

llm_prompt <- paste(
  "You are a assistant who parses codebooks to match their variables to variables in a dataset. Dataset variables:",
  paste(get_column_info(data_tibble), collapse = ", "),
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

# Parse the variable mapping JSON as a tibble while fixing a stupid bug I found a few times.
fixed_json <- gsub("\\\\'92", "\u2019", variable_mapping$answer)
variable_mapping_tibble <- jsonlite::fromJSON(fixed_json, simplifyVector = TRUE) %>%
  tibble::enframe(name = "data_variable", value = "codebook_info") %>%
  tidyr::unnest_wider(codebook_info)
print (variable_mapping_tibble)

#####

# 2 Descriptive statistics from the paper

###### 

# Now, we extract the same structured descriptive statistics from the paper. 
# This can be done more efficiently by extracting regex parts and feeding this to the llm istead
# Of the whole method text. For sample size I did some experimentation (but not structured)
# (this can be found in data_check/sample_size_stuff/sample_size.R).
# This leads to a ~75% decrease in input tokens, Although I noticed that output tokens are more often the
# Time-bottleneck.


# define prompts for descriptive statistics and sample size etc. (very sloppy written right now.)
# Especially for non-numeric this is very underwhelming right now :(
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
  # This one is the important one for now!
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

# extract all these features from paper using the whole method :(.

answers <- map(features, function(feature) {
  answer <- llm(
    system_prompt = feature$prompt,
    text = full_text
  )
  list(name = feature$name, answer = answer)
})

# Now verbosely define the values coming out of here. Big mitake right now is not making them 
# Numerical.
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

# OFten, the variable names are not the same between the paper and the data, 
# so we need to do some cleaning and matching before we can compare the statistics. 
# This is a simple example of how we can do that, but it will likely need to be more complex in practice.
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
  # Add collumns identifying the source for each
  name = "data_variable",
  value = "paper_variable"
) %>%
  mutate(paper_variable = map_chr(paper_variable, ~ if (is.null(.x)) NA_character_ else .x))



# rename the paper_descriptivestatistics "data_type" column to "paper_variable" to 
# match with the matching_mapping_tibble
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

# Rename the variable here aswell
data_desc_stats <- desc_stats %>%
  mutate(across(everything(), ~ tolower(as.character(.)))) %>%
    rename(variable = data_type)



# Make everythign that should be numberical, numerical
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

# Now we can compare!
paper_desc_stats
data_desc_stats

results <- compare_table(data_desc_stats, paper_desc_stats) 
print(results)


# Write the results in an MD report so its easily parsable.
writeLines(generate_md_report(results, report_title = sprintf("Data Comparison Report for paper %s", paper$id)), sprintf("./data_check/comparison_results/%s.md", paper$id))


# Create the output! I pushed an example so it can be observed and not ran
dir.create("./data_check/comparison_results/", showWarnings = FALSE)
write_csv(results, sprintf("./data_check/comparison_results/%s.csv", paper$id))


