# The goal of this script is to index the data repository into multiple data types:
# These types are:
#   Raw or analytically ready data 
#   Documentations (README)
#   Others


source("./data_check/helper.R")
library(metacheck)
llm_use(TRUE)
llm_model("ollama/gpt-oss:20b-cloud")

# ── Constants ─────────────────────────────────────────────────────────────────

ARCHIVE_EXTS <- c("zip", "gz", "tar", "tgz", "bz2", "xz")

RULES <- list(
  # Extensions that are unambiguous
  ext_map = c(
    tsv  = "data",   rds  = "data",
    rda  = "data",   rdata= "data",   parquet = "data",
    feather="data",  sav  = "data",   dta  = "data",   sas7bdat = "data",
    r    = "code",   rmd  = "code",   qmd  = "code",   py = "code",
    ipynb= "code",   sql  = "code",   sh   = "code",
    pdf  = "doc",    docx = "doc",    pptx = "doc",
    bib  = "doc",    ris  = "doc",
    png  = "asset",  jpg  = "asset",  svg  = "asset",  gif = "asset"
  ),
  # Name patterns that override extension ambiguity
  name_patterns = list(
    readme   = "readme",
    codebook = "codebook|code.?book|data.?dict|dictionary|variable.?list|metadata",
    data     = "^data_|_data\\.|_raw\\.|_output\\.|results_",
    doc      = "^notes|^report|^manuscript|^paper|^draft|^summary"
  )
)

LLM_BATCH_SIZE <- 20  # max files per LLM call

CONTENT_LABELS <- c("data", "codebook", "code", "doc")

OUT_BASE <- "./data_check/data"

#   For now, we start with an hardcoded paper. This paper will later be replaced with an input point into the code
filename = "/Users/levibaruch/dev/metacheck-datacheck/data-raw/psychsci/grobid_0.8.2/0956797615620784.xml" # This one works for sure! Ideal 
# filename = "/Users/levibaruch/dev/metacheck-datacheck/data-raw/psychsci/grobid_0.8.2/0956797620948821.xml"

# Lets pick a random paper for now so we can test things
# xml_dir  <- "/Users/levibaruch/dev/metacheck-datacheck/data-raw/psychsci/grobid_0.8.2"
# xml_files <- list.files(xml_dir, pattern = "\\.xml$", full.names = TRUE)
# filename  <- sample(xml_files, 1)
print(filename)
paper <- read(filename)


# 0: Define derived locations from paper
target_directory <- file.path(OUT_BASE, paper$id)
structure_directory <-("./data_check/structure")
# 1: Download associated data from paper

links <- osf_links(paper)
print(links)
unique_links <- unique(links$text)

# Download the data from all the osf links only if the data is not already on disk
# (Simple check for the folder right now, assuming that if folder exitst, the data does aswell.)
# TO DO: Use the list of files from the other OSF functions to check data downloaded instead of assuming folder exists = data exists.
if (!dir.exists(target_directory)){
  for (link in unique_links) {
    if (link == "tvyxz" | link == "osf.io/tvyxz/" | link == "osf.io/tvyxz") {
      print("Open Practise Badge repo detected; skipping")
      next
    }
    osf_file_download(link, download_to = target_directory, max_download_size = Inf,max_file_size = NULL)
  }
}

# 2: lets get an idea of what is in the folder


files <- list.files(target_directory, full.names = TRUE, recursive = TRUE)

# ── 2a. Unpack any archives found ──────────────────────────────────────────
archive_paths <- files[tolower(tools::file_ext(files)) %in% ARCHIVE_EXTS]

if (length(archive_paths) > 0) {
  message("── Unpacking ", length(archive_paths), " archive(s) ──")
  lapply(archive_paths, unpack_archive)
  # Re-scan: pick up extracted files, keep archives themselves out of classification
  files <- list.files(target_directory, full.names = TRUE, recursive = TRUE)
  files <- files[!(tolower(tools::file_ext(files)) %in% ARCHIVE_EXTS)]
}

# ── 2c. Rule-based classification (free, instant) ──────────────────────────

# Apply rules to all files
file_df <- data.frame(
  path    = files,
  stringsAsFactors = FALSE
)

rule_results      <- lapply(files, classify_by_rules)
file_df$label     <- vapply(rule_results, `[[`, character(1), "label")
file_df$certain   <- vapply(rule_results, `[[`, logical(1),   "certain")

# ── 2d. LLM pass: classify uncertain files AND assign experiment group ───────
# One call returns both label and group, avoiding a redundant second pass.

uncertain <- file_df[!file_df$certain, ]

if (nrow(uncertain) > 0) {

  uncertain_relpaths <- sub(paste0("^", normalizePath(target_directory), "/?"), "",
                            normalizePath(uncertain$path))

  CLASSIFY_PROMPT <- 'You are classifying research-project files.
For each numbered path return ONLY a JSON array, same order, no prose.
Each element: {"filename": "<exact path>", "label": "<label>", "group": "<group>"}

label  — pick one: data | codebook | code | doc | readme | asset | other
group  — pick one:
  "ex<N>" if the file belongs to a specific numbered experiment/study
           (e.g. "ex1", "ex2") — infer from BOTH the folder names AND the
           filename itself (e.g. "Study 1.xlsx" or "Experiment2_data.csv"
           count as experiment indicators even without a subfolder)
  "other" for anything not part of a numbered experiment (e.g. meta-analyses,
           pretests, stimuli, introduction materials, shared files, etc.)
  "na"    if label is readme | asset | other (group is not applicable)

You MUST echo back the exact path string provided.'

  parsed <- llm_batch(
    paths         = uncertain_relpaths,
    system_prompt = CLASSIFY_PROMPT,
    user_prefix   = "Classify these files:",
    key_col       = "filename",
    extra_cols    = c("label", "group"),
    fallback_vals = list(label = "other", group = "other")
  )

  file_df$label[!file_df$certain] <- parsed$label

  # Apply experiment refinement immediately for newly-classified content files
  is_content <- parsed$label %in% CONTENT_LABELS & parsed$group != "na"
  if (any(is_content)) {
    uncertain_idx <- which(!file_df$certain)
    file_df$label[uncertain_idx[is_content]] <-
      paste0(parsed$label[is_content], "-", parsed$group[is_content])
  }
}

# ── 2e. Experiment grouping for rule-certain content files ───────────────────
# Uncertain files already got their group above; this pass covers the files
# whose type was resolved by rules (ext_map / name_patterns) and therefore
# never went through the LLM classification call.

certain_content_rows <- file_df$certain & (file_df$label %in% CONTENT_LABELS)

if (any(certain_content_rows)) {
  certain_content_df <- file_df[certain_content_rows, ]

  rel_paths <- sub(paste0("^", normalizePath(target_directory), "/?"), "",
                   normalizePath(certain_content_df$path))

  EXPERIMENT_PROMPT <- 'You are analysing a research project file tree.
Files may be organised into experiments/studies (e.g. "Experiment 1", "Study 2",
"Exp3") or into non-experiment sections (e.g. "Mini meta", "Pretest", "Stimuli").

For each numbered path return ONLY a JSON array, same order, no prose.
Each element: {"path": "<exact path as given>", "group": "<group>"}

Rules:
- Return "ex<N>" (e.g. "ex1") if the file belongs to a numbered experiment/study.
  Infer from BOTH folder names AND the filename itself (e.g. "Study 1.docx" or
  "Experiment2_data.csv" count as experiment indicators even without a subfolder).
- Return "other" for anything not tied to a specific numbered experiment.
- You MUST echo back the exact path string provided.'

  exp_parsed <- llm_batch(
    paths         = rel_paths,
    system_prompt = EXPERIMENT_PROMPT,
    user_prefix   = "Classify which experiment (or 'other') each file belongs to:",
    key_col       = "path",
    extra_cols    = c("group"),
    fallback_vals = list(group = "other")
  )

  file_df$label[certain_content_rows] <-
    paste0(certain_content_df$label, "-", exp_parsed$group)
}

# ── 2g. Summary & save ─────────────────────────────────────────────────────

file_df$filename <- basename(file_df$path)
file_df$ext      <- tools::file_ext(file_df$path)
file_df$paper_id <- paper$id

cat("\n── File inventory ──────────────────────────────\n")
print(table(file_df$label))

if (!dir.exists(structure_directory)) dir.create(structure_directory, recursive = TRUE)
out_path <- file.path(structure_directory, paste0(paper$id, "_structure.csv"))
write.csv(file_df[, c("paper_id", "path", "filename", "ext", "label", "certain")],
          out_path, row.names = FALSE)
message("── Saved file index → ", out_path)

# Handy named list for downstream steps
files_by_type <- split(file_df$path, file_df$label)