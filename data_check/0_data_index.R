# The goal of this script is to index the data repository into multiple data types:
# These types are:
#   Raw data (data which cannot be processed to gain summary statistics)
#   Analytically ready data (there is a better term for this)
#   Documentations (README)



library(metacheck)
llm_use(TRUE)
#   For now, we start with an hardcoded paper. This paper will later be replaced with an input point into the code

filename = "/Users/levibaruch/dev/metacheck-datacheck/data-raw/psychsci/grobid_0.8.2/0956797615620784.xml" # This one works for sure! Ideal 
paper <- read(filename)


# 0: Define derived locations from paper
target_directory <- sprintf("./data_check/data/%s", paper$id)

# 1: Download associated data from paper

links <- osf_links(paper)

# Download the data from all the osf links only if the data is not already on disk 
# (Simple check for the folder right now, assuming that if folder exitst, the data does aswell.)
# TO DO: Use the list of files from the other OSF functions to check data downloaded instead of assuming folder exists = data exists.
if (!dir.exists(target_directory)){
  for (link in links$text) {
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
ARCHIVE_EXTS <- c("zip", "gz", "tar", "tgz", "bz2", "xz")

unpack_archive <- function(path) {
  ext  <- tolower(tools::file_ext(path))
  stem <- tools::file_path_sans_ext(basename(path))
  dest <- file.path(dirname(path), stem)

  if (dir.exists(dest)) {
    message("  skipping (already unpacked): ", basename(path))
    return(dest)
  }

  dir.create(dest, recursive = TRUE)
  message("  unpacking: ", basename(path), " → ", dest)

  tryCatch({
    if (ext == "zip") {
      utils::unzip(path, exdir = dest)
    } else if (ext %in% c("tar", "tgz", "gz", "bz2", "xz")) {
      utils::untar(path, exdir = dest)
    }
    dest
  }, error = function(e) {
    warning("Failed to unpack ", basename(path), ": ", conditionMessage(e))
    NULL
  })
}

archive_paths <- files[tolower(tools::file_ext(files)) %in% ARCHIVE_EXTS]

if (length(archive_paths) > 0) {
  message("── Unpacking ", length(archive_paths), " archive(s) ──")
  lapply(archive_paths, unpack_archive)
  # Re-scan: pick up extracted files, keep archives themselves out of classification
  files <- list.files(target_directory, full.names = TRUE, recursive = TRUE)
  files <- files[!(tolower(tools::file_ext(files)) %in% ARCHIVE_EXTS)]
}

# ── 2c. Rule-based classification (free, instant) ──────────────────────────

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

classify_by_rules <- function(path) {
  # tolower() normalises .R/.Rmd/.QMD etc. so ext_map keys can be plain lowercase
  fname <- tolower(basename(path))
  ext   <- tools::file_ext(fname)
  stem  <- tools::file_path_sans_ext(fname)

  # 1. Name pattern takes priority (catches codebooks saved as .xlsx, .csv, etc.)
  for (label in names(RULES$name_patterns)) {
    if (grepl(RULES$name_patterns[[label]], stem, perl = TRUE)) {
      return(list(label = label, certain = TRUE))
    }
  }
  # 2. Unambiguous extension
  if (ext %in% names(RULES$ext_map)) {
    # txt is a common catch-all — mark uncertain
    certain <- !(ext %in% c("txt"))
    return(list(label = RULES$ext_map[[ext]], certain = certain))
  }

  # 3. Ambiguous extensions (csv, xlsx, xls, html) and unknowns — send to LLM
  list(label = NA_character_, certain = FALSE)
}

# Apply rules to all files
file_df <- data.frame(
  path    = files,
  stringsAsFactors = FALSE
)

rule_results      <- lapply(files, classify_by_rules)
file_df$label     <- vapply(rule_results, `[[`, character(1), "label")
file_df$certain   <- vapply(rule_results, `[[`, logical(1),   "certain")

# ── 2d. LLM pass for uncertain files only ──────────────────────────────────
# Batched into one call to minimise token in and output.

uncertain <- file_df[!file_df$certain, ]

if (nrow(uncertain) > 0) {

  # Build a compact numbered list of just the filenames
  file_list_text <- paste(
    seq_len(nrow(uncertain)),
    basename(uncertain$path),
    sep = ". ",
    collapse = "\n"
  )

  CLASSIFY_PROMPT <- "You are classifying research project files.
For each numbered filename return ONLY a JSON array, same order, no prose.
Each element: {\"label\": \"<label>\"}
Labels (pick one): data | codebook | code | doc | readme | asset | other"

  batch_input <- paste0(
    "Classify these filenames:\n\n", file_list_text,
    "\n\nReturn only a JSON array with ", nrow(uncertain), " objects."
  )

  raw <- llm(system_prompt = CLASSIFY_PROMPT, text = batch_input)

  # Strip markdown fences if present, then parse
  extract_json <- function(txt) {
    txt <- trimws(txt)
    # Remove ```json ... ``` or ``` ... ``` wrappers
    txt <- gsub("^```(?:json)?\\s*|\\s*```$", "", txt, perl = TRUE)
    trimws(txt)
  }

  parsed <- tryCatch({
    jsonlite::fromJSON(extract_json(raw$answer))
  }, error = function(e) {
    message("── LLM raw response (for debugging) ──\n", raw, "\n───────────────────────────────────────")
    warning("LLM response parse failed; labelling uncertain files as 'other'")
    data.frame(label = rep("other", nrow(uncertain)))
  })

  file_df$label[!file_df$certain] <- parsed$label
}

# ── 2e. Summary ────────────────────────────────────────────────────────────

file_df$filename <- basename(file_df$path)
file_df$ext      <- tools::file_ext(file_df$path)

cat("\n── File inventory ──────────────────────────────\n")
print(table(file_df$label))

# Handy named list for downstream steps
files_by_type <- split(file_df$path, file_df$label)