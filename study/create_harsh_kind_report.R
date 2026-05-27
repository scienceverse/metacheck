library(httr)
library(jsonlite)
library(devtools)
# devtools::install_github("scienceverse/svutils")
library(svutils)
# remotes::install_github("scienceverse/metacheck", ref = "dev_report_project")
library(metacheck)
# devtools::install_github("omegahat/RDCOMClient")
library(RDCOMClient)

# Specify LLM settings

llm_use(TRUE)
llm_model("ollama/qwen2.5:3b")
llm_max_calls(500)

# Function to convert word to pdf (if this does not work, you can do it manually)
# Word needs to be open for the conversion to work

docx_to_pdf_word <- function(docx_path) {
  # Ensure path is in Windows format
  docx_path <- normalizePath(docx_path, winslash = "\\", mustWork = TRUE)

  # Build output filename (same folder, .pdf extension)
  pdf_path <- sub("\\.docx$", ".pdf", docx_path, ignore.case = TRUE)

  # Start Word
  word <- COMCreate("Word.Application")
  word[["Visible"]] <- FALSE

  # Open DOCX
  doc <- word[["Documents"]]$Open(docx_path)

  # 17 = wdExportFormatPDF (Word constant)
  doc$ExportAsFixedFormat(
    OutputFileName = pdf_path,
    ExportFormat   = 17
  )

  # Close and quit Word
  doc$Close(FALSE)
  word$Quit()

  return(pdf_path)
}

# Get a pdf file from PsyArXiv

page_i <- 1 # get which page (1 is latest)
# The sort=-date_created with the - means we sort based on newest preprints.
res <- GET(paste("https://api.osf.io/v2/preprints/?filter[provider]=psyarxiv&sort=-date_created&page=", page_i, sep = "")) # access page i
preprints <- fromJSON(rawToChar(res$content))
preprints <- preprints$data # temporarily store data of current page
preprint_i <- 2 # number 1 to 10 from the page
preprint_info_1 <- osf_info(preprints$id[[preprint_i]])
preprint_info_2 <- osf_info(preprint_info_1$primary_file)
preprint_filename <- preprint_info_2$name
preprint_filename <- gsub("[^A-Za-z0-9_.-]", "_", preprint_filename)
print(preprint_filename)
destination_file <- file.path("c://preprint", preprint_filename)
download.file(preprint_info_2$download_url, destfile = destination_file, mode = "wb")

if (grepl("\\.pdf$", tolower(destination_file))) {
  cat("Skipping conversion step, file is already a PDF.\n")
  pdf_path <- destination_file
} else {
  cat("Converting file...\n")
  # Convert to PDF
  pdf_path <- docx_to_pdf_word(destination_file)
}

# Convert to GROBID
convert_grobid(
  file_path = pdf_path,
  save_path = pdf_path,
  api_url = "https://grobid2.work.abed.cloud/"
) # Need to connect to uni network or EduVPN

# Make xml path
xml_path <- paste0(pdf_path, ".xml") # Read paper into metacheck
paper <- read(xml_path)
# Get emails. Not all papers will have them
email_grobid <- paper$authors[[1]][["email"]]
email_grobid <- regmatches(email_grobid, regexpr("[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}", email_grobid))
author_orcid <- svutils::get_orcid(paper$author$given[1], paper$author$family[1])
author_info <- svutils::orcid_person(author_orcid)
author_email_orcid <- paste(author_info$email[1])
paste(c(email_grobid, author_email_orcid))

# Create Report

output_qmd_filename <- paste(pdf_path, "_report.qmd", sep = "")

report_harsh(paper,
  modules = c(
    "stat_effect_size_harsh.R",
    "marginal_harsh.R",
    "power_harsh.R",
    "code_check_harsh.R",
    "open_practices_harsh.R",
    "stat_p_exact_harsh.R",
    "stat_p_nonsig_harsh.R"
  ),
  output_format = "qmd",
  output_file = output_qmd_filename,
  args = list(
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/power_harsh.R" = list(
      think = FALSE
    )
  )
)

report_kind(paper,
  modules = c(
    "stat_effect_size_kind.R",
    "marginal_kind.R",
    "power_kind.R",
    "code_check_kind.R",
    "open_practices_kind.R",
    "stat_p_exact_kind.R",
    "stat_p_nonsig_kind.R"
  ),
  output_format = "qmd",
  output_file = output_qmd_filename,
  args = list(
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/power_harsh.R" = list(
      think = FALSE
    )
  )
)

# Optionally, remove validation info:

output_qmd_filename |>
  readLines() |>
  gsub(".validation { display: block;", ".validation { display: none;", x = _, fixed = TRUE) |>
  writeLines(qmd_path)

# Compile as html
quarto::quarto_render(qmd_path, output_format = "html")
browseURL(gsub("\\.qmd$", ".html", qmd_path))
