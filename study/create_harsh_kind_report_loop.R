library(httr)
library(jsonlite)
library(devtools)
library(svutils)
library(metacheck)
library(RDCOMClient)
source("sanitize_html.R")   # wherever you save the file
# ---- Settings ----------------------------------------------------------------

page     <- 1            # Starting OSF API page (10 preprints per page; 1 = newest)
reports  <- 1           # Total number of reports to generate
log_file <- "c://preprint/report_log.tsv"  # Tab-delimited log of generated reports

# Each report cycles through 3 types in order:
#   1 = harsh
#   2 = kind without validation info
#   3 = kind with validation info

# ---- LLM settings ------------------------------------------------------------

llm_use(TRUE)
llm_model("ollama/qwen2.5:3b")
llm_max_calls(500)

# ---- Logging -----------------------------------------------------------------

log_report <- function(log_file, preprint_id, filename, report_type) {
  row <- data.frame(
    preprint_id = preprint_id,
    filename    = filename,
    report_type = report_type
  )
  write.table(row,
              file      = log_file,
              sep       = "\t",
              row.names = FALSE,
              col.names = !file.exists(log_file),
              append    = TRUE,
              quote     = FALSE
  )
}

# ---- Helpers -----------------------------------------------------------------

docx_to_pdf_word <- function(docx_path) {
  docx_path <- normalizePath(docx_path, winslash = "\\", mustWork = TRUE)
  pdf_path  <- sub("\\.docx$", ".pdf", docx_path, ignore.case = TRUE)

  word <- COMCreate("Word.Application")
  word[["Visible"]] <- FALSE
  on.exit(tryCatch(word$Quit(0L), error = \(e) NULL))

  doc <- word[["Documents"]]$Open(docx_path)
  doc$ExportAsFixedFormat(OutputFileName = pdf_path, ExportFormat = 17L)

  return(pdf_path)
}

remove_validation <- function(qmd_path) {
  qmd_path |>
    readLines() |>
    (\(x) {
      x <- gsub(
        ".validation { display: block;",
        ".validation { display: none;",
        x, fixed = TRUE
      )

      out        <- character(0)
      in_callout <- FALSE

      for (line in x) {
        if (grepl("^:::.*callout", line)) { in_callout <- TRUE;  next }
        if (in_callout && grepl("^:::", line)) { in_callout <- FALSE; next }
        if (!in_callout) out <- c(out, line)
      }

      out
    })() |>
    writeLines(qmd_path)
}

# ---- Module lists ------------------------------------------------------------

harsh_modules <- c(
  "stat_effect_size_harsh",
  "marginal_harsh",
  "power_harsh",
  "code_check_harsh",
  "stat_p_exact_harsh",
  "stat_p_nonsig_harsh",
  "repo_check_harsh",
  "stat_check_harsh"
)

kind_modules <- c(
  "stat_effect_size_kind",
  "marginal_kind",
  "power_kind",
  "code_check_kind",
  "stat_p_exact_kind",
  "stat_p_nonsig_kind",
  "repo_check_kind",
  "stat_check_kind"
)

# ---- Main loop ---------------------------------------------------------------

page_i     <- page
preprint_i <- 1
page_data  <- NULL

for (report_num in seq_len(reports)) {

  report_type <- (report_num - 1) %% 3 + 1
  type_label  <- c("harsh", "kind_no_validation", "kind_with_validation")[report_type]

  cat(sprintf("\n=== Report %d/%d | page %d | preprint %d | type: %s ===\n",
              report_num, reports, page_i, preprint_i, type_label))

  # Fetch page from OSF when needed
  if (is.null(page_data)) {
    cat(sprintf("Fetching page %d from OSF API...\n", page_i))
    res       <- GET(paste0("https://api.osf.io/v2/preprints/?filter[provider]=psyarxiv&sort=-date_created&page=", page_i))
    preprints <- fromJSON(rawToChar(res$content))
    page_data <- preprints$data
  }

  # Download preprint file
  preprint_id       <- page_data$id[[preprint_i]]
  preprint_info_1   <- osf_info(preprint_id)
  preprint_info_2   <- osf_info(preprint_info_1$primary_file)
  preprint_filename <- gsub("[^A-Za-z0-9_.-]", "_", preprint_info_2$name)

  if (length(preprint_filename) == 0 || !nzchar(preprint_filename)) {
    cat("Skipping preprint", preprint_id, ": no valid file found.\n")
    preprint_i <- preprint_i + 1
    if (preprint_i > 10) {
      page_i     <- page_i + 1
      preprint_i <- 1
      page_data  <- NULL
    }
    next
  }

  cat("File:", preprint_filename, "\n")

  destination_file <- file.path("c://preprint", preprint_filename)
  download.file(preprint_info_2$download_url, destfile = destination_file, mode = "wb")

  if (grepl("\\.pdf$", tolower(destination_file))) {
    pdf_path <- destination_file
  } else {
    cat("Converting to PDF...\n")
    pdf_path <- docx_to_pdf_word(destination_file)
  }

  # Convert to GROBID XML
  convert_grobid(
    file_path = pdf_path,
    save_path = pdf_path,
    api_url   = "https://grobid2.work.abed.cloud/"
  )

  xml_path <- paste0(pdf_path, ".xml")
  # paper    <- read(xml_path)

  # # Get author contact info
  # email_grobid <- paper$authors[[1]][["email"]]
  # email_grobid <- regmatches(email_grobid, regexpr("[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}", email_grobid))
  # author_orcid <- svutils::get_orcid(paper$author$given[1], paper$author$family[1])
  # author_info  <- svutils::orcid_person(author_orcid)
  # author_email_orcid <- paste(author_info$email[1])
  # cat("Emails:", paste(c(email_grobid, author_email_orcid), collapse = ", "), "\n")

  # Output paths
  output_qmd  <- paste0(pdf_path, "_", type_label, "_report.qmd")

  # Generate report
  if (report_type == 1) {
    report_harsh(paper,
                 modules       = harsh_modules,
                 output_format = "qmd",
                 output_file   = output_qmd,
                 args          = list("power_harsh" = list(think = FALSE))
    )

  } else {
    report_kind(paper,
                modules       = kind_modules,
                output_format = "qmd",
                output_file   = output_qmd,
                args          = list("power_kind" = list(think = FALSE))
    )

    if (report_type == 2) {
      remove_validation(output_qmd)
    }
  }

  # Render to HTML
  quarto::quarto_render(output_qmd, output_format = "html")

  output_html <- sub("\\.qmd$", ".html", output_qmd)
  sanitize_html(output_html)

  log_report(log_file, preprint_id, preprint_filename, type_label)

  # Advance preprint index; fetch next page when exhausted
  preprint_i <- preprint_i + 1
  if (preprint_i > 10) {
    page_i     <- page_i + 1
    preprint_i <- 1
    page_data  <- NULL
  }
}

cat(sprintf("\nDone! Generated %d reports starting from page %d.\n", reports, page))
