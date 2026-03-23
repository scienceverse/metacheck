# Remove papers that have CSV codebooks/readmes from codebook_summary.csv so they
# will be re-processed by run_2_codebook_bulk.R with the improved CSV parser.
# Run from data_check/ root: Rscript runners/reset_csv_codebook_papers.R

OUTPUT_DIR   <- "./data_check/outputs"
SUMMARY_CSV  <- "./data_check/results/codebook_summary.csv"

# ── 1. Collect paper_ids that have at least one CSV codebook/readme ────────────

structure_files <- list.files(OUTPUT_DIR, pattern = "^structure\\.csv$",
                              recursive = TRUE, full.names = TRUE)
if (length(structure_files) == 0) stop("No structure.csv files found under ", OUTPUT_DIR)

csv_codebook_papers <- unique(unlist(lapply(structure_files, function(f) {
  df <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE,
             colClasses = c(paper_id = "character")),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)
  hits <- df[tolower(df$ext) == "csv" &
             tolower(df$type) %in% c("codebook", "readme"), , drop = FALSE]
  if (nrow(hits) == 0) return(NULL)
  hits$paper_id
})))

message(length(csv_codebook_papers), " paper(s) with CSV codebooks/readmes identified.")

# ── 2. Remove those paper_ids from codebook_summary.csv ───────────────────────

if (!file.exists(SUMMARY_CSV)) {
  message("No codebook_summary.csv found — nothing to remove.")
  quit(save = "no")
}

summary_df <- read.csv(SUMMARY_CSV, stringsAsFactors = FALSE,
                       colClasses = c(paper_id = "character"))
before <- nrow(summary_df)

summary_df <- summary_df[!summary_df$paper_id %in% csv_codebook_papers, , drop = FALSE]
removed <- before - nrow(summary_df)

if (removed == 0) {
  message("None of those papers were in codebook_summary.csv — nothing changed.")
} else {
  write.csv(summary_df, SUMMARY_CSV, row.names = FALSE)
  message(removed, " row(s) removed from codebook_summary.csv. ",
          nrow(summary_df), " row(s) remain.")
}
