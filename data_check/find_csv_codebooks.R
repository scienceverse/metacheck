# Find CSV files classified as 'codebook' or 'readme' across all structure.csv outputs.
# Run from data_check/ root: Rscript find_csv_codebooks.R

OUTPUT_DIR <- "./data_check/outputs"

structure_files <- list.files(OUTPUT_DIR, pattern = "^structure\\.csv$",
                              recursive = TRUE, full.names = TRUE)

if (length(structure_files) == 0) stop("No structure.csv files found under ", OUTPUT_DIR)

results <- lapply(structure_files, function(f) {
  df <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE,
             colClasses = c(paper_id = "character")),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)
  hits <- df[tolower(df$ext) == "csv" &
             tolower(df$type) %in% c("codebook", "readme"), ,
             drop = FALSE]
  if (nrow(hits) == 0) return(NULL)
  hits[, c("paper_id", "filename", "rel_path", "type", "group")]
})

out <- do.call(rbind, Filter(Negate(is.null), results))

if (is.null(out) || nrow(out) == 0) {
  message("No CSV codebook/readme files found.")
} else {
  message(nrow(out), " CSV codebook/readme file(s) found across ",
          length(unique(out$paper_id)), " paper(s).\n")
  print(out, row.names = FALSE)
}
