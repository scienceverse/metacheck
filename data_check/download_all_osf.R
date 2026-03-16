# download_all_osf.R
# ─────────────────────────────────────────────────────────────────────────────
# Download all OSF files for all papers in advance, separated from processing
# Writes progress to download_progress.csv
# ─────────────────────────────────────────────────────────────────────────────

library(metacheck)

# ── Constants ─────────────────────────────────────────────────────────────────

DATA_DIR          <- "./data_check/data"
XML_DIR           <- "./data-raw/psychsci/grobid_0.8.2"
PROGRESS_CSV      <- "./data_check/download_progress.csv"
BADGE_REPOS       <- c("tvyxz", "osf.io/tvyxz/", "osf.io/tvyxz")
DOWNLOAD_TIMEOUT_SEC <- 10 * 60  # 10 minutes; set to NULL to disable

# ── Discover all papers ───────────────────────────────────────────────────────

all_ids <- tools::file_path_sans_ext(
  list.files(XML_DIR, pattern = "\\.xml$", full.names = FALSE)
)
if (length(all_ids) == 0) stop("No XML files found in ", XML_DIR)

# ── Load prior progress ───────────────────────────────────────────────────────

done_ids <- character(0)
if (file.exists(PROGRESS_CSV)) {
  prior <- tryCatch(
    read.csv(PROGRESS_CSV, stringsAsFactors = FALSE,
             colClasses = c(paper_id = "character")),
    error = function(e) NULL
  )
  if (!is.null(prior) && "paper_id" %in% names(prior)) {
    done_ids <- unique(as.character(prior$paper_id))
    message("── Resuming: ", length(done_ids), " paper(s) already downloaded")
  }
}

# ── Determine papers to download ──────────────────────────────────────────────

remaining_ids <- setdiff(all_ids, done_ids)

if (length(remaining_ids) == 0) {
  message("── All papers already downloaded.")
  q(save = "no")
}

message("── Will download ", length(remaining_ids), " paper(s)")

# ── Helper: append one row to progress CSV ────────────────────────────────────

append_progress_row <- function(pid, success, error_msg = NA_character_) {
  row <- data.frame(
    paper_id = pid,
    success = success,
    error = error_msg,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  write_header <- !file.exists(PROGRESS_CSV)
  write.table(row, PROGRESS_CSV, append = TRUE, sep = ",",
              row.names = FALSE, col.names = write_header)
}

# ── Run downloads ─────────────────────────────────────────────────────────────

for (i in seq_along(remaining_ids)) {
  pid <- remaining_ids[i]

  # Re-check in case another process already handled this
  if (file.exists(PROGRESS_CSV)) {
    already <- tryCatch(
      read.csv(PROGRESS_CSV, stringsAsFactors = FALSE,
               colClasses = c(paper_id = "character")),
      error = function(e) NULL
    )
    if (!is.null(already) && pid %in% already$paper_id) {
      message("  skipping (already downloaded): ", pid)
      next
    }
  }

  cat("\n══════════════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Download %d / %d  —  %s\n", i, length(remaining_ids), pid))
  cat("══════════════════════════════════════════════════════════════════════\n")

  result <- tryCatch({
    # Load paper metadata from XML
    xml_path <- file.path(XML_DIR, paste0(pid, ".xml"))
    paper <- read(xml_path)
    stopifnot(!is.null(paper$id))

    # Extract OSF links
    links <- osf_links(paper)
    unique_links <- setdiff(unique(links$text), BADGE_REPOS)

    if (length(unique_links) == 0) {
      stop("no OSF data links found")
    }

    target_dir <- file.path(DATA_DIR, pid)

    # If already downloaded and non-empty, skip
    if (dir.exists(target_dir)) {
      files <- list.files(target_dir, recursive = TRUE)
      if (length(files) > 0) {
        message("  already downloaded (", length(files), " files found)")
        append_progress_row(pid, TRUE, NA_character_)
        next
      }
      # Empty directory — delete and retry
      message("  empty folder found — deleting and retrying: ", target_dir)
      unlink(target_dir, recursive = TRUE)
    }

    # Download files
    message("  downloading from ", length(unique_links), " link(s)...")
    if (!is.null(DOWNLOAD_TIMEOUT_SEC)) {
      setTimeLimit(elapsed = DOWNLOAD_TIMEOUT_SEC, transient = TRUE)
      on.exit(setTimeLimit(elapsed = Inf, transient = FALSE), add = TRUE)
    }
    osf_file_download(unique_links, download_to = target_dir,
                      max_download_size = 10e9, max_file_size = NULL)
    setTimeLimit(elapsed = Inf, transient = FALSE)

    # Verify download worked
    if (!dir.exists(target_dir)) {
      stop("download produced no directory")
    }
    files <- list.files(target_dir, recursive = TRUE)
    if (length(files) == 0) {
      stop("directory exists but contains no files after retry")
    }

    message("  ✓ downloaded ", length(files), " file(s)")
    list(success = TRUE, error = NA_character_)
  }, error = function(e) {
    message("  ✗ FAILED: ", conditionMessage(e))
    list(success = FALSE, error = conditionMessage(e))
  })

  append_progress_row(pid, result$success, result$error)
}

# ── Print summary ─────────────────────────────────────────────────────────────

progress_df <- read.csv(PROGRESS_CSV, stringsAsFactors = FALSE,
                        colClasses = c(paper_id = "character"))

cat("\n\n")
cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║                  DOWNLOAD SUMMARY                                    ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n\n")

n_ok   <- sum(progress_df$success)
n_fail <- nrow(progress_df) - n_ok

cat(sprintf("── Success rate: %d / %d  (%.0f%%)\n",
            n_ok, nrow(progress_df), 100 * n_ok / nrow(progress_df)))

if (n_fail > 0) {
  cat("\n── Failed downloads:\n")
  fails <- progress_df[!progress_df$success, ]
  for (j in seq_len(nrow(fails))) {
    cat(sprintf("   %s — %s\n", fails$paper_id[j], fails$error[j]))
  }
}

cat("\n── Progress saved to: ", PROGRESS_CSV, "\n")
cat("── Files downloaded to: ", DATA_DIR, "\n")
