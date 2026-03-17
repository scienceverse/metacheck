# run_single.R
# ─────────────────────────────────────────────────────────────────────────────
# Run the full pipeline (index + codebook label) for one randomly selected
# paper.  Useful for smoke-testing the pipeline and inspecting outputs.
#
# Usage: Rscript data_check/run_single.R
#
# Output: data_check/outputs/<paper_id>/
#           structure.csv, columns.csv   (from run_index)
#           labels.csv, codebook_coverage.csv  (from run_codebook_label)
# ─────────────────────────────────────────────────────────────────────────────

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

source("./data_check/0_index.R")
source("./data_check/2_codebook_label.R")

# ── Discover all papers ───────────────────────────────────────────────────────
# XML_DIR is defined in 0_index.R

all_ids <- tools::file_path_sans_ext(
  list.files(XML_DIR, pattern = "\\.xml$", full.names = FALSE)
)

if (length(all_ids) == 0) {
  stop("No paper IDs found in ", XML_DIR)
}

# IDs must stay as character strings — no numeric coercion
pid <- sample(all_ids, 1L)

cat("\n══════════════════════════════════════════════════════════════════════\n")
cat(sprintf("  Paper: %s\n", pid))
cat("══════════════════════════════════════════════════════════════════════\n\n")

# ── Stage 1: run_index ────────────────────────────────────────────────────────

KNOWN_ERROR_CODES <- c("no_links", "download_failed", "empty_repo", "too_large")

cat("── Stage 1: run_index ──────────────────────────────────────────────────\n")

t1_start <- proc.time()[["elapsed"]]

stage1 <- tryCatch(
  run_index(paper_id = pid, download = TRUE),
  error = function(e) {
    msg <- conditionMessage(e)
    list(success = FALSE, error = msg)
  }
)

t1_elapsed <- proc.time()[["elapsed"]] - t1_start

if (isFALSE(stage1$success)) {
  err <- stage1$error
  code <- if (any(startsWith(err, KNOWN_ERROR_CODES))) {
    sub(":.*$", "", err)
  } else {
    "error"
  }
  cat(sprintf("  FAILED — %s\n  %s\n", code, err))
  cat("\n  (Stage 2 skipped)\n")
  quit(save = "no", status = 1)
}

cat(sprintf(
  "  success=TRUE  files=%s  data_files=%s  columns=%s  elapsed=%.1fs\n",
  stage1$n_files %||% "NA",
  stage1$n_data_files %||% "NA",
  stage1$n_columns %||% "NA",
  t1_elapsed
))

# ── Stage 2: run_codebook_label ───────────────────────────────────────────────

cat("\n── Stage 2: run_codebook_label ─────────────────────────────────────────\n")

columns_path <- file.path(OUTPUT_DIR, pid, "columns.csv")

if (!file.exists(columns_path)) {
  cat("  Stage 2 skipped — no columns.csv\n")
  quit(save = "no", status = 0)
}

t2_start <- proc.time()[["elapsed"]]

stage2 <- tryCatch(
  run_codebook_label(paper_id = pid),
  error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  }
)

t2_elapsed <- proc.time()[["elapsed"]] - t2_start

if (isFALSE(stage2$success)) {
  cat(sprintf("  FAILED — %s\n", stage2$error))
  quit(save = "no", status = 1)
}

cat(sprintf(
  "  label_status=%s  labelled=%s  unlabelled=%s  elapsed=%.1fs\n",
  stage2$label_status %||% "NA",
  stage2$n_labelled %||% "NA",
  stage2$n_unlabelled %||% "NA",
  t2_elapsed
))

# ── Done ──────────────────────────────────────────────────────────────────────

out_dir <- file.path(OUTPUT_DIR, pid)
cat(sprintf("\n── Outputs: %s\n\n", out_dir))
