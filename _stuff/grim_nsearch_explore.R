# Explore: where do sample sizes live relative to the means we want to check?
# Premise to test: for a flagged/checkable mean WITHOUT a same-sentence N,
# is the correct N recoverable from a "participant sentence" earlier in the
# paper, and is the *nearest-earlier* such N the right one?
suppressMessages(library(metacheck))
suppressMessages(library(dplyr))

# --- participant-sentence detector ---------------------------------------
# subject nouns x reporting verbs/contexts, plus an integer
subj <- paste0("\\b(participant|subject|respondent|student|undergraduate|",
               "adult|child|children|individual|people|person|volunteer|",
               "patient|infant|toddler|sample|男|woman|women|man|men)\\w*")
verb <- paste0("\\b(participat|recruit|collect|respond|complet|took part|",
               "take part|enroll|sampl|consent|compr7s|comprised|consist|",
               "includ|tested|ran|run|analy|final|total|remain)\\w*")
num  <- "\\b\\d{1,4}\\b"

# a mean we'd want to GRIM-check: M = x.xx with 2 decimals
mean_rx <- "\\bM(ean)?\\s*[=:]\\s*-?\\d+\\.\\d{2}\\b"

# extract a plausible participant N from a sentence (reuse a light version)
n_in_sentence <- function(s) {
  # n = NN  OR  NN <subjnoun>  OR  sample of NN
  pats <- c("\\b[Nn]s?\\s*=\\s*(\\d{1,4})\\b",
            paste0("\\b(\\d{1,4})\\s+(?:[A-Za-z-]+\\s+){0,2}", subj),
            paste0("samples?\\s+of\\s+(\\d{1,4})"))
  out <- integer(0)
  for (p in pats) {
    m <- gregexpr(p, s, perl = TRUE, ignore.case = TRUE)[[1]]
    if (m[[1]] == -1) next
    out <- c(out, as.integer(sub(p, "\\1", regmatches(s, list(m))[[1]],
                                 perl = TRUE, ignore.case = TRUE)))
  }
  unique(out[!is.na(out) & out >= 5 & out <= 5000])
}

# --- sweep psychsci ------------------------------------------------------
# For each paper: get the ordered text table, locate mean-bearing sentences
# and participant sentences, and for each mean with NO same-sentence N,
# record the nearest-earlier participant N (and how far away).
results <- lapply(psychsci, function(p) {
  tt <- text_search(p)               # ordered sentences with text_id
  if (nrow(tt) == 0) return(NULL)
  tt <- tt[order(tt$text_id), ]
  tt$has_mean <- grepl(mean_rx, tt$text, perl = TRUE)
  tt$par_ns   <- lapply(tt$text, n_in_sentence)
  tt$has_n    <- lengths(tt$par_ns) > 0

  mean_rows <- which(tt$has_mean)
  if (length(mean_rows) == 0) return(NULL)

  lapply(mean_rows, function(r) {
    same_n <- tt$par_ns[[r]]
    # nearest earlier participant-N sentence
    earlier <- which(tt$has_n & seq_len(nrow(tt)) < r)
    nearest <- if (length(earlier)) max(earlier) else NA_integer_
    data.frame(
      paper_id = p$paper_id,
      mean_text_id = tt$text_id[r],
      same_sentence_n = length(same_n) > 0,
      n_earlier_sources = length(earlier),
      nearest_gap_sentences = if (is.na(nearest)) NA_integer_ else r - nearest,
      nearest_n = if (is.na(nearest)) NA_character_
                  else paste(tt$par_ns[[nearest]], collapse = ";")
    )
  }) |> do.call(rbind, args = _)
}) |> do.call(rbind, args = _)

cat("=== Mean-bearing sentences across psychsci ===\n")
cat("total mean sentences:", nrow(results), "\n")
cat("with same-sentence N:", sum(results$same_sentence_n),
    sprintf("(%.0f%%)\n", 100*mean(results$same_sentence_n)))

no_same <- results[!results$same_sentence_n, ]
cat("\n=== Means WITHOUT same-sentence N (the hard cases):",
    nrow(no_same), "===\n")
cat("have >=1 earlier participant-N sentence:",
    sum(no_same$n_earlier_sources > 0),
    sprintf("(%.0f%%)\n", 100*mean(no_same$n_earlier_sources > 0)))
cat("\ndistance (sentences) to nearest earlier participant-N:\n")
print(summary(no_same$nearest_gap_sentences))
cat("\nnumber of competing earlier N-sources before the mean:\n")
print(summary(no_same$n_earlier_sources))
cat("\n% where the single nearest-earlier sentence has exactly one N:\n")
one_n <- !grepl(";", no_same$nearest_n) & !is.na(no_same$nearest_n)
cat(sprintf("%.0f%%\n", 100*mean(one_n)))

saveRDS(results, "_stuff/grim_nsearch_results.rds")
