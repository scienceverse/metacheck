# Does "nearest-earlier participant-N" recover the CORRECT N?
# Ground truth = means that have a same-sentence N (we know the answer).
# Hide it, then check whether nearest-earlier participant-N matches.
suppressMessages(library(metacheck))
suppressMessages(library(dplyr))

subj <- paste0("\\b(participant|subject|respondent|student|undergraduate|",
               "adult|child|children|individual|people|person|volunteer|",
               "patient|infant|toddler|sample|woman|women|man|men)\\w*")
n_in_sentence <- function(s) {
  pats <- c("\\b[Nn]s?\\s*=\\s*(\\d{1,4})\\b",
            paste0("\\b(\\d{1,4})\\s+(?:[A-Za-z-]+\\s+){0,2}", subj),
            "samples?\\s+of\\s+(\\d{1,4})")
  out <- integer(0)
  for (p in pats) {
    m <- gregexpr(p, s, perl = TRUE, ignore.case = TRUE)[[1]]
    if (m[[1]] == -1) next
    out <- c(out, as.integer(sub(p, "\\1", regmatches(s, list(m))[[1]],
                                 perl = TRUE, ignore.case = TRUE)))
  }
  unique(out[!is.na(out) & out >= 5 & out <= 5000])
}
n_eq_in_sentence <- function(s) {  # strict same-sentence "n = NN" ground truth
  m <- gregexpr("\\b[Nn]s?\\s*=\\s*(\\d{1,4})\\b", s, perl = TRUE)[[1]]
  if (m[[1]] == -1) return(integer(0))
  as.integer(sub("\\b[Nn]s?\\s*=\\s*(\\d{1,4})\\b", "\\1",
                 regmatches(s, list(m))[[1]], perl = TRUE))
}
mean_rx <- "\\bM(ean)?\\s*[=:]\\s*-?\\d+\\.\\d{2}\\b"

hits <- 0; in_set <- 0; total <- 0
gaps <- integer(0)
for (p in psychsci) {
  tt <- text_search(p); if (nrow(tt) == 0) next
  tt <- tt[order(tt$text_id), ]
  tt$par_ns <- lapply(tt$text, n_in_sentence)
  tt$has_n  <- lengths(tt$par_ns) > 0

  for (r in which(grepl(mean_rx, tt$text, perl = TRUE))) {
    truth <- n_eq_in_sentence(tt$text[r])      # need a known same-sentence n=
    if (length(truth) == 0) next
    total <- total + 1
    earlier <- which(tt$has_n & seq_len(nrow(tt)) < r)
    if (!length(earlier)) next
    nearest <- max(earlier)
    gaps <- c(gaps, r - nearest)
    cand_nearest <- tt$par_ns[[nearest]]
    all_earlier  <- unlist(tt$par_ns[earlier])
    if (any(truth %in% cand_nearest)) hits <- hits + 1   # nearest is right
    if (any(truth %in% all_earlier))  in_set <- in_set + 1  # somewhere earlier
  }
}

cat("Ground-truth means (same-sentence n= known):", total, "\n")
cat(sprintf("Correct N is the NEAREST earlier participant-N: %d (%.0f%%)\n",
            hits, 100*hits/total))
cat(sprintf("Correct N is among ANY earlier participant-N: %d (%.0f%%)\n",
            in_set, 100*in_set/total))
cat("\nGap (sentences) from mean to nearest earlier participant-N:\n")
print(summary(gaps))
