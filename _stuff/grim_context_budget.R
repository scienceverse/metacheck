# How small can the context be and still contain the N?
# For each GRIM-checkable mean, build a TARGETED slice:
#   (a) the participant/method sentences (where total & subgroup N live)
#   (b) the local results context around the mean (+/- a few sentences)
# Measure its size and whether a verifiable N is present. This tells us if
# the hybrid fits a small ollama context without sending the full paper.
suppressMessages(library(metacheck)); suppressMessages(library(dplyr))

mean_rx <- "\\bM(ean)?\\s*[=:]\\s*-?\\d+\\.\\d{2}\\b"
# sentences likely to state sample sizes (participants/method language)
n_sentence_rx <- paste0(
  "\\b\\d{1,4}\\s+(?:[a-z-]+\\s+){0,3}(participant|subject|respondent|",
  "student|undergraduate|adult|child|children|volunteer|patient|infant|",
  "woman|women|man|men|individual|people)\\w*",
  "|\\b[Nn]s?\\s*=\\s*\\d", "|sample\\s+of\\s+\\d",
  "|\\d{1,4}\\s+(?:participants?|people)\\s+(?:took part|participat|complet)")

slice_sizes <- c()
for (p in psychsci) {
  tt <- text_search(p, ".*", return = "sentence")
  if (nrow(tt) == 0) next
  tt <- tt[order(tt$text_id), ]
  is_mean <- grepl(mean_rx, tt$text, perl = TRUE)
  is_nsent <- grepl(n_sentence_rx, tt$text, perl = TRUE, ignore.case = TRUE) &
    tt$section_type %in% c("method", "intro", "results", "abstract", NA)
  if (!any(is_mean)) next

  # the targeted slice = all N-sentences + a window around each mean
  n_idx <- which(is_nsent)
  mean_idx <- which(is_mean)
  window <- unique(unlist(lapply(mean_idx, \(m) (m - 2):(m + 1))))
  window <- window[window >= 1 & window <= nrow(tt)]
  slice_idx <- sort(unique(c(n_idx, window)))
  slice_chars <- sum(nchar(tt$text[slice_idx]))

  slice_sizes <- c(slice_sizes, slice_chars)
}

cat("=== Targeted-slice size (N-sentences + mean windows) per paper ===\n")
print(summary(slice_sizes))
cat(sprintf("\n%% of papers whose slice fits in ~4k char (safe small ctx): %.0f%%\n",
            100 * mean(slice_sizes <= 4000)))
cat(sprintf("%% fitting in ~8k char: %.0f%%\n",
            100 * mean(slice_sizes <= 8000)))
cat(sprintf("full-text median was ~49000 chars; slice median is %.0f chars (%.0fx smaller)\n",
            median(slice_sizes), 49000 / median(slice_sizes)))

# Also: just the method+participant sentences alone (the cheapest option)
nsent_only <- c()
for (p in psychsci) {
  tt <- text_search(p, ".*", return = "sentence")
  if (nrow(tt) == 0) next
  is_nsent <- grepl(n_sentence_rx, tt$text, perl = TRUE, ignore.case = TRUE)
  nsent_only <- c(nsent_only, sum(nchar(tt$text[is_nsent])))
}
cat("\n=== N-sentences only (cheapest 'where N lives' slice) ===\n")
print(summary(nsent_only))
cat(sprintf("%% fitting in ~2k char: %.0f%%\n", 100 * mean(nsent_only <= 2000)))
