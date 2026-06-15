# Ground-truth validation of the GRIM module.
# One test paper per case so the paragraph-level N fallback cannot
# cross-contaminate cases (test_paper puts all text in one paragraph).
suppressMessages(library(metacheck))
suppressMessages(library(dplyr))
source("inst/modules/grim.R")

# each case: text (1+ sentences, same paragraph), expected flag
cases <- list(
  # --- planted inconsistencies (mean impossible for n, items = 1) ---
  list(text = "Happiness was high (M = 5.19, SD = 1.34, n = 28).", expected = TRUE),
  list(text = "The 28 participants reported high happiness (M = 5.19, SD = 1.34).", expected = TRUE),
  list(text = "Twenty-eight participants reported high happiness (M = 5.19, SD = 1.34).", expected = TRUE),
  list(text = "A sample of 28 reported high happiness (M = 5.19, SD = 1.34).", expected = TRUE),
  list(text = "Forty-nine undergraduates took part (mean age = 20.52 years).", expected = TRUE),
  list(text = "Scores were low (M = 3.51, SD = 0.4) for the group with n = 10.", expected = TRUE),
  list(text = "The 33 students rated the task (M = 7.49, SD = 1.2).", expected = TRUE),
  # df recovery: t(27) -> n = 28 or 29; 5.19 impossible for both
  list(text = "Scores were elevated (M = 5.19, SD = 1.10), t(27) = 2.30, p = .03.", expected = TRUE),
  # paragraph fallback: n in the previous sentence
  list(text = c("We recruited 28 participants for the study.",
                "Happiness was high (M = 5.19, SD = 1.34)."), expected = TRUE),
  # --- known consistent ---
  list(text = "Happiness was high (M = 5.18, SD = 1.34, n = 28).", expected = FALSE),
  list(text = "The 30 participants reported anxiety (M = 2.57, SD = 1.1).", expected = FALSE),
  list(text = "Sixty participants took part (age: M = 20.90 years, SD = 2.18).", expected = FALSE),
  list(text = "A sample of 25 rated the items (M = 4.16, SD = 0.9).", expected = FALSE),
  list(text = "Twenty-five students rated the items (M = 4.16, SD = 0.9).", expected = FALSE),
  list(text = "We tested 56 participants (mean age = 24.59 years, SD = 2.77).", expected = FALSE),
  # df recovery, consistent: 5.18 = 145/28
  list(text = "Scores were elevated (M = 5.18, SD = 1.10), t(27) = 2.30, p = .03.", expected = FALSE),
  # paragraph fallback, consistent
  list(text = c("We recruited 28 participants for the study.",
                "Happiness was high (M = 5.18, SD = 1.34)."), expected = FALSE),
  # --- FP traps: should NOT be flagged ---
  list(text = "Mean RT was 523.45 ms for the n = 28 sample.", expected = FALSE),
  list(text = "Participants earned money (M = €1.55, SD = €0.30, n = 28).", expected = FALSE),
  list(text = "We excluded 3 participants, leaving N = 100 (M = 5.19, SD = 1.1).", expected = FALSE),
  list(text = "Thirteen participants were dropped, yielding 788 (age: M = 40.04).", expected = FALSE),
  list(text = "The toddlers were young (mean age = 2.54 years; 30 toddlers took part).", expected = FALSE),
  list(text = "In Study 2 participants were accurate (M = 5.19, SD = 1.1).", expected = FALSE),
  list(text = "Infants were tested (age: M = 261.17 days; 20 infants took part).", expected = FALSE),
  list(text = "Accuracy was high (M = 5.44, SD = 1.2, N = 230).", expected = FALSE),
  # Welch fractional df must not yield an n
  list(text = "The groups differed (M = 3.51, SD = 1.1), t(23.92) = 2.45, p = .02.", expected = FALSE)
)

papers <- lapply(seq_along(cases), \(i) {
  p <- test_paper(cases[[i]]$text)
  p$paper_id <- sprintf("case_%02d", i)
  p
}) |> paperlist()

res <- grim(papers)
tbl <- res$table

expected <- sapply(cases, `[[`, "expected")
flagged_papers <- unique(tbl$paper_id[tbl$consistent %in% FALSE])
flagged <- sprintf("case_%02d", seq_along(cases)) %in% flagged_papers

hits   <- sum(expected & flagged)
misses <- sum(expected & !flagged)
fas    <- sum(!expected & flagged)
crs    <- sum(!expected & !flagged)

cat("=== Ground-truth results (", length(cases), "cases ) ===\n")
cat(sprintf("Hits: %d | Misses: %d | False alarms: %d | Correct rejections: %d\n",
            hits, misses, fas, crs))
cat(sprintf("Sensitivity: %.2f | Specificity: %.2f | Precision: %.2f\n",
            hits / (hits + misses), crs / (crs + fas),
            ifelse(hits + fas > 0, hits / (hits + fas), NA)))

err <- which(expected != flagged)
if (length(err)) {
  cat("\n--- Misclassified cases ---\n")
  for (i in err) {
    cat(sprintf("[%02d] expected %s, got %s: %s\n", i,
                expected[i], flagged[i],
                paste(cases[[i]]$text, collapse = " ")))
  }
}

# --- official interface check ---
cat("\n=== module_run() end-to-end ===\n")
op2 <- module_run(psychsci[["0956797616667447"]], "inst/modules/grim.R")
cat("flagged paper:", op2$traffic_light, "|", op2$summary_text, "\n")
cat("module_report renders:", nchar(module_report(op2)) > 0, "\n")
