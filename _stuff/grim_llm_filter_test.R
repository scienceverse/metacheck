suppressMessages(library(metacheck)); suppressMessages(library(dplyr))
source("inst/modules/grim.R")
llm_use(TRUE); llm_model("ollama/qwen2.5:3b"); llm_max_calls(40)

# papers with known FP types (EEG amplitudes, stimulus props) + a real TP (age)
ids <- c("0956797620954815",  # EEG amplitudes (uV) - should be suppressed
         "0956797617702699",  # word stimulus ratings - mixed
         "0956797616667447",  # working-memory genuine flag - should be KEPT
         "0956797616634654")  # mean age genuine flag - should be KEPT
sub <- psychsci[ids]

cat("=== deterministic (use_llm = FALSE) ===\n")
det <- grim(sub)
cat("flagged:", sum(det$table$consistent %in% FALSE), "\n\n")

cat("=== with LLM filter (use_llm = TRUE) ===\n")
llmf <- grim(sub, use_llm = TRUE)
t <- llmf$table
cat("flagged:", sum(t$consistent %in% FALSE),
    "| suppressed:", sum(t$llm_noninteger %in% TRUE), "\n\n")
cat(llmf$summary_text, "\n\n")

# show what got suppressed vs kept
chk <- det$table[det$table$consistent %in% FALSE,
                 c("paper_id","reported","n_source")]
chk$llm_noninteger <- t$llm_noninteger[match(
  paste(chk$paper_id, chk$reported),
  paste(t$paper_id, t$reported))]
chk$verdict <- ifelse(chk$llm_noninteger %in% TRUE, "SUPPRESSED", "kept")
print(chk, row.names = FALSE)
