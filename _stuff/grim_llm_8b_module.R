suppressMessages(library(metacheck)); suppressMessages(library(dplyr))
source("inst/modules/grim.R")
llm_use(TRUE); llm_model("ollama/llama3.1:8b"); llm_max_calls(40)

ids <- c("0956797620954815","0956797617702699",
         "0956797616667447","0956797616634654")
sub <- psychsci[ids]
det <- grim(sub)
flagged <- det$table[det$table$consistent %in% FALSE, ]
cat("deterministic flags:", nrow(flagged), "\n")

r <- grim(sub, use_llm = TRUE)
t <- r$table
cat("with 8B LLM filter -> flags remaining:", sum(t$consistent %in% FALSE),
    "| suppressed:", sum(t$llm_noninteger %in% TRUE), "\n")
cat(r$traffic_light, "|", r$summary_text, "\n\n")
chk <- data.frame(paper = flagged$paper_id, mean = flagged$reported,
                  n_source = flagged$n_source)
chk$verdict <- ifelse(
  t$llm_noninteger[match(paste(flagged$paper_id, flagged$reported),
                         paste(t$paper_id, t$reported))] %in% TRUE,
  "SUPPRESSED", "KEPT (flag)")
print(chk, row.names = FALSE)
