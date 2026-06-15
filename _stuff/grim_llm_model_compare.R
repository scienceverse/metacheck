suppressMessages(library(metacheck)); suppressMessages(library(dplyr))
source("inst/modules/grim.R")
llm_use(TRUE); llm_max_calls(20)

ids <- c("0956797620954815","0956797617702699",
         "0956797616667447","0956797616634654")
sub <- psychsci[ids]
det <- grim(sub)
flagged <- det$table[det$table$consistent %in% FALSE, ]

for (m in c("ollama/qwen2.5:3b","ollama/qwen3.5:4b")) {
  llm_model(m)
  r <- grim(sub, use_llm = TRUE)
  t <- r$table
  chk <- data.frame(paper = flagged$paper_id, mean = flagged$reported)
  chk$supp <- t$llm_noninteger[match(paste(flagged$paper_id, flagged$reported),
                                     paste(t$paper_id, t$reported))]
  cat("\n=== ", m, " ===\n")
  cat("flags remaining:", sum(t$consistent %in% FALSE),
      "| suppressed:", sum(t$llm_noninteger %in% TRUE), "\n")
  print(chk, row.names = FALSE)
}
