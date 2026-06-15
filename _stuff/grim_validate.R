# Iterative evaluation of the GRIM module on psychsci
# Run: Rscript _stuff/grim_validate.R
suppressMessages(library(metacheck))
suppressMessages(library(dplyr))
source("inst/modules/grim.R")

res <- grim(psychsci)
tbl <- res$table

cat("=== GRIM on psychsci (250 papers) ===\n")
cat("Papers with checkable means:", length(unique(tbl$paper_id)), "\n")
cat("Checkable means:", nrow(tbl), "\n")
cat("  by n source:\n")
print(table(tbl$n_source))
cat("Flagged inconsistent:", sum(tbl$consistent == FALSE), "\n")
cat("  by n source:\n")
print(table(tbl$n_source[tbl$consistent == FALSE]))
cat("  of which consistent at items 2-4:",
    sum(tbl$consistent == FALSE & !is.na(tbl$min_items)), "\n")
cat("  inconsistent at any items 1-4:",
    sum(tbl$consistent == FALSE & is.na(tbl$min_items)), "\n\n")

flagged <- tbl[tbl$consistent == FALSE, ]
flagged <- flagged[order(flagged$n_source, flagged$paper_id), ]
cat("=== All flagged sentences (for manual precision review) ===\n")
for (i in seq_len(nrow(flagged))) {
  cat(sprintf("[%d] %s | %s | n=%s (%s) | items=%s\n    %s\n\n",
              i, flagged$paper_id[[i]], flagged$reported[[i]],
              flagged$n[[i]], flagged$n_source[[i]],
              ifelse(is.na(flagged$min_items[[i]]), ">4",
                     flagged$min_items[[i]]),
              substr(gsub("\\s+", " ", flagged$text[[i]]), 1, 280)))
}

cat("=== Sample of PASSED rows (extraction sanity) ===\n")
set.seed(42)
ok <- tbl[tbl$consistent == TRUE, ]
idx <- sample(seq_len(nrow(ok)), min(15, nrow(ok)))
for (i in idx) {
  cat(sprintf("OK %s | n=%s\n   %s\n",
              ok$reported[[i]], ok$n[[i]],
              substr(gsub("\\s+", " ", ok$text[[i]]), 1, 200)))
}

saveRDS(tbl, "_stuff/grim_psychsci_table.rds")
