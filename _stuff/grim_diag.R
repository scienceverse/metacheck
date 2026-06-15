# Diagnose GRIM extraction recall on psychsci
suppressMessages(library(metacheck))
suppressMessages(library(dplyr))

mean_rx <- "\\b(?:M|Mage|[Mm]ean(?:\\s+age)?)\\s*[=:]\\s*[-−]?\\s*(\\d+\\.\\d+)"
n_rx <- "\\b[Nn]s?\\s*=\\s*(\\d{1,3}(?:,\\d{3})*)\\b"

s1 <- text_search(psychsci, mean_rx, return = "sentence",
                  perl = TRUE, ignore.case = FALSE)
cat("sentences with mean pattern:", nrow(s1), "\n")
s2 <- text_search(s1, n_rx, return = "sentence",
                  perl = TRUE, ignore.case = FALSE)
cat("  + n pattern in same sentence:", nrow(s2), "\n")

# what does mean/SD reporting actually look like?
sd_sent <- text_search(psychsci, "\\bSDs?\\b", return = "sentence", perl = TRUE)
cat("sentences containing SD:", nrow(sd_sent), "\n\n")
set.seed(1)
cat("--- sample SD sentences ---\n")
samp <- sample(sd_sent$text, 12)
for (s in samp) cat("*", substr(gsub("\\s+", " ", s), 1, 170), "\n")

# how is M reported - check for mangled equals signs
cat("\nM with normal '=':",
    nrow(text_search(psychsci, "\\bM\\s*=\\s*\\d", return = "match", perl = TRUE)), "\n")
cat("M with any non-word char then digits:", "\n")
m_any <- text_search(psychsci, "\\bM\\s*[^\\w\\s]{1,2}\\s*\\d+\\.\\d+", return = "match", perl = TRUE)
cat("  count:", nrow(m_any), "\n")
ops <- gsub(".*?M\\s*([^\\w\\s]{1,2}).*", "\\1", m_any$text, perl = TRUE)
print(sort(table(ops), decreasing = TRUE))

# n reporting variants
cat("\nn= variants:", nrow(text_search(psychsci, n_rx, return = "match", perl = TRUE)), "\n")
n_any <- text_search(psychsci, "\\b[Nn]s?\\s*[^\\w\\s]{1,2}\\s*\\d", return = "match", perl = TRUE)
nops <- gsub(".*?[Nn]s?\\s*([^\\w\\s]{1,2}).*", "\\1", n_any$text, perl = TRUE)
print(sort(table(nops), decreasing = TRUE) |> head(8))
