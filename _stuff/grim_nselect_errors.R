suppressMessages(library(metacheck)); suppressMessages(library(dplyr))
llm_use(TRUE); llm_model("ollama/llama3.1:8b"); llm_max_calls(60)

subj <- paste0("\\b(participant|subject|respondent|student|undergraduate|",
  "adult|child|children|individual|people|person|volunteer|",
  "patient|infant|toddler|sample|woman|women|man|men)\\w*")
n_in_sentence <- function(s) {
  pats <- c("\\b[Nn]s?\\s*=\\s*(\\d{1,4})\\b",
            paste0("\\b(\\d{1,4})\\s+(?:[A-Za-z-]+\\s+){0,2}", subj),
            "samples?\\s+of\\s+(\\d{1,4})")
  out <- integer(0)
  for (pp in pats) {
    m <- gregexpr(pp, s, perl = TRUE, ignore.case = TRUE)[[1]]
    if (m[[1]] == -1) next
    out <- c(out, as.integer(sub(pp, "\\1", regmatches(s, list(m))[[1]],
                                 perl = TRUE, ignore.case = TRUE)))
  }
  unique(out[!is.na(out) & out >= 5 & out <= 5000])
}
n_eq <- function(s) {
  m <- gregexpr("\\b[Nn]s?\\s*=\\s*(\\d{1,4})\\b", s, perl = TRUE)[[1]]
  if (m[[1]] == -1) return(integer(0))
  as.integer(sub("\\b[Nn]s?\\s*=\\s*(\\d{1,4})\\b", "\\1",
                 regmatches(s, list(m))[[1]], perl = TRUE))
}
mean_rx <- "\\bM(ean)?\\s*[=:]\\s*-?\\d+\\.\\d{2}\\b"

sys <- paste(
  "A mean from a psychology paper needs its sample size (n) to check it.",
  "You are given the SENTENCE containing the mean, and a numbered list of",
  "CANDIDATE sample sizes found earlier in the paper, each with its sentence.",
  "Pick the candidate whose n is the number of observations the TARGET mean",
  "was computed from. If the mean is for a subgroup, pick that subgroup's n.",
  "If none clearly applies, choose 0.",
  "Answer with JSON: {\"choice\": <candidate number, or 0>}.")

tasks <- list()
for (p in psychsci) {
  tt <- text_search(p); if (nrow(tt) == 0) next
  tt <- tt[order(tt$text_id), ]
  tt$par_ns <- lapply(tt$text, n_in_sentence); tt$has_n <- lengths(tt$par_ns) > 0
  for (r in which(grepl(mean_rx, tt$text, perl = TRUE))) {
    truth <- n_eq(tt$text[r]); if (length(truth) == 0) next
    earlier <- which(tt$has_n & seq_len(nrow(tt)) < r); if (!length(earlier)) next
    earlier <- tail(earlier, 8)
    cand_n <- lapply(earlier, \(e) tt$par_ns[[e]]); cand_tx <- tt$text[earlier]
    mean_sent <- gsub("\\b[Nn]s?\\s*=\\s*\\d{1,4}\\b", "n=[hidden]", tt$text[r])
    lines <- sapply(seq_along(earlier), \(j) sprintf("%d. n=%s | %s", j,
      paste(cand_n[[j]], collapse = "/"),
      substr(gsub("\\s+", " ", cand_tx[j]), 1, 150)))
    tasks[[length(tasks) + 1]] <- list(truth = truth, cand_n = cand_n,
      truth_in_cands = any(truth %in% unlist(cand_n)),
      mean_sent = substr(gsub("\\s+", " ", mean_sent), 1, 180),
      text = sprintf("SENTENCE: %s\n\nCANDIDATES:\n%s",
        substr(gsub("\\s+", " ", mean_sent), 1, 180), paste(lines, collapse = "\n")))
  }
}
res <- llm(text = sapply(tasks, `[[`, "text"), system_prompt = sys,
           params = list(seed = 8675309))
res <- json_expand(res, suffix = c("", ".llm"))
choice <- suppressWarnings(as.integer(res$choice))

cat("=== Per-case errors ===\n")
for (i in seq_along(tasks)) {
  ti <- tasks[[i]]; ch <- choice[i]
  picked <- if (!is.na(ch) && ch >= 1 && ch <= length(ti$cand_n)) ti$cand_n[[ch]] else integer(0)
  if (!any(ti$truth %in% picked)) {
    cat(sprintf("[MISS] truth=%s picked=%s truth_in_cands=%s\n   %s\n",
      paste(ti$truth, collapse = "/"), paste(picked, collapse = "/"),
      ti$truth_in_cands, ti$mean_sent))
  }
}
cat(sprintf("\nCeiling: truth was among candidates in %d/%d cases (%.0f%%)\n",
  sum(sapply(tasks, `[[`, "truth_in_cands")), length(tasks),
  100 * mean(sapply(tasks, `[[`, "truth_in_cands"))))
