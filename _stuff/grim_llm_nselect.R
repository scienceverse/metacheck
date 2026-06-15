# Can the 8B model SELECT the correct N among earlier candidates?
# Ground truth: means with a known same-sentence n=. We hide that n, give the
# model the mean-sentence + earlier candidate Ns (each with its sentence),
# and ask it to pick the N for that specific mean.
suppressMessages(library(metacheck))
suppressMessages(library(dplyr))
llm_use(TRUE); llm_model("ollama/llama3.1:8b"); llm_max_calls(60)

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
  "CANDIDATE sample sizes found earlier in the paper, each with the sentence",
  "it came from. Pick the candidate whose n is the number of observations",
  "the TARGET mean was computed from. If the mean is for a subgroup, pick",
  "that subgroup's n. If none clearly applies, choose 0.",
  "Answer with JSON: {\"choice\": <candidate number, or 0>}.")

# build ground-truth tasks (cap candidates to the 8 nearest earlier)
build_tasks <- function() {
  tasks <- list()
  for (p in psychsci) {
    tt <- text_search(p); if (nrow(tt) == 0) next
    tt <- tt[order(tt$text_id), ]
    tt$par_ns <- lapply(tt$text, n_in_sentence)
    tt$has_n <- lengths(tt$par_ns) > 0
    for (r in which(grepl(mean_rx, tt$text, perl = TRUE))) {
      truth <- n_eq(tt$text[r]); if (length(truth) == 0) next
      earlier <- which(tt$has_n & seq_len(nrow(tt)) < r)
      if (!length(earlier)) next
      earlier <- tail(earlier, 8)               # nearest 8
      cand_n  <- lapply(earlier, \(e) tt$par_ns[[e]])
      cand_tx <- tt$text[earlier]
      # hide the target n from the mean sentence
      mean_sent <- gsub("\\b[Nn]s?\\s*=\\s*\\d{1,4}\\b", "n = [hidden]",
                        tt$text[r])
      cand_lines <- sapply(seq_along(earlier), \(j) sprintf(
        "%d. n=%s | %s", j, paste(cand_n[[j]], collapse = "/"),
        substr(gsub("\\s+", " ", cand_tx[j]), 1, 160)))
      tasks[[length(tasks) + 1]] <- list(
        truth = truth,
        cand_n = cand_n,
        text = sprintf("SENTENCE: %s\n\nCANDIDATES:\n%s",
                       substr(gsub("\\s+", " ", mean_sent), 1, 200),
                       paste(cand_lines, collapse = "\n")))
    }
  }
  tasks
}

tasks <- build_tasks()
cat("ground-truth selection tasks:", length(tasks), "\n")

res <- llm(text = sapply(tasks, `[[`, "text"), system_prompt = sys,
           params = list(seed = 8675309))
res <- json_expand(res, suffix = c("", ".llm"))
choice <- suppressWarnings(as.integer(res$choice))

correct <- 0; nearest_correct <- 0
for (i in seq_along(tasks)) {
  ch <- choice[i]
  picked <- if (!is.na(ch) && ch >= 1 && ch <= length(tasks[[i]]$cand_n))
    tasks[[i]]$cand_n[[ch]] else integer(0)
  if (any(tasks[[i]]$truth %in% picked)) correct <- correct + 1
  # nearest = last candidate
  if (any(tasks[[i]]$truth %in% tasks[[i]]$cand_n[[length(tasks[[i]]$cand_n)]]))
    nearest_correct <- nearest_correct + 1
}
cat(sprintf("LLM picks correct N:      %d/%d (%.0f%%)\n",
            correct, length(tasks), 100*correct/length(tasks)))
cat(sprintf("Nearest-earlier baseline: %d/%d (%.0f%%)\n",
            nearest_correct, length(tasks), 100*nearest_correct/length(tasks)))
