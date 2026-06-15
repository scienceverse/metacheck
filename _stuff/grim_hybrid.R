# Hybrid GRIM N-recovery (slice-based, no full text, no invented means)
#
# Design (preserves the safety property "LLM never invents a number to accuse"):
#   1. REGEX extracts the verbatim mean strings (e.g. "2.63"). The LLM is never
#      asked what the means are, so it cannot hallucinate a mean to flag.
#   2. For a mean lacking a reliable same-sentence N, we build a TARGETED SLICE:
#        - participant/method sentences (where total & subgroup N live)
#        - the local window around that mean (+/- 2 sentences)
#      Median slice ~3.3k chars (15x smaller than full text), fits an 8k ollama
#      context in 96% of psychsci papers -> runs on constrained RAM / no GPU.
#   3. The LLM is asked ONLY: for THIS exact mean, what n was it computed from,
#      and is the data integer? It must return one of the n values present in
#      the slice (or 0). We then GRIM-check (mean_string, n) deterministically.
#
# This file is a standalone experiment/validation harness, not yet wired into
# the module.
suppressMessages(library(metacheck)); suppressMessages(library(dplyr))
llm_use(TRUE); llm_model("ollama/llama3.1:8b"); llm_max_calls(80)

mean_rx <- "\\bM(?:ean)?\\s*[=:]\\s*-?(\\d+\\.\\d{2,3})\\b"
n_sentence_rx <- paste0(
  "\\b\\d{1,4}\\s+(?:[a-z-]+\\s+){0,3}(participant|subject|respondent|",
  "student|undergraduate|adult|child|children|volunteer|patient|infant|",
  "woman|women|man|men|individual|people)\\w*",
  "|\\b[Nn]s?\\s*=\\s*\\d", "|sample\\s+of\\s+\\d")

# build the targeted slice for one paper (sentence table -> slice text)
build_slice <- function(tt, mean_idx, max_chars = 7000) {
  is_nsent <- grepl(n_sentence_rx, tt$text, perl = TRUE, ignore.case = TRUE) &
    (tt$section_type %in% c("method", "intro", "results", "abstract") |
       is.na(tt$section_type))
  window <- unlist(lapply(mean_idx, \(m) (m - 2):(m + 1)))
  idx <- sort(unique(c(which(is_nsent), window)))
  idx <- idx[idx >= 1 & idx <= nrow(tt)]
  txt <- paste(sprintf("[s%d] %s", idx, tt$text[idx]), collapse = "\n")
  substr(txt, 1, max_chars)
}

# regex Ns present in a slice (the LLM must choose from these)
slice_ns <- function(slice) {
  pats <- c("\\b[Nn]s?\\s*=\\s*(\\d{1,4})\\b",
            "\\b(\\d{1,4})\\s+(?:[a-z-]+\\s+){0,3}(?:participant|subject|student|adult|child|volunteer|woman|women|man|men|infant|patient|people|individual)\\w*",
            "sample\\s+of\\s+(\\d{1,4})")
  out <- integer(0)
  for (pp in pats) {
    m <- gregexpr(pp, slice, perl = TRUE, ignore.case = TRUE)[[1]]
    if (m[[1]] == -1) next
    out <- c(out, as.integer(sub(pp, "\\1", regmatches(slice, list(m))[[1]],
                                 perl = TRUE, ignore.case = TRUE)))
  }
  unique(out[!is.na(out) & out >= 5 & out <= 10000])
}

# ONE job only: the LLM supplies the N (the part regex/proximity fail at).
# Data-type (integer vs continuous) stays with the module's deterministic
# unit filter + the separate shipped LLM integer-FILTER, because the 8B model
# is unreliable at integer/continuous typing when asked in the abstract
# (it defaults to "false" even for clear Likert means -- confirmed round 2 & 4).
sys <- paste0(
  "You are given an excerpt from a psychology paper and ONE target mean ",
  "copied verbatim from it. Return the sample size n that THIS specific mean ",
  "was computed from: the number of participants/observations for that ",
  "group, condition, or sample. If the mean is for a subgroup, give that ",
  "subgroup's n. The number must appear in the excerpt. Use 0 if you cannot ",
  "determine it.\n",
  "Return ONLY JSON: {\"n\": <number>}")

# recover n for one extracted mean via the slice + LLM
recover_n <- function(slice, mean_string) {
  prompt <- sprintf("TARGET MEAN: M = %s\n\nEXCERPT:\n%s", mean_string, slice)
  ans <- tryCatch(
    metacheck:::.llm_ollama_native(prompt, sys, model = "llama3.1:8b",
      think = FALSE, options = list(num_ctx = 8192, seed = 8675309)),
    error = \(e) NULL)
  if (is.null(ans)) return(NA_integer_)
  j <- sub(".*?(\\{.*\\}).*", "\\1", ans, perl = TRUE)
  parsed <- tryCatch(jsonlite::fromJSON(j), error = \(e) NULL)
  if (is.null(parsed)) return(NA_integer_)
  suppressWarnings(as.integer(parsed$n %||% NA))
}

# deterministic data-type gate (mirror of the module's unit filter): a mean
# is GRIM-eligible unless its sentence shows non-integer units/contexts
noninteger_unit_rx <- paste0(
  "\\bM\\s*[=:]\\s*-?\\d+\\.\\d+\\s*(ms|msec|s|sec|seconds|min|cm|mm|kg|",
  "[Hh]z|[µu]V|mV|%|°)\\b",
  "|reaction time|response time|latenc|amplitude|µV|\\buV\\b|",
  "proportion|\\brates?\\b|difference score")

# end-to-end on a paper: regex means -> slice -> LLM n -> GRIM
run_paper <- function(p) {
  tt <- text_search(p, ".*", return = "sentence")
  if (nrow(tt) == 0) return(NULL)
  tt <- tt[order(tt$text_id), ]
  mean_idx <- which(grepl(mean_rx, tt$text, perl = TRUE))
  if (!length(mean_idx)) return(NULL)
  slice <- build_slice(tt, mean_idx)
  valid_ns <- slice_ns(slice)

  out <- list()
  for (mi in mean_idx) {
    sent <- tt$text[mi]
    # deterministic data-type gate: skip means in non-integer-unit contexts
    elig <- !grepl(noninteger_unit_rx, sent, perl = TRUE, ignore.case = TRUE)
    ms <- regmatches(sent, gregexpr(mean_rx, sent, perl = TRUE))[[1]]
    ms <- sub(mean_rx, "\\1", ms, perl = TRUE)
    for (m in ms) {
      llm_n <- recover_n(slice, m)
      # SAFETY: only accept an n the regex actually found in the slice
      n_ok <- !is.na(llm_n) && llm_n %in% valid_ns
      testable <- n_ok && elig && llm_n < 10^nchar(sub("^\\d+\\.", "", m))
      grim_ok <- if (testable)
        tryCatch(scrutiny::grim(m, as.integer(llm_n)), error = \(e) NA) else NA
      out[[length(out) + 1]] <- data.frame(
        paper_id = p$paper_id, mean = m, llm_n = llm_n,
        n_in_slice = n_ok, eligible = elig, grim_ok = grim_ok)
    }
  }
  do.call(rbind, out)
}

ids <- c("0956797616667447", "09567976211049439", "0956797620954815")
for (id in ids) {
  cat("\n=== paper", id, "===\n")
  r <- run_paper(psychsci[[id]])
  if (is.null(r)) { cat("  none\n"); next }
  print(r, row.names = FALSE)
  flagged <- r[r$grim_ok %in% FALSE, ]
  if (nrow(flagged)) {
    cat("FLAGGED (integer + n-in-slice + GRIM-inconsistent):\n")
    print(flagged[, c("mean","llm_n")], row.names = FALSE)
  }
}
