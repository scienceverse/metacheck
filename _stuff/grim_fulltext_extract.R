# Your idea, properly tested: send the FULL paper text to the LLM and have
# it return each reported mean paired with (a) the sample size that mean was
# computed from and (b) whether the data are integers. One pass per paper.
# Uses free-text JSON (the installed llm() has a bug in structured `type=`
# output via ollama: `%%` instead of `&&`), parsed with jsonlite.
suppressMessages(library(metacheck)); suppressMessages(library(dplyr))
llm_use(TRUE); llm_model("ollama/llama3.1:8b"); llm_max_calls(60)

# preflight: full-text extraction needs the 8B model at >=16k context (~6.5 GiB)
free_mb <- tryCatch(
  as.numeric(gsub("[^0-9]", "", grep("Free",
    system("wmic OS get FreePhysicalMemory /value", intern = TRUE),
    value = TRUE))) / 1024, error = \(e) NA)
if (!is.na(free_mb) && free_mb < 6500) {
  message(sprintf(
    "WARNING: only %d MB free RAM; llama3.1:8b at 16k context needs ~6500 MB.\nExpect HTTP 500. Free RAM or use a larger machine, then rerun.",
    round(free_mb)))
}

sys <- paste0(
  "You extract reported means from a psychology paper for a numerical ",
  "consistency check (GRIM). For EACH mean reported in the text (written ",
  "like 'M = 4.32' or 'mean = 4.32'), output an object with:\n",
  "- \"mean\": the mean exactly as printed (keep trailing zeros, as a string)\n",
  "- \"n\": the integer sample size THIS mean was computed from. Find the ",
  "number of participants/observations for that specific group, condition, ",
  "or sample, which may be stated far away (e.g. in the Method section or a ",
  "subgroup description). Use 0 if you cannot determine it.\n",
  "- \"data_type\": \"integer\" if the values averaged are whole numbers ",
  "(Likert ratings, counts, items correct, age in whole years); ",
  "\"continuous\" otherwise (reaction times, EEG amplitudes, proportions, ",
  "physical measurements, scores averaged across multiple scale items)\n",
  "- \"variable\": a few words naming what was measured\n",
  "Skip test statistics, confidence intervals, SDs, and percentages. ",
  "Return ONLY a JSON array of these objects, bracketed by ```json and ```.")

# NOTE: a full psychsci paper is ~15k tokens, so ollama needs num_ctx >= 16384,
# which for llama3.1:8b needs ~6.5 GiB free RAM. If the model errors with
# HTTP 500 / "more system memory than available", free RAM or use a machine
# with more memory (this is the documented blocker, not an accuracy result).
# We call the native ollama API directly so we can set num_ctx.
extract_paper <- function(p, max_chars = 60000, num_ctx = 16384) {
  full <- substr(paste(p$text$text, collapse = " "), 1, max_chars)
  ans <- tryCatch(
    metacheck:::.llm_ollama_native(
      text = full, system_prompt = sys, model = "llama3.1:8b",
      think = FALSE, options = list(num_ctx = num_ctx, seed = 8675309)),
    error = \(e) { message("  llm error: ", e$message); NULL })
  if (is.null(ans)) return(NULL)
  raw <- ans
  json <- sub(".*?```json\\s*", "", raw); json <- sub("\\s*```.*", "", json)
  if (!grepl("\\[", json)) json <- raw  # fallback: maybe bare array
  parsed <- tryCatch(jsonlite::fromJSON(json), error = \(e) {
    message("  parse error"); NULL })
  if (is.null(parsed) || length(parsed) == 0) return(NULL)
  parsed <- as.data.frame(parsed)
  parsed$paper_id <- p$paper_id
  parsed
}

# hand-verified expectations:
# 0956797616667447: WM means 3.03 (n=22) & 2.63 (n=31), integer Likert -> 2.63 GRIM-inconsistent
# 09567976211049439: self-esteem 2.75 (n=94) & 1.24 (n=111), integer
# 0956797620954815: EEG amplitudes in uV -> continuous (should NOT be GRIM-flagged)
ids <- c("0956797616667447", "09567976211049439", "0956797620954815")

for (id in ids) {
  cat("\n=== paper", id, "===\n")
  r <- extract_paper(psychsci[[id]])
  if (is.null(r) || nrow(r) == 0) { cat("  (no extraction)\n"); next }
  cols <- intersect(c("mean", "n", "data_type", "variable"), names(r))
  print(r[, cols], row.names = FALSE)
  # GRIM-check integer means with usable n
  if (all(c("mean","n","data_type") %in% names(r))) {
    chk <- r[r$data_type == "integer" & r$n >= 5 & r$n < 100, ]
    if (nrow(chk)) {
      chk$grim_ok <- mapply(function(m, n)
        tryCatch(scrutiny::grim(as.character(m), as.integer(n)), error=\(e) NA),
        chk$mean, chk$n)
      cat("-- GRIM on integer means --\n")
      print(chk[, c("mean","n","grim_ok","variable")], row.names = FALSE)
    }
  }
}
