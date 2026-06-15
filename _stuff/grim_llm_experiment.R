# Experiment: can a local LLM (ollama) retrieve the n / scale items /
# measure type for GRIM-flagged means from the surrounding PARAGRAPH?
# Hypothesis to test: paragraph-level context may not suffice, because
# measure details usually live in the Method/Measures section.
suppressMessages(library(metacheck))
suppressMessages(library(dplyr))

llm_use(TRUE)
llm_model("ollama/qwen2.5:3b")
llm_max_calls(40)

# flagged rows from the last full psychsci run
tbl <- readRDS("_stuff/grim_psychsci_table.rds")
flagged <- tbl[tbl$consistent %in% FALSE, ]
# one row per unique sentence/mean, max 14 for runtime
flagged <- flagged[!duplicated(paste(flagged$paper_id, flagged$reported)), ]
set.seed(1)
n_cases <- min(14, nrow(flagged))
flagged <- flagged[sample(seq_len(nrow(flagged)), n_cases), ]

# paragraph context
flagged <- text_expand(flagged, psychsci, expand_to = "paragraph")

# build prompts: target mean + paragraph
flagged$prompt_text <- sprintf(
  "TARGET MEAN: %s\n\nPARAGRAPH:\n%s",
  flagged$reported, flagged$expanded
)

schema <- readLines("_stuff/grim_llm_schema.json") |> paste(collapse = "\n")
preface <- paste(
  "You will receive a paragraph from a psychology paper and a TARGET mean",
  "reported in it. Extract information about the TARGET mean only.",
  "Use null when information is missing from this text; do not invent",
  "values and do not use knowledge from outside the text.",
  "Return a single JSON object following the schema below,",
  "bracketed by ```json and ```."
)
system_prompt <- paste(preface, schema, sep = "\n\n")

res <- llm(
  text = flagged$prompt_text,
  system_prompt = system_prompt,
  params = list(seed = 8675309)
)

out <- json_expand(res, suffix = c("", ".llm"))

# --- evaluation ---
cat("=== LLM paragraph extraction vs regex pairing (", n_cases, "cases ) ===\n\n")
for (i in seq_len(nrow(flagged))) {
  o <- out[i, ]
  cat(sprintf("[%02d] %s | %s | regex n=%s (%s)\n",
              i, flagged$paper_id[[i]], flagged$reported[[i]],
              flagged$n[[i]], flagged$n_source[[i]]))
  cat(sprintf("     LLM: n=%s | items=%s | type=%s | integer=%s\n",
              o$n %||% NA, o$scale_items %||% NA,
              o$measure_type %||% NA, o$integer_data %||% NA))
  cat(sprintf("     n_quote: %s\n", substr(o$n_quote %||% "", 1, 90)))
  if (!is.null(o$items_quote) && !is.na(o$items_quote))
    cat(sprintf("     items_quote: %s\n", substr(o$items_quote, 1, 90)))
  cat("\n")
}

# summary counts
has <- \(x) !is.null(x) && sum(!is.na(x))
cat("=== Summary ===\n")
cat("n found in paragraph:        ", has(out$n), "/", n_cases, "\n")
cat("scale_items found:           ", has(out$scale_items), "/", n_cases, "\n")
cat("measure_type classified:     ",
    sum(!is.na(out$measure_type) & out$measure_type != "unknown"), "/", n_cases, "\n")
cat("judged non-integer (FP cue): ",
    sum(out$integer_data %in% FALSE), "/", n_cases, "\n")
if (".error" %in% names(out) || "error" %in% names(out))
  cat("LLM errors:", sum(out$error %in% TRUE, out$.error %in% TRUE), "\n")

saveRDS(out, "_stuff/grim_llm_results.rds")
