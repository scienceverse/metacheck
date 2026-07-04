# api.R
# Plumber API for metacheck
# Accepts bibr JSON uploads for single-paper analysis

library(plumber)
library(metacheck)

# --- LLM configuration (env-driven, per-deploy) -----------------------------
# Load-bearing: metacheck.llm.use defaults to FALSE and the LLM-using modules
# silently fall back to non-LLM paths — without this block the LLM half of
# the API is dead with no error.
#
# GEMINI_API_KEY, not GOOGLE_API_KEY: metacheck maps GOOGLE_API_KEY to the
# google_vertex provider (needs a GCP project; errors on a bare API key);
# google_gemini is the provider that works with AI-Studio keys.
if (nzchar(Sys.getenv("GEMINI_API_KEY"))) {
  metacheck::llm_use(TRUE)
  model_str <- Sys.getenv("METACHECK_LLM_MODEL", "")
  metacheck::llm_model(if (nzchar(model_str)) model_str
                       else "google_gemini/gemini-3.1-flash-lite-preview")
  # default llm_max_calls is 30 and llm() *errors* (not truncates) past it —
  # too low for big papers
  max_calls_str <- Sys.getenv("METACHECK_LLM_MAX_CALLS", "")
  metacheck::llm_max_calls(if (nzchar(max_calls_str)) as.integer(max_calls_str) else 200L)
  logger::log_info("LLM enabled: {metacheck::llm_model()}")
} else {
  logger::log_info("GEMINI_API_KEY not set — LLM modules will use fallbacks")
}

#* @apiTitle metacheck API
#* @apiDescription API for analyzing academic papers. Upload bibr JSON (from the bibr extraction pipeline) to extract metadata, authors, references, and run metacheck modules.

#* @plumber
function(pr) {
  # Paper analysis endpoints - upload bibr JSON to analyze.
  #
  # The restricted parser set is load-bearing: each handler parses the
  # multipart upload itself (mime::parse_multipart) and reads the file with
  # .read_bibr. "multi" must stay registered — plumber's body-read step feeds
  # rook.input, and without it mime sees an empty body and every upload 500s
  # (the "none" parser breaks exactly this way). Restricting the inner parsers
  # to "octet" keeps file parts as raw bytes: with plumber's defaults the
  # application/json file part (the Content-Type clients set for a .json
  # upload) would be JSON-parsed a *second* time — wasteful on payloads up to
  # the 50MB cap — and a malformed body would throw an uncaught 500 in
  # plumber's parse phase, before the handler's read_paper()/error_response()
  # can turn it into a clean 400.
  paper_pr <- plumber::pr("endpoints/paper.R")
  plumber::pr_set_parsers(paper_pr, c("multi", "octet"))
  plumber::pr_mount(pr, "/paper", paper_pr)
}

#* Health check endpoint
#* @get /health
#* @serializer json
function() {
  list(status = "ok", timestamp = Sys.time())
}
