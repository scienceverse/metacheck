#' Set default options
#'
#' @param libname libname
#' @param pkgname pkgname
#'
#' @returns NULL
#' @export
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  op <- options()

  # check available API keys and set default LLM provider
  api_keys <- c(
    # most common?
    ollama = Sys.getenv("OLLAMA_BASE_URL"),
    groq = Sys.getenv("GROQ_API_KEY"),
    openai = Sys.getenv("OPENAI_API_KEY"),
    google_gemini = Sys.getenv("GEMINI_API_KEY"),
    google_vertex = Sys.getenv("GOOGLE_API_KEY"),
    # others alphabetically
    anthropic = Sys.getenv("ANTHROPIC_API_KEY"),
    cloudflare = Sys.getenv("CLOUDFLARE_API_KEY"),
    deepseek = Sys.getenv("DEEPSEEK_API_KEY"),
    huggingface = Sys.getenv("HUGGINGFACE_API_KEY"),
    mistral = Sys.getenv("MISTRAL_API_KEY"),
    openrouter = Sys.getenv("OPENROUTER_API_KEY"),
    perplexity = Sys.getenv("PERPLEXITY_API_KEY"),
    portkey = Sys.getenv("PORTKEY_API_KEY"),
    # No API KEYS
    azure_openai = Sys.getenv("AZURE_OPENAI_ENDPOINT"),
    databricks = Sys.getenv("DATABRICKS_HOST")
    # not GITHUB_PAT: the github provider no longer works (models_github() is
    # defunct), and a GITHUB_PAT set for other tools made it the default (U20)
  )
  api_keys <- api_keys[api_keys != ""]
  if (length(api_keys)) {
    llm_model <- names(api_keys)[[1]]
  } else {
    llm_model <- NULL
  }

  op.pkg <- list(
    metacheck.verbose = TRUE,
    metacheck.llm_max_calls = 30L,
    metacheck.llm.model = llm_model,
    metacheck.llm.use = FALSE,
    metacheck.report_type = "full",
    metacheck.osf.delay = 0,
    metacheck.osf.api = "https://api.osf.io/v2",
    metacheck.osf.api.calls = 0
  )
  # only set if not already set
  toset <- !(names(op.pkg) %in% names(op))
  if (any(toset)) options(op.pkg[toset])

  # The OSF token is not checked here: loading the package must not use the
  # internet (CRAN), and the token is checked the first time it is used
  # (.key_use() in .osf_headers()), which leaves a refused token out.

  invisible()
}

#' On Attach
#'
#' @param libname libname
#' @param pkgname pkgname
#'
#' @returns startup message
#' @export
#' @keywords internal
#' @noRd
.onAttach <- function(libname, pkgname) {
  # check if email is set
  email <- getOption("metacheck.email") %||% ""

  if (!grepl(".+@.+\\..+$", email) | email == "metacheck@scienceverse.org") {
    mailset <- "\n\u26A0\uFE0F Set an email to use APIs like OpenAlex\nmetacheck::email('your@address.org')\n"
  } else {
    mailset <- paste0(
      "\n\uD83D\uDCE7 The email for APIs like OpenAlex:",
      "\n", email, "\n"
    )
  }

  # No check for a newer version on GitHub: attaching the package must not
  # use the internet (CRAN), and CRAN tells its users about new versions.
  v <- utils::packageVersion("metacheck")


  stripe <- paste0(
    "\033[31m*****", # red
    "\033[33m*****", # yellow
    "\033[32m*****", # green
    "\033[34m*****", # blue
    # "\033[36m*****" # cyan
    "\033[35m*****\033[0m" # magenta
  )

  stripe <- paste0(
    "\033[32m",
    rep("*", 47) |> paste(collapse = ""),
    "\033[0m"
  )

  if (!interactive()) {
    stripe <- rep("*", 47) |> paste(collapse = "")
  }
  paste(
    "\n",
    stripe,
    paste("\u2705 Welcome to metacheck beta version", v),
    "\n\u2139 For support and examples visit:",
    "https://www.scienceverse.org/metacheck/",
    mailset,
    "\U0001F9EA This is beta software; please check any",
    "results. Check module validation info for",
    "false positive and negative rates.",
    stripe,
    sep = "\n"
  ) |> packageStartupMessage()
}
