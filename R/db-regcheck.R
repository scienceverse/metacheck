# RegCheck API client
#
# RegCheck (https://github.com/JamieCummins/regcheck) compares a study
# registration with the corresponding published paper using an LLM, one
# comparison per "dimension" (e.g. sample size, hypotheses, exclusion
# criteria). A fork (https://github.com/Matilda03/regcheck-fork) adds fully
# local inference via Ollama, so comparisons can run without any data leaving
# your machine.

# default server locations per client
.regcheck_hosted_url <- "https://preregpt-8584b32c9141.herokuapp.com"
.regcheck_local_url <- "http://localhost:8000"

#' RegCheck server URL for a client
#'
#' Returns the base URL to use for a given RegCheck client. An explicit
#' `base_url` always wins, then the `REGCHECK_BASE_URL` environment variable,
#' then a client-specific default: the local RegCheck fork
#' (`http://localhost:8000`) for `"ollama"`, and the hosted RegCheck app for
#' the API-based clients (`"groq"`, `"openai"`, `"deepseek"`).
#'
#' @param client the LLM client RegCheck should use
#' @param base_url an explicit base URL, or NULL to use defaults
#'
#' @returns the base URL with any trailing slash removed
#' @export
#' @keywords internal
regcheck_base_url <- function(client = "ollama", base_url = NULL) {
  url <- base_url %||% ""
  if (!nzchar(url)) url <- Sys.getenv("REGCHECK_BASE_URL")
  if (!nzchar(url)) {
    url <- if (identical(client, "ollama")) {
      .regcheck_local_url
    } else {
      .regcheck_hosted_url
    }
  }
  sub("/+$", "", url)
}

#' Compare a Preregistration with a Paper via RegCheck
#'
#' Sends the full text of a paper and the text of its (pre)registration to a
#' [RegCheck](https://github.com/JamieCummins/regcheck) server, which uses an
#' LLM to compare them dimension by dimension (e.g., sample size, hypotheses,
#' exclusion criteria) and judge for each dimension whether the paper deviates
#' from the registration.
#'
#' @details
#' By default this uses the locally running
#' [RegCheck fork](https://github.com/Matilda03/regcheck-fork) with Ollama
#' (`client = "ollama"`, `http://localhost:8000`), so no text leaves your
#' machine. Set `client` to `"groq"`, `"openai"` or `"deepseek"` to use the
#' hosted RegCheck app instead; note that this sends the full paper and
#' registration text to that server and the corresponding LLM provider.
#'
#' All RegCheck servers (including a local one) require an API token. Set it
#' in your `.Renviron` (use `usethis::edit_r_environ()`) as:
#'
#' `REGCHECK_API_TOKEN="your_token"`
#'
#' For a local server this must match the `REGCHECK_API_TOKEN` you configured
#' when starting the server.
#'
#' A comparison makes one LLM call per dimension and can take several minutes.
#' The function polls the server until the comparison completes.
#'
#' @param paper_text the full text of the paper (single string)
#' @param prereg_text the text of the preregistration (single string); provide
#'   exactly one of `prereg_text` or `registration_id`
#' @param registration_id a clinical trial registration ID (NCT number) to
#'   compare against, as an alternative to `prereg_text`
#' @param client the LLM client the RegCheck server should use: "ollama"
#'   (local), "groq", "openai", or "deepseek"
#' @param base_url the base URL of the RegCheck server; defaults to
#'   `http://localhost:8000` for ollama and the hosted RegCheck app otherwise
#'   (can also be set with the `REGCHECK_BASE_URL` environment variable)
#' @param api_token the RegCheck API token
#' @param dimensions optional data frame with columns `dimension` and
#'   `definition` to override the server's default comparison dimensions
#' @param reasoning_effort reasoning effort for OpenAI models ("low",
#'   "medium", "high"); ignored by other clients
#' @param poll_interval seconds between polls of the task status
#' @param timeout maximum seconds to wait for the comparison to complete
#'
#' @returns a data frame with one row per compared dimension, with columns
#'   `dimension`, `deviation_judgement` ("yes", "no", or "missing"),
#'   `paper_summary`, `prereg_summary`, `deviation_information`,
#'   `paper_quotes`, and `prereg_quotes`. The raw server result is attached
#'   as the "regcheck_result" attribute.
#' @export
#'
#' @examples
#' \dontrun{
#' paper <- demopaper()
#' paper_text <- paste(paper$text$text, collapse = " ")
#' prereg_text <- "We will collect 100 participants..."
#' rc <- regcheck_compare(paper_text, prereg_text)
#' }
regcheck_compare <- function(paper_text,
                             prereg_text = NULL,
                             registration_id = NULL,
                             client = c("ollama", "groq", "openai", "deepseek"),
                             base_url = NULL,
                             api_token = Sys.getenv("REGCHECK_API_TOKEN"),
                             dimensions = NULL,
                             reasoning_effort = "medium",
                             poll_interval = 5,
                             timeout = 3600) {
  client <- match.arg(client)

  # validate inputs ----
  if (!nzchar(api_token)) {
    stop("RegCheck requires an API token. Set REGCHECK_API_TOKEN in your ",
         ".Renviron (see ?regcheck_compare).", call. = FALSE)
  }
  if (!is.character(paper_text) || length(paper_text) != 1 ||
      !nzchar(trimws(paper_text))) {
    stop("paper_text must be a single non-empty string", call. = FALSE)
  }
  has_prereg <- !is.null(prereg_text) && nzchar(trimws(prereg_text))
  has_reg_id <- !is.null(registration_id) && nzchar(trimws(registration_id))
  if (has_prereg == has_reg_id) {
    stop("Provide exactly one of prereg_text or registration_id",
         call. = FALSE)
  }

  url <- regcheck_base_url(client, base_url)

  # the RegCheck server's text-to-PDF step only supports latin-1, so
  # transliterate Greek letters and other scientific symbols (which would
  # otherwise crash the server worker) to readable equivalents
  paper_text <- .regcheck_sanitize(paper_text)
  if (has_prereg) prereg_text <- .regcheck_sanitize(prereg_text)

  # submit the comparison ----
  created <- .regcheck_submit(
    url, api_token,
    paper_text = paper_text,
    prereg_text = if (has_prereg) prereg_text else NULL,
    registration_id = if (has_reg_id) registration_id else NULL,
    client = client,
    dimensions = dimensions,
    reasoning_effort = reasoning_effort
  )

  message("RegCheck task ", created$task_id, " queued on ", url)

  # poll until done ----
  result <- .regcheck_poll(
    url, api_token, created$task_id,
    poll_interval = poll_interval, timeout = timeout
  )

  tidy <- regcheck_tidy(result)
  attr(tidy, "regcheck_result") <- result
  tidy
}

#' Make text safe for the RegCheck server
#'
#' The RegCheck server converts submitted text to PDF with a latin-1-only
#' library, so any character outside latin-1 (Greek letters, typographic
#' quotes, math symbols) crashes the comparison worker. This transliterates
#' the symbols common in scientific text to readable latin-1 equivalents
#' (e.g. the Greek letter alpha becomes "alpha", typographic quotes become
#' straight quotes) and strips anything else that cannot be represented.
#'
#' @param text a character string
#'
#' @returns the text with only latin-1-safe characters
#' @keywords internal
.regcheck_sanitize <- function(text) {
  # greek letters and scientific symbols -> readable names
  map <- c(
    "α" = "alpha", "β" = "beta", "γ" = "gamma",
    "δ" = "delta", "Δ" = "Delta", "ε" = "epsilon",
    "ζ" = "zeta", "η" = "eta", "θ" = "theta",
    "ι" = "iota", "κ" = "kappa", "λ" = "lambda",
    "μ" = "mu", "ν" = "nu", "ξ" = "xi",
    "π" = "pi", "ρ" = "rho", "σ" = "sigma",
    "Σ" = "Sigma", "τ" = "tau", "φ" = "phi",
    "υ" = "upsilon", "χ" = "chi", "ψ" = "psi",
    "ω" = "omega", "Ω" = "Omega",
    "²" = "2", "³" = "3", "⁰" = "0", "¹" = "1",
    "≤" = "<=", "≥" = ">=", "≠" = "!=", "≈" = "~",
    "×" = "x", "·" = "*", "−" = "-", "±" = "+/-",
    "–" = "-", "—" = "-", "‘" = "'", "’" = "'",
    "“" = '"', "”" = '"', "…" = "...", "°" = " degrees"
  )
  for (ch in names(map)) {
    text <- gsub(ch, map[[ch]], text, fixed = TRUE)
  }
  # strip any remaining characters latin-1 cannot represent, but return the
  # string UTF-8 encoded: the *content* must be latin-1-safe for the server,
  # while the JSON/file transport must stay valid UTF-8
  out <- iconv(text, from = "UTF-8", to = "latin1", sub = "")
  if (is.na(out)) out <- iconv(text, to = "latin1", sub = "")
  enc2utf8(out)
}

#' Submit a RegCheck comparison
#'
#' Tries the JSON text endpoint (`POST /api/v1/comparisons/text`, available on
#' the upstream RegCheck server); if the server does not have it (the local
#' ollama fork), falls back to the multipart endpoint
#' (`POST /api/v1/comparisons`) with the texts uploaded as .txt files.
#'
#' @param url base URL (no trailing slash)
#' @param api_token the RegCheck API token
#' @param paper_text,prereg_text,registration_id,client,dimensions,reasoning_effort
#'   see [regcheck_compare()]
#'
#' @returns the parsed creation response (list with task_id, status_url)
#' @keywords internal
.regcheck_submit <- function(url, api_token, paper_text,
                             prereg_text = NULL, registration_id = NULL,
                             client = "ollama", dimensions = NULL,
                             reasoning_effort = "medium") {
  body <- list(
    paper_text = paper_text,
    client = client,
    reasoning_effort = reasoning_effort,
    append_previous_output = TRUE,
    multiple_experiments = FALSE
  )
  if (!is.null(prereg_text)) body$registration_text <- prereg_text
  if (!is.null(registration_id)) body$registration_id <- registration_id
  if (!is.null(dimensions)) {
    body$dimensions <- lapply(seq_len(nrow(dimensions)), \(i) {
      list(dimension = dimensions$dimension[[i]],
           definition = dimensions$definition[[i]])
    })
  }

  req <- httr2::request(paste0(url, "/api/v1/comparisons/text")) |>
    httr2::req_headers(Authorization = paste("Bearer", api_token)) |>
    httr2::req_body_json(body)

  resp <- tryCatch(
    httr2::req_perform(req),
    httr2_http_404 = function(e) NULL,
    httr2_http_405 = function(e) NULL
  )

  if (is.null(resp)) {
    # server has no /text endpoint (e.g. the local ollama fork):
    # fall back to the multipart endpoint with .txt uploads
    resp <- .regcheck_submit_multipart(
      url, api_token, paper_text,
      prereg_text = prereg_text, registration_id = registration_id,
      client = client, dimensions = dimensions,
      reasoning_effort = reasoning_effort
    )
  }

  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Submit a RegCheck comparison via the multipart endpoint
#'
#' @inheritParams .regcheck_submit
#' @returns an httr2 response
#' @keywords internal
.regcheck_submit_multipart <- function(url, api_token, paper_text,
                                       prereg_text = NULL,
                                       registration_id = NULL,
                                       client = "ollama", dimensions = NULL,
                                       reasoning_effort = "medium") {
  paper_file <- tempfile("regcheck_paper_", fileext = ".txt")
  writeLines(paper_text, paper_file, useBytes = TRUE)
  on.exit(unlink(paper_file), add = TRUE)

  fields <- list(
    paper = curl::form_file(paper_file, type = "text/plain"),
    client = client,
    reasoning_effort = reasoning_effort,
    append_previous_output = "yes",
    multiple_experiments = "no"
  )

  if (!is.null(prereg_text)) {
    prereg_file <- tempfile("regcheck_prereg_", fileext = ".txt")
    writeLines(prereg_text, prereg_file, useBytes = TRUE)
    on.exit(unlink(prereg_file), add = TRUE)
    fields$registration_file <- curl::form_file(prereg_file,
                                                type = "text/plain")
  }
  if (!is.null(registration_id)) fields$registration_id <- registration_id
  if (!is.null(dimensions)) {
    # the multipart endpoint takes dimensions as a JSON array of
    # {dimension, definition} objects
    fields$dimensions <- jsonlite::toJSON(
      dimensions[, c("dimension", "definition")],
      auto_unbox = TRUE
    ) |> as.character()
  }

  httr2::request(paste0(url, "/api/v1/comparisons")) |>
    httr2::req_headers(Authorization = paste("Bearer", api_token)) |>
    httr2::req_body_multipart(!!!fields) |>
    httr2::req_perform()
}

#' Poll a RegCheck task until it completes
#'
#' @param url base URL (no trailing slash)
#' @param api_token the RegCheck API token
#' @param task_id the task to poll
#' @param poll_interval seconds between polls
#' @param timeout maximum seconds to wait
#'
#' @returns the task result (list with an `items` element)
#' @keywords internal
.regcheck_poll <- function(url, api_token, task_id,
                           poll_interval = 5, timeout = 3600) {
  status_url <- paste0(url, "/api/v1/comparisons/", task_id)
  deadline <- Sys.time() + timeout

  repeat {
    Sys.sleep(poll_interval)

    status <- httr2::request(status_url) |>
      httr2::req_headers(Authorization = paste("Bearer", api_token)) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = FALSE)

    state <- status$state %||% "unknown"

    if (state == "success") {
      message("RegCheck comparison complete.")
      return(status$result)
    }
    if (state == "failure") {
      stop("RegCheck comparison failed: ",
           status$status %||% "unknown error", call. = FALSE)
    }
    if (Sys.time() > deadline) {
      stop("Timed out waiting for RegCheck task ", task_id, call. = FALSE)
    }

    message("RegCheck running (",
            status$processed_dimensions %||% 0, "/",
            status$total_dimensions %||% 0, " dimensions)")
  }
}

#' Tidy a RegCheck result
#'
#' Converts the raw result of a RegCheck comparison (a list with an `items`
#' element, one item per compared dimension) into a data frame.
#'
#' @param result a raw RegCheck result list, as returned by the RegCheck API
#'
#' @returns a data frame with one row per dimension and columns `dimension`,
#'   `deviation_judgement`, `paper_summary`, `prereg_summary`,
#'   `deviation_information`, `paper_quotes`, and `prereg_quotes`
#' @export
#' @keywords internal
regcheck_tidy <- function(result) {
  items <- result$items %||% list()

  rows <- lapply(items, \(item) {
    data.frame(
      dimension = item$dimension %||% NA_character_,
      deviation_judgement = item$deviation_judgement %||% NA_character_,
      paper_summary = item$paper_content_summary %||% NA_character_,
      prereg_summary = item$registration_content_summary %||% NA_character_,
      deviation_information = item$deviation_information %||% NA_character_,
      paper_quotes = item$paper_content_quotes %||% NA_character_,
      prereg_quotes = item$registration_content_quotes %||% NA_character_
    )
  })

  if (length(rows) == 0) {
    return(data.frame(
      dimension = character(0),
      deviation_judgement = character(0),
      paper_summary = character(0),
      prereg_summary = character(0),
      deviation_information = character(0),
      paper_quotes = character(0),
      prereg_quotes = character(0)
    ))
  }

  do.call(rbind, rows)
}
