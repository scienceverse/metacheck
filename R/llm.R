#' Query an LLM
#'
#' Ask a large language model (LLM) any question you want about a vector of
#' text or the text from a text_search(). When `type` is provided, uses
#' ellmer's structured output API to guarantee output conforming to the type
#' spec; otherwise returns free-text responses in an `answer` column.
#'
#' You will need to get your own API key from <https://console.groq.com/keys>. To avoid having to type it out, add it to the .Renviron file in the following format (you can use `usethis::edit_r_environ()` to access the .Renviron file)
#'
#' GROQ_API_KEY="key_value_asdf"
#'
#' See <https://console.groq.com/docs> for more information
#'
#' @param text The text to send to the LLM (vector of strings, or data frame with the text in a column)
#' @param system_prompt A system prompt to set the behavior of the assistant
#' @param type An optional ellmer type specification for structured extraction
#'   (e.g., from `type_object()`, `type_from_schema()`). When provided, the
#'   provider enforces the schema and returns structured columns instead of
#'   free text.
#' @param text_col The name of the text column if text is a data frame
#' @param model the LLM model name (see `llm_model_list()`) in the format "provider" or "provider/model"
#' @param params a named list to pass to `ellmer::params()`
#' @param phase optional label naming the calling step (e.g. "Identifying scales") shown in the progress bar so a slow LLM pass is identifiable; the model name is appended automatically
#'
#' @return a data frame of results
#'
#' @export
#' @examples
#' \dontrun{
#' # Free-text query
#' text <- c("hello", "number", "ten", 12)
#' system_prompt <- "Is this a number? Answer only 'TRUE' or 'FALSE'"
#' is_number <- llm(text, system_prompt)
#'
#' # Structured extraction
#' type_spec <- ellmer::type_object(
#'   is_number = ellmer::type_boolean("Whether the input is a number")
#' )
#' result <- llm(c("hello", "42"), "Classify the input.", type = type_spec)
#' }
llm <- function(text, system_prompt,
                type = NULL,
                text_col = "text",
                model = llm_model(),
                params = list(),
                phase = NULL) {
  ## error detection ----
  if (!llm_use()) {
    stop("Set llm_use(TRUE) to use LLM functions")
  }

  if (is.null(model) || !is.character(model) || length(model) != 1 || !nzchar(model)) {
    stop(
      "No LLM model set. Use llm_model('provider/model') or pass model = 'provider/model'",
      call. = FALSE
    )
  }

  if (!is.list(params)) {
    stop("params must be a named list", call. = FALSE)
  }

  params_list <- params

  # make a data frame if text is a vector
  if (!is.data.frame(text)) {
    text <- data.frame(text = text)
    names(text) <- text_col
  }

  # set up answer data frame to return ----
  unique_text <- unique(text[[text_col]])
  # Sanitise before anything downstream keys on the text (cache key, request
  # body). Research data routinely carries invalid UTF-8 (a mis-encoded
  # apostrophe/°/é in a free-text survey answer) and stray control bytes; ellmer
  # serialises those into a JSON request body the provider then cannot parse
  # (Mistral: HTTP 400 "There was an error parsing the body"). One guard here
  # protects every caller and every provider. See .llm_sanitise_text().
  unique_text <- .llm_sanitise_text(unique_text)
  ncalls <- length(unique_text)

  if (ncalls == 0) stop("No calls to the LLM")
  if (ncalls > llm_max_calls()) {
    stop("This would make ", ncalls, " calls to the LLM, but your maximum number of calls is set to ", llm_max_calls(),
         ". Set `llm_max_calls(", ncalls, ")` (or higher) to allow this call.", call. = FALSE)
  }

  # Set up the llm ----
  # default temperature to 0 for deterministic extraction/classification
  if (is.null(params_list$temperature)) {
    params_list$temperature <- 0
  }

  # check params early so malformed params fail before provider/network work
  params <- tryCatch({
    do.call(ellmer::params, params_list)
  }, error = \(e) {
    stop("Misspecified params argument:\n", e$message, call. = FALSE)
  })

  # check if json schema type is set for a structured return
  structured <- !is.null(type)

  # ollama checks ----
  use_ollama_native <- FALSE
  if (grepl("^ollama", model)) {
    # ollama's /v1/ endpoint ignores think=FALSE; native /api/chat honours it.
    # Use the native path only for unstructured calls that are not "thinking";
    # structured output goes through the /v1/ endpoint (ellmer type schema).
    use_ollama_native <- !isTRUE(params_list$think) && !structured
    if (use_ollama_native) {
      ollama_options <- params_list
      ollama_options$think <- NULL
    }
    ollama_base_url <- Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")

    # check ollama is up
    ollama_up <- tryCatch({
      paste0(ollama_base_url, "/api/version") |>
        httr2::request() |>
        httr2::req_timeout(3) |>
        httr2::req_perform()
      TRUE
    }, error = \(e) FALSE)
    if (!ollama_up) stop("Ollama is not running at ", ollama_base_url,
                         ". Start ollama and try again.", call. = FALSE)

    # check model exists or set model if not specified
    ollama_model <- sub("^ollama\\/?", "", model)
    models <- ellmer::models_ollama(ollama_base_url)
    if (nrow(models) == 0) {
      stop("Ollama is installed, but there are no models loaded", call. = FALSE)
    } else if (is.null(ollama_model) || ollama_model == "") {
      ollama_model <- models$id[[1]]
      message(paste0("Using model = \"", ollama_model, "\"."))
    } else if (!ollama_model %in% models$id) {
      stop("Ollama is installed, but the model ", ollama_model,
           " is not available", call. = FALSE)
    }
  }

  # route vllm/<model> through chat_vllm() so custom endpoints can be used
  make_chat <- function() {
    if (startsWith(model, "vllm/")) {
      vllm_model <- sub("^vllm/", "", model)
      vllm_base_url <- getOption("metacheck.llm.vllm.base_url")

      if (!nzchar(vllm_model)) {
        stop("For vllm, set model as 'vllm/<model-name>'", call. = FALSE)
      }
      if (is.null(vllm_base_url) || !nzchar(vllm_base_url)) {
        stop(
          "Set options(metacheck.llm.vllm.base_url = '<vllm-endpoint>/v1') to use vllm models",
          call. = FALSE
        )
      }

      return(ellmer::chat_vllm(
        model = vllm_model,
        base_url = vllm_base_url,
        credentials = function() Sys.getenv("VLLM_API_KEY"),
        system_prompt = system_prompt,
        params = params
      ))
    }

    ellmer::chat(
      name = model,
      system_prompt = system_prompt,
      params = params
    )
  }

  # set up progress bar ----
  # `phase` names the calling step (e.g. "Identifying scales") so a slow LLM
  # pass is visible for what it is, rather than a generic "Extracting data".
  # The model is appended so the user sees which LLM is being queried.
  base_label <- if (structured) "Extracting data" else "Querying LLM"
  label <- if (!is.null(phase) && nzchar(phase)) phase else base_label
  if (!is.null(model) && nzchar(model)) label <- sprintf("%s (%s)", label, model)
  pb <- pb(ncalls, paste0(label, " [:bar] :current/:total :elapsedfull"))

  # response cache ----
  # A call at temperature 0 is deterministic in (model, prompt, text, type,
  # params), so replay a stored result instead of re-issuing (and re-billing)
  # the request. Errors are never cached. See R/llm-cache.R.
  use_cache <- llm_cache()

  # iterate over the text ----
  responses <- lapply(seq_along(unique_text), function(i) {
    key <- if (use_cache)
      .llm_cache_key(unique_text[i], system_prompt, type, model, params) else NULL
    if (!is.null(key)) {
      hit <- .llm_cache_get(key)
      if (!is.null(hit)) {
        pb$tick()
        return(hit$df)
      }
    }
    tryCatch({
      if (use_ollama_native) {
        # native ollama API: think=FALSE is honoured here, unlike /v1/
        answer <- .llm_ollama_native(
          unique_text[i], system_prompt, ollama_model,
          think = FALSE, options = ollama_options
        )
        pb$tick()
        out <- list(answer = answer)
        if (!is.null(key)) .llm_cache_put(key, out)
        out
      } else {
        # fresh chat per call to avoid context accumulation
        msg <- utils::capture.output({
          chat <- make_chat()
        }, type = "message")
        # only show message first time
        if (length(msg) && i == 1) pb$message(msg)

        if (structured) {
          # Groq's structured-output mode sometimes rejects its own generation
          # (HTTP 400, "Failed to generate/validate JSON") when the model emits
          # JSON that does not match the schema. Generation is not
          # bit-reproducible server-side, so an identical retry usually
          # succeeds; allow two before recording the row as failed. Each retry
          # gets a fresh chat so the failed exchange cannot pollute the next
          # attempt.
          for (attempt in 1:3) {
            result <- tryCatch(
              chat$chat_structured(unique_text[i], type = type),
              error = function(e) e
            )
            if (!inherits(result, "condition")) break
            if (attempt == 3 || !.llm_json_retryable(result)) stop(result)
            chat <- make_chat()
          }
          df <- .unnest_result(result)
          # An empty result (e.g. the model returned an empty array, meaning
          # "nothing found") unnests to a 0-row frame; assigning a length-1
          # join key to a 0-row column errors, so only set it when there are
          # rows. A 0-row df drops out of the downstream left_join cleanly.
          if (nrow(df) > 0) df$.join_key. <- unique_text[i]
          pb$tick()
          # store the unnested df plus the raw result (which carries any
          # provider-returned reasoning content) for later inspection
          if (!is.null(key)) .llm_cache_put(key, df, raw = result)
          df
        } else {
          answer <- chat$chat(unique_text[i], echo = FALSE)
          pb$tick()
          out <- list(answer = trimws(answer))
          if (!is.null(key)) .llm_cache_put(key, out)
          out
        }
      }
    }, error = function(e) {
      pb$tick()
      msg <- .llm_error_message(e)
      # A systemic failure (bad auth, server down, unreachable endpoint) means
      # every call this run will fail and checks fall back to rules only. Say so
      # loudly and immediately, once per session, so the run is not silently
      # label-blind — the per-row warnings below still record each failure.
      if (.llm_is_systemic_error(e))
        .llm_systemic_notice$trip(paste0(
          "LLM appears unavailable this run — checks will fall back to ",
          "RULES ONLY (no LLM inference). Check the endpoint and API key ",
          "(vllm reads Sys.getenv(\"VLLM_API_KEY\")).\n  First error: ", msg))
      if (structured) {
        df <- data.frame(.error = TRUE, .error_msg = msg)
        df$.join_key. <- unique_text[i]
        df
      } else {
        list(answer = NA, error = TRUE, error_msg = msg)
      }
    })
  })

  # terminate the progress bar so its line is closed off with a newline
  pb$terminate()

  # join responses back to input ----
  if (structured) {
    response_df <- dplyr::bind_rows(responses)
    text$.join_key. <- text[[text_col]]
    # When every response was empty (the model found nothing in any input),
    # response_df has no `.join_key.` column, which would break the join. Add an
    # empty one so the left_join yields all-NA extracted columns instead.
    if (!".join_key." %in% names(response_df))
      response_df$.join_key. <- character(0)
    answer_df <- dplyr::left_join(text, response_df, by = ".join_key.",
                                  suffix = c("", ".extracted"))
    answer_df$.join_key. <- NULL
  } else {
    response_df <- do.call(dplyr::bind_rows, responses)
    response_df[text_col] <- unique_text
    answer_df <- dplyr::left_join(text, response_df, by = text_col)
  }

  # add metadata ----
  class(answer_df) <- c("metacheck_llm", "data.frame")
  attr(answer_df, "llm") <- list(
    system_prompt = system_prompt,
    model = model,
    type = type
  )

  # warn about errors ----
  if (structured && ".error" %in% names(answer_df)) {
    error_rows <- which(!is.na(answer_df$.error) & answer_df$.error)
    if (length(error_rows) > 0) {
      msgs <- unique(answer_df$.error_msg[error_rows])
      n_ok <- nrow(answer_df) - length(error_rows)
      warning(
        sprintf(
          "Note (not fatal): %d of %d LLM extraction%s failed and %s left blank; the other %d succeeded and all checks continued.\nRow%s %s: %s\n  %s",
          length(error_rows), nrow(answer_df), plural(nrow(answer_df)),
          if (length(error_rows) == 1) "was" else "were", n_ok,
          plural(length(error_rows)), paste(error_rows, collapse = ", "),
          if (length(error_rows) == 1) "reason" else "reasons",
          paste(msgs, collapse = "\n  ")),
        call. = FALSE)
    }
  } else if (!structured) {
    error_indices <- isTRUE(answer_df$error)
    if (any(error_indices)) {
      warn <- paste(which(error_indices), collapse = ", ") |>
        paste("There were errors in the following rows:", x = _)

      answer_df$error_msg[error_indices] |>
        unique() |>
        paste("\n  * ", x = _) |>
        paste(warn, x = _) |>
        warning()
    }
  }

  return(answer_df)
}

#' Call ollama native API with think support
#'
#' ellmer routes ollama via the OpenAI-compatible /v1/ endpoint, which ignores
#' think=FALSE. This helper calls /api/chat directly where think is honoured.
#'
#' @param text The text to send to the LLM (vector of strings, or data frame with the text in a column)
#' @param system_prompt A system prompt to set the behavior of the assistant
#' @param model the ollama model
#' @param think whether to use thinking mode (very slow)
#' @param options further options to pass to to the model
#' @param base_url the local URL
#'
#' @export
#' @keywords internal
.llm_ollama_native <- function(text, system_prompt,
                               model = NULL,
                               think = FALSE,
                               options = list(),
                               base_url = Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")) {

  if (isFALSE(think)) {
    system_prompt <- paste0("/nothink\n\n", system_prompt)
  }

  body <- list(
    model = model,
    think = think,
    stream = FALSE,
    options = if (length(options)) options else NULL,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = text)
    )
  )
  resp <- httr2::request(paste0(base_url, "/api/chat")) |>
    httr2::req_body_json(body) |>
    httr2::req_perform()
  trimws(httr2::resp_body_json(resp)$message$content)
}

# Build the message recorded for a failed LLM call. ellmer/httr2 HTTP errors
# carry the response object (on the condition itself or its parent), and its
# body holds the provider's actual reason — e.g. behind a bare "HTTP 400 Bad
# Request", Groq says "Please reduce the length of the messages or completion".
# Without it a user cannot tell an oversized prompt from a malformed schema.
# TRUE when a structured-output failure is worth retrying verbatim: Groq
# returns HTTP 400 "Failed to generate JSON" / "Failed to validate JSON" when
# the model's output does not match the requested schema, and since generation
# is not deterministic server-side the same request usually succeeds on a
# second attempt. Genuine request errors (bad key, oversized prompt, rate
# limit) do not match and fail immediately as before.
.llm_json_retryable <- function(e) {
  msg <- .llm_error_message(e)
  grepl(
    paste(
      "Failed to (generate|validate) JSON",
      # jsonlite lexical/parse errors: the model returned text that is not valid
      # JSON (a prose list, a truncated object, a bare number). Generation is not
      # deterministic, so a fresh attempt usually yields schema-valid JSON. Covers
      # "invalid char in json text", "malformed number ...", "premature EOF",
      # "unallowed token", etc.
      "lexical error",
      "malformed number",
      "premature EOF",
      "parse error",
      "```json",
      sep = "|"
    ),
    msg,
    ignore.case = TRUE
  )
}

# Does this error mean the LLM is SYSTEMICALLY unavailable — i.e. every LLM call
# this run will fail the same way, so all checks silently fall back to rules
# only? Three such classes: an authentication failure (HTTP 401/403, e.g. a
# missing/expired API key), the endpoint erroring server-side (HTTP >= 500), or a
# genuine transport failure (DNS, connection refused, timeout — the endpoint is
# unreachable). Distinguished from a transient single-call failure (a one-off 400
# or a malformed-JSON reply), which is NOT systemic and keeps only its per-row
# warning.
#
# The subtle case: an error with NO response object is usually transport-level,
# BUT a jsonlite parse error (the request returned 200, the body just was not
# valid JSON) also has no response — and is per-call, not systemic. So a
# no-response error only counts as systemic when it is NOT a retryable JSON/parse
# failure and its message looks like a real connection problem.
.llm_is_systemic_error <- function(e) {
  resp <- e$resp %||% e$parent$resp
  status <- tryCatch(httr2::resp_status(resp), error = function(e2) NA_integer_)
  if (isTRUE(status %in% c(401L, 403L))) return(TRUE)   # auth: every call fails
  if (isTRUE(status >= 500L)) return(TRUE)              # server down / erroring
  if (!is.null(resp)) return(FALSE)                     # got a response: per-call
  # No response object. A malformed-JSON reply also has none — that is a per-call
  # model hiccup, not the endpoint being down, so exclude it.
  if (.llm_json_retryable(e)) return(FALSE)
  # Otherwise treat it as systemic only if the message names a transport failure.
  transport <- paste(
    "could not resolve", "connection refused", "couldn't connect", "timed out",
    "timeout", "connection reset", "network is unreachable", "no route to host",
    "failed to connect", "empty reply from server", "handshake", "ssl",
    sep = "|")
  grepl(transport, conditionMessage(e), ignore.case = TRUE)
}

# Emit an explicit, immediate notice the FIRST time the LLM looks systemically
# unavailable in a session, then stay quiet — the per-call warnings still record
# every failure for warnings(). Uses message() (prints to stderr immediately),
# not warning() (which R queues and shows only at the end, deduplicated and
# capped at 50 — invisible mid-run, which is exactly the failure we are fixing).
.llm_systemic_notice <- local({
  done <- FALSE
  list(
    trip  = function(msg) { if (!done) { message(msg); done <<- TRUE }; invisible() },
    reset = function() done <<- FALSE
  )
})

# Make text safe to serialise into a JSON request body for any LLM provider.
# Two problems, both common in research data:
#   * invalid UTF-8 — a mis-encoded byte (a Latin-1 apostrophe/°/é a nominally-
#     UTF-8 file never validated) makes the JSON body unparseable. We reinterpret
#     invalid entries as Latin-1, a conversion that cannot fail (every byte is a
#     valid Latin-1 character), leaving valid text untouched.
#   * control characters — raw NUL / C0 control bytes (except tab/newline) are
#     illegal in JSON strings; strip them.
# Applied to the vector of prompts before caching or sending, so no malformed
# text reaches the provider regardless of which upstream path built it.
.llm_sanitise_text <- function(x) {
  if (is.null(x) || !length(x)) return(x)
  x <- as.character(x)
  bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8", sub = "")
  # Any residue still invalid (rare): drop the invalid bytes rather than fail.
  still <- !is.na(x) & !validUTF8(x)
  if (any(still)) x[still] <- iconv(x[still], from = "UTF-8", to = "UTF-8", sub = "")
  # Strip C0 control chars except tab (\t) and newline (\n). \x00 (NUL) cannot
  # appear in an R character string, so the range \x01-\x1f covers all that can.
  x <- gsub("[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F]", "", x, perl = TRUE)
  x
}

.llm_error_message <- function(e) {
  msg <- conditionMessage(e)
  resp <- e$resp %||% e$parent$resp
  if (is.null(resp)) return(msg)
  detail <- tryCatch({
    body <- httr2::resp_body_json(resp, check_type = FALSE)
    err <- body$error
    if (is.character(err)) err[1] else err$message %||% body$message
  }, error = function(e2) NULL)
  if (is.null(detail))
    detail <- tryCatch(httr2::resp_body_string(resp), error = function(e2) NULL)
  if (!is.character(detail) || length(detail) == 0 || !nzchar(detail[1]))
    return(msg)
  detail <- detail[1]
  if (nchar(detail) > 500) detail <- paste0(substr(detail, 1, 500), " [truncated]")
  paste0(msg, "\n  Provider says: ", detail)
}

#' Convert structured LLM result to a data frame
#'
#' Handles single objects, wrapper objects with a single array field,
#' and data frames. Converts NULLs to NAs for data frame compatibility.
#'
#' @param result a list from `chat$chat_structured()`
#' @returns a data frame
#' @keywords internal
.unnest_result <- function(result) {
  if (is.data.frame(result)) return(result)

  # If result is a list with a single field containing an array of objects,
  # unnest the array into rows (e.g., { power_analyses: [{...}, {...}] })
  if (is.list(result) && length(result) == 1) {
    inner <- result[[1]]
    if (is.list(inner) && !is.data.frame(inner)) {
      if (length(inner) == 0) {
        return(data.frame())
      }
      if (all(vapply(inner, is.list, logical(1)))) {
        return(dplyr::bind_rows(lapply(inner, function(item) {
          item[vapply(item, is.null, logical(1))] <- NA
          as.data.frame(item)
        })))
      }
    }
  }

  # Single object — convert NULLs to NAs and make one-row df
  if (is.list(result)) {
    result[vapply(result, is.null, logical(1))] <- NA
  }
  as.data.frame(result)
}

#' List LLM Models
#'
#' List available LLM models for the specified platform.
#'
#' For platforms other than groq, returns the value from the corresponding ellmer::models_platform function.
#'
#' @param platform The platform. If NULL, checks all platforms for which you have a valid API_KEY.
#'
#' @returns a data frame of models and info
#' @export
#'
#' @examples
#' \dontrun{
#' llm_model_list()
#' }
llm_model_list <- function(platform = NULL) {
  # get all ellmer models_* functions
  ef <- getNamespaceExports("ellmer") |>
    grep("models_.+", x = _, value = TRUE)
  names(ef) <- gsub("models_", "", ef)
  funcs <- lapply(ef, \(x) utils::getFromNamespace(x, "ellmer"))
  # ellmer doesn't have a groq or ollama model functions, so use ours
  funcs$groq <- .llm_model_list_groq
  #funcs$ollama <- .llm_model_list_ollama

  # if null, return all available platforms
  if (is.null(platform)) platform <- names(funcs)

  # error if any invalid platforms
  invalid <- setdiff(platform, names(funcs))
  if (length(invalid) > 0) {
    stop("Invalid platforms: ", paste(invalid, collapse = ", "))
  }

  # get models and ignore errors, add platform name
  models <- lapply(platform, \(p) {
    if (p != "ollama" && !online()) {
      return(NULL)
    }
    tryCatch({
        # skip if google api key isn't set, otherwise it requests login
      if (p %in% c("google_gemini", "google_vertex") &&
        Sys.getenv("GOOGLE_API_KEY") == "") {
        return(NULL)
      }

      model_func <- funcs[[p]]
      m <- model_func()
      #cols <- c("platform", names(m))
      m$platform <- p

      m
    }, error = \(e) {})
  })

  # reorder columns
  all_models <- dplyr::bind_rows(models)
  if (nrow(all_models)) {
    start <- c("platform", "id")
    end <- setdiff(names(all_models), start)
    all_models <- all_models[, c(start, end)]
  }

  return(all_models)
}

#' Get Groq Models
#'
#' Returns a list of available models in groq, excluding whisper and vision models (for audio and images) See <https://console.groq.com/docs/models> for more information.
#'
#' @returns a table of model info
#' @export
#'
#' @keywords internal
.llm_model_list_groq <- function() {
  API_KEY <- Sys.getenv("GROQ_API_KEY")
  url <- "https://api.groq.com/openai/v1/models"

  resp <- httr2::request(url) |>
    httr2::req_headers(Authorization = paste("Bearer", API_KEY)) |>
    httr2::req_perform()

  models <- do.call(
    dplyr::bind_rows,
    httr2::resp_body_json(resp)$data
  ) |>
    data.frame()

  models$created_at <- as.POSIXct(models$created) |>
    format("%Y-%m-%d") |>
    as.Date()
  rows <- models$active & !grepl("whisper|vision", models$id)
  cols <- names(models) |> setdiff(c("active", "created"))
  active <- models[rows, cols]

  return(active)
}

#' Set the maximum number of calls to the LLM
#'
#' @param n The maximum number of calls that the llm() function can make
#'
#' @return NULL
#' @export
#'
llm_max_calls <- function(n = NULL) {
  if (is.null(n)) {
    return(getOption("metacheck.llm_max_calls"))
  }
  if (!is.numeric(n)) stop("n must be a number")

  n <- as.integer(n)
  if (n < 1) {
    warning("n must be greater than 0; it was not changed from ", getOption("metacheck.llm_max_calls"))
  } else {
    options(metacheck.llm_max_calls = n)
  }

  invisible(getOption("metacheck.llm_max_calls"))
}

#' Set the default LLM model
#'
#' Use `llm_model_list()` to get a list of available models
#'
#' @param model the name of the model
#'
#' @return NULL
#' @export
#'
llm_model <- function(model = NULL) {
  if (missing(model)) {
    return(getOption("metacheck.llm.model"))
  } else if (is.null(model)) {
    options(metacheck.llm.model = NULL)
    invisible(getOption("metacheck.llm.model"))
  } else if (is.character(model)) {
    options(metacheck.llm.model = model)
    invisible(getOption("metacheck.llm.model"))
  } else {
    stop("set llm_model with the name of a model, use `llm_model_list()` to get available models")
  }
}


#' Set or get metacheck LLM use
#'
#' Mainly for use in optional LLM workflows in modules
#'
#' @param llm_use if logical, sets whether to use LLMs
#'
#' @returns the current option value (logical)
#' @export
#'
#' @examples
#' if (llm_use()) {
#'   print("We can use LLMs")
#' } else {
#'   print("We will not use LLMs")
#' }
llm_use <- function(llm_use = NULL) {
  if (is.null(llm_use)) {
    use <- getOption("metacheck.llm.use")
    if (!use) {
      return(FALSE)
    }

    return(TRUE)
  } else if (as.logical(llm_use) %in% c(TRUE, FALSE)) {
    options(metacheck.llm.use = as.logical(llm_use))
    invisible(getOption("metacheck.llm.use"))
  } else {
    stop("Set llm_use with TRUE or FALSE")
  }
}
