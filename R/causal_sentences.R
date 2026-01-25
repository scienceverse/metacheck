#' @title Extract causal relations from sentence(s) via a Hugging Face Space
#' @description
#' Sends one or more input sentences to a public Gradio app hosted on Hugging Face
#' (the *lakens-causal-sentences* Space), created based on code by Rasoul Norouzi, retrieves the result via Server-Sent Events (SSE),
#' and returns a tidy data frame with one row per detected cause–effect relation.
#'
#' @details
#' The function uses Gradio’s two-step queue API:
#' (1) a POST request enqueues the job and returns an `event_id`;
#' (2) a GET request streams `text/event-stream` frames until `event: complete`.
#' Many Gradio apps emit a **double-encoded** completion payload of the form
#' `["<final JSON string>"]`. This function unwraps that to obtain the final JSON
#' structure (an array of items containing `causal` and `relations`) before parsing.
#'
#' If a sentence has **no relations**, the output includes one row with `cause = NA`,
#' `effect = NA`, and the sentence’s `causal` flag (as returned by the model). For
#' sentences with **multiple relations**, the function returns one row per relation.
#'
#' @param sentence A character vector of one or more sentences to analyze for causal relations.
#' @param rel_mode Relation extraction mode. Options are `"auto"` (default) or `"neural_only"`.
#' @param rel_threshold Numeric threshold (default `0.5`) for deciding whether a relation is considered causal.
#' @param cause_decision Strategy for cause/effect detection. Options: `"cls_only"`, `"span_only"`, or `"cls+span"` (default).
#' @param timeout Maximum time (in seconds) to wait for the Hugging Face Space to return a result via SSE before aborting. Default is `10`.
#' @param verbose Logical; if `TRUE`, prints diagnostic information (URLs, status codes, event progression). Default `FALSE`.
#'
#' @return
#' A base `data.frame` with columns:
#' - `sentence` (character): the original input sentence,
#' - `causal`   (logical):   whether the sentence is causal per the model,
#' - `cause`    (character): extracted cause span (or `NA`),
#' - `effect`   (character): extracted effect span (or `NA`).
#'
#' @export
#'
#' @references
#' Norouzi, R., Kleinberg, B., Vermunt, J. K., & van Lissa, C. J. (2025).
#' Capturing causal claims: A fine-tuned text mining model for extracting causal sentences from social science papers.
#' *Research Synthesis Methods*, 16(1), 139–156. https://doi.org/10.1017/rsm.2024.13
#'
#' Hugging Face Model Card: rasoultilburg/SocioCausaNet
#' https://huggingface.co/rasoultilburg/SocioCausaNet
#'
#' @examples
#' \dontrun{
#' # Single sentence
#' df1 <- causal_relations("Smoking causes cancer")
#' print(df1)
#'
#' # Multiple sentences (batch)
#' df2 <- causal_relations(c("Insomnia causes depression.", "Rain leads to flooding."))
#' print(df2)
#'
#' # Custom parameters and verbose diagnostics
#' df3 <- causal_relations(
#'   sentence = "Stress increases blood pressure.",
#'   rel_mode = "auto",
#'   rel_threshold = 0.4,
#'   cause_decision = "cls+span",
#'   timeout = 10,
#'   verbose = TRUE
#' )
#' print(df3)
#' }
causal_relations <- function(sentence,
                             rel_mode = "auto",
                             rel_threshold = 0.5,
                             cause_decision = "cls+span",
                             timeout = 10,
                             verbose = FALSE) {
  # ---- Configuration (from API Recorder) ----
  base <- "https://lakens-causal-sentences.hf.space"
  prefix <- "gradio_api/call"
  api <- "/predict"

  # ---- handle empty vectors gracefully ----
  if (length(sentence) == 0 || all(trimws(sentence) == "")) {
    empty <- data.frame(
      sentence = character(0),
      causal = logical(0),
      cause = character(0),
      effect = character(0)
    )
    return(empty)
  }

  # ---- Parameter validation (targeted, helpful messages) ----
  if (!is.character(sentence) || length(sentence) < 1L) {
    stop("`sentence` must be a non-empty character vector.")
  }
  if (!is.character(rel_mode) || !(rel_mode %in% c("auto", "neural_only"))) {
    stop("`rel_mode` must be one of: 'auto', 'neural_only'.")
  }
  if (!is.numeric(rel_threshold) || length(rel_threshold) != 1L || is.na(rel_threshold)) {
    stop("`rel_threshold` must be a single numeric value.")
  }
  if (rel_threshold < 0 || rel_threshold > 1) {
    stop("`rel_threshold` must be in [0, 1].")
  }
  if (!is.character(cause_decision) || !(cause_decision %in% c("cls_only", "span_only", "cls+span"))) {
    stop("`cause_decision` must be one of: 'cls_only', 'span_only', 'cls+span'.")
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) || timeout <= 0) {
    stop("`timeout` must be a positive numeric scalar (seconds).")
  }
  if (!is.logical(verbose) || length(verbose) != 1L) {
    stop("`verbose` must be a single logical value (TRUE/FALSE).")
  }

  # ---- Helpers ----
  is_json_text <- function(s) {
    is.character(s) && length(s) == 1L && grepl("^[\\[{]", s)
  }

  post_enqueue <- function(one_sentence) {
    payload <- list(data = list(one_sentence, rel_mode, rel_threshold, cause_decision))
    body <- jsonlite::toJSON(payload, auto_unbox = TRUE)
    url_post <- paste0(base, "/", prefix, api)

    h_post <- curl::new_handle()
    curl::handle_setheaders(h_post,
      "Content-Type" = "application/json",
      "User-Agent"   = "causal_relations/0.1 (R curl/jsonlite)"
    )
    curl::handle_setopt(h_post, postfields = body)

    if (verbose) {
      message(sprintf("[POST] %s", url_post))
    }

    resp <- curl::curl_fetch_memory(url_post, handle = h_post)
    if (verbose) {
      message(sprintf("[POST] HTTP %s, %s bytes", resp$status_code, length(resp$content)))
    }
    if (resp$status_code < 200 || resp$status_code >= 300) {
      stop(sprintf(
        "POST failed (HTTP %s). Check base URL or parameters.\nURL: %s\nBody: %s",
        resp$status_code, url_post, rawToChar(resp$content)
      ))
    }

    # Sanity: Content-Type is often application/json
    # (not strictly required here because we parse by content)
    j <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE)
    event_id <- NULL
    if (!is.null(j$event_id)) event_id <- j$event_id
    if (is.null(event_id) && !is.null(j$EVENT_ID)) event_id <- j$EVENT_ID
    if (is.null(event_id) && !is.null(j$id)) event_id <- j$id
    if (is.null(event_id) && !is.null(j$event) && !is.null(j$event$id)) event_id <- j$event$id
    if (is.null(event_id)) {
      stop(sprintf(
        "No `event_id` found in POST response.\nURL: %s\nResponse: %s",
        url_post, jsonlite::toJSON(j, auto_unbox = TRUE)
      ))
    }
    event_id
  }

  get_until_complete <- function(event_id) {
    url_get <- paste0(base, "/", prefix, api, "/", event_id)
    complete_payload <- NULL
    buf <- raw(0)
    last_event <- NULL

    if (verbose) {
      message(sprintf("[GET/SSE] %s", url_get))
    }

    cb <- function(x) {
      buf <<- c(buf, x)
      s <- rawToChar(buf)
      parts <- strsplit(s, "\n", fixed = TRUE)[[1]]
      if (length(parts) > 1) {
        buf <<- charToRaw(utils::tail(parts, 1))
        for (ln in utils::head(parts, -1)) {
          if (!nzchar(ln)) next
          if (grepl("^event:\\s*", ln)) {
            last_event <<- sub("^event:\\s*", "", ln)
            if (verbose) message(sprintf("[SSE] event: %s", last_event))
          } else if (grepl("^data:\\s*", ln)) {
            payload_line <- sub("^data:\\s*", "", ln)
            if (!is.null(last_event) && identical(last_event, "complete")) {
              complete_payload <<- payload_line
              if (verbose) message("[SSE] received complete payload")
              return(0L) # stop streaming
            }
          }
        }
      }
      1L # continue streaming
    }

    h_get <- curl::new_handle()
    curl::handle_setopt(h_get,
      timeout = timeout,
      # optional: followlocation
      # followlocation = TRUE
      useragent = "causal_relations/0.1 (R curl/jsonlite)"
    )

    curl::curl_fetch_stream(url_get, cb, handle = h_get)

    if (is.null(complete_payload)) {
      stop(sprintf(
        "SSE timed out after %s seconds without `event: complete`.\nURL: %s\nEVENT_ID: %s",
        timeout, url_get, event_id
      ))
    }
    complete_payload
  }

  unwrap_final_json <- function(complete_payload) {
    # Try ["...JSON..."] form first
    decoded <- tryCatch(jsonlite::fromJSON(complete_payload, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.character(decoded) && length(decoded) >= 1L && is_json_text(decoded[[1]])) {
      return(decoded[[1]])
    }
    # Or direct JSON text
    if (is_json_text(complete_payload)) {
      return(complete_payload)
    }
    stop("Unexpected SSE payload format; cannot unwrap to final JSON.")
  }

  parse_relations_df <- function(final_json_text, original_sentence) {
    x <- jsonlite::fromJSON(final_json_text, simplifyVector = FALSE)
    if (!is.list(x)) {
      stop("Final payload is not a JSON array.")
    }
    rows <- list()
    idx <- 1L
    for (item in x) {
      causal_flag <- isTRUE(item$causal)
      rels <- item$relations
      if (is.null(rels) || length(rels) == 0L) {
        rows[[idx]] <- data.frame(
          sentence = as.character(original_sentence),
          causal = as.logical(causal_flag),
          cause = NA_character_,
          effect = NA_character_,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      } else {
        for (r in rels) {
          cause_val <- if (!is.null(r$cause)) as.character(r$cause) else NA_character_
          effect_val <- if (!is.null(r$effect)) as.character(r$effect) else NA_character_
          rows[[idx]] <- data.frame(
            sentence = as.character(original_sentence),
            causal = as.logical(causal_flag),
            cause = cause_val,
            effect = effect_val,
            stringsAsFactors = FALSE
          )
          idx <- idx + 1L
        }
      }
    }
    if (length(rows) == 0L) {
      df <- data.frame(
        sentence = character(),
        causal = logical(),
        cause = character(),
        effect = character(),
        stringsAsFactors = FALSE
      )
    } else {
      df <- do.call(rbind, rows)
    }
    df
  }

  # ---- Batch: loop over sentences, bind rows ----
  all_rows <- list()
  k <- 1L
  for (one_sentence in sentence) {
    if (verbose) {
      message(sprintf("=== Processing sentence %d/%d ===", k, length(sentence)))
    }
    event_id <- post_enqueue(one_sentence)
    complete_payload <- get_until_complete(event_id)
    final_json_text <- unwrap_final_json(complete_payload)
    df_one <- parse_relations_df(final_json_text, original_sentence = one_sentence)
    all_rows[[k]] <- df_one
    k <- k + 1L
  }

  if (length(all_rows) == 0L) {
    out <- data.frame(
      sentence = character(),
      causal = logical(),
      cause = character(),
      effect = character(),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, all_rows)
  }

  # Deterministic ordering: by input order, then cause/effect
  if (nrow(out) > 0L) {
    ord <- order(match(out$sentence, sentence), out$cause, out$effect, na.last = TRUE)
    out <- out[ord, , drop = FALSE]
    rownames(out) <- NULL
  }

  out
}
