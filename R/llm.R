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
                params = list()) {
  ## error detection ----
  if (!llm_use()) {
    stop("Set llm_use(TRUE) to use LLM functions")
  }

  # make a data frame if text is a vector
  if (!is.data.frame(text)) {
    text <- data.frame(text = text)
    names(text) <- text_col
  }

  # set up answer data frame to return ----
  unique_text <- unique(text[[text_col]])
  ncalls <- length(unique_text)

  if (ncalls == 0) stop("No calls to the LLM")
  if (ncalls > llm_max_calls()) {
    stop("This would make ", ncalls, " calls to the LLM, but your maximum number of calls is set to ", llm_max_calls(), ". Use `llm_max_calls()` to change this.", call. = FALSE)
  }

  # Set up the llm ----
  # default temperature to 0 for deterministic extraction/classification
  if (is.null(params$temperature)) {
    params$temperature <- 0
  }

  # check if json schema type is set for a structured return
  structured <- !is.null(type)

  # ollama checks ----
  use_ollama_native <- FALSE
  if (grepl("^ollama", model)) {
    # ollama's /v1/ endpoint ignores think=FALSE; native /api/chat honours it
    use_ollama_native <- !isTRUE(params$think) %% !structured
    if (use_ollama_native) {
      ollama_options <- params
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

  # check params ----
  tryCatch({
    params <- do.call(ellmer::params, params)
  }, error = \(e) {
    stop("Misspecified params argument:\n", e$message, call. = FALSE)
  })

  # set up progress bar ----
  label <- if (structured) "Extracting data" else "Querying LLM"
  pb <- pb(ncalls, paste0(label, " [:bar] :current/:total :elapsedfull"))

  # iterate over the text ----
  responses <- lapply(seq_along(unique_text), function(i) {
    tryCatch({
      if (use_ollama_native) {
        # native ollama API: think=FALSE is honoured here, unlike /v1/
        answer <- .llm_ollama_native(
          unique_text[i], system_prompt, ollama_model,
          think = FALSE, options = ollama_options
        )
        pb$tick()
        list(answer = answer)
      } else {
        # fresh chat per call to avoid context accumulation
        msg <- utils::capture.output({
          chat <- ellmer::chat(
            name = model,
            system_prompt = system_prompt,
            params = params
          )
        }, type = "message")
        # only show message first time
        if (length(msg) && i == 1) pb$message(msg)

        if (structured) {
          result <- chat$chat_structured(unique_text[i], type = type)
          df <- .unnest_result(result)
          df$.join_key. <- unique_text[i]
          pb$tick()
          df
        } else {
          answer <- chat$chat(unique_text[i], echo = FALSE)
          pb$tick()
          list(answer = trimws(answer))
        }
      }
    }, error = function(e) {
      pb$tick()
      if (structured) {
        df <- data.frame(.error = TRUE, .error_msg = e$message)
        df$.join_key. <- unique_text[i]
        df
      } else {
        list(answer = NA, error = TRUE, error_msg = e$message)
      }
    })
  })

  # join responses back to input ----
  if (structured) {
    response_df <- dplyr::bind_rows(responses)
    text$.join_key. <- text[[text_col]]
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
      warning("There were extraction errors in rows: ",
              paste(error_rows, collapse = ", "),
              "\n", paste("  *", msgs, collapse = "\n"))
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
    tryCatch({
        # skip if google api key isn't set, otherwise it requests login
      if (p %in% c("google_gemini", "google_vertex") &&
        Sys.getenv("GOOGLE_API_KEY") == "") {
        return(NULL)
      }

      model_func <- funcs[[p]]
      m <- model_func()
      cols <- c("platform", names(m))
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
  if (is.null(model)) {
    return(getOption("metacheck.llm.model"))
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
