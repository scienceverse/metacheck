#' Query an LLM
#'
#' Ask a large language model (LLM) any question you want about a vector of text or the text from a search_text().
#'
#' You will need to get your own API key from <https://console.groq.com/keys>. To avoid having to type it out, add it to the .Renviron file in the following format (you can use `usethis::edit_r_environ()` to access the .Renviron file)
#'
#' GROQ_API_KEY="key_value_asdf"
#'
#' See <https://console.groq.com/docs> for more information
#'
#' @param text The text to send to the LLM (vector of strings, or data frame with the text in a column)
#' @param query The query to ask of the LLM
#' @param text_col The name of the text column if text is a data frame
#' @param model the LLM model name (see `llm_models()`)
#' @param maxTokens The maximum integer of completion tokens returned per query
#' @param temperature Controls randomness in responses. Lower values make responses more deterministic. Recommended range: 0.5-0.7 to prevent repetitions or incoherent outputs; valued between 0 inclusive and 2 exclusive
#' @param top_p Nucleus sampling threshold (between 0 and 1); usually alter this or temperature, but not both
#' @param seed Set for reproducible responses
#' @param API_KEY your API key for the LLM
#'
#' @return a list of results
#'
#' @export
#' @examples
#' \donttest{
#'   text <- c("hello", "number", "ten", 12)
#'   query <- "Is this a number? Answer only 'TRUE' or 'FALSE'"
#'   is_number <- llm(text, query)
#'   is_number
#' }
llm <- function(text, query,
                text_col = "text",
                model = "llama3-70b-8192",
                maxTokens = 1024,
                temperature = 0.5,
                top_p = 0.95,
                seed = sample(1000000:9999999, 1),
                API_KEY = Sys.getenv("GROQ_API_KEY")) {
  ## error detection ----
  #site_down("api.groq.com")

  if (API_KEY == "") {
    stop("You need to include the argument API_KEY or set the variable GROQ_API_KEY in your Renviron")
  }

  models <- llm_models(API_KEY)
  if (!model %in% models$id) {
    stop("The model '", model, "' is not available, see `llm_models()`")
  }

  if (!is.numeric(temperature)) {
    stop("The argument `temperature` must be a positive number")
  } else if (temperature < 0 | temperature > 2) {
    stop("The argument `temperature` must be between 0.0 and 2.0")
  }

  if (!is.numeric(top_p)) {
    stop("The argument `top_p` must be a positive number")
  } else if (top_p < 0 | top_p > 1) {
    stop("The argument `top_p` must be between 0.0 and 1.0")
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
  maxcalls <- getOption("papercheck.llm_max_calls")
  if (ncalls > maxcalls) {
    stop("This would make ", ncalls, " calls to the LLM, but your maximum number of calls is set to ", maxcalls, ". Use `set_llm_max_calls()` to change this.")
  }


  # Set up the llm ----
  responses <- replicate(length(unique_text), list(), simplify = FALSE)
  # setup
  url <- "https://api.groq.com/openai/v1/chat/completions"

  messages <- list(list(role = "system", content = query),
                   list(role = "user", content = ""))

  bodylist <- list(messages = messages,
                   model = model[1],
                   temperature = as.numeric(temperature[1]),
                   max_completion_tokens = as.integer(maxTokens[1]),
                   top_p = top_p[1],
                   seed = seed,
                   stream = FALSE,
                   stop = NULL)

  config <- httr::add_headers(
    Authorization = paste("Bearer", API_KEY),
    `Content-Type` = "application/json"
  )

  # set up progress bar ----
  if (verbose()) {
    pb <- progress::progress_bar$new(
      total = ncalls, clear = FALSE, show_after = 0,
      format = "Querying LLM [:bar] :current/:total :elapsedfull"
    )
    pb$tick(0)
  }

  # interate over the text ----
  # TODO: check rate limits and pause
  # https://console.groq.com/docs/rate-limits
  for (i in seq_along(unique_text)) {
    bodylist$messages[[2]]$content <- unique_text[i]

    responses[[i]] <- tryCatch({

      response <- httr::POST(
        url, config,
        body = bodylist,
        encode = "json")

      content <- httr::content(response)

      if (!response$status_code %in% 200:299) {
        # TODO: better error messages
        stop(response$status_code)
      }

      list(
        answer = content$choices[[1]]$message$content |> trimws(),
        time = content$usage$total_time,
        tokens = content$usage$total_tokens
      )
    }, error = function(e) {
      return(list(
        answer = NA,
        error = TRUE,
        error_msg = e$message
      ))
    })

    if (verbose()) pb$tick()
  }

  # add responses to the return df ----
  response_df <- do.call(dplyr::bind_rows, responses)
  response_df[text_col] <- unique_text
  answer_df <- dplyr::left_join(text, response_df, by = text_col) |>
    # set time and tokens to 0 if duplicate text
    dplyr::mutate(time = ifelse(dplyr::row_number() == 1, time, 0),
                  tokens = ifelse(dplyr::row_number() == 1, tokens, 0),
                  .by = dplyr::all_of(text_col))

  # add metadata about the query ----
  class(answer_df) <- c("ppchk_llm", "data.frame")
  attr(answer_df, "llm") <- bodylist
  attr(answer_df, "llm")$messages[[2]]$content <- ""

  # warn about errors ----
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

  return(answer_df)
}

#' List Available LLM Models
#'
#' Returns a list of available models in groq, excluding whisper models (for audio) and sorting by creation date. See <https://console.groq.com/docs/models> for more information.
#'
#' @param API_KEY groq API key from <https://console.groq.com/keys>
#'
#' @returns a data frame of models and info
#' @export
#'
#' @examples
#' \donttest{
#'   llm_models()
#' }
llm_models <- function(API_KEY = Sys.getenv("GROQ_API_KEY")) {
  url <- "https://api.groq.com/openai/v1/models"
  config <- httr::add_headers(
    Authorization = paste("Bearer", API_KEY)
  )

  response <- httr::GET(
    url, config,
    encode = "json")

  models <- do.call(dplyr::bind_rows,
                    httr::content(response)$data) |>
    data.frame()

  active <- dplyr::filter(models, active,
                          !grepl("whisper", id)) |>
    dplyr::select(id, owned_by, created, context_window) |>
    dplyr::arrange(dplyr::desc(created))
  active$created <- as.POSIXct(active$created) |> format("%Y-%m-%d")
  return(active)
}

#' Set the maximum number of calls to the LLM
#'
#' @param n The maximum number of calls that the llm() function can make
#'
#' @return NULL
#' @export
#'
set_llm_max_calls <- function(n = 10) {
  if (!is.numeric(n)) stop("n must be a number")
  n <- as.integer(n)
  if (n < 1) {
    warning("n must be greater than 0; it was not changed from ", getOption("papercheck.llm_max_calls"))
  } else {
    options(papercheck.llm_max_calls = n)
  }

  invisible()
}


# python_setup <- function(envname = "r-reticulate") {
#   if (!reticulate::py_available(TRUE)) {
#     stop("You need to install python (e.g. `reticulate::install_python()` )")
#   }
#
#   # set up virtual environment
#   message("Setting up virtual environment ", envname, "...")
#   req <- system.file("python/requirements.txt", package = "papercheck")
#   if (!reticulate::virtualenv_exists(envname)) {
#     reticulate::virtualenv_create(envname, requirements = req)
#   } else {
#     reticulate::virtualenv_install(envname, requirements = req)
#   }
#
#   # check for .Renviron
#   if (Sys.getenv("RETICULATE_PYTHON") == "") {
#     message <- "Add the following line to your .Renviron file, and restart R:"
#
#      message <- sprintf("%s\nRETICULATE_PYTHON=\"%s/%s/bin/python\"",
#               message, reticulate::virtualenv_root(), envname)
#
#     base::message(message)
#   }
#
#   message("Done!")
# }

