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
#' @param model the LLM model name (see `llm_model_list()` for groq models, or use "gemini")
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
#' \dontrun{
#'   text <- c("hello", "number", "ten", 12)
#'   query <- "Is this a number? Answer only 'TRUE' or 'FALSE'"
#'   is_number <- llm(text, query)
#'   is_number
#' }
llm <- function(text, query,
                text_col = "text",
                model = llm_model(),
                maxTokens = 1024,
                temperature = 0.5,
                top_p = 0.95,
                seed = sample(1000000:9999999, 1),
                API_KEY = NULL) {
  ## error detection ----
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
  if (ncalls > llm_max_calls()) {
    stop("This would make ", ncalls, " calls to the LLM, but your maximum number of calls is set to ", llm_max_calls(), ". Use `llm_max_calls()` to change this.", call. = FALSE)
  }

  # get API key
  if (is.null(API_KEY)) {
    apis <- list(
      groq = Sys.getenv("GROQ_API_KEY"),
      gemini = Sys.getenv("GEMINI_API_KEY"),
      google = Sys.getenv("GOOGLE_API_KEY")
    )
    API_KEY <- apis$groq
    if (model == "gemini") API_KEY <- apis$gemini
    if (API_KEY == "") API_KEY <- apis$google
  }

  if (!llm_use(API_KEY = API_KEY)) {
    stop("Set llm_use(TRUE) to use LLM functions")
  }

  # check valid groq model
  if (model != "gemini") {
    models <- llm_model_list(API_KEY)
    if (!model %in% models$id) {
      stop("The model '", model, "' is not available, see `llm_model_list()`")
    }
  }

  # Set up the llm ----
  responses <- replicate(length(unique_text), list(), simplify = FALSE)
  # setup
  params <- ellmer::params(
    temperature = as.numeric(temperature[1]),
    top_p = top_p[1],
    max_tokens = as.integer(maxTokens[1]),
    seed = seed
  )

  if (model == "gemini") {
    chat <- ellmer::chat_google_gemini(
      system_prompt = query,
      params = params
    )
  } else {
    chat <- ellmer::chat_groq(
      system_prompt = query,
      model = model,
      params = params
    )
  }

  # set up progress bar ----
  pb <- pb(ncalls, "Querying LLM [:bar] :current/:total :elapsedfull")

  # interate over the text ----
  # TODO: check rate limits and pause
  # https://console.groq.com/docs/rate-limits
  for (i in seq_along(unique_text)) {
    responses[[i]] <- tryCatch({
      answer <- chat$chat(unique_text[i], echo = FALSE)

      list(
        answer = trimws(answer)
      )
    }, error = function(e) {
      return(list(
        answer = NA,
        error = TRUE,
        error_msg = e$message
      ))
    })

    pb$tick()
  }

  # add responses to the return df ----
  response_df <- do.call(dplyr::bind_rows, responses)
  response_df[text_col] <- unique_text
  answer_df <- dplyr::left_join(text, response_df, by = text_col)

  # add metadata about the query ----
  class(answer_df) <- c("metacheck_llm", "data.frame")
  attr(answer_df, "llm") <- c(list(query = query,
                                   model = model),
                              params)

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
#' Returns a list of available models in groq, excluding whisper and vision models (for audio and images) and sorting by creation date. See <https://console.groq.com/docs/models> for more information.
#'
#' @param API_KEY groq API key from <https://console.groq.com/keys>
#'
#' @returns a data frame of models and info
#' @export
#'
#' @examples
#' \donttest{
#'   llm_model_list()
#' }
llm_model_list <- function(API_KEY = Sys.getenv("GROQ_API_KEY")) {

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

  rows <- models$active & !grepl("whisper|vision", models$id)
  cols <- c("id", "owned_by", "created", "context_window")
  active <- models[rows, cols]
  #active <- sort_by(active, rev(active$created))
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
llm_max_calls <- function(n = NULL) {
  if (is.null(n)) return(getOption("metacheck.llm_max_calls"))
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


# python_setup <- function(envname = "r-reticulate") {
#   if (!reticulate::py_available(TRUE)) {
#     stop("You need to install python (e.g. `reticulate::install_python()` )")
#   }
#
#   # set up virtual environment
#   message("Setting up virtual environment ", envname, "...")
#   req <- system.file("python/requirements.txt", package = "metacheck")
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





#' Set or get metacheck LLM use
#'
#' Mainly for use in optional LLM workflows in modules, also checks if the GROQ API key is set and returns false if it isn't.
#'
#' @param llm_use if logical, sets whether to use LLMs
#' @param API_KEY your API key for the LLM
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
llm_use <- function(llm_use = NULL,
                    API_KEY = Sys.getenv("GROQ_API_KEY")) {
  if (is.null(llm_use)) {
    use <- getOption("metacheck.llm.use")
    if (!use) return(FALSE)

    # # check if API KEY set
    # if (API_KEY == "") {
    #   message("Set the environment variable GROQ_API_KEY to use LLMs")
    #   return(FALSE)
    # }
    #
    # # check if api online
    # if (!online("api.groq.com")) {
    #   message("api.groq.com is not available")
    #   return(FALSE)
    # }

    return(TRUE)
  } else if (as.logical(llm_use) %in% c(TRUE, FALSE)) {
    options(metacheck.llm.use = as.logical(llm_use))
    invisible(getOption("metacheck.llm.use"))
  } else {
    stop("set llm_use with TRUE or FALSE")
  }
}
