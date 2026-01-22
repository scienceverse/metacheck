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
#' @param system_prompt A system prompt to set the behavior of the assistant
#' @param text_col The name of the text column if text is a data frame
#' @param model the LLM model name (see `llm_model_list()`) in the format "provider" or "provider/model"
#' @param params a named list to pass to `ellmer::params()`
#'
#' @return a list of results
#'
#' @export
#' @examples
#' \dontrun{
#'   text <- c("hello", "number", "ten", 12)
#'   system_prompt <- "Is this a number? Answer only 'TRUE' or 'FALSE'"
#'   is_number <- llm(text, system_prompt)
#'   is_number
#' }
llm <- function(text, system_prompt,
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
  responses <- replicate(ncalls, list(), simplify = FALSE)

  if (ncalls == 0) stop("No calls to the LLM")
  if (ncalls > llm_max_calls()) {
    stop("This would make ", ncalls, " calls to the LLM, but your maximum number of calls is set to ", llm_max_calls(), ". Use `llm_max_calls()` to change this.", call. = FALSE)
  }

  # Set up the llm ----
  tryCatch({
    params <- do.call(ellmer::params, params)
  }, error = \(e) {
    stop("Misspecified params argument:\n", e$message, call. = FALSE)
  })

  tryCatch({
    chat <- ellmer::chat(
      name = model,
      system_prompt = system_prompt,
      params = params
    )
  }, error = \(e) {
    stop("Error setting up LLM:\n", e$message, call. = FALSE)
  })

  # set up progress bar ----
  pb <- pb(ncalls, "Querying LLM [:bar] :current/:total :elapsedfull")

  # iterate over the text ----
  # this works better than ellmer parallel functions for now because
  # it keeps the relationship between unique_text index and
  # response index where responses return have 0+ items
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

  # add metadata about the system_prompt ----
  class(answer_df) <- c("metacheck_llm", "data.frame")
  attr(answer_df, "llm") <- c(list(system_prompt = system_prompt,
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
#'   llm_model_list()
#' }
llm_model_list <- function(platform = NULL) {
  # get all ellmer models_* functions
  ef <- utils::getNamespaceExports("ellmer") |>
    grep("models_.+", x = _, value = TRUE)
  names(ef) <- gsub("models_", "", ef)
  funcs <- lapply(ef, \(x) getFromNamespace(x, "ellmer"))
  # ellmer doesn't have a groq model function, so use ours
  funcs$groq <- models_groq

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
models_groq <- function() {
  API_KEY <- Sys.getenv("GROQ_API_KEY")
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

  models$created_at <- as.POSIXct(models$created) |> format("%Y-%m-%d") |> as.Date()
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
