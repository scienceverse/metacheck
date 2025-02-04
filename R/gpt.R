#' Query ChatGPT
#'
#' Ask ChatGPT any question you want about the text from a search_text()
#'
#' You need to use your own ChatGPT API key. To avoid having to type it out, add it to the .Renviron file in the following format (you can use `usethis::edit_r_environ()` to access the .Renviron file)
#'
#' CHATGPT_KEY="key_value_asdf"
#'
#' @param text The text to send to chatGPT (vector of strings, or data frame with the text in a column)
#' @param query The query to ask of chatGPT
#' @param context The context to send to chatGPT
#' @param text_col The name of the text column if text is a data frame
#' @param group_by the column(s) to group by if text is a data frame
#' @param CHATGPT_KEY your API key for ChatGPT
#' @param gpt_model the model name to pass to ChatOpenAI
#' @param chunk_size text chunk size for embeddings
#' @param chunk_overlap overlap between text chunks for embeddings
#' @param temperature temperature value for ChatGPT (0.0 to 2.0)
#' @param include_query Whether to include the query and context strings in the returned table
#'
#' @return a list of results
#'
#' @export
gpt <- function(text, query,
                context = "You are a scientist",
                text_col = "text",
                group_by = "id",
                CHATGPT_KEY = Sys.getenv("CHATGPT_KEY"),
                gpt_model = "gpt-3.5-turbo-1106",
                chunk_size = 500,
                chunk_overlap = 100,
                temperature = 0,
                include_query = FALSE) {
  ## error detection ----
  #site_down("chat.openai.com")

  if (CHATGPT_KEY == "") {
    stop("You need to include the argument CHATGPT_KEY or set the variable CHATGPT_KEY in your Renviron")
  }

  if (!is.numeric(chunk_size)) {
    stop("The argument `chunk_size` must be a positive integer")
  } else if (chunk_size < 1) {
    stop("The argument `chunk_size` must be larger than 0")
  }

  if (!is.numeric(chunk_overlap)) {
    stop("The argument `chunk_overlap` must be a positive integer")
  } else if (chunk_overlap < 0) {
    stop("The argument `chunk_overlap` must be 0 or larger")
  } else if (chunk_overlap > chunk_size) {
    stop("The argument `chunk_overlap` must be smaller than `chunk_size`")
  }

  if (!is.numeric(temperature)) {
    stop("The argument `temperature` must be a positive number")
  } else if (temperature < 0 | temperature > 2) {
    stop("The argument `temperature` must be between 0.0 and 2.0")
  }

  # make a data frame if text is a vector
  if (!is.data.frame(text)) {
    text <- data.frame(text = text)
    names(text) <- text_col
    for (x in group_by) text[[x]] = x
  }

  # set up answer data frame to return ----
  answer_df <- unique(text[, group_by, drop = FALSE])
  rownames(answer_df) <- NULL
  ncalls <- nrow(answer_df)
  if (ncalls == 0) stop("No calls to chatGPT")
  maxcalls <- getOption("papercheck.gpt_max_calls")
  if (ncalls > maxcalls) {
    stop("This would make ", ncalls, " calls to chatGPT, but your maximum number of calls is set to ", maxcalls, ". Use `set_gpt_max_calls() to change this.")
  }

  # set up progress bar ----
  if (getOption("scienceverse.verbose")) {
    pb <- progress::progress_bar$new(
      total = ncalls, clear = FALSE, show_after = 0,
      format = "Querying ChatGPT [:bar] :current/:total :elapsedfull"
    )
    pb$tick(0)
  }

  # load/check python stuff ----
  if (!reticulate::py_available(TRUE)) {
    stop("You need to install Python to use the chatGPT functions")
  }

  py_gpt <- NULL # stops annoying cmdcheck warning
  # load script
  pyscript <- system.file("python/gpt.py", package = "papercheck")
  tryCatch(reticulate::source_python(pyscript),
           error = \(e) { stop("Error in python, try using `gpt_setup()`")})

  # call chatgpt ----
  file <- tempfile(fileext = ".txt")
  response <- replicate(nrow(answer_df), list(), simplify = FALSE)

  for (i in 1:ncalls) {
    subtext <- dplyr::semi_join(text,
                                answer_df[i, , drop = FALSE],
                                by = group_by)
    write(subtext[[text_col]], file)

    response[[i]] <- tryCatch({
      py_gpt(file, query, context, CHATGPT_KEY,
             gpt_model = gpt_model,
             chunk_size = chunk_size,
             chunk_overlap = chunk_overlap,
             temperature = temperature)
    }, error = function(e) {
      return(list(result = list(answer = NA),
                  callback = list(total_cost = NA),
                  error = e$message))
    })

    if (getOption("scienceverse.verbose")) pb$tick()
  }

  answer_df$answer <- sapply(response, \(x) x$result$answer)
  answer_df$cost <- sapply(response, \(x) x$callback$total_cost)

  if (include_query) {
    answer_df$query = query
    answer_df$context = context
  }

  # check for errors ----
  errors <- lapply(response, \(x) x$error)
  error_indices <- !sapply(errors, is.null)
  if (any(error_indices)) {
    answer_df$error <- error_indices

    warn <- paste(names(errors)[error_indices], collapse = ", ") |>
      paste("There were errors in the following:", x = _)

    errors[error_indices] |>
      unique()|>
      paste("\n  * ", x = _) |>
      paste(warn, x = _) |>
      warning()
  }

  message("Total cost: $", sum(answer_df$cost) |> round(5))

  return(answer_df)
}

#' Set the maximum number of calls to ChatGPT
#'
#' @param n The maximum number of calls to ChatGPT that the gpt() function can make
#'
#' @return NULL
#' @export
#'
set_gpt_max_calls <- function(n = 10) {
  if (!is.numeric(n)) stop("n must be a number")
  n <- as.integer(n)
  if (n < 1) {
    warning("n must be greater than 0; it was not changed from ", getOption("papercheck.gpt_max_calls"))
  } else {
    options(papercheck.gpt_max_calls = n)
  }

  invisible()
}


gpt_setup <- function(envname = "r-reticulate") {
  if (!reticulate::py_available(TRUE)) {
    stop("You need to install python")
  }

  # set up virtual environment
  message("Setting up virtual environment ", envname, "...")
  req <- system.file("python/requirements.txt", package = "papercheck")
  if (!reticulate::virtualenv_exists(envname)) {
    reticulate::virtualenv_create(envname, requirements = req)
  } else {
    reticulate::virtualenv_install(envname, requirements = req)
  }

  # check for .Renviron
  rp <- Sys.getenv("RETICULATE_PYTHON") == ""
  ck <- Sys.getenv("CHATGPT_KEY") == ""
  if (rp | ck) {
    message <- "Add the following line to your .Renviron file, and restart R:"

    if (rp & ck) {
      message <- "Add the following lines to your .Renviron file, and restart R:"
    }
    if (rp) {
      message <- sprintf("%s\nRETICULATE_PYTHON=\"%s/%s/bin/python\"",
              message, reticulate::virtualenv_root(), envname)
    }
    if (ck) {
      message <- sprintf("%s\nCHATGPT_KEY=\"sk-proj-your-chatgpt-api-key-here\"", message)
    }
    base::message(message)
  }

  message("Done!")
}
