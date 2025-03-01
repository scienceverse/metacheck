#' #' Machine Learning Classifier
#' #'
#' #' @param text the text to classify
#' #' @param model_dir the directory of the model
#' #' @param class_col the name of the classification column
#' #' @param map a vector of keys and values (e.g., `c("0" = "no", "1" = yes")`)
#' #' @param text_col the name of the text column if text is a data frame
#' #' @param return_prob whether to return the probability along with the classification
#' #'
#' #' @return a data frame with the text and classifications
#' #' @export
#' #'
#' ml <- function(text, model_dir,
#'                class_col = "classification",
#'                map = NULL,
#'                text_col = "text",
#'                return_prob = FALSE) {
#'   # error detection ----
#'   if (!dir.exists(model_dir)) {
#'     stop("The directory ", model_dir, " could not be found")
#'   }
#'   model_files <- list.files(model_dir)
#'   # TODO: check if all these are required
#'   expected_files <- c("config.json",
#'                       "model.safetensors",
#'                       "special_tokens_map.json",
#'                       "tokenizer_config.json",
#'                       "vocab.txt")
#'   if (!all(expected_files %in% model_files)) {
#'     missing_files <- setdiff(expected_files, model_files) |>
#'       paste(collapse = ", ")
#'     stop("The model is missing files: ", missing_files)
#'   }
#'
#'   # make a data frame if text is a vector----
#'   if (!is.data.frame(text)) {
#'     text <- data.frame(text = text)
#'     names(text) <- text_col
#'   }
#'
#'   # load/check python stuff ----
#'   if (!reticulate::py_available(TRUE)) {
#'     stop("You need to install Python to use machine learning functions")
#'   }
#'
#'   py_ml_classifier <- NULL # stops annoying cmdcheck warning
#'   # load script
#'   pyscript <- system.file("python/ml-classifier.py", package = "papercheck")
#'   tryCatch(reticulate::source_python(pyscript),
#'            error = \(e) { stop("Error in ml-classifier.py script, is your reticulate setup correct?")})
#'
#'   # set up progress bar ----
#'   if (verbose()) {
#'     pb <- progress::progress_bar$new(
#'       total = nrow(text), clear = FALSE,
#'       format = "Classifying text [:bar] :current/:total :elapsedfull"
#'     )
#'     pb$tick(0)
#'     Sys.sleep(0.2)
#'     pb$tick(0)
#'   }
#'
#'
#'   response <- replicate(nrow(text), list(), simplify = FALSE)
#'   for (i in 1:nrow(text)) {
#'     t <- text[[text_col]][[i]]
#'
#'     response[[i]] <- tryCatch({
#'       res <- py_ml_classifier(t, model_dir)
#'       list(
#'         classification = res$classification,
#'         probs = res$prob
#'       )
#'     }, error = function(e) {
#'       return(list(classification = NA,
#'                   probs = NA,
#'                   error = e$message))
#'     })
#'
#'     if (verbose()) pb$tick()
#'   }
#'
#'   text[[class_col]] <- sapply(response, \(x) x$classification)
#'   # add mapping ----
#'   if (!is.null(map)) {
#'     if (!all(text[[class_col]] %in% names(map))) {
#'       warning("The mapping was not applied because some values did not match")
#'     } else {
#'       text[[class_col]] <- dplyr::recode(text[[class_col]], !!!map)
#'     }
#'   }
#'
#'   # add probability ----
#'   if (return_prob) {
#'     text[[paste0(class_col, "_prob")]] <- sapply(response, \(x) x$probs)[2, ]
#'   }
#'
#'   # check for errors ----
#'   errors <- lapply(response, \(x) x$error)
#'   error_indices <- !sapply(errors, is.null)
#'   if (any(error_indices)) {
#'     text$error <- error_indices
#'
#'     warn <- paste(names(errors)[error_indices], collapse = ", ") |>
#'       paste("There were errors in the following:", x = _)
#'
#'     errors[error_indices] |>
#'       unique()|>
#'       paste("\n  * ", x = _) |>
#'       paste(warn, x = _) |>
#'       warning()
#'   }
#'
#'   return(text)
#' }
