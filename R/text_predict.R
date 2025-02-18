#' Get Distinctive Words
#'
#' @param text a vector of text to assess
#' @param classification a vector of the classification
#' @param n the number of top distnctive words to get
#'
#' @returns a data frame of the words
#' @export
distinctive_words <- function(text, classification, n = 15) {
  ground_truth <- data.frame(
    text = text,
    classification = classification
  )

  word_frequencies <- ground_truth |>
    tidytext::unnest_tokens(word, text) |>
    dplyr::anti_join(tidytext::stop_words, by = "word") |>
    # remove blanks
    dplyr::filter(!grepl("^\\d*$", word)) |>
    dplyr::count(classification, word, sort = TRUE)

  # tfidf <- tidytext::bind_tf_idf(tbl = word_frequencies,
  #                                term = word,
  #                                document = classification,
  #                                n = n)

  word_importance <- word_frequencies |>
    tidyr::pivot_wider(names_from = classification,
                values_from = n,
                values_fill = 0) |>
    setNames(c("word", "n_0", "n_1")) |>
    dplyr::mutate(
      total = n_0 + n_1,
      difference = abs(n_1/sum(n_1) - n_0/sum(n_0))
    ) %>%
    dplyr::arrange(dplyr::desc(difference)) |>
    dplyr::filter(total >= 3)  # Filter out rare words

  # Return top N most distinctive words
  return(head(word_importance, n))
}

#' Text features
#'
#' @param text a vector of the text strings to extract features from
#' @param words a vector of words to find
#' @param has_number whether to code the presence of numbers
#' @param has_symbol a named vector of symbols to detect
#'
#' @returns a data frame of features for each text
#' @export
text_features <- function(text, words, has_number = TRUE,
                        has_symbol = c(has_equals = "=")) {
  if (is.data.frame(words)) {
    words <- words$word
  }

  # Basic features
  basic_features <- data.frame(
    word_count = stringr::str_count(text, "\\w+")
  )

  if (has_number) {
    basic_features$has_number <- grepl("\\d+", text) |> as.numeric()
  }

  if (names(has_symbol) |> is.null()) {
    names(has_symbol) <- paste0("has_", has_symbol)
  }

  for (i in seq_along(has_symbol)) {
    name <- names(has_symbol)[[i]]
    symbol <- has_symbol[[i]]
    basic_features[[name]] = grepl(symbol, text, fixed = TRUE) |> as.numeric()
  }

  # Word-based features from important words
  # TODO: make this use the same method as original word detection
  #   so it doesn't match stems of words
  word_features <- sapply(words, function(w) {
    as.numeric(grepl(w, text, ignore.case = TRUE))
  })

  # Combine all features
  cbind(basic_features, as.data.frame(word_features))
}

#' Predict Classification
#'
#' @param model the model to fit
#' @param text the text to predict
#' @param words a vector of words to find
#' @param has_number whether to code the presence of numbers
#' @param has_symbol a named vector of symbols to detect
#'
#' @returns a vector of predictions
#' @export
predict_classification <- function(model, text, words,
                                   has_number = TRUE,
                                   has_symbol = c(has_equals = "=")) {
  features <- text_features(text, words, has_number, has_symbol)
  predictions <- predict(model, features, type = "response")

  return(predictions)
}
