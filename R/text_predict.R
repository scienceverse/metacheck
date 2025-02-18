#' Get Distinctive Words
#'
#' @param text a vector of text to assess
#' @param classification a vector of the classification
#' @param n the number of top distinctive words to get
#' @param stop_words a vector or data frame of words to exclude
#' @param numbers what to do with numeric values: "any", "specific", "remove"
#' @param stem_language the language to use for stemming words, from SnowballC::getStemLanguages(), set to FALSE for no stemming
#' @param min_total the minimum total number of incidences to incude a word, defaults to 10% of the number of text strings
#'
#' @returns a data frame of the words
#' @export
distinctive_words <- function(text, classification,
                              n = Inf,
                              stop_words = c(),
                              numbers = c("any", "specific", "remove"),
                              stem_language = "porter",
                              min_total = length(text)/10) {
  ground_truth <- data.frame(
    text = text,
    classification = classification
  )

  # set up stop words ----
  if (length(stop_words) == 0) stop_words <- character(0)
  if (!is.data.frame(stop_words)) {
    stop_words <- data.frame(word = stop_words)
  }
  if (!"word" %in% names(stop_words)) {
    names(stop_words)[1] <- "word"
  }

  # tokenize ----
  word_frequencies <- ground_truth |>
    tidytext::unnest_tokens(word, text) |>
    dplyr::anti_join(stop_words, by = "word")

  # numbers ----
  numbers <- match.arg(numbers)
  if (numbers == "remove") {
    word_frequencies <- word_frequencies |>
      dplyr::filter(!grepl("^\\d*(\\.)?\\d+$", word))
  } else if (numbers == "any") {
    word_frequencies$word <- gsub("^\\d*(\\.)?\\d+$", "###", word_frequencies$word)
  }

  # stemming ----
  if (stem_language %in% SnowballC::getStemLanguages()) {
    word_frequencies$word <- SnowballC::wordStem(
      words = word_frequencies$word,
      language = stem_language
    )
  }

  # select important words ----
  word_importance <- word_frequencies |>
    dplyr::count(classification, word, sort = TRUE) |>
    tidyr::pivot_wider(names_from = classification,
                values_from = n,
                values_fill = 0) |>
    stats::setNames(c("word", "n_0", "n_1")) |>
    dplyr::mutate(
      total = n_0 + n_1,
      freq_0 = n_0/sum(n_0),
      freq_1 = n_1/sum(n_1),
      difference = abs(freq_0 - freq_1)
    ) |>
    dplyr::filter(total >= min_total) |>
    dplyr::arrange(dplyr::desc(difference))

  # Return top N most distinctive words
  return(utils::head(word_importance, n))
}

#' Text features
#'
#' @param text a vector of the text strings to extract features from
#' @param words a vector of words to find
#' @param word_count whether to include word count
#' @param has_number whether to code the presence of numbers
#' @param has_symbol a named vector of symbols to detect
#' @param stem_language the language to use for stemming words, from SnowballC::getStemLanguages(), set to FALSE for no stemming
#' @param values whether to return the count of words (0+) in a string or the presence (0/1)
#'
#' @returns a data frame of features for each text
#' @export
text_features <- function(text, words,
                          word_count = TRUE,
                          has_number = TRUE,
                          has_symbol = c(has_equals = "="),
                          stem_language = "porter",
                          values = c("presence", "count")) {
  if (is.data.frame(words)) {
    words <- words$word
  }

  # deal with ### as a word
  if ("###" %in% words) {
    has_number <- TRUE
    words <- words[!grepl("^###$", words)]
  }

  # stemming ----
  word_frequencies <- data.frame(
    id = seq_along(text),
    text = text
  ) |>
    tidytext::unnest_tokens(word, text)

  if (stem_language %in% SnowballC::getStemLanguages()) {
    word_frequencies$word <- SnowballC::wordStem(
      words = word_frequencies$word,
      language = stem_language
    )
  }

  word_features <- word_frequencies |>
    dplyr::filter(word %in% words) |>
    dplyr::count(id, word)

  if (match.arg(values) == "presence") {
    word_features$n <- as.integer(word_features$n > 0)
  }

  word_features <- word_features |>
    tidyr::pivot_wider(names_from = word,
                       values_from = n,
                       values_fill = 0)

  missing_words <- setdiff(words, names(word_features))
  for (w in missing_words) {
    word_features[[w]] <- 0
  }

  word_features <- data.frame(id = seq_along(text)) |>
    dplyr::left_join(word_features, by = "id")

  word_features[is.na(word_features)] <- 0
  word_features <- word_features[, words]

  # Basic features
  basic_features <- data.frame(
    row.names = seq_along(text)
  )

  if (word_count) {
    basic_features$word_count <- dplyr::count(word_frequencies, id)$n
  }

  if (has_number) {
    basic_features$has_number <- grepl("\\d+", text) |>
      as.numeric()
  }

  if (length(has_symbol) > 0 &&
      names(has_symbol) |> is.null()) {
    names(has_symbol) <- paste0("has_", has_symbol)
  }

  for (i in seq_along(has_symbol)) {
    name <- names(has_symbol)[[i]]
    symbol <- has_symbol[[i]]
    basic_features[[name]] = grepl(symbol, text, fixed = TRUE) |> as.numeric()
  }

  # Combine all features
  cbind(basic_features, word_features)
}

