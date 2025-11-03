text <- c(
  "small effects, medium effects and large effects",
  "2 small effects",
  "small effect of X",
  "medium effects",
  "medium effect of Y",
  "large effect sizes",
  "large effect size of Z",
  "no effect of X",
  "any effect of Y",
  "an effect of Z",
  "effect size is 0.4",
  "effect = 0.4",
  "small children show an effect of 0.4",
  "effects of something large"
)
classification <- c(1, 1, 1, 1, 1, 1, 1,
                    0, 0 ,0, 0, 0, 0, 0)

test_that("exists", {
  expect_true(is.function(metacheck::distinctive_words))
  expect_no_error(helplist <- help(distinctive_words, metacheck))

  expect_true(is.function(metacheck::text_features))
  expect_no_error(helplist <- help(text_features, metacheck))
})

test_that("errors", {
  expect_error(distinctive_words())
  expect_error(text_features())
  expect_error(predict_classification())
})

test_that("distinctive_words", {
  # default
  words <- distinctive_words(text, classification)
  expected <- c("medium", "effect", "of", "###", "larg", "small",
                "an", "size", "x", "y", "z")
  expect_equal(words$word, expected)
  exp_names <- c("word", "n_1", "n_0", "total",
                 "freq_1", "freq_0", "difference")
  expect_equal(names(words), exp_names)

  # logical classification
  words <- distinctive_words(text, as.logical(classification))
  expect_equal(words$word, expected)
  exp_names <- c("word", "n_TRUE", "n_FALSE", "total",
                 "freq_TRUE", "freq_FALSE", "difference")
  expect_equal(names(words), exp_names)

  # two text value classification
  class <- ifelse(classification, "B", "A")
  words <- distinctive_words(text, class)
  expect_equal(words$word, expected)
  exp_names <- c("word", "n_B", "n_A", "total",
                 "freq_B", "freq_A", "difference")
  expect_equal(names(words), exp_names)

  # change n
  words <- distinctive_words(text, classification, n = 5)
  expect_equal(words$word, expected[1:5])

  # change stop_words
  stop_words <- c("an", "of","x", "y", "z")
  words <- distinctive_words(text, classification,
                              stop_words = stop_words)
  expected <- c("medium", "effect", "###", "larg", "small", "size")
  expect_equal(words$word, expected)

  # change number to remove
  words <- distinctive_words(text, classification, numbers = "remove")
  expected <- c("medium", "effect", "of", "larg", "small",
                "an", "size", "x", "y", "z")
  expect_equal(words$word, expected)

  # change number to specific
  words <- distinctive_words(text, classification, numbers = "specific")
  expected <- c("0.4", "medium", "effect", "of", "larg",
                "small", "an", "size", "x", "y", "z")
  expect_equal(words$word, expected)
})

test_that("text_features", {
  words <- distinctive_words(text, classification, numbers = "remove")

  # default
  features <- text_features(text, words)

  cols <- c("word_count", "has_number", "has_equals", words$word)
  expect_equal(names(features), cols)
  expect_equal(features$effect, rep(1, nrow(features)))

  # return
  features <- text_features(text, words, values = "count")
  expect_equal(features$effect[[1]], 3)

  # has_number
  features <- text_features(text, words, has_number = FALSE)
  cols <- c("word_count", "has_equals", words$word)
  expect_equal(names(features), cols)

  # has_symbols
  features <- text_features(text, words, has_symbol = c())
  cols <- c("word_count", "has_number", words$word)
  expect_equal(names(features), cols)

  # unnamed symbols
  features <- text_features(text, words, has_symbol = c("+", "="))
  cols <- c("word_count", "has_number", "has_+", "has_=", words$word)
  expect_equal(names(features), cols)

  # named symbols
  has_symbol <- c(has_plus = "+", has_equals = "=")
  features <- text_features(text, words, has_symbol = has_symbol)
  cols <- c("word_count", "has_number", names(has_symbol), words$word)
  expect_equal(names(features), cols)
})
