test_that("exists", {
  expect_true(is.function(papercheck::distinctive_words))
  expect_no_error(helplist <- help(distinctive_words, papercheck))

  expect_true(is.function(papercheck::text_features))
  expect_no_error(helplist <- help(text_features, papercheck))

  expect_true(is.function(papercheck::predict_classification))
  expect_no_error(helplist <- help(predict_classification, papercheck))
})

# test_that("errors", {
#
# })
#
# test_that("defaults", {
#
# })
