# test_that("exists", {
#   expect_true(is.function(ml))
# })
#
#
# test_that("basic", {
#   skip("python install is messed up")
#   skip_on_cran()
#   model_dir <- system.file("modules/sample-size", package = "papercheck")
#
#   if (model_dir == "") {
#     skip("needs big classifier: sample-size")
#   }
#
#   text <- c(
#     "The sample size is 100.",
#     "My nose is cold."
#   )
#
#   results <- ml(text, model_dir)
#   expect_equal(results$text, text)
#   expect_equal(results$classification, c(1, 0))
#   expect_equal(names(results), c("text", "classification"))
#
#   # customise args
#   results <- ml(text, model_dir,
#                 text_col = "mytext",
#                 class_col = "x",
#                 map = c("0" = "no", "1" = "yes"),
#                 return_prob = TRUE)
#   expect_equal(results$mytext, text)
#   expect_equal(results$x, c("yes", "no"))
#   expect_true(results$x_prob[[1]] > results$x_prob[[2]])
#
#   # bad mapping
#   expect_warning( results <- ml(text, model_dir,
#                                 map = c("no" = 0, "yes" = 1)),
#                   "The mapping was not applied because some values did not match")
#   expect_equal(results$classification, c(1, 0))
#
#   # paper ----
#   skip("long")
#   paper <- read(demoxml())
#   text <- search_text(paper, section = "method")
#   system.time( gresults <- ml(text, model_dir) )
#
#   dplyr::filter(gresults, classification == 1)$text |>
#     paste(collapse = "\n\n") |> cat()
# })
