test_that("stat_p_exact", {
  paper <- demodir() |> read()
  paper <- paper[[1]]

  module <- "stat_p_exact"
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "green")
  expect_equal(nrow(mod_output$table), 14)

  # add imprecise p-values
  paper$full_text[1, "text"] <- "Bad p-value example (p < .05)"
  paper$full_text[2, "text"] <- "Bad p-value example (p<.05)"
  paper$full_text[3, "text"] <- "Bad p-value example (p < 0.05)"
  paper$full_text[4, "text"] <- "Bad p-value example; p < .05"
  paper$full_text[5, "text"] <- "Bad p-value example (p < .005)"
  paper$full_text[6, "text"] <- "Bad p-value example (p > 0.05)"
  paper$full_text[7, "text"] <- "Bad p-value example (p > .1)"
  paper$full_text[8, "text"] <- "Bad p-value example (p = n.s.)"
  paper$full_text[9, "text"] <- "Bad p-value example; p=ns"
  paper$full_text[10, "text"] <- "OK p-value example; p < .001"
  paper$full_text[11, "text"] <- "OK p-value example; p < .0005"

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 25)

  # iteration
  paper <- psychsci
  mod_output <- module_run(paper, module)
  lt05 <- grepl("p < .05", mod_output$table$text) |> sum()
  expect_equal(lt05, 174)
  expect_equal(mod_output$table$p_comp[[1]], "<")
  expect_equal(mod_output$table$p_value[[1]], 0.001)
})

test_that("marginal", {
  paper <- demodir() |> read()
  paper <- paper[[1]]
  module <- "marginal"

  # no relevant text
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "green")
  expect_equal(nrow(mod_output$table), 0)
  expect_equal(mod_output$report, "No effects were described with terms related to 'marginally significant'.")

  # add marginal text
  paper$full_text[1, "text"] <- "This effect was marginally significant."
  paper$full_text[12, "text"] <- "This effect approached significance."

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 2)

  # iteration
  mod_output <- module_run(psychsci, module)
  # expect_true(unique(mod_output$table$id) |> length() > 1)
})

# test_that("sample-size", {
#   skip("python install is messed up")
#   skip_on_cran()
#   model_dir <- system.file("modules/sample-size", package = "metacheck")
#
#   if (model_dir == "") {
#     skip("needs big classifier: sample-size")
#   }
#
#   paper <- demoxml() |> read() |>
#     search_text(".{30, }", section = "method", return = "sentence")
#   module <- "sample-size-ml"
#
#   mod_output <- module_run(paper, module)
#   expect_equal(mod_output$traffic_light, "green")
#   expect_equal(nrow(mod_output$table), 2)
#   expect_equal(mod_output$module, module)
# })



test_that("stat_check", {
  paper <- demoxml() |> read()
  module <- "stat_check"

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 2)
  expect_equal(mod_output$table$raw[[2]], "t(97.2) = -1.96, p = 0.152")
  expect_equal(mod_output$module, module)

  # iteration
  paper <- psychsci[1:2]
  expect_no_error(
    mod_output <- module_run(paper, module)
  )
})


