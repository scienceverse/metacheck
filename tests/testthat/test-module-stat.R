test_that("stat_p_exact", {
  paper <- test_paper("The p = .0123")

  module <- "stat_p_exact"
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "green")
  expect_equal(nrow(mod_output$table), 1)

  # add imprecise p-values
  paper <- test_paper(c(
    "Bad p-value example (p < .05)",
    "Bad p-value example (p<.05)",
    "Bad p-value example (p < 0.05)",
    "Bad p-value example; p < .05",
    "Bad p-value example (p < .005)",
    "Bad p-value example (p > 0.05)",
    "Bad p-value example (p > .1)",
    "Bad p-value example (p = n.s.)",
    "Bad p-value example; p=ns",
    "OK p-value example; p < .001",
    "OK p-value example; p < .0005"
  ))

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 11)

  # iteration
  paper <- psychsci
  mod_output <- module_run(paper, module)
  lt05 <- grepl("p < .05", mod_output$table$text) |> sum()
  # expect_equal(lt05, 174)
  expect_equal(mod_output$table$p_comp[[1]], "<")
  expect_equal(mod_output$table$p_value[[1]], 0.001)
})

test_that("marginal", {
  module <- "marginal"

  # no relevant text
  paper <- test_paper("This effect is absent.")
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "green")
  expect_equal(nrow(mod_output$table), 0)

  # add marginal text
  paper <- test_paper(c(
    "This effect was marginally significant (p = .065).",
    "This effect approached significance (p = 0.34)."
  ))

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
#   paper <- demopaper() |>
#     search_text(".{30, }", section = "method", return = "sentence")
#   module <- "sample-size-ml"
#
#   mod_output <- module_run(paper, module)
#   expect_equal(mod_output$traffic_light, "green")
#   expect_equal(nrow(mod_output$table), 2)
#   expect_equal(mod_output$module, module)
# })



test_that("stat_check", {
  paper <- demopaper()
  module <- "stat_check"

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 2)
  expect_equal(mod_output$table$raw[[2]] |> gsub("\\s", "", x = _),
               "t(97.2)=-1.96,p=0.152")
  expect_equal(mod_output$module, module)

  # iteration
  paper <- psychsci[1:2]
  expect_no_error(
    mod_output <- module_run(paper, module)
  )
})


