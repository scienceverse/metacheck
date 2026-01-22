test_that("power, no LLM", {
  module <- "power"
  mods <- module_list()
  expect_true(module %in% mods$name)

  llm_use(FALSE)

  # no relevant text
  paper <- psychsci[[1]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), 0)
  expect_equal(nrow(mod_output$summary_table), 1)
  expect_equal(mod_output$summary_table$power_n, 0)
  expect_equal(mod_output$summary_table$power_complete, NA_integer_)

  # several power sentences in one paragraph
  paper <- psychsci[[10]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(nrow(mod_output$table), 2)
  expect_equal(nrow(mod_output$summary_table), 1)
  expect_equal(mod_output$summary_table$power_n, 2)
  expect_equal(mod_output$summary_table$power_complete, NA_integer_)

  # multiple papers
  paper <- psychsci[10:15]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(nrow(mod_output$table), 4)
  expect_equal(nrow(mod_output$summary_table), 6)
  expect_equal(mod_output$summary_table$power_n, c(2, 0, 0, 0, 1, 1))
  expect_equal(mod_output$summary_table$power_complete, rep(NA_integer_, 6))

  # only false positives
  paper <- paper()
  paper$full_text <- data.frame(
    id = paper$id,
    text = "Our 12 participants have a lot of power to detect a moth."
  )
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(nrow(mod_output$table), 1)
  expect_equal(mod_output$summary_table$power_n, 1)
})

# httptest::start_capturing()
# httptest::use_mock_api()

test_that("power, with LLM", {
  skip_llm()

  module <- "power"
  llm_use(TRUE)
  llm_model("groq/llama-3.3-70b-versatile")

  # only false positives
  paper <- paper()
  paper$full_text <- data.frame(
    id = paper$id,
    text = "Our 12 participants have a lot of power to detect a moth."
  )
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), 0)
  expect_equal(mod_output$summary_table$power_n, 0)

  # only some info
  paper <- paper()
  paper$full_text <- data.frame(
    id = paper$id,
    text = "The a priori power analysis determined a sample size of 15 in each group for 80% power with a medium effect size."
  )
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 1)
  expect_equal(mod_output$table$sample_size, 30)
  expect_equal(mod_output$table$power, 0.8)
  expect_equal(mod_output$table$effect_size, NA)
  expect_equal(mod_output$table$alpha_level, NA)
  expect_equal(mod_output$table$complete, FALSE)

  # the example from the prompt
  paper <- paper()
  paper$full_text <- data.frame(
    id = paper$id,
    text = "An a priori power analysis was conducted to estimate the sample size required to achieve 80% power to detect a Cohen's d of 0.2 using an unpaired t-test at an alpha level of 0.05. This required a total sample size of 300 participants. A second a priori power analysis was conducted to estimate the required sample size for a secondary outcome. To achieve 80% power to detect a Cohen's f of 0.1 using a one-way ANOVA, a sample size of 350 was required. The a priori power analyses were conducted with G*Power."
  )
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 2)
  expect_equal(mod_output$table$statistical_test,
               c("unpaired t-test", "1-way ANOVA"))
  expect_equal(mod_output$table$sample_size, c(300, 350))
  expect_equal(mod_output$table$alpha_level, c(0.05, NA))
  expect_equal(mod_output$table$power, c(0.8, 0.8))
  expect_equal(mod_output$table$effect_size, c(0.2, 0.1))
  expect_equal(mod_output$table$effect_size_metric, c("Cohen's d", "Cohen's f"))

  expect_equal(mod_output$table$software, c("G*Power", "G*Power"))
  expect_equal(mod_output$table$complete, c(T, F))

  # no relevant text
  paper <- psychsci[[1]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), 0)
  expect_equal(nrow(mod_output$summary_table), 1)
  expect_equal(mod_output$summary_table$power_n, 0)
  expect_equal(mod_output$summary_table$power_complete, NA_integer_)

  # several power sentences in one paragraph
  paper <- psychsci[[10]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 3)
  expect_setequal(mod_output$table$sample_size, c(13500, 24, 72))
  expect_equal(nrow(mod_output$summary_table), 1)
  expect_equal(mod_output$summary_table$power_n, 3)
  expect_equal(mod_output$summary_table$power_complete, 0)

  # multiple papers
  paper <- psychsci[10:15]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 5)
  expect_equal(nrow(mod_output$summary_table), 6)
  expect_equal(mod_output$table$complete, c(F, F, F, F, F))
  expect_equal(mod_output$summary_table$power_n, c(3, 0, 0, 0, 1, 1))
  expect_equal(mod_output$summary_table$power_complete, c(0, NA, NA, NA, 0, 0))
})

# httptest::stop_mocking()
# httptest::stop_capturing()

