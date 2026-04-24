test_that("power, no LLM", {
  module <- "power"
  mods <- module_list()
  expect_true(module %in% mods$name)

  llm_use(FALSE)

  # no relevant text
  paper <- test_paper("I love to power pose.")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "na")
  expect_equal(nrow(mo$table), 0)
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$power_n, 0)
  expect_equal(mo$summary_table$power_complete, NA_integer_)

  # several power sentences in one paragraph
  power_text <- c(
    "An a priori power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that for a Cohen's d = 0.5, an alpha level of 0.05, and a desired power level of 80% required at least 64 participants in each group.",
    "A sensitivity power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that with 64 participants in each group, and an alpha level of 0.05, a desired power level of 80% was reached for an effect size of d = 0.5."
  )
  paper <- test_paper(power_text)
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$power_type, c("apriori"))
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$power_n, 1)
  expect_equal(mo$summary_table$power_complete, NA_integer_)

  # multiple paragraphs
  paper$text$paragraph_id <- 0:1
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$power_type, c("apriori", "sensitivity"))
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$power_n, 2)
  expect_equal(mo$summary_table$power_complete, NA_integer_)

  # multiple papers
  paper <- paperlist(
    test_paper(power_text[[1]]),
    test_paper(power_text[[2]])
  )
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$power_type, c("apriori", "sensitivity"))
  expect_equal(nrow(mo$summary_table), 2)
  expect_equal(mo$summary_table$power_n, c(1, 1))
  expect_equal(mo$summary_table$power_complete, rep(NA_integer_, 2))

  # only false positives
  paper <- test_paper(text = "Our 12 participants have a lot of power to detect a moth.")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "yellow")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$summary_table$power_n, 1)
})

test_that("power, with LLM", {
  skip_llm()

  module <- "power"
  llm_use(TRUE)
  llm_model("groq/llama-3.3-70b-versatile")

  # only false positives
  paper <- test_paper(text = "Our 12 participants have a lot of power to detect a moth.")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "na")
  expect_equal(nrow(mo$table), 0)
  expect_equal(mo$summary_table$power_n, 0)

  # only some info
  paper <- test_paper(text = "The a priori power analysis determined a sample size of 15 in each group for 80% power with a medium effect size."
  )
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "red")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$sample_size, 30)
  expect_equal(mo$table$power, 0.8)
  expect_equal(mo$table$effect_size, NA)
  expect_equal(mo$table$alpha_level, NA)
  expect_equal(mo$table$complete, FALSE)

  # the example from the prompt
  paper <- test_paper(text = "An a priori power analysis was conducted to estimate the sample size required to achieve 80% power to detect a Cohen's d of 0.2 using an unpaired t-test at an alpha level of 0.05. This required a total sample size of 300 participants. A second a priori power analysis was conducted to estimate the required sample size for a secondary outcome. To achieve 80% power to detect a Cohen's f of 0.1 using a one-way ANOVA, a sample size of 350 was required. The a priori power analyses were conducted with G*Power."
  )
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "red")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$statistical_test,
               c("unpaired t-test", "1-way ANOVA"))
  expect_equal(mo$table$sample_size, c(300, 350))
  expect_equal(mo$table$alpha_level, c(0.05, NA))
  expect_equal(mo$table$power, c(0.8, 0.8))
  expect_equal(mo$table$effect_size, c(0.2, 0.1))
  expect_equal(mo$table$effect_size_metric, c("Cohen's d", "Cohen's f"))

  expect_equal(mo$table$software, c("G*Power", "G*Power"))
  expect_equal(mo$table$complete, c(T, F))

  # no relevant text
  paper <- test_paper("I love to power pose.")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "na")
  expect_equal(nrow(mo$table), 0)
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$power_n, 0)
  expect_equal(mo$summary_table$power_complete, NA_integer_)

  # several power sentences in one paragraph
  power_text <- c(
    "An a priori power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that for a Cohen's d = 0.5, an alpha level of 0.05, and a desired power level of 80% required at least 64 participants in each group.",
    "A sensitivity power analysis for an independent samples t-test, conducted using G*Power, indicated that with 64 participants in each group, and an alpha level of 0.05, power of 0.91 was reached for an effect size of d = 0.5."
  )
  paper <- test_paper(power_text)
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "green")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$power_type, c("apriori", "sensitivity"))
  expect_equal(mo$table$statistical_test, c("unpaired t-test", "unpaired t-test"))
  expect_equal(mo$table$sample_size, c(128, 128))
  expect_equal(mo$table$alpha_level, c(0.05, 0.05))
  expect_equal(mo$table$power, c(0.8, 0.91))
  expect_equal(mo$table$effect_size, c(0.5, 0.5))
  expect_equal(mo$table$effect_size_metric, c("Cohen's d", "Cohen's d"))
  expect_equal(mo$table$software, c("pwr", "G*Power"))
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$power_n, 2)
  expect_equal(mo$summary_table$power_complete, 2)

  # incomplete power
  power_text <- c(
    "An a priori power analysis for an independent samples t-test, conducted using the pwr.t.test function from pwr (Champely, 2020), indicated that for a Cohen's d = 0.5, and a desired power level of 80% required at least 64 participants in each group.",
    "A sensitivity power analysis for a paired samples t-test, conducted using G-Power, indicated that with 64 participants, an adequate power was reached for an effect size of d = 0.5."
  )
  paper <- test_paper(power_text)
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "red")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$power_type, c("apriori", "sensitivity"))
  expect_equal(mo$table$statistical_test, c("unpaired t-test", "paired t-test"))
  expect_equal(mo$table$sample_size, c(128, 64))
  expect_equal(mo$table$alpha_level, c(NA, NA))
  expect_equal(mo$table$power, c(0.8, NA))
  expect_equal(mo$table$effect_size, c(0.5, 0.5))
  expect_equal(mo$table$effect_size_metric, c("Cohen's d", "Cohen's d"))
  expect_equal(mo$table$software, c("pwr", "G*Power"))
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$power_n, 2)
  expect_equal(mo$summary_table$power_complete, 0)

  # multiple papers
  paper <- paperlist(
    test_paper(power_text[[1]]),
    test_paper(power_text[[2]])
  )
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "red")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$power_type, c("apriori", "sensitivity"))
  expect_equal(mo$table$statistical_test, c("unpaired t-test", "paired t-test"))
  expect_equal(mo$table$sample_size, c(128, 64))
  expect_equal(mo$table$alpha_level, c(NA, NA))
  expect_equal(mo$table$power, c(0.8, NA))
  expect_equal(mo$table$effect_size, c(0.5, 0.5))
  expect_equal(mo$table$effect_size_metric, c("Cohen's d", "Cohen's d"))
  expect_equal(mo$table$software, c("pwr", "G*Power"))
  expect_equal(nrow(mo$summary_table), 2)
  expect_equal(mo$summary_table$power_n, c(1, 1))
  expect_equal(mo$summary_table$power_complete, c(0,0))
})


