# what regcheck_compare() returns: a tidy per-dimension comparison table
mock_regcheck_table <- data.frame(
  dimension = c("Sample size", "Hypotheses", "Exclusion criteria"),
  deviation_judgement = c("yes", "no", "missing"),
  paper_summary = c("120 participants", "H1: A > B", "not described"),
  prereg_summary = c("100 participants", "H1: A > B", "exclude RT < 200 ms"),
  deviation_information = c(
    "Paper sample (120) exceeds preregistered sample (100).",
    "Consistent.",
    "Cannot be assessed from the paper."
  ),
  paper_quotes = "[PAPER_0001] quote",
  prereg_quotes = "[REG_0001] quote"
)

test_that("reg_check module exists", {
  mods <- module_list()
  expect_true("reg_check" %in% mods$name)
})

test_that("no preregistrations", {
  paper <- test_paper("There are no links here.")
  mo <- module_run(paper, "reg_check")

  expect_equal(mo$traffic_light, "na")
  expect_match(mo$summary_text, "No preregistrations")
  expect_equal(mo$summary_table$regcheck_deviations, 0)
})

test_that("standalone run (prereg_check run internally)", {
  testthat::local_mocked_bindings(
    regcheck_compare = function(...) mock_regcheck_table,
    .package = "metacheck"
  )

  guid <- "5xysn"
  paper <- test_paper(url = paste0("https://osf.io/", guid))
  mo <- module_run(paper, "reg_check")

  # one row per compared dimension, labelled with prereg and paper ids
  expect_equal(nrow(mo$table), 3)
  expect_contains(names(mo$table),
                  c("dimension", "deviation_judgement", "prereg_id",
                    "paper_id"))
  expect_equal(unique(mo$table$prereg_id), guid)
  expect_equal(unique(mo$table$paper_id), paper_id(paper))

  # judgement counts in the summary table
  expect_equal(mo$summary_table$regcheck_deviations, 1)
  expect_equal(mo$summary_table$regcheck_consistent, 1)
  expect_equal(mo$summary_table$regcheck_unclear, 1)

  # presented in the report with the LLM disclaimer
  expect_true(any(grepl("RegCheck", mo$report)))
  expect_true(any(grepl("potential deviation", mo$report)))
  expect_true(any(grepl("large language model", mo$report)))
  expect_match(mo$summary_text, "flagged 1 potential deviation")

  # traffic light stays informational (no automated evaluation)
  expect_equal(mo$traffic_light, "info")
}, "mock")

test_that("chained after prereg_check", {
  testthat::local_mocked_bindings(
    regcheck_compare = function(...) mock_regcheck_table,
    .package = "metacheck"
  )

  paper <- demopaper()
  mo <- paper |>
    module_run("prereg_check") |>
    module_run("reg_check")

  # demopaper has 2 preregistrations -> one comparison per prereg
  expect_equal(nrow(mo$table), 6)
  expect_setequal(unique(mo$table$prereg_id), c("48ncu", "by8i8v"))
  expect_equal(mo$summary_table$regcheck_deviations, 2)
  expect_equal(mo$traffic_light, "info")
}, "mock")

test_that("duplicate prereg links are compared only once", {
  calls <- 0
  testthat::local_mocked_bindings(
    regcheck_compare = function(...) {
      calls <<- calls + 1
      mock_regcheck_table
    },
    .package = "metacheck"
  )

  paper <- test_paper(url = "https://osf.io/5xysn")
  prereg_out <- module_run(paper, "prereg_check")
  # simulate a paper that links the same preregistration twice
  prereg_out$table <- rbind(prereg_out$table, prereg_out$table)

  mo <- module_run(prereg_out, "reg_check")

  expect_equal(calls, 1)
  expect_equal(nrow(mo$table), 3)
  expect_equal(unique(mo$table$prereg_id), "5xysn")
}, "mock")

test_that("regcheck failure returns an error light", {
  testthat::local_mocked_bindings(
    regcheck_compare = function(...) stop("RegCheck server unreachable"),
    .package = "metacheck"
  )

  guid <- "5xysn"
  paper <- test_paper(url = paste0("https://osf.io/", guid))
  expect_message(
    mo <- module_run(paper, "reg_check"),
    "RegCheck comparison failed"
  )

  expect_equal(mo$traffic_light, "error")
  expect_match(mo$summary_text, "RegCheck comparison failed")
  expect_true(is.na(mo$summary_table$regcheck_deviations))
}, "mock")
