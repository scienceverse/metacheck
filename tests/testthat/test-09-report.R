test_that("error", {
  expect_true(is.function(metacheck::report))

  expect_error(report(1), "The paper argument must be a paper object")

  paper <- demoxml() |> read()
  expect_error( paper_report <- report(paper, modules = c("notamodule")),
                "notamodule")
})

test_that("defaults", {
  paper <- demoxml() |> read()
  # skip modules that require external APIs
  modules <- c(
    "stat_p_exact", "marginal", "stat_effect_size", "stat_check"
  )

  # qmd
  paper <- psychsci[[94]]
  qmd <- tempfile(fileext = ".qmd")
  if (file.exists(qmd)) unlink(qmd)
  paper_report <- report(paper, modules,
    output_file = qmd,
    output_format = "qmd"
  )
  expect_equal(paper_report, qmd)
  expect_true(file.exists(qmd))
  # rstudioapi::documentOpen(qmd)

  skip_if_not_installed("quarto")
  skip_on_cran()

  # html
  html <- tempfile(fileext = ".html")
  if (file.exists(html)) unlink(html)
  paper_report <- report(paper, modules,
    output_file = html,
    output_format = "html"
  )
  expect_equal(paper_report, html)
  expect_true(file.exists(html))
  # browseURL(html)

  # pdf
  skip("pdf")
  pdf <- tempfile(fileext = ".pdf")
  if (file.exists(pdf)) unlink(pdf)
  paper_report <- report(paper, modules,
    output_file = pdf,
    output_format = "pdf"
  )
  expect_equal(paper_report, pdf)
  expect_true(file.exists(pdf))
  # browseURL(pdf)
})

test_that("report pass args", {
  paper <- demoxml() |> read()
  modules <- c("stat_p_exact", "modules/no_error.R")
  tf <- tempfile(fileext = ".qmd")

  args <- list(
    "modules/no_error.R" = list(demo_arg = "Look for me in the text!",
                                irrelevant_arg = 1:10)
  )
  r <- report(paper, modules, output_file = tf, args = args)
  # browseURL(r)

  qmd_txt <- readLines(r)
  find_arg <- grepl(args$`modules/no_error.R`$demo_arg, qmd_txt, fixed = TRUE)
  expect_true(any(find_arg))

  # make sure exact p doesn't fail
  exact_runs <- grepl("(#exact-p-values){.red}", qmd_txt, fixed = TRUE)
  expect_true(any(exact_runs))

  unlink(r)
})

test_that("detected", {
  skip_on_ci()
  skip_if_not_installed("quarto")
  skip_on_cran()

  paper <- demoxml() |> read()
  # skip modules that require osf.api
  modules <- c(
    "stat_p_exact", "marginal", "stat_effect_size", "stat_check"
  )

  # add a retracted paper
  retracted <- data.frame(
    xref_id = "x",
    ref = "Test retracted paper",
    doi = retractionwatch()$doi[[1]],
    bibtype = "Article",
    title = "Fake",
    journal = "Fake Journal",
    year = 2025,
    authors = "Hmmm",
    id = paper$id
  )
  paper$bib <- rbind(paper$bib, retracted)

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
  paper$full_text[10, "text"] <- "Bad p-value example (p > 0.05)"
  paper$full_text[11, "text"] <- "Bad p-value example (p > 0.05)"

  # add marginal text
  paper$full_text[12, "text"] <- "This effect approached significance."

  # add OSF links
  paper$full_text[13, "text"] <- "https://osf.io/5tbm9/"
  paper$full_text[14, "text"] <- "https://osf.io/629bx/"

  # qmd
  qmd <- tempfile(fileext = ".qmd")
  if (file.exists(qmd)) unlink(qmd)
  paper_report <- report(paper, modules,
    output_file = qmd,
    output_format = "qmd"
  )
  expect_equal(paper_report, qmd)
  expect_true(file.exists(qmd))
  # rstudioapi::documentOpen(qmd)


  # html
  html <- tempfile(fileext = ".html")
  if (file.exists(html)) unlink(html)
  paper_report <- report(paper, modules,
    output_file = html,
    output_format = "html"
  )
  expect_equal(paper_report, html)
  expect_true(file.exists(html))
  # browseURL(html)

  # pdf
  skip("pdf")
  pdf <- tempfile(fileext = ".pdf")
  if (file.exists(pdf)) unlink(pdf)
  paper_report <- report(paper, modules,
    output_file = pdf,
    output_format = "pdf"
  )
  expect_equal(paper_report, pdf)
  expect_true(file.exists(pdf))
  # browseURL(pdf)
})

test_that("module_report", {
  expect_true(is.function(metacheck::module_report))

  expect_error(module_report())

  # set up module output
  module_output <- module_run(psychsci[[4]], "stat_p_exact")

  report <- module_report(module_output)
  expect_true(grepl("^### Exact P-Values \\{\\.red\\}", report))

  report <- module_report(module_output, header = 4, maxrows = 20, trunc_cell = 10)
  expect_true(grepl("^#### Exact P-Values \\{\\.red\\}", report))

  report <- module_report(module_output, header = "Custom header")
  expect_true(grepl("^Custom header", report))

  # print.metacheck_module_output
  op <- capture_output(print(module_output))
  expect_true(grepl("^Exact P-Values", op))
})

