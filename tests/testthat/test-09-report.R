test_that("error", {
  expect_true(is.function(report))

  expect_error( report(1), "The paper argument must be a paper object" )

  paper <- demoxml() |> read_grobid()
  expect_error( paper_report <- report(paper, modules = c("notamodule")),
                "Some modules are not available: notamodule",
                fixed = TRUE)
})

test_that("defaults", {
  paper <- demoxml() |> read_grobid()
  # skip modules that require osf.api
  modules <- c("exact_p", "marginal", "effect_size", "statcheck",
               "retractionwatch", "ref_consistency")

  # qmd
  qmd <- tempfile(fileext = ".qmd")
  if (file.exists(qmd)) unlink(qmd)
  paper_report <- report(paper, modules,
                         output_file = qmd,
                         output_format = "qmd")
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
                         output_format = "html")
  expect_equal(paper_report, html)
  expect_true(file.exists(html))
  # browseURL(html)

  # pdf
  skip("pdf")
  pdf <- tempfile(fileext = ".pdf")
  if (file.exists(pdf)) unlink(pdf)
  paper_report <- report(paper, modules,
                         output_file = pdf,
                         output_format = "pdf")
  expect_equal(paper_report, pdf)
  expect_true(file.exists(pdf))
  # browseURL(pdf)
})

test_that("detected", {
  skip_if_not_installed("quarto")
  skip_on_cran()

  paper <- demoxml() |> read_grobid()
  # skip modules that require osf.api
  modules <- c("exact_p", "marginal", "effect_size", "statcheck",
               "retractionwatch", "ref_consistency")

  # add a retracted paper
  retracted <- data.frame(
    bib_id = "x",
    ref = "Test retracted paper",
    doi = retractionwatch$doi[[1]],
    bibtype = "Article",
    title = "Fake",
    journal = "Fake Journal",
    year = 2025,
    authors = "Hmmm"
  )
  paper$references <- rbind(paper$references, retracted)

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
                         output_format = "qmd")
  expect_equal(paper_report, qmd)
  expect_true(file.exists(qmd))
  # rstudioapi::documentOpen(qmd)


  # html
  html <- tempfile(fileext = ".html")
  if (file.exists(html)) unlink(html)
  paper_report <- report(paper, modules,
                         output_file = html,
                         output_format = "html")
  expect_equal(paper_report, html)
  expect_true(file.exists(html))
  # browseURL(html)

  # pdf
  skip("pdf")
  pdf <- tempfile(fileext = ".pdf")
  if (file.exists(pdf)) unlink(pdf)
  paper_report <- report(paper, modules,
                         output_file = pdf,
                         output_format = "pdf")
  expect_equal(paper_report, pdf)
  expect_true(file.exists(pdf))
  # browseURL(pdf)
})

test_that("module_report", {
  expect_true(is.function(papercheck::module_report))

  expect_error(module_report())

  # set up module output
  module_output <- module_run(psychsci[1:4], "all_p_values")

  report <- module_report(module_output)
  expect_true(grepl("Showing 4 of 4 rows", report))
  expect_true(grepl("^## List All P-Values \\{\\.info\\}", report))

  report <- module_report(module_output, header = 3, maxrows = 20, trunc_cell = 10)
  expect_true(grepl("Showing 4 of 4 rows", report))
  expect_true(grepl("^### List All P-Values \\{\\.info\\}", report))
  expect_true(grepl("0956797...", report, fixed = TRUE))

  report <- module_report(module_output, header = "Custom header")
  expect_true(grepl("^Custom header", report))

  op <- capture_output(print(module_output))
  expect_true(grepl("^|id               | p_values|", op))
  expect_true(grepl("\n\nShowing 4 of 4 rows$", op))
  expect_true(grepl("|0956797614557697 |       27|", op))
})
