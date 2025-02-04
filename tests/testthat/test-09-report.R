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

  # qmd
  qmd <- tempfile(fileext = ".qmd")
  if (file.exists(qmd)) unlink(qmd)
  paper_report <- report(paper,
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
  paper_report <- report(paper,
                         output_file = html,
                         output_format = "html")
  expect_equal(paper_report, html)
  expect_true(file.exists(html))
  # browseURL(html)

  # pdf
  skip("pdf")
  pdf <- tempfile(fileext = ".pdf")
  if (file.exists(pdf)) unlink(pdf)
  paper_report <- report(paper,
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

  # add a retracted paper
  retracted <- data.frame(
    bib_id = "x",
    doi = retractionwatch$doi[[1]],
    ref = "Test retracted paper"
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
  paper_report <- report(paper,
                         output_file = qmd,
                         output_format = "qmd")
  expect_equal(paper_report, qmd)
  expect_true(file.exists(qmd))
  # rstudioapi::documentOpen(qmd)


  # html
  html <- tempfile(fileext = ".html")
  if (file.exists(html)) unlink(html)
  paper_report <- report(paper,
                         output_file = html,
                         output_format = "html")
  expect_equal(paper_report, html)
  expect_true(file.exists(html))
  # browseURL(html)

  # pdf
  skip("pdf")
  pdf <- tempfile(fileext = ".pdf")
  if (file.exists(pdf)) unlink(pdf)
  paper_report <- report(paper,
                         output_file = pdf,
                         output_format = "pdf")
  expect_equal(paper_report, pdf)
  expect_true(file.exists(pdf))
  # browseURL(pdf)
})
