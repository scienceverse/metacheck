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
    "exact_p", "marginal", "effect_size", "statcheck",
    "retractionwatch", "ref_consistency"
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

test_that("detected", {
  skip_on_ci()
  skip_if_not_installed("quarto")
  skip_on_cran()

  paper <- demoxml() |> read()
  # skip modules that require osf.api
  modules <- c(
    "exact_p", "marginal", "effect_size", "statcheck",
    "retractionwatch", "ref_consistency"
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
  module_output <- module_run(psychsci[[4]], "exact_p")

  report <- module_report(module_output)
  expect_true(grepl("^## Exact P-Values \\{\\.red\\}", report))

  report <- module_report(module_output, header = 3, maxrows = 20, trunc_cell = 10)
  expect_true(grepl("^### Exact P-Values \\{\\.red\\}", report))

  report <- module_report(module_output, header = "Custom header")
  expect_true(grepl("^Custom header", report))

  # print.metacheck_module_output
  op <- capture_output(print(module_output))
  expect_true(grepl("^Exact P-Values", op))
})

test_that("scroll_table", {
  expect_true(is.function(metacheck::scroll_table))

  table <- data.frame(uc = LETTERS,
                      lc = letters)
  obs <- scroll_table(table)
  expect_true(grepl("```{r}", obs, fixed = TRUE))
  expect_true(grepl("escape = FALSE", obs, fixed = TRUE))

  obs <- scroll_table(table, escape = TRUE)
  expect_true(grepl("escape = TRUE", obs, fixed = TRUE))

  # vector vs unnamed table version
  table <- data.frame(table = LETTERS)
  colnames(table) <- ""
  obs_table <- scroll_table(table)
  obs_vec <- scroll_table(LETTERS)
  expect_equal(obs_table, obs_vec)

  # set scroll after scroll_above
  obs_scroll <- scroll_table(1:10)
  obs_no <- scroll_table(1:2)
  obs_no10 <- scroll_table(1:10, scroll_above = 10)

  expect_true(grepl("scrollY", obs_scroll))
  expect_false(grepl("scrollY", obs_no))
  expect_false(grepl("scrollY", obs_no10))

  # colwidths
  obs <- scroll_table(data.frame(a = 1, b = 2), c(.3, .7))
  expect_true(grepl('targets = 0, width = "30%"', obs))
  expect_true(grepl('targets = 1, width = "70%"', obs))

  obs <- scroll_table(data.frame(a = 1, b = 2, c = 3, d = 4), c(.1, .4))
  expect_true(grepl('targets = 0, width = "10%"', obs))
  expect_true(grepl('targets = 1, width = "40%"', obs))

  obs <- scroll_table(data.frame(a = 1, b = 2, c = 3, d = 4), c(NA, 200, NA, NA))
  expect_false(grepl('targets = 0', obs))
  expect_true(grepl('targets = 1, width = "200px"', obs))
  expect_false(grepl('targets = 2', obs))
  expect_false(grepl('targets = 3', obs))
})

test_that("collapse_section", {
  expect_true(is.function(metacheck::collapse_section))

  expect_error(collapse_section())
  expect_error(collapse_section("a", callout = "d"))

  text <- "hello"
  obs <- collapse_section(text)
  expect_true(grepl("callout-tip", obs))

  obs <- collapse_section(text, callout = "warning")
  expect_true(grepl("callout-warning", obs))
})

test_that("plural", {
  expect_true(is.function(metacheck::plural))

  s0 <- plural(0)
  expect_equal(s0, "s")
  s1 <- plural(1)
  expect_equal(s1, "")
  s2 <- plural(2)
  expect_equal(s2, "s")

  s0 <- plural(0, "is", "are")
  expect_equal(s0, "are")
  s1 <- plural(1, "is", "are")
  expect_equal(s1, "is")
  s2 <- plural(2, "is", "are")
  expect_equal(s2, "are")
})

test_that("link", {
  expect_true(is.function(metacheck::link))

  obs <- link("https://google.com")
  exp <- "<a href='https://google.com' target='_blank'>google.com</a>"
  expect_equal(obs, exp)

  obs <- link("http://google.com")
  exp <- "<a href='http://google.com' target='_blank'>google.com</a>"
  expect_equal(obs, exp)

  obs <- link("https://google.com", "Google")
  exp <- "<a href='https://google.com' target='_blank'>Google</a>"
  expect_equal(obs, exp)

  obs <- link("https://google.com", "Google", FALSE)
  exp <- "<a href='https://google.com'>Google</a>"
  expect_equal(obs, exp)

  url <- c("https://google.com", "https://scienceverse.org")
  text <- c("Google", "Scienceverse")
  obs <- link(url, text, FALSE)
  exp <- c("<a href='https://google.com'>Google</a>",
           "<a href='https://scienceverse.org'>Scienceverse</a>")
  expect_equal(obs, exp)

  url <- c(NA, "https://scienceverse.org")
  text <- c("Google", "Scienceverse")
  obs <- link(url, text, FALSE)
  exp <- c(NA,
           "<a href='https://scienceverse.org'>Scienceverse</a>")
  expect_equal(obs, exp)
})
