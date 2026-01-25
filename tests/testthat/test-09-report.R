test_that("errors", {
  expect_true(is.function(metacheck::report))
  expect_no_error(helplist <- help(report, metacheck))

  paper <- read(demoxml())
  modules <- "all_p_values"
  output_file <- withr::local_tempfile(fileext = ".qmd")
  output_format <- "qmd"

  # paper not a paper or paperlist
  bad_paper <- 1
  expect_error(report(bad_paper, modules, output_file, output_format),
               "The paper argument must be a paper object")

  # non-existent module
  bad_modules <- "notamodule"
  expect_error(report(paper, bad_modules, output_file, output_format),
               "notamodule")

  # bad output_file path
  bad_output_file <- "not/a/path/file.html"
  expect_error(report(paper, modules, bad_output_file, output_format),
               "output_file")

  # bad format
  bad_output_format <- "pdf"
  expect_error(report(paper, modules, output_file, bad_output_format),
               "output_format")

  # format case
  ok_output_format <- "QMD"
  expect_no_error(rep <- report(paper, modules, output_file, ok_output_format))
  save_path <- attr(rep, "save_path")
  expect_equal(save_path, output_file)
})

test_that("rendering error", {
  # should give a warning and return the path to the saved qmd
  paper <- read(demoxml())
  modules <- c("bad-report")
  output_format <- "html"

  # qmd fails to render
  output_file <- withr::local_tempfile(fileext = paste0(".", output_format))
  expect_warning(rep<- report(paper, modules, output_file, output_format),
                 "There was an error rendering your report")

  exp <- sub("html$", "qmd", output_file)
  save_path <- attr(rep, "save_path")
  expect_equal(save_path, exp)
  # browseURL(report_file)
})

test_that("report return list", {
  paper <- demoxml() |> read()
  modules <- c("stat_p_exact", "marginal")
  output_file <- withr::local_tempfile(fileext = ".qmd")
  output_format <- "qmd"
  paper_report <- report(paper, modules, output_file, output_format)
  expect_equal(names(paper_report), modules)
  expect_s3_class(paper_report[[1]], "metacheck_module_output")
  expect_equal(paper_report$stat_p_exact$module, "stat_p_exact")
})

test_that("report paperlist", {
  paper <- psychsci[1:2]
  modules <- c("stat_p_exact", "marginal")
  output_file <- withr::local_tempfile(pattern = "_", fileext = ".qmd")
  output_format <- "qmd"
  paper_report <- report(paper, modules, output_file, output_format)
  expect_equal(length(paper_report), 2)
  expect_equal(names(paper_report), names(paper))
  expect_equal(names(paper_report[[1]]), modules)
  expect_equal(names(paper_report[[2]]), modules)
  sp1 <- attr(paper_report[[1]], "save_path")
  expect_true(file.exists(sp1))
  sp2 <- attr(paper_report[[2]], "save_path")
  expect_true(file.exists(sp2))
  sp <- attr(paper_report, "save_path")
  expect_equal(names(sp), names(paper))
  expect_equal(sp[[1]], sp1)
  expect_equal(sp[[2]], sp2)

  output_file1 <- withr::local_tempfile(fileext = ".qmd")
  output_file2 <- withr::local_tempfile(fileext = ".qmd")
  pr1 <- report(paper[1], modules, output_file1, output_format)
  pr2 <- report(paper[[2]], modules, output_file2, output_format)
  expect_true(file.exists(output_file1))
  expect_true(file.exists(output_file2))

  expect_equal(paper_report[[1]], pr1, ignore_attr = TRUE)
  expect_equal(paper_report[[2]], pr2, ignore_attr = TRUE)

  # vector of output_files
  output_file <- c(withr::local_tempfile(fileext = ".qmd"),
                   withr::local_tempfile(fileext = ".qmd"))
  paper_report <- report(paper, modules, output_file, output_format)
  expect_true(file.exists(output_file[[1]]))
  expect_true(file.exists(output_file[[2]]))
})

test_that("render qmd", {
  paper <- demoxml() |> read()
  modules <- c("stat_p_exact", "marginal")

  # qmd
  paper <- psychsci[[94]]
  output_file <- withr::local_tempfile(fileext = ".qmd")
  output_format <- "qmd"
  paper_report <- report(paper, modules, output_file, output_format)
  save_path <- attr(paper_report, "save_path")
  expect_equal(save_path, output_file)
  expect_true(file.exists(output_file))
  # browseURL(output_file)
})

test_that("render html", {
  skip_if_not_installed("quarto")
  skip_on_cran()

  paper <- demoxml() |> read()
  modules <- c("stat_p_exact", "marginal")
  output_file <- withr::local_tempfile(fileext = ".html")
  output_format <- "html"

  paper_report <- report(paper, modules, output_file, output_format)
  save_path <- attr(paper_report, "save_path")
  expect_true(file.exists(save_path))
  # browseURL(html)
})

test_that("report pass args", {
  # pass arguments to modules from args
  paper <- demoxml() |> read()
  modules <- c("stat_p_exact", "modules/no_error.R")
  output_file <- withr::local_tempfile(fileext = ".qmd")
  output_format <- "qmd"

  args <- list(
    "modules/no_error.R" = list(demo_arg = "Look for me in the text!",
                                irrelevant_arg = 1:10)
  )
  r <- report(paper, modules, output_file, output_format, args = args)
  save_path <- attr(r, "save_path")
  # browseURL(r)

  qmd_txt <- readLines(save_path)
  find_arg <- grepl(args$`modules/no_error.R`$demo_arg, qmd_txt, fixed = TRUE)
  expect_true(any(find_arg))

  # make sure exact p doesn't fail
  exact_runs <- grepl("(#exact-p-values){.red}", qmd_txt, fixed = TRUE)
  expect_true(any(exact_runs))
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
  qmd <- withr::local_tempfile(fileext = ".qmd")
  if (file.exists(qmd)) unlink(qmd)
  paper_report <- report(paper, modules,
                         output_file = qmd,
                         output_format = "qmd"
  )
  save_path <- attr(paper_report, "save_path")
  expect_equal(save_path, qmd)
  expect_true(file.exists(qmd))
  # rstudioapi::documentOpen(qmd)


  # html
  html <- withr::local_tempfile(fileext = ".html")
  if (file.exists(html)) unlink(html)
  paper_report <- report(paper, modules,
                         output_file = html,
                         output_format = "html"
  )
  #expect_equal(paper_report, html)
  expect_true(file.exists(html))
  # browseURL(html)
})



test_that("module_report", {
  expect_true(is.function(metacheck::module_report))

  expect_error(module_report())

  # set up module output
  module_output <- module_run(psychsci[[4]], "stat_p_exact")

  report <- module_report(module_output)
  expect_true(grepl("^### \\S* Exact P-Values", report))

  report <- module_report(module_output, header = 4)
  expect_true(grepl("^#### \\S* Exact P-Values", report))

  report <- module_report(module_output, header = "Custom header")
  expect_true(grepl("^Custom header", report))

  # print.metacheck_module_output
  op <- capture_output(print(module_output))
  expect_true(grepl("^Exact P-Values", op))
})


test_that("module_report howitworks", {
  paper <- read(demoxml())

  module <- "no_error"
  module_output <- module_run(paper, module)
  rep <- module_report(module_output)

  expect_true(grepl("Lisa DeBruine and Daniel Lakens", rep))
  expect_true(grepl("^### .* Demo No Error", rep))
  expect_true(grepl("Demo description", rep))
  expect_true(grepl("Demo details...", rep))

  module <- "bad-report"
  module_output <- module_run(paper, module)
  rep <- module_report(module_output)

  expect_true(grepl("^### \\S* Bad Report \\{#bad-report \\.info\\}", rep))
  expect_false(grepl("This module was developed by", rep))
})


test_that("report_module_run", {
  expect_true(is.function(metacheck::report_module_run))
  expect_no_error(helplist <- help(report_module_run, metacheck))

  paper <- read(demoxml())
  modules <- "all_p_values"
  mo <- report_module_run(paper, modules)
  expect_equal(names(mo), modules)

  modules <- c("stat_p_nonsig", "stat_p_exact")
  mo <- report_module_run(paper, modules)
  expect_setequal(names(mo), modules)
})

test_that("report_qmd", {
  expect_true(is.function(metacheck::report_qmd))
  expect_no_error(helplist <- help(report_qmd, metacheck))

  paper <- read(demoxml())
  modules <- "stat_p_nonsig"
  mo <- report_module_run(paper, modules)
  report_text <- report_qmd(mo, paper)
  expect_true(grepl("MetaCheck Report", report_text))
  expect_true(grepl(paper$info$title, report_text))
  expect_true(grepl("Non-Significant P Value Check", report_text, fixed = TRUE))
})
