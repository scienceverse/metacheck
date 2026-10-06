test_that("errors", {
  expect_true(is.function(metacheck::report))
  expect_no_error(helplist <- help(report, metacheck))

  paper <- demopaper()
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
  paper <- demopaper()
  modules <- test_path("modules", "bad-report.R")
  output_format <- "html"

  # qmd fails to render
  output_file <- withr::local_tempfile(fileext = paste0(".", output_format))
  expect_warning(rep <- report(paper, modules, output_file, output_format),
                 "There was an error rendering your report")

  exp <- sub("html$", "qmd", output_file)
  save_path <- attr(rep, "save_path")
  expect_equal(save_path, exp)
  # browseURL(report_file)
})

test_that("report return list", {
  paper <- demopaper()
  modules <- c("stat_p_exact", "marginal")
  output_file <- withr::local_tempfile(fileext = ".qmd")
  output_format <- "qmd"
  paper_report <- report(paper, modules, output_file, output_format)
  expect_equal(names(paper_report), modules)
  expect_s3_class(paper_report[[1]], "metacheck_module_output")
  expect_equal(paper_report$stat_p_exact$module, "stat_p_exact")
})

test_that("report paperlist", {
  skip_no_psychsci()
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

test_that("report_repository errors on a bad path", {
  expect_true(is.function(metacheck::report_repository))
  expect_no_error(helplist <- help(report_repository, metacheck))

  expect_error(report_repository(1), "single path")
  expect_error(report_repository(c("a", "b")), "single path")
  expect_error(report_repository(tempfile()), "No folder found")
})

test_that("report_repository defaults output_file to the folder's own name", {
  # A relative output_file (report_repository()'s own default) is written
  # relative to WHATEVER the working directory is when it runs -- tested here
  # by changing into the fixture directory via a plain function call (not
  # withr::local_dir()/on.exit() placed directly in this test_that() block:
  # both were confirmed, while writing this feature, not to reliably restore
  # at the end of a test_that() block under this project's actual test
  # runner (devtools::test()) -- see test-report-helpers.R's "report_type"
  # test for the full story. An ordinary function's OWN on.exit() does
  # restore reliably even when the function is called from inside
  # test_that(), which is why this is wrapped in one here.
  in_dir <- function(dir, code) {
    old <- setwd(dir)
    on.exit(setwd(old), add = TRUE)
    force(code)
  }

  cwd_before <- getwd()
  d <- withr::local_tempdir()
  dir.create(file.path(d, "my_repo"))
  writeLines("id,x\n1,2", file.path(d, "my_repo", "data.csv"))

  rep <- in_dir(d, report_repository("my_repo", output_format = "qmd",
                                     modules = "repo_check"))
  save_path <- attr(rep, "save_path")
  expect_equal(save_path, "my_repo_report.qmd")
  expect_true(file.exists(file.path(d, save_path)))
  # The working directory must be back to normal: a leftover change here
  # would silently break every later test using a relative path (e.g.
  # module_find()'s own "modules/..." lookups) -- see this test's own
  # comment above for the real incident that taught us to check this.
  expect_equal(getwd(), cwd_before)
})

test_that("report_repository's args are only set on the first module", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "repo1"))
  writeLines("a script", file.path(d, "repo1", "analysis.R"))
  output_file <- withr::local_tempfile(fileext = ".qmd")

  # explicit args for the first module (repo_check) must survive being
  # merged with the local_path/local_only report_repository() injects, and
  # the second module (code_check) must receive NEITHER of those two --
  # it is meant to reuse repo_check's chained output, not re-read the folder
  # itself (see report_repository()'s own comment on why).
  args <- list(repo_check = list(cache = TRUE))
  rep <- report_repository(file.path(d, "repo1"), output_file = output_file,
                           output_format = "qmd",
                           modules = c("repo_check", "code_check"),
                           args = args)

  expect_true(file.exists(output_file))
  qmd_txt <- paste(readLines(output_file), collapse = "\n")
  # analysis.R was found via repo_check's own folder read and reached
  # code_check's report without code_check being told local_path itself.
  expect_match(qmd_txt, "analysis.R", fixed = TRUE)
})

test_that("report_repository passes report_type through to the generated report", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "repo2"))
  writeLines("id,x\n1,2", file.path(d, "repo2", "data.csv"))
  output_file <- withr::local_tempfile(fileext = ".qmd")

  report_repository(file.path(d, "repo2"), output_file = output_file,
                    output_format = "qmd", report_type = "simple",
                    modules = "repo_check")

  qmd_txt <- paste(readLines(output_file), collapse = "\n")
  # report_qmd()'s own setup chunk records which mode generated the text --
  # see its own comment for why the subprocess needs this spelled out.
  expect_match(qmd_txt, 'report_type\\("simple"\\)')
})

test_that("render qmd", {
  paper <- demopaper()
  modules <- c("stat_p_exact", "marginal")

  # qmd
  skip_no_psychsci()
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
  skip_if_quick()
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("quarto")

  paper <- demopaper()
  modules <- c("stat_p_exact", "marginal")
  output_file <- withr::local_tempfile(fileext = ".html")
  output_format <- "html"

  paper_report <- report(paper, modules, output_file, output_format)
  save_path <- attr(paper_report, "save_path")
  expect_true(file.exists(save_path))
  # browseURL(save_path)
})

test_that("render html report_type = 'simple' produces a static, email-safe file", {
  skip_if_quick()
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("quarto")
  skip_if_not_installed("rmarkdown")

  llm_use(FALSE)
  paper <- test_paper("Example text with p = .03 and p = .5.")
  modules <- c("stat_p_exact", "stat_p_nonsig")
  output_file <- withr::local_tempfile(fileext = ".html")

  prev_report_type <- report_type()
  paper_report <- report(paper, modules, output_file, "html",
                         report_type = "simple")
  # report_type() must never leak out of report() into the caller's session,
  # whichever branch of the render (success or the error/warning fallback)
  # actually ran -- see report()'s own on.exit() comment for the bug this
  # guards: a bare on.exit() call elsewhere in the same function used to
  # silently wipe this restoration out.
  expect_equal(report_type(), prev_report_type)

  save_path <- attr(paper_report, "save_path")
  expect_true(file.exists(save_path))
  expect_match(save_path, "\\.html$")

  txt <- paste(readLines(save_path, warn = FALSE), collapse = "\n")
  # No DT::datatable() widget, no Quarto tabsets/theme-toggle bundle: this is
  # the whole point of report_type = "simple" (see report_qmd()'s own
  # comment for why Quarto's HTML format cannot be used for this at all).
  expect_false(grepl("html-widget", txt, fixed = TRUE))
  expect_false(grepl("htmlwidget-", txt, fixed = TRUE))
  expect_false(grepl("panel-tabset", txt, fixed = TRUE))
  expect_false(grepl("quarto-color-scheme-toggle", txt, fixed = TRUE))
  # Real content still made it through the render.
  expect_match(txt, "Exact", fixed = TRUE)
  expect_match(txt, "dt-static", fixed = TRUE)
})

test_that("report pass args", {
  # pass arguments to modules from args
  paper <- demopaper()
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
  skip_if_quick()
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("quarto")

  paper <- demopaper()
  # skip modules that require osf.api
  modules <- c(
    "stat_p_exact", "marginal", "stat_effect_size", "stat_check"
  )

  # add imprecise p-values
  paper$text[1, "text"] <- "Bad p-value example (p < .05)"
  paper$text[2, "text"] <- "Bad p-value example (p<.05)"
  paper$text[3, "text"] <- "Bad p-value example (p < 0.05)"
  paper$text[4, "text"] <- "Bad p-value example; p < .05"
  paper$text[5, "text"] <- "Bad p-value example (p < .005)"
  paper$text[6, "text"] <- "Bad p-value example (p > 0.05)"
  paper$text[7, "text"] <- "Bad p-value example (p > .1)"
  paper$text[8, "text"] <- "Bad p-value example (p = n.s.)"
  paper$text[9, "text"] <- "Bad p-value example; p=ns"
  paper$text[10, "text"] <- "Bad p-value example (p > 0.05)"
  paper$text[11, "text"] <- "Bad p-value example (p > 0.05)"

  # add marginal text
  paper$text[12, "text"] <- "This effect approached significance."

  # add OSF links
  paper$text[13, "text"] <- "https://osf.io/5tbm9/"
  paper$text[14, "text"] <- "https://osf.io/629bx/"

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
  skip_no_psychsci()
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
  paper <- demopaper()

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


test_that("module_report validation", {
  paper <- demopaper()
  # A tagged module's <validation> block is emitted as a Quarto fenced div,
  # which renders to <div class="validation">. It used to be written as raw
  # <p class='validation'> HTML, which leaked literal "}}" / "\if{html}{\out{"
  # into the report text (see the comment in R/report.R).
  v <- "::: {.validation}"

  module <- test_path("modules", "no_error.R")
  module_output <- module_run(paper, module)
  rep <- module_report(module_output)
  expect_true(grepl(v, rep, fixed = TRUE))
  expect_true(grepl("Here is my demo validation", rep, fixed = TRUE))

  # no validation
  module <- "all_urls"
  module_output <- module_run(paper, module)
  rep <- module_report(module_output)
  expect_false(grepl(v, rep, fixed = TRUE))
})


test_that("report_module_run", {
  expect_true(is.function(metacheck::report_module_run))
  expect_no_error(helplist <- help(report_module_run, metacheck))

  paper <- demopaper()
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

  paper <- demopaper()
  modules <- "stat_p_nonsig"
  mo <- report_module_run(paper, modules)
  report_text <- report_qmd(mo, paper)
  expect_true(grepl("MetaCheck Report", report_text))
  expect_true(grepl(paper$info$title, report_text))

  mi <- module_info(modules)
  expect_true(grepl(mi$title, report_text, fixed = TRUE))
})
