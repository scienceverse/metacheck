test_that("module_list", {
  expect_true(is.function(metacheck::module_list))
  expect_no_error(helplist <- help(module_list, metacheck))

  builtin <- module_list(external = FALSE)
  expect_true(is.data.frame(builtin))
  exp <- c("name", "title", "description", "section", "path", "source")
  expect_equal(names(builtin), exp)

  # print format
  op <- capture_output(print(builtin))
  expect_true(grepl("*** GENERAL ***", op, fixed = TRUE))

  # check all names and titles are unique
  expect_equal(builtin$name, unique(builtin$name))
  expect_equal(builtin$title, unique(builtin$title))
})

test_that("module_find", {
  expect_true(is.function(metacheck:::module_find))
  expect_no_error(helplist <- help(module_find, metacheck))

  expect_error(module_find())
  expect_error(module_find("notamodule"),
               "There were no modules that matched notamodule",
               fixed = TRUE )
  log <- lastlog()
  expect_equal(log$module, "notamodule")
  expect_equal(log$label, "module_find")

  # find built-in modules
  builtin <- module_list()
  path <- module_find(builtin$name[[1]])
  expect_true(file.exists(path))

  # find modules in wd or modules directory
  path <- module_find("no_error")
  expect_equal(path, "modules/no_error.R")

  path <- module_find("setup")
  expect_equal(path, "./setup.R")
})

test_that("module_info", {
  expect_true(is.function(metacheck::module_info))
  expect_no_error(helplist <- help(module_info, metacheck))

  expect_error(module_info("bad_arg"),
               "There were no modules that matched bad_arg")

  info <- module_info("marginal")
  expect_equal(info$title, "Marginal Significance")
  expect_equal(info$keywords, "results")
  expect_equal(info$author, "Daniel Lakens (\\email{D.Lakens@tue.nl})")
  expect_equal(info$description, "List all sentences that describe an effect as 'marginally significant'.")
  expect_equal(info$func_name, "marginal")

  mod_path <- test_path("modules", "no_error.R")
  info <- module_info(mod_path)
  expect_equal(info$title, "Demo No Error")
  expect_equal(info$description, "Demo description")
  expect_true(grepl("Demo details...", info$details, fixed = TRUE))
  expect_equal(info$author, list("Lisa DeBruine (\\email{debruine@gmail.com})", "Daniel Lakens"))
  expect_equal(info$func_name, "pvals2")
  expect_equal(info$param[[1]], list(name = "paper",
                                     description = "a paper object or paperlist object"))
  expect_equal(info$import, "dplyr")
})

test_that("module_help", {
  expect_true(is.function(metacheck::module_help))
  expect_no_error(helplist <- help(module_help, metacheck))

  expect_error(module_help("bad_arg"),
               "There were no modules that matched bad_arg")

  ml <- capture.output(module_list())
  mh <- capture.output(module_help())
  expect_equal(mh, ml)

  # marginal
  help <- module_help("marginal")

  title <- "Marginal Significance"
  usage <- "module_run(paper, \"marginal\")"
  desc <- "List all sentences that describe an effect as 'marginally significant'."
  def <- "- paper: a paper object or paperlist object  "

  output <- capture.output(help)
  expect_equal(output[[1]], title)
  expect_equal(output[[3]], desc)
  expect_equal(output[[5]], usage)
  expect_equal(output[[7]], def)

  expect_equal(class(help), "metacheck_module_help")
  expect_equal(help$title, title)
  expect_equal(help$description, desc)

  # TODO: test with argument
})

test_that("module_template", {
  expect_true(is.function(metacheck::module_template))
  expect_no_error(helplist <- help(module_template, metacheck))

  expect_error(module_template("a module", tempdir()),
               "The module_name must contain only letters, numbers, and _",
               fixed = TRUE)

  # there is no default folder (CRAN: no writing into the working directory)
  expect_error(module_template("demo"), "path")

  mod_dir <- withr::local_tempdir()
  module_template("demo", mod_dir)
  expect_true(file.exists(file.path(mod_dir, "demo.R")))

  # the module the template writes runs (#532): it used columns id and
  # section, which text_search() does not return
  expect_no_error(mo <- module_run(demopaper(), file.path(mod_dir, "demo.R")))
  expect_true("paper_id" %in% names(mo$summary_table))
  expect_equal(mo$traffic_light, "info")
})

test_that("module_run", {
  expect_true(is.function(metacheck::module_run))
  expect_no_error(helplist <- help(module_run, metacheck))

  paper <- demopaper()

  # errors
  expect_error( module_run() )
  expect_error( module_run(paper) )
  expect_error( module_run(paper, "notamodule"),
                "There were no modules that matched notamodule")

  module <- test_path("modules", "module-error.R")
  expect_error(module_run(paper, module),
               "The module .* code has errors")
  log <- lastlog()
  expect_equal(log$label, "module-error")
  expect_match(log$error, "unexpected")

  module <- test_path("modules", "code-error.R")
  expect_error(module_run(paper, module),
               "Running the module .* produced errors")
  log <- lastlog()
  expect_equal(log$label, "code-error")
  expect_equal("argument 1 is empty", log$error)

  module <- test_path("modules", "missing-pkg.R")
  expect_error(module_run(paper, module),
               "notarealpkg")

  module <- test_path("modules", "missing-importFrom.R")
  expect_error(module_run(paper, module),
               "dplyr::notarealfunction")

  # demo
  module <- test_path("modules", "no_error.R")
  mod_output <- module_run(paper, module)
  expected_summary <- data.frame(paper_id = paper$paper_id, p_values = 3)

  expect_equal(mod_output$module, module)
  expect_equal(mod_output$title, "Demo No Error")
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(mod_output$section, "results")
  expect_equal(mod_output$report, "This is the report text.")
  expect_equal(mod_output$summary_text, "summary text")
  expect_equal(mod_output$paper, paper)
  expect_equal(mod_output$summary_table, expected_summary)

  first_char <- substr(mod_output$table$text, 1, 1)
  expect_true(all(first_char == "p"))

  # with argument
  mod_output <- module_run(paper, module,
                           demo_arg = "demo")
  expect_equal(mod_output$summary_text, "summary textdemo")
})

test_that("chaining modules - one paper", {
  paper <- demopaper()

  # chained run
  a <- module_run(paper, "all_p_values")
  b <- module_run(a, "chained")
  expect_equal(b$table, a$table[1:2, 1:2])

  # run without chaining
  c <- module_run(paper, "chained")
  expect_equal(c$table, data.frame(a = "not from prev"))
})

test_that("chaining modules - paperlist", {
  skip_no_psychsci()
  paper <- psychsci[1:5]

  p <- module_run(paper, "all_p_values")
  url <- module_run(paper, "all_urls")
  noerrors <- test_path("modules", "no_error.R")

  x <- paper |>
    module_run("all_p_values") |>
    module_run("all_urls") |>
    module_run(noerrors)

  expect_equal(names(x$summary_table), c("paper_id", "p_values", "urls", "p_values.no_error"))
  expect_equal(x$summary_table$p_values, p$summary_table$p_values)
  expect_equal(x$summary_table$urls, url$summary_table$urls)
})



test_that("get_prev_outputs", {
  expect_true(is.function(metacheck::get_prev_outputs))
  expect_no_error(helplist <- help(get_prev_outputs, metacheck))

  .__mc__prev_outputs <- list(mod_1 = list(a = 1, b = 2))
  f <- function(module, item) { get_prev_outputs(module, item, 1) }
  expect_equal(f("mod_1", "a"), 1)
  expect_null(f("mod_2", "a"))
})

test_that("all builtin modules have essential components", {
  modules <- module_list(external = FALSE)$name
  section_levels <- c("general", "intro", "method", "results", "discussion", "reference")

  # expect some content in roxygen header
  for (module in modules) {
    info <- module_info(module)
    expect_true(nzchar(info$title))
    expect_true(nzchar(info$description))
    expect_true(nzchar(info$details))

    expect_in(info$keywords[[1]], section_levels)
  }
})

test_that("all_urls", {
  paper <- demopaper()
  module <- "all_urls"
  urls <- module_run(paper, module)
  expect_equal(urls$traffic_light, "info")
  expect_contains(urls$table$text, "https://osf.io/48ncu")
  expect_equal(urls$module, module)

  # iteration
  skip_no_psychsci()
  paper <- psychsci[1:5]
  mod_output <- module_run(paper, module)
  ids <- mod_output$table$paper_id |> unique()
  expect_contains(names(paper), ids)
})

test_that("every module runs on an empty paper or paper list and reports na (U79)", {
  # an empty paper list stopped nearly every module (paper_table() returned
  # no columns, text_search() logical ones), and an empty paper stopped the
  # repository modules, ethics_check, power and prereg_check
  skip_if_quick()
  llm_use(FALSE)
  for (m in module_list()$name) {
    for (input in list(paper(), paperlist(), list())) {
      light <- tryCatch(
        suppressWarnings(suppressMessages(module_run(input, m)))$traffic_light,
        error = function(e) paste("error:", conditionMessage(e)))
      # nothing to check is "na", not red ("no statement found") or green
      expect_equal(light, "na", label = m)
    }
  }
})

test_that("paper = NULL checks only a local folder (U78)", {
  # documented as allowed, but paper_id(NULL) stopped repo_check and every
  # module that runs it; the unknown id also became "0" through na_replace
  skip_if_quick()
  llm_use(FALSE)
  d <- withr::local_tempdir()
  write.csv(data.frame(id = 1:3, score = c(4, 5, 6)), file.path(d, "data.csv"),
            row.names = FALSE)
  writeLines(c("# analysis", "d <- read.csv(\"data.csv\")"), file.path(d, "analysis.R"))
  for (m in c("repo_check", "code_check", "data_check")) {
    mo <- suppressWarnings(suppressMessages(module_run(NULL, m, local_path = d)))
    expect_equal(nrow(mo$summary_table), 1, label = m)
    expect_true(is.na(mo$summary_table$paper_id), label = m)
  }
  mo <- suppressWarnings(suppressMessages(module_run(NULL, "repo_check", local_path = d)))
  expect_setequal(mo$table$file_name, c("analysis.R", "data.csv"))
})

test_that("a module's na_replace fills only its own summary columns in a chain (U77)", {
  # stat_check's na_replace = 0 turned open_practices' list cells into 0
  p1 <- test_paper("All data are at https://osf.io/abcde."); p1$paper_id <- "a"
  p2 <- test_paper("Nothing here, t(20) = 2.1, p = .04."); p2$paper_id <- "b"
  mo <- module_run(paperlist(p1, p2), "open_practices") |> module_run("stat_check")
  expect_true(all(vapply(mo$summary_table$code_statements, is.character, logical(1))))
})

test_that("marginal cites its reference with readable authors (U3)", {
  # the authors as one string were re-parsed into "Hartgerink &amp;, J. CH"
  r <- paste(unlist(module_run(demopaper(), "marginal")$report), collapse = "\n")
  expect_match(r, "Olsson-Collentine A, van Assen MALM, Hartgerink CHJ (2019)", fixed = TRUE)
})
