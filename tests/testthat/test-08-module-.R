test_that("module_list", {
  expect_true(is.function(metacheck::module_list))
  expect_no_error(helplist <- help(module_list, metacheck))

  builtin <- module_list()
  expect_true(is.data.frame(builtin))
  exp <- c("name", "title", "description", "section", "path")
  expect_equal(names(builtin), exp)

  op <- capture_output(print(builtin))
  expect_true(grepl("*** GENERAL ***", op, fixed = TRUE))
})

test_that("module_find", {
  expect_true(is.function(metacheck:::module_find))
  expect_no_error(helplist <- help(module_find, metacheck))

  expect_error(module_find())
  expect_error(module_find("notamodule"),
               "There were no modules that matched notamodule",
               fixed = TRUE )


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
  expect_equal(info$author, "Daniel Lakens")
  expect_equal(info$description, "List all sentences that describe an effect as 'marginally significant'.")
  expect_equal(info$func_name, "marginal")

  info <- module_info("modules/no_error.R")
  expect_equal(info$title, "List All P-Values (Test version)")
  expect_equal(info$description, "List all p-values in the text, returning the matched text (e.g., 'p = 0.04')\nand document location in a table.")
  expect_equal(info$details, "Here are some details...")
  expect_equal(info$author, list("Lisa DeBruine", "Daniel Lakens"))
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
  desc <- "List all sentences that describe an effect as 'marginally significant'."
  example <- 'module_run(psychsci, "marginal")'

  output <- capture.output(help)
  expect_equal(output[[1]], title)
  expect_equal(output[[3]], desc)

  expect_equal(class(help), "metacheck_module_help")
  expect_equal(help$title, title)
  expect_equal(help$description, desc)
})

test_that("module_template", {
  expect_true(is.function(metacheck::module_template))
  expect_no_error(helplist <- help(module_template, metacheck))

  expect_error(module_template("a module"),
               "The module_name must contain only letters, numbers, and _",
               fixed = TRUE)

  module_template("demo")
  expect_true(file.exists("modules/demo.R"))
  unlink("modules/demo.R")
})

test_that("module_run", {
  expect_true(is.function(metacheck::module_run))
  expect_no_error(helplist <- help(module_run, metacheck))

  paper <- demoxml() |> read()

  # errors
  expect_error( module_run() )
  expect_error( module_run(paper) )
  expect_error( module_run(paper, "notamodule"),
                "There were no modules that matched notamodule")

  expect_error(module_run(paper, "modules/module-error.R"),
               "The module code has errors")

  expect_error(module_run(paper, "modules/code-error.R"),
               "Running the module produced errors")

  expect_error(module_run(paper, "modules/missing-pkg.R"),
               "notarealpkg")

  expect_error(module_run(paper, "modules/missing-importFrom.R"),
               "dplyr::notarealfunction")

  # demo
  module <- "modules/no_error.R"
  mod_output <- module_run(paper, module)
  expected_summary <- data.frame(id = "to_err_is_human", p_values = 3)

  expect_equal(mod_output$module, module)
  expect_equal(mod_output$title, "List All P-Values (Test version)")
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(mod_output$section, "results")
  expect_equal(mod_output$report, "report text")
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
  paper <- read(demoxml())

  # chained run
  a <- module_run(paper, "all_p_values")
  b <- module_run(a, "chained")
  expect_equal(b$table, a$table[1:2, 1:2])

  # run without chaining
  c <- module_run(paper, "chained")
  expect_equal(c$table, data.frame(a = "not from prev"))
})

test_that("chaining modules - paperlist", {
  paper <- psychsci[1:50]

  p <- module_run(paper, "all_p_values")
  url <- module_run(paper, "all_urls")

  x <- paper |>
    module_run("all_p_values") |>
    module_run("all_urls") |>
    module_run("modules/no_error.R")

  expect_equal(names(x$summary_table), c("id", "p_values", "urls", "p_values.no_error"))
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

test_that("all_p_values", {
  paper <- read(demoxml())
  module <- "all_p_values"
  p <- module_run(paper, module)
  expect_equal(p$traffic_light, "info")
  expect_equal(nrow(p$table), 3)
  expect_equal(p$module, module)
  expect_equal(p$section, "results")

  # iteration: text modules need no special adaptation
  paper <- psychsci
  expect_no_error( mod_output <- module_run(paper, module) )
  expect_equal(nrow(mod_output$table), 4832)

  # check problem with minus sign at end
  minus <- mod_output$table$text[grep("-$", mod_output$table$text)]
  e <- mod_output$table$text[grep("e", mod_output$table$text)]

  expect_equal(length(minus), 0)
  expect_equal(length(e), 9L)

  # specific values
  expected <- c(
    "p=.05",
    "p\n=\n.05",
    "p = .05",
    "p < .05",
    "p > .05",
    "p <= .05",
    "p >= .05",
    "p == .05",
    "p << .05",
    "p >> .05",
    "p ≤ .05",
    "p ≥ .05",
    "p ≪ .05",
    "p ≫ .05",
    "p ≠ .05",
    "p-value = .05",
    "pvalue = .05",
    "p = 0.05",
    "p = 0.05",
    "p = 0.5e-1",
    "p = n.s.",
    "p = ns",
    "p = 5.0x10^-2",
    "p = 5.0 x 10^-2",
    "p = 5.0 x 10 ^ -2",
    "p = 5.0 * 10 ^ -2",
    "p = 5.0e-2",
    "p = 5.0 e-2",
    "p = 5.0 e -2"
  )
  not <- c(
    "up = 0.05",
    "p = stuff",
    "p = -0.05",
    "p less than 0.05",
    "p = 12.05"
  )

  paper <- data.frame(
    id = 1,
    text = c(expected, not),
    expected = rep(c(T, F), c(length(expected), length(not)))
  )
  mod_output <- module_run(paper, module)
  expect_true(!"" %in% mod_output$table$p_comp)
  expect_equal(mod_output$table$p_value[1:20], rep(0.05, 20))
  expect_equal(mod_output$table$p_value[21:22], rep(NA_real_, 2))
  expect_equal(mod_output$table$p_value[23:29], rep(0.05, 7))
})

test_that("all_urls", {
  paper <- read(demoxml())
  module <- "all_urls"
  urls <- module_run(paper, module)
  expect_equal(urls$traffic_light, "info")
  expect_equal(nrow(urls$table), 6)
  expect_equal(urls$module, module)

  # iteration
  paper <- psychsci[1:20]
  mod_output <- module_run(paper, module)
  ids <- mod_output$table$id |> unique()
  expect_true(all(ids %in% names(paper)))
})
