test_that("exists", {
  expect_true(is.function(papercheck::module_run))
  expect_no_error(helplist <- help(module_run, papercheck))

  expect_true(is.function(papercheck::module_list))
  expect_no_error(helplist <- help(module_list, papercheck))

  expect_true(is.function(papercheck:::module_find))
  expect_no_error(helplist <- help(module_find, papercheck))

  expect_true(is.function(papercheck::module_help))
  expect_no_error(helplist <- help(module_help, papercheck))

  expect_true(is.function(papercheck::module_info))
  expect_no_error(helplist <- help(module_info, papercheck))
})

test_that("errors", {
  paper <- read_grobid(demoxml())
  expect_error( module_run() )
  expect_error( module_run(paper) )

  expect_error(module_help("bad_arg"),
               "There were no modules that matched bad_arg")
  expect_error(module_info("bad_arg"),
               "There were no modules that matched bad_arg")

  # setwd("tests/testthat/")

  expect_error(module_find("notamodule"),
               "There were no modules that matched notamodule",
               fixed = TRUE )

  expect_error( module_run(paper, "notamodule"),
                "There were no modules that matched notamodule")

  expect_error(module_run(paper, "modules/module-error.R"),
               "The module code has errors")

  expect_error(module_run(paper, "modules/code-error.R"),
               "Running the module produced errors")
})

test_that("module_list", {
  builtin <- module_list()
  expect_true(is.data.frame(builtin))
  exp <- c("name", "title", "description", "path")
  expect_equal(names(builtin), exp)

  expect_error(module_find())
  path <- module_find(builtin$name[[1]])
  expect_true(file.exists(path))
})

test_that("module_info", {
  info <- module_info("marginal")
  expect_equal(info$title, "Marginal Significance")
  expect_equal(info$author, "Daniel Lakens")
  expect_equal(info$description, "List all sentences that describe an effect as 'marginally significant'.")
  expect_equal(info$func_name, "marginal")

  example <- 'module_run(psychsci, "marginal")'
  class(example) <- "rd"
  expect_equal(info$examples, example)

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
  ml <- capture.output(module_list())
  mh <- capture.output(module_help())
  expect_equal(mh, ml)

  # marginal
  help <- module_help("marginal")

  title <- "Marginal Significance"
  desc <- "List all sentences that describe an effect as 'marginally significant'."
  example <- 'module_run(psychsci, "marginal")'
  expected <- c(
    title,
    "",
    desc,
    "",
    "``` r",
    example,
    "```"
  )

  output <- capture.output(help)
  expect_equal(output, expected)

  expect_equal(class(help), "ppchk_module_help")
  expect_equal(help$title, title)
  expect_equal(help$description, desc)

  class(example) <- "rd"
  expect_equal(help$examples, example)
})

test_that("test", {
  paper <- demoxml() |> read_grobid()

  module <- "modules/no_error.R"
  mod_output <- module_run(paper, module)
  expected_summary <- data.frame(id = "to_err_is_human", p_values = 0)

  expect_equal(mod_output$module, module)
  expect_equal(mod_output$title, "List All P-Values (Test version)")
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(mod_output$report, "")
  expect_equal(mod_output$paper, paper)
  expect_equal(mod_output$summary, expected_summary)

  first_char <- substr(mod_output$table$text, 1, 1)
  expect_true(all(first_char == "p"))
})

# test_that("LLM", {
#   skip_on_cran()
#   skip_if_offline("api.groq.com")
#
#   paper <- read_grobid(demoxml())
#   sec <- search_text(paper,
#                       section = "method",
#                       return = "section")
#
#   model <- "llama-3.3-70b-versatile"
#   expect_message({
#     mod_output <- module_run(sec, "llm_summarise",
#                              model = model,
#                              seed = 8675309)
#   })
#
#   expect_equal(names(mod_output$table),
#                c("text", "section", "header", "div", "p",
#                  "s", "id", "answer", "time", "tokens"))
#   # expect_equal(mod_output$table$answer, "This study randomly assigned 50 scientists to use an automated error-checking tool and 50 to use a checklist to examine whether automation reduces errors in scientific manuscripts.")
#
#   # get attributes
#   atts <- attr(mod_output$table, "llm")
#   expect_equal(atts$seed, 8675309)
#   expect_equal(atts$model, model)
# })

test_that("all_p_values", {
  paper <- read_grobid(demoxml())
  module <- "all_p_values"
  p <- module_run(paper, module)
  expect_equal(p$traffic_light, "info")
  expect_equal(nrow(p$table), 3)
  expect_equal(p$module, module)

  # iteration: text modules need no special adaptation
  paper <- psychsci
  expect_no_error( mod_output <- module_run(paper, module) )
  expect_equal(nrow(mod_output$table), 4832)

  # check problem with minus sign at end
  minus <- mod_output$table$text[grep("-$", mod_output$table$text)]
  e <- mod_output$table$text[grep("e", mod_output$table$text)]

  expect_equal(length(minus), 0)
  expect_equal(length(e), 9L)
})

test_that("all_urls", {
  paper <- read_grobid(demoxml())
  module <- "all_urls"
  urls <- module_run(paper, module)
  expect_equal(urls$traffic_light, "info")
  expect_equal(nrow(urls$table), 3)
  expect_equal(urls$module, module)

  # iteration
  paper <- psychsci[1:20]
  mod_output <- module_run(paper, module)
  ids <- mod_output$table$id |> unique()
  expect_true(all(ids %in% names(paper)))
})

test_that("osf_check", {
  skip_on_ci()
  skip_if_not(osf_api_check() == "ok")
  module <- "osf_check"

  verbose(FALSE)

  text <- data.frame(
    text = c("https://osf.io/5tbm9/",
             "https://osf.io/629bx/",
             "osf.io/ abcd5"),
    id = c("private", "public", "unfound")
  )
  expect_warning( mo <- module_run(text, module), "abcd5" )

  if (all(mo$table$status == "unknown")) {
    skip("OSF is down")
  }
  exp <- c("closed", "open", "unfound")
  expect_equal(mo$table$status, exp)

  # iteration
  paper <- psychsci[5:10]
  mod_output <- module_run(paper, module)
  ids <- mod_output$table$id |> unique()
  expect_equal(ids, c("0956797615569001",
                      "0956797615569889",
                      "0956797615583071"))

  verbose(TRUE)
})

test_that("retractionwatch", {
  paper <- demoxml() |> read_grobid()
  module <- "retractionwatch"

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(mod_output$table$doi, "10.1177/0956797614520714")
  expect_equal(mod_output$report, "You cited some papers in the Retraction Watch database (as of 2025-05-20). These may be retracted, have corrections, or expressions of concern.")

  # iteration
  paper <- psychsci
  mod_output <- module_run(paper, module)
  dois <- mod_output$table$doi |> unique()
  expect_equal(dois, c("10.1177/0956797612470827",
                       "10.1186/gb-2013-14-10-r115",
                       "10.1038/s41562-023-01749-9"))
})

test_that("exact_p", {
  paper <- demodir() |> read_grobid()
  paper <- paper[[1]]

  module <- "exact_p"
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "green")
  expect_equal(nrow(mod_output$table), 0)
  expect_equal(mod_output$report, "All p-values were reported with standard precision")

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
  paper$full_text[10, "text"] <- "OK p-value example; p < .001"
  paper$full_text[11, "text"] <- "OK p-value example; p < .0005"

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 9)
  expect_equal(mod_output$report, "You may have reported some imprecise p-values")

  # iteration
  paper <- psychsci
  mod_output <- module_run(paper, module)
  lt05 <- grepl("p < .05", mod_output$table$text) |> sum()
  expect_equal(lt05, 174)
  expect_equal(mod_output$table$p_comp[[1]], "<")
  expect_equal(mod_output$table$p_value[[1]], 0.05)
})

test_that("marginal", {
  paper <- demodir() |> read_grobid()
  paper <- paper[[1]]
  module <- "marginal"

  # no relevant text
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "green")
  expect_equal(nrow(mod_output$table), 0)
  expect_equal(mod_output$report, "No effects were described as marginally/borderline/close to significant.")

  # add marginal text
  paper$full_text[1, "text"] <- "This effect was marginally significant."
  paper$full_text[12, "text"] <- "This effect approached significance."

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 2)
  expect_equal(mod_output$report, "You described effects as marginally/borderline/close to significant. It is better to write 'did not reach the threshold alpha for significance'.")

  # iteration
  mod_output <- module_run(psychsci, module)
  expect_true(unique(mod_output$table$id) |> length() > 1)
})

# test_that("sample-size", {
#   skip("python install is messed up")
#   skip_on_cran()
#   model_dir <- system.file("modules/sample-size", package = "papercheck")
#
#   if (model_dir == "") {
#     skip("needs big classifier: sample-size")
#   }
#
#   paper <- demoxml() |> read_grobid() |>
#     search_text(".{30, }", section = "method", return = "sentence")
#   module <- "sample-size-ml"
#
#   mod_output <- module_run(paper, module)
#   expect_equal(mod_output$traffic_light, "green")
#   expect_equal(nrow(mod_output$table), 2)
#   expect_equal(mod_output$module, module)
# })

test_that("ref_consistency", {
  paper <- demoxml() |> read_grobid()
  module <- "ref_consistency"

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 2)
  expect_equal(mod_output$module, module)

  # iteration
  paper <- psychsci[c(23, 25)]
  mod_output1 <- module_run(paper[[1]], module)
  mod_output2 <- module_run(paper[[2]], module)
  mod_output3 <- module_run(paper, module)
  expect_equal(rbind(mod_output1$table, mod_output2$table),
               mod_output3$table)

  mod_output1$traffic_light
  mod_output2$traffic_light
  mod_output3$traffic_light
})

test_that("statcheck", {
  paper <- demoxml() |> read_grobid()
  module <- "statcheck"

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 1)
  expect_equal(mod_output$table$raw, "t(97.2) = -1.96, p = 0.152")
  expect_equal(mod_output$module, module)

  # iteration
  paper <- psychsci[1:2]
  expect_no_error(
    mod_output <- module_run(paper, module)
  )
})

test_that("chaining modules", {
  paper <- psychsci[1:50]

  p <- module_run(paper, "all_p_values")
  url <- module_run(paper, "all_urls")

  x <- paper |>
    module_run("all_p_values") |>
    module_run("all_urls") |>
    module_run("modules/no_error.R")

  expect_equal(names(x$summary), c("id", "p_values", "urls", "p_values.no_error"))
  expect_equal(x$summary$p_values, p$summary$p_values)
  expect_equal(x$summary$urls, url$summary$urls)
})
