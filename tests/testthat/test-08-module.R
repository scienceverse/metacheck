test_that("exists", {
  expect_true(is.function(papercheck::module_run))
  expect_true(is.function(papercheck::module_list))
  expect_true(is.function(papercheck:::module_find))

  builtin <- module_list()
  expect_true(is.data.frame(builtin))
  exp <- c("name", "title", "description", "type", "path")
  expect_equal(names(builtin), exp)
  expect_error(module_find())
  path <- module_find(builtin$name[[1]])
  expect_true(file.exists(path))

  paper <- read_grobid(demoxml())
  expect_error( module_run() )
  expect_error( module_run(paper) )

  # setwd("tests/testthat/")

  expect_error(module_find("notamodule"),
               "There were no modules that matched notamodule",
               fixed = TRUE )

  expect_error( module_run(paper, "notamodule"),
                "There were no modules that matched notamodule",
                fixed = TRUE )

  expect_error(module_run(paper, "modules/code-no-file.mod"),
             "The code file does-not-exist.R could not be found",
             fixed = TRUE)


  expect_error(module_run(paper, "modules/badjson.mod"),
               "The module has a problem with JSON format")
})

test_that("errors", {
  paper <- psychsci[1]
  module <- "modules/error.mod"
  expect_error( module_run(paper, module) )
})

test_that("text", {
  paper <- demoxml() |> read_grobid()

  module <- "all-p-values"
  mod_output <- module_run(paper, module)

  expect_equal(mod_output$module, module)
  expect_equal(mod_output$title, "List All P-Values")
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(mod_output$report, "")

  first_char <- substr(mod_output$table$text, 1, 1)
  expect_true(all(first_char == "p"))
})

test_that("code", {
  paper <- demoxml() |> read_grobid()

  # code from json
  mod_output <- module_run(paper, "retractionwatch")
  expect_equal(names(mod_output$table),
               c("bib_id", "doi", "ref", "id", "retractionwatch", "text"))
  expect_equal(mod_output$report, "You cited some papers in the Retraction Watch database (as of 2025-02-28). These may be retracted, have corrections, or expressions of concern.")
})

# test_that("ml", {
#   skip("python install is messed up")
#   # errors
#   paper <- demoxml() |> read_grobid()
#
#   args <- list(model_dir = "no-exist",
#                result_col = "class",
#                map = c(`0` = "no", `1` = "yes"))
#   expect_error( module_run_ml(paper, args, "."),
#                 "The model directory no-exist could not be found")
# })

test_that("LLM", {
  skip_on_cran()
  skip_if_offline("api.groq.com")

  paper <- read_grobid(demoxml())
  sec <- search_text(paper,
                      section = "method",
                      return = "section")

  model <- "llama3-70b-8192"
  expect_message({
    mod_output <- module_run(sec, "llm-summarise",
                           model = model,
                           seed = 8675309)
  })

  expect_equal(names(mod_output$table),
               c("text", "section", "header", "div", "p",
                 "s", "id", "answer", "time", "tokens"))
  # expect_equal(mod_output$table$answer, "This study randomly assigned 50 scientists to use an automated error-checking tool and 50 to use a checklist to examine whether automation reduces errors in scientific manuscripts.")

  # get attributes
  atts <- attr(mod_output$table, "llm")
  expect_equal(atts$seed, 8675309)
  expect_equal(atts$model, model)
})

test_that("all-p-values", {
  paper <- read_grobid(demoxml())
  module <- "all-p-values"
  p <- module_run(paper, module)
  expect_equal(p$traffic_light, "info")
  expect_equal(nrow(p$table), 3)
  expect_equal(p$module, module)

  # iteration: text modules need no special adaptation
  paper <- psychsci
  expect_no_error( mod_output <- module_run(paper, module) )
  expect_equal(nrow(mod_output$table), 4190)

  # check problem with minus sign at end
  minus <- mod_output$table$text[grep("-$", mod_output$table$text)]
  e <- mod_output$table$text[grep("e", mod_output$table$text)]

  expect_equal(length(minus), 0)
  expect_equal(length(e), 7L)
})

test_that("all-urls", {
  paper <- read_grobid(demoxml())
  module <- "all-urls"
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

test_that("osf-check", {
  skip_if_offline("osf.io")
  module <- "osf-check"

  text <- data.frame(
    text = c("https://osf.io/5tbm9/",
             "https://osf.io/629bx/"),
    section = NA, # TODO: search_text not require section column
    id = c("private", "public")
  )
  mo <- module_run(text, module)
  exp <- c(`https://osf.io/5tbm9` = "closed",
           `https://osf.io/629bx` = "open"
  )
  expect_equal(mo$table$status, exp)

  # iteration
  paper <- psychsci[5:10]
  mod_output <- module_run(paper, module)
  ids <- mod_output$table$id |> unique()
  expect_equal(ids, c("0956797615569001",
                      "0956797615569889",
                      "0956797615583071"))
})

test_that("retractionwatch", {
  paper <- demoxml() |> read_grobid()
  module <- "retractionwatch"

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(mod_output$table$doi, "10.1177/0956797614520714")
  expect_equal(mod_output$report, "You cited some papers in the Retraction Watch database (as of 2025-02-28). These may be retracted, have corrections, or expressions of concern.")

  # iteration
  paper <- psychsci
  mod_output <- module_run(paper, module)
  dois <- mod_output$table$doi |> unique()
  expect_equal(dois, c("10.1177/0956797612470827",
                       "10.1186/gb-2013-14-10-r115",
                       "10.1038/s41562-023-01749-9"))
})

test_that("imprecise-p", {
  paper <- demodir() |> read_grobid()
  paper <- paper[[1]]

  module <- "imprecise-p"
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
  expect_equal(lt05, 74)
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

test_that("ref-consistency", {
  paper <- demoxml() |> read_grobid()
  module <- "ref-consistency"

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 2)
  expect_equal(mod_output$module, module)

  paper <- psychsci[c(23, 25)]
  mod_output1 <- module_run(paper[[1]], module)
  mod_output2 <- module_run(paper[[2]], module)
  mod_output3 <- module_run(paper, module)
  expect_equal(rbind(mod_output1$table, mod_output2$table),
               mod_output3$table)
})

test_that("statcheck", {
  paper <- demoxml() |> read_grobid()
  module <- "statcheck"

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 1)
  expect_equal(mod_output$table$raw, "t(97.2) = -1.96, p = 0.152")
  expect_equal(mod_output$module, module)

  paper <- psychsci[100:101]
  expect_no_error(
    mod_output <- module_run(paper, module)
  )
})

