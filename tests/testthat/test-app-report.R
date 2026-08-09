# Tests for the report_app() shiny server logic, driven with
# shiny::testServer() (no browser, no shinytest2 dependency).

test_that("report_app", {
  expect_true(is.function(metacheck::report_app))
  expect_no_error(helplist <- help(report_app, metacheck))
})

test_that("report_app.R attaches metacheck itself", {
  # https://github.com/scienceverse/metacheck/issues/320 -- shiny::runApp()
  # sources report_app.R (and the tab files it source()s in turn) into the
  # GLOBAL environment, not metacheck's own namespace. The tab files call
  # metacheck's exported functions unqualified (e.g. llm_model_list() in
  # tabs/options.R) -- those only resolve if metacheck happens to already be
  # on the search path, which was never guaranteed: metacheck::report_app()
  # (as opposed to library(metacheck); report_app()) failed with "could not
  # find function" because only shiny/shinyjs/shinydashboard were attached,
  # never metacheck itself. A source-level check (not a live detach/reattach,
  # which would corrupt every other test's session) since callr::r() would
  # run against whatever metacheck happens to be INSTALLED, not the source
  # tree under test.
  app_path <- system.file("app", "report_app.R", package = "metacheck")
  testthat::skip_if(app_path == "", "metacheck app dir not installed")
  lines <- readLines(app_path)
  attach_block_end <- grep("^\\}\\)", lines)[1]
  attach_block <- lines[seq_len(attach_block_end)]
  expect_true(any(grepl("^\\s*library\\(metacheck\\)\\s*$", attach_block)))
})


test_that("report_app server loads", {
  skip_shiny()
  env <- load_app_env("report_app.R")
  expect_true(is.function(env$server))
  expect_false(is.null(env$ui))
})

test_that("GDPR message reflects the privacy settings", {
  skip_shiny()
  env <- load_app_env("report_app.R")

  # helper: render the gdpr_privacy_ui to plain text for the given settings
  gdpr_text <- function(crossref, pubpeer, repos, llm, grobid = "metacheck") {
    txt <- NULL
    shiny::testServer(env$server, {
      session$setInputs(
        query_crossref       = crossref,
        query_pubpeer        = pubpeer,
        query_repos          = repos,
        llm_model_choice     = llm,
        grobid_server_choice = grobid
      )
      txt <<- as.character(output$gdpr_privacy_ui$html %||% output$gdpr_privacy_ui)
    })
    txt
  }

  # all external options off + local grobid -> nothing leaves the machine
  t1 <- gdpr_text(FALSE, FALSE, FALSE, "none", grobid = "local")
  expect_match(t1, "No data is sent to external servers")
  expect_match(t1, "DOIs are not sent to CrossRef or PubPeer")
  expect_match(t1, "not retrieve information from online data repositories")
  expect_match(t1, "LLM is not enabled")

  # +crossref, -pubpeer, +repos, LLM local
  t2 <- gdpr_text(TRUE, FALSE, TRUE, "ollama/bozo")
  expect_match(t2, "GDPR compliant server at Eindhoven University")
  expect_match(t2, "DOIs are sent to CrossRef, but not PubPeer")
  expect_match(t2, "APIs are used to retrieve information from online data repositories ")
  expect_match(t2, "local LLM model ollama/bozo is enabled")

  # -crossref, +pubpeer, - repos, LLM external, grobid external
  t3 <- gdpr_text(FALSE, TRUE, FALSE, "github/nono", "huggingface")
  expect_match(t3, "PDF file is converted using an external server")
  expect_match(t3, "DOIs are sent to PubPeer, but not CrossRef")
  expect_match(t3, "not retrieve information from online ")
  expect_match(t3, "external LLM model github/nono is enabled")
})

