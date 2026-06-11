# Tests for the report_app() shiny server logic, driven with
# shiny::testServer() (no browser, no shinytest2 dependency).

test_that("report_app", {
  expect_true(is.function(metacheck::report_app))
  expect_no_error(helplist <- help(report_app, metacheck))
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

