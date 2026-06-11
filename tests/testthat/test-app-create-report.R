# Tests for the create_report_app() shiny server logic, driven with
# shiny::testServer() (no browser, no shinytest2 dependency).

test_that("create_report_app server loads", {
  skip_if_not_installed("shiny")
  env <- load_app_env("create_report_app.R")
  expect_true(is.function(env$server))
  expect_false(is.null(env$ui))
})

test_that("GDPR message reflects the privacy settings", {
  skip_if_not_installed("shiny")
  env <- load_app_env("create_report_app.R")

  # helper: render the gdpr_privacy_ui to plain text for the given settings
  gdpr_text <- function(crossref, pubpeer, repos, llm, grobid = "metacheck") {
    txt <- NULL
    shiny::testServer(env$server, {
      session$setInputs(
        create_crossref      = crossref,
        create_pubpeer       = pubpeer,
        create_repos         = repos,
        create_use_llm       = llm,
        grobid_server_choice = grobid
      )
      txt <<- as.character(output$gdpr_privacy_ui$html %||% output$gdpr_privacy_ui)
    })
    txt
  }

  # all external options off + local grobid -> nothing leaves the machine
  t1 <- gdpr_text(FALSE, FALSE, FALSE, FALSE, grobid = "local")
  expect_match(t1, "No data is sent to external servers")
  expect_match(t1, "No data is sent to or retrieved from external servers")
  expect_match(t1, "Ollama large model is not enabled")
  expect_false(grepl("Change the settings", t1))

  # crossref on -> DOIs sent + call to action appears first
  t2 <- gdpr_text(TRUE, FALSE, FALSE, FALSE)
  expect_match(t2, "DOIs are sent to external servers")
  expect_match(t2, "Change the settings in the 'Options' tab")

  # repos on -> repository line
  t3 <- gdpr_text(FALSE, FALSE, TRUE, FALSE)
  expect_match(t3, "online data repositories")
  expect_match(t3, "Change the settings in the 'Options' tab")

  # llm on -> the "not enabled" line disappears
  t4 <- gdpr_text(FALSE, FALSE, FALSE, TRUE)
  expect_false(grepl("Ollama large model is not enabled", t4))

  # grobid server choice drives the conversion line
  expect_match(gdpr_text(FALSE, FALSE, FALSE, FALSE, grobid = "metacheck"),
               "Eindhoven University of Technology")
  expect_match(gdpr_text(FALSE, FALSE, FALSE, FALSE, grobid = "huggingface"),
               "converted using an external server")
})
