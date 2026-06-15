# # Tests for the metacheck_app() shiny server logic, driven with shiny::testServer().
#
# # the app's source("tabs/options_demo.R") puts demo_gating / demo_toggle_on in
# # the global env (same as when the app actually runs), so read them from there
#
# test_that("metacheck_app", {
#   expect_true(is.function(metacheck::metacheck_app))
#   expect_no_error(helplist <- help(metacheck_app, metacheck))
#
#   expect_error(metacheck_app(1))
#
#   with_mocked_bindings(
#     `requireNamespace` = function(...) FALSE,
#     {
#       expect_warning(metacheck_app(), "You need to install the following packages")
#     }
#   )
# })
#
# get_demo_obj <- function(name) get(name, envir = globalenv())
#
# test_that("metacheck_app server loads", {
#   skip_shiny()
#   env <- load_app_env("metacheck_app.R")
#   expect_true(is.function(env$server))
#   expect_false(is.null(env$ui))
#   # the gating map and helper are defined (sourced from options_demo.R)
#   expect_true(is.list(get_demo_obj("demo_gating")))
#   expect_true(is.function(get_demo_obj("demo_toggle_on")))
# })
#
# test_that("demo_toggle_on handles checkboxes and the llm radio", {
#   skip_shiny()
#   load_app_env("metacheck_app.R")
#   f <- get_demo_obj("demo_toggle_on")
#
#   # checkbox-style logicals
#   expect_true(f(TRUE))
#   expect_false(f(FALSE))
#   expect_false(f(NULL))
#   # the llm radio: "none" is off, any other backend is on
#   expect_false(f("none"))
#   expect_true(f("ollama"))
#   expect_true(f("groq"))
# })
#
# test_that("gating map covers the expected external modules", {
#   skip_shiny()
#   load_app_env("metacheck_app.R")
#   g <- get_demo_obj("demo_gating")
#
#   expect_setequal(
#     names(g),
#     c("create_crossref", "create_pubpeer", "create_repos",
#       "create_llm_backend", "create_causal")
#   )
#   expect_true("ref_accuracy"  %in% g$create_crossref)
#   expect_true("ref_pubpeer"   %in% g$create_pubpeer)
#   expect_true("code_check"    %in% g$create_repos)
#   expect_true("repo_check"    %in% g$create_repos)
#   expect_true("power"         %in% g$create_llm_backend)
#   expect_true("causal_claims" %in% g$create_causal)
#
#   # every gated module is a real module
#   all_mods <- module_list()$name
#   for (m in unlist(g)) expect_true(m %in% all_mods, info = m)
# })
#
# test_that("loading the demo paper populates the paper and text search", {
#   skip_shiny()
#   env <- load_app_env("metacheck_app.R")
#
#   shiny::testServer(env$server, {
#     # seed module_list so its init observer doesn't error on an empty value
#     session$setInputs(module_list = module_list()$name[[1]])
#
#     # initially no paper
#     expect_equal(length(my_paper()), 0)
#
#     # load the bundled demo paper
#     session$setInputs(demo = 1)
#     session$flushReact()
#     expect_gt(length(my_paper()), 0)
#     expect_s3_class(my_paper()[[1]], "scivrs_paper")
#   })
# })
#
# test_that("creating a report with no modules selected does not call report()", {
#   skip_shiny()
#   env <- load_app_env("metacheck_app.R")
#
#   shiny::testServer(env$server, {
#     session$setInputs(module_list = module_list()$name[[1]])
#
#     # load a paper but select no modules, then click Create Report
#     session$setInputs(demo = 1)
#     session$flushReact()
#     session$setInputs(report_run = 1)
#     session$flushReact()
#
#     # the guard should stop before producing a report (no report path set)
#     expect_equal(report_path(), "")
#   })
# })
