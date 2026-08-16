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


# The repository modules (repo_check, code_check, data_check, codebook_check)
# all take local_path/local_only and are all gated on the same setting, so the
# app keeps them in one `repo_modules` vector. These tests pin down that the
# vector matches the modules that actually ship, and that both places the app
# uses it — the module list it runs, and the R code it shows the user — agree.
repo_mods <- c("repo_check", "code_check", "data_check", "codebook_check")

test_that("the app's module list names modules that exist", {
  skip_shiny()
  env <- load_app_env("report_app.R")

  mods <- environment(env$server)$validated_modules
  # validated_modules is created inside server(), so read it from a running
  # session rather than from the file's top level.
  shiny::testServer(env$server, {
    mods <<- validated_modules
    repo_in_app <<- repo_modules
  })

  expect_setequal(repo_in_app, repo_mods)
  expect_true(all(repo_mods %in% mods))
  # all_p_values reports every p value rather than flagging a problem, so it
  # is deliberately not offered in the app
  expect_false("all_p_values" %in% mods)

  # every named module must resolve to a file in inst/modules/
  moddir <- system.file("modules", package = "metacheck")
  for (m in mods) {
    expect_true(file.exists(file.path(moddir, paste0(m, ".R"))),
                info = paste("no module file for", m))
  }
})

test_that("repository modules are dropped only when there is no repo and no local path", {
  skip_shiny()
  env <- load_app_env("report_app.R")

  # Render the R code snippet for a set of options and return it as text.
  snippet <- function(crossref = TRUE, pubpeer = TRUE, repos = TRUE,
                      llm = "none", local = "", grobid = "metacheck") {
    txt <- NULL
    shiny::testServer(env$server, {
      session$setInputs(
        query_crossref       = crossref,
        query_pubpeer        = pubpeer,
        query_repos          = repos,
        llm_model_choice     = llm,
        local_path           = local,
        grobid_server_choice = grobid
      )
      txt <<- as.character(output$r_code$html %||% output$r_code)
    })
    txt
  }

  # repositories queried, no local folder: all four modules run, no args needed
  s1 <- snippet(repos = TRUE, local = "")
  for (m in repo_mods) expect_match(s1, paste0('"', m, '"'), fixed = TRUE)
  expect_no_match(s1, "args = list", fixed = TRUE)

  # no repositories and no local folder: all four are dropped
  s2 <- snippet(repos = FALSE, local = "")
  for (m in repo_mods) expect_no_match(s2, paste0('"', m, '"'), fixed = TRUE)

  # no repositories but a real local folder: all four run, with local_only
  dir <- withr::local_tempdir()
  s3 <- snippet(repos = FALSE, local = dir)
  for (m in repo_mods) {
    expect_match(s3, paste0('"', m, '"'), fixed = TRUE)
    expect_match(s3, paste0(m, " = list(local_path = "), fixed = TRUE)
  }
  expect_equal(lengths(regmatches(s3, gregexpr("local_only = TRUE", s3))),
               length(repo_mods))

  # repositories queried AND a local folder: local_path given, local_only off
  s4 <- snippet(repos = TRUE, local = dir)
  for (m in repo_mods) {
    expect_match(s4, paste0(m, " = list(local_path = "), fixed = TRUE)
  }
  expect_no_match(s4, "local_only = TRUE", fixed = TRUE)

  # a local path that does not exist is treated as no local folder at all
  s5 <- snippet(repos = FALSE, local = file.path(dir, "does_not_exist"))
  for (m in repo_mods) expect_no_match(s5, paste0('"', m, '"'), fixed = TRUE)

  # whitespace around the path is trimmed before it is tested for existence
  s6 <- snippet(repos = FALSE, local = paste0("  ", dir, "  "))
  for (m in repo_mods) expect_match(s6, paste0('"', m, '"'), fixed = TRUE)
})

test_that("the generated R code is syntactically valid R", {
  skip_shiny()
  env <- load_app_env("report_app.R")

  snippet_code <- function(crossref, pubpeer, repos, llm, local) {
    txt <- NULL
    shiny::testServer(env$server, {
      session$setInputs(
        query_crossref       = crossref,
        query_pubpeer        = pubpeer,
        query_repos          = repos,
        llm_model_choice     = llm,
        local_path           = local,
        grobid_server_choice = "metacheck"
      )
      txt <<- as.character(output$r_code$html %||% output$r_code)
    })
    # strip the <pre><code> wrapper and undo HTML escaping
    txt |>
      gsub("<[^>]*>", "", x = _) |>
      gsub("&quot;", '"', x = _) |>
      gsub("&#39;", "'", x = _) |>
      gsub("&amp;", "&", x = _) |>
      gsub("&lt;", "<", x = _) |>
      gsub("&gt;", ">", x = _)
  }

  dir <- withr::local_tempdir()
  grid <- expand.grid(crossref = c(TRUE, FALSE),
                      pubpeer  = c(TRUE, FALSE),
                      repos    = c(TRUE, FALSE),
                      local    = c("", dir))
  for (i in seq_len(nrow(grid))) {
    code <- snippet_code(grid$crossref[i], grid$pubpeer[i], grid$repos[i],
                         "none", as.character(grid$local[i]))
    expect_no_error(parse(text = code))
  }
})

test_that("modules that need a setting are dropped when it is off", {
  skip_shiny()
  env <- load_app_env("report_app.R")

  snippet <- function(crossref = TRUE, pubpeer = TRUE) {
    txt <- NULL
    shiny::testServer(env$server, {
      session$setInputs(
        query_crossref       = crossref,
        query_pubpeer        = pubpeer,
        query_repos          = TRUE,
        llm_model_choice     = "none",
        local_path           = "",
        grobid_server_choice = "metacheck"
      )
      txt <<- as.character(output$r_code$html %||% output$r_code)
    })
    txt
  }

  # ref_pubpeer needs the PubPeer API; ref_accuracy needs the CrossRef
  # bib_match table, so each disappears when its setting is off
  expect_match(snippet(), '"ref_pubpeer"', fixed = TRUE)
  expect_match(snippet(), '"ref_accuracy"', fixed = TRUE)
  expect_no_match(snippet(pubpeer = FALSE), '"ref_pubpeer"', fixed = TRUE)
  expect_no_match(snippet(crossref = FALSE), '"ref_accuracy"', fixed = TRUE)
})

test_that("the report run passes the right modules and args to report()", {
  skip_shiny()
  env <- load_app_env("report_app.R")

  # Drive the upload handler without touching the network or Quarto: stub the
  # pipeline functions in the app's own environment (its parent is globalenv,
  # so these shadow the package versions) and capture what report() is given.
  captured <- new.env()
  env$convert       <- function(...) "fake.json"
  env$read          <- function(...) structure(list(id = "test"), class = "scivrs_paper")
  env$add_bib_match <- function(paper, ...) paper
  env$report        <- function(paper, modules, output_file, output_format, args, ...) {
    captured$modules <- modules
    captured$args    <- args
    output_file
  }
  env$quarto_render <- function(...) invisible(NULL)

  # capture the report() call, then stop before Quarto renders
  run <- function(repos, local, crossref = TRUE, pubpeer = TRUE) {
    rm(list = ls(captured), envir = captured)
    pdf <- withr::local_tempfile(fileext = ".pdf")
    writeLines("x", pdf)
    shiny::testServer(env$server, {
      session$setInputs(
        query_crossref       = crossref,
        query_pubpeer        = pubpeer,
        query_repos          = repos,
        llm_model_choice     = "none",
        local_path           = local,
        grobid_server_choice = "metacheck"
      )
      session$setInputs(upload_pdf = data.frame(
        name = "paper.pdf", datapath = pdf,
        size = 1, type = "application/pdf"))
    })
    as.list(captured)
  }

  dir <- withr::local_tempdir()

  # repositories on, no local folder: all four repo modules run, no args
  r1 <- run(repos = TRUE, local = "")
  expect_true(all(repo_mods %in% r1$modules))
  expect_length(r1$args, 0)

  # repositories off, no local folder: all four are dropped
  r2 <- run(repos = FALSE, local = "")
  expect_false(any(repo_mods %in% r2$modules))

  # repositories off but a local folder given: all four run against it, and
  # local_only is TRUE so nothing is fetched online
  r3 <- run(repos = FALSE, local = dir)
  expect_true(all(repo_mods %in% r3$modules))
  expect_setequal(names(r3$args), repo_mods)
  for (m in repo_mods) {
    expect_equal(r3$args[[m]]$local_path, dir)
    expect_true(r3$args[[m]]$local_only)
  }

  # repositories on AND a local folder: local_only must be FALSE, otherwise
  # the app would silently skip the online repositories the user asked for
  r4 <- run(repos = TRUE, local = dir)
  expect_setequal(names(r4$args), repo_mods)
  for (m in repo_mods) {
    expect_equal(r4$args[[m]]$local_path, dir)
    expect_false(r4$args[[m]]$local_only)
  }

  # the settings-dependent modules behave the same way here as in the snippet
  expect_false("ref_pubpeer"  %in% run(TRUE, "", pubpeer  = FALSE)$modules)
  expect_false("ref_accuracy" %in% run(TRUE, "", crossref = FALSE)$modules)
  expect_true("ref_accuracy"  %in% run(TRUE, "", crossref = TRUE)$modules)
})

test_that("the report run and the shown R code select the same modules", {
  skip_shiny()
  # The module list the app runs is built inside the upload handler, which also
  # converts a PDF and renders Quarto, so it cannot be driven here. Instead read
  # the selection rules straight out of the app file and check that the two
  # blocks apply the same conditions to the same modules.
  appdir <- system.file("app", package = "metacheck")
  code <- readLines(file.path(appdir, "report_app.R"))

  # the run block uses input$ directly, the snippet block uses local variables
  run_block  <- code[grep("modules <- validated_modules", code):
                     grep("^\\s+skip_online <- ", code)]
  snip_block <- code[grep("mods_used    <- validated_modules", code):
                     grep("mods_str     <- ", code)]

  normalise <- function(x) {
    x |>
      paste(collapse = " ") |>
      gsub("isTRUE\\(input\\$query_([a-z]+)\\)", "use_\\1", x = _) |>
      gsub("input\\$query_([a-z]+)", "use_\\1", x = _) |>
      # keep repo_modules intact; only the working variable is renamed
      gsub("\\bmods_used\\b|(?<!repo_)\\bmodules\\b", "M", x = _, perl = TRUE) |>
      gsub("has_local_snip|has_local", "L", x = _) |>
      gsub("\\s+", " ", x = _)
  }

  # both blocks must drop ref_pubpeer, ref_accuracy and the repo modules
  for (blk in list(run_block, snip_block)) {
    n <- normalise(blk)
    expect_match(n, "if \\(!use_pubpeer\\) M <- setdiff\\(M, \"ref_pubpeer\"\\)")
    expect_match(n, "if \\(!use_crossref\\) M <- setdiff\\(M, \"ref_accuracy\"\\)")
    expect_match(n, "if \\(!use_repos && !L\\) M <- setdiff\\(M, repo_modules\\)")
  }
})

test_that("the app reads the crossref setting that the options tab defines", {
  skip_shiny()
  appdir <- system.file("app", package = "metacheck")
  app  <- readLines(file.path(appdir, "report_app.R"))
  opts <- readLines(file.path(appdir, "tabs", "options.R"))

  # every input$<name> the server reads must be created by one of the tabs
  used <- regmatches(app, gregexpr("input\\$[a-zA-Z_]+", app)) |>
    unlist() |> unique() |> sub("input\\$", "", x = _)
  report_tab <- readLines(file.path(appdir, "tabs", "report.R"))
  # inputs are created in the tab files and also by the server's own renderUI
  # blocks (e.g. the "View Report Again" button), so scan all three files
  all_ui <- c(opts, report_tab, app)
  defined <- c(
    regmatches(all_ui, gregexpr('"[a-zA-Z_]+"', all_ui)) |>
      unlist() |> gsub('"', "", x = _),
    "tabs"  # the sidebar menu id, set in report_app.R itself
  )
  expect_true(all(used %in% defined),
              info = paste("inputs read but never defined:",
                           paste(setdiff(used, defined), collapse = ", ")))
})



# --- anonymous usage statistics ----------------------------------------
# Kept to justify further development in grant applications. These tests pin
# down that they stay anonymous: a row says something happened, never who did
# it or what they checked.

# Take the usage functions out of the app file without running the rest of it
# (the UI queries an LLM server as it is built).
usage_env <- function() {
  appdir <- system.file("app", package = "metacheck")
  testthat::skip_if(appdir == "", "metacheck app dir not installed")
  code  <- readLines(file.path(appdir, "report_app.R"), warn = FALSE)
  start <- grep("^usage_file <- function", code)
  end   <- grep("^## UI ----", code)[1]
  env <- new.env(parent = globalenv())
  eval(parse(text = paste(code[start:(end - 1)], collapse = "\n")), envir = env)
  env
}

test_that("nothing is recorded when the report app runs locally", {
  skip_shiny()
  Sys.unsetenv("SHINY_PORT")   # local: development and testing
  dir <- file.path(tempdir(), "report_usage_local")
  unlink(dir, recursive = TRUE)
  withr::local_envvar(METACHECK_USAGE_DIR = dir)

  env <- usage_env()
  env$usage_record("session")
  env$usage_record("report", 1L, 14L)

  expect_false(file.exists(env$usage_file()))
  expect_equal(nrow(env$usage_summary()), 0)
})

test_that("sessions and reports are counted on a Shiny server", {
  skip_shiny()
  dir <- file.path(tempdir(), "report_usage_hosted")
  unlink(dir, recursive = TRUE)
  withr::local_envvar(METACHECK_USAGE_DIR = dir, SHINY_PORT = "3838")

  env <- usage_env()
  env$usage_record("session")
  env$usage_record("report", 1L, 14L)
  env$usage_record("report", 1L, 12L)

  s <- env$usage_summary()
  expect_equal(nrow(s), 1)
  expect_equal(s$sessions, 1L)
  expect_equal(s$reports, 2L)
  expect_equal(s$modules, 26L)   # 14 + 12
})

test_that("nothing identifying is written to the report statistics", {
  skip_shiny()
  dir <- file.path(tempdir(), "report_usage_anon")
  unlink(dir, recursive = TRUE)
  withr::local_envvar(METACHECK_USAGE_DIR = dir, SHINY_PORT = "3838")

  env <- usage_env()
  env$usage_record("session")
  env$usage_record("report", 1L, 14L)

  txt <- paste(readLines(env$usage_file(), warn = FALSE), collapse = " ")
  # no file name, no paper content, no account name
  expect_no_match(txt, ".pdf", fixed = TRUE)
  expect_no_match(txt, Sys.info()[["user"]], fixed = TRUE)

  # only these four columns exist, so nothing else can creep in unnoticed
  x <- utils::read.csv(env$usage_file())
  expect_setequal(names(x), c("date", "event", "reports", "modules"))
  # a date, not a timestamp: two rows on the same day are indistinguishable
  expect_false(any(grepl(":", x$date)))
})

test_that("an unwritable folder is handled rather than erroring", {
  skip_shiny()
  # On a Shiny Server the app runs as the `shiny` user, whose home directory
  # may not be writable, so this must fail quietly rather than break the app.
  bad <- tempfile(); writeLines("x", bad)   # a FILE where a folder must be
  on.exit(unlink(bad), add = TRUE)
  withr::local_envvar(METACHECK_USAGE_DIR = file.path(bad, "sub"),
                      SHINY_PORT = "3838")

  env <- usage_env()
  expect_equal(env$usage_file(), "")
  expect_no_error(env$usage_record("report", 1L, 14L))
  expect_equal(nrow(env$usage_summary()), 0)
})

test_that("the code says the statistics are anonymous and why they are kept", {
  skip_shiny()
  # The comment is what stops someone quietly adding an identifier later.
  appdir <- system.file("app", package = "metacheck")
  code <- paste(readLines(file.path(appdir, "report_app.R"), warn = FALSE),
                collapse = " ")
  expect_match(code, "ANONYMOUS USAGE STATISTICS", fixed = TRUE)
  expect_match(code, "grant applications", fixed = TRUE)
  expect_match(code, "NO identifier", fixed = TRUE)
  # and a report is only counted once it has actually been produced
  expect_match(code, 'usage_record("report", 1L, length(modules))', fixed = TRUE)
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
