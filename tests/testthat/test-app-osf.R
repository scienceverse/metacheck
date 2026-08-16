# Tests for the osf_app() shiny server logic, driven with
# shiny::testServer(). Every OSF function is stubbed in the app's environment
# (its parent is globalenv, so these shadow the package versions), so no test
# here reaches the network.
#
# The app reads SHINY_PORT once, when its environment is created, to decide
# whether it is hosted (the browser downloads directly) or local (metacheck
# downloads). The hosted tests set that variable, and helper.R wraps
# test_that, which puts on.exit/withr cleanup on the wrong frame — so tests
# for the local route clear it themselves rather than trusting the order.
Sys.unsetenv("SHINY_PORT")

test_that("osf_app", {
  expect_true(is.function(metacheck::osf_app))
  expect_no_error(helplist <- help(osf_app, metacheck))
})

test_that("osf_app server loads", {
  skip_shiny()
  env <- load_app_env("osf_app.R")
  expect_true(is.function(env$server))
  expect_false(is.null(env$ui))
})

# A listing like osf_user_projects() returns: one row per project, with the
# public flag NA where the project could not be read at all.
fake_projects <- function() {
  data.frame(
    osf_id   = c("aaaaa", "bbbbb", "ccccc", "ddddd"),
    name     = c("ManyLabs 2", "Pilot study", "ManyLabs 3 secret", NA),
    category = c("project", "project", "project", "project"),
    public   = c(TRUE, FALSE, FALSE, NA),
    osf_url  = paste0("https://osf.io/",
                      c("aaaaa", "bbbbb", "ccccc", "ddddd"))
  )
}

# Stub the OSF calls so the app can run offline. `type` decides whether the
# entered ID is treated as a user profile or a single project.
stub_osf <- function(env, type = "users", projects = fake_projects(),
                     captured = NULL, pat = "test-token") {
  env$osf_type          <- function(id, ...) type
  env$osf_user_projects <- function(id, ...) projects
  env$osf_info          <- function(id, ...) {
    data.frame(osf_id = id, name = "One project",
               category = "project", public = TRUE)
  }
  env$.osf_pat_validate <- function(pat, ...) identical(pat, "good-token")
  # Behaves like the real osf_pat(): remembers what it is given, and returns
  # it when asked. `pat` seeds it, so a test can start with a token already
  # set (the app refuses to download a private project without one).
  stored_pat <- pat
  env$osf_pat <- function(new_pat = NULL) {
    if (is.null(new_pat)) return(stored_pat)
    stored_pat <<- new_pat
    invisible(new_pat)
  }
  env$osf_file_download <- function(osf_id, ...) {
    if (!is.null(captured)) {
      captured$calls <- c(captured$calls %||% list(),
                          list(c(list(osf_id = osf_id), list(...))))
    }
    data.frame(osf_id = osf_id, downloaded = TRUE,
               download_path = list(...)$download_to %||% NA_character_)
  }
  env
}

test_that("a user ID is listed, a project ID becomes a one-row list", {
  skip_shiny()

  env <- stub_osf(load_app_env("osf_app.R"), type = "users")
  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    expect_equal(nrow(projects()), 4)
    expect_equal(nrow(filtered()), 4)
  })

  # a single project resolves through osf_info() into one row
  env2 <- stub_osf(load_app_env("osf_app.R"), type = "nodes")
  shiny::testServer(env2$server, {
    session$setInputs(osf_id = "6nt4v", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    expect_equal(nrow(projects()), 1)
    expect_equal(projects()$osf_id, "6nt4v")
    expect_equal(projects()$name, "One project")
  })
})

test_that("bad or unreadable IDs are reported rather than downloaded", {
  skip_shiny()

  env <- stub_osf(load_app_env("osf_app.R"))
  shiny::testServer(env$server, {
    # nothing entered
    session$setInputs(osf_id = "", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    expect_match(list_error(), "Enter an OSF user or project ID")
    expect_null(projects())

    # not an OSF ID at all
    session$setInputs(osf_id = "this is not an id")
    session$setInputs(find_projects = 2)
    expect_match(list_error(), "not a valid OSF ID")
    expect_null(projects())
  })

  # a valid ID the OSF cannot read
  env2 <- stub_osf(load_app_env("osf_app.R"), type = "inaccessible")
  shiny::testServer(env2$server, {
    session$setInputs(osf_id = "zzzzz", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    expect_match(list_error(), "could not be\\s+read")
    expect_null(projects())
  })
})

test_that("the search box and the private-only box filter the listing", {
  skip_shiny()
  env <- stub_osf(load_app_env("osf_app.R"))

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    expect_equal(nrow(filtered()), 4)

    # search matches the title, and is not case sensitive
    session$setInputs(search_term = "manylabs")
    expect_setequal(filtered()$osf_id, c("aaaaa", "ccccc"))

    # search also matches an ID that is pasted in
    session$setInputs(search_term = "bbbbb")
    expect_equal(filtered()$osf_id, "bbbbb")

    # a word that appears nowhere gives an empty listing, not an error
    session$setInputs(search_term = "nothing matches this")
    expect_equal(nrow(filtered()), 0)

    # private only: the unknown-access project (public = NA) is NOT private,
    # so it must not be included
    session$setInputs(search_term = "", private_only = TRUE)
    expect_setequal(filtered()$osf_id, c("bbbbb", "ccccc"))

    # the two filters combine
    session$setInputs(search_term = "manylabs", private_only = TRUE)
    expect_equal(filtered()$osf_id, "ccccc")
  })
})

test_that("ticking one row selects only that project", {
  skip_shiny()
  env <- stub_osf(load_app_env("osf_app.R"))

  # This is the behaviour that was broken: with many projects showing, ticking
  # a single row must leave exactly that one project selected.
  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)

    # everything ticked, then one row only
    session$setInputs(project_table_rows_selected = 1:4)
    expect_equal(nrow(selected_projects()), 4)

    session$setInputs(project_table_rows_selected = 2L)
    expect_equal(selected_projects()$osf_id, "bbbbb")
    expect_equal(nrow(selected_projects()), 1)

    # the same after narrowing to private projects: row 1 of that shorter
    # list is the first private project, and nothing else comes with it
    session$setInputs(private_only = TRUE)
    session$setInputs(project_table_rows_selected = 1L)
    expect_equal(selected_projects()$osf_id, "bbbbb")
    expect_equal(nrow(selected_projects()), 1)

    # untick everything
    session$setInputs(project_table_rows_selected = integer(0))
    expect_equal(nrow(selected_projects()), 0)
  })
})

test_that("the selection always refers to the rows currently shown", {
  skip_shiny()
  env <- stub_osf(load_app_env("osf_app.R"))

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)

    # row 2 of the full list is bbbbb
    session$setInputs(project_table_rows_selected = 2L)
    expect_equal(selected_projects()$osf_id, "bbbbb")

    # after narrowing to the ManyLabs projects (aaaaa, ccccc), row 2 is ccccc.
    # What is downloaded is what is ticked in the list as it now stands.
    session$setInputs(search_term = "manylabs")
    expect_equal(selected_projects()$osf_id, "ccccc")

    # a row number left over from a longer list is ignored rather than
    # downloaded as a missing project
    session$setInputs(project_table_rows_selected = c(1L, 9L))
    expect_equal(selected_projects()$osf_id, "aaaaa")
    expect_false(any(is.na(selected_projects()$osf_id)))
  })
})

test_that("select all and clear act on what the filter is showing", {
  skip_shiny()
  env <- stub_osf(load_app_env("osf_app.R"))

  # select_all and clear_all tick and untick rows in the table itself, which
  # testServer cannot observe, so check the row numbers they ask for.
  appdir <- system.file("app", package = "metacheck")
  code <- paste(readLines(file.path(appdir, "osf_app.R")), collapse = "\n")

  # select all asks for every row of the filtered table, not the full listing
  expect_match(code, "seq_len(nrow(tbl))", fixed = TRUE)
  expect_match(code, 'DT::selectRows(DT::dataTableProxy("project_table"), integer(0))',
               fixed = TRUE)

  # and the selection is read from the table rather than from a separate list
  # that is pushed back into it, which is what broke single-row selection
  expect_no_match(code, "chosen_ids", fixed = TRUE)
})

test_that("a new listing clears the previous selection", {
  skip_shiny()
  env <- stub_osf(load_app_env("osf_app.R"))

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1:4)
    expect_equal(nrow(selected_projects()), 4)

    # listing a different user must not carry the old ticks over. The app
    # unticks the table, and the browser would report the empty selection
    # back; testServer has no browser, so check the listing was replaced.
    session$setInputs(osf_id = "aaaaa")
    session$setInputs(find_projects = 2)
    session$setInputs(project_table_rows_selected = integer(0))
    expect_equal(nrow(selected_projects()), 0)
  })
})

test_that("the project list is shown on one page with no paging", {
  skip_shiny()
  # Paging would hide rows that "select all" appears to have selected, so the
  # table is drawn with every project on one scrolling page.
  appdir <- system.file("app", package = "metacheck")
  code <- readLines(file.path(appdir, "osf_app.R"))
  start <- grep("DT::datatable", code)[[1]]
  dt <- paste(code[start:(start + 8)], collapse = " ")
  expect_match(dt, "paging = FALSE", fixed = TRUE)
  expect_no_match(dt, "pageLength", fixed = TRUE)
})

test_that("only the ticked projects are downloaded, into the chosen folder", {
  skip_shiny()
  captured <- new.env()
  env <- stub_osf(load_app_env("osf_app.R"), captured = captured)
  dir <- withr::local_tempdir()

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)

    session$setInputs(
      download_to = dir, dl_mode = "all", metadata = TRUE, unzip = TRUE,
      ignore_folder_structure = FALSE,
      max_file_size = NA, max_download_size = NA)

    # nothing ticked yet
    session$setInputs(start_download = 1)
    expect_match(dl_error(), "No projects are selected")
    expect_null(captured$calls)

    # tick the first and third row
    session$setInputs(project_table_rows_selected = c(1L, 3L))
    expect_setequal(selected_projects()$osf_id, c("aaaaa", "ccccc"))

    session$setInputs(start_download = 2)
    expect_equal(dl_error(), "")
    expect_length(captured$calls, 2)
    expect_setequal(vapply(captured$calls, function(x) x$osf_id, ""),
                    c("aaaaa", "ccccc"))
    for (call in captured$calls) {
      expect_equal(call$download_to, dir)
      expect_equal(call$mode, "all")
      expect_true(call$metadata)
    }
    expect_equal(nrow(dl_result()), 2)
  })
})

test_that("the download folder must exist before anything is fetched", {
  skip_shiny()
  captured <- new.env()
  env <- stub_osf(load_app_env("osf_app.R"), captured = captured)
  dir <- withr::local_tempdir()

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L,
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)

    # empty path
    session$setInputs(download_to = "")
    session$setInputs(start_download = 1)
    expect_match(dl_error(), "Enter a folder")
    expect_null(captured$calls)

    # a path that does not exist
    session$setInputs(download_to = file.path(dir, "not_here"))
    session$setInputs(start_download = 2)
    expect_match(dl_error(), "does not exist")
    expect_null(captured$calls)

    # a real folder works
    session$setInputs(download_to = dir)
    session$setInputs(start_download = 3)
    expect_equal(dl_error(), "")
    expect_length(captured$calls, 1)
  })
})

test_that("size limits are only sent in the modes that list files first", {
  skip_shiny()
  captured <- new.env()
  env <- stub_osf(load_app_env("osf_app.R"), captured = captured)
  dir <- withr::local_tempdir()

  run_mode <- function(session, mode, n) {
    session$setInputs(dl_mode = mode, max_file_size = 5,
                      max_download_size = 50)
    session$setInputs(start_download = n)
    captured$calls[[length(captured$calls)]]
  }

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = dir,
                      metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE)

    # mode "all" takes whole archives with no file listing, so the limits
    # cannot be applied and must not be passed as if they were
    call_all <- run_mode(session, "all", 1)
    expect_null(call_all$max_file_size)
    expect_null(call_all$max_download_size)

    # "select" and "zip" list the files first, so the limits do apply
    call_sel <- run_mode(session, "select", 2)
    expect_equal(call_sel$max_file_size, 5)
    expect_equal(call_sel$max_download_size, 50)

    call_zip <- run_mode(session, "zip", 3)
    expect_equal(call_zip$max_file_size, 5)
    expect_equal(call_zip$max_download_size, 50)

    # whichever mode is chosen must be the mode that is used
    expect_equal(call_all$mode, "all")
    expect_equal(call_sel$mode, "select")
    expect_equal(call_zip$mode, "zip")
  })
})

test_that("every setting reaches osf_file_download", {
  skip_shiny()
  captured <- new.env()
  env <- stub_osf(load_app_env("osf_app.R"), captured = captured)
  dir <- withr::local_tempdir()

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = dir,
                      dl_mode = "zip", metadata = FALSE, unzip = FALSE,
                      ignore_folder_structure = TRUE,
                      max_file_size = 7, max_download_size = 70)
    session$setInputs(start_download = 1)

    call <- captured$calls[[1]]
    expect_equal(call$osf_id, "aaaaa")
    expect_equal(call$download_to, dir)
    expect_equal(call$mode, "zip")
    expect_false(call$metadata)
    expect_false(call$unzip)
    expect_true(call$ignore_folder_structure)
    expect_equal(call$max_file_size, 7)
    expect_equal(call$max_download_size, 70)

    # flip every switch and check each one moves
    session$setInputs(dl_mode = "select", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = 1, max_download_size = 2)
    session$setInputs(start_download = 2)
    call2 <- captured$calls[[2]]
    expect_equal(call2$mode, "select")
    expect_true(call2$metadata)
    expect_true(call2$unzip)
    expect_false(call2$ignore_folder_structure)
    expect_equal(call2$max_file_size, 1)
    expect_equal(call2$max_download_size, 2)
  })
})

# --- hosted (Shiny Server) behaviour -----------------------------------
# Hosted, the browser fetches each archive straight from files.osf.io and the
# server never stores the files. SHINY_PORT is what the app reads to tell the
# two apart, so setting it switches the app into that mode.

test_that("hosted, the browser is sent OSF addresses and the server stores nothing", {
  skip_shiny()
  # Set for this test only. on.exit/local_envvar attach to the wrapped
  # test_that in helper.R rather than to this block, so the variable would
  # leak into the local-route tests below and send them down the wrong branch.
  Sys.setenv(SHINY_PORT = "3838")
  captured <- new.env()
  env <- stub_osf(load_app_env("osf_app.R"), captured = captured)
  # one component per project, as osf_info(recursive = TRUE) reports it
  env$osf_info <- function(id, ...) {
    data.frame(osf_id = id, name = "One project", osf_type = "nodes",
               category = "project", public = TRUE)
  }

  sent <- new.env()
  shiny::testServer(env$server, {
    session$sendCustomMessage <- function(type, message) {
      sent$type <- type
      sent$message <- message
    }
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = c(1L, 2L),
                      download_to = "", dl_mode = "all", metadata = TRUE,
                      unzip = TRUE, ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)

    # the server must NOT download anything itself
    expect_null(captured$calls)
    # and it must not demand a folder on the server
    expect_equal(dl_error(), "")

    expect_equal(sent$type, "osfDirectDownload")
    items <- sent$message$items
    expect_length(items, 2)
    for (it in items) {
      # exactly the address verified to serve a zip with cross-origin access
      expect_match(it$url,
                   "^https://files\\.osf\\.io/v1/resources/[a-z0-9]+/providers/osfstorage/\\?zip=$")
      expect_match(it$filename, "\\.zip$")
    }
    expect_setequal(vapply(items, function(x) x$id, ""), c("aaaaa", "bbbbb"))
  })
})

test_that("hosted, the token is passed to the browser for private projects", {
  skip_shiny()
  # Set for this test only. on.exit/local_envvar attach to the wrapped
  # test_that in helper.R rather than to this block, so the variable would
  # leak into the local-route tests below and send them down the wrong branch.
  Sys.setenv(SHINY_PORT = "3838")
  env <- stub_osf(load_app_env("osf_app.R"))
  env$osf_info <- function(id, ...) {
    data.frame(osf_id = id, name = "p", osf_type = "nodes",
               category = "project", public = FALSE)
  }
  # the app asks osf_pat() for the token it hands over
  env$osf_pat <- function(pat = NULL) if (is.null(pat)) "secret-token" else invisible(pat)

  sent <- new.env()
  shiny::testServer(env$server, {
    session$sendCustomMessage <- function(type, message) sent$message <- message
    session$setInputs(osf_id = "4i578", search_term = "", private_only = TRUE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = "",
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)
    expect_equal(sent$message$token, "secret-token")
  })
})

test_that("hosted, progress reported by the browser is shown and errors surface", {
  skip_shiny()
  # Set for this test only. on.exit/local_envvar attach to the wrapped
  # test_that in helper.R rather than to this block, so the variable would
  # leak into the local-route tests below and send them down the wrong branch.
  Sys.setenv(SHINY_PORT = "3838")
  env <- stub_osf(load_app_env("osf_app.R"))
  env$osf_info <- function(id, ...) {
    data.frame(osf_id = id, name = "p", osf_type = "nodes",
               category = "project", public = TRUE)
  }

  shiny::testServer(env$server, {
    session$sendCustomMessage <- function(type, message) NULL
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = "",
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)
    expect_true(dl_running())

    # bytes arriving, with no total (the OSF sends no length for these)
    session$setInputs(direct_progress = list(
      state = "start", id = "aaaaa", message = "Project", received = 2097152,
      total = 0, at = 1))
    expect_match(direct_status(), "2.0 MB so far")

    # finished: the app stops reporting a download in progress
    session$setInputs(direct_progress = list(
      state = "done", id = "aaaaa", message = "", at = 2))
    expect_false(dl_running())
    expect_equal(direct_done(), "aaaaa")

    # a failure in the browser is reported to the user, not swallowed
    session$setInputs(direct_progress = list(
      state = "error", id = "aaaaa",
      message = "the OSF answered 403 (not authorised)", at = 3))
    expect_match(dl_error(), "403")
    expect_false(dl_running())
  })
})

test_that("hosted, a multi-component project becomes one archive per component", {
  skip_shiny()
  # Set for this test only. on.exit/local_envvar attach to the wrapped
  # test_that in helper.R rather than to this block, so the variable would
  # leak into the local-route tests below and send them down the wrong branch.
  Sys.setenv(SHINY_PORT = "3838")
  env <- stub_osf(load_app_env("osf_app.R"))
  # a project with two components besides itself
  env$osf_info <- function(id, ...) {
    data.frame(osf_id = c(id, "comp1", "comp2"),
               name = c("root", "c1", "c2"),
               osf_type = c("nodes", "nodes", "nodes"),
               category = "project", public = TRUE)
  }

  sent <- new.env()
  shiny::testServer(env$server, {
    session$sendCustomMessage <- function(type, message) sent$message <- message
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = "",
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)

    ids <- vapply(sent$message$items, function(x) x$id, "")
    expect_setequal(ids, c("aaaaa", "comp1", "comp2"))
    # each gets its own address and its own file name
    expect_equal(length(unique(vapply(sent$message$items, function(x) x$url, ""))), 3)
  })
})

test_that("the browser code always sends an Authorization header", {
  skip_shiny()
  # The OSF returns Access-Control-Allow-Origin ONLY on requests carrying an
  # Authorization header, and a browser discards a cross-origin response
  # without it. Verified against the live API on 2026-08-14: no header gives
  # a 200 with allow-origin absent, "Bearer none" gives 200 with the header,
  # and "Bearer " (empty) gives 400. So the header must always be sent, with
  # a non-empty placeholder when the visitor has no token.
  appdir <- system.file("app", package = "metacheck")
  js <- paste(readLines(file.path(appdir, "www", "osf_direct.js"), warn = FALSE),
              collapse = "\n")
  expect_match(js, "'Authorization'", fixed = TRUE)
  expect_match(js, "'none'", fixed = TRUE)
  # not written as a conditional that would omit the header without a token
  expect_no_match(js, "if (token) headers['Authorization']", fixed = TRUE)
})

test_that("a folder pasted from Windows Explorer is accepted", {
  skip_shiny()
  # The hosted tests above set SHINY_PORT, and it is read once when the app
  # environment is created, so clear it before loading for the local route.
  Sys.unsetenv("SHINY_PORT")
  captured <- new.env()
  env <- stub_osf(load_app_env("osf_app.R"), captured = captured)
  dir <- file.path(tempdir(), "osf_paste_test")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, dl_mode = "all",
                      metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)

    # Explorer's "Copy as path" wraps the path in quotation marks, which
    # dir.exists() does not recognise
    session$setInputs(download_to = paste0('"', dir, '"'))
    session$setInputs(start_download = 1)
    expect_equal(dl_error(), "")
    expect_length(captured$calls, 1)
    # the quotation marks are stripped before the path is used
    expect_equal(captured$calls[[1]]$download_to, dir)

    # backslashes, as Windows writes them, work too
    session$setInputs(download_to = gsub("/", "\\", dir, fixed = TRUE))
    session$setInputs(start_download = 2)
    expect_equal(dl_error(), "")
    expect_length(captured$calls, 2)
  })
})

test_that("empty size boxes are passed as no limit, not as NA", {
  skip_shiny()
  Sys.unsetenv("SHINY_PORT")   # local route: see the note above
  captured <- new.env()
  env <- stub_osf(load_app_env("osf_app.R"), captured = captured)
  dir <- withr::local_tempdir()

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    # "select" applies the limits, but the boxes are empty
    session$setInputs(project_table_rows_selected = 1L, download_to = dir,
                      dl_mode = "select", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)

    call <- captured$calls[[1]]
    expect_null(call$max_file_size)
    expect_null(call$max_download_size)
  })
})

test_that("the first token typed reaches the download", {
  skip_shiny()
  Sys.unsetenv("SHINY_PORT")
  # The token used to be stored by an observeEvent with ignoreInit = TRUE,
  # which swallowed the FIRST value typed, so a private project always failed
  # with a permission refusal even though a token had been entered.
  env <- load_app_env("osf_app.R")
  stored <- ""
  set_count <- 0
  env$osf_pat <- function(pat = NULL) {
    if (is.null(pat)) return(stored)
    stored <<- pat
    set_count <<- set_count + 1
    invisible(pat)
  }
  env$osf_type <- function(id, ...) "users"
  env$osf_user_projects <- function(id, ...) data.frame(
    osf_id = "ppppp", name = "Private one", category = "project",
    public = FALSE, osf_url = "https://osf.io/ppppp")
  seen <- list()
  env$osf_file_download <- function(osf_id, ...) {
    seen[[length(seen) + 1]] <<- env$osf_pat()
    data.frame(osf_id = osf_id, downloaded = TRUE, download_path = "x")
  }

  dir <- file.path(tempdir(), "osf_token_test")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(env$server, {
    # typed once, never edited again and no "Check token" press
    session$setInputs(osf_pat = "first-token-typed")
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = dir,
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)

    # the private project is NOT refused, because a token was given
    expect_equal(dl_error(), "")
    expect_length(seen, 1)
    expect_equal(seen[[1]], "first-token-typed")
    expect_gt(set_count, 0)
  })
})

test_that("a private project without a token is refused before downloading", {
  skip_shiny()
  Sys.unsetenv("SHINY_PORT")
  env <- load_app_env("osf_app.R")
  env$osf_pat <- function(pat = NULL) if (is.null(pat)) "" else invisible(pat)
  env$osf_type <- function(id, ...) "users"
  env$osf_user_projects <- function(id, ...) data.frame(
    osf_id = "ppppp", name = "Private one", category = "project",
    public = FALSE, osf_url = "https://osf.io/ppppp")
  called <- 0
  env$osf_file_download <- function(...) { called <<- called + 1; NULL }

  dir <- file.path(tempdir(), "osf_notoken_test")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = dir,
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)

    # the warning appears before the download is even attempted
    ui <- as.character(output$selection_ui$html %||% output$selection_ui)
    expect_match(ui, "private", ignore.case = TRUE)

    session$setInputs(start_download = 1)
    # refused outright rather than retried with backoff until it gives up
    expect_equal(called, 0)
    expect_match(dl_error(), "private")
    expect_match(dl_error(), "no OSF token is set")
  })
})

test_that("a download that fetched nothing is reported as a failure", {
  skip_shiny()
  Sys.unsetenv("SHINY_PORT")
  env <- load_app_env("osf_app.R")
  env$osf_pat <- function(pat = NULL) if (is.null(pat)) "tok" else invisible(pat)
  env$osf_type <- function(id, ...) "users"
  env$osf_user_projects <- function(id, ...) data.frame(
    osf_id = "aaaaa", name = "A", category = "project",
    public = TRUE, osf_url = "u")
  # osf_file_download() reports a refusal in the table, not by raising an error
  env$osf_file_download <- function(osf_id, ...) data.frame(
    osf_id = osf_id, downloaded = FALSE, download_path = NA_character_)

  dir <- file.path(tempdir(), "osf_fail_test")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = dir,
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)

    # the rendered HTML wraps lines, so flatten it before matching
    status <- as.character(output$download_status_ui$html %||%
                             output$download_status_ui) |>
      gsub("\\s+", " ", x = _)
    expect_match(status, "None of the 1 selected project could be downloaded")
    expect_no_match(status, "Downloaded 1 item", fixed = TRUE)
  })
})

# --- anonymous usage statistics ----------------------------------------
# Kept to justify further development in grant applications. These tests pin
# down that they stay anonymous: a row says something happened, never who did
# it, and nothing identifying may reach the file.

test_that("nothing is recorded when the app runs locally", {
  skip_shiny()
  Sys.unsetenv("SHINY_PORT")   # local: development and testing
  env <- stub_osf(load_app_env("osf_app.R"))
  stats_dir <- file.path(tempdir(), "usage_local_test")
  dir.create(stats_dir, showWarnings = FALSE)
  on.exit(unlink(stats_dir, recursive = TRUE), add = TRUE)
  env$usage_file <- function() file.path(stats_dir, "usage.csv")
  env$osf_file_download <- function(osf_id, ...) data.frame(
    osf_id = osf_id, osf_project = osf_id, downloaded = TRUE,
    bytes = 2e6, download_path = "x")

  dir <- file.path(tempdir(), "usage_local_dl")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = dir,
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)
    expect_equal(nrow(dl_result()), 1)   # the download still happened
  })

  # no file, and therefore no counts
  expect_false(file.exists(env$usage_file()))
  expect_equal(nrow(env$usage_summary()), 0)
})

test_that("a session and a download are counted on a Shiny server", {
  skip_shiny()
  Sys.setenv(SHINY_PORT = "3838")
  env <- stub_osf(load_app_env("osf_app.R"))
  # write somewhere temporary rather than the real data directory
  stats_dir <- file.path(tempdir(), "usage_count_test")
  dir.create(stats_dir, showWarnings = FALSE)
  on.exit(unlink(stats_dir, recursive = TRUE), add = TRUE)
  env$usage_file <- function() file.path(stats_dir, "usage.csv")
  env$osf_info <- function(id, ...) data.frame(
    osf_id = id, name = "p", osf_type = "nodes",
    category = "project", public = TRUE)

  shiny::testServer(env$server, {
    session$sendCustomMessage <- function(type, message) NULL
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = c(1L, 2L), download_to = "",
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)
    # the browser reports each archive as it finishes, with its size
    session$setInputs(direct_progress = list(
      state = "done", id = "aaaaa", message = "", received = 2e6, at = 1))
    session$setInputs(direct_progress = list(
      state = "done", id = "bbbbb", message = "", received = 1e6, at = 2))
  })

  s <- env$usage_summary()
  expect_equal(nrow(s), 1)
  expect_equal(s$sessions, 1L)
  expect_equal(s$downloads, 2L)   # one row per finished archive
  expect_equal(s$projects, 2L)
  expect_gt(s$gb, 0)
  Sys.unsetenv("SHINY_PORT")
})

test_that("nothing identifying is written to the statistics", {
  skip_shiny()
  Sys.setenv(SHINY_PORT = "3838")   # only the hosted app records anything
  env <- stub_osf(load_app_env("osf_app.R"), pat = "secret-token-value")
  stats_dir <- file.path(tempdir(), "usage_anon_test")
  dir.create(stats_dir, showWarnings = FALSE)
  on.exit(unlink(stats_dir, recursive = TRUE), add = TRUE)
  env$usage_file <- function() file.path(stats_dir, "usage.csv")
  env$osf_info <- function(id, ...) data.frame(
    osf_id = id, name = "p", osf_type = "nodes",
    category = "project", public = TRUE)

  shiny::testServer(env$server, {
    session$sendCustomMessage <- function(type, message) NULL
    session$setInputs(osf_pat = "secret-token-value")
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = "",
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)
    session$setInputs(direct_progress = list(
      state = "done", id = "aaaaa", message = "", received = 1e6, at = 1))
  })

  txt <- paste(readLines(env$usage_file(), warn = FALSE), collapse = " ")
  # the token, the OSF IDs, and the account name must all be absent
  expect_no_match(txt, "secret-token-value", fixed = TRUE)
  expect_no_match(txt, "aaaaa", fixed = TRUE)   # the project that was downloaded
  expect_no_match(txt, "4i578", fixed = TRUE)   # the account that was listed
  expect_no_match(txt, Sys.info()[["user"]], fixed = TRUE)

  # only these four columns exist, so nothing else can creep in unnoticed
  x <- utils::read.csv(env$usage_file())
  expect_setequal(names(x), c("date", "event", "projects", "bytes"))
  # a date, not a timestamp: two rows on the same day are indistinguishable
  expect_false(any(grepl(":", x$date)))
  Sys.unsetenv("SHINY_PORT")
})

test_that("recording usage never interrupts the app", {
  skip_shiny()
  Sys.setenv(SHINY_PORT = "3838")   # so recording is actually attempted
  on.exit(Sys.unsetenv("SHINY_PORT"), add = TRUE)
  env <- stub_osf(load_app_env("osf_app.R"))
  # a path that cannot be written to
  env$usage_file <- function() file.path(tempdir(), "no_such_dir", "x", "u.csv")
  env$osf_info <- function(id, ...) data.frame(
    osf_id = id, name = "p", osf_type = "nodes",
    category = "project", public = TRUE)

  sent <- new.env()
  shiny::testServer(env$server, {
    session$sendCustomMessage <- function(type, message) sent$message <- message
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = "",
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)
    session$setInputs(direct_progress = list(
      state = "done", id = "aaaaa", message = "", received = 1e6, at = 1))

    # the download still went ahead even though the statistics could not be kept
    expect_equal(dl_error(), "")
    expect_length(sent$message$items, 1)
    expect_equal(direct_done(), "aaaaa")
  })
})

test_that("the code says the statistics are anonymous and why they are kept", {
  skip_shiny()
  # The comment is the thing that stops someone quietly adding an identifier
  # later, so check it is actually there.
  appdir <- system.file("app", package = "metacheck")
  code <- paste(readLines(file.path(appdir, "osf_app.R"), warn = FALSE),
                collapse = " ")
  expect_match(code, "ANONYMOUS USAGE STATISTICS", fixed = TRUE)
  expect_match(code, "grant applications", fixed = TRUE)
  expect_match(code, "NO identifier", fixed = TRUE)
})

test_that("a folder that cannot be written to is refused before downloading", {
  skip_shiny()
  Sys.unsetenv("SHINY_PORT")
  captured <- new.env()
  env <- stub_osf(load_app_env("osf_app.R"), captured = captured)
  env$usage_file <- function(...) ""

  # A folder that EXISTS but refuses new files, which is the case that matters:
  # a path that does not exist is caught by an earlier check, so it would not
  # exercise this one. C:/ is exactly what a user hit in practice.
  candidates <- c("C:/", "C:/Windows/System32", "/proc", "/sys")
  readonly <- NULL
  for (d in candidates) {
    if (!dir.exists(d)) next
    f <- file.path(d, paste0(".metacheck_probe_", Sys.getpid()))
    ok <- tryCatch(file.create(f, showWarnings = FALSE),
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(ok)) { unlink(f); next }
    readonly <- d
    break
  }
  skip_if(is.null(readonly), "no read-only directory available to test with")

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L,
                      download_to = readonly,
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)

    # the problem is visible before the download is started
    ui <- gsub("<[^>]*>", " ", as.character(output$folder_check_ui$html %||%
                                              output$folder_check_ui))
    expect_match(ui, "cannot be written")

    session$setInputs(start_download = 1)
    # nothing was fetched, and the message is about saving, not downloading
    expect_null(captured$calls)
    expect_match(dl_error(), "cannot be written to")
    expect_match(dl_error(), "administrator rights")
  })
})

test_that("an unwritable folder is reported as the path is typed", {
  skip_shiny()
  Sys.unsetenv("SHINY_PORT")
  env <- stub_osf(load_app_env("osf_app.R"))
  env$usage_file <- function(...) ""
  good <- file.path(tempdir(), "writable_check")
  dir.create(good, showWarnings = FALSE)
  on.exit(unlink(good, recursive = TRUE), add = TRUE)

  flat <- function(x) {
    gsub("\\s+", " ", gsub("<[^>]*>", " ", as.character(x)))
  }

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)

    # nothing typed yet: nothing to say
    session$setInputs(download_to = "")
    expect_null(output$folder_check_ui)

    # a folder that does not exist
    session$setInputs(download_to = file.path(good, "nope"))
    expect_match(flat(output$folder_check_ui$html %||% output$folder_check_ui),
                 "does not exist")

    # a folder that can be written to
    session$setInputs(download_to = good)
    expect_match(flat(output$folder_check_ui$html %||% output$folder_check_ui),
                 "can be saved here")
  })
})

test_that("dir_writable tells a writable folder from one that is not", {
  skip_shiny()
  env <- load_app_env("osf_app.R")
  good <- file.path(tempdir(), "dw_good")
  dir.create(good, showWarnings = FALSE)
  on.exit(unlink(good, recursive = TRUE), add = TRUE)
  notdir <- tempfile(); writeLines("x", notdir)
  on.exit(unlink(notdir), add = TRUE)

  expect_true(env$dir_writable(good))
  expect_false(env$dir_writable(file.path(notdir, "sub")))  # parent is a file
  expect_false(env$dir_writable(file.path(good, "missing")))
  expect_false(env$dir_writable(""))
  expect_false(env$dir_writable(notdir))   # a file, not a directory

  # a folder that exists but refuses new files, which is the case that a
  # dir.exists() check alone would miss
  ro <- Filter(function(d) {
    if (!dir.exists(d)) return(FALSE)
    f <- file.path(d, paste0(".probe_", Sys.getpid()))
    ok <- tryCatch(file.create(f, showWarnings = FALSE),
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(ok)) unlink(f)
    !isTRUE(ok)
  }, c("C:/", "C:/Windows/System32", "/proc", "/sys"))
  if (length(ro) > 0) {
    expect_true(dir.exists(ro[[1]]))          # it exists ...
    expect_false(env$dir_writable(ro[[1]]))   # ... but cannot be written to
  }
  # the test file it creates is removed again
  expect_length(list.files(good, all.files = TRUE, no.. = TRUE), 0)
})

test_that("a download failure explains itself rather than failing silently", {
  skip_shiny()
  Sys.unsetenv("SHINY_PORT")
  env <- stub_osf(load_app_env("osf_app.R"))
  env$usage_file <- function(...) ""
  # the package explains a failure in a warning, which only reaches the console
  env$osf_file_download <- function(osf_id, ...) {
    warning("aaaaa is private and its files could not be downloaded without ",
            "an authorised OSF token.", call. = FALSE)
    data.frame(osf_id = osf_id, osf_project = osf_id, downloaded = FALSE,
               bytes = 0, download_path = NA_character_)
  }
  dir <- file.path(tempdir(), "explain_fail")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = dir,
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)

    status <- gsub("\\s+", " ", gsub("<[^>]*>", " ",
      as.character(output$download_status_ui$html %||%
                     output$download_status_ui)))
    # says the saving was fine, so the problem is at the OSF end
    expect_match(status, "could be saved to your folder")
    # and repeats what the download itself said
    expect_match(status, "authorised OSF token")
  })
})

test_that("a finished download says how to check what was downloaded", {
  skip_shiny()
  Sys.unsetenv("SHINY_PORT")
  env <- stub_osf(load_app_env("osf_app.R"))
  env$usage_file <- function(...) ""
  dir <- file.path(tempdir(), "advice_local")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # a Windows-style path, to check it is shown in a form R can parse
  saved <- file.path(dir, "aaaaa", "Project_aaaaa")
  env$osf_file_download <- function(osf_id, ...) data.frame(
    osf_id = osf_id, osf_project = osf_id, downloaded = TRUE,
    bytes = 1e6, download_path = saved)

  shiny::testServer(env$server, {
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = dir,
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)

    status <- gsub("\\s+", " ", gsub("<[^>]*>", " ",
      as.character(output$download_status_ui$html %||%
                     output$download_status_ui)))
    expect_match(status, "report_repository(", fixed = TRUE)
    # downloaded locally the files are ALREADY unzipped, so it must not tell
    # people to unzip anything
    expect_no_match(status, "Unzip", fixed = TRUE)
    # the path is shown with forward slashes so the line can be pasted into R
    expect_match(status, gsub("\\\\", "/", saved), fixed = TRUE)
    # the href survives only in the raw HTML, which the flattening above strips
    raw <- as.character(output$download_status_ui$html %||% output$download_status_ui)
    expect_match(raw, "metacheck_book/", fixed = TRUE)
  })
})

test_that("a hosted download says to unzip before checking", {
  skip_shiny()
  Sys.setenv(SHINY_PORT = "3838")
  on.exit(Sys.unsetenv("SHINY_PORT"), add = TRUE)
  env <- stub_osf(load_app_env("osf_app.R"))
  env$usage_file <- function(...) ""
  env$osf_info <- function(id, ...) data.frame(
    osf_id = id, name = "p", osf_type = "nodes",
    category = "project", public = TRUE)

  shiny::testServer(env$server, {
    session$sendCustomMessage <- function(type, message) NULL
    session$setInputs(osf_id = "4i578", search_term = "", private_only = FALSE)
    session$setInputs(find_projects = 1)
    session$setInputs(project_table_rows_selected = 1L, download_to = "",
                      dl_mode = "all", metadata = TRUE, unzip = TRUE,
                      ignore_folder_structure = FALSE,
                      max_file_size = NA, max_download_size = NA)
    session$setInputs(start_download = 1)
    session$setInputs(direct_progress = list(
      state = "done", id = "aaaaa", message = "", received = 1e6, at = 1))

    status <- gsub("\\s+", " ", gsub("<[^>]*>", " ",
      as.character(output$download_status_ui$html %||%
                     output$download_status_ui)))
    # the browser saved a zip, so unzipping comes first
    expect_match(status, "zip archive")
    expect_match(status, "Unzip", fixed = TRUE)
    expect_match(status, "report_repository(", fixed = TRUE)
    # the href survives only in the raw HTML, which the flattening above strips
    raw <- as.character(output$download_status_ui$html %||% output$download_status_ui)
    expect_match(raw, "metacheck_book/", fixed = TRUE)
  })
})

test_that("the folder is typed rather than picked from a browser", {
  skip_shiny()
  # A native dialogue blocks the R process and would open on the server when
  # hosted; the in-page browsers are worse to use than pasting a path.
  appdir <- system.file("app", package = "metacheck")
  # comments explain WHY there is no browser, so check the code only
  lines <- readLines(file.path(appdir, "osf_app.R"), warn = FALSE)
  code <- paste(grep("^\\s*#", lines, invert = TRUE, value = TRUE),
                collapse = "\n")
  expect_no_match(code, "shinyFiles", fixed = TRUE)
  expect_no_match(code, "choose.dir", fixed = TRUE)
  expect_no_match(code, "browse_folder", fixed = TRUE)

  # shinyFiles must not be a declared dependency any more
  expect_false("shinyFiles" %in% names(utils::packageDescription("metacheck")))
  sug <- utils::packageDescription("metacheck")$Suggests %||% ""
  expect_no_match(sug, "shinyFiles", fixed = TRUE)
})

test_that("the download folder starts empty, not at the app's own directory", {
  skip_shiny()
  # getwd() inside a Shiny app is the folder the app file lives in, which is
  # never a sensible place to save downloads, so the box starts blank.
  appdir <- system.file("app", package = "metacheck")
  app <- paste(readLines(file.path(appdir, "osf_app.R"), warn = FALSE),
               collapse = "\n")
  # (?s) so the pattern spans the lines of the call
  block <- regmatches(app, regexpr(
    '(?s)textInput\\(\\s*"download_to".*?width = "100%"\\s*\\)',
    app, perl = TRUE))
  expect_length(block, 1)
  expect_no_match(block, "getwd()", fixed = TRUE)
  expect_match(block, 'value = ""', fixed = TRUE)
  expect_match(block, "C:/some_folder_on_your_computer", fixed = TRUE)
})

test_that("the app links to the manual chapter instead of showing R code", {
  skip_shiny()
  appdir <- system.file("app", package = "metacheck")
  ui <- paste(readLines(file.path(appdir, "tabs", "osf_download.R"), warn = FALSE),
              collapse = " ")

  # the chapter that explains doing this in R, including archiving on Zenodo
  expect_match(ui, "metacheck_book/", fixed = TRUE)
  expect_match(ui, "chapters/archiving-osf-to-zenodo.html", fixed = TRUE)
  expect_match(ui, 'target = "_blank"', fixed = TRUE)

  # the generated code block is gone from both the tab and the server
  expect_no_match(ui, "r_code", fixed = TRUE)
  app <- paste(readLines(file.path(appdir, "osf_app.R"), warn = FALSE),
               collapse = " ")
  expect_no_match(app, "output$r_code", fixed = TRUE)
})

test_that("the token is checked without being written anywhere", {
  skip_shiny()
  env <- stub_osf(load_app_env("osf_app.R"))

  shiny::testServer(env$server, {
    # the token box is a password input, so it is never echoed back
    session$setInputs(osf_pat = "good-token")
    session$setInputs(pat_check = 1)
    expect_true(TRUE)  # reaching here means no error was raised

    session$setInputs(osf_pat = "bad-token")
    session$setInputs(pat_check = 2)
    expect_true(TRUE)
  })

  # the token must be entered as a password field, not a plain text box
  appdir <- system.file("app", package = "metacheck")
  ui <- readLines(file.path(appdir, "tabs", "osf_list.R"))
  expect_true(any(grepl('passwordInput\\("osf_pat"', ui)))
  expect_false(any(grepl('textInput\\("osf_pat"', ui)))
})
