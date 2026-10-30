test_that(".onLoad", {
  op.defaults <- c(
    metacheck.verbose = TRUE,
    metacheck.llm_max_calls = 30L,
    metacheck.llm.use = FALSE,
    metacheck.llm.model = "groq",
    metacheck.osf.delay = 0,
    metacheck.osf.api = "https://api.osf.io/v2",
    metacheck.osf.api.calls = 0
  )

  # op.current <- names(op.defaults) |> sapply(getOption)
  names(op.defaults) |> sapply(\(o) options(setNames(list(NULL), o)))
  op.null <- names(op.defaults) |> sapply(getOption)
  expect_true(sapply(op.null, is.null) |> all())

  metacheck:::.onLoad()
  op.reset <- names(op.defaults) |> sapply(getOption)
  expect_false(sapply(op.reset, is.null) |> any())
  expect_equal(op.reset, op.defaults)
})

test_that(".onAttach", {
  op <- capture_message(metacheck:::.onAttach())
  expect_true(grepl("Welcome to metacheck", op))
  expect_true(grepl("This is beta software", op))
})


test_that("llm_use", {
  withr::defer(llm_use(FALSE))
  expect_true(is.function(metacheck::llm_use))
  expect_no_error(helplist <- help(llm_use, metacheck))

  expect_error(llm_use("G"))
  expect_invisible(llm_use(TRUE))
  expect_visible(llm_use())

  expect_equal(llm_use(FALSE), FALSE)
  expect_equal(llm_use(), FALSE)
  expect_equal(llm_use(TRUE), TRUE)
  expect_equal(getOption("metacheck.llm.use"), TRUE)
  expect_equal(llm_use(0), FALSE)
  expect_equal(llm_use("FALSE"), FALSE)

  # llm_use() only true if online & API
  llm_use(1)
  expect_equal(getOption("metacheck.llm.use"), TRUE)
  llm_use("TRUE")
  expect_equal(getOption("metacheck.llm.use"), TRUE)
})

test_that("email", {
  orig <- email()
  e <- "debruine@gmail.com"
  expect_invisible(email(email = e))
  expect_error(email("email"))
  expect_equal(email(), e)
  expect_equal(email(email = e), e)
  expect_equal(email(), e)
  expect_visible(email())
  email(orig)
})


test_that(".batch_query", {
  expect_true(is.function(metacheck:::.batch_query))

  expect_error(.batch_query())

  urls <- c()
  obs <- .batch_query(urls)
  exp <- list()
  expect_equal(obs, exp)

  urls <- "notawebsite"
  expect_warning(obs <- .batch_query(urls), "notawebsite")

  # skip_api("httpbin.org")

  urls <- "https://httpbin.org/get"
  obs <- .batch_query(urls)
  expect_equal(length(obs), 1)

  urls <- c("https://httpbin.org/status/429",
            "https://httpbin.org/status/404",
            "https://httpbin.org/status/200")
  batch_size <- 2
  msg <- "X"
  delay = 0
  obs <- .batch_query(urls, batch_size, msg, delay)
  expect_equal(length(obs), 3)
  expect_equal(obs[[1]]$status_code, 429)
  expect_equal(obs[[2]]$status_code, 404)
  expect_equal(obs[[3]]$status_code, 200)
}, "mock")

test_that(".batch_query sets a per-request timeout, default and overridden", {
  # Regression test for a real production hang (confirmed live 2026-09-07,
  # Cooper corpus rerun): req_perform_sequential() has no timeout of its own,
  # so a connection the remote accepts but then stalls on (no error, no
  # close, just silence) blocks forever. This is the shared query helper
  # behind every repository/API integration in the package (OSF, Dataverse,
  # Dryad, Figshare, Zenodo, Crossref, ...) -- the actual hang was
  # zenodo_info()'s call here, stuck over an hour on one Zenodo record lookup.
  captured <- NULL
  local_mocked_bindings(
    req_perform_sequential = function(reqs, ...) {
      captured <<- reqs
      stop("stop before any real request -- only inspecting req$options")
    },
    .package = "httr2"
  )

  tryCatch(.batch_query("https://example.org/a"), error = function(e) NULL)
  expect_equal(captured[[1]]$options$timeout_ms, 60000)  # 60s default

  tryCatch(.batch_query("https://example.org/a", timeout_s = 5),
          error = function(e) NULL)
  expect_equal(captured[[1]]$options$timeout_ms, 5000)
})


test_that(".batch_query normalizes a connection-level failure to NULL", {
  # Regression test for a real crash (confirmed live 2026-09-16, Cooper
  # corpus rerun / 4TU DOI investigation): req_error(is_error = \(resp) FALSE)
  # only suppresses errors on a real HTTP status, not a connection-level
  # failure (timeout, DNS, dropped connection). req_perform_sequential's
  # on_error = "continue" still returns something for that slot, but an
  # httr2_failure condition object rather than an httr2_response -- and
  # every archive-*.R caller (dryad_info, figshare_info, dataverse_info,
  # mendeley_info, dataone_info, gitlab_info, reshare_info, ...) checks only
  # is.null(resp) before calling httr2::resp_status(resp), which errors
  # ("resp must be an HTTP response object") on an httr2_failure. This
  # normalizes any non-response result to NULL here, once, so every
  # existing is.null(resp) check downstream already does the right thing.
  failure <- structure(
    list(message = "Failed to connect"),
    class = c("httr2_failure", "error", "condition")
  )
  local_mocked_bindings(
    req_perform_sequential = function(reqs, ...) list(failure),
    .package = "httr2"
  )

  obs <- .batch_query("https://example.org/a")
  expect_equal(obs, list(NULL))
})


test_that(".batch_query waits between batches but not after the last one", local({
  # Regression test (2026-10-13 survey): the courtesy delay ran after EVERY
  # batch, including the last, so a single-URL lookup -- what every *_info()
  # record query sends -- paid a flat extra `delay` with no request after it
  # to pace. Sys.sleep() is in base R and cannot be mocked, so this is timed:
  # requests are answered instantly by the mock, leaving the delay as the
  # only time spent. The bounds are wide so a slow machine cannot fail it.
  n_calls <- 0
  local_mocked_bindings(
    req_perform_sequential = function(reqs, ...) {
      n_calls <<- n_calls + 1
      lapply(reqs, \(r) httr2::response(status_code = 200))
    },
    .package = "httr2"
  )

  # one URL, one batch: no sleep at all
  t1 <- system.time(obs <- .batch_query("https://example.org/a", msg = NULL,
                                        delay = 2))[["elapsed"]]
  expect_length(obs, 1)
  expect_lt(t1, 1.5)

  # three URLs in batches of two: one sleep, between batch 1 and batch 2
  t2 <- system.time(obs <- .batch_query(
    paste0("https://example.org/", 1:3), batch_size = 2, msg = NULL,
    delay = 1))[["elapsed"]]
  expect_length(obs, 3)
  expect_gte(t2, 0.9)
  expect_lt(t2, 1.9)
  expect_equal(n_calls, 3)   # 1 batch, then 2 batches
}))


test_that("path_sanitize", {
  expect_true(is.function(metacheck::path_sanitize))
  expect_no_error(helplist <- help(path_sanitize, metacheck))

  expect_error(path_sanitize())

  # defaults
  path <- " has/ spaces/\\backslashes/><|?chars/.dot.is.ok "
  obs <- path_sanitize(path)
  exp <- "has/_spaces/_backslashes/_chars/.dot.is.ok"
  expect_equal(exp, obs)

  # replacement
  obs <- path_sanitize(path, replacement = "~")
  exp <- "has/~spaces/~backslashes/~chars/.dot.is.ok"
  expect_equal(exp, obs)

  # remove_whitespace
  obs <- path_sanitize(path, remove_whitespace = FALSE)
  exp <- "has/ spaces/_backslashes/_chars/.dot.is.ok"
  expect_equal(exp, obs)

  # keep_sep
  obs <- path_sanitize(path, keep_sep = FALSE)
  exp <- "has_spaces_backslashes_chars_.dot.is.ok"
  expect_equal(exp, obs)
})

test_that(".portable_path changes names some operating system cannot write (#557)", {
  # characters not allowed in a Windows name, and control characters
  expect_equal(.portable_path("Title: a subtitle/code*1?.R"), "Title_ a subtitle/code_1_.R")
  expect_equal(.portable_path('a<b>c"d|e.csv'), "a_b_c_d_e.csv")
  expect_equal(.portable_path("Icon\r"), "Icon_")
  # Windows drops a final dot or space, so "x " and "x" would be different folders elsewhere
  expect_equal(.portable_path("day_files /Rabdeau_et_al./a.csv"), "day_files/Rabdeau_et_al/a.csv")
  # reserved names, in any case and with any extension
  expect_equal(.portable_path(c("CON", "data/nul.txt", "lpt1.csv")), c("_CON", "data/_nul.txt", "_lpt1.csv"))
  # a backslash separates folders on Windows only
  expect_equal(.portable_path("a\\b.csv"), "a/b.csv")
  # names that are fine stay as they are
  expect_equal(.portable_path(c("data/a b-c_d.e.csv", "console.R", NA)), c("data/a b-c_d.e.csv", "console.R", NA))
  # a name of only dots or spaces keeps a name; "." and ".." are left to the caller
  expect_equal(.portable_path(c("a/.../b", " /b", "../b")), c("a/_/b", "_/b", "../b"))
})

test_that(".safe_write_path makes each part portable but keeps a drive (#557)", {
  expect_equal(.safe_write_path("C:/x/a:b.txt"), "C:/x/a_b.txt")
  expect_equal(.safe_write_path("/x/y: z/a.txt"), "/x/y_ z/a.txt")
  expect_equal(.safe_write_path("x/a.txt"), "x/a.txt")
})

test_that(".zip_member_target makes a member's name portable and still refuses leaving the folder (#557)", {
  expect_equal(.zip_member_target("File S5/ Almont*1.jpg", "dest"), "dest/File S5/ Almont_1.jpg")
  expect_true(is.na(.zip_member_target("../x.csv", "dest")))
})
