test_that("osf_links", {
  expect_true(is.function(metacheck::osf_links))
  expect_no_error(helplist <- help(osf_links, metacheck))

  exp <- c("osf.io/e2aks", "osf.io/tvyxz/", "osf.com/nope")
  paper <- test_paper(url = exp)
  obs <- osf_links(paper)
  # osf_links() strips trailing slashes so a hyperlink and a bare body-text
  # mention of the same project (which commonly differ only by a trailing
  # slash) dedupe to one row -- see its own roxygen for the rationale.
  expect_equal(obs$href, sub("/+$", "", exp[1:2]))
})

test_that("osf_type", {
  expect_true(is.function(metacheck::osf_type))
  expect_no_error(helplist <- help(osf_type, metacheck))

  skip_on_cran()
  skip_on_covr()
  skip_on_ci()

  examples <- list(project = "pngda",
                   component = "https://osf.io/6nt4v",
                   private = "ybm3c",
                   file = "osf.io/75qgk",
                   preprint = "xp5cy",
                   user = "4i578",
                   reg = "8c3kb",
                   bad = "xx")


  otype <- osf_type(examples$project)
  expect_equal(otype, "nodes")

  otype <- osf_type(examples$component)
  expect_equal(otype, "nodes")

  otype <- osf_type(examples$private)
  expect_equal(otype, "nodes")

  otype <- osf_type(examples$file)
  expect_equal(otype, "files")

  otype <- osf_type(examples$preprint)
  expect_equal(otype, "preprints")

  otype <- osf_type(examples$user)
  expect_equal(otype, "users")

  otype <- osf_type(examples$reg)
  expect_equal(otype, "registrations")

  expect_warning(otype <- osf_type(examples$bad))
  expect_equal(otype, NA_character_)
}, "mock")

test_that("osf_check_id", {
  expect_true(is.function(metacheck::osf_check_id))
  expect_no_error(helplist <- help(osf_check_id, metacheck))

  # 5-letter
  osf_id <- "pngda"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, osf_id)

  # vector
  osf_id <- c("pngda", "8c3kb")
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, osf_id)

  # vector with invalid values
  osf_id <- c("pngda", "xxx", "8c3kb")
  expect_warning(checked_id <- osf_check_id(osf_id))
  expect_equal(checked_id, c("pngda", NA, "8c3kb"))

  # waterbutler id
  osf_id <- "6846ed88e49694cd45ab8375"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, osf_id)

  # waterbutler url
  osf_id <- "https://osf.io/j3gcx/files/osfstorage/6846ed88e49694cd45ab8375"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "6846ed88e49694cd45ab8375")

  # invalidwaterbutler id
  osf_id <- "6846ed894cd45ab8375"
  expect_warning(checked_id <- osf_check_id(osf_id))
  expect_true(is.na(checked_id))

  # urls
  osf_id <- "https://osf.io/pngda"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "pngda")

  osf_id <- "http://osf.io/pngda"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "pngda")

  # url with no http
  osf_id <- "osf.io/pngda"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "pngda")

  # deal with rogue whitespace
  osf_id <- "osf .io/pngda"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "pngda")

  # file storage
  osf_id <- "https://osf.io/j3gcx/files/osfstorage"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "j3gcx")

  osf_id <- "xx"
  expect_warning(checked_id <- osf_check_id(osf_id))
  expect_true(is.na(checked_id))

  # view-only link
  osf_id <- "https://osf.io/pngda/?view_only=5acf039f24ac4ea28afec473548dd7f4"
  checked_id <- osf_check_id(osf_id)
  expect_equal(checked_id, "pngda?view_only=5acf039f24ac4ea28afec473548dd7f4")

  # vector
  osf_id <- c(
    "6846ed88e49694cd45ab8375",
    "https://osf.io/j3gcx/files/osfstorage/6846ed88e49694cd45ab8375",
    "PNGDA",
    "pngda",
    "https://osf.io/pngda",
    "http://osf.io/pngda",
    "osf.io/pngda",
    "osf .io/pngda",
    "https://osf.io/pngda/files/osfstorage",
    "https://osf.io/pngda/?view_only=5acf039f24ac4ea28afec473548dd7f4",
    "https://osf.io/pngda?view_only=5acf039f24ac4ea28afec473548dd7f4",
    "xx",
    "6846ed88e49694cd45a"
  )

  # produces two warnings
  expect_warning(expect_warning(obs <- osf_check_id(osf_id)))
  exp <- rep(c("6846ed88e49694cd45ab8375", "pngda", NA_character_), c(2, 9, 2))
  exp[10:11] <- "pngda?view_only=5acf039f24ac4ea28afec473548dd7f4"
  expect_equal(obs, exp)
})

test_that("osf_check_id is silent for NA and bare osf.io platform links", {
  # NA identifiers and links to the OSF homepage (no project ID by
  # construction — papers often cite osf.io generically) are NA out with no
  # warning; the warning is reserved for strings that look like they should
  # carry an ID but don't.
  ids <- c(NA, "http://osf.io", "https://osf.io/", "osf.io",
           "https://www.osf.io/", "pngda")
  expect_no_warning(obs <- osf_check_id(ids))
  expect_equal(obs, c(rep(NA_character_, 5), "pngda"))
})


test_that("osf_delay", {
  expect_true(is.function(metacheck::osf_delay))
  expect_no_error(helplist <- help(osf_delay, metacheck))

  expect_gte(osf_delay(), 0)

  obs <- osf_delay(.005)
  expect_equal(obs, .005)
  expect_equal(osf_delay(), .005)

  osf_delay(0)
})

test_that("osf_api_check", {
  status <- osf_api_check()
  expect_equal(status, "OK")

  httptest2::without_internet({
    expect_error(osf_api_check())
    expect_warning(osf_api_check(on_error = "warn"))
    expect_no_condition(osf_api_check(on_error = "ignore"))
  })
}, "mock")


test_that("osf_get_all_pages", {
  osf_api <- getOption("metacheck.osf.api")

  # fewer than 10
  url <- sprintf("%s/nodes/pngda/files/osfstorage/", osf_api)
  data <- osf_get_all_pages(url)
  files <- c("test-folder", "README", "papercheck.png")
  expect_true(all(files %in% data$attributes$name))

  # more than 10
  url <- sprintf("%s/nodes/yt32c/files/osfstorage/", osf_api)
  data <- osf_get_all_pages(url)
  expect_equal(nrow(data), 14)

  # no results
  url <- sprintf("%s/nodes/y6a34/files/osfstorage/", osf_api)
  data <- osf_get_all_pages(url)
  expect_equal(data, dplyr::tibble())

  # limit pages
  url <- sprintf("%s/preprints/", osf_api)
  data <- osf_get_all_pages(url, 1)
  expect_equal(nrow(data), 10)

  data <- osf_get_all_pages(url, 2)
  expect_equal(nrow(data), 20)

  url <- sprintf("%s/preprints/?page=5", osf_api)
  data <- osf_get_all_pages(url, 5)
  expect_equal(nrow(data), 10)
}, "mock")

test_that(".osf_status_error classifies OSF's HTTP error statuses", {
  # https://github.com/scienceverse/metacheck/issues/338 -- a private,
  # embargoed, withdrawn, or deleted OSF resource returns a real HTTP error
  # status (403/404/410); osf_get_all_pages() used to disable httr2's error
  # handling entirely and parse whatever body came back as if it were real
  # data, making an inaccessible resource indistinguishable from one that
  # genuinely has no data. This is the status -> osf_error classification
  # osf_get_all_pages() now uses to tell the two apart, pulled out as its own
  # pure function so it is testable without mocking HTTP (this project's
  # custom test_that() wrapper in helper.R does not play well with
  # testthat::local_mocked_bindings() on shared httr2 internals like
  # req_perform() -- it leaked into unrelated tests later in this same file
  # when tried that way).
  expect_equal(metacheck:::.osf_status_error(403), "forbidden")
  expect_equal(metacheck:::.osf_status_error(401), "forbidden")
  expect_equal(metacheck:::.osf_status_error(404), "not_found")
  expect_equal(metacheck:::.osf_status_error(410), "gone")
  expect_equal(metacheck:::.osf_status_error(500), "request_failed")
  expect_equal(metacheck:::.osf_status_error(400), "request_failed")
  # not an error: 2xx and 3xx are NULL (not converted to an osf_error result)
  expect_null(metacheck:::.osf_status_error(200))
  expect_null(metacheck:::.osf_status_error(201))
  expect_null(metacheck:::.osf_status_error(304))
})

test_that(".osf_error_result carries osf_error without breaking length() == 0 checks", {
  # NULL cannot carry an attribute in R (attr(NULL, x) <- y errors) -- this
  # confirms the empty-list() substitute behaves like NULL for every existing
  # caller that only checks length(result) == 0, while still carrying the
  # attribute those that check `attr(result, "osf_error")` need.
  out <- metacheck:::.osf_error_result("not_found")
  expect_equal(length(out), 0)
  expect_equal(attr(out, "osf_error"), "not_found")
  expect_false(is.null(out)) # is.null(out) is FALSE -- callers must check length()/attr(), not is.null()
})

test_that("osf_get_all_pages callers only need length() == 0, no crash on an error result", {
  # A caller like prereg_check() does `if (length(reg_info) == 0) return(NULL)`
  # -- confirm that keeps working unchanged against the new error-result shape.
  out <- metacheck:::.osf_error_result("forbidden")
  expect_equal(length(out), 0)
  # dplyr::bind_rows() (used by osf_info()'s recursive children/files loop)
  # must silently skip it like it already does for NULL
  combined <- dplyr::bind_rows(data.frame(a = 1), out, data.frame(a = 2))
  expect_equal(nrow(combined), 2)
})

test_that("osf_info", {
  expect_true(is.function(metacheck::osf_info))
  expect_no_error(helplist <- help(osf_info, metacheck))

  examples <- c(project = "pngda",
                component = "https://osf.io/6nt4v",
                private = "ybm3c",
                file = "osf.io/75qgk",
                preprint = "xp5cy",
                user = "4i578",
                reg = "8c3kb",
                duplicate = "6nt4v",
                bad = "xx")
  osf_url <- data.frame(
    url = examples,
    type = names(examples)
  )
  expect_warning(table <- osf_info(osf_url))
  expect_equal(table$url, osf_url$url)
  expect_equal(table$type, osf_url$type)
  expect_equal(table[2, 3:10], table[8, 3:10], ignore_attr = TRUE)

  self <- c(
    "https://api.osf.io/v2/nodes/pngda/",
    "https://api.osf.io/v2/nodes/6nt4v/",
    "https://api.osf.io/v2/nodes/ybm3c/",
    "https://api.osf.io/v2/files/6846ed6a29684b023953943e/",
    "https://api.osf.io/v2/preprints/xp5cy_v1/",
    "https://api.osf.io/v2/users/4i578/",
    "https://api.osf.io/v2/registrations/8c3kb/",
    "https://api.osf.io/v2/nodes/6nt4v/",
    NA
  )
  expect_equal(table$self, self)

  # vector
  osf_url <- "pngda"
  table <- osf_info(osf_url)
  expect_equal(table$osf_url, osf_url)
  expect_equal(table$name, "Papercheck Test")

  # table with id_col
  osf_url <- data.frame(
    id = 100,
    osf_id = "pngda"
  )
  id_col <- "osf_id"
  table <- osf_info(osf_url, id_col)
  expect_equal(table$osf_id, osf_url$osf_id)
  expect_equal(table$name, "Papercheck Test")
  expect_equal(table$project, "pngda")

  # only one URL
  osf_url <- "https://osf.io/pngda"
  table <- osf_info(osf_url)
  expect_equal(table$name, "Papercheck Test")

  # no links
  paper <- test_paper("No links")
  osf_url <- osf_links(paper)
  info <- osf_info(osf_url, recursive = TRUE)
  expect_equal(nrow(info), 0)
  expect_equal(osf_url, info)
}, "mock")

test_that("osf_info - recursive", {
  # recursive
  osf_url <- "yt32c"
  table <- osf_info(osf_url, recursive = TRUE)
  expect_equal(nrow(table), 16)
  expect_equal(table$parent[1:2], c("ckjef", "yt32c"))

  # recursive with duplicates and NA vector
  osf_url <- c("yt32c", "yt32c", NA)
  table <- osf_info(osf_url, recursive = TRUE)
  expect_equal(nrow(table), 1 + 15)

  # recursive with duplicates and NA table. A missing id (NA) is treated as
  # "no link supplied" and returns NA silently, without a warning (only a
  # supplied-but-malformed id warns) — see osf_check_id().
  osf_url <- data.frame(parent_id = c("yt32c", "yt32c", NA),
                        n = 1:3)
  expect_no_warning(table <- osf_info(osf_url, recursive = TRUE))
  expect_equal(nrow(table), 3 + 15)
  expect_equal(table$n, c(1:3, rep(NA, 15)))
}, "mock")

test_that("osf_info recursive", {
  # folders can only have wb IDs,
  # files only have wb IDs until someone looks at them on the web
  #  and then they get 5-letter guids
  # currently just using wb IDs for all files

  osf_url <- "j3gcx"
  info <- osf_info(osf_url, recursive = TRUE)
  folders <- paste0("nest-", 1:4) |> c("empty")
  files <- paste0("test-", 1:4, ".txt")
  expect_in(folders, info$name)
  expect_in(files, info$name)

  folder_ids <- info$osf_id[info$kind %in% "folder"]
  expect_in(folder_ids[1:5], info$parent)

  # contains github and osfstorage files
  osf_url <- "mc45x"
  info <- osf_info(osf_url, recursive = TRUE)
  files <- sprintf("%02d.R", 1:10)
  expect_in(files, info$name)
}, "mock")

test_that("osf_info stops a listing at max_requests and marks it cut", {
  # the session cache is off for the tests (setup.R); this test checks it
  withr::local_options(metacheck.osf.cache = TRUE)
  osf_cache_clear()
  on.exit(osf_cache_clear(), add = TRUE)
  n0 <- metacheck:::.osf_requests$n
  full <- osf_info("j3gcx", recursive = TRUE)
  n_full <- metacheck:::.osf_requests$n - n0
  expect_false("listing_cut" %in% names(full))
  expect_gt(n_full, 2)

  osf_cache_clear()
  n0 <- metacheck:::.osf_requests$n
  expect_warning(cut <- osf_info("j3gcx", recursive = TRUE, max_requests = 2),
                 "stopped at the limit of 2 requests")
  # no new folder is opened once 2 requests are used; a request already
  # started still has its retries (the recorded responses have no answer for
  # the component tree, which is asked 3 times), so more than 2 can be made
  expect_lt(metacheck:::.osf_requests$n - n0, n_full)
  expect_true(all(cut$listing_cut))
  expect_lt(sum(cut$kind %in% "file"), sum(full$kind %in% "file"))

  # the cut listing is kept for this session under its limit only: a call
  # without a limit lists the project in full again
  expect_no_warning(again <- osf_info("j3gcx", recursive = TRUE, max_requests = 2))
  expect_identical(again, cut)
  whole <- osf_info("j3gcx", recursive = TRUE)
  expect_false("listing_cut" %in% names(whole))
  expect_equal(sum(whole$kind %in% "file"), sum(full$kind %in% "file"))
}, "mock")

test_that("osf_info keeps a complete recursive listing in the lasting cache, and goes on with a cut one", {
  full <- osf_info("j3gcx", recursive = TRUE)

  dir <- withr::local_tempdir()
  withr::local_options(metacheck.repo_info_cache.dir = dir)
  key <- metacheck:::.osf_cache_key("j3gcx", TRUE)
  complete_file <- metacheck:::.repo_info_cache_path("osf_listing", key)
  part_file <- metacheck:::.repo_info_cache_path("osf_listing_part", key)

  # cut short: only its progress is stored, not as a complete listing
  expect_warning(first <- osf_info("j3gcx", recursive = TRUE, cache = TRUE, max_requests = 2),
                 "run again with `cache = TRUE`")
  expect_true(all(first$listing_cut))
  expect_false(file.exists(complete_file))
  expect_true(file.exists(part_file))

  # each later run goes on where the previous one stopped, until complete
  runs <- 1
  repeat {
    x <- suppressWarnings(osf_info("j3gcx", recursive = TRUE, cache = TRUE, max_requests = 2))
    runs <- runs + 1
    if (!"listing_cut" %in% names(x) || runs > 20) break
  }
  expect_gt(runs, 2)
  expect_false("listing_cut" %in% names(x))
  expect_setequal(x$name[x$kind %in% "file"], full$name[full$kind %in% "file"])
  expect_setequal(x$osf_id, full$osf_id)
  expect_equal(nrow(x), nrow(full))
  expect_true(file.exists(complete_file))
  expect_false(file.exists(part_file))

  # complete: a later run reads it without asking the OSF to list anything,
  # whatever its limit
  n0 <- metacheck:::.osf_requests$n
  expect_no_warning(hit <- osf_info("j3gcx", recursive = TRUE, cache = TRUE, max_requests = 2))
  expect_equal(metacheck:::.osf_requests$n, n0)
  expect_identical(hit, x)
}, "mock")

test_that("osf_info - view_only", {
  skip_if_offline("api.osf.io")

  osf_id <- "iywec?view_only=f171281f212f4435917b16a9e581a73b"
  self <- "https://api.osf.io/v2/nodes/iywec/?view_only=f171281f212f4435917b16a9e581a73b"

  obs <- osf_info(osf_id)
  expect_equal(obs$osf_type, "nodes")
  expect_equal(obs$self, self)

  skip_if_quick()

  obs <- osf_info(osf_id, recursive = TRUE)
  exp <- c("Objects", "osfstorage", "comb.jpg")
  expect_contains(obs$name, exp)
})


test_that("osf_check_id skips route names and registration schemas (U51)", {
  # osf.io/j3gcx/files/ gave the id "files", osf.io/prereg gave "prere", and
  # osf.io/<id>/register/<schema> gave the schema id
  x <- c("https://osf.io/j3gcx/files/", "osf.io/prereg",
         "https://osf.io/a5bmw/register/565fb3678c5e4a66b5582f67",
         "https://osf.io/abcde/files/osfstorage/5f1e2d3c4b5a69788796a5b4",
         "https://osf.io/abcde/?view_only=123")
  expect_equal(suppressWarnings(unname(osf_check_id(x))),
               c("j3gcx", NA, "a5bmw", "5f1e2d3c4b5a69788796a5b4", "abcde?view_only=123"))
})
