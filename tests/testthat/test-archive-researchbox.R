test_that("exists", {
  expect_true(is.function(metacheck::rbox_links))
  expect_no_error(helplist <- help(rbox_links, metacheck))
  expect_true(is.function(metacheck::rbox_token))
  expect_no_error(helplist <- help(rbox_token, metacheck))
  expect_true(is.function(metacheck::rbox_files))
  expect_no_error(helplist <- help(rbox_files, metacheck))
  expect_true(is.function(metacheck::rbox_search))
  expect_no_error(helplist <- help(rbox_search, metacheck))

  expect_error(rbox_links(bad_arg))
  expect_error(rbox_search(bad_arg))
})

test_that("rbox_links", {
  paper <- test_paper(url = "https://researchbox.org/801")
  links <- rbox_links(paper)
  expect_equal(nrow(links), 1)
  expect_equal(links$href[[1]], "https://researchbox.org/801")

  # a peer-review link keeps its passcode, also in the text, and a plain link
  # to the same box in the same paper takes it, so the box is named once
  review <- "https://researchbox.org/1150&PEER_REVIEW_passcode=MJUAAS"
  paper <- test_paper(text = c("Files: researchbox.org/1150", "Other box: researchbox.org/801"),
                      url = review)
  links <- rbox_links(paper)
  expect_setequal(unique(links$href), c(review, "researchbox.org/801"))
  paper <- test_paper(text = paste("Files:", review))
  expect_equal(unique(rbox_links(paper)$href), review)
})

test_that("rbox_token", {
  withr::local_options(metacheck.researchbox.token = NULL)
  withr::local_envvar(RESEARCHBOX_API_KEY = "from-renviron")
  expect_equal(rbox_token(), "from-renviron")

  rbox_token("set-in-session")
  expect_equal(rbox_token(), "set-in-session")

  expect_error(rbox_token(1), "single string")
  expect_error(rbox_token(c("a", "b")), "single string")
})

test_that(".rbox_id", {
  ids <- .rbox_id(c("https://researchbox.org/801",
                    "researchbox.org/1150&PEER_REVIEW_passcode=MJUAAS",
                    "4377", "https://osf.io/abcde", NA))
  expect_equal(ids, c("801", "1150", "4377", NA, NA))
})

test_that(".rbox_code", {
  codes <- .rbox_code(c("https://researchbox.org/801",
                        "researchbox.org/1150&PEER_REVIEW_passcode=MJUAAS", NA))
  expect_equal(codes, c(NA, "MJUAAS", NA))
})

test_that(".rbox_iso_date", {
  expect_equal(.rbox_iso_date("June 09, 2023"), "2023-06-09")
  expect_equal(.rbox_iso_date("November 8, 2025"), "2025-11-08")
  expect_equal(.rbox_iso_date("not a date"), "not a date")
  expect_true(is.na(.rbox_iso_date(NA_character_)))
})

test_that(".rbox_info from the web page", {
  # without a key the box's web page is read
  withr::local_options(metacheck.researchbox.token = "")
  url <- "https://researchbox.org/801"
  info <- .rbox_info(url)

  # using expect_match because RB keeps subtly changing formats
  expect_equal(info$rb_url, url)
  expect_match(info$RB_target, "Are Empathic People Better Adjusted",)
  # in the same form as from the API
  expect_equal(info$RB_license, "CC-BY-4.0")
  expect_equal(info$RB_public, "2023-06-09")
  expect_match(info$RB_authors, "Joyce He")
  expect_match(info$RB_authors, "Stéphane Côté")
  expect_match(info$RB_abstract, "Are individuals adept at perceiving other")
  # only the API gives these
  expect_true(is.na(info$RB_title))
  expect_true(is.na(info$RB_paper_doi))

  files <- info$files[[1]]
  expect_equal(nrow(files), 9)

  ## peer review version
  rb_url <- "https://researchbox.org/1150&PEER_REVIEW_passcode=MJUAAS"
  info <- .rbox_info(rb_url)
  expect_equal(info$rb_url, rb_url)
  expect_equal(info$RB_public, "2024-10-07")
}, "mock")

test_that(".rbox_info from the API", {
  withr::local_options(metacheck.researchbox.token = "test-token")
  url <- "https://researchbox.org/801"
  info <- .rbox_info(url)

  expect_equal(info$rb_url, url)
  expect_match(info$RB_target, "Are Empathic People Better Adjusted")
  # the reference comes with HTML markup, which is removed
  expect_false(grepl("<", info$RB_target, fixed = TRUE))
  expect_equal(info$RB_authors, "Joyce He, Stéphane Côté")
  expect_match(info$RB_abstract, "Are individuals adept at perceiving other")
  expect_equal(info$RB_title, "Empathic Accuracy and Self Views on Adju")
  expect_equal(info$RB_status, "public")
  expect_equal(info$RB_paper_doi, "10.1177/09567976231185127")
  expect_equal(info$RB_zenodo_doi, "10.5281/zenodo.15038649")
  expect_equal(info$RB_license, "CC-BY-4.0")
  expect_equal(info$RB_license_url, "https://creativecommons.org/licenses/by/4.0/")
  expect_equal(info$RB_public, "2023-06-09")
  expect_equal(info$RB_last_modified, "2023-05-29T23:36:09Z")

  files <- info$files[[1]]
  expect_equal(nrow(files), 10)
  expect_true(all(c("name", "file_id", "section", "type", "size", "md5") %in% names(files)))
  expect_true(all(nchar(files$md5) == 32))

  # both routes give the same columns
  withr::local_options(metacheck.researchbox.token = "")
  page <- .rbox_info(url)
  expect_setequal(names(info), names(page))
}, "mock")

test_that(".rbox_info reads the web page when the API refuses", {
  withr::local_options(metacheck.researchbox.token = "test-token")
  local_mocked_bindings(
    .rbox_info_api = function(rb_url) data.frame(
      rb_url = rb_url, error = "API rate limit exhausted: list_files same-box hourly limit"),
    .rbox_info_page = function(rb_url) data.frame(rb_url = rb_url, RB_title = NA)
  )
  info <- .rbox_info("https://researchbox.org/801")
  expect_false("error" %in% names(info))

  # both failed: the API's reason first
  local_mocked_bindings(
    .rbox_info_page = function(rb_url) data.frame(rb_url = rb_url, error = "HTTP 403")
  )
  info <- .rbox_info("https://researchbox.org/801")
  expect_equal(info$error, "API rate limit exhausted: list_files same-box hourly limit; web page: HTTP 403")

  # a box the API cannot find may be one that is not public: the web page
  # tells which, and its answer is the one reported
  local_mocked_bindings(
    .rbox_info_api = function(rb_url) data.frame(rb_url = rb_url, error = "unfound"),
    .rbox_info_page = function(rb_url) data.frame(rb_url = rb_url, error = "not public: x")
  )
  info <- .rbox_info("https://researchbox.org/801")
  expect_equal(info$error, "not public: x")
})

test_that("rbox_info", {
  testthat::local_mocked_bindings(
    online = \(...) TRUE
  )
  withr::local_options(metacheck.researchbox.token = "")

  paper <- test_paper(url = c(
    "https://researchbox.org/4377",
    "https://researchbox.org/6018"
  ))
  links <- rbox_links(paper)
  info <- rbox_info(links, "href")

  #expected
  public <- c("2025-11-28", "2026-02-15")

  expect_equal(info$href, links$href)
  expect_equal(info$RB_public, public)
}, "mock")

test_that("rbox_info cache", {
  testthat::local_mocked_bindings(online = \(...) TRUE)
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.cache.dir = withr::local_tempdir(),
                       metacheck.state.dir = withr::local_tempdir())
  first <- rbox_info("https://researchbox.org/801", cache = TRUE)

  # the second call asks ResearchBox nothing, for any link to the same box
  local_mocked_bindings(.rbox_info = function(...) stop("no request expected"))
  again <- rbox_info("researchbox.org/801", cache = TRUE)
  expect_equal(again$rb_url, "researchbox.org/801")
  expect_equal(again$RB_title, first$RB_title)
}, "mock")

test_that("rbox_files", {
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.cache.dir = withr::local_tempdir(),
                       metacheck.state.dir = withr::local_tempdir())
  files <- rbox_files("https://researchbox.org/801")
  expect_equal(nrow(files), 10)
  expect_equal(names(files), c("rb_url", "name", "file_id", "section", "type",
                               "description", "extension", "size", "md5"))
  expect_equal(unique(files$rb_url), "https://researchbox.org/801")

  # a file list cached by rbox_files() is reused
  rbox_files("https://researchbox.org/801", cache = TRUE)
  local_mocked_bindings(.rbox_api = function(...) stop("no request expected"))
  expect_equal(rbox_files("https://researchbox.org/801", cache = TRUE), files)

  # a box that could not be listed gets a row with the reason
  local_mocked_bindings(.rbox_api = function(...) list(
    ok = FALSE, error = "API rate limit exhausted: list_files same-box hourly limit", body = NULL))
  expect_warning(files <- rbox_files("https://researchbox.org/4377"), "did not list")
  expect_equal(files$error, "API rate limit exhausted: list_files same-box hourly limit")

  withr::local_options(metacheck.researchbox.token = "")
  expect_error(rbox_files("https://researchbox.org/801"), "needs a ResearchBox API key")
}, "mock")

test_that("a peer-review link sends its passcode to the API", {
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.cache.dir = withr::local_tempdir(),
                       metacheck.state.dir = withr::local_tempdir())
  sent <- list()
  local_mocked_bindings(.rbox_api = function(action, ..., code = NA_character_, path = NULL) {
    sent[[action]] <<- code
    list(ok = TRUE, error = NA_character_, body = list(files = data.frame(
      basename = "a.csv", file_id = 1, section = "Study 1", type = "Data",
      description = "", extension = "csv", size = 1, md5 = strrep("0", 32))))
  })
  url <- "https://researchbox.org/1150&PEER_REVIEW_passcode=MJUAAS"
  rbox_files(url)
  expect_equal(sent$list_files, "MJUAAS")
  .rbox_info(url)
  expect_equal(sent$box_meta, "MJUAAS")

  # a link without one sends none
  rbox_files("https://researchbox.org/801")
  expect_true(is.na(sent$list_files))
})

test_that("rbox_search", {
  withr::local_options(metacheck.researchbox.token = "test-token")
  hits <- rbox_search("happi*", limit = 3)
  expect_equal(nrow(hits), 3)
  expect_equal(attr(hits, "total"), 123)
  expect_equal(hits$rb_url[[1]], "https://researchbox.org/9614")
  expect_true(all(c("box_id", "file_id", "var_name", "var_description") %in% names(hits)))
  expect_s3_class(hits$upload_time, "POSIXct")

  expect_error(rbox_search(1), "single string")
  expect_error(rbox_search("happi*", limit = 0), "at least 1")
  expect_error(rbox_search("happi*", type = "video"))
  withr::local_options(metacheck.researchbox.token = "")
  expect_error(rbox_search("happi*"), "needs a ResearchBox API key")
}, "mock")

test_that("rbox_search pages through the hits", {
  withr::local_options(metacheck.researchbox.token = "test-token")
  asked <- list()
  local_mocked_bindings(.rbox_api = function(action, ...) {
    args <- list(...)
    asked[[length(asked) + 1]] <<- args
    n <- max(0, min(args$limit, 250 - args$offset))
    hits <- if (n > 0) data.frame(box_id = seq_len(n) + args$offset) else list()
    list(ok = TRUE, error = NA_character_, body = list(total = 250, hits = hits))
  })

  hits <- rbox_search("x", limit = Inf)
  expect_equal(nrow(hits), 250)
  expect_equal(vapply(asked, \(a) a$offset, numeric(1)), c(0, 100, 200))
  expect_equal(hits$box_id, 1:250)

  asked <- list()
  hits <- rbox_search("x", limit = 30, offset = 10)
  expect_equal(nrow(hits), 30)
  expect_equal(length(asked), 1)
  expect_equal(hits$box_id[[1]], 11)

  # no hits
  local_mocked_bindings(.rbox_api = function(action, ...) list(
    ok = TRUE, error = NA_character_, body = list(total = 0, hits = list())))
  hits <- rbox_search("x")
  expect_equal(nrow(hits), 0)
  expect_equal(attr(hits, "total"), 0)

  # a failed first request stops; a later one keeps the hits so far
  local_mocked_bindings(.rbox_api = function(action, ...) list(
    ok = FALSE, error = "HTTP 400: query too short", body = NULL))
  expect_error(rbox_search("x"), "query too short")
  calls <- 0
  local_mocked_bindings(.rbox_api = function(action, ...) {
    calls <<- calls + 1
    if (calls > 1) return(list(ok = FALSE, error = "API rate limit exhausted: x", body = NULL))
    list(ok = TRUE, error = NA_character_,
         body = list(total = 250, hits = data.frame(box_id = 1:100)))
  })
  expect_warning(hits <- rbox_search("x", limit = Inf), "offset = 100")
  expect_equal(nrow(hits), 100)
})

test_that(".rbox_api turns refusals into messages", {
  withr::local_options(metacheck.researchbox.token = "",
                       metacheck.cache.dir = withr::local_tempdir(),
                       metacheck.state.dir = withr::local_tempdir())
  expect_match(.rbox_api("box_meta", box_id = 1)$error, "no ResearchBox API key")

  withr::local_options(metacheck.researchbox.token = "test-token")
  json <- \(status, x, ...) httr2::response(
    status, headers = list(`Content-Type` = "application/json", ...), body = charToRaw(x))
  replies <- list(
    list(json(429, '{"ok":false,"error":"list_files same-box hourly limit"}'),
         "API rate limit exhausted: list_files same-box hourly limit"),
    # a wait longer than a minute is not sat out, but named
    list(json(429, '{"ok":false,"error":"list_files same-box hourly limit","retry_after_seconds":1800}',
              `Retry-After` = "1800"),
         "API rate limit exhausted: list_files same-box hourly limit (ResearchBox asks to wait 1800 seconds)"),
    list(httr2::response(500, body = raw(0)), "unfound"),
    list(json(400, '{"ok":false,"error":"invalid box_id"}'), "HTTP 400: invalid box_id")
  )
  for (r in replies) {
    local_mocked_bindings(req_perform = function(req, ...) r[[1]], .package = "httr2")
    res <- .rbox_api("box_meta", box_id = 1)
    expect_false(res$ok)
    expect_equal(res$error, r[[2]])
  }

  # how long ResearchBox asks to wait
  expect_equal(.rbox_retry_after(json(429, '{"ok":false,"error":"too soon; retry later","retry_after_seconds":4}')), 4)
  expect_equal(.rbox_retry_after(json(429, '{"ok":false}', `Retry-After` = "30")), 30)
  expect_true(is.na(.rbox_retry_after(json(429, '{"ok":false,"error":"list_files same-box hourly limit"}'))))
  expect_true(is.na(.rbox_retry_after(json(200, '{"ok":true}'))))
})

test_that("boxes without a passcode are not sent after 8 answers of 'unfound'", {
  # ResearchBox blocks the key for 10 days after 10 requests for boxes that
  # are not public, and answers those as if the box did not exist
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.state.dir = withr::local_tempdir())
  local_mocked_bindings(req_perform = function(req, ...) httr2::response(500, body = raw(0)),
                        .package = "httr2")
  for (i in 1:8) expect_equal(.rbox_api("box_meta", box_id = i)$error, "unfound")
  expect_length(.rbox_unfound_times(), 8)
  expect_match(.rbox_api("list_files", box_id = 9)$error, "^not sent: ")

  # a request with a passcode is still sent, and its answer is not counted
  expect_equal(.rbox_api("box_meta", box_id = 9, code = "ABCDEF")$error, "unfound")
  expect_length(.rbox_unfound_times(), 8)

  # answers older than 10 days no longer count
  saveRDS(rep(Sys.time() - as.difftime(11, units = "days"), 8), .rbox_unfound_path())
  expect_equal(.rbox_api("box_meta", box_id = 9)$error, "unfound")
  expect_length(.rbox_unfound_times(), 1)
})

# A box of three files, zipped as ResearchBox's API does (folders by type,
# with a table of contents unless `toc = FALSE`), and the file list the API
# gives for it. `bad_md5` names a file whose checksum in the table of contents
# is wrong.
.rbox_test_box <- function(d, toc = TRUE, bad_md5 = NULL) {
  box <- file.path(d, "box")
  dir.create(file.path(box, "Data"), recursive = TRUE)
  dir.create(file.path(box, "Codebook"))
  dir.create(file.path(box, "Code"))
  writeLines("id,x\n1,2", file.path(box, "Data", "study1.csv"))
  writeLines("var,desc\nx,a value", file.path(box, "Codebook", "study1.csv___CODEBOOK.csv"))
  writeLines("x <- 1", file.path(box, "Code", "analysis.R"))
  paths <- c("Data/study1.csv", "Codebook/study1.csv___CODEBOOK.csv", "Code/analysis.R")
  files <- data.frame(
    section = "Study 1", type = c("Data", "Codebook", "Code"), description = "",
    basename = basename(paths), extension = c("csv", "csv", "R"),
    size = file.size(file.path(box, paths)), file_id = 1:3,
    md5 = unname(tools::md5sum(file.path(box, paths))))
  if (toc) {
    toc_files <- data.frame(type = files$type, section = files$section,
                            description = files$description, file_name = files$basename,
                            file_id = files$file_id, size = files$size, md5 = files$md5)
    toc_files$md5[toc_files$file_name %in% bad_md5] <- strrep("0", 32)
    jsonlite::write_json(list(box_id = 99999, files = toc_files),
                         file.path(box, "Table of Contents Box #99999.json"),
                         auto_unbox = TRUE)
    writeLines("type,section", file.path(box, "Table of Contents Box #99999.csv"))
    paths <- c(paths, "Table of Contents Box #99999.json", "Table of Contents Box #99999.csv")
  }
  withr::with_dir(box, utils::zip(file.path(d, "box.zip"), paths, flags = "-q"))
  list(zip = file.path(d, "box.zip"), files = files)
}

test_that("rbox_file_download checks the files against the table of contents", {
  box <- .rbox_test_box(withr::local_tempdir())
  skip_if_not(file.exists(box$zip), "zip utility unavailable")
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.cache.dir = withr::local_tempdir(),
                       metacheck.state.dir = withr::local_tempdir())
  asked <- character(0)
  zip_file <- box$zip
  local_mocked_bindings(.rbox_api = function(action, ..., code = NA_character_, path = NULL) {
    asked <<- c(asked, action)
    if (action == "download_files") file.copy(zip_file, path, overwrite = TRUE)
    list(ok = TRUE, error = NA_character_, body = list(files = box$files))
  })

  url <- "https://researchbox.org/99999"
  files <- rbox_file_download(url)
  # one request: the checksums come with the download
  expect_equal(asked, "download_files")
  # the table of contents is not one of the box's files, and is kept aside
  expect_setequal(files$name, c("Data/study1.csv", "Codebook/study1.csv___CODEBOOK.csv",
                                "Code/analysis.R"))
  expect_true(file.exists(file.path(.repo_cache_subdir(url), "Table of Contents Box #99999.json")))
  expect_true(all(files$checksum_ok))
  expect_equal(nrow(attr(files, "failed")), 0)

  # a box already downloaded is reused without a request, checks included
  asked <- character(0)
  again <- rbox_file_download(url)
  expect_equal(asked, character(0))
  expect_true(all(again$checksum_ok))

  # a file that does not match its checksum is listed as failed, and the box
  # is downloaded again on the next call
  zip_file <- .rbox_test_box(withr::local_tempdir(), bad_md5 = "analysis.R")$zip
  url2 <- "https://researchbox.org/99998"
  expect_warning(files <- rbox_file_download(url2), "did not arrive intact")
  expect_equal(files$checksum_ok[files$name == "Code/analysis.R"], FALSE)
  failed <- attr(files, "failed")
  expect_equal(failed$name, "analysis.R")
  expect_equal(failed$error, "checksum does not match")
  asked <- character(0)
  suppressWarnings(rbox_file_download(url2))
  expect_equal(asked, "download_files")
})

test_that("rbox_file_download without a table of contents asks for the file list", {
  box <- .rbox_test_box(withr::local_tempdir(), toc = FALSE)
  skip_if_not(file.exists(box$zip), "zip utility unavailable")
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.cache.dir = withr::local_tempdir(),
                       metacheck.state.dir = withr::local_tempdir())
  asked <- character(0)
  listed_ok <- TRUE
  local_mocked_bindings(.rbox_api = function(action, ..., code = NA_character_, path = NULL) {
    asked <<- c(asked, action)
    if (action == "download_files") file.copy(box$zip, path, overwrite = TRUE)
    if (action == "list_files" && !listed_ok) {
      return(list(ok = FALSE, error = "API rate limit exhausted: x", body = NULL))
    }
    list(ok = TRUE, error = NA_character_, body = list(files = box$files))
  })

  files <- rbox_file_download("https://researchbox.org/99999")
  expect_equal(asked, c("download_files", "list_files"))
  expect_true(all(files$checksum_ok))

  # a refused file list: the files arrive, unchecked
  listed_ok <- FALSE
  files <- rbox_file_download("https://researchbox.org/99998")
  expect_equal(nrow(files), 3)
  expect_true(all(is.na(files$checksum_ok)))
  expect_equal(nrow(attr(files, "failed")), 0)
})

test_that("rbox_file_download of chosen files", {
  d <- withr::local_tempdir()
  box <- .rbox_test_box(d, toc = FALSE)
  skip_if_not(file.exists(box$zip), "zip utility unavailable")
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.cache.dir = withr::local_tempdir(),
                       metacheck.state.dir = withr::local_tempdir())
  sent <- list()
  local_mocked_bindings(.rbox_api = function(action, ..., code = NA_character_, path = NULL) {
    if (action == "download_files") {
      sent <<- list(...)
      # the data file comes with its codebook
      withr::with_dir(file.path(d, "box"), utils::zip(
        path, c("Data/study1.csv", "Codebook/study1.csv___CODEBOOK.csv"), flags = "-q"))
    }
    list(ok = TRUE, error = NA_character_, body = list(files = box$files))
  })

  files <- rbox_file_download("https://researchbox.org/99999", file_ids = 1,
                              subfolders = "section")
  expect_equal(sent$file_ids, "1")
  expect_equal(sent$subfolders, "section")
  expect_equal(nrow(files), 2)
  # the codebook was not asked for, but is checked as well
  expect_true(all(files$checksum_ok))
  expect_equal(nrow(attr(files, "failed")), 0)

  # a file asked for that the box does not have did not arrive
  files <- suppressWarnings(rbox_file_download("https://researchbox.org/99998",
                                               file_ids = c(1, 7)))
  failed <- attr(files, "failed")
  expect_equal(failed$name, "file_id 7")
  expect_equal(failed$error, "not in the download")

  # chosen files need the API
  withr::local_options(metacheck.researchbox.token = "")
  files <- rbox_file_download("https://researchbox.org/99999", file_ids = 1)
  expect_equal(nrow(files), 0)
  expect_match(attr(files, "failed")$error, "needs the ResearchBox API")
})

test_that("several links to one box download it once, with the passcode", {
  box <- .rbox_test_box(withr::local_tempdir())
  skip_if_not(file.exists(box$zip), "zip utility unavailable")
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.cache.dir = withr::local_tempdir(),
                       metacheck.state.dir = withr::local_tempdir())
  sent <- list()
  local_mocked_bindings(.rbox_api = function(action, ..., code = NA_character_, path = NULL) {
    sent[[length(sent) + 1]] <<- list(action = action, code = code)
    if (action == "download_files") file.copy(box$zip, path, overwrite = TRUE)
    list(ok = TRUE, error = NA_character_, body = NULL)
  })

  plain <- "https://researchbox.org/99999"
  review <- "https://researchbox.org/99999&PEER_REVIEW_passcode=ABCDEF"
  files <- rbox_file_download(c(plain, review))
  expect_length(sent, 1)
  expect_equal(sent[[1]]$code, "ABCDEF")
  # every link gets the box's files
  expect_equal(nrow(files), 6)
  expect_setequal(unique(files$rb_url), c(plain, review))

  # a later call through any link to the box reuses the download
  rbox_file_download(review)
  rbox_file_download("researchbox.org/99999")
  expect_length(sent, 1)
})

test_that(".rbox_license", {
  withr::local_options(metacheck.researchbox.token = "test-token",
                       metacheck.cache.dir = withr::local_tempdir(),
                       metacheck.state.dir = withr::local_tempdir())
  asked <- 0
  local_mocked_bindings(.rbox_api = function(action, ..., code = NA_character_, path = NULL) {
    asked <<- asked + 1
    list(ok = TRUE, error = NA_character_, body = list(license_id = "CC-BY-4.0"))
  })
  expect_equal(.rbox_license("https://researchbox.org/801", cache = TRUE), "CC-BY-4.0")
  # cached for any link to the box
  expect_equal(.rbox_license("researchbox.org/801", cache = TRUE), "CC-BY-4.0")
  expect_equal(asked, 1)

  # the web page when the API fails
  local_mocked_bindings(
    .rbox_api = function(...) list(ok = FALSE, error = "HTTP 503", body = NULL),
    .rbox_info_page = function(rb_url) data.frame(rb_url = rb_url, RB_license = "CC-BY-4.0")
  )
  expect_equal(.rbox_license("https://researchbox.org/802"), "CC-BY-4.0")
  local_mocked_bindings(
    .rbox_info_page = function(rb_url) data.frame(rb_url = rb_url, error = "unfound")
  )
  expect_true(is.na(.rbox_license("https://researchbox.org/803")))
})

test_that("an empty ResearchBox page, and several boxes that all fail (U43, U45)", {
  withr::local_options(metacheck.researchbox.token = "",
                       metacheck.cache.dir = withr::local_tempdir(),
                       metacheck.state.dir = withr::local_tempdir())
  # an empty body stopped resp_body_string() before the status was checked
  for (st in c(200, 403)) {
    local_mocked_bindings(req_perform = function(req, ...) httr2::response(st, body = raw(0)),
                          .package = "httr2")
    info <- suppressWarnings(.rbox_info("https://researchbox.org/1"))
    expect_equal(info$error, if (st == 200) "unfound" else "HTTP 403")
  }
  # the page of a box that does not exist, and of one that is not public,
  # were read as a box without files
  pages <- list(c("<p>Problem. There is no ResearchBox #1</p>", "^unfound$"),
                c("<script>window.location.replace('captcha_wall.php?t=x')</script>", "^not public: "))
  for (page in pages) {
    local_mocked_bindings(req_perform = function(req, ...) httr2::response(200, body = charToRaw(page[[1]])),
                          .package = "httr2")
    info <- suppressWarnings(.rbox_info("https://researchbox.org/1"))
    expect_match(info$error, page[[2]])
  }
  # several boxes that all fail: no rows, and each box's reason in the
  # "failed" attribute; the join stopped
  local_mocked_bindings(.rbox_info_page = function(u, ...) data.frame(rb_url = u, error = "unfound"))
  files <- suppressWarnings(
    rbox_file_download(c("https://researchbox.org/1", "https://researchbox.org/2")))
  expect_equal(nrow(files), 0)
  failed <- attr(files, "failed")
  expect_equal(failed$rb_url, c("https://researchbox.org/1", "https://researchbox.org/2"))
  expect_equal(failed$error, c("unfound", "unfound"))
})
