test_that("convert_bibr selfhosted backend", {
  expect_true(is.function(metacheck::convert_bibr))
  expect_no_error(helplist <- help(convert_bibr, metacheck))

  expect_error(convert_bibr(bad_arg))

  skip_api("localhost:8000")
  skip_if_quick()
  skip_if_not(.bibr_isalive("http://localhost:8000", error = FALSE), message = "bibr local host not available")

  # pdf
  file_name <- "to_err_is_human.pdf"
  file_path <- system.file("demo", file_name, package = "metacheck")
  save_path <- withr::local_tempdir()
  json_path <- convert_bibr(file_path, save_path, backend = "selfhosted")
  expect_match(json_path, "to_err_is_human\\.json")
  expect_true(file.exists(json_path) |> all())
  pdf <- .read_bibr(json_path)
  expect_equal(pdf$info$file_name, file_name)
  expect_match(pdf$paper_id, "^[a-f0-9]{16}$")

  # docx
  file_name <- "to_err_is_human.docx"
  file_path <- system.file("demo", file_name, package = "metacheck")
  save_path <- withr::local_tempdir()
  json_path <- convert_bibr(file_path, save_path, backend = "selfhosted")
  expect_true(file.exists(json_path) |> all())
  expect_match(json_path, "to_err_is_human\\.json")
  docx <- .read_bibr(json_path)
  expect_equal(docx$info$file_name, file_name)
  expect_match(docx$paper_id, "^[a-f0-9]{16}$")

  # doc
  file_name <- "to_err_is_human.doc"
  file_path <- system.file("demo", file_name, package = "metacheck")
  save_path <- withr::local_tempdir()
  json_path <- convert_bibr(file_path, save_path, backend = "selfhosted")
  expect_true(file.exists(json_path) |> all())
  expect_match(json_path, "to_err_is_human\\.json")
  doc <- .read_bibr(json_path)
  expect_equal(doc$info$file_name, file_name)
  expect_match(doc$paper_id, "^[a-f0-9]{16}$")

  # multiple files
  file_name <- c("preprint.pdf", "published.pdf")
  file_path <- c(test_path("fixtures", "formats", file_name))
  save_path <- withr::local_tempdir()
  json_path <- convert_bibr(file_path, save_path, backend = "selfhosted")
  expect_match(json_path[[1]], "preprint\\.json")
  expect_match(json_path[[2]], "published\\.json")
  expect_true(file.exists(json_path) |> all())

})

test_that("convert_bibr selfhosted sends include_figures in request", {
  file_path <- demofile("pdf")

  captured_body <- NULL
  httr2::local_mocked_responses(function(req) {
    captured_body <<- req$body$data
    httr2::response_json(body = list(status = "ok"))
  })

  save_path <- withr::local_tempdir()
  tryCatch(
    convert_bibr(file_path, save_path, backend = "selfhosted",
                 include_figures = TRUE),
    error = function(e) NULL
  )
  expect_equal(captured_body$include_figures, "true")

  captured_body <- NULL
  tryCatch(
    convert_bibr(file_path, save_path, backend = "selfhosted",
                 include_figures = FALSE),
    error = function(e) NULL
  )
  expect_equal(captured_body$include_figures, "false")
})

test_that("convert_bibr selfhosted sends page params in request", {
  file_path <- demofile("pdf")

  captured_body <- NULL
  httr2::local_mocked_responses(function(req) {
    captured_body <<- req$body$data
    httr2::response_json(body = list(status = "ok"))
  })

  save_path <- withr::local_tempdir()
  tryCatch(
    convert_bibr(file_path, save_path, backend = "selfhosted",
                 start_page = 3, end_page = 10),
    error = function(e) NULL
  )
  # zero-based: start_page 3 -> 2, end_page 10 -> 9
  expect_equal(captured_body$start_page, "2")
  expect_equal(captured_body$end_page, "9")

  # defaults omit page fields entirely
  captured_body <- NULL
  tryCatch(
    convert_bibr(file_path, save_path, backend = "selfhosted",
),
    error = function(e) NULL
  )
  expect_null(captured_body$start_page)
  expect_null(captured_body$end_page)

  # start_page = 1 is default, still omitted
  captured_body <- NULL
  tryCatch(
    convert_bibr(file_path, save_path, backend = "selfhosted",
                 start_page = 1, end_page = 5),
    error = function(e) NULL
  )
  expect_null(captured_body$start_page)
  expect_equal(captured_body$end_page, "4")
})


test_that("convert_bibr scivrs backend", {
  skip_api(bibr_url)
  skip_if_quick()
  skip_if_not(.bibr_isalive(bibr_url, error = FALSE),
              message = "bibr not available")

  # pdf
  file_path <- demofile("pdf")
  save_path <- withr::local_tempdir()
  json_path <- convert_bibr(file_path, save_path, backend = "scivrs")
  expect_match(json_path, "to_err_is_human\\.json")
  expect_true(file.exists(json_path) |> all())
  pdf <- .read_bibr(json_path)
  expect_equal(pdf$info$file_name, basename(file_path))
  #expect_match(pdf$paper_id, "^[a-f0-9]{16}$")

  # docx
  file_path <- demofile("docx")
  save_path <- withr::local_tempdir()
  json_path <- convert_bibr(file_path, save_path, backend = "scivrs")
  expect_true(file.exists(json_path) |> all())
  expect_match(json_path, "to_err_is_human\\.json")
  docx <- .read_bibr(json_path)
  expect_equal(docx$info$file_name, basename(file_path))
  #expect_match(docx$paper_id, "^[a-f0-9]{16}$")

  # doc
  file_path <- demofile("doc")
  save_path <- withr::local_tempdir()
  json_path <- convert_bibr(file_path, save_path, backend = "scivrs")
  expect_true(file.exists(json_path) |> all())
  expect_match(json_path, "to_err_is_human\\.json")
  doc <- .read_bibr(json_path)
  expect_equal(doc$info$file_name, basename(file_path))
  #expect_match(doc$paper_id, "^[a-f0-9]{16}$")

  # multiple files
  file_name <- c("preprint.pdf", "published.pdf")
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- convert_bibr(file_path, save_path, backend = "scivrs")
  expect_match(json_path[[1]], "preprint\\.json")
  expect_match(json_path[[2]], "published\\.json")
  expect_true(file.exists(json_path) |> all())

})

test_that("convert_bibr scivrs sends include_figures in request", {
  skip_if_quick()

  file_path <- demofile("pdf")

  captured_body <- NULL
  httr2::local_mocked_responses(function(req) {
    if (!is.null(req$body$data)) {
      captured_body <<- req$body$data
    }
    if (grepl("/jobs$", req$url)) {
      httr2::response_json(body = list(job_id = "test-job"))
    } else if (grepl("/result$", req$url)) {
      httr2::response_json(body = list(text = "mock"))
    } else {
      httr2::response_json(body = list(status = "complete"))
    }
  })

  save_path <- withr::local_tempdir()
  withr::local_envvar(SCIVRS_API_KEY = "sv_test_key")
  tryCatch(
    convert_bibr(file_path, save_path, backend = "scivrs", include_figures = TRUE),
    error = function(e) NULL
  )
  expect_equal(captured_body$include_figures, "true")

  captured_body <- NULL
  tryCatch(
    convert_bibr(file_path, save_path, backend = "scivrs", include_figures = FALSE),
    error = function(e) NULL
  )
  expect_equal(captured_body$include_figures, "false")
})

test_that("convert_bibr scivrs sends page params in request", {
  file_path <- demofile("pdf")

  captured_body <- NULL
  httr2::local_mocked_responses(function(req) {
    if (!is.null(req$body$data)) {
      captured_body <<- req$body$data
    }
    if (grepl("/jobs$", req$url)) {
      httr2::response_json(body = list(job_id = "test-job"))
    } else if (grepl("/result$", req$url)) {
      httr2::response_json(body = list(text = "mock"))
    } else {
      httr2::response_json(body = list(status = "complete"))
    }
  })

  save_path <- withr::local_tempdir()
  withr::local_envvar(SCIVRS_API_KEY = "sv_test_key")
  tryCatch(
    convert_bibr(file_path, save_path, backend = "scivrs",
                 start_page = 2, end_page = 5),
    error = function(e) NULL
  )
  # zero-based: start_page 2 -> 1, end_page 5 -> 4
  expect_equal(captured_body$start_page, "1")
  expect_equal(captured_body$end_page, "4")

  # defaults omit page fields
  captured_body <- NULL
  tryCatch(
    convert_bibr(file_path, save_path, backend = "scivrs"),
    error = function(e) NULL
  )
  expect_null(captured_body$start_page)
  expect_null(captured_body$end_page)
})

test_that("an empty address variable counts as not set", {
  # Linux keeps a variable set to "" (Windows removes it), so
  # Sys.getenv(x, default) gave "" there and no request could be made
  surf_url <- "https://metacheckserve.metacheck.src.surf-hosted.nl/api"
  withr::local_envvar(SURF_API_URL = "")
  expect_equal(metacheck:::.surf_api_url(), surf_url)
  withr::local_envvar(SURF_API_URL = "https://example.org/api")
  expect_equal(metacheck:::.surf_api_url(), "https://example.org/api")
})

test_that("convert_bibr auto-detects backend", {
  withr::local_envvar(SCIVRS_API_KEY = "", BIBR_API_URL = "", SURF_API_URL = "")

  # match the default URLs resolved inside convert_bibr()
  surf_url <- "https://metacheckserve.metacheck.src.surf-hosted.nl/api"
  scivrs_url <- "https://platform.metacheck.app"
  selfhosted_url <- "http://localhost:8000"

  captured_url <- NULL
  httr2::local_mocked_responses(function(req) {
    captured_url <<- req$url
    if (grepl("/jobs$", req$url)) {
      httr2::response_json(body = list(job_id = "test-job"))
    } else if (grepl("/result$", req$url)) {
      httr2::response_json(body = list(text = "mock"))
    } else {
      httr2::response_json(body = list(status = "complete"))
    }
  })
  file_path <- demofile("pdf")
  save_path <- withr::local_tempdir()

  # no key or bibr address set -> surf
  tryCatch(convert_bibr(file_path, save_path), error = function(e) NULL)
  expect_equal(captured_url, paste0(surf_url, "/convert"))

  # SURF_API_URL set -> surf at that address
  withr::local_envvar(SURF_API_URL = "https://surf.example.org/api")
  captured_url <- NULL
  tryCatch(convert_bibr(file_path, save_path), error = function(e) NULL)
  expect_equal(captured_url, "https://surf.example.org/api/convert")

  # BIBR_API_URL set -> selfhosted (no auth needed)
  withr::local_envvar(BIBR_API_URL = selfhosted_url)
  captured_url <- NULL
  tryCatch(convert_bibr(file_path, save_path), error = function(e) NULL)
  expect_true(startsWith(captured_url, selfhosted_url))

  # explicit api_url -> selfhosted, as convert() passes for a local bibr
  withr::local_envvar(BIBR_API_URL = "")
  captured_url <- NULL
  tryCatch(convert_bibr(file_path, save_path, api_url = "http://localhost:8001"),
           error = function(e) NULL)
  expect_true(startsWith(captured_url, "http://localhost:8001/papers/extract"))

  # SCIVRS_API_KEY set -> scivrs
  withr::local_envvar(SCIVRS_API_KEY = "sv_test")
  captured_url <- NULL
  tryCatch(convert_bibr(file_path, save_path), error = function(e) NULL)
  expect_true(startsWith(captured_url, scivrs_url))

  # explicit api_key -> scivrs
  withr::local_envvar(SCIVRS_API_KEY = "")
  captured_url <- NULL
  tryCatch(
    convert_bibr(file_path, save_path, api_key = "sv_explicit"),
    error = function(e) NULL
  )
  expect_true(startsWith(captured_url, scivrs_url))
})

test_that("convert_bibr surf backend sends the PDF to /convert", {
  file_path <- demofile("pdf")
  save_path <- withr::local_tempdir()

  captured <- NULL
  httr2::local_mocked_responses(function(req) {
    captured <<- req
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw('{"paper_id":"abc","source":{"file_name":"input.pdf"},"metadata":{"title":"T"}}')
    )
  })

  json_path <- convert_bibr(file_path, save_path, backend = "surf",
                            api_url = "https://surf.example.org/api")
  expect_equal(captured$url, "https://surf.example.org/api/convert")
  expect_equal(httr2::req_get_method(captured), "POST")
  expect_named(captured$body$data, "file")
  # no key is sent
  expect_null(captured$headers$Authorization)

  # saved under the PDF's name, with the PDF's own name in the JSON
  # (the server calls every upload input.pdf)
  expect_equal(json_path, file.path(save_path, "to_err_is_human.json"))
  json <- jsonlite::read_json(json_path)
  expect_equal(json$source$file_name, "to_err_is_human.pdf")
  expect_equal(json$metadata$title, "T")

  # page and figure settings are not used, and say so
  expect_warning(
    convert_bibr(file_path, save_path, backend = "surf", start_page = 2),
    "not used"
  )
  expect_warning(
    convert_bibr(file_path, save_path, backend = "surf", include_figures = TRUE),
    "not used"
  )
})

test_that("convert_bibr surf backend errors", {
  save_path <- withr::local_tempdir()

  # only PDFs, refused before anything is sent
  httr2::local_mocked_responses(function(req) stop("nothing should be sent"))
  expect_error(convert_bibr(demofile("docx"), save_path, backend = "surf"),
               "converts only PDF files")

  # the server's reason is passed on
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 422,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw('{"error":"bibr could not read the PDF"}')
    )
  })
  expect_error(convert_bibr(demofile("pdf"), save_path, backend = "surf"),
               "HTTP 422.*bibr could not read the PDF")

  # an answer without JSON gives the HTTP status
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 503)
  })
  expect_error(convert_bibr(demofile("pdf"), save_path, backend = "surf"),
               "HTTP 503")
})

test_that("convert_bibr surf backend converts", {
  surf_url <- "https://metacheckserve.metacheck.src.surf-hosted.nl/api"
  skip_api(paste0(surf_url, "/health"))
  skip_if_quick()

  file_path <- demofile("pdf")
  save_path <- withr::local_tempdir()
  json_path <- convert_bibr(file_path, save_path, backend = "surf")
  expect_match(json_path, "to_err_is_human\\.json")
  expect_true(file.exists(json_path))
})

test_that(".bibr_isalive", {
  expect_true(is.function(metacheck:::.bibr_isalive))

  expect_error(.bibr_isalive())

  # not a url
  api_url <- "bibr"
  expect_error(.bibr_isalive(api_url))
  expect_false(.bibr_isalive(api_url, error = FALSE))

  # url, not bibr
  api_url <- "https://google.com"
  expect_error(.bibr_isalive(api_url))
  expect_false(.bibr_isalive(api_url, error = FALSE))

  # TODO: mock this
  skip_api(bibr_url)

  api_url <- bibr_url
  alive <- .bibr_isalive(api_url, error = FALSE)
  expect_in(alive, c(TRUE, FALSE))

  # wrong API key
  api_key <- "XXX"
  expect_error(.bibr_isalive(api_url, api_key),
               "API key is not valid")
  alive <- .bibr_isalive(api_url, api_key, error = FALSE)
  expect_false(alive)
})


test_that("format_bib_authors", {
  expect_true(is.function(metacheck::format_bib_authors))
  expect_no_error(helplist <- help(format_bib_authors, metacheck))

  expect_error(format_bib_authors(bad_arg))

  # NULL
  authors <- NULL
  obs <- format_bib_authors(authors)
  exp <- NA_character_
  expect_equal(obs, exp)

  # empty df
  authors <- data.frame(given = character(0),
                        family = character(0))
  obs <- format_bib_authors(authors)
  exp <- NA_character_
  expect_equal(obs, exp)

  # 1 df
  authors <- data.frame(given = c("Alice H.", "Wendy"),
                        family = c("Eagly", "Wood"))
  obs <- format_bib_authors(authors)
  exp <- "Eagly, Alice H.; Wood, Wendy"
  expect_equal(obs, exp)

  # list of dfs
  authors <- list(
    data.frame(given = c("Alice H.", "Wendy"),
               family = c("Eagly", "Wood")),
    data.frame(given = "Lisa",
               family = "DeBruine")
  )
  obs <- format_bib_authors(authors)
  exp <- c("Eagly, Alice H.; Wood, Wendy",
           "DeBruine, Lisa")
  expect_equal(obs, exp)

  # character vector
  authors <- c("DeBruine, L", "Lakens, D")
  obs <- format_bib_authors(authors)
  exp <- c("DeBruine, L; Lakens, D")
  expect_equal(obs, exp)

  # list of character vectors
  authors <- list(
    c("DeBruine, L", "Lakens, D"),
    "Werner, J"
  )
  obs <- format_bib_authors(authors)
  exp <- c("DeBruine, L; Lakens, D", "Werner, J")
  expect_equal(obs, exp)
})


test_that("convert_bibr selfhosted sends the bearer token and timeout (#490)", {
  file_path <- demofile("pdf")
  seen <- list()
  httr2::local_mocked_responses(function(req) {
    seen$auth <<- !is.null(req$headers$Authorization)
    seen$timeout <<- req$options$timeout_ms
    httr2::response_json(body = list(status = "ok"))
  })
  save_path <- withr::local_tempdir()

  withr::local_envvar(BIBR_API_KEY = "test-token")
  convert_bibr(file_path, save_path, backend = "selfhosted", timeout = 900)
  expect_true(seen$auth)
  expect_equal(seen$timeout, 900 * 1000)

  seen <- list()
  withr::local_envvar(BIBR_API_KEY = "")
  convert_bibr(file_path, save_path, backend = "selfhosted")
  expect_false(seen$auth)
})

test_that(".bibr_isalive accepts a self-hosted bibr /ready answer (#491)", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(
      status = "ready",
      checks = list(ocr = "ok", classifiers = "ok", redis = "ok")
    ))
  })
  expect_true(.bibr_isalive("http://localhost:8000", NULL, error = FALSE))

  # the platform's answer, with a failing bibr check, is still "not ready"
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(status = "ready",
                                     checks = list(bibr = "error")))
  })
  expect_false(.bibr_isalive("http://localhost:8000", NULL, error = FALSE))
})

test_that(".surf_isalive", {
  expect_true(is.function(metacheck:::.surf_isalive))
  api_url <- "https://surf.example.org/api"

  # ready
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(status = "ok", bibr = TRUE))
  })
  expect_true(.surf_isalive(api_url))

  # its bibr is not ready, but it has a GROBID of its own, which its /convert
  # then uses
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(status = "ok", bibr = FALSE, grobid = "local"))
  })
  expect_true(.surf_isalive(api_url))

  # up, but neither its bibr nor a GROBID of its own
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(status = "ok", bibr = FALSE, grobid = "remote"))
  })
  expect_false(.surf_isalive(api_url, error = FALSE))
  expect_error(.surf_isalive(api_url), "bibr is not ready")

  # an error status
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 502)
  })
  expect_false(.surf_isalive(api_url, error = FALSE))
  expect_error(.surf_isalive(api_url), "HTTP 502")

  # not a url
  expect_false(.surf_isalive("surf", error = FALSE))
  expect_error(.surf_isalive("surf"), "Connection to the SURF API failed")
})
