test_that("convert", {
  expect_true(is.function(metacheck::convert))
  expect_no_error(helplist <- help(convert, metacheck))

  expect_error(convert(bad_arg))

  # no files
  file_path <- file.path(withr::local_tempdir(), "emptydir")
  dir.create(file_path)
  expect_error(convert(file_path), "No PDF, XML, DOC or DOCX files detected")

  # no relevant files
  txt_path <- file.path(file_path, "hi.txt")
  write("hi", txt_path)
  expect_error(convert(file_path), "No PDF, XML, DOC or DOCX files detected")
})

test_that("XML-auto", {
  file_path <- demofile("xml")
  save_path <- withr::local_tempdir()

  obs <- convert(file_path, save_path)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
})

test_that("XML-grobid", {
  file_path <- demofile("xml")
  save_path <- withr::local_tempdir()
  method <- "grobid"

  obs <- convert(file_path, save_path, method)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))

  # no bib_match unless crossref_lookup = TRUE
  expect_disjoint("bib_match", names(paper))
})

test_that("XML-crossref", {
  file_path <- demofile("xml")
  save_path <- withr::local_tempdir()

  obs <- convert(file_path, save_path, crossref_lookup = TRUE)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
  expect_in("bib_match", names(paper))
}, "mock")

test_that("PDF-auto", {
  file_path <- demofile("pdf")
  save_path <- withr::local_tempdir()

  obs <- convert(file_path, save_path, api_url = grobid_url)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
}, "mock")

test_that("PDF-grobid", {
  file_path <- demofile("pdf")
  save_path <- withr::local_tempdir()
  method <- "grobid"

  obs <- convert(file_path, save_path, method, api_url = grobid_url)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))
  # default saved XML
  xml <- sub("\\.json", "\\.xml", obs)
  expect_true(file.exists(xml))

  paper <- read(obs)
  expect_true(paper_validate(paper))
}, "mock")

test_that("PDF-XML-grobid keep_xml = TRUE", {
  file_path <- demofile("pdf")
  save_path <- withr::local_tempdir()
  method <- "grobid"

  obs <- convert(file_path, save_path, method, api_url = grobid_url,
                 keep_xml = TRUE)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))
  xml <- sub("\\.json", "\\.xml", obs)
  expect_true(file.exists(xml))

  paper <- read(obs)
  expect_true(paper_validate(paper))
}, "mock")

test_that("PDF-XML-grobid keep_xml = FALSE", {
  file_path <- demofile("pdf")
  save_path <- withr::local_tempdir()
  method <- "grobid"

  obs <- convert(file_path, save_path, method, api_url = grobid_url,
                 keep_xml = FALSE)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))
  xml <- sub("\\.json", "\\.xml", obs)
  expect_false(file.exists(xml))

  paper <- read(obs)
  expect_true(paper_validate(paper))
  # named after the PDF, not after a random temporary file
  expect_equal(basename(obs), "to_err_is_human.json")
  expect_equal(paper$paper_id, "to_err_is_human")
}, "mock")

test_that("PDF-bibr", {
  skip_api(bibr_url)
  skip_if_not(.bibr_isalive(bibr_url, error = FALSE),
              message = "bibr not available")

  file_path <- demofile("pdf")
  save_path <- withr::local_tempdir()
  method <- "bibr"

  obs <- convert(file_path, save_path, method)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
})

test_that("DOC-auto", {
  skip_api(bibr_url)
  skip_if_not(.bibr_isalive(bibr_url, error = FALSE),
              message = "bibr not available")

  file_path <- demofile("doc")
  save_path <- withr::local_tempdir()

  obs <- convert(file_path, save_path)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
})

test_that("DOCX-auto", {
  skip_api(bibr_url)
  skip_if_not(.bibr_isalive(bibr_url, error = FALSE),
              message = "bibr not available")

  file_path <- demofile("doc")
  save_path <- withr::local_tempdir()

  obs <- convert(file_path, save_path)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
})


test_that("convert(method = 'bibr') passes its arguments to convert_bibr (#489)", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(paper_id = "abc"))
  })
  save_path <- withr::local_tempdir()
  json_path <- convert(demofile("pdf"), save_path, method = "bibr",
                       backend = "selfhosted",
                       api_url = "http://localhost:8000")
  expect_true(file.exists(json_path))
})

test_that("convert uses the SURF API first", {
  withr::local_envvar(SURF_API_URL = "https://surf.example.org/api")
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("/health$", req$url)) {
      httr2::response_json(body = list(status = "ok", bibr = TRUE))
    } else {
      httr2::response_json(body = list(paper_id = "abc",
                                       source = list(file_name = "input.pdf")))
    }
  })
  save_path <- withr::local_tempdir()

  # auto, bibr and grobid: SURF first (it returns the paper JSON in each case)
  for (method in c("auto", "bibr", "grobid")) {
    urls <- character(0)
    expect_message(
      json_path <- convert(demofile("pdf"), save_path, method = method),
      "Using the SURF API"
    )
    expect_equal(urls, c("https://surf.example.org/api/health",
                         "https://surf.example.org/api/convert"))
    expect_true(file.exists(json_path))
  }

  # a given server, key or backend, or a DOCX file: SURF is not asked
  urls <- character(0)
  convert(demofile("pdf"), save_path, method = "bibr",
          backend = "selfhosted", api_url = "http://localhost:8000")
  expect_false(any(grepl("surf.example.org", urls)))

  urls <- character(0)
  convert(demofile("pdf"), save_path, method = "bibr",
          api_url = "http://localhost:8000")
  expect_false(any(grepl("surf.example.org", urls)))

  urls <- character(0)
  convert(demofile("docx"), save_path, method = "bibr",
          backend = "selfhosted", api_url = "http://localhost:8000")
  expect_false(any(grepl("surf.example.org", urls)))

  # grobid: SURF is not asked (the mocked answer is no grobid XML, so the
  # conversion itself fails with a warning)
  urls <- character(0)
  suppressWarnings(convert(demofile("pdf"), save_path, method = "grobid",
                           api_url = "http://localhost:8070"))
  expect_false(any(grepl("surf.example.org", urls)))
})

test_that("after SURF, convert tries TU/e, then this computer, then the online list", {
  local_mocked_bindings(.convert_servers = function(service = NULL) list(
    list(id = "TUE grobid 0.8.2", service = "grobid", url = "https://grobid.hti.ieis.tue.nl"),
    list(id = "Grobid FULL 0.9.0", service = "grobid", url = "https://grobidOrg-grobid.hf.space")))
  ids <- vapply(.convert_candidates(), `[[`, "", "id")
  expect_equal(ids, c("TUE grobid 0.8.2", "local grobid", "local bibr", "Grobid FULL 0.9.0"))

  # the TU/e server is tried even when the online list cannot be read
  local_mocked_bindings(.convert_servers = function(service = NULL) list())
  cand <- .convert_candidates()
  expect_equal(cand[[1]]$url, "https://grobid.hti.ieis.tue.nl")

  # convert_grobid() without a server: the first GROBID server that answers
  local_mocked_bindings(
    .convert_servers = function(service = NULL) list(
      list(id = "TUE grobid 0.8.2", service = "grobid", url = "https://grobid.hti.ieis.tue.nl"),
      list(id = "Grobid FULL 0.9.0", service = "grobid", url = "https://grobidOrg-grobid.hf.space")),
    .grobid_isalive = function(api_url, error = TRUE) grepl("hf.space", api_url))
  expect_equal(suppressMessages(.grobid_first_alive("https://grobid.hti.ieis.tue.nl")),
               "https://grobidOrg-grobid.hf.space")
})

test_that("convert passes over the SURF API when it cannot convert", {
  withr::local_envvar(SURF_API_URL = "https://surf.example.org/api")
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("/health$", req$url)) {
      httr2::response_json(body = list(status = "ok", bibr = FALSE))
    } else {
      httr2::response_json(body = list(paper_id = "abc"))
    }
  })
  save_path <- withr::local_tempdir()

  # bibr not ready: the given backend is used instead
  expect_no_message(
    convert(demofile("pdf"), save_path, method = "bibr",
            backend = "selfhosted", api_url = "http://localhost:8000"),
    message = "SURF"
  )
  expect_false(any(grepl("surf.example.org/api/convert", urls)))
})
