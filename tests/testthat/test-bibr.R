test_that("bibr_convert selfhosted backend", {
  expect_true(is.function(metacheck::bibr_convert))
  expect_no_error(helplist <- help(bibr_convert, metacheck))

  expect_error(bibr_convert(bad_arg))

  skip_api("localhost:8000")
  skip_if_quick()

  # pdf
  file_name <- "to_err_is_human.pdf"
  file_path <- test_path("fixtures", "formats", file_name)
  save_dir <- withr::local_tempdir()
  json_path <- bibr_convert(file_path, save_dir, backend = "selfhosted")
  expect_match(json_path, "to_err_is_human\\.json")
  expect_true(file.exists(json_path) |> all())
  pdf <- read_bibr(json_path)
  expect_equal(pdf$info$file_name, file_name)
  expect_match(pdf$paper_id, "^[a-f0-9]{16}$")

  # docx
  file_name <- "to_err_is_human.docx"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- bibr_convert(file_path, save_path, backend = "selfhosted")
  expect_true(file.exists(json_path) |> all())
  expect_match(json_path, "to_err_is_human\\.json")
  docx <- read_bibr(json_path)
  expect_equal(docx$info$file_name, file_name)
  expect_match(docx$paper_id, "^[a-f0-9]{16}$")

  # doc
  file_name <- "to_err_is_human.doc"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- bibr_convert(file_path, save_path, backend = "selfhosted")
  expect_true(file.exists(json_path) |> all())
  expect_match(json_path, "to_err_is_human\\.json")
  doc <- read_bibr(json_path)
  expect_equal(doc$info$file_name, file_name)
  expect_match(doc$paper_id, "^[a-f0-9]{16}$")

  # multiple files
  file_name <- c("to_err_is_human.pdf", "published.pdf")
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- bibr_convert(file_path, save_path, backend = "selfhosted")
  expect_match(json_path[[1]], "to_err_is_human\\.json")
  expect_match(json_path[[2]], "published\\.json")
  expect_true(file.exists(json_path) |> all())

})

test_that("bibr_convert selfhosted sends include_figures in request", {
  file_path <- test_path("fixtures", "formats", "to_err_is_human.pdf")

  captured_body <- NULL
  httr2::local_mocked_responses(function(req) {
    captured_body <<- req$body$data
    httr2::response_json(body = list(status = "ok"))
  })

  save_dir <- withr::local_tempdir()
  tryCatch(
    bibr_convert(file_path, save_dir, backend = "selfhosted",
                 include_figures = TRUE),
    error = function(e) NULL
  )
  expect_equal(captured_body$include_figures, "true")

  captured_body <- NULL
  tryCatch(
    bibr_convert(file_path, save_dir, backend = "selfhosted",
                 include_figures = FALSE),
    error = function(e) NULL
  )
  expect_equal(captured_body$include_figures, "false")
})

test_that("bibr_convert selfhosted sends page params in request", {
  file_path <- test_path("fixtures", "formats", "to_err_is_human.pdf")

  captured_body <- NULL
  httr2::local_mocked_responses(function(req) {
    captured_body <<- req$body$data
    httr2::response_json(body = list(status = "ok"))
  })

  save_dir <- withr::local_tempdir()
  tryCatch(
    bibr_convert(file_path, save_dir, backend = "selfhosted",
                 start_page = 3, end_page = 10),
    error = function(e) NULL
  )
  # zero-based: start_page 3 -> 2, end_page 10 -> 9
  expect_equal(captured_body$start_page, "2")
  expect_equal(captured_body$end_page, "9")

  # defaults omit page fields entirely
  captured_body <- NULL
  tryCatch(
    bibr_convert(file_path, save_dir, backend = "selfhosted",
),
    error = function(e) NULL
  )
  expect_null(captured_body$start_page)
  expect_null(captured_body$end_page)

  # start_page = 1 is default, still omitted
  captured_body <- NULL
  tryCatch(
    bibr_convert(file_path, save_dir, backend = "selfhosted",
                 start_page = 1, end_page = 5),
    error = function(e) NULL
  )
  expect_null(captured_body$start_page)
  expect_equal(captured_body$end_page, "4")
})


test_that("bibr_convert scivrs backend", {
  skip_api("platform.metacheck.app")
  skip_if_quick()

  # pdf
  file_name <- "to_err_is_human.pdf"
  file_path <- test_path("fixtures", "formats", file_name)
  save_dir <- withr::local_tempdir()
  json_path <- bibr_convert(file_path, save_dir, backend = "scivrs")
  expect_match(json_path, "to_err_is_human\\.json")
  expect_true(file.exists(json_path) |> all())
  pdf <- read_bibr(json_path)
  expect_equal(pdf$info$file_name, file_name)
  #expect_match(pdf$paper_id, "^[a-f0-9]{16}$")

  # docx
  file_name <- "to_err_is_human.docx"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- bibr_convert(file_path, save_path, backend = "scivrs")
  expect_true(file.exists(json_path) |> all())
  expect_match(json_path, "to_err_is_human\\.json")
  docx <- read_bibr(json_path)
  expect_equal(docx$info$file_name, file_name)
  #expect_match(docx$paper_id, "^[a-f0-9]{16}$")

  # doc
  file_name <- "to_err_is_human.doc"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- bibr_convert(file_path, save_path, backend = "scivrs")
  expect_true(file.exists(json_path) |> all())
  expect_match(json_path, "to_err_is_human\\.json")
  doc <- read_bibr(json_path)
  expect_equal(doc$info$file_name, file_name)
  #expect_match(doc$paper_id, "^[a-f0-9]{16}$")

  # multiple files
  file_name <- c("to_err_is_human.pdf", "published.pdf")
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- bibr_convert(file_path, save_path, backend = "scivrs")
  expect_match(json_path[[1]], "to_err_is_human\\.json")
  expect_match(json_path[[2]], "published\\.json")
  expect_true(file.exists(json_path) |> all())

})

test_that("bibr_convert scivrs sends include_figures in request", {
  file_path <- test_path("fixtures", "formats", "to_err_is_human.pdf")

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

  save_dir <- withr::local_tempdir()
  withr::local_envvar(SCIVRS_API_KEY = "sv_test_key")
  tryCatch(
    bibr_convert(file_path, save_dir, backend = "scivrs", include_figures = TRUE),
    error = function(e) NULL
  )
  expect_equal(captured_body$include_figures, "true")

  captured_body <- NULL
  tryCatch(
    bibr_convert(file_path, save_dir, backend = "scivrs", include_figures = FALSE),
    error = function(e) NULL
  )
  expect_equal(captured_body$include_figures, "false")
})

test_that("bibr_convert scivrs sends page params in request", {
  file_path <- test_path("fixtures", "formats", "to_err_is_human.pdf")

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

  save_dir <- withr::local_tempdir()
  withr::local_envvar(SCIVRS_API_KEY = "sv_test_key")
  tryCatch(
    bibr_convert(file_path, save_dir, backend = "scivrs",
                 start_page = 2, end_page = 5),
    error = function(e) NULL
  )
  # zero-based: start_page 2 -> 1, end_page 5 -> 4
  expect_equal(captured_body$start_page, "1")
  expect_equal(captured_body$end_page, "4")

  # defaults omit page fields
  captured_body <- NULL
  tryCatch(
    bibr_convert(file_path, save_dir, backend = "scivrs"),
    error = function(e) NULL
  )
  expect_null(captured_body$start_page)
  expect_null(captured_body$end_page)
})

test_that("bibr_convert auto-detects backend", {
  withr::local_envvar(SCIVRS_API_KEY = "")

  # match the default URLs resolved inside bibr_convert()
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
  file_path <- test_path("fixtures", "formats", "to_err_is_human.pdf")
  save_dir <- withr::local_tempdir()

  # no key set -> selfhosted (no auth needed)
  tryCatch(bibr_convert(file_path, save_dir), error = function(e) NULL)
  expect_true(startsWith(captured_url, selfhosted_url))

  # SCIVRS_API_KEY set -> scivrs
  withr::local_envvar(SCIVRS_API_KEY = "sv_test")
  captured_url <- NULL
  tryCatch(bibr_convert(file_path, save_dir), error = function(e) NULL)
  expect_true(startsWith(captured_url, scivrs_url))

  # explicit api_key -> scivrs
  withr::local_envvar(SCIVRS_API_KEY = "")
  captured_url <- NULL
  tryCatch(
    bibr_convert(file_path, save_dir, api_key = "sv_explicit"),
    error = function(e) NULL
  )
  expect_true(startsWith(captured_url, scivrs_url))
})

test_that("platform_bibr_convert is deprecated", {
  expect_true(is.function(metacheck::platform_bibr_convert))
  withr::local_envvar(SCIVRS_API_KEY = "sv_test_key")
  httr2::local_mocked_responses(function(req) {
    if (grepl("/jobs$", req$url)) {
      httr2::response_json(body = list(job_id = "test-job"))
    } else if (grepl("/result$", req$url)) {
      httr2::response_json(body = list(text = "mock"))
    } else {
      httr2::response_json(body = list(status = "complete"))
    }
  })
  file_path <- test_path("fixtures", "formats", "to_err_is_human.pdf")
  save_dir <- withr::local_tempdir()
  expect_warning(
    tryCatch(
      platform_bibr_convert(file_path, save_dir),
      error = function(e) NULL
    ),
    "deprecated"
  )
})


test_that("read_bibr", {
  expect_true(is.function(metacheck::read_bibr))
  expect_no_error(helplist <- help(read_bibr, metacheck))

  expect_error(read_bibr(bad_arg))

  # single paper from legacy zip
  file_path <- test_path("fixtures", "formats", "to_err_is_human.json")
  paper <- read_bibr(file_path)

  expect_true(paper_validate(paper))
  #expect_match(paper$paper_id, "^[a-f0-9]{14,16}$")
})

test_that("read", {
  expect_true(is.function(metacheck::read))
  expect_no_error(helplist <- help(read, metacheck))

  expect_error(read(bad_arg))
})

test_that("read - single paper", {
  file_path <- test_path("fixtures", "formats", "to_err_is_human.json")
  paper <- read(file_path)

  expect_s3_class(paper, "scivrs_paper")
  expect_true(paper_validate(paper))
  #expect_match(paper$paper_id, "^[a-f0-9]{14,16}$")

  # check for no urls ending in .
  end_dot <- grepl("\\.$", paper$url$href)
  expect_true(all(!end_dot))

  #expect_true(all(!is.na(paper$figure$image)))
})

test_that("read - no images", {
  file_path <- test_path("fixtures", "formats", "to_err_is_human.json")
  paper <- read(file_path, include_images = FALSE)

  expect_true(paper_validate(paper))
  expect_true(all(is.na(paper$figure$image)))
})

test_that("read - vector of paths", {
  file_path <- test_path("fixtures", "formats", "to_err_is_human.json")
  paper <- read(file_path)

  file_path <- c(
    test_path("fixtures", "formats", "to_err_is_human.json"),
    test_path("fixtures", "psychsci", "0956797613520608.json")
  )
  papers <- read(file_path)
  expect_s3_class(papers, "scivrs_paperlist")
  expect_true(paper_validate(papers[[1]]))
  expect_true(paper_validate(papers[[2]]))

  expect_equal(paper, papers[[1]])
  expect_equal(names(papers),
               c(papers[[1]]$paper_id, papers[[2]]$paper_id))
})

test_that("read - directory", {
  file_path <- test_path("fixtures", "psychsci")
  papers <- read(file_path)
  expect_s3_class(papers, "scivrs_paperlist")
  expect_true(paper_validate(papers[[1]]))
  expect_true(paper_validate(papers[[2]]))
  expect_true(paper_validate(papers[[3]]))
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
