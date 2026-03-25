test_that("bibr_convert", {
  expect_true(is.function(metacheck::bibr_convert))
  expect_no_error(helplist <- help(bibr_convert, metacheck))

  expect_error(bibr_convert(bad_arg))

  skip_api("api.bibr.metacheck.app")
  skip_if_quick()

  # pdf
  file_name <- "to_err_is_human.pdf"
  file_path <- test_path("fixtures", "formats", file_name)
  save_dir <- withr::local_tempdir()
  json_path <- bibr_convert(file_path, save_dir)
  expect_match(json_path, "to_err_is_human\\.json")
  expect_true(file.exists(json_path) |> all())
  pdf <- read_bibr(json_path)
  expect_equal(pdf$info$file_name, file_name)
  expect_match(pdf$paper_id, "^[a-f0-9]{16}$")

  # docx
  file_name <- "to_err_is_human.docx"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- bibr_convert(file_path, save_path)
  expect_true(file.exists(json_path) |> all())
  expect_match(json_path, "to_err_is_human\\.json")
  docx <- read_bibr(json_path)
  expect_equal(docx$info$file_name, file_name)
  expect_match(docx$paper_id, "^[a-f0-9]{16}$")

  # doc
  file_name <- "to_err_is_human.doc"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- bibr_convert(file_path, save_path)
  expect_true(file.exists(json_path) |> all())
  expect_match(json_path, "to_err_is_human\\.json")
  doc <- read_bibr(json_path)
  expect_equal(doc$info$file_name, file_name)
  expect_match(doc$paper_id, "^[a-f0-9]{16}$")

  # multiple files
  file_name <- c("to_err_is_human.pdf", "published.pdf")
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- bibr_convert(file_path, save_path)
  expect_match(json_path[[1]], "to_err_is_human\\.json")
  expect_match(json_path[[2]], "published\\.json")
  expect_true(file.exists(json_path) |> all())
})


test_that("platform_bibr_convert", {
  expect_true(is.function(metacheck::platform_bibr_convert))
  expect_no_error(helplist <- help(platform_bibr_convert, metacheck))

  expect_error(platform_bibr_convert(bad_arg))

  skip_api("platform.metacheck.app")
  skip_if_quick()

  # pdf
  file_name <- "to_err_is_human.pdf"
  file_path <- test_path("fixtures", "formats", file_name)
  save_dir <- withr::local_tempdir()
  json_path <- platform_bibr_convert(file_path, save_dir)
  expect_match(json_path, "to_err_is_human\\.json")
  expect_true(file.exists(json_path) |> all())
  pdf <- read_bibr(json_path)
  expect_equal(pdf$info$file_name, file_name)
  #expect_match(pdf$paper_id, "^[a-f0-9]{16}$")

  # docx
  file_name <- "to_err_is_human.docx"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- platform_bibr_convert(file_path, save_path)
  expect_true(file.exists(json_path) |> all())
  expect_match(json_path, "to_err_is_human\\.json")
  docx <- read_bibr(json_path)
  expect_equal(docx$info$file_name, file_name)
  #expect_match(docx$paper_id, "^[a-f0-9]{16}$")

  # doc
  file_name <- "to_err_is_human.doc"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- platform_bibr_convert(file_path, save_path)
  expect_true(file.exists(json_path) |> all())
  expect_match(json_path, "to_err_is_human\\.json")
  doc <- read_bibr(json_path)
  expect_equal(doc$info$file_name, file_name)
  #expect_match(doc$paper_id, "^[a-f0-9]{16}$")

  # multiple files
  file_name <- c("to_err_is_human.pdf", "published.pdf")
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  json_path <- platform_bibr_convert(file_path, save_path)
  expect_match(json_path[[1]], "to_err_is_human\\.json")
  expect_match(json_path[[2]], "published\\.json")
  expect_true(file.exists(json_path) |> all())
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

