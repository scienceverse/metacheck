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
  zip_path <- bibr_convert(file_path, save_dir)
  expect_match(zip_path, "to_err_is_human\\.zip")
  expect_true(file.exists(zip_path) |> all())
  pdf <- read_bibr(zip_path)
  expect_equal(pdf$info$file_name, file_name)
  expect_match(pdf$paper_id, "^[a-f0-9]{16}$")

  # docx
  file_name <- "to_err_is_human.docx"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  zip_path <- bibr_convert(file_path, save_path)
  expect_true(file.exists(zip_path) |> all())
  expect_match(zip_path, "to_err_is_human\\.zip")
  docx <- read_bibr(zip_path)
  expect_equal(docx$info$file_name, file_name)
  expect_match(docx$paper_id, "^[a-f0-9]{16}$")

  # doc
  file_name <- "to_err_is_human.doc"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  zip_path <- bibr_convert(file_path, save_path)
  expect_true(file.exists(zip_path) |> all())
  expect_match(zip_path, "to_err_is_human\\.zip")
  doc <- read_bibr(zip_path)
  expect_equal(doc$info$file_name, file_name)
  expect_match(doc$paper_id, "^[a-f0-9]{16}$")

  # html
  # file_name <- "to_err_is_human.html"
  # file_path <- test_path("fixtures", "formats", file_name)
  # save_path <- withr::local_tempdir()
  # zip_path <- bibr_convert(file_path, save_path)
  # expect_true(file.exists(zip_path) |> all())
  # expect_match(zip_path, "to_err_is_human\\.zip")
  # html <- read_bibr(zip_path)
  # expect_equal(html$info$file_name, file_name)
  # expect_match(html$paper_id, "^[a-f0-9]{14}$")

  # multiple files
  file_name <- c("to_err_is_human.pdf", "published.pdf")
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  zip_path <- bibr_convert(file_path, save_path)
  expect_match(zip_path[[1]], "to_err_is_human\\.zip")
  expect_match(zip_path[[2]], "published\\.zip")
  expect_true(file.exists(zip_path) |> all())
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
  zip_path <- platform_bibr_convert(file_path, save_dir)
  expect_match(zip_path, "to_err_is_human\\.zip")
  expect_true(file.exists(zip_path) |> all())
  pdf <- read_bibr(zip_path)
  expect_equal(pdf$info$file_name, file_name)
  expect_match(pdf$paper_id, "^[a-f0-9]{16}$")

  # docx
  file_name <- "to_err_is_human.docx"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  zip_path <- platform_bibr_convert(file_path, save_path)
  expect_true(file.exists(zip_path) |> all())
  expect_match(zip_path, "to_err_is_human\\.zip")
  docx <- read_bibr(zip_path)
  expect_equal(docx$info$file_name, file_name)
  expect_match(docx$paper_id, "^[a-f0-9]{16}$")

  # doc
  file_name <- "to_err_is_human.doc"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  zip_path <- platform_bibr_convert(file_path, save_path)
  expect_true(file.exists(zip_path) |> all())
  expect_match(zip_path, "to_err_is_human\\.zip")
  doc <- read_bibr(zip_path)
  expect_equal(doc$info$file_name, file_name)
  expect_match(doc$paper_id, "^[a-f0-9]{16}$")

  # multiple files
  file_name <- c("to_err_is_human.pdf", "published.pdf")
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  zip_path <- platform_bibr_convert(file_path, save_path)
  expect_match(zip_path[[1]], "to_err_is_human\\.zip")
  expect_match(zip_path[[2]], "published\\.zip")
  expect_true(file.exists(zip_path) |> all())
})


test_that("read_bibr", {
  expect_true(is.function(metacheck::read_bibr))
  expect_no_error(helplist <- help(read_bibr, metacheck))

  expect_error(read_bibr(bad_arg))

  # single paper
  file_path <- test_path("fixtures", "bibr", "to_err_is_human.zip")
  paper <- read_bibr(file_path)

  expect_s3_class(paper, "scivrs_paper")
  obs <- names(paper)
  exp <- c("paper_id", "info", "authors", "text", "links", "tables",
           "sections", "bib", "xrefs", "figures", "equations")
  expect_contains(obs, exp)
  expect_match(paper$paper_id, "^[a-f0-9]{14,16}$")

})

test_that("read", {
  expect_true(is.function(metacheck::read))
  expect_no_error(helplist <- help(read, metacheck))

  expect_error(read(bad_arg))

  # single paper
  file_path <- test_path("fixtures", "bibr", "to_err_is_human.zip")
  paper <- read(file_path)

  expect_s3_class(paper, "scivrs_paper")
  expect_true(validate_paper(paper))
  expect_match(paper$paper_id, "^[a-f0-9]{14,16}$")

  # check for no links ending in .
  end_dot <- grepl("\\.$", paper$links$url)
  expect_true(all(!end_dot))

  # vector of paths
  file_path <- c(
    test_path("fixtures", "formats", "to_err_is_human.zip"),
    test_path("fixtures", "psychsci", "0956797613520608.zip")
  )
  papers <- read(file_path)
  expect_s3_class(papers, "scivrs_paperlist")
  expect_s3_class(papers[[1]], "scivrs_paper")
  expect_s3_class(papers[[2]], "scivrs_paper")

  expect_equal(paper, papers[[1]])
  expect_equal(names(papers),
               c(papers[[1]]$paper_id, papers[[2]]$paper_id))

  # directory
  file_path <- test_path("fixtures", "psychsci")
  ps <- read(file_path)
  expect_s3_class(ps, "scivrs_paperlist")
  expect_s3_class(ps[[1]], "scivrs_paper")
})
