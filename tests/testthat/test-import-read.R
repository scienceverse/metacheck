
test_that(".read_bibr", {
  expect_true(is.function(metacheck::.read_bibr))
  expect_no_error(helplist <- help(.read_bibr, metacheck))

  expect_error(.read_bibr(bad_arg))

  # single paper from json
  file_path <- demofile("json")
  paper <- .read_bibr(file_path)

  expect_true(paper_validate(paper))
  #expect_match(paper$paper_id, "^[a-f0-9]{14,16}$")
})

test_that("read", {
  expect_true(is.function(metacheck::read))
  expect_no_error(helplist <- help(read, metacheck))

  expect_error(read(bad_arg))

  # no files
  file_path <- file.path(withr::local_tempdir(), "emptydir")
  dir.create(file_path)
  expect_message(paper <- read(file_path))
  expect_equal(length(paper), 0)

  # no relevant files
  txt_path <- file.path(file_path, "hi.txt")
  write("hi", txt_path)
  expect_message(paper <- read(file_path))
  expect_equal(length(paper), 0)
})

test_that("read - json and xml together", {
  # set up temp
  file_path <- file.path(withr::local_tempdir(), "a")
  dir.create(file_path)
  file.copy(demofile("xml"), file.path(file_path, "demo.xml"))
  file.copy(demofile("json"), file.path(file_path, "demo.json"))

  paper <- read(file_path)
  expect_true(.is_paper(paper))
})

test_that("read - recursive", {
  # set up temp
  a <- file.path(withr::local_tempdir(), "a")
  b1 <- file.path(a, "b1")
  b2 <- file.path(a, "b2")
  dir.create(a)
  dir.create(b1)
  dir.create(b2)

  sink <- paper_write(psychsci[[1]], save_path = a )
  sink <- paper_write(psychsci[[1]], save_path = b1)
  sink <- paper_write(psychsci[[2]], save_path = b1)
  sink <- paper_write(psychsci[[3]], save_path = b1)
  sink <- paper_write(psychsci[[3]], save_path = b2)

  # no recursive ----
  paper <- read(a)
  expect_equal(paper$paper_id, psychsci[[1]]$paper_id)

  paper <- read(b1)
  expect_equal(paper_id(paper), paper_id(psychsci[1:3]))

  # recursive ----
  paper <- read(a, recursive = TRUE)
  expect_equal(length(paper), 5)
  # exp <- list.files(a, recursive = TRUE) |>
  #   gsub("\\.json$", "", x = _)
  exp <- paper_id(psychsci[1:3]) |> _[c(1,1,2,3,3)]
  expect_equal(paper_id(paper), exp)
})

test_that("read - single paper", {
  file_path <- demofile("json")
  paper <- read(file_path)

  expect_s3_class(paper, "scivrs_paper")
  expect_true(paper_validate(paper))

  # check for no urls ending in .
  end_dot <- grepl("\\.$", paper$url$href)
  expect_true(all(!end_dot))
  expect_true(all(is.na(paper$figure$image)))
})

# test_that("read - images", {
#   file_path <- system.file("demos/to_err_is_human.json", package = "metacheck")
#   paper <- read(file_path, include_images = TRUE)
#
#   expect_true(paper_validate(paper))
#   expect_true(all(!is.na(paper$figure$image)))
# })

test_that("read - vector of paths", {
  file_path <- demofile("json")
  paper <- read(file_path)

  file_path <- c(
    demofile("json"),
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
