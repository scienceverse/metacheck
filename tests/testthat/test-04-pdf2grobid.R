test_that("works", {
  expect_true(is.function(pdf2grobid))

  filename <- demoxml()
  expect_error(pdf2grobid(filename, grobid_url = "notawebsite"),
               "The grobid server notawebsite is not available")

  # invalid file type
  skip_if_offline("localhost")
  expect_error(pdf2grobid("no.exist", grobid_url = "localhost"), "does not exist")
})

grobid_server <- "kermitt2-grobid.hf.space"

test_that("defaults", {
  skip_if_offline(grobid_server)

  filename <- demopdf()

  xml <- pdf2grobid(filename, NULL)
  expect_s3_class(xml, "xml_document")

  file.remove(list.files(tempdir(), "\\.xml", full.names = TRUE))
  xml_file <- pdf2grobid(filename, tempdir())
  exp <- file.path(tempdir(), "to_err_is_human.xml")
  expect_equal(xml_file, exp)

  xml2 <- read_grobid_xml(xml_file)
  expect_equal(xml, xml2)
  file.remove(list.files(tempdir(), "\\.xml", full.names = TRUE))
})

test_that("batch", {
  skip_if_offline(grobid_server)

  grobid_dir <- demodir()

  file.remove(list.files(tempdir(), "\\.xml", full.names = TRUE))
  xml_files <- pdf2grobid(grobid_dir, tempdir())
  actual <- list.files(tempdir(), "\\.xml")
  expected <- list.files(grobid_dir, "\\.xml")
  expect_equal(actual, expected)
  file.remove(list.files(tempdir(), "\\.xml", full.names = TRUE))

  filenames <- list.files(grobid_dir, ".pdf", full.names = TRUE)
  xml_files <- pdf2grobid(filenames[2:3], tempdir())
  actual <- list.files(tempdir(), "\\.xml")
  expected <- list.files(grobid_dir, "\\.xml")[2:3]
  expect_equal(actual, expected)
  file.remove(list.files(tempdir(), "\\.xml", full.names = TRUE))
})


test_that("local", {
  skip_on_cran()
  skip_if_offline("localhost:8070")
  local_url <- "http://localhost:8070"

  filename <- demopdf()

  xml <- pdf2grobid(filename, NULL, local_url)
  expect_s3_class(xml, "xml_document")

  file.remove(list.files(tempdir(), "\\.xml", full.names = TRUE))
  xml_file <- pdf2grobid(filename, tempdir(), local_url)
  exp <- file.path(tempdir(), "to_err_is_human.xml")
  expect_equal(xml_file, exp)

  xml2 <- read_grobid_xml(xml_file)
  expect_equal(xml, xml2)
  file.remove(list.files(tempdir(), "\\.xml", full.names = TRUE))
})


# test_that("grobid consistency", {
#   # docker run --rm --init --ulimit core=0 -p 8070:8070 lfoppno/grobid:0.8.1
#   skip("Very long")
#   skip_if_offline("localhost:8070")
#   local_url <- "http://localhost:8070"
#
#   t1 <- file.path(tempdir(), "try1")
#   t2 <- file.path(tempdir(), "try2")
#   dir.create(t1, showWarnings = FALSE)
#   dir.create(t2, showWarnings = FALSE)
#   #files <- list.files("pdf/psyarxiv", full.names = TRUE)
#   files <- list.files("pdf/psychsci/", full.names = TRUE)
#   xml1 <- pdf2grobid(files[1:20], save_path = t1, grobid_url = local_url)
#   xml2 <- pdf2grobid(files[1:20], save_path = t2, grobid_url = local_url)
#
#   # check the identicalness of the XML files
#   f1 <- list.files(t1, full.names = TRUE)
#   f2 <- list.files(t2, full.names = TRUE)
#
#   df <- purrr::map_df(seq_along(f1), function(i) {
#     x1 <- readLines(f1[i]) |>
#       gsub(" (ref|xml:id)=\"#?_[0-9a-zA-Z_ #]{7,20}\"", "", x = _) |>
#       gsub(" when=\"\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}\\+\\d{4}\"", "", x = _)
#     x2 <- readLines(f2[i])|>
#       gsub(" (ref|xml:id)=\"#?_[0-9a-zA-Z_ #]{7,20}\"", "", x = _) |>
#       gsub(" when=\"\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}\\+\\d{4}\"", "", x = _)
#
#     nomatch <- which(x1 != x2)
#
#     data.frame(
#       id = rep(i, length(nomatch)),
#       lines = nomatch,
#       f1 = x1[nomatch],
#       f2 = x2[nomatch]
#     )
#   })
#
#   })
