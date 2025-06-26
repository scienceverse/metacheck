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
  skip_on_cran()
  skip_if_offline(grobid_server)

  filename <- demopdf()
  first_sentence <- "Although intentional dishonestly might be a successful way to boost creativity"
  last_sentence <- "We conclude the use of automated checks has potential to reduce the number of mistakes in scientific manuscripts"

  xml <- pdf2grobid(filename, NULL)
  expect_s3_class(xml, "xml_document")
  body <- xml2::xml_find_all(xml, "//body") |> xml2::xml_text()
  expect_true(grepl(first_sentence, body))
  expect_true(grepl(last_sentence, body))

  file.remove(list.files(tempdir(), "\\.xml", full.names = TRUE))

  # save to tempdir
  xml_file <- pdf2grobid(filename, tempdir())
  exp <- file.path(tempdir(), "to_err_is_human.xml")
  expect_equal(xml_file, exp)
  xml2 <- read_grobid_xml(xml_file)

  # fails if when is not identical, so remove it
  when <- "when=\"\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}\\+0000\""
  xml_txt <- sub(when, "", xml)
  xml2_txt <- sub(when, "", xml2)
  expect_equal(xml_txt, xml2_txt)

  # parameters
  # https://grobid.readthedocs.io/en/latest/Grobid-service/

  default_params <- list(
    start=-1,
    end=-1,
    consolidateCitations=0, # 0, 1, 2
    consolidateHeader=0, # 0, 1, 2, 3
    consolidateFunders=0, # 0, 1, 2
    includeRawAffiliations=0,
    includeRawCitations=0,
    includeRawCopyrights=0,
    teiCoordinates=list(),
    segmentSentences=0,
    generateIDs=0,
    flavor=NULL # https://grobid.readthedocs.io/en/latest/Grobid-specialized-processes/
  )

  # reference consolidation
  ref <- get_refs(xml)
  xml_cite0 <- pdf2grobid(filename, NULL, consolidateCitations = 0)
  xml_cite1 <- pdf2grobid(filename, NULL, consolidateCitations = 1)
  xml_cite2 <- pdf2grobid(filename, NULL, consolidateCitations = 2)
  ref0 <- get_refs(xml_cite0)
  ref1 <- get_refs(xml_cite1)
  ref2 <- get_refs(xml_cite2)

  wrongtitle <- "Equivalence testing for psychological research"
  righttitle <- "Equivalence Testing for Psychological Research: A Tutorial"
  expect_equal(ref$references$title[[3]], righttitle)
  expect_equal(ref0$references$title[[3]], wrongtitle)
  expect_equal(ref1$references$title[[3]], righttitle)
  expect_equal(ref2$references$title[[3]], wrongtitle)

  rightauthors <- "Daniël Lakens, Anne M Scheel, Peder M Isager"
  wrongauthors <- "D Lakens"
  expect_equal(ref$references$authors[[3]], rightauthors)
  expect_equal(ref0$references$authors[[3]], wrongauthors)
  expect_equal(ref1$references$authors[[3]], rightauthors)
  expect_equal(ref2$references$authors[[3]], wrongauthors)

  # change start and end pages
  xml3 <- pdf2grobid(filename, NULL, start = 2, end = 3)
  body <- xml2::xml_find_all(xml3, "//body") |> xml2::xml_text()
  expect_false(grepl(first_sentence, body))
  expect_true(grepl("^\\s*Results", body))
  expect_true(grepl(last_sentence, body))

  xml4 <- pdf2grobid(filename, NULL, start = 2, end = 2)
  body <- xml2::xml_find_all(xml4, "//body") |> xml2::xml_text()
  expect_false(grepl(first_sentence, body))
  expect_true(grepl("^\\s*Results", body))
  expect_false(grepl(last_sentence, body))

  # clean up
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
