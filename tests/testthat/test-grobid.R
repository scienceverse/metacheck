test_that("grobid_to_bibr", {
  expect_true(is.function(metacheck::grobid_to_bibr))
  expect_no_error(helplist <- help(grobid_to_bibr, metacheck))

  expect_error(grobid_to_bibr(bad_arg))

  xml_file <- test_path("fixtures", "formats", "to_err_is_human.pdf.tei.xml")
  paper <- grobid_to_bibr(xml_file)

  expect_s3_class(paper, "scivrs_paper")
  text_cols <- c("text_id", "paragraph_id", "section_id", "text")
  expect_in(names(paper$text), text_cols)

  expect_equal(paper$bib$doi[[4]], "10.0000/0123456789")
})

test_that("read", {
  expect_true(is.function(metacheck::read))
  expect_no_error(helplist <- help(read, metacheck))

  expect_error(read(bad_arg))

  xml_file <- test_path("fixtures", "formats", "to_err_is_human.xml")
  zip_file <- test_path("fixtures", "formats", "to_err_is_human.zip")
  title <- "To Err is Human: An Empirical Investigation"

  # grobid xml
  obs_xml <- read(xml_file)
  expect_s3_class(obs_xml, "scivrs_paper")
  expect_equal(obs_xml$info$title, title)

  # bibr zip
  obs_zip <- read(zip_file)
  expect_s3_class(obs_zip, "scivrs_paper")
  expect_match(obs_zip$info$title, "To Err is Human")

  # both
  file_path <- c(xml_file, zip_file)
  obs <- read(file_path)
  expect_equal(length(obs), 2)
  expect_s3_class(obs, "scivrs_paperlist")
})

test_that("grobid_convert", {
  expect_true(is.function(metacheck::grobid_convert))
  expect_no_error(helplist <- help(grobid_convert, metacheck))

  expect_error(grobid_convert(bad_arg))
})

test_that("invalid URL error", {
  filename <- test_path("fixtures", "formats", "to_err_is_human.pdf")
  expect_error(grobid_convert(filename, grobid_url = "notawebsite"),
               "grobid_url must be a valid URL, starting with http or https!")

  # URL without http/https detected"
  expect_error(grobid_convert(filename, grobid_url = "kermitt2-grobid.hf.space"),
               "grobid_url must be a valid URL, starting with http or https!")
})


test_that("non-Grobid URL rejected", {
  skip_if_offline("google.com")

  filename <- test_path("fixtures", "formats", "to_err_is_human.pdf")
  expect_error(grobid_convert(filename, grobid_url = "https://google.com"))
})

test_that("missing single file errors", {
  skip_if_offline() # offline error happens before filename error

  filename <- "wrongfile.pdf"
  expect_error(grobid_convert(filename), "wrongfile.pdf does not exist")
})

test_that("missing batch files just warn", {
  skip_if_offline() # offline error happens before filename error
  filename <- c("wrongfile.pdf", "wrongfile.pdf")
  expect_warning(x <- grobid_convert(filename),
                 "2 of 2 files did not convert")
  exp <- c("wrongfile.pdf" = NA_character_, "wrongfile.pdf" = NA_character_)
  expect_equal(x, exp)
})

# TODO: figure out why mock_api isn't wotking
# returns a different api file each time
# httptest::start_capturing()
# httptest::use_mock_api()

test_that("bad PDF", {
  skip_api("kermitt2-grobid.hf.space")

  filename <- test_path("fixtures", "problems", "xml_with_pdf_extension.pdf")
  expect_error(grobid_convert(filename), "Internal Server Error")

  filename2 <- c(filename, "wrongfile.pdf")
  expect_warning( x <- grobid_convert(filename2), "2 of 2 files did not convert")
  exp <- c(NA_character_, NA_character_)
  names(exp) <- filename2
  expect_equal(x, exp)
})

test_that("makes missing save directory - single", {
  skip_api("kermitt2-grobid.hf.space")

  newdir <- file.path(withr::local_tempdir(), "testnewdir")

  # single file, path with uncreated dir
  save_path <- file.path(newdir, "file.xml")
  filename <- test_path("fixtures", "formats", "to_err_is_human.pdf")
  obs_path <- grobid_convert(filename, save_path = save_path)
  expect_true(dir.exists(newdir))
  expect_equal(obs_path, save_path)
})

test_that("makes missing save directory - multiple", {
  skip_api("kermitt2-grobid.hf.space")

  save_path <- file.path(withr::local_tempdir(), "testnewdir")

  # multiple files with uncreated dir
  f1 <- test_path("fixtures", "debruine")
  filename <- list.files(f1, "pdf", full.names = TRUE)[1:2]
  obs_path <- grobid_convert(filename, save_path = save_path)
  exp_path <- sub(paste0("^", f1 , "/"), "", filename) |>
    sub("\\.pdf", "\\.xml", x = _) |>
    file.path(save_path, x = _) |>
    setNames(filename)
  expect_true(dir.exists(save_path))
  expect_equal(obs_path, exp_path)
  expect_true(file.exists(exp_path[[1]]))
  expect_true(file.exists(exp_path[[2]]))
})

test_that("makes missing save directory - specific", {
  skip_api("kermitt2-grobid.hf.space")

  newdir <- file.path(withr::local_tempdir(), "testnewdir")

  # multiple files with uncreated dir and specific file names (no .xml)
  save_path <- file.path(newdir, c("A", "B"))
  dir <- test_path("fixtures", "debruine")
  filename <- list.files(dir, "pdf", full.names = TRUE)[1:2]
  obs_path <- grobid_convert(filename, save_path = save_path)
  exp_path <- paste0(save_path, ".xml") |> setNames(filename)
  expect_true(dir.exists(newdir))
  expect_equal(obs_path, exp_path)
  expect_true(file.exists(exp_path[[1]]))
  expect_true(file.exists(exp_path[[2]]))
})

test_that("defaults", {
  skip_api("kermitt2-grobid.hf.space")

  filename <- test_path("fixtures", "formats", "to_err_is_human.pdf")
  first_sentence <- "Although intentional dishonestly might be a successful way to boost creativity"
  last_sentence <- "We conclude the use of automated checks has potential to reduce the number of mistakes in scientific manuscripts"

  xml <- grobid_convert(filename, NULL)
  expect_s3_class(xml, "xml_document")
  body <- xml2::xml_find_all(xml, "//text") |> xml2::xml_text()
  expect_true(grepl(first_sentence, body))
  expect_true(grepl(last_sentence, body))

  file.remove(list.files(withr::local_tempdir(), "\\.xml", full.names = TRUE))

  # save to withr::local_tempdir
  dir <- withr::local_tempdir()
  xml_file <- grobid_convert(filename, dir)
  exp <- file.path(dir, "to_err_is_human.xml")
  expect_equal(xml_file, exp)
  xml2 <- read_xml(xml_file)

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
    consolidate_citations=0, # 0, 1, 2
    consolidate_header=0, # 0, 1, 2, 3
    consolidate_funders=0, # 0, 1, 2
    includeRawAffiliations=0,
    includeRawCitations=0,
    includeRawCopyrights=0,
    teiCoordinates=list(),
    segmentSentences=0,
    generateIDs=0,
    flavor=NULL # https://grobid.readthedocs.io/en/latest/Grobid-specialized-processes/
  )

  # reference consolidation
  ref <- tei_bib(xml)
  xml_cite0 <- grobid_convert(filename, NULL, consolidate_citations = 0)
  xml_cite1 <- grobid_convert(filename, NULL, consolidate_citations = 1)
  xml_cite2 <- grobid_convert(filename, NULL, consolidate_citations = 2)
  ref0 <- tei_bib(xml_cite0)
  ref1 <- tei_bib(xml_cite1)
  ref2 <- tei_bib(xml_cite2)

  ref_n <- 4
  wrongtitle <- "Equivalence testing for psychological research"
  righttitle <- "Equivalence Testing for Psychological Research: A Tutorial"
  expect_equal(ref$title[[ref_n]], wrongtitle)
  expect_equal(ref0$title[[ref_n]], wrongtitle)
  expect_equal(ref1$title[[ref_n]], righttitle)
  expect_equal(ref2$title[[ref_n]], wrongtitle)

  rightauthors <- "Daniël Lakens, Anne M Scheel, Peder M Isager"
  wrongauthors <- "D Lakens"
  expect_equal( ref$authors[[ref_n]], wrongauthors)
  expect_equal(ref0$authors[[ref_n]], wrongauthors)
  expect_equal(ref1$authors[[ref_n]], rightauthors)
  expect_equal(ref2$authors[[ref_n]], wrongauthors)

  # change start and end pages
  xml3 <- grobid_convert(filename, NULL, start = 2, end = 3)
  body <- xml2::xml_find_all(xml3, "//body") |> xml2::xml_text()
  expect_false(grepl(first_sentence, body))
  expect_true(grepl("^\\s*Results", body))
  expect_true(grepl(last_sentence, body))

  xml4 <- grobid_convert(filename, NULL, start = 2, end = 2)
  body <- xml2::xml_find_all(xml4, "//body") |> xml2::xml_text()
  expect_false(grepl(first_sentence, body))
  expect_true(grepl("^\\s*Results", body))
  expect_false(grepl(last_sentence, body))
})

test_that("batch - directory", {
  skip_api("kermitt2-grobid.hf.space")

  grobid_dir <- test_path("fixtures", "debruine")
  save_path <- withr::local_tempdir()

  xml_files <- grobid_convert(grobid_dir, save_path)
  actual <- list.files(save_path, "\\.xml")
  expected <- list.files(grobid_dir, "\\.xml")
  expect_equal(actual, expected)
})

test_that("batch - multiple filenames", {
  skip_api("kermitt2-grobid.hf.space")

  grobid_dir <- test_path("fixtures", "debruine")
  save_path <- withr::local_tempdir()

  filenames <- list.files(grobid_dir, ".pdf", full.names = TRUE)
  xml_files <- grobid_convert(filenames[2:3], save_path)
  actual <- list.files(save_path, "\\.xml")
  expected <- list.files(grobid_dir, "\\.xml")[2:3]
  expect_equal(actual, expected)
})


test_that("local", {
  skip_api("kermitt2-grobid.hf.space")
  skip_if_offline("localhost:8070")

  local_url <- "http://localhost:8070"
  filename <- test_path("fixtures", "formats", "to_err_is_human.pdf")
  xml <- grobid_convert(filename, NULL, local_url)
  expect_s3_class(xml, "xml_document")

  save_path <- withr::local_tempdir()
  xml_file <- grobid_convert(filename, save_path, local_url)
  exp <- file.path(save_path, "to_err_is_human.xml")
  expect_equal(xml_file, exp)

  xml2 <- read_xml(xml_file)
  expect_equal(xml, xml2)
})


# test_that("grobid consistency", {
#   # docker run --rm --init --ulimit core=0 -p 8070:8070 lfoppno/grobid:0.8.1
#   skip_if_quick()
#   skip_if_offline("localhost:8070")
#   local_url <- "http://localhost:8070"
#
#   t1 <- file.path(withr::local_tempdir(), "try1")
#   t2 <- file.path(withr::local_tempdir(), "try2")
#   dir.create(t1, showWarnings = FALSE)
#   dir.create(t2, showWarnings = FALSE)
#   #files <- list.files("pdf/psyarxiv", full.names = TRUE)
#   files <- list.files("pdf/psychsci/", full.names = TRUE)
#   xml1 <- grobid_convert(files[1:20], save_path = t1, grobid_url = local_url)
#   xml2 <- grobid_convert(files[1:20], save_path = t2, grobid_url = local_url)
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

# httptest::stop_mocking()
# httptest::stop_capturing()


