test_that("tei_to_bibr", {
  expect_true(is.function(metacheck::tei_to_bibr))
  expect_no_error(helplist <- help(tei_to_bibr, metacheck))

  expect_error(tei_to_bibr(bad_arg))

  xml_file <- test_path("fixtures", "formats", "to_err_is_human.pdf.tei.xml")
  paper <- tei_to_bibr(xml_file)

  expect_s3_class(paper, "scivrs_paper")
  text_cols <- c("text_id", "paragraph_id", "section_id", "text")
  expect_in(names(paper$text), text_cols)

  expect_equal(paper$bib$doi[[4]], "10.0000/0123456789")
})

test_that("read", {
  expect_true(is.function(metacheck::read))
  expect_no_error(helplist <- help(read, metacheck))

  expect_error(read(bad_arg))

  xml_file <- test_path("fixtures", "examples", "to_err_is_human.xml")
  zip_file <- test_path("fixtures", "bibr", "to_err_is_human.zip")
  title <- "To Err is Human: An Empirical Investigation"

  # grobid xml
  obs_xml <- read(xml_file)
  expect_s3_class(obs_xml, "scivrs_paper")
  expect_equal(obs_xml$info$title, title)

  # bibr zip
  obs_zip <- read(zip_file)
  expect_s3_class(obs_zip, "scivrs_paper")
  expect_equal(obs_zip$info$title, title)

  # both
  file_path <- c(xml_file, zip_file)
  obs <- read(file_path)
  expect_equal(length(obs), 2)
  expect_s3_class(obs, "scivrs_paperlist")
})



