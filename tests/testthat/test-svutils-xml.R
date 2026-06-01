test_that(".xml_find_text", {
  expect_true(is.function(metacheck::.xml_find_text))
  expect_no_error(helplist <- help(.xml_find_text, metacheck))

  expect_error(.xml_find_text("string"))

  xml <- xml2::read_xml("<p>A <b>B1</b> and <b>B2</b></p>")

  no <- .xml_find_text(xml, "//a")
  expect_equal(no, "")

  b <- .xml_find_text(xml, "//p //b")
  expect_equal(b, c("B1", "B2"))

  bjoin <- .xml_find_text(xml, "//p //b", join = "-")
  expect_equal(bjoin, "B1-B2")
})

test_that(".xml_find1_text", {
  expect_true(is.function(metacheck::.xml_find1_text))
  expect_no_error(helplist <- help(.xml_find1_text, metacheck))

  expect_error(.xml_find1_text("string"))

  xml <- xml2::read_xml("<p>A <b>B1</b> and <b>B2</b></p>")

  b <- .xml_find1_text(xml, "//p //b")
  expect_equal(b, "B1")

  expect_error(.xml_find1_text(xml, "//p //b", join = "-"))

  no <- .xml_find1_text(xml, "//a")
  expect_equal(no, "")
})

test_that(".xml_read_grobid", {
  expect_true(is.function(metacheck::.xml_read_grobid))
  expect_no_error(helplist <- help(.xml_read_grobid, metacheck))

  expect_error(.xml_read_grobid("bad_arg"))

  path <- demofile("xml")
  xml <- .xml_read_grobid(path)

  title <- xml2::xml_find_first(xml, "//title")
  level <- xml2::xml_attr(title, "level")
  expect_equal("a", level)
})

