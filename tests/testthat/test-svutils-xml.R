test_that(".xml_find_text", {
  expect_true(is.function(metacheck:::.xml_find_text))
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
  expect_true(is.function(metacheck:::.xml_find1_text))
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
  expect_true(is.function(metacheck:::.xml_read_grobid))
  expect_no_error(helplist <- help(.xml_read_grobid, metacheck))

  expect_error(.xml_read_grobid("bad_arg"))

  path <- demofile("xml")
  xml <- .xml_read_grobid(path)

  title <- xml2::xml_find_first(xml, "//title")
  level <- xml2::xml_attr(title, "level")
  expect_equal("a", level)
})

test_that(".xml_read_grobid keeps a number before a tag, and fixes omega and g z (U16)", {
  # "<" counted as an operator, so a number before a tag lost its space; the
  # omega rule read "\u03C" as "<", and "\\g" was a literal g ("dgz" -> "gz")
  tei <- withr::local_tempfile(fileext = ".xml")
  writeLines(c('<TEI xmlns="http://www.tei-c.org/ns/1.0"><text><body><div>',
               '<p>See <ref type="table">Table 1</ref> and Hamilton 1964</p>',
               '<p>It was \u03c9 p 2 = .12 and g z = 0.5 and dgz stays</p>',
               '</div></body></text></TEI>'), tei, useBytes = TRUE)
  p <- xml2::xml_text(xml2::xml_find_all(.xml_read_grobid(tei), "//p"))
  expect_equal(p, c("See Table 1 and Hamilton 1964",
                    "It was \u03c9p\u00b2 = .12 and gz = 0.5 and dgz stays"))
})
