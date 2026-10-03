# Grobid TEI to bibr export schema 12.0: grobid_to_bibr(schema_version)

tei <- function(name) test_path("fixtures", "formats", paste0(name, ".pdf.tei.xml"))

test_that("Grobid TEI converts to bibr 12.0 that reads back the same", {
  save_path <- withr::local_tempdir()

  for (xml_path in c(demofile("xml"), tei("published"), tei("preprint"))) {
    paper <- .grobid_to_bibr(xml_path, schema_version = "12.0")
    expect_true(paper_validate(paper))

    json_path <- grobid_to_bibr(xml_path, save_path, schema_version = "12.0")
    if (requireNamespace("jsonvalidate", quietly = TRUE)) {
      expect_valid_bibr12(json_path)
    }

    paper2 <- read(json_path)
    expect_equal(paper2, paper)
  }
})

test_that("Grobid 12.0: source, producer, ids and targets", {
  paper <- .grobid_to_bibr(demofile("xml"), schema_version = "12.0")

  # the source is the PDF next to the TEI
  expect_equal(paper$paper_id, "to_err_is_human")
  expect_equal(paper$info$file_name, "to_err_is_human.pdf")
  expect_equal(paper$info$input_format, "pdf")
  expect_equal(paper$info$sha256, .bibr12_sha256(demofile("pdf")))
  expect_equal(paper$extraction$producer,
               list(name = "grobid", version = "0.9.0", build_sha = NULL))
  expect_equal(paper$extraction$converter$name, "metacheck")
  # when Grobid extracted it: <application when="2026-05-27T08:58+0000">
  expect_equal(paper$extraction$completed_at, "2026-05-27T08:58:00Z")

  # 1-based ids, references before captions and footnotes
  expect_equal(paper$section$section_id, seq_len(nrow(paper$section)))
  expect_equal(paper$bib$bib_id, 1:5)
  expect_equal(paper$text$text_id, seq_len(nrow(paper$text)))
  refs <- paper$section$section_id[paper$section$section_type == "references"]
  expect_equal(paper$text$section_id[paper$bib$text_id], rep(refs, 5))
  expect_true(max(paper$bib$text_id) < min(paper$figure$text_id))

  # a citation resolves to the right reference
  cite <- paper$xref[paper$xref$xref_type == "bib", ]
  expect_equal(cite$contents, "(Gino and Wiltermuth 2014)")
  expect_equal(paper$bib$title[[cite$target_id]],
               "Evil Genius? How Dishonesty Can Lead to Greater Creativity")

  # figures in document order: Grobid lists Figure 2 first
  expect_equal(paper$figure$label, c("2", "1"))
  # a caption starts with its label, which Grobid's figDesc can leave out
  expect_equal(paper$figure$caption[[2]], "Figure 1: The simulated data.")
  expect_equal(paper$text$text[paper$figure$text_id], paper$figure$caption)
  expect_true(all(is.na(paper$text$section_id[paper$figure$text_id])))
  fig <- paper$xref[paper$xref$xref_type == "figure", ]
  expect_equal(paper$figure$label[fig$target_id], fig$contents)

  # a table, and a table reference Grobid did not link
  expect_equal(paper$table$label, "1")
  expect_match(paper$table$caption, "^Table 1: The average number of mistakes")
  expect_equal(paper$table$contents[[1]][[2]], c("control", "10.90", "4.50"))
  tab <- paper$xref[paper$xref$xref_type == "table", ]
  expect_true(is.na(tab$target_id))

  # a footnote and the reference to it
  expect_equal(paper$footnote$label, "1")
  expect_equal(paper$text$text[[paper$footnote$text_id]],
               "Remember, this is a demo and none of this is real.")
  foot <- paper$xref[paper$xref$xref_type == "foot", ]
  expect_equal(foot$target_id, paper$footnote$footnote_id)

  # no caption or footnote pseudo-sections
  expect_false(any(paper$section$section_type %in% c("figure", "table", "foot")))
  body <- paper$text[!paper$text$text_id %in% c(paper$figure$text_id,
                                                paper$table$text_id,
                                                paper$footnote$text_id), ]
  expect_false(anyNA(body$section_id))

  # warnings are codes and messages
  codes <- vapply(paper$extraction$warnings, \(w) w$code, character(1))
  expect_equal(codes, "METACHECK_FLOAT_SECTION_UNKNOWN")
})

test_that("Grobid 12.0 figure and footnote references target their rows", {
  paper <- .grobid_to_bibr(tei("preprint"), schema_version = "12.0")

  fig <- paper$xref[paper$xref$xref_type == "figure", ]
  expect_false(anyNA(fig$target_id))
  expect_equal(paper$figure$label[fig$target_id], fig$contents)

  paper <- .grobid_to_bibr(tei("published"), schema_version = "12.0")
  foot <- paper$xref[paper$xref$xref_type == "foot", ]
  expect_equal(paper$footnote$label[foot$target_id], c("1", "2"))
})

test_that("Grobid 12.0 without the PDF: the TEI is the source", {
  xml_path <- file.path(withr::local_tempdir(), "demo.xml")
  file.copy(demofile("xml"), xml_path)

  paper <- .grobid_to_bibr(xml_path, schema_version = "12.0")
  expect_equal(paper$paper_id, "demo")
  expect_equal(paper$info$file_name, "demo.xml")
  expect_equal(paper$info$input_format, "tei")
  expect_equal(paper$info$sha256, .bibr12_sha256(xml_path))
  codes <- vapply(paper$extraction$warnings, \(w) w$code, character(1))
  expect_in("METACHECK_SOURCE_IS_TEI", codes)
})

test_that("Grobid 12.0 without an extraction time: the conversion time", {
  xml_path <- file.path(withr::local_tempdir(), "demo.xml")
  readLines(demofile("xml"), warn = FALSE) |>
    sub('(<application[^>]*) when="[^"]*"', "\\1", x = _) |>
    writeLines(xml_path)

  paper <- .grobid_to_bibr(xml_path, schema_version = "12.0")
  completed_at <- as.POSIXct(paper$extraction$completed_at, tz = "UTC",
                             format = "%Y-%m-%dT%H:%M:%SZ")
  expect_lt(abs(difftime(completed_at, Sys.time(), units = "mins")), 10)
})

test_that("grobid_to_bibr keeps its default output", {
  paper <- grobid_to_bibr(demofile("xml"), NULL)
  expect_null(paper$extraction)
  expect_match(paper$info$input_format, "^grobid ")

  expect_error(grobid_to_bibr(demofile("xml"), NULL, schema_version = "11"),
               "schema_version must be NULL or \"12.0\"")
})

test_that("the same PDF through bibr and Grobid has the same source.sha256", {
  # bibr exports of the fixture PDFs; set METACHECK_BIBR12_EXPORTS to a folder
  # with published.json and preprint.json to check both
  exports <- Sys.getenv("METACHECK_BIBR12_EXPORTS",
                        test_path("fixtures", "bibr12"))

  for (name in c("preprint", "published")) {
    bibr_path <- file.path(exports, paste0(name, ".json"))
    if (!file.exists(bibr_path)) next

    bibr <- read(bibr_path)
    grobid <- .grobid_to_bibr(tei(name), schema_version = "12.0")

    expect_equal(grobid$info$sha256, bibr$info$sha256)
    expect_equal(grobid$info$file_name, bibr$info$file_name)
    expect_equal(grobid$info$input_format, "pdf")
    expect_equal(bibr$info$input_format, "pdf")
    expect_equal(bibr$extraction$producer$name, "bibr")
    expect_equal(grobid$extraction$producer$name, "grobid")

    # both find about the same number of references
    expect_lt(abs(nrow(grobid$bib) - nrow(bibr$bib)), 0.1 * nrow(bibr$bib))
  }

  bibr_path <- file.path(exports, "preprint.json")
  skip_if_not(file.exists(bibr_path), "no bibr export of preprint.pdf")
  expect_equal(.grobid_to_bibr(tei("preprint"), schema_version = "12.0")$info$title,
               read(bibr_path)$info$title)
})
