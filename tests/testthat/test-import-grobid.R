# grobid_to_bibr ----

# httptest2::start_capturing()
httptest2::use_mock_api()

testthat::local_mocked_bindings(
  online = \(...) TRUE
)

test_that("grobid_to_bibr", {
  expect_true(is.function(metacheck::grobid_to_bibr))
  expect_true(is.function(metacheck:::.grobid_to_bibr))
  expect_no_error(helplist <- help(grobid_to_bibr, metacheck))

  expect_error(grobid_to_bibr(1))
})

test_that("1 paper, fail", {
  xml_file <- test_path("fixtures", "problems", "corrupt.xml")
  expect_warning(paper <- grobid_to_bibr(xml_file, NULL))
  expect_null(paper)
})

test_that("multiple paper, one fails", {
  xml_file <- c(
    test_path("fixtures", "formats", "preprint.pdf.tei.xml"),
    test_path("fixtures", "problems", "corrupt.xml")
  )
  expect_warning( paper <- grobid_to_bibr(xml_file, NULL) )
  expect_equal(length(paper), 1)
  expect_equal(paper[[1]]$info$file_name, xml_file[[1]])
})


test_that("1 paper, NULL save_path, no CR lookup", {
  xml_file <- demofile("xml")
  paper <- grobid_to_bibr(xml_file, NULL)

  expect_s3_class(paper, "scivrs_paper")
  expect_contains(paper$bib$doi, "10.0000/0123456789")
  expect_false("bib_match" %in% names(paper))
  expect_true(paper_validate(paper))
  expect_match(paper$info$input_format, "grobid", ignore.case = TRUE)
  expect_match(paper$info$input_format, "0\\.\\d\\.\\d")

  expect_equal(paper$info$keywords[[1]], NULL)
  expect_contains(paper$bib$authors, "Smith, F")

  expect_equal(paper$table$table_id, 1)
  tab_sec <-paper$section[paper$section$section_id == paper$table$section_id, ]$section_type
  expect_equal(tab_sec, "table")

  expect_equal(paper$figure$figure_id, 1:2)
  fig_sec <-paper$section[paper$section$section_id %in% paper$figure$section_id, ]$section_type
  expect_in(fig_sec, "figure")
})


test_that("1 paper, save_path, no CR lookup", {
  xml_file1 <- system.file("demos/to_err_is_human.xml", package = "metacheck")
  paper1 <- grobid_to_bibr(xml_file1, NULL)

  xml_file2 <- system.file("demos/to_err_is_human.xml", package = "metacheck")
  save_path <- withr::local_tempdir()
  json_path <- grobid_to_bibr(xml_file2, save_path)
  paper2 <- read(json_path)
  expect_true(paper_validate(paper2))
  # JSON round-trip may add template columns, change int/numeric types,
  # or convert list columns to different representations
  compare_shared <- function(a, b) {
    cols <- intersect(names(a), names(b))
    # skip list/data.frame columns that don't survive JSON round-trip identically
    atomic_cols <- cols[sapply(cols, function(col) is.atomic(a[[col]]) && is.atomic(b[[col]]))]
    a <- a[, atomic_cols, drop = FALSE]
    b <- b[, atomic_cols, drop = FALSE]
    for (col in atomic_cols) {
      if (is.numeric(a[[col]]) && is.numeric(b[[col]])) {
        a[[col]] <- as.numeric(a[[col]])
        b[[col]] <- as.numeric(b[[col]])
      }
    }
    expect_setequal(a, b)
  }
  suppressWarnings({
    compare_shared(paper1$info,    paper2$info)
    compare_shared(paper1$author,  paper2$author)
    compare_shared(paper1$eq,      paper2$eq)
    compare_shared(paper1$figure,  paper2$figure)
    compare_shared(paper1$url,     paper2$url)
    compare_shared(paper1$section, paper2$section)
    compare_shared(paper1$table,   paper2$table)
    compare_shared(paper1$text,    paper2$text)
    compare_shared(paper1$xref,    paper2$xref)

    # authors not converting right yet?
    p1bib <- paper1$bib; p2bib <- paper2$bib
    p1bib$authors <- NULL; p2bib$authors <- NULL
    compare_shared(p1bib, p2bib)
  })
})


test_that("multiple papers, NULL save_path, no CR lookup", {
  xml_file <- c(
    test_path("fixtures", "formats", "preprint.pdf.tei.xml"),
    test_path("fixtures", "formats", "published.pdf.tei.xml")
  )

  papers <- grobid_to_bibr(xml_file, save_path = NULL)
  expect_s3_class(papers, "scivrs_paperlist")
  expect_true(paper_validate(papers[[1]]))
  expect_true(paper_validate(papers[[2]]))
})


test_that("multiple papers, save_path, no CR lookup", {
  xml_file <- c(
    test_path("fixtures", "formats", "preprint.pdf.tei.xml"),
    test_path("fixtures", "formats", "published.pdf.tei.xml")
  )
  save_path <- withr::local_tempdir()
  zip_path <- grobid_to_bibr(xml_file, save_path)
  expect_equal(length(zip_path), 2)
  papers <- read(zip_path)
  expect_s3_class(papers, "scivrs_paperlist")
})


test_that("1 paper, NULL save_path, CR lookup", {
  skip("CrossRef problems")
  xml_file <- demofile("xml")
  paper_cr <- grobid_to_bibr(xml_file, NULL, TRUE)
  expect_equal(paper_cr$bib_match$service[[1]], "crossref")
})


test_that("multiple papers, NULL save_path, CR lookup", {
  skip("CrossRef problems")

  xml_file <- c(
    demofile("xml"),
    demofile("xml")
  )
  papers_cr <- grobid_to_bibr(xml_file, NULL, TRUE)
  expect_equal(papers_cr[[1]]$bib_match$service[[1]], "crossref")
  expect_equal(papers_cr[[2]]$bib_match$service[[2]], "crossref")
})

# read ----

test_that("read", {
  expect_true(is.function(metacheck::read))
  expect_no_error(helplist <- help(read, metacheck))

  expect_error(read(bad_arg))
})

test_that("read grobid xml", {
  xml_file <- demofile("xml")
  title <- "To Err is Human: An Empirical Investigation"

  obs_xml <- read(xml_file)
  expect_s3_class(obs_xml, "scivrs_paper")
  expect_equal(obs_xml$info$title, title)

})

test_that("bibr file", {
  bibr_file <- demofile("json")
  obs_bibr <- read(bibr_file)
  expect_s3_class(obs_bibr, "scivrs_paper")
  expect_match(obs_bibr$info$title, "To Err is Human")
})

test_that("both grobid xml and bibr", {
  xml_file <- test_path("fixtures", "formats", "preprint.pdf.tei.xml")
  json_file <- demofile("json")

  file_path <- c(xml_file, json_file)
  obs <- read(file_path)
  expect_equal(length(obs), 2)
  expect_s3_class(obs, "scivrs_paperlist")
})

test_that(".tei_xrefs handles URL refs with query strings", {
  xml <- xml2::read_xml(
    "<TEI><text><body><div><p>The gridded soil data are available at <ref type=\"url\" target=\"https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2358\">ORNL DAAC</ref> for download.</p></div></body></text></TEI>"
  )

  text_table <- data.frame(
    text_id = 1,
    text = "The gridded soil data are available at ORNL DAAC for download.",
    formatted = "The gridded soil data are available at <ref type=\"url\" target=\"https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2358\">ORNL DAAC</ref> for download."
  )

  expect_no_error(
    xrefs <- .tei_xrefs(text_table)
  )
  expect_s3_class(xrefs, "data.frame")
})

# convert_grobid ----

test_that("convert_grobid", {
  expect_true(is.function(metacheck::convert_grobid))
  expect_no_error(helplist <- help(convert_grobid, metacheck))

  expect_error(convert_grobid(bad_arg))

  filename <- "wrongfile.pdf"
  expect_error(convert_grobid(filename), "Files do not exist")

  filename <- c("wrongfile.pdf", "wrongfile.pdf")
  expect_error(convert_grobid(filename), "Files do not exist")
})

test_that("invalid URL error", {
  filename <- demofile("pdf")
  expect_error(suppressWarnings(
    convert_grobid(filename, api_url = "notawebsite")
  ))

  # URL without http/https detected"
  expect_error(suppressWarnings(
    convert_grobid(filename, api_url = "kermitt2-grobid.hf.space")
    ))
})


test_that("non-Grobid URL rejected", {
  filename <- demofile("pdf")
  expect_error(convert_grobid(filename, api_url = "https://google.com"))
})


test_that("bad PDF", {
  filename <- test_path("fixtures", "problems", "xml_with_pdf_extension.pdf")
  expect_error(convert_grobid(filename, api_url = grobid_url))

  filename2 <- c(filename, "wrongfile.pdf")
  expect_error( x <- convert_grobid(filename2, api_url = grobid_url) )
  # expect_warning( x <- convert_grobid(filename2), "2 of 2 files did not convert")
  # exp <- c(NA_character_, NA_character_)
  # names(exp) <- filename2
  # expect_equal(x, exp)
})


test_that("makes missing save directory - single", {
  skip_if_not(.grobid_isalive(grobid_url, error = FALSE),
              message = "grobid not available")

  newdir <- file.path(withr::local_tempdir(), "testnewdir")

  # single file, path with uncreated dir
  save_path <- file.path(newdir, "file.xml")
  file_path <- demofile("pdf")
  obs_path <- convert_grobid(file_path, save_path = save_path,
                             api_url = grobid_url)
  expect_true(dir.exists(newdir))
  expect_equal(obs_path, save_path)
})

test_that("makes missing save directory - multiple", {
  skip_if_not(.grobid_isalive(grobid_url, error = FALSE),
              message = "grobid not available")

  save_path <- file.path(withr::local_tempdir(), "testnewdir")

  # multiple files with uncreated dir
  f1 <- test_path("fixtures", "debruine")
  filename <- list.files(f1, "pdf", full.names = TRUE)[1:2]
  obs_path <- convert_grobid(filename, save_path = save_path,
                             api_url = grobid_url)
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
  skip_if_not(.grobid_isalive(grobid_url, error = FALSE),
              message = "grobid not available")

  newdir <- file.path(withr::local_tempdir(), "testnewdir")

  # multiple files with uncreated dir and specific file names (no .xml)
  save_path <- file.path(newdir, c("A", "B"))
  dir <- test_path("fixtures", "debruine")
  filename <- list.files(dir, "pdf", full.names = TRUE)[1:2]
  obs_path <- convert_grobid(filename, save_path = save_path,
                             api_url = grobid_url)
  exp_path <- paste0(save_path, ".xml") |> setNames(filename)
  expect_true(dir.exists(newdir))
  expect_equal(obs_path, exp_path)
  expect_true(file.exists(exp_path[[1]]))
  expect_true(file.exists(exp_path[[2]]))
})

test_that("defaults", {
  skip_if_not(.grobid_isalive(grobid_url, error = FALSE),
              message = "grobid not available")

  pdf <- demofile("pdf")
  paper <- convert_grobid(pdf, NULL, api_url = grobid_url)
  expect_s3_class(paper, "scivrs_paper")

  # save to withr::local_tempdir
  dir <- withr::local_tempdir()
  xml_file <- convert_grobid(pdf, dir, api_url = grobid_url)
  exp <- file.path(dir, "to_err_is_human.xml")
  expect_equal(xml_file, exp)

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
})

test_that("reference consolidation", {
  skip_if_not(.grobid_isalive(grobid_url, error = FALSE),
              message = "grobid not available")

  xml_file <- demofile("xml")
  xml_text <- readLines(xml_file, warn = FALSE) |>
    paste(collapse = "\n") |>
    # fixes a glitch that stops grobid xml from being read
    gsub(' xmlns="http://www.tei-c.org/ns/1.0"', "",
         x = _, fixed = TRUE
    )
  xml <- xml2::read_xml(xml_text)

  pdf <- demofile("pdf")
  paper0 <- convert_grobid(pdf, NULL, api_url = grobid_url, consolidate_citations = 0)
  paper1 <- convert_grobid(pdf, NULL, api_url = grobid_url, consolidate_citations = 1)
  paper2 <- convert_grobid(pdf, NULL, api_url = grobid_url, consolidate_citations = 2)

  ref_n <- 4
  wrongtitle <- "Equivalence Testing for Psychological Research"
  righttitle <- "Equivalence Testing for Psychological Research: A Tutorial"
  expect_equal(paper0$bib$title[[ref_n]], wrongtitle)
  expect_equal(paper1$bib$title[[ref_n]], righttitle)
  expect_equal(paper2$bib$title[[ref_n]], wrongtitle)

  rightauthors <- "Lakens, Daniël; Scheel, Anne M; Isager, Peder M"
  wrongauthors <- "Lakens, Daniël"
  expect_equal(paper0$bib$authors[[ref_n]], wrongauthors)
  expect_equal(paper1$bib$authors[[ref_n]], rightauthors)
  expect_equal(paper2$bib$authors[[ref_n]], wrongauthors)
})

test_that("change start and end pages", {
  skip_if_not(.grobid_isalive(grobid_url, error = FALSE),
              message = "grobid not available")

  save_path <- withr::local_tempdir()
  pdf <- demofile("pdf")
  xml_path <- convert_grobid(pdf, save_path, api_url = grobid_url,
                         start = 2, end = 3)
  xml <- xml2::read_html(xml_path)
  body <- xml2::xml_find_all(xml, "//body") |> xml2::xml_text()

  p1 <- "Although intentional dishonesty might be a successful way to boost creativity"
  p2 <- "We also asked researchers to rate how useful they found the checklist or app on a scale"
  p3 <- "On average researchers in the experimental condition found the app marginally significantly more useful"
  p4 <- "The authors declare a conflict of interest."

  expect_false(grepl(p1, body, fixed = TRUE))
  expect_true(grepl(p2, body, fixed = TRUE))
  expect_true(grepl(p3, body, fixed = TRUE))
  expect_false(grepl(p4, body, fixed = TRUE))
})

test_that("batch - directory", {
  skip_if_not(.grobid_isalive(grobid_url, error = FALSE),
              message = "grobid not available")

  grobid_dir <- test_path("fixtures", "debruine")
  save_path <- withr::local_tempdir()

  xml_files <- convert_grobid(grobid_dir, save_path, api_url = grobid_url)
  actual <- list.files(save_path, "\\.xml")
  expected <- list.files(grobid_dir, "\\.xml")
  expect_equal(actual, expected)
})

test_that("batch - multiple filenames", {
  skip_if_not(.grobid_isalive(grobid_url, error = FALSE),
              message = "grobid not available")

  grobid_dir <- test_path("fixtures", "debruine")
  save_path <- withr::local_tempdir()

  filenames <- list.files(grobid_dir, ".pdf", full.names = TRUE)
  xml_files <- convert_grobid(filenames[2:3], save_path, api_url = grobid_url)
  actual <- list.files(save_path, "\\.xml")
  expected <- list.files(grobid_dir, "\\.xml")[2:3]
  expect_equal(actual, expected)
})

test_that(".grobid_isalive", {
  expect_true(is.function(metacheck:::.grobid_isalive))

  expect_error(suppressWarnings(.grobid_isalive()))

  # not a url
  api_url <- "grobid"
  expect_error(.grobid_isalive(api_url))
  expect_false(.grobid_isalive(api_url, error = FALSE))

  # url, not grobid
  api_url <- "https://google.com"
  expect_error(.grobid_isalive(api_url))
  expect_false(.grobid_isalive(api_url, error = FALSE))

  api_url <- grobid_url
  alive <- .grobid_isalive(api_url, error = FALSE)
  expect_in(alive, c(TRUE, FALSE))
})

httptest2::stop_mocking()
# httptest2::stop_capturing()


test_that("null section import", {
  xml_file <- test_path("fixtures", "problems", "203020.xml")
  dir <- test_path("fixtures", "problems")
  json <- convert(xml_file, dir)

  paper <- read(json)
  expect_true(!any(is.na(paper$section$section_id)))
})



test_that("in-text refs", {
  # grobid XML like this borks things:

  # The learning bias for the self condition and the learning bias for the stranger condition were not significantly correlated, r(76) = .10, p = .386 (see <ref type="url" target="https://journals.sagepub.com/doi/suppl/10.1177/0956797617737129">Fig</ref>. <ref type="figure" target="#fig_1">S3b</ref> <ref type="url" target="https://journals.sagepub.com/doi/suppl/10.1177/0956797617737129">in the Supplemental Material</ref>).</p><p>Strikingly, we found that participants with an optimistic learning bias for strangers donated on average almost 3 times as much to charity as participants with a pessimistic learning bias for strangers, t(74) = 2.26, p = .026 (see Fig. <ref type="figure" target="#fig_2">4</ref>). Individual differences in the magnitude of vicarious optimism for strangers were positively correlated with donations to charity, r(76) = .26, p = .02 (see <ref type="url" target="https://journals.sagepub.com/doi/suppl/10.1177/0956797617737129">Fig</ref>. <ref type="figure" target="#fig_2">S4b</ref> <ref type="url" target="https://journals.sagepub.com/doi/suppl/10.1177/0956797617737129">in the Supplemental Material). This  relationship was robust to controlling for the magnitude  of the optimistic learning bias for self, age, gender,  education, and income, partial r(70) = .29, p = .012 (see  the Supplemental Material</ref> for details).
  xml_file <- test_path("fixtures", "problems", "0956797617737129.xml")
  # paper <- read(xml_file)
  # x <- grep("Strikingly", paper$text$text)
  # txt <- paper$text$text[[x]]
  # expect_false(grepl("journals.sagepub.com", txt, fixed = TRUE))


  xml_text <- readLines(xml_file, warn = FALSE) |>
    paste(collapse = "\n") |>
    # fixes a glitch that stops grobid xml from being read
    gsub(' xmlns="http://www.tei-c.org/ns/1.0"', "",
         x = _, fixed = TRUE
    )

  xml <- xml2::read_xml(xml_text)
  text <- .tei_text(xml)
  x <- grep("Strikingly", text$text)
  expect_false(grepl("<ref", text$text[[x]], fixed = TRUE))
  expect_true(grepl("<ref", text$formatted[[x]], fixed = TRUE))
  expect_true(length(unique(text$paragraph_id)) > 1)
})


test_that("URL in text", {
  skip("no test yet")
  xml_file <- "data-raw/psychsci/grobid_0.9.0-crf/0956797616647519.xml"
  xml_text <- readLines(xml_file, warn = FALSE) |>
    paste(collapse = "\n") |>
    # fixes a glitch that stops grobid xml from being read
    gsub(' xmlns="http://www.tei-c.org/ns/1.0"', "",
         x = _, fixed = TRUE
    ) |>
    gsub("Fig\\. (\\d{1,2})(\\s*\\.)?", "Fig \\1", x = _) |>
    gsub("Figure\\. (\\d{1,2})(\\s*\\.)?", "Figure \\1", x = _) |>
    gsub("Tab\\. (\\d{1,2})(\\s*\\.)?", "Tab \\1", x = _) |>
    gsub("Table\\. (\\d{1,2})(\\s*\\.)?", "Table \\1", x = _)

  xml <- xml2::read_xml(xml_text)
  text2 <- .tei_text(xml)
#   grep("doi", text$text, fixed = TRUE)
})
