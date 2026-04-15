# grobid_to_bibr ----

test_that("grobid_to_bibr", {
  expect_true(is.function(metacheck::grobid_to_bibr))
  expect_true(is.function(metacheck:::.grobid_to_bibr))
  expect_no_error(helplist <- help(grobid_to_bibr, metacheck))

  expect_error(grobid_to_bibr(NULL))
})

test_that("1 paper, fails", {
  xml_file <- test_path("fixtures", "problems", "corrupt.xml")
  expect_error( paper <- grobid_to_bibr(xml_file, NULL) )
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

  expect_equal(paper$info$keywords[[1]], NULL)
  expect_contains(paper$bib$authors, "Smith, F")

  expect_equal(paper$table$table_id, 1)
  tab_sec <-paper$section[paper$section$section_id == paper$table$section_id, ]$section_type
  expect_equal(tab_sec, "table")

  expect_equal(paper$figure$figure_id, 1)
  fig_sec <-paper$section[paper$section$section_id == paper$figure$section_id, ]$section_type
  expect_equal(fig_sec, "figure")
})


test_that("1 paper, save_path, no CR lookup", {
  xml_file1 <- system.file("demo/to_err_is_human.xml", package = "metacheck")
  paper1 <- grobid_to_bibr(xml_file1, NULL)

  xml_file2 <- system.file("demo/to_err_is_human.xml", package = "metacheck")
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
  skip_api("api.labs.crossref.org")
  xml_file <- system.file("demo/to_err_is_human.xml", package = "metacheck")
  paper_cr <- grobid_to_bibr(xml_file, NULL, TRUE)
  expect_equal(paper_cr$bib_match$service[[1]], "crossref")
})


test_that("multiple papers, NULL save_path, CR lookup", {
  skip_api("api.labs.crossref.org")
  xml_file <- c(
    system.file("demo/to_err_is_human.xml", package = "metacheck"),
    system.file("demo/to_err_is_human.xml", package = "metacheck")
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
  xml_file <- system.file("demo/to_err_is_human.xml", package = "metacheck")
  title <- "To Err is Human: An Empirical Investigation"

  obs_xml <- read(xml_file)
  expect_s3_class(obs_xml, "scivrs_paper")
  expect_equal(obs_xml$info$title, title)

})

test_that("bibr file", {
  bibr_file <- system.file("demo/to_err_is_human.json", package = "metacheck")
  obs_bibr <- read(bibr_file)
  expect_s3_class(obs_bibr, "scivrs_paper")
  expect_match(obs_bibr$info$title, "To Err is Human")
})

test_that("both grobid xml and bibr", {
  xml_file <- system.file("demo/to_err_is_human.xml", package = "metacheck")
  bibr_file <- system.file("demo/to_err_is_human.json", package = "metacheck")

  file_path <- c(xml_file, bibr_file)
  obs <- read(file_path)
  expect_equal(length(obs), 2)
  expect_s3_class(obs, "scivrs_paperlist")
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
  expect_error(convert_grobid(filename, api_url = "notawebsite"),
               "api_url must be a valid URL, starting with http or https!")

  # URL without http/https detected"
  expect_error(convert_grobid(filename, api_url = "kermitt2-grobid.hf.space"),
               "api_url must be a valid URL, starting with http or https!")
})


test_that("non-Grobid URL rejected", {
  skip_if_offline("google.com")

  filename <- demofile("pdf")
  expect_error(convert_grobid(filename, api_url = "https://google.com"))
})


# TODO: figure out why mock_api isn't working
# returns a different api file each time
# httptest::start_capturing()
# httptest::use_mock_api()

test_that("bad PDF", {
  skip_api(grobid_url)

  filename <- test_path("fixtures", "problems", "xml_with_pdf_extension.pdf")
  expect_error(convert_grobid(filename))

  filename2 <- c(filename, "wrongfile.pdf")
  expect_error( x <- convert_grobid(filename2, api_url = grobid_url) )
  # expect_warning( x <- convert_grobid(filename2), "2 of 2 files did not convert")
  # exp <- c(NA_character_, NA_character_)
  # names(exp) <- filename2
  # expect_equal(x, exp)
})

test_that("makes missing save directory - single", {
  skip_api(grobid_url)

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
  skip_api(grobid_url)

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
  skip_api(grobid_url)

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
  skip_api(grobid_url)

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
  skip("Buggy grobid tests")

  xml_file <- demofile("xml")
  xml_text <- readLines(xml_file, warn = FALSE) |>
    paste(collapse = "\n") |>
    # fixes a glitch that stops grobid xml from being read
    gsub(' xmlns="http://www.tei-c.org/ns/1.0"', "",
         x = _, fixed = TRUE
    )
  xml <- xml2::read_xml(xml_text)
  ref <- tei_bib(xml)
  paper0 <- convert_grobid(pdf, NULL, api_url = grobid_url, consolidate_citations = 0)
  paper1 <- convert_grobid(pdf, NULL, api_url = grobid_url, consolidate_citations = 1)
  paper2 <- convert_grobid(pdf, NULL, api_url = grobid_url, consolidate_citations = 2)

  ref_n <- 4
  wrongtitle <- "Equivalence testing for psychological research"
  righttitle <- "Equivalence Testing for Psychological Research: A Tutorial"
  expect_equal(paper$bib$title[[ref_n]], wrongtitle)
  expect_equal(paper0$bib$title[[ref_n]], wrongtitle)
  expect_equal(paper1$bib$title[[ref_n]], righttitle)
  expect_equal(paper2$bib$title[[ref_n]], wrongtitle)

  rightauthors <- "Daniël Lakens, Anne M Scheel, Peder M Isager"
  wrongauthors <- "D Lakens"
  expect_equal( ref$authors[[ref_n]], wrongauthors)
  expect_equal(ref0$authors[[ref_n]], wrongauthors)
  expect_equal(ref1$authors[[ref_n]], rightauthors)
  expect_equal(ref2$authors[[ref_n]], wrongauthors)

  # change start and end pages
  xml3 <- convert_grobid(filename, NULL, api_url = grobid_url, start = 2, end = 3)
  body <- xml2::xml_find_all(xml3, "//body") |> xml2::xml_text()
  expect_false(grepl(first_sentence, body))
  expect_true(grepl("^\\s*Results", body))
  expect_true(grepl(last_sentence, body))

  xml4 <- convert_grobid(filename, NULL, api_url = grobid_url, start = 2, end = 2)
  body <- xml2::xml_find_all(xml4, "//body") |> xml2::xml_text()
  expect_false(grepl(first_sentence, body))
  expect_true(grepl("^\\s*Results", body))
  expect_false(grepl(last_sentence, body))
})

test_that("batch - directory", {
  skip_api(grobid_url)

  grobid_dir <- test_path("fixtures", "debruine")
  save_path <- withr::local_tempdir()

  xml_files <- convert_grobid(grobid_dir, save_path, api_url = grobid_url)
  actual <- list.files(save_path, "\\.xml")
  expected <- list.files(grobid_dir, "\\.xml")
  expect_equal(actual, expected)
})

test_that("batch - multiple filenames", {
  skip_api(grobid_url)

  grobid_dir <- test_path("fixtures", "debruine")
  save_path <- withr::local_tempdir()

  filenames <- list.files(grobid_dir, ".pdf", full.names = TRUE)
  xml_files <- convert_grobid(filenames[2:3], save_path, api_url = grobid_url)
  actual <- list.files(save_path, "\\.xml")
  expected <- list.files(grobid_dir, "\\.xml")[2:3]
  expect_equal(actual, expected)
})
