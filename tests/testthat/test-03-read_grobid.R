# setwd("tests/testthat/")

test_that("error", {
  expect_true(is.function(read))

  # invalid file type
  expect_warning(r <- read("no.exist"))
  expect_null(r)

  # non-grobid XML
  filename <- tempfile(fileext = ".xml")
  xml2::read_html("<p>Hello</p>") |>
    xml2::write_xml(filename, options = "as_xml")
  expect_no_warning(g1 <- read(filename))
  expect_equal(g1$full_text |> nrow(), 0)

  # valid grobid with no text
  filename <- "examples/notext.xml"
  expect_no_error(notext <- read(filename))
  expect_equal( nrow( search_text(notext) ), 0)

  # bad file
  filename <- "examples/badxml.xml"
  expect_warning(g2 <- read(filename),
                 "The file examples/badxml.xml was not valid XML",
                 fixed = TRUE)
  expect_null(g2)

  # bib problems
  expect_no_error(g3 <- read("examples/bib_problem.xml"))

  # warning on batch import
  expect_warning(all <- read("examples"),
                 "The file examples/badxml.xml was not valid XML",
                 fixed = TRUE)
  expect_equal(length(all), 8)
})

test_that("basics", {
  filename <- demoxml()
  s <- read(filename)
  expect_equal(class(s), c("scivrs_paper", "list"))

  title <- "To Err is Human: An Empirical Investigation"
  expect_equal(s$id, "to_err_is_human")
  expect_equal(s$info$title, title)

  expect_equal(substr(s$info$description, 1, 10), "This paper")

  expect_equal(nrow(s$full_text), 24)

  # check grobid alias
  s2 <- read_grobid(filename)
  expect_equal(s, s2)
})

test_that("urls", {
  filename <- demoxml()
  s <- read(filename)
  # check for markdown [text](url) style
  # osf <- search_text(s, "\\[.+\\]\\(.+\\)", return = "match")

  # check for <url> style
  osf <- search_text(s, "(?<=<)[^>]+(?=>)", return = "match", perl = TRUE)
  expect_equal(osf$text, c("https://osf.io/5tbm9",
                           "https://osf.io/629bx"))
})


test_that("read_xml", {
  expect_true(is.function(read_xml))

  # non-grobid XML
  filename <- tempfile(fileext = ".xml")
  xml2::read_html("<p>Hello</p>") |>
    xml2::write_xml(filename, options = "as_xml")
  xml <- read_xml(filename)
  expect_equal(xml2::xml_text(xml), "Hello")

  filename <- demoxml()
  xml <- read_xml(filename)
  expect_s3_class(xml, "xml_document")

  title <- xml2::xml_find_first(xml, "//title") |> xml2::xml_text()
  exp <- "To Err is Human: An Empirical Investigation"
  expect_equal(title, exp)
})

test_that("tei_info", {
  filename <- "examples/0956797613520608.xml"
  xml <- read_xml(filename)
  info <- tei_info(xml)

  # expected
  title <- "Continuous Theta-Burst Stimulation Demonstrates a Causal Role of Premotor Homunculus in Action Understanding"
  description <- "agent performing the same or a similar action (di"
  keywords <- c("mirror-neuron system",
                "action understanding",
                "theta-burst stimulation",
                "social cognition",
                "theory of mind",
                "social interaction",
                "social perception")
  doi <- "10.1177/0956797613520608"
  received <- "2013-08-16"
  accepted <- "2013-12-22"

  expect_equal(info$title, title)
  expect_equal(info$description, description)
  expect_equal(info$keywords, keywords)
  expect_equal(info$doi, doi)
  expect_equal(info$received, received)
  expect_equal(info$accepted, accepted)
})


test_that("tei_full_text", {
  xml <- read_xml("examples/0956797613520608.xml")
  body <- tei_full_text(xml) |> process_full_text()
  sections <- c("abstract", "intro", "method", "results",
                "discussion", "acknowledgement","funding",
                "annex", "fig", "tab")
  expect_equal(unique(body$section), sections)

  # no repeat section/div numbers
  divs <- dplyr::count(body, section, header, div)
  expect_equal(nrow(divs), length(unique(paste(divs$section, divs$div))))
})

test_that("sections", {
  # sections read in correctly
  xml <- read_xml("examples/10.1002_ece3.11050.xml")
  body <- tei_full_text(xml) |> process_full_text()
  expect_equal(body$section[35:37], rep("method", 3))
})

test_that("get figures ", {
  xml <- read_xml("examples/0956797613520608.xml")
  text <- tei_full_text(xml) |> process_full_text()
  figs <- sum(text$section == "fig")
  tabs <- sum(text$section == "tab")
  fig_ids <- text$div[text$section == "fig"] |> unique()

  expect_equal(figs, 22)
  expect_equal(tabs, 1)
  expect_equal(fig_ids, 0:3)
})

# test_that("get tables ", {
#   expect_true(is.function(get_tables))
#
#   filename <- "examples/0956797613520608.xml"
#   xml <- read_xml(filename)
#   tbls <- get_tables(xml)
# }

test_that("get notes ", {
  xml <- read_xml("footnotes/3544548.3580942.xml")
  text <- tei_full_text(xml) |> process_full_text()
  notes <- sum(text$section == "foot")
  note_ids <- text$div[text$section == "foot"] |> unique()

  expect_equal(notes, 8)
  expect_equal(note_ids, 0:7)

  xml <- read_xml("examples/0956797613520608.xml")
  text <- tei_full_text(xml) |> process_full_text()
  notes <- sum(text$section == "foot")

  expect_equal(notes, 0)
})

test_that("tei_authors", {
  xml <- "examples/0956797613520608.xml" |> read_xml()

  authors <- tei_authors(xml)
  expect_equal(length(authors), 7)
  expect_equal(authors[[1]]$name,
               list(surname = "Michael", given = "John"))
  expect_equal(authors[[1]]$email, "johnmichaelaarhus@gmail.com")
  expect_equal(authors[[2]]$affiliation[[2]],
               list(department = "Institute of Cognitive Neuroscience",
                    institution = "University College London"))
})

test_that("xml2bib", {
  xml <- "examples/0956797613520608.xml" |> read_xml()
  refs <- xml2::xml_find_all(xml, "//listBibl //biblStruct")

  # journal article
  ref <- refs[[1]]
  bib1 <- xml2bib(ref)
  bibtext1 <- format(bib1, width = NULL)
  observed <- gsub("\\n", " ", bibtext1) |> gsub("“|”", "\"", x = _)
  expected <- "Basso A, Capitani E, Della Sala S, Laiacona M, Spinnler H (1987). \"Recovery from ideomotor apraxia: A study on acute stroke patients.\" _Brain_, *110*, 747-760."
  expect_true(inherits(bib1, "bibentry"))
  expect_equal(observed, expected)

  # book chapter
  ref <- refs[[7]]
  bib7 <- xml2bib(ref)
  bibtext7 <- format(bib7)
  observed <- gsub("\\n", " ", bibtext7) |> gsub("“|”", "\"", x = _)
  expected <- "Csibra G (2008). \"Action mirroring and action understanding: An alternative account.\" In Haggard P, Rossetti Y, Kawato M (eds.), _Sensorimotor foundation of higher cognition: Attention and performance_, 435-458. Oxford University Press."
  expect_equal(observed, expected)

  # in press
  ref <- refs[[6]]
  bib6 <- xml2bib(ref)
  bibtext6 <- format(bib6)
  observed <- gsub("\\n", " ", bibtext6) |> gsub("“|”", "\"", x = _)
  expected <- "Cook R, Bird G, Catmur C, Press C, Heyes CM (in press). \"Mirror neurons: From origin to function.\" _Behavioral & Brain Sciences_."
  expect_equal(observed, expected)

  # book
  ref <- refs[[37]]
  bib37 <- xml2bib(ref)
  bibtext37 <- format(bib37)
  expected <- "Schneider W, Eschman A, Zuccolotto A (2001). _E-Prime user's guide_. Psychology Software Tools."
  expect_equal(gsub("\\n", " ", bibtext37), expected)

  expect_no_error( bib_all <- lapply(refs[2], xml2bib) )
})

test_that("tei_bib", {
  expect_true(is.function(tei_bib))

  filename <- demoxml()
  xml <- read_xml(filename)

  refs <- tei_bib(xml)
  expect_equal(names(refs), c("references", "citations"))

  exp <- c("bib_id", "ref", "doi", "bibtype",
           "title", "journal", "year", "authors")
  expect_equal(names(refs$references), exp)
  expect_equal(nrow(refs$references), 2)

  expect_equal(names(refs$citations), c("bib_id", "text"))

  # no raw_references
  expect_no_error(g <- read("examples/bib2.xml"))
  expect_true( all(!is.na(g$references$ref)) )


  # exclude <bibr> with no type
  filename <- "psychsci/light/0956797613520608.xml"
  xml <- read_xml(filename)
  refs <- tei_bib(xml)

  start_b <- refs$citations$bib_id |> grepl("^b", x = _)
  expect_true(all(start_b))
})

test_that("iteration", {
  expect_error(read("noxml"),
               "^There are no xml, docx or txt files in the directory noxml")

  grobid_dir <- demodir()
  s <- read(grobid_dir)

  file_list <- list.files(grobid_dir, ".xml")

  expect_equal(length(s), 3)
  expect_equal(names(s) |> paste0(".xml"), file_list)
  expect_s3_class(s, "scivrs_paperlist")
  expect_s3_class(s[[1]], "scivrs_paper")
  expect_s3_class(s[[2]], "scivrs_paper")
  expect_s3_class(s[[3]], "scivrs_paper")

  expect_equal(s[[1]]$id, "eyecolor")
  expect_equal(s[[2]]$id, "incest")
  expect_equal(s[[3]]$id, "prereg")

  expect_equal(s[[1]]$info$title, "Positive sexual imprinting for human eye color")
  expect_equal(s[[2]]$info$title, "Having other-sex siblings predicts moral attitudes to sibling incest, but not parent-child incest")
  expect_equal(s[[3]]$info$title, "Will knowledge about more efficient study designs increase the willingness to pre-register?")

  # separate xmls
  filenames <- demodir() |> list.files(".xml", full.names = TRUE)
  s <- read(filenames)
  expect_s3_class(s, "scivrs_paperlist")
  expect_equal(names(s) |> paste0(".xml"), file_list)

  s <- read(filenames[3:1])
  expect_s3_class(s, "scivrs_paperlist")
  expect_equal(names(s) |> paste0(".xml"), file_list[3:1])

  # recursive file search
  s <- read("nested")
  expect_s3_class(s, "scivrs_paperlist")
  nested_files <- c("3544548.3580942",
                    "nest/3613904.3642568")
  expect_equal(s[[1]]$info$filename, "nested/3544548.3580942.xml")
  expect_equal(s[[2]]$info$filename, "nested/nest/3613904.3642568.xml")
  s[[2]]$info$filename
  expect_true(all(nested_files %in% names(s)))
})


test_that("grobid-versions", {
  filename <- list.files("grobid-test/full", full.names = TRUE)

  # read_xml
  xml <- read_xml(filename[[1]])
  expect_s3_class(xml, "xml_document")

  # read
  paper <- read(filename)

  # small
  sfilename <- list.files("grobid-test/small", full.names = TRUE)
  sxml <- read_xml(sfilename[[1]])
  spaper <- read(sfilename)

  skip("Grobid version testing: failures expected")

  # paper 1
  p <- 1
  pinfo <- paper$info[[p]]
  pinfo$filename <- NULL
  sinfo <- spaper$info[[p]]
  sinfo$filename <- NULL
  expect_equal(pinfo, sinfo)
  expect_equal(paper[[p]]$references, spaper[[p]]$references)
  expect_equal(paper[[p]]$citations, spaper[[p]]$citations)

  mismatch <- sapply(1:nrow(paper[[p]]$full_text), \(i) {
    !all(paper[[p]]$full_text[i, "text"] == spaper[[p]]$full_text[i, "text"]) |> isTRUE()
  }) |> which()

  paper[[p]]$full_text[25, "text"]
  spaper[[p]]$full_text[25, "text"]

  # full parsed DOI better and sentence 25
  # short parsed Fig 3 correctly

  # paper 2
  p <- 2
  pinfo <- paper$info[[p]]
  pinfo$filename <- NULL
  sinfo <- spaper$info[[p]]
  sinfo$filename <- NULL
  expect_equal(pinfo, sinfo)
  expect_equal(paper[[p]]$references, spaper[[p]]$references)
  expect_equal(paper[[p]]$citations, spaper[[p]]$citations)

  mismatch <- sapply(1:nrow(paper[[p]]$full_text), \(i) {
    !all(paper[[p]]$full_text[i, "text"] == spaper[[p]]$full_text[i, "text"]) |> isTRUE()
  }) |> which()

  paper[[p]]$full_text[125, "text"]
  spaper[[p]]$full_text[125, "text"]

  # small read line 125 correctly (full added fig 6 text)
  # full read 129 correctly

  # paper 3
  p <- 3
  pinfo <- paper$info[[p]]
  pinfo$filename <- NULL
  sinfo <- spaper$info[[p]]
  sinfo$filename <- NULL
  expect_equal(pinfo, sinfo)
  expect_equal(paper[[p]]$references, spaper[[p]]$references)
  expect_equal(paper[[p]]$citations, spaper[[p]]$citations)

  mismatch <- sapply(1:nrow(paper[[p]]$full_text), \(i) {
    !all(paper[[p]]$full_text[i, "text"] == spaper[[p]]$full_text[i, "text"]) |> isTRUE()
  }) |> which()

  paper[[p]]$full_text[2, "text"]
  spaper[[p]]$full_text[2, "text"]

  # short: better citation 10, did not miss the whole abstract!
  # long: absolutely mangled identifying the abstract, one citation problem because of this

  # paper 4
  p <- 4
  pinfo <- paper$info[[p]]
  pinfo$filename <- NULL
  sinfo <- spaper$info[[p]]
  sinfo$filename <- NULL
  expect_equal(pinfo, sinfo)
  expect_equal(paper[[p]]$references, spaper[[p]]$references)
  expect_equal(paper[[p]]$citations, spaper[[p]]$citations)

  mismatch <- sapply(1:nrow(paper[[p]]$full_text), \(i) {
    !all(paper[[p]]$full_text[i, "text"] == spaper[[p]]$full_text[i, "text"]) |> isTRUE()
  }) |> which()

  paper[[p]]$full_text[149, "text"]
  spaper[[p]]$full_text[147, "text"]
  View(paper[[p]]$full_text)
  View(spaper[[p]]$full_text)

  # short parsed 97 better
  # long: Parsed 103 better, added irrelevant table text line 149

  # paper 5
  p <- 5
  pinfo <- paper$info[[p]]
  pinfo$filename <- NULL
  sinfo <- spaper$info[[p]]
  sinfo$filename <- NULL
  expect_equal(pinfo, sinfo)
  expect_equal(paper[[p]]$references, spaper[[p]]$references)
  expect_equal(paper[[p]]$citations, spaper[[p]]$citations)

  mismatch <- sapply(1:nrow(paper[[p]]$full_text), \(i) {
    !all(paper[[p]]$full_text[i, "text"] == spaper[[p]]$full_text[i, "text"]) |> isTRUE()
  }) |> which()

  # no mismatch!
})

test_that("get_app_info", {
  filename <- "examples/to_err_is_human.xml"
  xml <- read_xml(filename)
  info <- get_app_info(xml)

  expect_equal(info$version, "0.8.1")
  expect_equal(info$when, "2025-02-25T18:30+0000")
  expect_equal(info$url, "https://github.com/kermitt2/grobid")

  # filename <- "grobid-test/full/0956797613520608.xml"
  # paper <- read(filename)
  # expect_equal(paper$app$version, "0.8.2")
  # expect_equal(paper$app$when, "2025-05-18T17:52+0000")
  # expect_equal(paper$app$url, "https://github.com/kermitt2/grobid")
})

test_that("paper_validate", {
  expect_true(is.function(paper_validate))

  expect_error(paper_validate())
  expect_error(paper_validate(1))
  expect_error(paper_validate(list(hi = 1)))

  # single valid paper
  paper <- psychsci[[1]]
  pc <- paper_validate(paper)

  expect_equal(pc$id, paper$id)
  expect_equal(pc$valid, TRUE)
  expect_equal(pc$doi, "")
  expect_equal(pc$title, "")
  expect_equal(pc$abstract, "")
  expect_equal(pc$refs, 41)

  # batch
  paper <- psychsci
  pc <- paper_validate(paper)

  expect_true(is.data.frame(pc))
})

