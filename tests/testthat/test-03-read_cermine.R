#setwd("tests/testthat/")


test_that("defaults", {
  filename <- system.file("psychsci/0956797620955209.cermine.xml",
                          package = "papercheck")
  paper <- read(filename)
  title <- "Preregistered Direct Replication of “Sick Body, Vigilant Mind: The Biological Immune System Activates the Behavioral Immune System”"
  abstract <- "^The tendency to attend to and avoid cues to pathogens"
  keywords <- c("evolutionary psychology", "threat", "attention", "disgust",
                "health", "open data", "preregistered")
  cols <- c("text", "section", "header", "div", "p", "s", "id")

  expect_equal(paper$info$filename, filename)
  expect_equal(paper$info$title, title)
  expect_true(grepl(abstract, paper$info$description))
  expect_equal(paper$info$keywords, keywords)
  expect_equal(paper$full_text |> nrow(), 252)
  expect_equal(colnames(paper$full_text), cols)
  expect_equal(paper$bib |> nrow(), 42)
  expect_equal(paper$xrefs |> nrow(), 78)

  # check cermine alias
  paper2 <- read_cermine(filename)
  expect_equal(paper, paper2)
})

test_that("read_xml", {
  filename <- system.file("extdata/to_err_is_human.cermine.xml",
                          package = "papercheck")
  xml <- read_xml(filename)
  expect_s3_class(xml, "xml_document")

  title <- xml2::xml_find_first(xml, "//article-title") |>
    xml2::xml_text()
  exp <- "To Err is Human: An Empirical Investigation"
  expect_equal(title, exp)
})

test_that("nlm_info", {
  filename <- "cermine_example.xml"
  xml <- read_xml(filename)
  info <- nlm_info(xml)

  # expected
  title <- "Ambivalent Sexism and Tolerance of Violence Against Women in India"
  description <- "We examined associations between sexist beliefs and tolerance of violence against women in India using a nationally representative probability sample of adults (n = 133,398). Research consistently indicates that hostile sexism fosters tolerance of violence against women. However, benevolent sexism is sometimes associated with higher tolerance and sometimes with lower tolerance of violence. We proposed that this inconsistency could be resolved by considering the source of violence: Is violence perpetrated by outsiders or intimate partners? Results of a multigroup structural equation model showed that endorsement of hostile sexism was related to greater tolerance of violence regardless of the source. In contrast, endorsement of benevolent sexism was associated with lower tolerance of violence from outsiders but was simultaneously associated with higher tolerance of spousal violence. These opposing processes indicate that although benevolent sexism promises women protection from violence, the very same ideology legitimizes spousal violence, thereby reinforcing men's power within intimate relationships."
  keywords <- c("sexism", "violence", "India",
                "gender", "open data", "open materials")
  #doi <- "10.1177/0956797613520608"
  doi <- ""

  expect_equal(info$title, title)
  expect_equal(info$description, description)
  expect_equal(info$keywords, keywords)
  expect_equal(info$doi, doi)
})

test_that("nlm_full_text", {
  filename <- system.file("psychsci/0956797620955209.cermine.xml",
                          package = "papercheck")
  xml <- read_xml(filename)
  body <- nlm_full_text(xml) |> process_full_text()
  sections <- c("abstract", "intro", "method", "results",
                "discussion")
  expect_equal(unique(body$section), sections)
})


test_that("get_authors", {
  filename <- system.file("psychsci/0956797620955209.cermine.xml",
                          package = "papercheck")
  xml <- read_xml(filename)

  expect_no_error( authors <- nlm_authors(xml) )
  # these are pretty messed up, but it's cermine's fault
})

test_that("jats_bib", {
  filename <- system.file("psychsci/0956797620955209.cermine.xml",
                          package = "papercheck")
  xml <- read_xml(filename)

  bib <- jats_bib(xml)

  bibnames <- c("xref_id", "ref", "doi", "bibtype", "title", "journal", "authors",
                "year", "volume", "issue", "fpage", "lpage")
  expect_equal(names(bib), bibnames)
  expect_equal(nrow(bib), 42)
})

test_that("iteration", {
  cermine_dir <- system.file("cermine", package = "papercheck")
  s <- read(cermine_dir)

  file_list <- list.files(cermine_dir, ".xml")

  expect_equal(length(s), 3)
  expect_equal(names(s) |> paste0(".xml"), file_list)
  expect_s3_class(s[[1]], "scivrs_paper")
  expect_s3_class(s[[2]], "scivrs_paper")
  expect_s3_class(s[[3]], "scivrs_paper")

  expect_equal(s[[1]]$id, "eyecolor.cermine")
  expect_equal(s[[2]]$id, "incest.cermine")
  expect_equal(s[[3]]$id, "prereg.cermine")

  # TODO:fix nbsp character in titles
  #expect_equal(s[[1]]$info$title, "Positive sexual imprinting for human eye color")
  expect_equal(s[[2]]$info$title, "Having other-sex siblings predicts moral attitudes to sibling incest, but not parent-child incest")
  #expect_equal(s[[3]]$info$title, "Will knowledge about more efficient study designs increase the willingness to pre-register?")
  expect_equal(s[[3]]$info$title, "")

  # separate xmls
  filenames <- cermine_dir |> list.files(".xml", full.names = TRUE)
  s <- read(filenames)
  expect_equal(names(s) |> paste0(".xml"), file_list)

  s <- read(filenames[3:1])
  expect_equal(names(s) |> paste0(".xml"), file_list[3:1])

  # recursive file search
  filename <- system.file(package="papercheck")
  suppressWarnings(
    s <- read(filename)
  )
  skip_on_covr()
  nested_files <- c("extdata/to_err_is_human.cermine",
                    "cermine/eyecolor.cermine",
                    "cermine/incest.cermine",
                    "cermine/prereg.cermine",
                    "psychsci/0956797620955209.cermine")
  expect_true(all(nested_files %in% names(s)))
})

