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
  expect_equal(paper$references |> nrow(), 42)
  expect_equal(paper$citations |> nrow(), 71)

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

test_that("get_full_text", {
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

test_that("get_cermine_refs", {
  filename <- system.file("psychsci/0956797620955209.cermine.xml",
                          package = "papercheck")
  xml <- read_xml(filename)

  refs <- jats_bib(xml)
  expect_equal(names(refs), c("references", "citations"))

  bibnames <- c("bib_id", "ref", "doi", "bibtype", "title", "journal", "authors",
                "year", "volume", "issue", "fpage", "lpage")
  expect_equal(names(refs$references), bibnames)
  expect_equal(nrow(refs$references), 42)

  expect_equal(names(refs$citations), c("bib_id", "text"))
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

