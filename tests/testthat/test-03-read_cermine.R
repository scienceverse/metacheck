#setwd("tests/testthat/")

test_that("exists", {
  expect_true(is.function(papercheck::read_cermine))
  expect_no_error(helplist <- help(read_cermine, papercheck))
})

test_that("errors", {
  expect_error(read_cermine(bad_arg))
})

test_that("defaults", {
  filename <- system.file("psychsci/0956797620955209.cermine.xml",
                          package = "papercheck")
  paper <- read_cermine(filename)
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
  expect_equal(paper$references |> nrow(), 36)
  expect_equal(paper$citations |> nrow(), 34)
})

test_that("read_cermine_xml", {
  expect_true(is.function(read_cermine_xml))

  # non-cermine XML
  filename <- tempfile(fileext = "xml")
  xml2::read_html("<p>Hello</p>") |>
    xml2::write_xml(filename, options = "as_xml")
  expect_error( read_cermine_xml(filename),
                "does not parse as a valid Cermine XML")

  filename <- system.file("extdata/to_err_is_human.cermine.xml",
                          package = "papercheck")
  xml <- read_cermine_xml(filename)
  expect_s3_class(xml, "xml_document")

  title <- xml2::xml_find_first(xml, "//article-title") |>
    xml2::xml_text()
  exp <- "To Err is Human: An Empirical Investigation"
  expect_equal(title, exp)
})

test_that("get_full_text", {
  filename <- system.file("psychsci/0956797620955209.cermine.xml",
                          package = "papercheck")
  xml <- read_cermine_xml(filename)
  body <- get_cermine_full_text(xml, "test")
  sections <- c("abstract", "intro", "method", "results",
                "discussion")
  expect_equal(unique(body$section), sections)
})


test_that("get_authors", {
  filename <- system.file("psychsci/0956797620955209.cermine.xml",
                          package = "papercheck")
  xml <- read_cermine_xml(filename)

  expect_no_error( authors <- get_cermine_authors(xml) )
  # these are pretty messed up, but it's cermine's fault
})

test_that("get_cermine_refs", {
  expect_true(is.function(get_cermine_refs))

  filename <- system.file("psychsci/0956797620955209.cermine.xml",
                          package = "papercheck")
  xml <- read_cermine_xml(filename)

  refs <- get_cermine_refs(xml)
  expect_equal(names(refs), c("references", "citations"))

  expect_equal(names(refs$references), c("bib_id", "doi", "ref"))
  expect_equal(nrow(refs$references), 36)

  expect_equal(names(refs$citations), c("bib_id", "text"))
})

test_that("iteration", {
  expect_error(read_cermine("noxml"),
               "^There are no xml files in the directory")

  cermine_dir <- system.file("cermine", package = "papercheck")
  s <- read_cermine(cermine_dir)

  file_list <- list.files(cermine_dir, ".xml")

  expect_equal(length(s), 3)
  expect_equal(names(s) |> paste0(".xml"), file_list)
  expect_s3_class(s[[1]], "scivrs_paper")
  expect_s3_class(s[[2]], "scivrs_paper")
  expect_s3_class(s[[3]], "scivrs_paper")

  expect_equal(s[[1]]$id, "eyecolor.cermine")
  expect_equal(s[[2]]$id, "incest.cermine")
  expect_equal(s[[3]]$id, "prereg.cermine")

  expect_equal(s[[1]]$info$title, "Positive sexual imprinting for human eye color")
  expect_equal(s[[2]]$info$title, "Having other-sex siblings predicts moral attitudes to sibling incest, but not parent-child incest")
  #expect_equal(s[[3]]$info$title, "Will knowledge about more efficient study designs increase the willingness to pre-register?")
  expect_equal(s[[3]]$info$title, NA_character_)

  # separate xmls
  filenames <- cermine_dir |> list.files(".xml", full.names = TRUE)
  s <- read_cermine(filenames)
  expect_equal(names(s) |> paste0(".xml"), file_list)

  s <- read_cermine(filenames[3:1])
  expect_equal(names(s) |> paste0(".xml"), file_list[3:1])

  # recursive file search
  suppressWarnings(
    s <- read_cermine(system.file(package="papercheck"))
  )
  nested_files <- c("extdata/to_err_is_human.cermine",
                    "cermine/eyecolor.cermine",
                    "cermine/incest.cermine",
                    "cermine/prereg.cermine",
                    "psychsci/0956797620955209.cermine")
  expect_true(all(nested_files %in% names(s)))
})

