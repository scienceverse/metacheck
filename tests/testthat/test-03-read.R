test_that("exists", {
  expect_true(is.function(papercheck::read))
  expect_true(is.function(papercheck:::read_xml))
  expect_true(is.function(papercheck:::apa_info))
  expect_true(is.function(papercheck:::apa_authors))
  expect_true(is.function(papercheck:::apa_full_text))
  expect_true(is.function(papercheck:::nlm_info))
  expect_true(is.function(papercheck:::nlm_authors))
  expect_true(is.function(papercheck:::nlm_full_text))
  expect_true(is.function(papercheck:::jats_bib))
  expect_true(is.function(papercheck:::tei_info))
  expect_true(is.function(papercheck:::tei_authors))
  expect_true(is.function(papercheck:::tei_full_text))
  expect_true(is.function(papercheck:::tei_bib))
  expect_no_error(helplist <- help(read, papercheck))
})

test_that("errors", {
  expect_error(read(bad_arg))
})

test_that("APA", {
  filename <- "formats/apa.xml"
  paper <- read(filename)

  expect_equal(paper$id, "apa")
  expect_equal(paper$info$filename, filename)
  expect_equal(paper$info$title, "“You’re Just Envious”: Inferring Benign and Malicious Envy From Facial Expressions and Contextual Information")
  expect_equal(paper$info$keywords, c("benign and malicious envy",
                                      "bodily expression",
                                      "facial expression",
                                      "social context",
                                      "social perception"))
  expect_true(grepl("^Envy shapes social hierarchies", paper$info$description))

  expect_equal(length(paper$authors), 3)
  author1 <- list(surname = "Lange", given = "Jens")
  orcid2 <- "0000-0001-6939-8174"
  aff3 <- "Department of Social Psychology, University of Amsterdam"
  expect_equal(paper$authors[[1]]$name, author1)
  expect_equal(paper$authors[[2]]$orcid, orcid2)
  expect_equal(paper$authors[[3]]$affiliation, aff3)

  # full text
  expect_equal(nrow(paper$full_text), 1213)
  sections <- unique(paper$full_text$section)
  exp <- c("abstract", "intro", "method", "results", "discussion",
           "foot", "annex", "fig")
  expect_equal(sections, exp)

  headers <- unique(paper$full_text$header)
  expect_true("Materials and Procedure" %in% headers)

  # references
  expect_equal(paper$references |> nrow(), 78)
  expect_equal(paper$citations |> nrow(), 143)
  obs_bibtypes <- paper$references$bibtype |> unique()
  exp_bibtypes <- c("Article", "Misc", "InCollection")
  expect_equal(obs_bibtypes, exp_bibtypes)
})

test_that("TEI", {
  filename <- "formats/published.pdf.tei.xml"
  paper <- read(filename)

  expect_equal(paper$id, "published.pdf.tei")
  expect_equal(paper$info$filename, filename)
  #expect_equal(paper$info$title, "“You’re Just Envious”: Inferring Benign and Malicious Envy From Facial Expressions and Contextual Information")
  expect_equal(paper$info$keywords, c("benign and malicious envy",
                                      "bodily expression",
                                      "facial expression",
                                      "social context",
                                      "social perception"))
  expect_true(grepl("^Envy shapes social hierarchies", paper$info$description))

  expect_equal(length(paper$authors), 3)
  author1 <- list(surname = "Lange", given = "Jens")
  orcid2 <- "0000-0001-6939-8174"
  aff3 <- list(
    list(
      department = "Department of Social Psychology",
      institution = "University of Amsterdam"
    )
  )
  expect_equal(paper$authors[[1]]$name, author1)
  expect_equal(paper$authors[[2]]$orcid, orcid2)
 # expect_equal(paper$authors[[3]]$affiliation, aff3)

  # full text
  expect_equal(nrow(paper$full_text), 551)
  sections <- unique(paper$full_text$section)
  exp <- c("abstract", "intro", "method", "results", "discussion",
           "funding", "availability", "fig", "foot")
  expect_equal(sections, exp)

  headers <- unique(paper$full_text$header)
  expect_true("Materials and Procedure" %in% headers)

  # references
  expect_equal(paper$references |> nrow(), 77) # 78?
  expect_equal(paper$citations |> nrow(), 133)
  obs_bibtypes <- paper$references$bibtype |> unique()
  exp_bibtypes <- c("Article", "Misc", "InCollection")
  # expect_equal(obs_bibtypes, exp_bibtypes)
})

test_that("Cermine", {
  filename <- "formats/cermine-published.xml"
  paper <- read(filename)

  expect_equal(paper$id, "cermine-published")
  expect_equal(paper$info$filename, filename)
  expect_equal(paper$info$title, "“You're Just Envious”: Inferring Benign and Malicious Envy From Facial Expressions and Contextual Information")
  expect_equal(paper$info$keywords, c("benign and malicious envy",
                                      "bodily expression",
                                      "facial expression",
                                      "social context",
                                      "social perception"))
  expect_true(grepl("^Envy shapes social hierarchies", paper$info$description))

  expect_equal(length(paper$authors), 3)
  author1 <- list(surname = "Lange", given = "Jens")
  orcid2 <- "0000-0001-6939-8174"
  aff3 <- "Department of Social Psychology, University of Amsterdam"
  expect_equal(paper$authors[[1]]$name, author1)
  #expect_equal(paper$authors[[2]]$orcid, orcid2)
  expect_equal(paper$authors[[3]]$affiliation[[1]], aff3)

  # full text
  expect_equal(nrow(paper$full_text), 873)
  sections <- unique(paper$full_text$section)
  exp <- c("abstract", "intro", "method", "results", "discussion",
           "foot", "annex", "fig")
  expect_equal(sections, exp[1:5])

  headers <- unique(paper$full_text$header)
  expect_true("Materials and Procedure" %in% headers)

  # references
  expect_equal(paper$references |> nrow(), 62) # ? why not 78
  expect_equal(paper$citations |> nrow(), 129)
  obs_bibtypes <- paper$references$bibtype |> unique()
  exp_bibtypes <- c("Article", "Misc", "InCollection")
  expect_equal(obs_bibtypes, exp_bibtypes)
})
