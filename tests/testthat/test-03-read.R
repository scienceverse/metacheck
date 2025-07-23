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

test_that("apa-info", {
  filename <- "formats/apa.xml"
  xml <- read_xml(filename)
  info <- apa_info(xml)

  exp <- list(
    title = "“You’re Just Envious”: Inferring Benign and Malicious Envy From Facial Expressions and Contextual Information",
    description = "Envy shapes social hierarchies. To protect their rank, envied persons react to the threat posed by enviers. Doing so requires that envied persons initially perceive who envies them. However, a common perspective is that envy lacks a unique expression and that enviers disguise their experience, preventing the social perception of envy. In contrast to this perspective, recent evidence indicates that observers perceive benign and malicious forms of envy accurately when they can integrate information about targets. These findings suggest that observers infer envy based on multiple, contextual cues. We hypothesized that observers infer envy from facial and bodily expressions in comparison situations. Specifically, observers should infer benign envy when a target, who encounters an advantaged person, turns with disappointment toward the advantage. Conversely, observers should infer malicious envy when the target turns with anger toward the advantaged person. Three preregistered studies tested these hypotheses (total N = 693). In Studies 1 and 2, targets turned with an emotional or neutral expression either toward a person silhouette or a valuable object, and participants rated targets’ envy. In Study 3, participants performed the same task with more realistic stimuli. Across studies, emotional display and head turning had independent effects on inferences of benign and malicious envy. Furthermore, observers inferred envy more when the target expressed an emotion instead of remaining neutral. We discuss how the results inform research on the social perception of envy.",
  keywords = c("benign and malicious envy", "bodily expression",
               "facial expression", "social context", "social perception"),
  doi = "10.1037/emo0001047",
  pub_print = "2022-02-NA",
  pub_online = "2022-01-06",
  received = "2021-01-08",
  revised = "2021-06-30",
  accepted = "2021-09-08"
)
  expect_equal(info$title, exp$title)
  expect_equal(info$description, exp$description)
  expect_equal(info$keywords, exp$keywords)
  expect_equal(info$doi, exp$doi)
  expect_equal(info$pub_print, exp$pub_print)
  expect_equal(info$pub_online, exp$pub_online)
  expect_equal(info$received, exp$received)
  expect_equal(info$revised, exp$revised)
  expect_equal(info$accepted, exp$accepted)
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

