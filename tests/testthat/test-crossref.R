test_that("exists", {
  expect_true(is.function(papercheck::crossref))
  expect_no_error(helplist <- help(crossref, papercheck))

  expect_true(is.function(papercheck::openalex))
  expect_no_error(helplist <- help(openalex, papercheck))
})

test_that("crossref", {
  skip()
  skip_if_offline("api.labs.crossref.org")

  doi <- "10.1177/fake"
  expect_message(cr <- crossref(doi))
  expect_equal(cr, list())

  # single doi
  doi <- "10.1177/0956797614520714"
  cr <- crossref(doi)
  expect_equal(cr$`cr-labs-updates`[[1]]$`update-nature`, "Retraction")

  # list of DOIs
  dois <- info_table(psychsci, "doi")
  dois$doi <- gsub("pss\\.", "", dois$doi) |> gsub("sagepub\\.", "", x = _)
  doi <- dois$doi[1:2]
  cr2 <- crossref(doi)
})

test_that("openalex", {
  skip_if_offline("api.openalex.org")

  doi <- "10.1177/fake"
  expect_warning(oa <- openalex(doi))
  expect_equal(oa, list(error = doi))

  # short DOI
  doi <- "10.1177/0956797614520714"
  oa <- openalex(doi)
  expect_equal(oa$is_retracted, TRUE)

  # long DOI
  doi <- c("https://doi.org/10.1177/0956797613520608")
  oa <- openalex(doi)
  # expect_equal(oa$id, "https://openalex.org/W2134722098") disabled ID comparison due to changing ids on OpenAlex side

  # multiple DOIs
  dois <- c("10.1177/0956797613520608", "10.1177/0956797614522816")
  oa <- openalex(dois)
  # expect_equal(oa[[1]]$id, "https://openalex.org/W2134722098") disabled ID comparison due to changing ids on OpenAlex side
  # expect_equal(oa[[2]]$id, "https://openalex.org/W2103593746") disabled ID comparison due to changing ids on OpenAlex side

  # DOI from paper
  paper <- psychsci[[1]]
  oa <- openalex(paper)
  # expect_equal(oa$id, "https://openalex.org/W2134722098") disabled due to changing ids on OpenAlex side

  # DOIs from paperlist
  paper <- psychsci[1:2]
  oa <- openalex(paper)
  # expect_equal(oa[[1]]$id, "https://openalex.org/W2134722098") disabled ID comparison due to changing ids on OpenAlex side
  # expect_equal(oa[[2]]$id, "https://openalex.org/W2103593746") disabled ID comparison due to changing ids on OpenAlex side

  # one malformatted DOI
  paper <- psychsci[10:11]
  paper[[2]]$info$doi <- paste0(paper[[2]]$info$doi, "x")
  expect_warning(oa <- openalex(paper))
  # expect_equal(oa[[1]]$id, "https://openalex.org/W1824074316") disabled ID comparison due to changing ids on OpenAlex side
  expect_equal(oa[[2]], list(error = paper[[2]]$info$doi))

  # select
  doi <- "10.1177/0956797614520714"
  oa <- openalex(doi, select = "is_retracted")
  expect_equal(oa$is_retracted, TRUE)
})

test_that("exists", {
  expect_true(is.function(papercheck::paper_info))
  expect_no_error(helplist <- help(paper_info, papercheck))
})

test_that("paper_info", {
  paper <- psychsci[[1]]

  # good DOI, in OpenAlex
  p1 <- paper_info(paper)

  # bad DOI
  paper$info$doi <- "baddoi"
  p2 <- paper_info(paper)
})
