fmt <- function(name) test_path("fixtures", "formats", name)

test_that("peer_review: the reviews a publisher's XML prints", {
  mo <- module_run(read(fmt("jats_cases.xml")), "peer_review", lookup = FALSE)
  expect_equal(mo$traffic_light, "info")
  t <- mo$table
  expect_equal(t$review_type, c("referee_report", "author_response"))
  expect_equal(t$reviewer, c("Rita Reviewer", NA))
  expect_equal(t$where, c("https://doi.org/10.1234/jtc.2026.001.r001", "in the article's XML"))
  expect_true(all(t$has_text))
  expect_equal(mo$review_text$text[mo$review_text$review_id == 1], "The reviewer liked it.")
  st <- mo$summary_table
  expect_true(st$peer_review_open)
  expect_equal(c(st$review_n, st$review_text_n), c(2L, 2L))
  # each review with its text is in a box that folds out
  expect_equal(sum(grepl("callout-note", mo$report, fixed = TRUE)), 2)
  expect_true(any(grepl("We changed the analysis.", mo$report, fixed = TRUE)))
  expect_match(mo$summary_text, "The peer review of this article is public: we found 2 documents, 2 with their text")
})

test_that("peer_review: the report has a row per address", {
  env <- new.env(parent = asNamespace("metacheck"))
  sys.source(metacheck:::module_find("peer_review"), envir = env)
  # one document: its kind; several: counted by kind, in the order given
  expect_equal(env$.peer_review_count("referee_report"), "Reviewer report")
  expect_equal(env$.peer_review_count(c("referee_report", "decision_letter", "referee_report", "history", "history")),
               "2 reviewer reports, 1 decision letter, 2 peer review histories")

  # twelve documents at one address are one row of the report
  docs <- data.frame(
    paper_id = "p", review_id = 1:4,
    review_type = c("referee_report", "decision_letter", "author_response", "decision_letter"),
    stage = "pre-publication", round = c(1L, 1L, 2L, 2L),
    date = c("2025-01-10", "2025-01-20", "2025-03-01", "2025-03-15"),
    reviewer = c("Rita Reviewer", NA, NA, NA), title = NA_character_, doi = NA_character_,
    where = "https://www.webofscience.com/api/gateway/wos/peer-review/10.1234/X",
    source = "crossref", has_text = FALSE)
  env$.peer_review_of <- function(p, id, lookup = TRUE) list(
    docs = docs, texts = data.frame(paper_id = character(0), review_id = integer(0),
                                    paragraph_id = integer(0), text = character(0)),
    pubpeer = list(n = 2L, url = "https://pubpeer.com/publications/ABC"), not_asked = character(0))
  mo <- env$peer_review(test_paper("This article was peer reviewed."))
  expect_equal(nrow(mo$table), 4)
  expect_match(mo$summary_text, "The peer review of this article is public: we found 4 documents. PubPeer has 2 comments on it.", fixed = TRUE)
  rep <- paste(mo$report, collapse = "\n")
  expect_match(rep, "1 reviewer report, 2 decision letters, 1 author response", fixed = TRUE)
  expect_match(rep, "1 to 2", fixed = TRUE)
  expect_match(rep, "2025-01-10 to 2025-03-15", fixed = TRUE)
  expect_match(rep, "These reviews are on Web of Science", fixed = TRUE)
  expect_match(rep, "pubpeer.com/publications/ABC", fixed = TRUE)
})

test_that("peer_review: a link to the peer review history", {
  # Wiley: the XML links to the history, and a sentence gives the same link
  mo <- module_run(read(fmt("wiley_cases.xml")), "peer_review", lookup = FALSE)
  expect_equal(mo$table$review_type, "history")
  expect_equal(mo$table$where, "https://publons.com/publon/10.1111/tst.10101")
  expect_false(mo$table$has_text)
  expect_true(any(grepl("Web of Science", mo$report, fixed = TRUE)))

  # a sentence that says where the reports are, in a paper without metadata
  p <- test_paper(c("The peer review reports for this article are available at https://example.org/reviews/12.",
                    "We thank two reviewers for their comments."))
  mo <- module_run(p, "peer_review", lookup = FALSE)
  expect_equal(mo$table$where, "https://example.org/reviews/12")
  expect_equal(mo$table$source, "text")
})

test_that("peer_review: nothing found", {
  mo <- module_run(test_paper("This article was peer reviewed."), "peer_review", lookup = FALSE)
  expect_equal(mo$traffic_light, "na")
  expect_false(mo$summary_table$peer_review_open)
  expect_equal(nrow(mo$table), 0)
  # a paper list: a row per paper
  both <- read(c(fmt("jats_cases.xml"), fmt("wiley_cases.xml")))
  mo <- module_run(both, "peer_review", lookup = FALSE)
  expect_equal(mo$summary_table$review_n, c(2L, 1L))
  expect_match(mo$summary_text, "2 of 2 papers have public peer review")
})

test_that("crossref_peer_reviews", {
  skip_if_offline("api.crossref.org")
  cr <- crossref_peer_reviews("10.7554/eLife.80984")
  skip_if(is.null(cr), "Crossref did not answer")
  expect_true(all(c("decision_letter", "author_response") %in% cr$review_type))
  expect_true(all(grepl("^10\\.7554/elife\\.80984\\.sa", cr$doi)))
  expect_equal(nrow(crossref_peer_reviews(NA_character_)), 0)
  # the publisher wrote the article's DOI in another way than it is given
  # here: in capitals (reviews on Web of Science), and with one capital
  rs <- crossref_peer_reviews("10.1098/rsob.210098")
  skip_if(is.null(rs), "Crossref did not answer")
  expect_true(nrow(rs) >= 5)
  expect_true(all(grepl("publons.com|webofscience.com", rs$url)))
  lower <- crossref_peer_reviews("10.7554/elife.80984")
  skip_if(is.null(lower), "Crossref did not answer")
  expect_equal(lower$doi, cr$doi)
})
