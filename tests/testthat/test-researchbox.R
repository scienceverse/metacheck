test_that("exists", {
  expect_true(is.function(metacheck::rbox_links))
  expect_no_error(helplist <- help(rbox_links, metacheck))

  expect_error(rbox_links(bad_arg))
})

test_that("rbox_links", {
  paper <- test_paper(url = "https://researchbox.org/801")
  links <- rbox_links(paper)
  expect_equal(nrow(links), 1)
  expect_equal(links$href[[1]], "https://researchbox.org/801")
})


#httptest2::start_capturing()
httptest2::use_mock_api()

test_that("rbox_info", {
  #skip_api("researchbox.org")

  url <- "https://researchbox.org/801"
  info <- rbox_info(url)

  target <- "He JC, Côté S. (2023) 'Are Empathic People Better Adjusted? A Test of Competing Models of Empathic Accuracy and Intrapersonal and Interpersonal Facets of Adjustment Using Self- and Peer Reports'.Psychological Science. V34(9):955-967. https://doi.org/10.1177/09567976231185127"
  license <- "All content posted to ResearchBox is under a CC By 4.0 License(all use is allowed as long as authorship of the content is attributed). When using content from ResearchBox please cite the original work, and provide a link to the URL for this box (https://researchbox.org/801)."
  authors <- "Joyce He (joyce.he@anderson.ucla.edu)\nStéphane Côté (stephane.cote@rotman.utoronto.ca)"
  abstract <- "Are individuals adept at perceiving others’ emotions optimally adjusted? We extend past research by conducting a high-powered pre-registered study that comprehensively tests five theoretical models of empathic ability (i.e., emotion recognition ability) and self-views and intra- and interpersonal facets of adjustment in a sample of 1126 undergraduate students from Canada and 2205 informants. We obtained both self- and peer-reports of adjustment and controlled for cognitive abilities as a potential confounding variable. Empathic accuracy ability (but not self-views of that ability) was positively related to relationship satisfaction rated by both participants and informants. Self-views about empathic accuracy (but not actual empathic accuracy) were positively related to life satisfaction rated by both participants and informants. All associations held when controlling for cognitive abilities."

  # using expect_match because RB keeps subtly changing formats
  expect_equal(info$rb_url, url)
  expect_match(info$RB_target, "Are Empathic People Better Adjusted",)
  expect_match(info$RB_license, "CC By 4.0", fixed = TRUE)
  expect_equal(info$RB_public, "June 09, 2023")
  expect_match(info$RB_authors, "Joyce He")
  expect_match(info$RB_authors, "Stéphane Côté")
  expect_match(info$RB_abstract, "Are individuals adept at perceiving other")

  files <- info$files[[1]]
  expect_equal(nrow(files), 9)

  #url <- "https://researchbox.org/4377&PEER_REVIEW_passcode=YHHCIU"

  ## peer review version
  rb_url <- "https://researchbox.org/1150&PEER_REVIEW_passcode=MJUAAS"
  info <- rbox_info(rb_url)
  expect_equal(info$rb_url, rb_url)
  expect_equal(info$RB_public, "October 07, 2024")
})

test_that("rbox_retrieve", {
  #skip_api("researchbox.org")
  testthat::local_mocked_bindings(
    online = \(...) TRUE
  )

  paper <- test_paper(url = c(
    "https://researchbox.org/4377",
    "https://researchbox.org/6018"
  ))
  links <- rbox_links(paper)
  info <- rbox_retrieve(links, "href")

  #expected
  public <- c("November 28, 2025", "February 15, 2026")

  expect_equal(info$href, links$href)
  expect_equal(info$RB_public, public)
})

httptest2::stop_mocking()
#httptest2::stop_capturing()


