test_that("exists", {
  expect_true(is.function(papercheck::researchbox_links))
  expect_no_error(helplist <- help(researchbox_links, papercheck))
})

test_that("errors", {
  expect_error(researchbox_links(bad_arg))
})

test_that("researchbox_links", {
  links <- researchbox_links(psychsci)
  expect_equal(nrow(links), 3)
  expect_equal(links$text[[1]], "https://researchbox.org/801")
})

test_that("researchbox_info", {
  skip_on_covr()
  skip_if_offline("researchbox.org")

  url <- "https://researchbox.org/801"
  info <- researchbox_info(url)

  target <- "He JC, Côté S. (2023) 'Are Empathic People Better Adjusted? A Test of Competing Models of Empathic Accuracy and Intrapersonal and Interpersonal Facets of Adjustment Using Self- and Peer Reports'.Psychological Science. V34(9):955-967.\ndoi: 10.1177/09567976231185127"
  license <- "All content posted to ResearchBox is under a\r CC By 4.0 License(all use is allowed as long as authorship of the content is attributed). When using content from ResearchBox please\r cite the original work, and provide a link to the URL for this box (https://researchbox.org/801)."
  authors <- "Joyce He (joyce.he@anderson.ucla.edu)\nStéphane Côté (stephane.cote@rotman.utoronto.ca)"
  abstract <- "Are individuals adept at perceiving others’ emotions optimally adjusted? We extend past research by conducting a high-powered pre-registered study that comprehensively tests five theoretical models of empathic ability (i.e., emotion recognition ability) and self-views and intra- and interpersonal facets of adjustment in a sample of 1126 undergraduate students from Canada and 2205 informants. We obtained both self- and peer-reports of adjustment and controlled for cognitive abilities as a potential confounding variable. Empathic accuracy ability (but not self-views of that ability) was positively related to relationship satisfaction rated by both participants and informants. Self-views about empathic accuracy (but not actual empathic accuracy) were positively related to life satisfaction rated by both participants and informants. All associations held when controlling for cognitive abilities."

  expect_equal(info$url, url)
  expect_equal(info$RB_target, target)
  expect_equal(info$RB_license, license)
  expect_equal(info$RB_public, "June 09, 2023")
  expect_equal(info$RB_authors, authors)
  expect_equal(info$RB_abstract, abstract)

  files <- info$files[[1]]
  expect_equal(nrow(files), 9)

  #url <- "https://researchbox.org/4377&PEER_REVIEW_passcode=YHHCIU"

  ## peer review version
  rb_url <- "https://researchbox.org/1150&PEER_REVIEW_passcode=MJUAAS"
  info <- researchbox_info(rb_url)
})

test_that("researchbox_retrieve", {
  links <- researchbox_links(psychsci)
  info <- researchbox_retrieve(links)

  #expected
  public <- c("June 09, 2023", "June 09, 2023", "October 07, 2024")

  expect_equal(info$text, links$text)
  expect_equal(info$RB_public, public)
})
