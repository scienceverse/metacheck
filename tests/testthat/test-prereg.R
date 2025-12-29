test_that("aspredicted_links", {
  expect_true(is.function(metacheck::aspredicted_links))
  expect_no_error(helplist <- help(aspredicted_links, metacheck))

  expect_error(aspredicted_links(bad_arg))

  links <- aspredicted_links(psychsci)
  expect_equal(names(links)[[1]], "text")
  expect_true(all(grepl("^https://aspredicted\\.org", links$text)))
  expect_equal(nrow(links), 74)
  expect_equal(links, unique(links))

  sentences <- expand_text(links, psychsci)

  paper <- data.frame(id = 1,
                      s = 1:10,
                      p = 1,
                      text = c("</aspredicted.org/stuff>", "hi",
                              "<https://aspredicted.org/stuff>", "hi",
                              "<https://aspredicted.org/ stuff>", "hi",
                              "<https://aspredicted.org/blind.php?", " x=stuff> hi",
                              "<https://aspredicted> .org/stuff.pdf", "hi"
                              ))
  links <- aspredicted_links(paper)
  exp <- c("https://aspredicted.org/stuff",
           "https://aspredicted.org/stuff",
           "https://aspredicted.org/stuff",
           "https://aspredicted.org/blind.php?x=stuff",
           "https://aspredicted.org/stuff.pdf")
  expect_equal(links$text, exp)

  # second trailing blind links
  paper <- psychsci$`09567976231204035`
  links <- aspredicted_links(paper)
  expect_true(all(links$text != "https://aspredicted.org/blind.php?"))

  # wierd aspredicted> .org
  paper <- psychsci$`0956797620948821`
  links <- aspredicted_links(paper)
  expect_true(any(grepl("/vp4rg", links$text)))
  expect_true(any(grepl("/3kq9y", links$text)))
})

# httptest::start_capturing()
httptest::use_mock_api()

test_that("aspredicted_retrieve blind", {
  # single blind link
  ap_url <- "https://aspredicted.org/blind.php?x=nq4xa3"
  suppressMessages( info <- aspredicted_retrieve(ap_url) )
  expect_equal(info$ap_url, ap_url)
  exp_auth <- "This pre-registration is currently anonymous to enable blind peer-review.\nIt has one author."
  expect_equal(info$AP_authors, exp_auth)
})

test_that("aspredicted_retrieve pdf", {
  # single pdf
  ap_url <- "https://aspredicted.org/ve2qn.pdf"
  suppressMessages( info <- aspredicted_retrieve(ap_url) )
  expect_equal(info$ap_url, ap_url)
  exp <- "How infants encode unexpected events: a SSVEP study"
  expect_equal(info$AP_title, exp)
})

test_that("aspredicted_retrieve multiple", {
  # multiple links in a table
  ap_url <- data.frame(link = c(
    "https://aspredicted.org/ve2qn.pdf",
    "https://aspredicted.org/blind.php?x=nq4xa3"
  ))
  suppressMessages( {
    time1 <- system.time( info <- aspredicted_retrieve(ap_url))
    time3 <- system.time( info <- aspredicted_retrieve(ap_url, wait = 3))
  })
  expect_true(time1[["elapsed"]] > 2)
  expect_true(time3[["elapsed"]] > 6)
})

test_that("aspredicted_info proj", {
  ap_url <- "https://aspredicted.org/Y2F_6B7"
  suppressMessages( info <- aspredicted_info(ap_url) )
  title <- "Children's prosocial behavior in response to awe-inspiring art"
  authors <- "This pre-registration is currently anonymous to enable blind peer-review.\nIt has 5 authors."

  expect_equal(info$ap_url, ap_url)
  expect_equal(info$AP_title, title)
  expect_equal(info$AP_authors, authors)
})


test_that("aspredicted_info pdf", {
  ap_url <- "https://aspredicted.org/ve2qn.pdf"
  suppressMessages( info <- aspredicted_info(ap_url) )
  title <- "How infants encode unexpected events: a SSVEP study"
  authors <- paste(sep = "\n",
    "Moritz Köster (Freie Universität Berlin) - moritz.koester@ur.de",
    "Miriam Langeloh (Max Planck Institute for Human Cognitive and) - langeloh@cbs.mpg.de",
    "Stephanie Höhl (Max Planck Institute for Human Cognitive and) - stefanie.hoehl@univie.ac.at"
  )

  expect_equal(info$ap_url, ap_url)
  expect_equal(info$AP_title, title)
  expect_equal(info$AP_authors, authors)
})

test_that("aspredicted_info blind", {
  ap_url <- "https://aspredicted.org/blind.php?x=nq4xa3"
  suppressMessages( info <- aspredicted_info(ap_url) )
  title <- "Depre_ctrl_elicit [3x2] [N800 MT]"
  authors <- "This pre-registration is currently anonymous to enable blind peer-review.\nIt has one author."
  hypo <- "Participants will imagine a depressed person as more likely to be overweight, less likely to take care of themselves, less likely to be successful at work, with a less attractive face, and more introverted compared to an actor that is explicitly described as not depressed and to a control condition."
  key_dv <- "^Seven questions .* extroverted\\?$"
  cond <- "^Participants will be .* quite OK psychologically speaking\\.$"
  outliers <- "Participants who respond .* will be excluded from analyses\\.$"

  expect_equal(info$ap_url, ap_url)
  expect_equal(info$AP_title, title)
  expect_equal(info$AP_authors, authors)
  expect_equal(info$AP_created, "2021/06/14 02:17 (PT)")
  expect_equal(info$AP_data, "No, no data have been collected for this study yet.")
  expect_equal(info$AP_hypotheses, hypo)
  expect_true(grepl(key_dv, info$AP_key_dv))
  expect_true(grepl(cond, info$AP_conditions))
  expect_equal(info$AP_analyses, "We will conduct two-way ANOVAs with gender and condition as IVs and all the seven DVs.")
  expect_true(grepl(outliers, info$AP_outliers))
  expect_equal(info$AP_sample_size, "800 Mturkers.")
  expect_equal(info$AP_anything_else, "")
  expect_equal(info$AP_version, "2.00")
})

httptest::stop_mocking()
# httptest::stop_capturing()
