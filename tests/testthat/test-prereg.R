test_that("exists", {
  expect_true(is.function(papercheck::aspredicted_links))
  expect_no_error(helplist <- help(aspredicted_links, papercheck))
})

test_that("errors", {
  expect_error(aspredicted_links(bad_arg))
})

test_that("aspredicted_links", {
  links <- aspredicted_links(psychsci)
  expect_equal(names(links)[[1]], "text")
  expect_true(all(grepl("^https://aspredicted\\.org", links$text)))
  expect_equal(nrow(links), 78)
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

test_that("aspredicted_info", {
  skip_on_covr()
  skip_if_offline("aspredicted.org")

  proj <- "https://aspredicted.org/Y2F_6B7"
  pdf <- "https://aspredicted.org/ve2qn.pdf"
  blind <- "https://aspredicted.org/blind.php?x=nq4xa3"

  # proj
  info <- aspredicted_info(proj)
  title <- "Children's prosocial behavior in response to awe-inspiring art"
  authors <- "This pre-registration is currently anonymous to enable blind peer-review.\nIt has 5 authors."

  expect_equal(info$ap_url, proj)
  expect_equal(info$AP_title, title)
  expect_equal(info$AP_authors, authors)

  # pdf
  info <- aspredicted_info(pdf)
  title <- "How infants encode unexpected events: a SSVEP study"
  authors <- paste(sep = "\n",
    "Moritz Köster (Freie Universität Berlin) - moritz.koester@ur.de",
    "Miriam Langeloh (Max Planck Institute for Human Cognitive and) - langeloh@cbs.mpg.de",
    "Stephanie Höhl (Max Planck Institute for Human Cognitive and) - stefanie.hoehl@univie.ac.at"
  )

  expect_equal(info$ap_url, pdf)
  expect_equal(info$AP_title, title)
  expect_equal(info$AP_authors, authors)

  # blind
  info <- aspredicted_info(blind)
  title <- "Depre_ctrl_elicit [3x2] [N800 MT]"
  authors <- "This pre-registration is currently anonymous to enable blind peer-review.\nIt has one author."
  hypo <- "Participants will imagine a depressed person as more likely to be overweight, less likely to take care of themselves, less likely to be successful at work, with a less attractive face, and more introverted compared to an actor that is explicitly described as not depressed and to a control condition."
  key_dv <- "Seven questions (all anchored at 1 = not at all and 7 = very much, with the exception of the introversion question which will be anchored at 1 = very introverted and 7 = very extroverted):\n\r Do you think this person is overweight?\n\r Do you think this person takes care of themselves?\n\r Do you think this person is successful at work?\n\r Do you think this person has an attractive face?\n\r Do you think this person is clinically depressed? (manipulation check)\n\r Do you think this person is introverted or extroverted?"
  cond <- "Participants will be randomly assigned to one of six conditions, between-subjects, as two factors will be manipualated (target gender: male vs. female, and status: depressed vs. not depressed vs. control). first participants will be assigned to either the male or the female condition:\n\r Male condition:\n\r Jonathan Smith is a bank clerk at LMS, a bank in New Orleans, Louisiana. He is originally from Houston, Texas. Jonathan is 38, has a Associate Degree in Management, and is Caucasian.\n\r Female condition:\n\r Jessica Smith is a bank clerk at LMS, a bank in New Orleans, Louisiana. She is originally from Houston, Texas. Jessica is 38, has a Associate Degree in Management, and is Caucasian.\n\r Then, participants will be assigned to the depression condition, the nondepression condition, or the control condition. Participants in the control condition will proceed to the questions without reading any additional text.\n\r Participants in the depression condition will read this:\n\r This person is clinically depressed. They often have insomnia and are in a state of a really bad mood.\n\r Participants in the nondepression condition will read this:\n\r This person is not depressed. They have their up and downs like everyone, but are overall quite OK psychologically speaking."

  outliers <- 'Participants who respond "Yes" to the question "Have you ever been on the planet Jupiter?" will be excluded from analyses.'

  expect_equal(info$ap_url, blind)
  expect_equal(info$AP_title, title)
  expect_equal(info$AP_authors, authors)
  expect_equal(info$AP_created, "2021/06/14 - 02:17 AM (PT)")
  expect_equal(info$AP_data, "No, no data have been collected for this study yet.")
  expect_equal(info$AP_hypotheses, hypo)
  expect_equal(info$AP_key_dv, key_dv)
  expect_equal(info$AP_conditions, cond)
  expect_equal(info$AP_analyses, "We will conduct two-way ANOVAs with gender and condition as IVs and all the seven DVs.")
  expect_equal(info$AP_outliers, outliers)
  expect_equal(info$AP_sample_size, "800 Mturkers.")
  expect_equal(info$AP_anything_else, "")
  expect_equal(info$AP_version, "2.00")
})
