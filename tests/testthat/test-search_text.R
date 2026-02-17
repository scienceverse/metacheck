test_that("search_text", {
  expect_true(is.function(metacheck::search_text))
  expect_no_error(helplist <- help(search_text, metacheck))

  expect_error(search_text(1))

  paper <- demopaper()

  expect_error(suppressWarnings(search_text(paper, "(bad pattern")),
               "Check the pattern argument")

  expect_warning(search_text(paper, "test", fixed = TRUE),
               "argument 'ignore.case = TRUE' will be ignored")
})

test_that("default", {
  paper <- demopaper()

  sig <- search_text(paper, "significant")

  expect_true(all(grepl("significant", sig$text)))
  expect_equal(nrow(sig), 3)

  # section
  res <- search_text(paper, "significant", "results")
  expect_equal(nrow(res), 2)
  expect_true(all(res$section_type == "results"))

  # multiple matches in a sentence
  equal <- search_text(paper, "[a-zA-Z][a-zA-Z\\(\\)]*\\s*=\\s*[\\.0-9-]*\\d",
                       section = "results",
                       return = "match")
  any_dupes <- duplicated(equal$text_id) |> any()
  expect_true(any_dupes)
})

test_that("test papers (no section table)", {
  paper <- paperlist(
    test_paper(LETTERS),
    test_paper(letters)
  )
  pattern <- "D"
  txt <- search_text(paper, pattern)
  expect_equal(txt$text, c("D", "d"))
})

test_that("table as first argument", {
  paper <- demopaper()

  sig <- search_text(paper, "significant")
  sig2 <- search_text(sig, "significant")
  expect_equal(sig, sig2)

  s3 <- search_text(sig, "significant", return = "id")
  expect_equal(nrow(s3), 1)
})

test_that("return", {
  s <- data.frame(
    text_id = 0:10,
    paragraph_id = c(1, 2, 3, 4, 4, 4, 5, 6, 6, 6, 6),
    section_id = c(1, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4),
    text = c("Introduction", "Method",
             "Participants", paste("Part", 1:3),
             "Measures", paste("Measures", 1:4)),
    id = "id.xml",
    section_type = rep(c("abstract", "method"), c(1, 10)),
    header = rep(c("", "Method", "Participants", "Measures"), c(1, 1, 4, 5))
  )

  res_s1 <- search_text(s, section = "method")
  res_s2 <- search_text(s, section = "method", return = "sentence")
  res_p <- search_text(s, section = "method", return = "paragraph")
  res_div <- search_text(s, section = "method", return = "header")
  # res_sec <- search_text(s, section = "method", return = "section")
  res_m <- search_text(s, "Part [0-9]", section = "method", return = "match")
  res_id <- search_text(s, return = "id")

  expect_equal(res_s1$text, res_s2$text)
  expect_equal(res_s1$text, s$text[2:11])

  expect_equal(res_p$paragraph_id, 2:6)
  expect_equal(res_p$header, c("Method", "Participants", "Participants", "Measures", "Measures"))
  expect_equal(res_p$text[3], paste("Part", 1:3, collapse = " "))
  expect_equal(res_p$text_id, c(NA, NA, NA, NA, NA))

  expect_equal(res_div$header, c("Method", "Participants", "Measures"))
  expect_equal(res_div$text[2], "Participants\n\nPart 1 Part 2 Part 3")
  expect_equal(res_div$paragraph_id, c(NA, NA, NA))
  expect_equal(res_div$text_id, c(NA, NA, NA))

  # expect_equal(res_sec$section_type, "method")
  # expect_equal(res_sec$div, NA)
  # expect_equal(res_sec$p, NA)
  # expect_equal(res_sec$s, NA)
  # expect_equal(res_sec$header, NA)

  expect_equal(res_m$text, paste("Part", 1:3))

  expect_equal(res_id$text, "Introduction\n\nMethod\n\nParticipants\n\nPart 1 Part 2 Part 3\n\nMeasures\n\nMeasures 1 Measures 2 Measures 3 Measures 4")
  expect_equal(NA, res_id$section_type)
  expect_equal(NA, res_id$section_id)
  expect_equal(NA, res_id$header)
  expect_equal(NA, res_id$paragraph_id)
  expect_equal(NA, res_id$text_id)
})

test_that("iteration", {
  paper <- psychsci[1:3]

  # search full text
  sig <- search_text(paper, "significant")
  expect_equal(nrow(sig), 18)

  equal <- search_text(paper, "=", section = "results")
  classes <- as.character(unique(equal$section_type))
  expect_equal(classes, "results")
})

test_that("odd errors", {
  # multiple identical matches in the same sentence
  paper <- test_paper("This and this and this and this.")
  pattern <- "this"
  x <- search_text(paper, pattern, return = "match")
  expect_equal(nrow(x), 4)

  # undefined columns selected
  # handle no returns in match
  paper <- test_paper("The word is not here.")
  pattern <- "significant"
  x <- search_text(paper, pattern, perl = TRUE, return = "match")
  expect_equal(nrow(x), 0)
  exp <- c("text_id", "section_id", "paragraph_id",
           "text", "id", "header", "section_type")
  expect_contains(names(x), exp)
})

test_that("exclude", {
  text <- c("Apple and Banana",
            "Just an apple.",
            "Bananas only here.",
            "Mango smoothie.")
  paper <- test_paper(text)

  pattern <- c("mango")
  x <- search_text(paper, pattern, exclude = TRUE)
  expect_equal(x$text, text[1:3])

  pattern <- c("apple")
  x <- search_text(paper, pattern, exclude = TRUE)
  expect_equal(x$text, text[3:4])
})

test_that("multiple patterns", {
  text <- c("Apple and Banana",
    "Just an apple.",
    "Bananas only here.",
    "Mango smoothie.")
  paper <- test_paper(text)

  pattern <- c("apple", "banana")
  x <- search_text(paper, pattern)
  expect_equal(x$text, text[1:3])

  pattern <- c("apple", "montana")
  x <- search_text(paper, pattern)
  expect_equal(x$text, text[1:2])

  pattern <- c("banana")
  x <- search_text(paper, pattern)
  expect_equal(x$text, text[c(1, 3)])

  pattern <- c("apple", "banana")
  x <- search_text(paper, pattern, exclude = TRUE)
  expect_equal(x$text, text[4])
})

test_that("search_header", {
  text <- c("Apple and Banana",
            "Just an apple.",
            "Bananas only here.",
            "Mango smoothie.")
  paper <- test_paper(text)
  paper$text$header <- c("Fruit", "Fruit", "Fruit", "No Apples here")

  pattern <- c("apple")
  x <- search_text(paper, pattern, search_header = TRUE)
  expect_equal(x$text, text[c(1, 2, 4)])

  pattern <- c("here")
  x <- search_text(paper, pattern, search_header = TRUE)
  expect_equal(x$text, text[3:4])

  pattern <- c("No")
  x <- search_text(paper, pattern, search_header = TRUE)
  expect_equal(x$text, text[4])

  pattern <- c("No")
  x <- search_text(paper, pattern, exclude = TRUE, search_header = TRUE)
  expect_equal(x$text, text[1:3])
})

