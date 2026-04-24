test_that("search_text", {
  expect_true(is.function(metacheck::search_text))
  expect_no_error(helplist <- help(search_text, metacheck))

  expect_error(search_text(1))

  paper <- demopaper()

  expect_error(suppressWarnings(search_text(paper, "(bad pattern")),
               "Check the pattern argument")

  expect_warning(search_text(paper, "test", fixed = TRUE),
               "argument 'ignore.case = TRUE' will be ignored")

  paper <- paper("no text")
  expect_no_warning(x <- search_text(paper))
  expect_equal(x$text, character(0))
})

test_that("default", {
  paper <- demopaper()

  sig <- search_text(paper, "significant")

  expect_true(all(grepl("significant", sig$text)))
  expect_equal(nrow(sig), 3)

  # multiple matches in a sentence
  equal <- search_text(paper, "[a-zA-Z][a-zA-Z\\(\\)]*\\s*=\\s*[\\.0-9-]*\\d",
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

  s3 <- search_text(sig, "significant", return = "paper_id")
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

  res_s1 <- search_text(s)
  res_s2 <- search_text(s, return = "sentence")
  res_p <- search_text(s, return = "paragraph")
  res_div <- search_text(s, return = "header")
  res_m <- search_text(s, "Part [0-9]", return = "match")
  res_id <- search_text(s, return = "paper_id")

  expect_equal(res_s1$text, res_s2$text)
  expect_equal(res_s1$text, s$text)

  expect_equal(res_p$paragraph_id, 1:6)
  expect_equal(res_p$header, c("", "Method", "Participants", "Participants", "Measures", "Measures"))
  expect_equal(res_p$text[4], paste("Part", 1:3, collapse = " "))
  expect_true(all(is.na(res_p$text_id)))

  expect_equal(res_div$header, c("", "Method", "Participants", "Measures"))
  expect_equal(res_div$text[3], "Participants\n\nPart 1 Part 2 Part 3")
  expect_true(all(is.na(res_div$paragraph_id)))
  expect_true(all(is.na(res_div$text_id)))

  expect_equal(res_m$text, paste("Part", 1:3))

  expect_equal(res_id$text, "Introduction\n\nMethod\n\nParticipants\n\nPart 1 Part 2 Part 3\n\nMeasures\n\nMeasures 1 Measures 2 Measures 3 Measures 4")
})

test_that("iteration", {
  paper <- list(
    test_paper(LETTERS[1:5]),
    test_paper(LETTERS[3:7]),
    test_paper(LETTERS[4:5])
  ) |> paperlist()

  # search full text
  C <- search_text(paper, "C")
  expect_equal(nrow(C), 2)
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
  expect_contains(names(x), "text")

  # missing cols
  paper <- data.frame(
    text = LETTERS
  )
  pattern <- "B"
  x <- search_text(paper, pattern)
  expect_equal(x$text, "B")
  expect_contains(names(x), "text_id")

  # no text
  paper <- data.frame(text = character(0))
  x <- search_text(paper)
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

test_that("vectors and non-table DFs", {
  paper <- LETTERS
  x <- search_text(paper, "[A-C]")
  expect_equal(x, LETTERS[1:3])

  paper <- data.frame(x = LETTERS)
  x <- search_text(paper, "[A-C]")
  expect_equal(x$x, LETTERS[1:3])
  expect_contains(names(x), "text_id")

  paper <- data.frame(x = LETTERS, y = rev(letters))
  x <- search_text(paper, "[A-C]")
  expect_equal(x$x, LETTERS[1:3])
  expect_equal(x$y, rev(letters)[1:3])
})

# test_that("search_header", {
#   text <- c("Apple and Banana",
#             "Just an apple.",
#             "Bananas only here.",
#             "Mango smoothie.")
#   paper <- test_paper(text)
#   paper$text$section_id <- 1:4
#   paper$sections <- data.frame(
#     section_id = 1:4,
#     header <- c("Fruit", "Fruit", "Fruit", "No Apples here"),
#     parent_section_id = 0,
#     section_type = "unknown",
#     classification_score = 0
#   )
#
#   pattern <- c("apple")
#   x <- search_text(paper, pattern, search_header = TRUE)
#   expect_equal(x$text, text[c(1, 2, 4)])
#
#   pattern <- c("here")
#   x <- search_text(paper, pattern, search_header = TRUE)
#   expect_equal(x$text, text[3:4])
#
#   pattern <- c("No")
#   x <- search_text(paper, pattern, search_header = TRUE)
#   expect_equal(x$text, text[4])
#
#   pattern <- c("No")
#   x <- search_text(paper, pattern, exclude = TRUE, search_header = TRUE)
#   expect_equal(x$text, text[1:3])
# })

