test_that("error", {
  filename <- demoxml()
  s <- read_grobid(filename)

  expect_true(is.function(search_text))

  expect_error(suppressWarnings(search_text(s, "(bad pattern")),
               "Check the pattern argument")

  expect_warning(search_text(s, "test", fixed = TRUE),
               "argument 'ignore.case = TRUE' will be ignored")
})

test_that("default", {
  s <- read_grobid(demoxml())

  sig <- search_text(s, "significant")

  expect_true(all(grepl("significant", sig$text)))
  expect_equal(nrow(sig), 3)

  # section
  res <- search_text(s, "significant", "results")
  expect_equal(nrow(res), 2)
  expect_true(all(res$section == "results"))

  # multiple matches in a sentence
  equal <- search_text(s, "[a-zA-Z][a-zA-Z\\(\\)]*\\s*=\\s*[\\.0-9-]*\\d",
                       section = "results",
                       return = "match")
  expect_equal(nrow(equal), 6)
  expect_equal(equal$text[[1]], c("M = 9.12"))
})

test_that("table as first argument", {
  s <- read_grobid(demoxml())

  sig <- search_text(s, "significant")
  sig2 <- search_text(sig, "significant")
  expect_equal(sig, sig2)

  s3 <- search_text(sig, "[a-zA-Z]+\\s*=\\s*[\\.0-9-]*\\d", return = "match")
  expect_equal(nrow(s3), 3)
})

test_that("return", {
  s <- data.frame(
    text = c("Introduction", "Method",
             "Participants", paste("Part", 1:3),
             "Measures", paste("Measures", 1:4)),
    section = rep(c("abstract", "method"), c(1, 10)),
    header = rep(c("", "Method", "Participants", "Measures"), c(1, 1, 4, 5)),
    div = c(1, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4),
    p = c(1, 2, 3, 4, 4, 4, 5, 6, 6, 6, 6),
    s = c(1, 1, 1, 1:3, 1, 1:4),
    id = "id.xml"
  )

  res_s1 <- search_text(s, section = "method")
  res_s2 <- search_text(s, section = "method", return = "sentence")
  res_p <- search_text(s, section = "method", return = "paragraph")
  res_div <- search_text(s, section = "method", return = "div")
  res_sec <- search_text(s, section = "method", return = "section")
  res_m <- search_text(s, "Part [0-9]", section = "method", return = "match")

  expect_equal(res_s1$text, res_s2$text)
  expect_equal(res_s1$text, s$text[2:11])

  expect_equal(res_p$p, 2:6)
  expect_equal(res_p$div, c(2, 3, 3, 4, 4))
  expect_equal(res_p$text[3], paste("Part", 1:3, collapse = " "))
  expect_equal(res_p$s, c(NA, NA, NA, NA, NA))

  expect_equal(res_div$div, 2:4)
  expect_equal(res_div$text[2], "Participants\n\nPart 1 Part 2 Part 3")
  expect_equal(res_div$p, c(NA, NA, NA))
  expect_equal(res_div$s, c(NA, NA, NA))

  expect_equal(res_sec$section, "method")
  expect_equal(res_sec$div, NA)
  expect_equal(res_sec$p, NA)
  expect_equal(res_sec$s, NA)
  expect_equal(res_sec$header, NA)

  expect_equal(res_m$text, paste("Part", 1:3))
})

test_that("iteration", {
  s <- read_grobid(demodir())

  # search full text
  sig <- search_text(s, "significant")
  expect_equal(nrow(sig), 13)

  equal <- search_text(s, "=", section = "results")
  classes <- as.character(unique(equal$section))
  expect_equal(classes, "results")
})


