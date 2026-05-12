test_that("exists", {
  expect_true(is.function(metacheck::expand_text))
  expect_no_error(helplist <- help(expand_text, metacheck))
  #expect_equal(helplist$topic, "expand_text")

  paper <- demopaper()
  expect_error(expand_text(1, paper), "The results table was not a table or object containing a table")

  expect_error(expand_text(paper$text, 1), "The paper argument doesn't seem to be a scivrs_paper object or a list of paper objects")
})

test_that("basic", {
  # set up example paper
  text <- c("The cat is nice.",
            "Ferrets are better than cats.",
            "I have a dog.")
  paper <- test_paper(text)
  paper$text$paragraph_id <- c(0, 1, 1)
  res_tbl <- search_text(paper, "cat", return = "match")

  # defaults
  expanded <- expand_text(res_tbl, paper)
  expected <- search_text(paper, "cat", return = "sentence")
  expect_equal(expanded$expanded, expected$text)

  # explicit expand_to
  expanded <- expand_text(res_tbl, paper, expand_to = "sentence")
  expect_equal(expanded$expanded, expected$text)

  # paragraph  (both examples in same paragraph)
  expanded <- expand_text(res_tbl, paper, expand_to = "paragraph")
  expected <- search_text(paper, "cat", return = "paragraph")
  expect_equal(expanded$expanded, expected$text)

  # section (both examples in same section)
  expanded <- expand_text(res_tbl, paper, expand_to = "section")
  expected <- search_text(paper, "cat", return = "section")
  expect_equal(expanded$expanded[1], expected$text[1])
  expect_equal(expanded$expanded[2], expected$text[1])
})

test_that("plus/minus", {
  paper <- test_paper(LETTERS)
  res_tbl <- search_text(paper, "D", return = "match")
  expanded <- expand_text(res_tbl, paper, plus = 1, minus = 1)
  exp <- "C D E"
  expect_equal(expanded$expanded, exp)

  expanded <- expand_text(res_tbl, paper, plus = 0, minus = 1)
  exp <- "C D"
  expect_equal(expanded$expanded, exp)

  # minus beyond scope
  expanded <- expand_text(res_tbl, paper, plus = 2, minus = 5)
  exp <- "A B C D E F"
  expect_equal(expanded$expanded, exp)
})

test_that("multiple papers", {
  paper <- paperlist(
    test_paper(LETTERS),
    test_paper(letters)
  )
  res_tbl <- search_text(paper, "D")
  expanded <- expand_text(res_tbl, paper, plus = 1)
  expected <- c("D E", "d e")
  expect_equal(expanded$expanded, expected)

  expanded <- expand_text(res_tbl, paper, expand_to = "paragraph")
  expected <- c(
    paste(LETTERS, collapse = " "),
    paste(letters, collapse = " ")
  )
  expect_equal(expanded$expanded, expected)
})

# test_that("module output", {
#   paper <- demopaper()
#
#   module_res <- module_run(paper, "all_p_values")
#   expected <- module_res$table |>
#     dplyr::left_join(paper$text, by = c("div", "p", "s")) |>
#     dplyr::pull(text.y)
#   expanded <- expand_text(module_res, paper)
#   expect_equal(expanded$expanded, expected, ignore_attr = TRUE)
# })
#
#
# test_that("issue 47", {
#   # some expand text had duplicated sentences
#   paper <- psychsci$`0956797614522816`
#   all_p <- module_run(paper, "all_p_values")
#
#   # Keep only nonsignificant p  value statements
#   results_table <- all_p$table |>
#     dplyr::filter(section == "results", div == 12, p == 3, s == 5)
#   expand_to <- "sentence"
#   plus <- 1
#   minus <- 1
#
#   # Also add sentence before and after
#   res <- expand_text(results_table, paper, expand_to, plus, minus)
#
#   # get location info for problem duplication
#   obs <- res$expanded[[1]]
#   obs_s <- res$s[[1]]
#   obs_p <- res$p[[1]]
#   obs_div <- res$div[[1]]
#
#   # get sentences plus and minus
#   text <- paper$text |>
#     dplyr::filter(s %in% (obs_s-minus):(obs_s+plus), p == obs_p, div == obs_div)
#   exp <- paste(text$text, collapse = " ")
#
#   expect_equal(obs, exp)
# })
#
#
# test_that("issue 72", {
#   # if the location info isn't present, expand_text returns an NA
#   paper <- psychsci[[1]]
#   results_table <- search_text(paper, "significant")
#   results_table$div[[1]] <- NA
#   obs <- expand_text(results_table, paper, expand_to = "paragraph")
#   expect_equal(obs$text[[1]], obs$expanded[[1]])
#
#   # underlying problem in xref creation
#   filename <- test_path("fixtures", "problem_xml", "paper_361.xml")
#   paper <- read(filename)
#   text <- search_text(paper, "\\(Lakens, 2022\\;")
#   b32 <- paper$xrefs |> dplyr::filter(xref_id == "b32")
#
#   # had small differences due to removing full stops after initials
#   expect_equal(b32$text, text$text)
# })
