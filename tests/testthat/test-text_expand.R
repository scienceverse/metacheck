test_that("exists", {
  expect_true(is.function(metacheck::text_expand))
  expect_no_error(helplist <- help(text_expand, metacheck))
  #expect_equal(helplist$topic, "text_expand")

  paper <- demopaper()
  expect_error(text_expand(1, paper), "The results table was not a table or object containing a table")

  expect_error(text_expand(paper$text, 1), "The paper argument doesn't seem to be a scivrs_paper object or a list of paper objects")
})

test_that("search_text alias", {
  expect_true(is.function(metacheck::expand_text))
  expect_no_error(helplist <- help(expand_text, metacheck))

  paper <- demopaper()
  s <- search_text(paper, "significant")
  et <- expand_text(s, paper, plus = 1)
  te <- text_expand(s, paper, plus = 1)

  expect_equal(et, te)
})

test_that("basic", {
  # set up example paper
  text <- c("The cat is nice.",
            "Ferrets are better than cats.",
            "I have a dog.")
  paper <- test_paper(text)
  paper$text$paragraph_id <- c(0, 1, 1)
  res_tbl <- text_search(paper, "cat", return = "match")

  # defaults
  expanded <- text_expand(res_tbl, paper)
  expected <- text_search(paper, "cat", return = "sentence")
  expect_equal(expanded$expanded, expected$text)

  # explicit expand_to
  expanded <- text_expand(res_tbl, paper, expand_to = "sentence")
  expect_equal(expanded$expanded, expected$text)

  # paragraph  (both examples in same paragraph)
  expanded <- text_expand(res_tbl, paper, expand_to = "paragraph")
  expected <- text_search(paper, "cat", return = "paragraph")
  expect_equal(expanded$expanded, expected$text)

  # section (both examples in same section)
  expanded <- text_expand(res_tbl, paper, expand_to = "section")
  expected <- text_search(paper, "cat", return = "section")
  expect_equal(expanded$expanded[1], expected$text[1])
  expect_equal(expanded$expanded[2], expected$text[1])
})

test_that("plus/minus", {
  paper <- test_paper(LETTERS)
  res_tbl <- text_search(paper, "D", return = "match")
  expanded <- text_expand(res_tbl, paper, plus = 1, minus = 1)
  exp <- "C D E"
  expect_equal(expanded$expanded, exp)

  expanded <- text_expand(res_tbl, paper, plus = 0, minus = 1)
  exp <- "C D"
  expect_equal(expanded$expanded, exp)

  # minus beyond scope
  expanded <- text_expand(res_tbl, paper, plus = 2, minus = 5)
  exp <- "A B C D E F"
  expect_equal(expanded$expanded, exp)
})

test_that("multiple papers", {
  paper <- paperlist(
    test_paper(LETTERS),
    test_paper(letters)
  )
  res_tbl <- text_search(paper, "D")
  expanded <- text_expand(res_tbl, paper, plus = 1)
  expected <- c("D E", "d e")
  expect_equal(expanded$expanded, expected)

  expanded <- text_expand(res_tbl, paper, expand_to = "paragraph")
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
#   expanded <- text_expand(module_res, paper)
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
#   res <- text_expand(results_table, paper, expand_to, plus, minus)
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
#   # if the location info isn't present, text_expand returns an NA
#   paper <- psychsci[[1]]
#   results_table <- text_search(paper, "significant")
#   results_table$div[[1]] <- NA
#   obs <- text_expand(results_table, paper, expand_to = "paragraph")
#   expect_equal(obs$text[[1]], obs$expanded[[1]])
#
#   # underlying problem in xref creation
#   filename <- test_path("fixtures", "problem_xml", "paper_361.xml")
#   paper <- read(filename)
#   text <- text_search(paper, "\\(Lakens, 2022\\;")
#   b32 <- paper$xrefs |> dplyr::filter(xref_id == "b32")
#
#   # had small differences due to removing full stops after initials
#   expect_equal(b32$text, text$text)
# })
