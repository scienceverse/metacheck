test_that("exists", {
  expect_true(is.function(papercheck::expand_text))
  expect_no_error(helplist <- help(expand_text, papercheck))
  #expect_equal(helplist$topic, "expand_text")

  paper <- demoxml() |> read()
  expect_error(expand_text(1, paper), "The results table was not a table or object containing a table")

  expect_error(expand_text(paper$full_text, 1), "The paper argument doesn't seem to be a scivrs_paper object or a list of paper objects")
})

test_that("basic", {
  paper <- demoxml() |> read()
  res_tbl <- search_text(paper, "p =", return = "match")

  # defaults
  expanded <- expand_text(res_tbl, paper)
  expected <- search_text(paper, "p =", return = "sentence")
  expect_equal(expanded$expanded, expected$text)

  # explicit expand_to
  expanded <- expand_text(res_tbl, paper, expand_to = "sentence")
  expect_equal(expanded$expanded, expected$text)

  # paragraph
  expanded <- expand_text(res_tbl, paper, expand_to = "paragraph")
  expected <- search_text(paper, "p =", return = "paragraph")
  expect_equal(expanded$expanded, expected$text)

  # div (both examples in same div)
  expanded <- expand_text(res_tbl, paper, expand_to = "div")
  expected <- search_text(paper, "p =", return = "div")
  expect_equal(expanded$expanded[1], expected$text[1])
  expect_equal(expanded$expanded[2], expected$text[1])

  # section (both examples in same section)
  expanded <- expand_text(res_tbl, paper, expand_to = "section")
  expected <- search_text(paper, "p =", return = "section")
  expect_equal(expanded$expanded[1], expected$text[1])
  expect_equal(expanded$expanded[2], expected$text[1])
})

test_that("plus/minus", {
  paper <- demoxml() |> read()
  res_tbl <- search_text(paper, "p =", return = "match")
  expected <- paper$full_text |>
    dplyr::filter(div == 3,
                  (p == 1 & s %in% 1:3) | (p == 2 & s %in% 0:2)) |>
    dplyr::summarise(text = paste(text, collapse = " "),
                     .by = c("id", "div", "p"))
  expanded <- expand_text(res_tbl, paper, plus = 1, minus = 1)
  expect_equal(expanded$expanded, expected$text, ignore_attr = TRUE)

  expected <- paper$full_text |>
    dplyr::filter(div == 3,
                  (p == 1 & s %in% 1:2) | (p == 2 & s %in% 0:1)) |>
    dplyr::summarise(text = paste(text, collapse = " "),
                     .by = c("id", "div", "p"))
  expanded <- expand_text(res_tbl, paper, plus = 0, minus = 1)
  expect_equal(expanded$expanded, expected$text, ignore_attr = TRUE)
})

test_that("multiple papers", {
  paper <- read(demodir())
  res_tbl <- search_text(paper, "replicate",
                         section = "intro", return = "match")
  expanded <- expand_text(res_tbl, paper)
  expected <- c("However, more recently, Royzman et al. (2008) did not replicate Lieberman and colleagues' results.",
                "Purely descriptive studies where all data is reported might not require pre-registration, small pilot studies to test the feasibility of a paradigm are not designed to test a hypothesis, and some researchers might only publish studies when they have directly replicated every test in an independent sample.")
  expect_equal(expanded$expanded, expected, ignore_attr = TRUE)

  expanded <- expand_text(res_tbl, paper, plus = 1)
  expected <- c("However, more recently, Royzman et al. (2008) did not replicate Lieberman and colleagues' results. Royzman et al's non-replication potentially calls into question the reliability of previously reported links between having an other-sex sibling and moral opposition to third-party sibling incest.",
                "Purely descriptive studies where all data is reported might not require pre-registration, small pilot studies to test the feasibility of a paradigm are not designed to test a hypothesis, and some researchers might only publish studies when they have directly replicated every test in an independent sample. If this high self-reported willingness to consider pre-registration of analysis plans materializes, this can be considered a substantial change in the way psychological research is done, and there might not be a strong need to further increase the willingness to pre-register studies.")
  expect_equal(expanded$expanded, expected, ignore_attr = TRUE)
})

test_that("module output", {
  paper <- demoxml() |> read()
  module_res <- module_run(paper, "all_p_values")
  expected <- module_res$table |>
    dplyr::left_join(paper$full_text, by = c("div", "p", "s")) |>
    dplyr::pull(text.y)
  expanded <- expand_text(module_res, paper)
  expect_equal(expanded$expanded, expected, ignore_attr = TRUE)
})


test_that("issue 47", {
  # some expand text had duplicated sentences
  paper <- psychsci$`0956797614522816`
  all_p <- module_run(paper, "all_p_values")

  # Keep only nonsignificant p  value statements
  results_table <- all_p$table |>
    dplyr::filter(section == "results", div == 12, p == 3, s == 5)
  expand_to <- "sentence"
  plus <- 1
  minus <- 1

  # Also add sentence before and after
  res <- expand_text(results_table, paper, expand_to, plus, minus)

  # get location info for problem duplication
  obs <- res$expanded[[1]]
  obs_s <- res$s[[1]]
  obs_p <- res$p[[1]]
  obs_div <- res$div[[1]]

  # get sentences plus and minus
  text <- paper$full_text |>
    dplyr::filter(s %in% (obs_s-minus):(obs_s+plus), p == obs_p, div == obs_div)
  exp <- paste(text$text, collapse = " ")

  expect_equal(obs, exp)
})
