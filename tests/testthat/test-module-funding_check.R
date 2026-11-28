test_that("funding_check with two funding title lines (U103)", {
  # the if() on a vector of title lines stopped the module for the whole list
  llm_use(FALSE)
  txt <- c("Funding", "This work was supported by NWO grant 123.", "Funding:", "None.")
  mo <- suppressMessages(module_run(test_paper(text = txt), "funding_check"))
  expect_match(mo$table$text, "supported by NWO grant 123", fixed = TRUE)
  expect_false(grepl("NA", mo$table$text, fixed = TRUE))
})

test_that("funding_check synonyms match, and the reference list starts at a sentence that begins with its title (U104)", {
  # "Ffoundation", "L(?i)terature" and "B(?i)ibliograpy" never matched, and
  # any sentence ending in "References <Capital>" was taken as the start
  env <- new.env(parent = asNamespace("metacheck"))
  sys.source(metacheck:::module_find("funding_check"), envir = env)
  s <- env$.create_synonyms()
  expect_true(any(grepl(paste(s$foundation, collapse = "|"), "Gates Foundation", perl = TRUE)))
  expect_true(any(grepl(paste(s$References, collapse = "|"), "Bibliography", perl = TRUE)))
  art <- c("Intro.", "References", "Smith 2020.", "Appendix: see the References Section.")
  expect_equal(env$.where_refs_txt(art), 2L)
})

test_that("funding_check_oi favours acknowledgment sections (U105)", {
  # the favoured types had only the British spelling "acknowledgement"
  p <- test_paper(c("This study was funded by the NSF grant 123.",
                    "This research was supported by NIH grant 456."))
  p$text$section_id <- c(0, 1)
  p$section <- dplyr::bind_rows(p$section, p$section)
  p$section$section_id <- c(0, 1)
  p$section$section_type <- c("intro", "acknowledgment")
  mo <- module_run(p, "funding_check_oi")
  expect_equal(mo$table$text, "This research was supported by NIH grant 456.")
})

test_that("funding_check: the funders a publisher's XML declares", {
  # declared funders count as a funding statement, with their registry DOI
  # and award numbers
  p <- read(test_path("fixtures", "formats", "wiley_cases.xml"))
  mo <- module_run(p, "funding_check")
  expect_equal(mo$traffic_light, "green")
  expect_equal(mo$funders$funder, "Test Research Council")
  expect_equal(mo$funders$funder_doi, "10.13039/501100000270")
  expect_equal(mo$funders$award_ids[[1]], c("TRC-1", "TRC-2"))
  expect_equal(mo$summary_table$funder_n, 1L)
  expect_equal(mo$summary_table$award_n, 2L)
  expect_true(mo$summary_table$funding_found)

  # declared funders and no sentence about funding: still found
  p$text <- p$text[!grepl("Council|TRC", p$text$text), ]
  mo <- module_run(p, "funding_check")
  expect_equal(mo$traffic_light, "green")
  expect_match(mo$summary_text, "metadata names its funders")

  # a paper read from a PDF has no funding table
  g <- read(demofile("xml"))
  mo <- module_run(g, "funding_check")
  expect_equal(nrow(mo$funders), 0)
  expect_equal(mo$summary_table$funder_n, 0L)
})
