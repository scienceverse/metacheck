test_that("icpsr_links finds openICPSR and ICPSR deposits in their forms (#660)", {
  p <- test_paper(text = c(
    "Data and code are available at https://doi.org/10.3886/E195681V1.",
    "See https://www.openicpsr.org/openicpsr/project/121688/version/V1/view and 10.3886/E121688.",
    "We used http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/04652 and 10.3886/ICPSR21600.v17."))
  l <- icpsr_links(p)
  expect_setequal(l$doi, c("10.3886/E195681V1", "10.3886/E121688V1", "10.3886/ICPSR04652", "10.3886/ICPSR21600.v17"))
  expect_equal(l$href[l$doi == "10.3886/E195681V1"], "https://doi.org/10.3886/E195681V1")
  expect_equal(.icpsr_doi_of(c("icpsr.umich.edu/web/ICPSR/studies/4652",
                               "https://www.icpsr.umich.edu/sites/icpsr/view/studies/195681/versions/V1",
                               "10.3886/e100462v3")),
               c("10.3886/ICPSR04652", "10.3886/E195681V1", "10.3886/E100462V3"))
  expect_equal(nrow(icpsr_links(test_paper(text = "No ICPSR link."))), 0)
})

test_that("an ICPSR study is others' unless its investigators include a paper author (#660)", {
  pf <- list(title = "Purpose in life as a predictor of mortality", doi = "10.1177/0956797614531799",
             family = c("Hill", "Turiano"), full = c("Patrick Hill", "Nicholas Turiano"), orcids = character(0),
             affiliations = character(0), year = 2014, date = as.Date("2014-07-01"), abstract = "")
  own <- function(...) .repo_ownership(data.frame(..., stringsAsFactors = FALSE), pf)
  # the open-data badge sentence, a reused study that lists this paper among
  # the publications that used it
  a <- own(source = "text", repo_url = "https://doi.org/10.3886/ICPSR04652.v6", repo_type = "icpsr",
           context = "All data and materials have been made publicly available via ICPSR.",
           dep_type = "Dataset", dep_creators = "Carol Ryff; David M. Almeida",
           dep_related = "IsDocumentedBy:10.1177/0956797614531799 | IsDocumentedBy:10.1000/other")
  expect_equal(a$ownership, "others_data")
  b <- own(source = "text", repo_url = "https://doi.org/10.3886/E101179V1", repo_type = "icpsr",
           context = "The data are available at https://doi.org/10.3886/E101179V1.",
           dep_type = "Dataset", dep_creators = "Patrick Hill; Nicholas Turiano")
  expect_equal(b$ownership, "own_this")
})

test_that("icpsr_info reads a deposit's DataCite record (#660)", {
  skip_if_not(online("api.datacite.org"))
  skip_on_cran()
  i <- icpsr_info(c("https://doi.org/10.3886/E195681V1", "https://doi.org/10.3886/ICPSR04652"))
  expect_equal(i$creators[1], "Stephen Wu; Qi Ge")
  expect_match(i$title[2], "Midlife in the United States", fixed = TRUE)
  expect_true(all(is.na(i$error)))
})

test_that("repo_check lists the paper's own ICPSR package and does not check it (#660)", {
  skip_if_not(online("api.datacite.org"))
  skip_on_cran()
  p <- test_paper(text = "Our data and code are available at https://doi.org/10.3886/E195681V1.")
  p$author <- data.frame(family = c("Wu", "Ge"), given = c("Stephen", "Qi"))
  mo <- module_run(p, "repo_check")
  expect_equal(mo$summary_table$icpsr_own_n, 1L)
  rep <- paste(unlist(mo$report), collapse = " ")
  expect_match(rep, "#### Replication packages at ICPSR", fixed = TRUE)
  expect_match(rep, "does not allow automatic downloads", fixed = TRUE)
})
