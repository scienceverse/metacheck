test_that("cran_links finds CRAN package links in their forms (#478, #660)", {
  p <- test_paper(text = c(
    "A stable version can be downloaded through CRAN at http://cran.r-project.org/web/packages/Reol/index.html.",
    "We used lme4 (https://CRAN.R-project.org/package=lme4) and its vignette https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html.",
    "Cite it as 10.32614/CRAN.package.metafor.",
    "R is at https://cran.r-project.org/."))
  l <- cran_links(p)
  expect_equal(l$package, c("Reol", "lme4", "metafor"))
  expect_equal(l$href[1], "https://CRAN.R-project.org/package=Reol")
  # the sentence is found through the link as the paper writes it
  expect_equal(.own_needle(l$href[1]), "cran.r-project.org/web/packages/reol")
  expect_equal(.cran_people("Roger Bivand [cre, aut] (<https://orcid.org/0000-0003-2392-6140>), Nicholas Lewin-Koh [aut]"),
               "Roger Bivand; Nicholas Lewin-Koh")
})

test_that("a CRAN package's authors settle whose package it is (#478, #660)", {
  pf <- list(title = "FuzzyQ: Fuzzy quantification of common and rare species", doi = "10.1111/xyz",
             family = c("Balbuena", "Morand"), full = c("Juan Balbuena", "Serge Morand"), orcids = character(0),
             affiliations = character(0), year = 2021, date = as.Date("2021-07-01"), abstract = "")
  own <- function(...) .repo_ownership(data.frame(..., stringsAsFactors = FALSE), pf)
  a <- own(source = "text", repo_url = "https://CRAN.R-project.org/package=FuzzyQ", repo_type = "cran",
           context = "FuzzyQ is provided as an R package, available on CRAN.", dep_type = "Software",
           dep_creators = "Juan A. Balbuena; Clara Monlleo-Borrull")
  expect_equal(a$ownership, "own_this")
  b <- own(source = "text", repo_url = "https://CRAN.R-project.org/package=lme4", repo_type = "cran",
           context = "Models were fitted with lme4, available on CRAN.", dep_type = "Software",
           dep_creators = paste(paste("Author", LETTERS[1:22]), collapse = "; "))
  expect_equal(b$ownership, "others_code")
  d <- own(source = "text", repo_url = "https://CRAN.R-project.org/package=specieslist", repo_type = "cran",
           context = "Species names were checked with the specieslist package.", dep_type = "Software",
           dep_creators = "Serge Morand")
  expect_equal(d$ownership, "own_earlier_code")
})

test_that("cran_info reads a package's record, also an archived one (#478, #660)", {
  skip_if_not(online("crandb.r-pkg.org"))
  skip_on_cran()
  i <- cran_info(c("https://CRAN.R-project.org/package=Reol", "https://CRAN.R-project.org/package=notapackagexyz"))
  expect_equal(i$creators[1], "Barb Banbury; Brian O'Meara")
  expect_equal(i$error[2], "unfound")
})

test_that("repo_check lists the paper's own CRAN package and does not check it (#478, #660)", {
  skip_if_not(online("crandb.r-pkg.org"))
  skip_on_cran()
  p <- test_paper(text = "Reol is available on CRAN at http://cran.r-project.org/web/packages/Reol/index.html.")
  p$author <- data.frame(family = c("Banbury", "O'Meara"), given = c("Barb", "Brian"))
  mo <- module_run(p, "repo_check")
  expect_equal(mo$summary_table$cran_own_n, 1L)
  expect_equal(mo$summary_table$repo_n, 0L)
  rep <- paste(unlist(mo$report), collapse = " ")
  expect_match(rep, "#### R packages on CRAN", fixed = TRUE)
})
