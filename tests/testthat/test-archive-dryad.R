test_that("dryad_links recognises every alternate Dryad DOI prefix", {
  # Regression test: repo_check missed real Dryad deposits cited under a
  # prefix other than the usual 10.5061/dryad.* form. The full prefix list
  # (see .dryad_doi_prefixes()'s own header comment) was found by
  # exhaustively random-sampling DataCite's DOI index for Dryad's client id
  # until no new prefix appeared across 40 consecutive batches, then each
  # one individually confirmed live via doi.org's own handle API to resolve
  # to a real datadryad.org dataset page.
  expect_true(is.function(metacheck::dryad_links))

  alt_prefixes <- setdiff(metacheck:::.dryad_doi_prefixes(), "10.5061")
  urls <- c(
    "https://doi.org/10.5061/dryad.j1fd7",
    paste0("https://doi.org/", alt_prefixes, "/ABC123"),
    "https://doi.org/10.9999/unrelated123"
  )
  paper <- test_paper(url = urls)

  links <- dryad_links(paper)

  expect_equal(nrow(links), length(alt_prefixes) + 1)
  expect_true(all(paste0(c("10.5061", alt_prefixes), "/") %in%
                    paste0(sub("[^/]+$", "", tolower(unname(links$dryad_doi))))))
})

test_that(".dryad_doi extracts the DOI from every recognised prefix", {
  expect_true(is.function(metacheck:::.dryad_doi))

  expect_equal(
    metacheck:::.dryad_doi("https://doi.org/10.5061/dryad.j1fd7"),
    "10.5061/dryad.j1fd7"
  )
  expect_equal(
    metacheck:::.dryad_doi("https://doi.org/10.25338/B8N33J"),
    "10.25338/b8n33j"
  )
  expect_equal(
    metacheck:::.dryad_doi("https://doi.org/10.5068/D14671"),
    "10.5068/d14671"
  )
  expect_equal(
    metacheck:::.dryad_doi("https://doi.org/10.6075/j0w37ttw"),
    "10.6075/j0w37ttw"
  )
  expect_equal(
    metacheck:::.dryad_doi("https://doi.org/10.7941/d18907"),
    "10.7941/d18907"
  )
  # An unrelated DOI under a different registrant prefix must not match --
  # the alternate-prefix list is an explicit, verified allowlist, not a
  # wildcard, precisely to avoid this.
  expect_true(is.na(metacheck:::.dryad_doi("https://doi.org/10.9999/unrelated123")))
})

test_that("dryad_info() does not crash on dryad_links()'s own output for an unfound DOI", {
  # Regression test: dryad_info(dryad_links(paper)) -- the documented,
  # normal usage -- crashed with "Join columns in `x` must be present in
  # the data" whenever the dataset wasn't found. dryad_links()'s output
  # already carries a dryad_doi column; dryad_info() independently
  # recomputed its own `ids$dryad_doi` and left-joined it onto the table
  # without dropping the caller's existing column first, producing
  # dryad_doi.x/.y suffixes that broke the SECOND join further down
  # (by = "dryad_doi") -- confirmed live 2026-09-19, and the same shape
  # in 6 other archive-*.R files (see their own test files / commit).
  paper <- test_paper(url = "https://doi.org/10.5061/dryad.notarealdataset999")
  links <- dryad_links(paper)
  expect_no_error(dryad_info(links))
})
