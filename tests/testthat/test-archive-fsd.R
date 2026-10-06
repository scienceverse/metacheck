test_that(".fsd_study_id extracts FSD study ids from every citation form", {
  expect_true(is.function(metacheck:::.fsd_study_id))

  # NOTE: .fsd_study_id() is NOT vectorised -- it uses regexpr() (a single
  # match) and an `if (length(m) == 0 || !nzchar(m))` guard that errors with
  # "'length = n' in coercion to 'logical(1)'" on an input of length > 1
  # (confirmed directly: .fsd_study_id(c("FSD1", "FSD2")) errors rather than
  # returning a length-2 result). Every other sibling id-extractor in this
  # package (.figshare_id(), .dataone_pid(), .mendeley_id()) is vectorised,
  # so this is likely an unintentional gap rather than a deliberate design
  # choice -- flagged, not fixed here. Each case below is therefore its own
  # single-element call, matching the function's real (non-vectorised)
  # contract rather than the vectorised one its siblings have.
  cases <- c(
    "https://services.fsd.tuni.fi/catalogue/FSD2653",
    "https://services.fsd.tuni.fi/catalogue/FSD2653/DDI/FSD2653_eng.xml",
    "https://doi.org/10.60686/t-fsd2653",
    "10.60686/t-fsd2653",
    "https://urn.fi/urn:nbn:fi:fsd:t-fsd2653",
    "see FSD2653 for details",
    "FSD_2653",
    "FSD-2653"
  )
  for (x in cases) expect_equal(.fsd_study_id(x), "FSD2653")

  expect_true(is.na(.fsd_study_id("not-an-fsd-reference")))
  expect_true(is.na(.fsd_study_id("")))
  expect_true(is.na(.fsd_study_id(NA)))
})


test_that("fsd_links finds real hyperlinks and a bare-text mention", {
  expect_true(is.function(metacheck::fsd_links))

  paper <- test_paper(url = c(
    "https://services.fsd.tuni.fi/catalogue/FSD2653",
    "https://doi.org/10.60686/t-fsd2653",
    "https://urn.fi/urn:nbn:fi:fsd:t-fsd2653",
    "https://osf.io/abcde"
  ))
  links <- fsd_links(paper)
  expect_equal(nrow(links), 3)
  expect_true(all(c(
    "https://services.fsd.tuni.fi/catalogue/FSD2653",
    "https://doi.org/10.60686/t-fsd2653",
    "https://urn.fi/urn:nbn:fi:fsd:t-fsd2653"
  ) %in% links$href))

  paper_text <- test_paper(text = "the data are available from FSD (study FSD2653).")
  links_text <- fsd_links(paper_text)
  expect_equal(nrow(links_text), 1)

  # Dedup is on the href STRING after a trailing-slash strip (same mechanism
  # osf_links()/figshare_links() use), not on the resolved study id -- a real
  # hyperlink to the catalogue page and a bare "FSD2653" text mention are two
  # different href strings, so they do NOT collapse into one row even though
  # both resolve to the same study.
  paper_mixed <- test_paper(
    url = "https://services.fsd.tuni.fi/catalogue/FSD2653/",
    text = "see FSD2653"
  )
  links_mixed <- fsd_links(paper_mixed)
  expect_equal(nrow(links_mixed), 2)

  # NOTE: the trailing-slash normalization comment in archive-fsd.R (mirrors
  # osf_links()'s own comment almost verbatim) describes collapsing a real
  # hyperlink and a bare-text mention of the SAME full URL into one row --
  # but fsd_bare_regex is "FSD[0-9]{3,6}" (always captures just the short id,
  # e.g. "FSD2653"), unlike osf_bare_regex ("(?:https?://)?osf\\.io/...",
  # which captures the full URL). So even when the exact same full URL
  # appears both as a real hyperlink AND written out in the body text, the
  # two never produce the same href string and this file's dedup does not
  # collapse them -- confirmed directly (2 rows, not 1). This appears to be
  # a real discrepancy between the comment and the regex, not intentional
  # behavior; documented here rather than silently worked around.
  paper_same_url_twice <- test_paper(
    url = "https://services.fsd.tuni.fi/catalogue/FSD2653",
    text = "available at https://services.fsd.tuni.fi/catalogue/FSD2653/ (archived)."
  )
  expect_equal(nrow(fsd_links(paper_same_url_twice)), 2)
})


test_that(".fsd_info reports unfound for an unparseable study id, no network", {
  expect_warning(
    obj <- .fsd_info("not-a-valid-fsd-url"),
    "is not a valid FSD study reference"
  )
  expect_equal(obj$error, "unfound")
})


test_that(".fsd_info retrieves real metadata for FSD2653", {
  info <- metacheck:::.fsd_info("https://services.fsd.tuni.fi/catalogue/FSD2653")

  expect_false("error" %in% names(info))
  expect_equal(info$FSD_title, "Finnish National Election Study 2011")
  expect_equal(info$FSD_doi, "10.60686/t-fsd2653")
  expect_equal(
    info$FSD_access,
    "The dataset is (B) available for research, teaching and study."
  )

  files <- info$files[[1]]
  expect_equal(nrow(files), 1)
  expect_equal(files$name, "daF2653e.por")
  expect_equal(files$format, "SPSS Portable")
  expect_equal(files$cases, 1298L)
  expect_equal(files$variables, 492L)
})


test_that("fsd_info returns error=unfound for a nonexistent study id", {
  expect_warning(
    result <- fsd_info("https://services.fsd.tuni.fi/catalogue/FSD99999999"),
    "could not be found"
  )
  expect_equal(result$error, "unfound")
})
