test_that("reshare_links finds real hyperlinks and a bare-text mention", {
  expect_true(is.function(metacheck::reshare_links))

  paper <- test_paper(url = c(
    "https://reshare.ukdataservice.ac.uk/854001/",
    "https://reshare.ukdataservice.ac.uk/854243",
    "https://doi.org/10.5255/UKDA-SN-854001",
    "https://osf.io/abcde"
  ))
  links <- reshare_links(paper)
  expect_equal(nrow(links), 3)

  by_href <- setNames(links$reshare_id, links$href)
  expect_equal(unname(by_href[["https://reshare.ukdataservice.ac.uk/854001"]]), "854001")
  expect_equal(unname(by_href[["https://reshare.ukdataservice.ac.uk/854243"]]), "854243")
  expect_equal(unname(by_href[["https://doi.org/10.5255/UKDA-SN-854001"]]), "854001")

  # Bare DOI mention in body text, no real hyperlink
  paper_text <- test_paper(text = "data are available at 10.5255/UKDA-SN-854001.")
  links_text <- reshare_links(paper_text)
  expect_equal(nrow(links_text), 1)
  expect_equal(unname(links_text$reshare_id), "854001")

  # Case-insensitivity of the DOI prefix on the url-table (hyperlink) branch
  paper_ci <- test_paper(url = "https://doi.org/10.5255/ukda-sn-854001")
  links_ci <- reshare_links(paper_ci)
  expect_equal(nrow(links_ci), 1)
})


test_that(".reshare_id extracts the eprint id from every citation form", {
  expect_true(is.function(metacheck:::.reshare_id))

  reshare_url <- c(
    "854001",
    "https://reshare.ukdataservice.ac.uk/854001",
    "https://reshare.ukdataservice.ac.uk/854001/",
    "http://reshare.ukdataservice.ac.uk/854001",
    "10.5255/UKDA-SN-854001",
    "https://doi.org/10.5255/UKDA-SN-854001",
    "https://dx.doi.org/10.5255/UKDA-SN-854001",
    "10.5255/ukda-sn-854001",
    "not-a-reshare-id",
    "",
    NA_character_
  )

  ids <- .reshare_id(reshare_url)
  expect_equal(unname(ids), c(
    "854001", "854001", "854001", "854001",
    "854001", "854001", "854001", "854001",
    NA_character_, NA_character_, NA_character_
  ))

  expect_equal(.reshare_id(NULL), character(0))
  expect_equal(.reshare_id(character(0)), character(0))
})


test_that(".reshare_info retrieves real metadata for a known public deposit", {
  info <- metacheck:::.reshare_info("854001")

  expect_false("error" %in% names(info))
  expect_equal(info$reshare_id, "854001")
  expect_equal(info$doi, "10.5255/UKDA-SN-854001")
  expect_true(grepl("Architectures of displacement", info$title))
})


test_that("reshare_info warns and marks error for a nonexistent eprint", {
  # A nonexistent eprint returns HTTP 401, not 404 (confirmed live
  # 2026-10-05) -- .reshare_info()'s `!= 200` check treats any non-200
  # identically, so this is already handled correctly; this test pins down
  # that real status code so a future refactor that special-cased "404
  # means not found" would not silently mishandle it.
  expect_warning(
    info <- reshare_info("999999999"),
    "could not be found"
  )
  expect_equal(info$error, "unfound")
})


test_that("reshare_info drops a stale pre-existing reshare_id column before recomputing it", {
  # Regression guard for the table/ids/left_join fix archive-reshare.R's own
  # comment describes: passing reshare_links()'s own output (which already
  # has a reshare_id column) back into reshare_info() must not leave a
  # stale value, or a .x/.y suffix column, in the result.
  table <- data.frame(
    reshare_url = "https://reshare.ukdataservice.ac.uk/854001",
    reshare_id = "WRONG_STALE_VALUE",
    stringsAsFactors = FALSE
  )
  result <- reshare_info(table, id_col = "reshare_url")

  expect_false(any(grepl("\\.x$|\\.y$", names(result))))
  expect_equal(result$reshare_id, "854001")
})
