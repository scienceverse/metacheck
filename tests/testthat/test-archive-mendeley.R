test_that("mendeley_links finds real hyperlinks and a bare-text mention", {
  expect_true(is.function(metacheck::mendeley_links))

  paper <- test_paper(url = c(
    "https://data.mendeley.com/datasets/vjtxybrc28/1",
    "https://doi.org/10.17632/vjtxybrc28.1",
    "https://osf.io/abcde"
  ))
  links <- mendeley_links(paper)
  expect_equal(nrow(links), 2)
  expect_true(all(unname(links$mendeley_id) == "vjtxybrc28"))
  expect_true(all(links$mendeley_link == "https://doi.org/10.17632/vjtxybrc28"))

  paper_text <- test_paper(text = "data are available at 10.17632/vjtxybrc28.1 under CC-BY")
  links_text <- mendeley_links(paper_text)
  expect_equal(nrow(links_text), 1)
  expect_equal(unname(links_text$mendeley_id), "vjtxybrc28")
})


test_that(".mendeley_id extracts the dataset id from every citation form", {
  expect_true(is.function(metacheck:::.mendeley_id))

  mendeley_url <- c(
    "vjtxybrc28",
    "https://doi.org/10.17632/vjtxybrc28.1",
    "https://doi.org/10.17632/vjtxybrc28",
    "10.17632/vjtxybrc28.1",
    "https://data.mendeley.com/datasets/vjtxybrc28/1",
    "https://data.mendeley.com/datasets/vjtxybrc28",
    "not-a-mendeley-url",
    "",
    NA_character_
  )

  ids <- .mendeley_id(mendeley_url)
  expect_equal(unname(ids), c(
    "vjtxybrc28", "vjtxybrc28", "vjtxybrc28", "vjtxybrc28",
    "vjtxybrc28", "vjtxybrc28",
    NA_character_, NA_character_, NA_character_
  ))

  expect_equal(.mendeley_id(NULL), character(0))
})


test_that("mendeley_links builds a NA link for an unresolvable id, not a malformed DOI", {
  paper <- test_paper(url = "https://data.mendeley.com/datasets/")
  links <- mendeley_links(paper)
  if (nrow(links) > 0) {
    expect_true(all(is.na(links$mendeley_link[is.na(links$mendeley_id)])))
  }
})


test_that(".mendeley_info retrieves real metadata for a known public dataset", {
  info <- metacheck:::.mendeley_info("vjtxybrc28")

  expect_false("error" %in% names(info))
  expect_equal(
    info$title,
    "Reference group influences and campaign exposure effects on rhino horn demand"
  )
  expect_equal(info$doi, "10.17632/vjtxybrc28.1")
  expect_equal(info$license, "CC BY 4.0")
  expect_true(grepl("Dang Vu", info$authors[[1]]))
  # files is the raw parsed-JSON list (rec$files), not a data frame, unlike
  # several sibling archive-*.R files' own `files` columns -- confirmed
  # directly rather than assumed.
  expect_equal(length(info$files[[1]]), 1)
  expect_equal(info$files[[1]][[1]]$filename, "rhusers1.csv")
})


test_that(".mendeley_info marks a malformed id as unfound (real HTTP 400)", {
  expect_warning(
    info <- metacheck:::.mendeley_info("zzzzznope"),
    "could not be found"
  )
  expect_equal(info$error, "unfound")
})


test_that(".mendeley_info documents current behaviour for a well-formed but nonexistent id", {
  # A nonexistent (but well-formed) dataset id does NOT 404 at the HTTP
  # layer -- confirmed live 2026-10-05: GET .../public-api/datasets/
  # doesnotexist12345xyz returns HTTP 200 with body
  # {"error":{"message":"error - dataset not found","status":404}}. The
  # real 404 is only inside the JSON payload. .mendeley_info()'s check
  # (resp_status != 200) does not catch this: the response parses fine as
  # JSON, so every real field access falls through to `%empty_or% NA`
  # because the expected fields simply aren't present in {"error": {...}}.
  # The practical result, documented here rather than fixed: no `error`
  # column is set at all, and every metadata field comes back NA. This is
  # reported as a finding, not treated as desired behaviour.
  info <- metacheck:::.mendeley_info("doesnotexist12345xyz")
  expect_false("error" %in% names(info))
  expect_true(is.na(info$title))
  expect_true(is.na(info$doi))
})
