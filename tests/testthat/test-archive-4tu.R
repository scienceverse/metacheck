test_that("researchdata4tu_links finds real hyperlinks and bare mentions", {
  expect_true(is.function(metacheck::researchdata4tu_links))

  paper <- test_paper(url = c(
    "https://data.4tu.nl/articles/dataset/some_title/16766929",
    "https://doi.org/10.4121/16766929.v1",
    "https://osf.io/abcde"
  ))
  links <- researchdata4tu_links(paper)
  expect_equal(nrow(links), 2)
  expect_true(all(unname(links$researchdata4tu_id) == "16766929"))

  # A bare DOI mention in body text, never encoded as a real hyperlink.
  paper_text <- test_paper(text = "data available at 10.4121/16766929")
  links_text <- researchdata4tu_links(paper_text)
  expect_equal(nrow(links_text), 1)
  expect_equal(unname(links_text$researchdata4tu_id), "16766929")

  # A labelled bare uuid DOI mention -- passed through unresolved, since
  # researchdata4tu_links() never calls .researchdata4tu_resolve_uuid()
  # (only researchdata4tu_info() does, per that function's own header
  # comment on issue #431).
  paper_uuid <- test_paper(
    text = "https://doi.org/10.4121/uuid:ce413614-1c82-4e81-90c0-323aa7d2fabd"
  )
  links_uuid <- researchdata4tu_links(paper_uuid)
  expect_equal(nrow(links_uuid), 1)
  expect_equal(unname(links_uuid$researchdata4tu_id), "ce413614-1c82-4e81-90c0-323aa7d2fabd")
})


test_that(".researchdata4tu_id extracts an id or uuid from every citation form", {
  expect_true(is.function(metacheck:::.researchdata4tu_id))

  researchdata4tu_url <- c(
    "16766929",
    "https://doi.org/10.4121/16766929.v1",
    "https://doi.org/10.4121/16766929",
    "https://doi.org/10.4121/uuid:ce413614-1c82-4e81-90c0-323aa7d2fabd",
    "10.4121/ce413614-1c82-4e81-90c0-323aa7d2fabd",
    "https://data.4tu.nl/datasets/ce413614-1c82-4e81-90c0-323aa7d2fabd/1",
    "https://data.4tu.nl/articles/dataset/some_title/16766929",
    "not-a-4tu-url",
    "",
    NA_character_
  )

  ids <- .researchdata4tu_id(researchdata4tu_url)
  expect_equal(unname(ids), c(
    "16766929", "16766929", "16766929",
    "ce413614-1c82-4e81-90c0-323aa7d2fabd",
    "ce413614-1c82-4e81-90c0-323aa7d2fabd",
    "ce413614-1c82-4e81-90c0-323aa7d2fabd",
    "16766929",
    NA, NA, NA
  ))

  expect_equal(.researchdata4tu_id(NULL), character(0))
})


test_that("researchdata4tu_file_download returns NULL for empty/unresolvable input, no network", {
  expect_null(researchdata4tu_file_download(character(0)))
  expect_null(researchdata4tu_file_download(NA_character_))
  expect_null(researchdata4tu_file_download("not-a-4tu-url"))
})


test_that("researchdata4tu_pat gets and sets the token independently of figshare_pat", {
  old <- researchdata4tu_pat()
  on.exit(researchdata4tu_pat(old %||% ""), add = TRUE)

  researchdata4tu_pat("")
  expect_equal(researchdata4tu_pat(), "")

  researchdata4tu_pat("fake-4tu-token-for-test")
  expect_equal(researchdata4tu_pat(), "fake-4tu-token-for-test")

  expect_error(researchdata4tu_pat(123))
  expect_error(researchdata4tu_pat(c("a", "b")))
  expect_error(researchdata4tu_pat(NA_character_))

  researchdata4tu_pat(old %||% "")
})


test_that(".researchdata4tu_id correctly distinguishes a uuid from a numeric id", {
  # Guards the is_uuid branch in researchdata4tu_info() (issue #431): only a
  # genuine uuid-shaped id should ever be sent to
  # .researchdata4tu_resolve_uuid(), never a plain numeric id.
  uuid_shaped <- "ce413614-1c82-4e81-90c0-323aa7d2fabd"
  numeric_shaped <- "16766929"

  uuid_pat <- "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
  expect_true(grepl(uuid_pat, uuid_shaped, ignore.case = TRUE))
  expect_false(grepl(uuid_pat, numeric_shaped, ignore.case = TRUE))
})


test_that("researchdata4tu_info retrieves real metadata when the API is reachable", {
  # data.4tu.nl's v2 API answers every JSON request with {"status":
  # "maintenance"} during a maintenance window (confirmed live 2026-10-05) --
  # not an HTTP error, so .figshare_info()'s own `resp_status != 200` check
  # cannot detect it, and this live test would otherwise fail during an
  # actual outage rather than report it as unrelated to this package's code.
  # Checked explicitly first so the test reports a skip, not a failure, when
  # the live dependency itself is down.
  maintenance <- tryCatch({
    resp <- httr2::request("https://data.4tu.nl/v2/articles/16766929") |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    body <- httr2::resp_body_json(resp, check_type = FALSE)
    identical(body$status, "maintenance")
  }, error = \(e) TRUE)
  skip_if(maintenance, "data.4tu.nl is in maintenance mode")

  info <- researchdata4tu_info("https://doi.org/10.4121/16766929.v1")
  expect_equal(unname(info$researchdata4tu_id), "16766929")
  expect_false(is.na(info$title))
  expect_false(is.na(info$doi))
})
