test_that(".dspace7_parse extracts host, uuid, and handle", {
  expect_true(is.function(metacheck:::.dspace7_parse))

  url <- c(
    "https://repository.gatech.edu/items/bac086e5-c606-474b-af1e-4a6122694af5",
    "https://repository.gatech.edu/handle/1853/67239",
    "See https://repository.gatech.edu/handle/1853/67239. for details",
    "https://conservancy.umn.edu/handle/11299.1/123",
    "https://example.com/items/bac086e5-c606-474b-af1e-4a6122694af5",
    "not a url",
    "",
    NA
  )

  parsed <- .dspace7_parse(url)

  expect_equal(parsed$host[[1]], "repository.gatech.edu")
  expect_equal(parsed$uuid[[1]], "bac086e5-c606-474b-af1e-4a6122694af5")
  expect_true(is.na(parsed$handle[[1]]))

  expect_equal(parsed$host[[2]], "repository.gatech.edu")
  expect_true(is.na(parsed$uuid[[2]]))
  expect_equal(parsed$handle[[2]], "1853/67239")

  # Sentence-final punctuation stripped from a trailing handle (sub("[.,;]+$", ...))
  expect_equal(parsed$handle[[3]], "1853/67239")

  # Multi-part dotted handle prefix
  expect_equal(parsed$handle[[4]], "11299.1/123")

  # Unrecognised host: no host match, but the uuid pattern still matches
  # anywhere in the string regardless of host (uuid/handle extraction is not
  # gated on a recognised host)
  expect_true(is.na(parsed$host[[5]]))
  expect_equal(parsed$uuid[[5]], "bac086e5-c606-474b-af1e-4a6122694af5")

  expect_true(is.na(parsed$host[[6]]))
  expect_true(is.na(parsed$host[[7]]))
  expect_true(is.na(parsed$host[[8]]))
})


test_that(".dspace7_parse resolves a UI-only host to its real API host (issue #461, Cambridge)", {
  # Apollo, University of Cambridge: the citable/DOI-redirect host
  # (www.repository.cam.ac.uk) does not itself serve /server/api (confirmed
  # live 2026-10-05: 404) -- the real API lives at a different subdomain,
  # api.repository.cam.ac.uk. .dspace7_parse()'s `host` column must carry the
  # API host, not the literal URL host, since it is used downstream to build
  # the actual /server/api request (dspace7_file_download()/.dspace7_info()).
  aliases <- .dspace7_api_host_aliases()
  expect_equal(unname(aliases[["www.repository.cam.ac.uk"]]), "api.repository.cam.ac.uk")

  parsed <- .dspace7_parse("https://www.repository.cam.ac.uk/handle/1810/123456")
  expect_equal(parsed$host[[1]], "api.repository.cam.ac.uk")
  expect_equal(parsed$handle[[1]], "1810/123456")

  # Case-insensitivity of the host match feeding into the alias lookup
  parsed_upper <- .dspace7_parse("https://WWW.REPOSITORY.CAM.AC.UK/handle/1810/1")
  expect_equal(parsed_upper$host[[1]], "api.repository.cam.ac.uk")

  # A host with no alias entry is returned unchanged
  parsed_gatech <- .dspace7_parse("https://repository.gatech.edu/handle/1853/67239")
  expect_equal(parsed_gatech$host[[1]], "repository.gatech.edu")
})


test_that(".dspace7_hosts includes the hosts added by issue #461", {
  hosts <- .dspace7_hosts()
  expect_true("ecommons.cornell.edu" %in% hosts)
  expect_true("www.repository.cam.ac.uk" %in% hosts)
  # Cornell was proposed as LEGACY DSpace by issue #461 but confirmed live
  # 2026-10-05 to actually be DSpace 7+ (/rest/test 404s with a DSpace-CRIS
  # branded page; /server/api returns a real DSpace 8 root document) -- it
  # must NOT also be listed as legacy.
  expect_false("ecommons.cornell.edu" %in% metacheck:::.dspace_legacy_hosts())
})


test_that(".dspace7_host_regex matches known hosts only, with dots escaped literally", {
  rx <- .dspace7_host_regex()
  expect_true(grepl(rx, "repository.gatech.edu", perl = TRUE))
  expect_true(grepl(rx, "www.repository.cam.ac.uk", perl = TRUE))
  expect_false(grepl(rx, "figshare.com", perl = TRUE))
  # Dots must be escaped literally, not left as a regex "any character"
  # wildcard -- substituting a non-dot character must NOT still match.
  expect_false(grepl(rx, "repositoryXgatechXedu", perl = TRUE))
})


test_that("dspace7_links finds real hyperlinks and bare mentions, deduplicated", {
  expect_true(is.function(metacheck::dspace7_links))

  paper <- test_paper(url = "https://repository.gatech.edu/handle/1853/67239/")
  links <- dspace7_links(paper)
  expect_equal(nrow(links), 1)
  # trailing slash stripped
  expect_equal(links$href, "https://repository.gatech.edu/handle/1853/67239")

  # Non-DSpace7 host excluded entirely
  paper_other <- test_paper(url = "https://figshare.com/articles/dataset/x/1")
  expect_equal(nrow(dspace7_links(paper_other)), 0)

  # Bare-mention fallback in body text, no real hyperlink
  paper_text <- test_paper(
    text = "data is available at repository.gatech.edu/handle/1853/67239 under an open license."
  )
  links_text <- dspace7_links(paper_text)
  expect_equal(nrow(links_text), 1)
  expect_true(grepl("repository.gatech.edu/handle/1853/67239", links_text$href, fixed = TRUE))

  # Same URL present as both a real hyperlink and a bare text mention
  # collapses to one row (same normalization osf_links()/figshare_links() use)
  paper_dup <- test_paper(
    url = "https://repository.gatech.edu/handle/1853/67239",
    text = "see https://repository.gatech.edu/handle/1853/67239"
  )
  expect_equal(nrow(dspace7_links(paper_dup)), 1)
})


test_that(".dspace7_info retrieves a real Georgia Tech item by uuid and by handle", {
  # Live test against the file's own documented reference item (verified
  # live again 2026-10-05, still resolves identically).
  by_uuid <- .dspace7_info("repository.gatech.edu", uuid = "bac086e5-c606-474b-af1e-4a6122694af5")
  by_handle <- .dspace7_info("repository.gatech.edu", handle = "1853/67239")

  expect_equal(by_uuid$dspace7_uuid, "bac086e5-c606-474b-af1e-4a6122694af5")
  expect_equal(by_handle$dspace7_uuid, "bac086e5-c606-474b-af1e-4a6122694af5")
  expect_equal(by_uuid$files[[1]]$name, "ALVARADOGARCIA-DISSERTATION-2022.pdf")
})


test_that(".dspace7_info resolves a real Cambridge item through the UI-host alias", {
  # Live test confirming the alias added for issue #461 actually produces a
  # working end-to-end request, not just a correct-looking parse.
  parsed <- .dspace7_parse("https://www.repository.cam.ac.uk/items/00000000-0000-0000-0000-000000000000")
  expect_equal(parsed$host[[1]], "api.repository.cam.ac.uk")

  # A syntactically valid but nonexistent uuid against the real (aliased)
  # API host should cleanly report "unfound", not fail outright by hitting
  # the wrong (UI) host.
  expect_warning(
    info <- .dspace7_info("api.repository.cam.ac.uk", uuid = "00000000-0000-0000-0000-000000000000"),
    "could not be found"
  )
  expect_equal(info$error, "unfound")
})
