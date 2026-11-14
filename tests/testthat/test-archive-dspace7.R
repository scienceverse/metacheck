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


# The test bodies below are wrapped in local(): helper.R's test_that()
# wrapper evaluates a test's code in the test FILE's environment, so a
# withr/testthat local_*() call made directly in it would stay in force
# until the end of the file instead of the end of the test.

# A mocked DSpace 7 server for the tests below: answers each request from
# `routes` (URL -> response) and records every URL asked for, so a test can
# check how many requests a lookup made.
local_dspace7_server <- function(routes, env = parent.frame()) {
  # DataCite's records are remembered for the session (R/repo-kind.R): each
  # test starts without them, so it sees the DataCite request it makes
  rm(list = ls(.repo_doi_cache), envir = .repo_doi_cache)
  withr::defer(rm(list = ls(.repo_doi_cache), envir = .repo_doi_cache), envir = env)
  seen <- new.env()
  seen$urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    seen$urls <- c(seen$urls, req$url)
    resp <- routes[[req$url]]
    if (is.null(resp)) return(httr2::response(status_code = 404, url = req$url))
    resp
  }, env = env)
  seen
}

ds7_bitstream <- function(name, size = 1) {
  list(name = name, sizeBytes = size, checkSum = list(value = "abc"),
       `_links` = list(content = list(href = paste0("https://h.example/server/api/core/bitstreams/", name, "/content"))))
}

ds7_item <- function(uuid, bundles) {
  list(uuid = uuid, name = "An item", metadata = list(),
       `_embedded` = list(bundles = list(`_embedded` = list(bundles = bundles))))
}

test_that(".dspace7_info resolves a handle in two requests and follows bitstream pages", local({
  uuid <- "11111111-2222-3333-4444-555555555555"
  api <- "https://repository.gatech.edu/server/api"
  original <- list(
    name = "ORIGINAL", uuid = "b-orig",
    `_embedded` = list(bitstreams = list(
      `_embedded` = list(bitstreams = list(ds7_bitstream("a.csv"), ds7_bitstream("b.csv"))),
      page = list(size = 2, totalElements = 3, totalPages = 2, number = 0),
      `_links` = list(`next` = list(href = paste0(api, "/core/bundles/b-orig/bitstreams?page=1&size=2")))
    )))
  license <- list(name = "LICENSE", uuid = "b-lic",
                  `_embedded` = list(bitstreams = list(
                    `_embedded` = list(bitstreams = list(ds7_bitstream("license.txt"))),
                    page = list(totalElements = 1))))
  routes <- list()
  routes[[paste0(api, "/pid/find?id=1853%2F67239")]] <- httr2::response(
    status_code = 302, url = paste0(api, "/pid/find?id=1853%2F67239"),
    headers = list(Location = paste0(api, "/core/items/", uuid)))
  routes[[paste0(api, "/core/items/", uuid, "?embed=bundles/bitstreams&embed.size=bundles/bitstreams=200")]] <-
    httr2::response_json(body = ds7_item(uuid, list(original, license)))
  routes[[paste0(api, "/core/bundles/b-orig/bitstreams?page=1&size=2")]] <-
    httr2::response_json(body = list(
      `_embedded` = list(bitstreams = list(ds7_bitstream("c.csv"))),
      page = list(size = 2, totalElements = 3, totalPages = 2, number = 1)))
  seen <- local_dspace7_server(routes)

  info <- .dspace7_info("repository.gatech.edu", handle = "1853/67239")
  expect_equal(info$dspace7_uuid, uuid)
  # all three ORIGINAL files, across two pages; LICENSE left out as before
  expect_equal(info$files[[1]]$name, c("a.csv", "b.csv", "c.csv"))
  # pid/find (not followed) + one embedded item request + the second page;
  # this took 4 requests before, and silently stopped at the first page
  expect_length(seen$urls, 3)
  expect_false(any(grepl("/bundles$", seen$urls)))

  # By uuid: the single embedded request plus the second page.
  seen$urls <- character(0)
  info2 <- .dspace7_info("repository.gatech.edu", uuid = uuid)
  expect_equal(info2$files[[1]]$name, c("a.csv", "b.csv", "c.csv"))
  expect_length(seen$urls, 2)
}))

test_that(".dspace7_info warns when a bitstream page cannot be read", local({
  uuid <- "11111111-2222-3333-4444-555555555555"
  api <- "https://repository.gatech.edu/server/api"
  original <- list(
    name = "ORIGINAL", uuid = "b-orig",
    `_embedded` = list(bitstreams = list(
      `_embedded` = list(bitstreams = list(ds7_bitstream("a.csv"))),
      page = list(totalElements = 2),
      `_links` = list(`next` = list(href = paste0(api, "/core/bundles/b-orig/bitstreams?page=1&size=1"))))))
  routes <- list()
  routes[[paste0(api, "/core/items/", uuid, "?embed=bundles/bitstreams&embed.size=bundles/bitstreams=200")]] <-
    httr2::response_json(body = ds7_item(uuid, list(original)))
  local_dspace7_server(routes)
  expect_warning(info <- .dspace7_info("repository.gatech.edu", uuid = uuid),
                 "only 1 of the 2 files")
  expect_equal(info$files[[1]]$name, "a.csv")
}))

test_that(".dspace7_info falls back to separate requests when embed is ignored", local({
  uuid <- "11111111-2222-3333-4444-555555555555"
  api <- "https://repository.gatech.edu/server/api"
  routes <- list()
  routes[[paste0(api, "/core/items/", uuid, "?embed=bundles/bitstreams&embed.size=bundles/bitstreams=200")]] <-
    httr2::response_json(body = list(uuid = uuid, name = "An item", metadata = list()))
  routes[[paste0(api, "/core/items/", uuid, "/bundles")]] <-
    httr2::response_json(body = list(`_embedded` = list(bundles = list(
      list(name = "ORIGINAL", uuid = "b-orig")))))
  routes[[paste0(api, "/core/bundles/b-orig/bitstreams?size=200")]] <-
    httr2::response_json(body = list(`_embedded` = list(bitstreams = list(ds7_bitstream("a.csv"))),
                                     page = list(totalElements = 1)))
  seen <- local_dspace7_server(routes)
  info <- .dspace7_info("repository.gatech.edu", uuid = uuid)
  expect_equal(info$files[[1]]$name, "a.csv")
  expect_length(seen$urls, 3)
}))

test_that(".dspace7_find_uuid reads the uuid from the redirect, or from a 200 answer", local({
  api <- "https://h.example/server/api"
  uuid <- "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  routes <- list()
  routes[[paste0(api, "/pid/find?id=1%2F2")]] <- httr2::response(
    status_code = 302, headers = list(Location = paste0(api, "/core/items/", toupper(uuid))))
  routes[[paste0(api, "/pid/find?id=1%2F3")]] <- httr2::response_json(body = list(uuid = uuid))
  local_dspace7_server(routes)
  expect_equal(.dspace7_find_uuid("h.example", "1/2"), uuid)
  expect_equal(.dspace7_find_uuid("h.example", "1/3"), uuid)
  expect_true(is.na(.dspace7_find_uuid("h.example", "1/4")))
}))

test_that(".dspace7_skip_doi_prefixes covers the other detectors but no DSpace 7 prefix", local({
  skip <- .dspace7_skip_doi_prefixes()
  expect_true(all(c("10.17605", "10.5281", "10.5061", "10.25338", "10.6084",
                    "10.26180", "10.4121", "10.17632", "10.18739", "10.5063",
                    "10.6085", "10.5255", "10.23668", "10.7910", "10.26300")
                  %in% skip))
  # DOI prefixes DataCite lists with URLs on .dspace7_hosts() installations
  # (facet of url:*<host>* for every host, 2026-10-13). 10.23673 is the one
  # that is also in a Dataverse table (datadoi.ee): dspace.ut.ee registers
  # DOIs under it too, so it must still be resolved.
  dspace7_prefixes <- c(
    "10.83384", "10.82482", "10.58086", "10.58164", "10.57776", "10.34642",
    "10.34621", "10.48684", "10.13020", "10.71959", "10.71700", "10.60668",
    "10.21256", "10.2312", "10.18420", "10.17862", "10.57996", "10.15157",
    "10.23673", "10.57839", "10.82419", "10.26041", "10.82463", "10.82921",
    "10.25593", "10.60644", "10.18747", "10.83160", "10.60473", "10.82218",
    "10.60757", "10.82366", "10.34744", "10.13028", "10.35090", "10.25330",
    "10.25676", "10.82589", "10.13025", "10.25818", "10.25540", "10.34899",
    "10.25722", "10.34944", "10.7275", "10.58142", "10.83129", "10.71781",
    "10.7298", "10.17863")
  expect_equal(intersect(skip, dspace7_prefixes), character(0))
}))

test_that(".dspace7_resolve_doi_url uses the handle API, follows hdl.handle.net, and caches", local({
  rm(list = ls(.dspace7_doi_cache), envir = .dspace7_doi_cache)
  withr::defer(rm(list = ls(.dspace7_doi_cache), envir = .dspace7_doi_cache))
  handle_json <- function(url) {
    httr2::response_json(body = list(responseCode = 1, values = list(
      list(index = 1, type = "URL", data = list(format = "string", value = url)))))
  }
  routes <- list()
  # 10.13020/q3y6-h459 -> hdl.handle.net/11299/208294 -> conservancy.umn.edu
  # (the real chain, verified 2026-10-13)
  routes[["https://doi.org/api/handles/10.13020%2Fq3y6-h459?type=URL"]] <-
    handle_json("http://hdl.handle.net/11299/208294")
  routes[["https://hdl.handle.net/api/handles/11299%2F208294?type=URL"]] <-
    handle_json("https://conservancy.umn.edu/handle/11299/208294")
  # a DOI still registered at a repository's former address
  routes[["https://doi.org/api/handles/10.35090%2Fgatech%2F5981?type=URL"]] <-
    handle_json("https://smartech.gatech.edu/handle/1853/70320")
  routes[["https://hdl.handle.net/api/handles/1853%2F70320?type=URL"]] <-
    handle_json("https://repository.gatech.edu/handle/1853/70320")
  # a DOI leading somewhere else entirely
  routes[["https://doi.org/api/handles/10.1177%2F0956797620000000?type=URL"]] <-
    handle_json("https://journals.sagepub.com/doi/10.1177/0956797620000000")
  # an unregistered DOI
  routes[["https://doi.org/api/handles/10.9999%2Fnone?type=URL"]] <-
    httr2::response_json(status_code = 404, body = list(responseCode = 100))
  # a server error: not a definite answer
  routes[["https://doi.org/api/handles/10.9999%2Ferror?type=URL"]] <-
    httr2::response(status_code = 500)
  seen <- local_dspace7_server(routes)

  # DataCite first (not found here), then the handle service twice
  expect_equal(.dspace7_resolve_doi_url("10.13020/q3y6-h459"),
               "https://conservancy.umn.edu/handle/11299/208294")
  expect_equal(seen$urls[1], "https://api.datacite.org/dois/10.13020/q3y6-h459")
  expect_length(seen$urls, 3)
  # the same DOI again, in another case: answered from the session cache
  expect_equal(.dspace7_resolve_doi_url("10.13020/Q3Y6-H459"),
               "https://conservancy.umn.edu/handle/11299/208294")
  expect_length(seen$urls, 3)

  expect_equal(.dspace7_resolve_doi_url("10.35090/gatech/5981"),
               "https://repository.gatech.edu/handle/1853/70320")
  expect_true(is.na(.dspace7_resolve_doi_url("10.1177/0956797620000000")))
  expect_true(is.na(.dspace7_resolve_doi_url("10.9999/none")))
  expect_true(exists("10.9999/none", envir = .dspace7_doi_cache, inherits = FALSE))
  expect_true(is.na(.dspace7_resolve_doi_url("10.9999/error")))
  expect_false(exists("10.9999/error", envir = .dspace7_doi_cache, inherits = FALSE))
}))

test_that("dspace7_links does not look up the paper's own DOI or DOIs other detectors handle", local({
  rm(list = ls(.dspace7_doi_cache), envir = .dspace7_doi_cache)
  withr::defer(rm(list = ls(.dspace7_doi_cache), envir = .dspace7_doi_cache))
  routes <- list()
  routes[["https://doi.org/api/handles/10.13020%2Fq3y6-h459?type=URL"]] <-
    httr2::response_json(body = list(responseCode = 1, values = list(
      list(type = "URL", data = list(value = "https://conservancy.umn.edu/handle/11299/208294")))))
  seen <- local_dspace7_server(routes)

  paper <- test_paper(text = c(
    "Data are at https://doi.org/10.13020/q3y6-h459.",
    "Cite this paper as https://doi.org/10.1177/09567976231234567.",
    "Materials: https://doi.org/10.17605/OSF.IO/ABCDE and 10.5281/zenodo.1234567.",
    "Also 10.5061/dryad.j1fd7 and 10.7910/DVN/ABCDEF."
  ))
  paper$info$doi <- "10.1177/09567976231234567"
  links <- dspace7_links(paper)

  expect_equal(links$href, "https://conservancy.umn.edu/handle/11299/208294")
  expect_equal(seen$urls, c("https://api.datacite.org/dois/10.13020/q3y6-h459",
                          "https://doi.org/api/handles/10.13020%2Fq3y6-h459?type=URL"))
}))

test_that("dspace7_links looks up only DOIs under a DSpace 7 prefix (#636)", local({
  rm(list = ls(.dspace7_doi_cache), envir = .dspace7_doi_cache)
  withr::defer(rm(list = ls(.dspace7_doi_cache), envir = .dspace7_doi_cache))
  routes <- list()
  routes[["https://doi.org/api/handles/10.13020%2Fq3y6-h459?type=URL"]] <-
    httr2::response_json(body = list(responseCode = 1, values = list(
      list(type = "URL", data = list(value = "https://conservancy.umn.edu/handle/11299/208294")))))
  seen <- local_dspace7_server(routes)

  # a journal's figure DOI and an article DOI: today looked up, never DSpace
  paper <- test_paper(text = c(
    "Data are at https://doi.org/10.13020/q3y6-h459.",
    "See Fig 1 (https://doi.org/10.1371/journal.pone.0000001.g001) and 10.1016/j.cognition.2020.104.",
    "Its source is 10.7554/eLife.12345."
  ))
  links <- dspace7_links(paper)
  expect_equal(links$href, "https://conservancy.umn.edu/handle/11299/208294")
  expect_equal(seen$urls, c("https://api.datacite.org/dois/10.13020/q3y6-h459",
                          "https://doi.org/api/handles/10.13020%2Fq3y6-h459?type=URL"))
}))

test_that(".dspace7_doi_prefixes has the DSpace 7 prefixes found in October and the shared hosts' ones (#636)", {
  p <- .dspace7_doi_prefixes()
  # the 50 prefixes of the test above (.dspace7_skip_doi_prefixes)
  expect_true(all(c("10.83384", "10.82482", "10.58086", "10.58164", "10.57776", "10.34642",
    "10.34621", "10.48684", "10.13020", "10.71959", "10.71700", "10.60668",
    "10.21256", "10.2312", "10.18420", "10.17862", "10.57996", "10.15157",
    "10.23673", "10.57839", "10.82419", "10.26041", "10.82463", "10.82921",
    "10.25593", "10.60644", "10.18747", "10.83160", "10.60473", "10.82218",
    "10.60757", "10.82366", "10.34744", "10.13028", "10.35090", "10.25330",
    "10.25676", "10.82589", "10.13025", "10.25818", "10.25540", "10.34899",
    "10.25722", "10.34944", "10.7275", "10.58142", "10.83129", "10.71781",
    "10.7298", "10.17863") %in% p))
  # ETH Zürich registers its DOIs with a handle address; Movebank and
  # Cambridge were found in the corpora's text
  expect_true(all(c("10.3929", "10.5441", "10.17863") %in% p))
  # never a journal's prefix
  expect_false(any(c("10.1371", "10.7554", "10.1016", "10.1177") %in% p))
  # every generated host passes the same host regex as the hand-written ones
  expect_true(all(grepl(.dspace7_host_regex(), paste0("https://", .dspace7_hosts_survey(), "/handle/1/2"))))
})

test_that("DSpace 7: a known host is matched by host name, and messages name the handle (U42, U211)", {
  # a host anywhere in the URL counted, and an item without a uuid was
  # named "(NA)"
  expect_true(metacheck:::.dspace7_on_known_host("https://ecommons.cornell.edu/items/x"))
  expect_true(metacheck:::.dspace7_on_known_host("https://sub.ecommons.cornell.edu/items/x"))
  expect_false(metacheck:::.dspace7_on_known_host("https://example.org/?from=ecommons.cornell.edu"))
  testthat::local_mocked_bindings(.dspace7_find_uuid = function(...) NA_character_)
  expect_warning(metacheck:::.dspace7_info("ecommons.cornell.edu", handle = "1813/12345"),
                 "(1813/12345)", fixed = TRUE)
})

test_that("DataCite says before reading whether a DSpace DOI is a publication (#637)", local({
  rm(list = ls(.dspace7_doi_cache), envir = .dspace7_doi_cache)
  withr::defer(rm(list = ls(.dspace7_doi_cache), envir = .dspace7_doi_cache))
  dc <- function(url, type) httr2::response_json(body = list(data = list(attributes = list(
    url = url, types = list(resourceTypeGeneral = type)))))
  routes <- list()
  routes[["https://api.datacite.org/dois/10.13020/thesis1"]] <-
    dc("https://conservancy.umn.edu/handle/11299/1", "Dissertation")
  routes[["https://api.datacite.org/dois/10.13020/data1"]] <-
    dc("https://conservancy.umn.edu/handle/11299/2", "Dataset")
  seen <- local_dspace7_server(routes)
  # a thesis: no further request, not a deposit
  expect_true(is.na(.dspace7_resolve_doi_url("10.13020/thesis1")))
  # a dataset: DataCite's address, without the handle service
  expect_equal(.dspace7_resolve_doi_url("10.13020/data1"), "https://conservancy.umn.edu/handle/11299/2")
  expect_equal(seen$urls, c("https://api.datacite.org/dois/10.13020/thesis1",
                            "https://api.datacite.org/dois/10.13020/data1"))
}))

test_that(".repo_publication_type reads EPrints, DSpace and DataCite types (#637)", {
  expect_equal(.repo_publication_type(c("data_collection", "dataset", "Dataset", "Software", "artefact", "Data paper")),
               rep(FALSE, 6))
  expect_equal(.repo_publication_type(c("article", "thesis", "book_section", "Doctoral thesis", "Journal Article",
                                        "Text", "JournalArticle", "conference_item", "Working paper")),
               rep(TRUE, 9))
  expect_equal(.repo_publication_type(c(NA, "")), c(NA, NA))
})
