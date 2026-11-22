test_that("exists", {
  expect_true(is.function(metacheck::psycharchives_links))
  expect_true(is.function(metacheck::psycharchives_info))
  expect_true(is.function(metacheck::psycharchives_file_download))
  expect_no_error(helplist <- help(psycharchives_links, metacheck))

  expect_error(psycharchives_links(bad_arg))
})

test_that("handle extraction", {
  h <- metacheck:::.psycharchives_handle
  expect_equal(h("https://hdl.handle.net/20.500.12034/17526"), "20.500.12034/17526")
  expect_equal(h("https://www.psycharchives.org/jspui/handle/20.500.12034/17526"),
               "20.500.12034/17526")
  expect_equal(h("20.500.12034/17526"), "20.500.12034/17526")
  expect_true(is.na(h("https://osf.io/abcde")))
})

test_that("psycharchives_links", {
  paper <- test_paper(url = "https://hdl.handle.net/20.500.12034/17526")
  links <- psycharchives_links(paper)
  expect_equal(nrow(links), 1)
  expect_equal(links$href[[1]], "https://hdl.handle.net/20.500.12034/17526")

  # also matches psycharchives.org item pages
  paper2 <- test_paper(url = "https://www.psycharchives.org/jspui/handle/20.500.12034/17526")
  expect_equal(nrow(psycharchives_links(paper2)), 1)
})

test_that("a PsychArchives DOI is found by the address it is registered with (#695)", {
  # 10.23668/psycharchives.12959 leads to handle 20.500.12034/8459 (DataCite,
  # 2026-11-19): the number in the DOI is not the handle. Mocked, so the test
  # needs no connection.
  addr <- "https://www.psycharchives.org/jspui/handle/20.500.12034/8459"
  reg <- function(doi) {
    url <- if (grepl("^10\\.23668/psycharchives\\.12959$", doi, ignore.case = TRUE)) addr else "https://example.org/x"
    list(url = url, ok = TRUE, publication = FALSE)
  }
  paper <- test_paper(c("Data are at https://doi.org/10.23668/psycharchives.12959.",
                        "Another DOI 10.23668/other.1 leads elsewhere."))
  links <- testthat::with_mocked_bindings(
    list(ds = dspace_links(paper), pa = psycharchives_links(paper)),
    .repo_doi_url = reg, .package = "metacheck")
  expect_equal(links$ds$href, addr)
  expect_equal(links$pa$href, addr)

  # also as a hyperlink
  paper <- test_paper(url = "https://doi.org/10.23668/psycharchives.12959")
  links <- testthat::with_mocked_bindings(dspace_links(paper), .repo_doi_url = reg, .package = "metacheck")
  expect_equal(unique(links$href), addr)

  # a DOI that could not be looked up gives no row
  links <- testthat::with_mocked_bindings(
    dspace_links(paper),
    .repo_doi_url = function(doi) list(url = NA_character_, ok = FALSE, publication = FALSE),
    .package = "metacheck")
  expect_equal(nrow(links), 0)
})

test_that(".psycharchives_info", {
  url <- "https://hdl.handle.net/20.500.12034/17526"
  info <- .psycharchives_info(url)

  expect_equal(info$pa_url, url)
  expect_match(info$PA_title, "International Cognitive Ability Resource")
  expect_match(info$PA_authors, "Doebler")
  # dc.rights is the access level; the licence is dc.rights.license
  expect_equal(info$PA_rights, "restrictedAccess")
  expect_equal(info$PA_license, "Terms for individual files apply")

  files <- info$files[[1]]
  # only public bitstreams are returned by the API (readme is public; the
  # restricted 1GB zip is not listed)
  expect_true("readme.txt" %in% files$name)
  expect_true(all(grepl("^https://www.psycharchives.org/rest/bitstreams/",
                        files$retrieve)))
}, "mock")

test_that("psycharchives_file_download", {
  testthat::local_mocked_bindings(
    online = \(...) TRUE
  )

  url <- "https://hdl.handle.net/20.500.12034/17526"
  files <- psycharchives_file_download(url)

  expect_true(is.data.frame(files))
  expect_true("readme.txt" %in% files$name)
  # deferred download: file_url set, file_location NA (fetched later by
  # download_repo_files())
  expect_true(all(!is.na(files$file_url)))
  expect_true(all(is.na(files$file_location)))
  expect_true("type" %in% names(files))

  # rights flag carried as an attribute (item is restrictedAccess), so the
  # module can warn about restricted files without a second API call
  rights <- attr(files, "rights")
  expect_equal(unname(rights), "restrictedAccess")
  expect_equal(names(rights), url)
  # the licence travels separately from the access level
  license <- attr(files, "license")
  expect_equal(unname(license), "Terms for individual files apply")
  expect_equal(names(license), url)
}, "mock")


# The test bodies below are wrapped in local(): helper.R's test_that()
# wrapper evaluates a test's code in the test FILE's environment, so a
# withr/testthat local_*() call made directly in it would stay in force
# until the end of the file instead of the end of the test.

test_that(".psycharchives_info reads metadata and bitstreams from one request", local({
  # /rest/handle/<handle>?expand=metadata,bitstreams returns the item, its
  # metadata and its bitstreams together (verified 2026-10-13; it used to
  # take three requests). The recorded response is used here.
  rest <- metacheck:::.psycharchives_rest
  calls <- character(0)
  local_mocked_bindings(.psycharchives_rest = function(path, host = "www.psycharchives.org") {
    calls <<- c(calls, path)
    rest(path, host)
  })
  info <- .psycharchives_info("https://hdl.handle.net/20.500.12034/17526")
  expect_equal(calls, "/handle/20.500.12034/17526?expand=metadata,bitstreams")
  expect_equal(info$files[[1]]$name, "readme.txt")
  expect_equal(info$PA_rights, "restrictedAccess")
}), "mock")

test_that(".psycharchives_info falls back to separate requests when expand is ignored", local({
  seen <- character(0)
  base <- "https://www.psycharchives.org/rest"
  httr2::local_mocked_responses(function(req) {
    seen <<- c(seen, req$url)
    # the shape a server that ignores `expand` sends: both fields null
    if (startsWith(req$url, paste0(base, "/handle/")))
      return(httr2::response(
        headers = list(`Content-Type` = "application/json"),
        body = charToRaw('{"uuid":"u-1","name":"An item","metadata":null,"bitstreams":null}')))
    if (req$url == paste0(base, "/items/u-1?expand=metadata"))
      return(httr2::response_json(body = list(metadata = list(
        list(key = "dc.rights", value = "openAccess"),
        list(key = "dc.rights.uri",
             value = "https://creativecommons.org/licenses/by/4.0/")))))
    if (req$url == paste0(base, "/items/u-1/bitstreams?limit=1000"))
      return(httr2::response_json(body = list(list(
        name = "data.csv", sizeBytes = 10, retrieveLink = "/rest/bitstreams/b-1/retrieve"))))
    httr2::response(status_code = 404)
  })
  info <- .psycharchives_info("https://hdl.handle.net/20.500.12034/1")
  expect_length(seen, 3)
  expect_equal(info$PA_rights, "openAccess")
  # no dc.rights.license, so the licence address is used
  expect_equal(info$PA_license, "https://creativecommons.org/licenses/by/4.0/")
  expect_equal(info$files[[1]]$retrieve, "https://www.psycharchives.org/rest/bitstreams/b-1/retrieve")
}))

test_that(".psycharchives_info accepts a DSpace 5 item, which has an id instead of a uuid", local({
  # datastorre.stir.ac.uk runs DSpace 5 (verified 2026-10-13): items carry a
  # numeric `id` and no `uuid`, and retrieveLink is given from the REST root
  # ("/bitstreams/<id>/retrieve"), which only answers under /rest.
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(
      id = 392, name = "Model output", type = "item",
      metadata = list(list(key = "dc.rights", value = "CC-BY 4.0")),
      bitstreams = list(list(name = "Table S1.docx", sizeBytes = 116140,
                             bundleName = "ORIGINAL",
                             retrieveLink = "/bitstreams/2030/retrieve"))))
  })
  info <- .psycharchives_info("https://datastorre.stir.ac.uk/handle/11667/141")
  expect_false("error" %in% names(info))
  expect_equal(info$PA_title, "Model output")
  # here dc.rights holds the licence itself, not an access level
  expect_equal(info$PA_license, "CC-BY 4.0")
  expect_equal(info$files[[1]]$retrieve,
               "https://datastorre.stir.ac.uk/rest/bitstreams/2030/retrieve")
}))

test_that(".psycharchives_info lists only the files the author deposited", local({
  # DSpace adds its own bundles next to ORIGINAL: LICENSE (the deposit
  # agreement), TEXT (text extracted from PDFs) and THUMBNAIL (previews).
  # Only ORIGINAL is what the author shared (as for DSpace 7); a bitstream
  # without a bundleName is kept.
  httr2::local_mocked_responses(function(req) {
    bs <- function(name, bundle) {
      b <- list(name = name, sizeBytes = 1,
                retrieveLink = paste0("/rest/bitstreams/", name, "/retrieve"))
      if (!is.na(bundle)) b$bundleName <- bundle
      b
    }
    httr2::response_json(body = list(
      uuid = "u-2", name = "An item",
      metadata = list(list(key = "dc.rights", value = "openAccess")),
      bitstreams = list(bs("data.csv", "ORIGINAL"), bs("license.txt", "LICENSE"),
                        bs("paper.pdf.txt", "TEXT"), bs("paper.pdf.jpg", "THUMBNAIL"),
                        bs("analysis.R", NA))))
  })
  info <- .psycharchives_info("https://hdl.handle.net/20.500.12034/2")
  expect_equal(info$files[[1]]$name, c("data.csv", "analysis.R"))
  # an access term in dc.rights is never reported as the licence
  expect_true(is.na(info$PA_license))
}))
