test_that(".dataone_pid handles a DOI suffix containing a further slash", {
  # Regression test: PISCO (data.piscoweb.org, DOI prefix 10.6085) mints
  # DataONE PIDs whose suffix itself contains a slash
  # (10.6085/AA/marine_ltm.20.1, confirmed live to be a real, resolving
  # DOI). The bare-DOI branch of .dataone_pid() used
  # [A-Za-z0-9._-]+ for the suffix, which cannot match across a "/" and so
  # silently returned NA for this whole class of PID -- found via a real
  # paper citing exactly this DOI with no host domain in the URL at all.
  expect_true(is.function(metacheck:::.dataone_pid))

  expect_equal(
    metacheck:::.dataone_pid("https://doi.org/10.6085/AA/marine_ltm.20.1"),
    "doi:10.6085/AA/marine_ltm.20.1"
  )
  expect_equal(
    metacheck:::.dataone_pid("10.6085/AA/marine_ltm.20.1"),
    "doi:10.6085/AA/marine_ltm.20.1"
  )
  # Existing single-segment-suffix shapes still work (no regression).
  expect_equal(
    metacheck:::.dataone_pid("https://doi.org/10.18739/A2GT5FG86"),
    "doi:10.18739/A2GT5FG86"
  )
  expect_equal(
    metacheck:::.dataone_pid("10.5063/PG1Q4B"),
    "doi:10.5063/PG1Q4B"
  )
  # A "doi:" marker in a landing-page URL still takes priority over the
  # bare-DOI branch, and also allows a slash in its own suffix.
  expect_equal(
    metacheck:::.dataone_pid("https://arcticdata.io/metacat/d1/mn/v2/view/doi:10.18739/A2GT5FG86"),
    "doi:10.18739/A2GT5FG86"
  )
})

test_that("dataone_links recognises the data.piscoweb.org host and its DOI prefix", {
  # Regression test: data.piscoweb.org (PISCO) added to .dataone_hosts()
  # 2026-09-19, confirmed live to run the standard Metacat API at
  # /metacat/d1/mn/v2/ (real DataONE XML node-capabilities response) and
  # to serve real systemMetadata for a real dataset under its own
  # registered DOI prefix, 10.6085.
  expect_true(is.function(metacheck::dataone_links))

  hosts <- metacheck:::.dataone_hosts()
  pisco <- Filter(function(h) identical(h$host, "data.piscoweb.org"), hosts)
  expect_length(pisco, 1)
  expect_equal(pisco[[1]]$doi_prefix, "10.6085")

  paper <- test_paper(
    text = "uploaded to DataONE and are accessible here: https://doi.org/10.6085/AA/marine_ltm.20.1"
  )
  links <- dataone_links(paper)

  expect_equal(nrow(links), 1)
  expect_equal(unname(links$dataone_host), "data.piscoweb.org")
  expect_equal(unname(links$dataone_pid), "doi:10.6085/AA/marine_ltm.20.1")
})


test_that(".dataone_info sizes no-<physical> files with one Solr query, HEAD only as fallback", local({
  # Regression test (2026-10-13 survey): an EML entity with no <physical>
  # element used to cost one HEAD request per file. One Solr query on the
  # member node now returns every documented object's size at once; the HEAD
  # request is kept only for a file the query did not answer for.
  eml <- paste0(
    '<eml:eml xmlns:eml="https://eml.ecoinformatics.org/eml-2.2.0"><dataset>',
    '<title>T</title>',
    '<otherEntity id="urn-uuid-aaa"><entityName>a.csv</entityName></otherEntity>',
    '<otherEntity id="urn-uuid-bbb"><entityName>b.csv</entityName></otherEntity>',
    '<dataTable><physical><objectName>c.csv</objectName><size>5</size>',
    '<distribution><online><url>https://cn.dataone.org/cn/v2/resolve/urn%3Auuid%3Accc</url>',
    '</online></distribution></physical></dataTable>',
    '</dataset></eml:eml>')

  solr_calls <- list()
  head_calls <- character(0)
  testthat::local_mocked_bindings(
    .batch_query = function(urls, ...) {
      list(httr2::response(status_code = 200, body = charToRaw(eml)))
    },
    .dataone_package_sizes = function(host, api_base, pid) {
      solr_calls[[length(solr_calls) + 1]] <<- c(host, api_base, pid)
      # urn:uuid:bbb deliberately missing from the reply
      c("doi:10.5063/F1TEST" = 999, "urn:uuid:aaa" = 10)
    },
    .dataone_object_size = function(host, api_base, pid) {
      head_calls <<- c(head_calls, pid)
      20
    }
  )

  info <- metacheck:::.dataone_info("doi:10.5063/F1TEST",
                                    host = "knb.ecoinformatics.org")
  files <- info$files[[1]]
  sizes <- stats::setNames(vapply(files, \(f) f$size, numeric(1)),
                           vapply(files, \(f) f$key, character(1)))

  expect_equal(sizes, c(a.csv = 10, b.csv = 20, c.csv = 5))
  expect_equal(solr_calls, list(c("knb.ecoinformatics.org", "/knb/d1/mn/v2/",
                                  "doi:10.5063/F1TEST")))
  expect_equal(head_calls, "urn:uuid:bbb")
}))


test_that(".dataone_package_sizes builds the Solr query and reads its reply", local({
  seen <- NULL
  reply <- list(response = list(numFound = 3, docs = list(
    list(id = "doi:10.5063/F11V5CFN", fileName = "meta.xml", size = 7093),
    list(id = "urn:uuid:760a24a6-79e6-46d3-a70d-738d8f1355d7",
         fileName = "Seed_tracking_data.csv", size = 213713),
    list(id = "urn:uuid:d0f4d60c-70a2-44b0-91c9-9a694ba4d27b",
         fileName = "Trapping_data.csv", size = 136236))))
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      seen <<- req$url
      httr2::response(status_code = 200,
                      headers = list(`Content-Type` = "application/json"),
                      body = charToRaw(jsonlite::toJSON(reply, auto_unbox = TRUE)))
    },
    .package = "httr2"
  )

  sizes <- metacheck:::.dataone_package_sizes(
    "knb.ecoinformatics.org", "/knb/d1/mn/v2/", "doi:10.5063/F11V5CFN")
  expect_equal(attr(sizes, "file_names"), c("meta.xml", "Seed_tracking_data.csv", "Trapping_data.csv"))

  # KNB labels its reply text/json
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(status_code = 200,
                      headers = list(`Content-Type` = "text/json"),
                      body = charToRaw(jsonlite::toJSON(reply, auto_unbox = TRUE)))
    },
    .package = "httr2"
  )
  expect_length(metacheck:::.dataone_package_sizes("h", "/x/", "doi:1"), 3)

  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      seen <<- req$url
      httr2::response(status_code = 200,
                      headers = list(`Content-Type` = "application/json"),
                      body = charToRaw(jsonlite::toJSON(reply, auto_unbox = TRUE)))
    },
    .package = "httr2"
  )
  sizes <- metacheck:::.dataone_package_sizes(
    "knb.ecoinformatics.org", "/knb/d1/mn/v2/", "doi:10.5063/F11V5CFN")
  expect_equal(seen, paste0(
    "https://knb.ecoinformatics.org/knb/d1/mn/v2/query/solr/",
    "?q=isDocumentedBy%3A%22doi%3A10.5063%2FF11V5CFN%22",
    "&fl=id,fileName,size&rows=1000&wt=json"))
  expect_equal(sizes[["urn:uuid:760a24a6-79e6-46d3-a70d-738d8f1355d7"]], 213713)
  expect_equal(sizes[["urn:uuid:d0f4d60c-70a2-44b0-91c9-9a694ba4d27b"]], 136236)

  # a node without the endpoint (404) or with no connection gives numeric(0)
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) httr2::response(status_code = 404),
    .package = "httr2"
  )
  expect_equal(metacheck:::.dataone_package_sizes("h", "/x/", "doi:1"), numeric(0))
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) stop("connection refused"),
    .package = "httr2"
  )
  expect_equal(metacheck:::.dataone_package_sizes("h", "/x/", "doi:1"), numeric(0))
}))

test_that(".dataone_info keeps the slashes of a file's PID", local({
  # data.piscoweb.org doi:10.6085/AA/marine_ltm.20.1: cut at its last slash, the
  # PID gave 404
  eml <- paste0(
    '<eml:eml xmlns:eml="https://eml.ecoinformatics.org/eml-2.2.0"><dataset><title>T</title>',
    '<dataTable><physical><objectName>a.csv</objectName><size>5</size><distribution><online>',
    '<url>https://cn.dataone.org/cn/v2/resolve/doi:10.6085/AA/marine_ltm.21.1</url>',
    '</online></distribution></physical></dataTable>',
    '<dataTable><physical><objectName>b.csv</objectName><size>6</size><distribution><online>',
    '<url>ecogrid://knb/knb.1403.1</url>',
    '</online></distribution></physical></dataTable>',
    '</dataset></eml:eml>')
  testthat::local_mocked_bindings(
    .batch_query = function(urls, ...) list(httr2::response(status_code = 200, body = charToRaw(eml))),
    .dataone_package_sizes = function(...) numeric(0))
  files <- metacheck:::.dataone_info("doi:1", host = "knb.ecoinformatics.org")$files[[1]]
  expect_equal(files[[1]]$pid, "doi:10.6085/AA/marine_ltm.21.1")
  expect_equal(files[[2]]$pid, "knb.1403.1")
}))

test_that(".dataone_info finds a file with no usable id by its file name", local({
  # KNB doi:10.5063/F1VD6WMF: a <physical> with no url whose name has a folder,
  # and the Solr reply has the object under the bare file name.
  eml <- paste0(
    '<eml:eml xmlns:eml="https://eml.ecoinformatics.org/eml-2.2.0"><dataset><title>T</title>',
    '<dataTable><physical><objectName>Results/trait_data.csv</objectName><size/></physical></dataTable>',
    '<otherEntity id="f7a2"><entityName>Results/twice.csv</entityName></otherEntity>',
    '<otherEntity id="f7a3"><entityName>absent.csv</entityName></otherEntity>',
    '</dataset></eml:eml>')
  testthat::local_mocked_bindings(
    .batch_query = function(urls, ...) list(httr2::response(status_code = 200, body = charToRaw(eml))),
    .dataone_package_sizes = function(host, api_base, pid) {
      out <- c("doi:1" = 7, "urn:uuid:aaa" = 134508, "urn:uuid:b1" = 1, "urn:uuid:b2" = 2)
      attr(out, "file_names") <- c("meta.xml", "trait_data.csv", "twice.csv", "twice.csv")
      out
    },
    .dataone_object_size = function(host, api_base, pid) stop("must not be called")
  )
  files <- metacheck:::.dataone_info("doi:1", host = "knb.ecoinformatics.org")$files[[1]]
  by_key <- stats::setNames(files, vapply(files, \(f) f$key, character(1)))

  expect_equal(by_key[["Results/trait_data.csv"]]$pid, "urn:uuid:aaa")
  expect_equal(by_key[["Results/trait_data.csv"]]$size, 134508)
  # two objects with that name: not guessed
  expect_true(is.na(by_key[["Results/twice.csv"]]$pid))
  expect_true(is.na(by_key[["absent.csv"]]$pid))
}))

test_that(".dataone_info asks for a DOI in upper case after a 404 (#476)", local({
  # Verified 2026-10-17: KNB answers 404 for doi:10.5063/f11z42jx and 200 for
  # doi:10.5063/F11Z42JX.
  eml <- paste0('<eml:eml xmlns:eml="https://eml.ecoinformatics.org/eml-2.2.0">',
                '<dataset><title>Kedougou Nutrient Diversity</title></dataset></eml:eml>')
  seen <- character(0)
  stored <- "doi%3A10.5063%2FF11Z42JX"
  testthat::local_mocked_bindings(
    .batch_query = function(urls, ...) {
      seen <<- c(seen, urls)
      list(if (endsWith(urls, stored))
        httr2::response(status_code = 200, body = charToRaw(eml))
      else httr2::response(status_code = 404))
    },
    .dataone_package_sizes = function(...) numeric(0)
  )

  info <- metacheck:::.dataone_info("doi:10.5063/f11z42jx", host = "knb.ecoinformatics.org")
  expect_equal(info$title, "Kedougou Nutrient Diversity")
  # the identifier as given stays the key dataone_info() joins on
  expect_equal(info$dataone_pid, "doi:10.5063/f11z42jx")
  expect_equal(info$doi, "doi:10.5063/F11Z42JX")
  expect_length(seen, 2)

  # found as written: no second request
  seen <- character(0)
  info <- metacheck:::.dataone_info("doi:10.5063/F11Z42JX", host = "knb.ecoinformatics.org")
  expect_equal(info$title, "Kedougou Nutrient Diversity")
  expect_length(seen, 1)

  # not found in either case: still unfound, after two requests
  seen <- character(0)
  expect_warning(info <- metacheck:::.dataone_info("doi:10.5063/nope", host = "knb.ecoinformatics.org"),
                 "could not be found")
  expect_equal(info$error, "unfound")
  expect_length(seen, 2)

  # an identifier that is not a DOI is never changed
  seen <- character(0)
  expect_warning(metacheck:::.dataone_info("urn:uuid:abc", host = "knb.ecoinformatics.org"))
  expect_length(seen, 1)
}))

test_that("DataONE: creators without a name part, a PID's final full stop, one fetch per dataset (U35, U40)", local({
  # a surname-only creator was "NA Smith"; "doi:10.5063/PG1Q4B." kept the
  # full stop; two URLs for one dataset fetched it twice and gave 4 rows
  eml <- paste0(
    '<eml:eml xmlns:eml="https://eml.ecoinformatics.org/eml-2.2.0"><dataset>',
    '<title>T</title>',
    '<creator><individualName><surName>Smith</surName></individualName></creator>',
    '<creator><organizationName>Arctic Lab</organizationName></creator>',
    '<creator><individualName><givenName>Ann</givenName><surName>Lee</surName></individualName></creator>',
    '</dataset></eml:eml>')
  testthat::local_mocked_bindings(
    .batch_query = function(urls, ...) list(httr2::response(status_code = 200, body = charToRaw(eml))),
    .dataone_package_sizes = function(...) numeric(0))
  info <- metacheck:::.dataone_info("doi:10.5063/F1TEST", host = "knb.ecoinformatics.org")
  expect_equal(info$authors[[1]], c("Smith", "Arctic Lab", "Ann Lee"))

  expect_equal(metacheck:::.dataone_pid("doi:10.5063/PG1Q4B."), "doi:10.5063/PG1Q4B")

  calls <- 0
  testthat::local_mocked_bindings(
    online = function(...) TRUE,
    .dataone_info = function(pid, host, pb = NULL) {
      calls <<- calls + 1
      data.frame(dataone_host = host, dataone_pid = pid, title = "T")
    })
  res <- dataone_info(c("https://knb.ecoinformatics.org/view/doi:10.5063/PG1Q4B",
                        "https://knb.ecoinformatics.org/view/doi:10.5063/PG1Q4B."))
  expect_equal(calls, 1)
  expect_equal(nrow(res), 2)
}))
