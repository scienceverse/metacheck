test_that("dataverse_links", {
  expect_true(is.function(metacheck::dataverse_links))
  expect_no_error(helplist <- help(dataverse_links, metacheck))

  expect_error(dataverse_links(bad_arg))

  paper <- test_paper(url = c(
    "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ABC123",
    "https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/XYZ999",
    "https://osf.io/abcde"
  ))

  links <- dataverse_links(paper)

  expect_equal(nrow(links), 2)
  expect_equal(links$href, c(
    "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ABC123",
    "https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/XYZ999"
  ))
  expect_equal(links$dataverse_host, c("dataverse.harvard.edu", "dataverse.nl"))
  expect_equal(links$dataverse_doi, c("10.7910/DVN/ABC123", "10.34894/XYZ999"))
})


test_that(".dataverse_parse", {
  expect_true(is.function(metacheck:::.dataverse_parse))

  url <- c(
    "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ABC123",
    "https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/XYZ999",
    "https://dataverse.harvard.edu/dataset.xhtml?id=12345", # no DOI
    "https://notdataverse.com/foo",                          # not a known host
    NA_character_
  )

  parsed <- .dataverse_parse(url)

  expect_equal(parsed$host, c("dataverse.harvard.edu", "dataverse.nl",
                              "dataverse.harvard.edu", NA, NA))
  expect_equal(parsed$doi, c("10.7910/DVN/ABC123", "10.34894/XYZ999", NA, NA, NA))

  # NULL / empty
  expect_equal(nrow(.dataverse_parse(character(0))), 0)
})

test_that(".dataverse_parse gives each URL its own host (U31, U155)", {
  # a URL without a host took the next URL's host, and dataverse.no matched
  # the start of dataverse.northwestern.edu
  parsed <- .dataverse_parse(c(
    "https://doi.org/10.18167/DVN1/T0DMFJ",
    "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ABC123",
    "https://dataverse.northwestern.edu/dataset.xhtml?persistentId=doi:10.18131/g3/XYZ"))
  expect_equal(parsed$host, c("dataverse.cirad.fr", "dataverse.harvard.edu", NA))
})


test_that(".dataverse_info", {
  expect_true(is.function(metacheck:::.dataverse_info))

  testthat::local_mocked_bindings(
    .batch_query = function(urls, msg = NULL, req_func = identity) {
      list(structure(list(
        status = 200,
        body = charToRaw(jsonlite::toJSON(list(
          status = "OK",
          data = list(
            persistentUrl = "https://doi.org/10.7910/DVN/ABC123",
            publicationDate = "2020-01-01",
            latestVersion = list(
              releaseTime = "2020-01-01T00:00:00Z",
              lastUpdateTime = "2020-01-02T00:00:00Z",
              license = list(name = "CC0 1.0"),
              metadataBlocks = list(citation = list(fields = list(
                list(typeName = "title", value = "Example Dataset"),
                list(typeName = "author", value = list(
                  list(authorName = list(value = "Doe, Jane"))
                ))
              ))),
              files = list(
                list(
                  label = "data.csv",
                  dataFile = list(
                    id = 999, filename = "data.csv", filesize = 1234,
                    checksum = list(`type` = "MD5", value = "abc123")
                  )
                )
              )
            )
          )
        ), auto_unbox = TRUE))
      ), class = "httr2_response"))
    }
  )
  testthat::local_mocked_bindings(
    resp_status = function(resp) resp$status,
    resp_body_json = function(resp) jsonlite::fromJSON(rawToChar(resp$body), simplifyVector = FALSE),
    .package = "httr2"
  )

  info <- .dataverse_info("dataverse.harvard.edu", "10.7910/DVN/ABC123")

  expect_equal(info$dataverse_host, "dataverse.harvard.edu")
  expect_equal(info$dataverse_doi, "10.7910/DVN/ABC123")
  expect_equal(info$title, "Example Dataset")
  expect_equal(info$license, "CC0 1.0")
  expect_equal(info$authors[[1]], "Doe, Jane")
  expect_equal(length(info$files[[1]]), 1)
})

test_that(".dataverse_info reads a plain-string licence and refuses a string data (U34)", {
  # an older installation sends the licence as a string: $name on it stopped
  # dataverse_info() for every dataset in the call
  answer <- NULL
  testthat::local_mocked_bindings(
    .batch_query = function(urls, msg = NULL, req_func = identity) {
      list(structure(list(status = 200, body = charToRaw(
        jsonlite::toJSON(answer, auto_unbox = TRUE))), class = "httr2_response"))
    }
  )
  testthat::local_mocked_bindings(
    resp_status = function(resp) resp$status,
    resp_body_json = function(resp) jsonlite::fromJSON(rawToChar(resp$body), simplifyVector = FALSE),
    .package = "httr2"
  )
  answer <- list(status = "OK", data = list(
    persistentUrl = "https://doi.org/10.7910/DVN/ABC123",
    latestVersion = list(license = "CC0 1.0")))
  info <- .dataverse_info("dataverse.harvard.edu", "10.7910/DVN/ABC123")
  expect_equal(info$license, "CC0 1.0")
  expect_null(info$error)

  answer <- list(status = "OK", data = "not a dataset")
  info <- .dataverse_info("dataverse.harvard.edu", "10.7910/DVN/ABC123")
  expect_equal(info$error, "parse_error")
})

test_that("a persistentId that decodes to invalid UTF-8 gives no DOI (U39)", {
  # it stopped the parse for every URL in the call, and the bare-DOI fallback
  # then cut the DOI at its "%" ("10.7910/DVN/AB")
  urls <- c("https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GGDUND",
            "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/AB%FFCD",
            "https://doi.org/10.18167/DVN1/T0DMFJ")
  p <- .dataverse_parse(urls)
  expect_equal(p$doi, c("10.7910/DVN/GGDUND", NA, "10.18167/DVN1/T0DMFJ"))
})


test_that(".dataverse_info reports not-found", {
  testthat::local_mocked_bindings(
    .batch_query = function(urls, msg = NULL, req_func = identity) {
      list(structure(list(status = 404), class = "httr2_response"))
    }
  )
  testthat::local_mocked_bindings(
    resp_status = function(resp) resp$status,
    .package = "httr2"
  )

  expect_warning(
    info <- .dataverse_info("dataverse.harvard.edu", "10.7910/DVN/NOPE"),
    "could not be found"
  )
  expect_equal(info$error, "unfound")
})


test_that("dataverse_links recognises a bare DOI with no host domain in the URL", {
  # Regression test: repo_check missed CIRAD Dataverse deposits cited as a
  # bare DOI ("https://doi.org/10.18167/DVN1/T0DMFJ", no "dataverse.cirad.fr"
  # string anywhere) even though dataverse.cirad.fr is already a known host
  # -- confirmed live against a real paper's Data Availability Statement.
  # 10.18167 was individually confirmed (2026-09-19) via DataCite's own DOI
  # index to belong to dataverse.cirad.fr specifically -- see
  # .dataverse_doi_prefix_hosts()'s own header comment for the full method.
  # A bare DOI citation, like the real one this test guards against, never
  # arrives as a clickable hyperlink in the `url` table (see .dataverse_
  # doi_prefix_hosts()'s own header comment) -- it is body text the source
  # PDF/HTML never made into a link, so it must be given as `text`, the
  # same way a real Data Availability Statement is, not as `url`.
  paper <- test_paper(text = c(
    "Data are available on the CIRAD Dataverse: https://doi.org/10.18167/DVN1/T0DMFJ.",
    "Also see https://doi.org/10.9999/unrelated123 for something unrelated."
  ))

  links <- dataverse_links(paper)

  expect_equal(nrow(links), 1)
  expect_equal(unname(links$dataverse_host), "dataverse.cirad.fr")
  expect_equal(unname(links$dataverse_doi), "10.18167/DVN1/T0DMFJ")
})


test_that(".dataverse_host_from_doi resolves a host from a verified prefix only", {
  expect_true(is.function(metacheck:::.dataverse_host_from_doi))

  expect_equal(
    metacheck:::.dataverse_host_from_doi("10.18167/DVN1/T0DMFJ"),
    "dataverse.cirad.fr"
  )
  expect_equal(
    metacheck:::.dataverse_host_from_doi("10.7910/DVN/ABC123"),
    "dataverse.harvard.edu"
  )
  # An unverified prefix must not resolve to any host -- the allowlist is
  # explicit and individually confirmed, not a general DOI-prefix guess
  # (the file's own top-of-file comment explains why that distinction
  # matters for Dataverse specifically, unlike Dryad's single-host case).
  expect_true(is.na(metacheck:::.dataverse_host_from_doi("10.9999/unrelated123")))
})


test_that("dataverse_info() does not crash on dataverse_links()'s own output for an unfound DOI", {
  # Regression test: dataverse_info(dataverse_links(paper)) -- the
  # documented, normal usage -- crashed with "Join columns in `x` must be
  # present in the data" whenever the dataset wasn't found.
  # dataverse_links()'s output already carries dataverse_host/dataverse_doi
  # columns; dataverse_info() independently recomputed its own `ids`
  # versions of both and left-joined them onto the table without dropping
  # the caller's existing columns first, producing .x/.y-suffixed
  # duplicates that broke the SECOND join further down (by =
  # c("dataverse_host", "dataverse_doi")) -- confirmed live 2026-09-19, and
  # the same shape in 6 other archive-*.R files (see their own test files).
  testthat::local_mocked_bindings(
    .batch_query = function(urls, msg = NULL, req_func = identity) {
      list(structure(list(status = 404), class = "httr2_response"))
    }
  )
  testthat::local_mocked_bindings(
    resp_status = function(resp) resp$status,
    .package = "httr2"
  )

  paper <- test_paper(url =
    "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NOPE")
  links <- dataverse_links(paper)
  expect_no_error(suppressWarnings(dataverse_info(links)))
})


test_that("dataverse_pat stores per-host tokens", {
  withr::local_options(
    metacheck.dataverse.pat.DATAVERSE_HARVARD_EDU = NULL,
    metacheck.dataverse.pat.DATAVERSE_NL = NULL
  )
  withr::local_envvar(
    DATAVERSE_PAT_DATAVERSE_HARVARD_EDU = "",
    DATAVERSE_PAT_DATAVERSE_NL = ""
  )

  expect_equal(dataverse_pat("dataverse.harvard.edu"), "")

  dataverse_pat("dataverse.harvard.edu", "hvd-token")
  expect_equal(dataverse_pat("dataverse.harvard.edu"), "hvd-token")
  # a different host's token is untouched
  expect_equal(dataverse_pat("dataverse.nl"), "")

  expect_error(dataverse_pat("dataverse.harvard.edu", 123))
})


test_that("dataverse_file_download", {
  expect_true(is.function(metacheck::dataverse_file_download))
  expect_no_error(helplist <- help(dataverse_file_download, metacheck))

  expect_null(dataverse_file_download(NA_character_, NA_character_))

  testthat::local_mocked_bindings(
    .dataverse_info = function(host, doi, pb = NULL) {
      data.frame(
        dataverse_host = host, dataverse_doi = doi,
        files = I(list(list(
          list(label = "small.csv", dataFile = list(
            id = 1, filename = "small.csv", filesize = 100,
            checksum = list(type = "MD5", value = "small")
          )),
          list(label = "big.bin", dataFile = list(
            id = 2, filename = "big.bin", filesize = 12 * 1024 * 1024,
            checksum = list(type = "MD5", value = "big")
          ))
        )))
      )
    }
  )

  tmpdir <- withr::local_tempdir()

  # small.csv has no real network target for this fake DOI, so the download
  # itself fails (as opposed to big.bin, which is dropped before any attempt
  # by the max_file_size filter) -- the "did not arrive intact" warning is
  # the function correctly reporting that real, expected failure.
  expect_warning(
    dl <- dataverse_file_download(
      "dataverse.harvard.edu", "10.7910/DVN/ABC123",
      download_to = tmpdir,
      max_file_size = 10
    ),
    "did not arrive intact"
  )

  # big.bin is over max_file_size: it stays in the table, not downloaded, as
  # documented (it was dropped, U36)
  expect_equal(nrow(dl), 2)
  expect_equal(unique(dl$dataverse_doi), "10.7910/DVN/ABC123")
  expect_equal(dl$key, c("small.csv", "big.bin"))
  expect_equal(dl$downloaded, c(FALSE, FALSE))

  folder <- file.path(tmpdir, "10.7910_DVN_ABC123")
  expect_true(dir.exists(folder))
  expect_equal(list.files(folder), character(0))

  # vectorised: multiple datasets. Each DOI fails its own download and warns
  # separately (the recursive per-pair call in dataverse_file_download()), so
  # expect_warning() alone would only catch the first and leave the second
  # uncaptured; withCallingHandlers asserts on every warning that occurs.
  withCallingHandlers(
    dl2 <- dataverse_file_download(
      host = c("dataverse.harvard.edu", "dataverse.nl"),
      doi = c("10.7910/DVN/ABC123", "10.34894/XYZ999"),
      download_to = tmpdir,
      max_file_size = 10
    ),
    warning = function(w) {
      expect_match(conditionMessage(w), "did not arrive intact")
      invokeRestart("muffleWarning")
    }
  )
  expect_setequal(dl2$dataverse_doi, c("10.7910/DVN/ABC123", "10.34894/XYZ999"))
  expect_true(all(dl2$downloaded == FALSE))

  # successful download path
  testthat::local_mocked_bindings(
    .dataverse_info = function(host, doi, pb = NULL) {
      data.frame(
        dataverse_host = host, dataverse_doi = doi,
        files = I(list(list(list(
          label = "ok.csv",
          dataFile = list(
            id = 3, filename = "ok.csv",
            # The mocked req_perform() below serves exactly "x,y\n1,2\n", so
            # the fixture has to declare that file's real length.
            filesize = nchar("x,y\n1,2\n"),
            # Not a real MD5, so the checksum comparison is skipped and the
            # size check stands on its own.
            checksum = list(type = "MD5", value = "notreal")
          )
        ))))
      )
    }
  )
  testthat::local_mocked_bindings(
    request = function(url) structure(list(url = url), class = "httr2_request"),
    req_headers = function(req, ...) req,
    req_timeout = function(req, seconds) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) {
      structure(list(status = 200, body = charToRaw("x,y\n1,2\n")), class = "httr2_response")
    },
    resp_status = function(resp) resp$status,
    resp_body_raw = function(resp) resp$body,
    .package = "httr2"
  )

  tmpdir_ok <- withr::local_tempdir()
  dl_ok <- dataverse_file_download(
    "dataverse.harvard.edu", "10.7910/DVN/OK000",
    download_to = tmpdir_ok,
    max_file_size = 10
  )

  expect_equal(nrow(dl_ok), 1)
  expect_true(dl_ok$downloaded[[1]])
  expect_true(file.exists(file.path(tmpdir_ok, "10.7910_DVN_OK000", "ok.csv")))
})

test_that("dataverse_file_download: a zero-byte file, and every file over the limit (U36)", {
  one_file <- function(size) function(host, doi, pb = NULL) data.frame(
    dataverse_host = host, dataverse_doi = doi,
    files = I(list(list(list(label = "empty.txt", dataFile = list(
      id = 5, filename = "empty.txt", filesize = size,
      checksum = list(type = "MD5", value = "notreal")))))))
  testthat::local_mocked_bindings(.dataverse_info = one_file(0))
  testthat::local_mocked_bindings(
    request = function(url) structure(list(url = url), class = "httr2_request"),
    req_headers = function(req, ...) req,
    req_timeout = function(req, seconds) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) {
      structure(list(status = 200, body = raw(0)), class = "httr2_response")
    },
    resp_status = function(resp) resp$status,
    .package = "httr2"
  )
  # resp_body_raw() refuses an empty body, which stopped the whole download
  tmp <- withr::local_tempdir()
  dl <- dataverse_file_download("dataverse.harvard.edu", "10.7910/DVN/EMPTY0",
                                download_to = tmp, max_file_size = 10)
  expect_true(dl$downloaded)

  # every file over the limit: listed with downloaded = FALSE and no folder,
  # instead of NULL
  testthat::local_mocked_bindings(.dataverse_info = one_file(50 * 1024 * 1024))
  tmp2 <- withr::local_tempdir()
  dl <- dataverse_file_download("dataverse.harvard.edu", "10.7910/DVN/BIG000",
                                download_to = tmp2, max_file_size = 10)
  expect_equal(dl$key, "empty.txt")
  expect_false(dl$downloaded)
  expect_true(is.na(dl$folder))
  expect_length(list.files(tmp2), 0)
})

# A dataset holding one large zip, for the unzip_types tests below. Mirrors the
# equivalent Zenodo fixture: far larger than max_file_size, which is the case
# that matters -- without the exemption in the size filter it would be
# discarded before anything could be read out of it.
.zip_dataset_info <- function(host, doi, pb = NULL) {
  data.frame(
    dataverse_host = host, dataverse_doi = doi,
    files = I(list(list(
      list(label = "bundle.zip", dataFile = list(
        id = 4, filename = "bundle.zip", filesize = 130 * 1024 * 1024,
        checksum = list(type = "MD5", value = "zzz")
      ))
    )))
  )
}

test_that("dataverse_file_download extracts only wanted members with unzip_types", {
  requested <- character(0)
  testthat::local_mocked_bindings(
    .dataverse_info = .zip_dataset_info,
    .dataverse_zip_members = function(url, dest, keep_types, max_file_size,
                                      size = NULL) {
      requested <<- c(requested, url)
      expect_equal(keep_types, "data")
      # the listed filesize is handed on, so zip_peek() needs no HEAD request
      expect_equal(size, 130 * 1024 * 1024)
      writeLines("id,x\n1,2", file.path(dest, "study.csv"))
      data.frame(name = "study.csv", path = file.path(dest, "study.csv"),
                 size = 9, ok = TRUE)
    }
  )

  tmp <- withr::local_tempdir()
  dl <- dataverse_file_download("dataverse.harvard.edu", "10.7910/DVN/ZIP001",
                                download_to = tmp,
                                unzip_types = "data", max_file_size = 10)

  expect_equal(nrow(dl), 1)
  expect_equal(dl$key, "bundle.zip")
  expect_true(dl$downloaded[[1]])
  expect_equal(dl$extracted[[1]], 1)
  expect_equal(requested, "https://dataverse.harvard.edu/api/access/datafile/4")
  expect_true(file.exists(file.path(tmp, "10.7910_DVN_ZIP001", "study.csv")))
  expect_true(is.na(dl$size_on_disk[[1]]))
})

test_that("dataverse_file_download falls back to a whole download when the zip cannot be read", {
  testthat::local_mocked_bindings(
    .dataverse_info = .zip_dataset_info,
    .dataverse_zip_members = function(url, dest, keep_types, max_file_size,
                                      size = NULL) NULL
  )
  testthat::local_mocked_bindings(
    request = function(url) structure(list(url = url), class = "httr2_request"),
    req_headers = function(req, ...) req,
    req_timeout = function(req, seconds) req,
    req_error = function(req, is_error) req,
    req_perform = function(req, path = NULL) {
      structure(list(status = 200, body = charToRaw("PK-not-a-real-zip")),
                class = "httr2_response")
    },
    resp_status = function(resp) resp$status,
    resp_body_raw = function(resp) resp$body,
    .package = "httr2"
  )

  tmp <- withr::local_tempdir()
  dl <- suppressWarnings(
    dataverse_file_download("dataverse.harvard.edu", "10.7910/DVN/ZIP002",
                            download_to = tmp,
                            unzip_types = "data", max_file_size = 10))

  expect_true(is.na(dl$extracted[[1]]))
  expect_true(file.exists(file.path(tmp, "10.7910_DVN_ZIP002", "bundle.zip")))
  expect_false(dl$downloaded[[1]])
})

test_that("unzip_types leaves datasets without a zip completely unchanged", {
  testthat::local_mocked_bindings(
    .dataverse_info = function(host, doi, pb = NULL) {
      data.frame(
        dataverse_host = host, dataverse_doi = doi,
        files = I(list(list(list(
          label = "plain.csv",
          dataFile = list(id = 5, filename = "plain.csv", filesize = 8,
                          checksum = NULL)
        ))))
      )
    }
  )
  testthat::local_mocked_bindings(
    request = function(url) structure(list(url = url), class = "httr2_request"),
    req_headers = function(req, ...) req,
    req_timeout = function(req, seconds) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) structure(
      list(status = 200, body = charToRaw("x,y\n1,2\n")), class = "httr2_response"),
    resp_status = function(resp) resp$status,
    resp_body_raw = function(resp) resp$body,
    .package = "httr2"
  )

  tmp <- withr::local_tempdir()
  dl <- dataverse_file_download("dataverse.harvard.edu", "10.7910/DVN/PLAIN01",
                                download_to = tmp, unzip_types = "data")

  expect_true(dl$downloaded[[1]])
  expect_true(is.na(dl$extracted[[1]]))
  expect_true(file.exists(file.path(tmp, "10.7910_DVN_PLAIN01", "plain.csv")))
})


test_that("dataverse_info queries a dataset once when two urls name it", local({
  # Regression test (2026-10-13 survey): dataverse_links() returns the
  # dataset.xhtml link AND the bare DOI found inside it as two rows, and
  # dataverse_info() queried both -- measured live on Harvard
  # doi:10.7910/DVN/SVVULN, whose dataset JSON was fetched twice.
  calls <- character(0)
  testthat::local_mocked_bindings(
    online = \(...) TRUE,
    .dataverse_info = function(host, doi, pb = NULL) {
      calls <<- c(calls, paste(host, doi))
      data.frame(dataverse_host = host, dataverse_doi = doi, title = "T")
    }
  )

  paper <- test_paper(text = paste(
    "Data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SVVULN"))
  links <- dataverse_links(paper)
  # the duplication this guards against starts here: two urls, one dataset
  expect_equal(nrow(links), 2)
  expect_equal(unique(links$dataverse_doi), "10.7910/DVN/SVVULN")

  info <- dataverse_info(links)
  expect_equal(calls, "dataverse.harvard.edu 10.7910/DVN/SVVULN")
  # both urls still receive the one result
  expect_equal(nrow(info), 2)
  expect_equal(info$title, c("T", "T"))
}))


test_that(".dataverse_resolve_doi_host reads the handle record, exactly, once per session", local({
  # Shared-prefix DOIs (DANS 10.17026, RODBUK) are resolved by asking
  # doi.org's handle API for the registered URL (~0.35 s) instead of
  # downloading the landing page via the redirect (~3.8 s; both measured
  # 2026-10-13), and the answer is remembered for the session because
  # dataverse_links() and dataverse_info() each resolve the same DOI.
  cache <- metacheck:::.dataverse_doi_host_cache
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  withr::defer(rm(list = ls(cache, all.names = TRUE), envir = cache))

  handle_reply <- function(url) list(
    responseCode = 1,
    values = list(list(index = 1, type = "URL",
                       data = list(format = "string", value = url))))

  requested <- character(0)
  reply <- handle_reply(
    "https://lifesciences.datastations.nl/citation?persistentId=doi:10.17026/dans-2b8-gx7j")
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      requested <<- c(requested, req$url)
      httr2::response(status_code = 200)
    },
    resp_body_json = function(resp, ...) reply,
    .package = "httr2"
  )

  dans <- c("archaeology.datastations.nl", "lifesciences.datastations.nl",
            "phys-techsciences.datastations.nl", "ssh.datastations.nl")
  expect_equal(
    metacheck:::.dataverse_resolve_doi_host("10.17026/dans-2b8-gx7j", dans),
    "lifesciences.datastations.nl")
  expect_equal(requested,
               "https://doi.org/api/handles/10.17026/dans-2b8-gx7j?type=URL")

  # second call (any case -- DOIs are case-insensitive): no new request
  expect_equal(
    metacheck:::.dataverse_resolve_doi_host("10.17026/DANS-2B8-GX7J", dans),
    "lifesciences.datastations.nl")
  expect_length(requested, 1)

  # The hostname must match exactly: "rodbuk.pl" is a substring of every
  # "<x>.rodbuk.pl" URL, and a substring test returned rodbuk.pl for a DOI
  # registered at uj.rodbuk.pl whenever rodbuk.pl was listed first.
  reply <- handle_reply("https://uj.rodbuk.pl/dataset.xhtml?persistentId=doi:10.26106/abc")
  expect_equal(
    metacheck:::.dataverse_resolve_doi_host("10.26106/abc",
                                             c("rodbuk.pl", "uj.rodbuk.pl")),
    "uj.rodbuk.pl")

  # A DOI with no URL in its handle record (responseCode 100) falls back to
  # the first candidate, as before.
  reply <- list(responseCode = 100, handle = "10.17026/nope")
  expect_equal(
    metacheck:::.dataverse_resolve_doi_host("10.17026/nope", dans),
    "archaeology.datastations.nl")
}))


test_that(".dataverse_resolve_doi_host does not remember a failed lookup", local({
  cache <- metacheck:::.dataverse_doi_host_cache
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  withr::defer(rm(list = ls(cache, all.names = TRUE), envir = cache))

  n <- 0
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) { n <<- n + 1; stop("no connection") },
    .package = "httr2"
  )
  cands <- c("archaeology.datastations.nl", "ssh.datastations.nl")
  expect_equal(metacheck:::.dataverse_resolve_doi_host("10.17026/x", cands),
               "archaeology.datastations.nl")
  expect_equal(metacheck:::.dataverse_resolve_doi_host("10.17026/x", cands),
               "archaeology.datastations.nl")
  # tried again the second time, since the first gave no answer
  expect_equal(n, 2)
}))

test_that("a Dataverse Handle (persistentId=hdl:) is parsed and asked for as a Handle (#476)", {
  parsed <- .dataverse_parse(c(
    "https://dataverse.nl/dataset.xhtml?persistentId=hdl:10411/B2VWDH",
    "https://dataverse.nl/dataset.xhtml?persistentId=hdl%3A10411%2FB2VWDH&version=1.0",
    "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GGDUND",
    "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910%2FDVN%2FGGDUND"))
  expect_equal(parsed$host, c("dataverse.nl", "dataverse.nl", "dataverse.harvard.edu",
                              "dataverse.harvard.edu"))
  expect_equal(parsed$doi, c("hdl:10411/B2VWDH", "hdl:10411/B2VWDH", "10.7910/DVN/GGDUND",
                             "10.7910/DVN/GGDUND"))

  seen <- character(0)
  testthat::local_mocked_bindings(
    .batch_query = function(urls, msg = NULL, req_func = identity) {
      seen <<- c(seen, urls)
      list(structure(list(status = 404), class = "httr2_response"))
    }
  )
  testthat::local_mocked_bindings(resp_status = function(resp) resp$status, .package = "httr2")
  suppressWarnings(.dataverse_info("dataverse.nl", "hdl:10411/B2VWDH"))
  suppressWarnings(.dataverse_info("dataverse.harvard.edu", "10.7910/DVN/GGDUND"))
  expect_equal(seen, c(
    "https://dataverse.nl/api/datasets/:persistentId/?persistentId=hdl:10411/B2VWDH",
    "https://dataverse.harvard.edu/api/datasets/:persistentId/?persistentId=doi:10.7910/DVN/GGDUND"))
})

test_that("a file Dataverse converted is taken as it was uploaded (#582)", {
  # the listing's checksum is the original's, and /api/access/datafile/<id>
  # sends the converted .tab, so every such file failed the checksum
  plain <- list(label = "analysis.R",
                dataFile = list(id = 1, filename = "analysis.R", filesize = 120))
  e <- .dataverse_file_entry(plain, "dv.example.org")
  expect_equal(e$name, "analysis.R")
  expect_equal(e$size, 120)
  expect_equal(e$url, "https://dv.example.org/api/access/datafile/1")

  conv <- list(label = "Study 1.tab",
               dataFile = list(id = 2, filename = "Study 1.tab", filesize = 900,
                               originalFileFormat = "application/x-spss-sav",
                               originalFileName = "study1.sav", originalFileSize = 1000))
  e <- .dataverse_file_entry(conv, "dv.example.org")
  expect_equal(e$name, "Study 1.sav")   # the listed name, the original's extension
  expect_equal(e$size, 1000)            # the original's size
  expect_equal(e$url, "https://dv.example.org/api/access/datafile/2?format=original")

  # converted, but the original's name and size are not given
  bare <- list(label = "x.tab",
               dataFile = list(id = 3, filename = "x.tab", filesize = 50,
                               originalFileFormat = "application/x-stata",
                               originalFileName = list(), originalFileSize = list()))
  e <- .dataverse_file_entry(bare, "dv.example.org")
  expect_equal(e$name, "x.tab")
  expect_true(is.na(e$size))            # not the converted file's size
  expect_match(e$url, "format=original$")
})

test_that("the repository survey's Dataverse hosts and prefixes are added to the hand-written lists (#636)", {
  hosts <- .dataverse_hosts()
  # hand-written and survey hosts, each once
  expect_true(all(c("dataverse.harvard.edu", "data.indores.fr", "dataverse.geus.dk") %in% hosts))
  expect_false(any(duplicated(hosts)))
  x <- .dataverse_doi_prefix_hosts()
  expect_false(any(duplicated(names(x))))
  # a survey prefix of a host the hand-written table writes with "www." goes
  # under that entry, not a second one
  expect_false(any(duplicated(sub("^www\\.", "", names(x)))))
  # a DOI under a survey prefix finds its host
  expect_equal(.dataverse_host_from_doi("10.48579/PRO/3LQH1M"), "data.indores.fr")
  # never another platform's own prefix (10.6084 is Figshare's, #434)
  p <- unlist(x, use.names = FALSE)
  expect_false(any(c("10.6084", "10.5281", "10.17605", "10.5061") %in% p))
  # every prefix is a full prefix: a table read as numbers loses a final 0
  # ("10.18150" -> "10.1815")
  expect_true(all(grepl("^10\\.[0-9]{4,9}$", p)))
  expect_true(all(c("10.18150") %in% unlist(.dataverse_doi_prefix_hosts_survey())))
})
