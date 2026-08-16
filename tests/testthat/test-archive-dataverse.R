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

  dl <- dataverse_file_download(
    "dataverse.harvard.edu", "10.7910/DVN/ABC123",
    download_to = tmpdir,
    max_file_size = 10
  )

  expect_equal(nrow(dl), 1)
  expect_equal(dl$dataverse_doi, "10.7910/DVN/ABC123")
  expect_equal(dl$key, "small.csv")
  expect_false(dl$downloaded)

  folder <- file.path(tmpdir, "10.7910_DVN_ABC123")
  expect_true(dir.exists(folder))
  expect_equal(list.files(folder), character(0))

  # vectorised: multiple datasets
  dl2 <- dataverse_file_download(
    host = c("dataverse.harvard.edu", "dataverse.nl"),
    doi = c("10.7910/DVN/ABC123", "10.34894/XYZ999"),
    download_to = tmpdir,
    max_file_size = 10
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
    .dataverse_zip_members = function(url, dest, keep_types, max_file_size) {
      requested <<- c(requested, url)
      expect_equal(keep_types, "data")
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
    .dataverse_zip_members = function(url, dest, keep_types, max_file_size) NULL
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
