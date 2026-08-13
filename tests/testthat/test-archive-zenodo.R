test_that("zenodo_links", {
  expect_true(is.function(metacheck::zenodo_links))
  expect_no_error(helplist <- help(zenodo_links, metacheck))

  expect_error(zenodo_links(bad_arg))

  paper <- test_paper(url = c(
    "https://zenodo.org/records/12345",
    "https://doi.org/10.5281/zenodo.98765",
    "https://osf.io/abcde"
  ))

  links <- zenodo_links(paper)

  expect_equal(nrow(links), 2)
  expect_equal(links$href, c(
    "https://zenodo.org/records/12345",
    "https://doi.org/10.5281/zenodo.98765"
  ))
  expect_equal(unname(links$zenodo_id), c("12345", "98765"))
  expect_equal(unname(links$zenodo_link), c(
    "https://doi.org/10.5281/zenodo.12345",
    "https://doi.org/10.5281/zenodo.98765"
  ))
})


test_that(".zenodo_id", {
  expect_true(is.function(metacheck:::.zenodo_id))

  zenodo_url <- c(
    "12345",
    "https://zenodo.org/records/12345",
    "https://zenodo.org/record/12345",
    "https://doi.org/10.5281/zenodo.12345",
    "zenodo.12345",
    "https://zenodo.org/records/12345 zenodo.98765", # malformed
    "not-a-zenodo-id",
    ""
  )

  ids <- .zenodo_id(zenodo_url)
  expect_equal(unname(ids), c(rep("12345", 6), NA, NA))

  # NULL
  zenodo_url <- NULL
  id <- .zenodo_id(zenodo_url)
  expect_equal(id, character(0))
})



test_that(".zenodo_info", {
  expect_true(is.function(metacheck:::.zenodo_info))

  zenodo_id <- .zenodo_id("10.5281/zenodo.2669586")
  info <- .zenodo_info(zenodo_id)

  expect_equal(info$zenodo_id, "2669586")
  expect_equal(info$title, "faux: Simulation for Factorial Designs")
  expect_equal(info$doi, "10.5281/zenodo.7852893")
  expect_equal(info$resource_type, "software")
  expect_equal(info$license, "mit-license")
  expect_gt(info$downloads, 200)
  #expect_equal(info$files[[1]]$key, "debruine/faux-v1.2.1.zip")

  zenodo_id <- "00000000"
  expect_warning(unfound <- .zenodo_info(zenodo_id))
  expect_equal(unfound$error, "unfound")
}, "mock")



test_that("zenodo_info", {
  expect_true(is.function(metacheck::zenodo_info))
  expect_no_error(helplist <- help(zenodo_info, metacheck))

  z <- c(
    "https://doi.org/10.5281/zenodo.17754445",
    "https://zenodo.org/records/123456789",
    "https://doi.org/10.5281/zenodo.17754445",
    NA_character_
  )
  info <- zenodo_info(z)

  expect_setequal(info$zenodo_id, c("17754445", "123456789"))
  expect_contains(info$title, "Example title")

  tbl <- data.frame(
    id = 1:3,
    href = c(
      "https://doi.org/10.5281/zenodo.17754445",
      "https://zenodo.org/records/123456789",
      "not-a-zenodo-id"
    )
  )
  info2 <- zenodo_info(tbl, "href")

  expect_equal(nrow(info2), 3)
  expect_equal(info2$href, tbl$href)
  expect_equal(info2$zenodo_id[1:2], c("17754445", "123456789"))
  expect_true(is.na(info2$zenodo_id[3]))
  expect_true(is.na(info2$title[3]))
}, "mock")


test_that("zenodo_file_download", {
  expect_true(is.function(metacheck::zenodo_file_download))
  expect_no_error(helplist <- help(zenodo_file_download, metacheck))

  expect_error(zenodo_file_download(bad_arg))

  testthat::local_mocked_bindings(
    zenodo_info = function(zenodo_url, id_col = 1, pb = NULL) {
      zid <- .zenodo_id(zenodo_url)
      data.frame(
        zenodo_url = as.character(zenodo_url),
        zenodo_id = zid,
        files = I(list(list(
          list(
            id = paste0("small_", zid),
            key = "small.csv",
            size = 100,
            checksum = "md5:small",
            links = list(self = NA_character_)
          ),
          list(
            id = paste0("big_", zid),
            key = "big.bin",
            size = 12 * 1024 * 1024,
            checksum = "md5:big",
            links = list(self = NA_character_)
          )
        )))
      )
    }
  )

  tmpdir <- withr::local_tempdir()

  dl <- zenodo_file_download(
    zenodo_id = "12345",
    download_to = tmpdir,
    max_file_size = 10
  )

  expect_equal(nrow(dl), 1)
  expect_equal(dl$zenodo_id, "12345")
  expect_equal(dl$key, "small.csv")
  expect_false(dl$downloaded)

  folder <- file.path(tmpdir, "12345")
  expect_true(dir.exists(folder))
  expect_equal(list.files(folder), character(0))

  dl2 <- zenodo_file_download(
    zenodo_id = c("12345", "67890"),
    download_to = tmpdir,
    max_file_size = 10
  )

  expect_setequal(dl2$zenodo_id, c("12345", "67890"))
  expect_true(all(dl2$downloaded == FALSE))

  testthat::local_mocked_bindings(
    zenodo_info = function(zenodo_url, id_col = 1, pb = NULL) {
      zid <- .zenodo_id(zenodo_url)
      data.frame(
        zenodo_url = as.character(zenodo_url),
        zenodo_id = zid,
        files = I(list(list(
          list(
            id = paste0("ok_", zid),
            key = "ok.csv",
            # The mocked req_perform() below serves exactly "x,y\n1,2\n", so
            # the fixture has to declare that file's real length: downloads are
            # checked against the size Zenodo reports, and a fixture claiming a
            # size its own test data does not have is (correctly) reported as a
            # file that did not arrive intact.
            size = nchar("x,y\n1,2\n"),
            # Not a real md5, so the checksum comparison is skipped for this
            # file and the size check stands on its own. A genuine md5 is
            # exercised in test-archive-zenodo-upload.R instead.
            checksum = "md5:ok",
            links = list(self = paste0("https://files.example/", zid, "/ok.csv"))
          )
        )))
      )
    }
  )

  testthat::local_mocked_bindings(
    request = function(url) structure(list(url = url), class = "httr2_request"),
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
  dl_ok <- zenodo_file_download(
    zenodo_id = "24680",
    download_to = tmpdir_ok,
    max_file_size = 10
  )

  expect_equal(nrow(dl_ok), 1)
  expect_true(dl_ok$downloaded[[1]])
  expect_true(file.exists(file.path(tmpdir_ok, "24680", "ok.csv")))
})

# A record holding one large zip, for the unzip_types tests below. The zip is far
# larger than max_file_size, which is the case that matters: without the
# exemption in the size filter it would be discarded before anything could be
# read out of it, and unzip_types could never do anything.
.zip_record_info <- function(zenodo_url, id_col = 1, pb = NULL) {
  zid <- .zenodo_id(zenodo_url)
  data.frame(
    zenodo_url = as.character(zenodo_url),
    zenodo_id = zid,
    files = I(list(list(
      list(
        id = paste0("z_", zid),
        key = "bundle.zip",
        size = 130 * 1024 * 1024,
        checksum = "md5:zzz",
        links = list(self = "https://files.example/bundle.zip")
      )
    )))
  )
}

test_that("zenodo_file_download extracts only wanted members with unzip_types", {
  # The archive holds one data file and one image. Only the data file should be
  # fetched; the image is never requested.
  requested <- character(0)
  testthat::local_mocked_bindings(
    zenodo_info = .zip_record_info,
    .zenodo_zip_members = function(url, dest, keep_types, max_file_size) {
      requested <<- c(requested, url)
      expect_equal(keep_types, "data")
      writeLines("id,x\n1,2", file.path(dest, "study.csv"))
      data.frame(name = "study.csv", path = file.path(dest, "study.csv"),
                 size = 9, ok = TRUE)
    }
  )

  tmp <- withr::local_tempdir()
  dl <- zenodo_file_download("13579", download_to = tmp,
                             unzip_types = "data", max_file_size = 10)

  expect_equal(nrow(dl), 1)
  expect_equal(dl$key, "bundle.zip")
  # The zip survived the 10 MB cap despite being 130 MB, because only its
  # selected members are transferred.
  expect_true(dl$downloaded[[1]])
  expect_equal(dl$extracted[[1]], 1)
  expect_equal(requested, "https://files.example/bundle.zip")
  expect_true(file.exists(file.path(tmp, "13579", "study.csv")))
  # Zenodo's size and MD5 describe the zip, which was never downloaded, so the
  # verification step must not measure the row against them.
  expect_true(is.na(dl$size_on_disk[[1]]))
})

test_that("zenodo_file_download falls back to a whole download when the zip cannot be read", {
  # A host that refuses range requests: .zenodo_zip_members() returns NULL, and
  # the archive must then be fetched whole rather than silently skipped.
  testthat::local_mocked_bindings(
    zenodo_info = .zip_record_info,
    .zenodo_zip_members = function(url, dest, keep_types, max_file_size) NULL
  )
  testthat::local_mocked_bindings(
    request = function(url) structure(list(url = url), class = "httr2_request"),
    req_timeout = function(req, seconds) req,
    req_error = function(req, is_error) req,
    req_retry = function(req, ...) req,
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
    zenodo_file_download("13579", download_to = tmp,
                         unzip_types = "data", max_file_size = 10))

  # The whole archive was downloaded, so this is an ordinary file row again:
  # `extracted` stays NA and the zip itself is on disk.
  expect_true(is.na(dl$extracted[[1]]))
  expect_true(file.exists(file.path(tmp, "13579", "bundle.zip")))
  # And because it is an ordinary row again, the usual size check applies to it
  # -- the mocked body is not 130 MB, so verification correctly rejects it.
  expect_false(dl$downloaded[[1]])
})

test_that("unzip_types leaves records without a zip completely unchanged", {
  testthat::local_mocked_bindings(
    zenodo_info = function(zenodo_url, id_col = 1, pb = NULL) {
      zid <- .zenodo_id(zenodo_url)
      data.frame(
        zenodo_url = as.character(zenodo_url), zenodo_id = zid,
        # size must match the body the mocked req_perform() serves below
        # ("x,y\n1,2\n", 8 bytes), or the verification step rejects the row.
        files = I(list(list(list(
          id = paste0("c_", zid), key = "plain.csv", size = 8,
          checksum = NA_character_,
          links = list(self = "https://files.example/plain.csv")))))
      )
    }
  )
  testthat::local_mocked_bindings(
    request = function(url) structure(list(url = url), class = "httr2_request"),
    req_timeout = function(req, seconds) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) structure(
      list(status = 200, body = charToRaw("x,y\n1,2\n")), class = "httr2_response"),
    resp_status = function(resp) resp$status,
    resp_body_raw = function(resp) resp$body,
    .package = "httr2"
  )

  tmp <- withr::local_tempdir()
  dl <- zenodo_file_download("11111", download_to = tmp, unzip_types = "data")

  expect_true(dl$downloaded[[1]])
  expect_true(is.na(dl$extracted[[1]]))   # nothing was unzipped
  expect_true(file.exists(file.path(tmp, "11111", "plain.csv")))
})
