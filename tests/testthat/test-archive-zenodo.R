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

  testthat::local_mocked_bindings(
    request = function(url) {
      structure(list(url = url), class = "httr2_request")
    },
    req_error = function(req, is_error) {
      req
    },
    req_perform = function(req) {
      structure(list(
        status = 200,
        body = list(
          metadata = list(
            title = "Example title",
            description = "Example description",
            publication_date = "2026-01-01",
            creators = list(list(name = "Jane Doe")),
            keywords = list("open", "data"),
            resource_type = list(type = "dataset"),
            journal = list(title = "Journal"),
            license = list(id = "cc-by-4.0")
          ),
          doi = "10.5281/zenodo.12345",
          updated = "2026-01-02",
          owners = list(list(id = 1)),
          stats = list(downloads = 10, unique_downloads = 5, views = 20),
          files = list(list(
            id = "f1",
            key = "data.csv",
            size = 101,
            checksum = "md5:abc",
            links = list(self = "https://files.example/f1")
          ))
        )
      ), class = "httr2_response")
    },
    resp_status = function(resp) {
      resp$status
    },
    resp_body_json = function(resp) {
      resp$body
    },
    .package = "httr2"
  )

  info <- .zenodo_info("https://doi.org/10.5281/zenodo.12345")

  expect_equal(info$zenodo_id, "12345")
  expect_equal(info$title, "Example title")
  expect_equal(info$doi, "10.5281/zenodo.12345")
  expect_equal(info$resource_type, "dataset")
  expect_equal(info$license, "cc-by-4.0")
  expect_equal(info$downloads, 10)
  expect_equal(length(info$files[[1]]), 1)

  testthat::local_mocked_bindings(
    request = function(url) {
      structure(list(url = url), class = "httr2_request")
    },
    req_error = function(req, is_error) {
      req
    },
    req_perform = function(req) {
      structure(list(status = 404), class = "httr2_response")
    },
    resp_status = function(resp) {
      resp$status
    },
    .package = "httr2"
  )

  expect_warning(unfound <- .zenodo_info("12345"))
  expect_equal(unfound$error, "unfound")
})


test_that("zenodo_info", {
  expect_true(is.function(metacheck::zenodo_info))
  expect_no_error(helplist <- help(zenodo_info, metacheck))

  expect_error(zenodo_info(bad_arg))
  testthat::local_mocked_bindings(
    online = function(...) TRUE,
    .zenodo_info = function(zenodo_id, pb = NULL) {
      data.frame(
        zenodo_id = as.character(zenodo_id),
        title = paste("Record", zenodo_id),
        files = I(list(list(list(
          id = paste0("f", zenodo_id),
          key = "data.csv",
          size = 123,
          checksum = "md5:test",
          links = list(self = NA_character_)
        ))))
      )
    }
  )

  z <- c(
    "https://doi.org/10.5281/zenodo.111",
    "https://zenodo.org/records/222",
    "https://doi.org/10.5281/zenodo.111",
    NA_character_
  )
  info <- zenodo_info(z)

  expect_setequal(info$zenodo_id, c("111", "222"))
  expect_true(all(grepl("Record", info$title)))

  tbl <- data.frame(
    id = 1:3,
    href = c(
      "https://doi.org/10.5281/zenodo.333",
      "https://zenodo.org/records/444",
      "not-a-zenodo-id"
    )
  )
  info2 <- zenodo_info(tbl, "href")

  expect_equal(nrow(info2), 3)
  expect_equal(info2$href, tbl$href)
  expect_equal(info2$zenodo_id[1:2], c("333", "444"))
  expect_true(is.na(info2$zenodo_id[3]))
  expect_true(is.na(info2$title[3]))
})


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
            size = 256,
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
