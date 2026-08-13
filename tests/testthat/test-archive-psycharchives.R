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

test_that(".psycharchives_info", {
  url <- "https://hdl.handle.net/20.500.12034/17526"
  info <- .psycharchives_info(url)

  expect_equal(info$pa_url, url)
  expect_match(info$PA_title, "International Cognitive Ability Resource")
  expect_match(info$PA_authors, "Doebler")
  expect_match(info$PA_license, "restrictedAccess")

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
}, "mock")
