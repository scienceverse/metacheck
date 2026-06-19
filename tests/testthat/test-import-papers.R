test_that("exists", {
  expect_true(is.function(metacheck::papers_available))
  expect_true(is.function(metacheck::papers_load))
  expect_true(is.function(metacheck::papers_remove))
  expect_true(is.function(metacheck::papers_metadata))
  expect_no_error(helplist <- help(papers_available, metacheck))
  expect_no_error(helplist <- help(papers_load, metacheck))
  expect_no_error(helplist <- help(papers_remove, metacheck))
  expect_no_error(helplist <- help(papers_metadata, metacheck))
})

test_that("papers_available", {
  avail <- papers_available("scienceverse/papers")

  expect_equal(names(avail), c("name", "tag", "size_mb", "cached"))
  expect_setequal(avail$name, c("bmcmed", "plosmed", "collabra", "jdm"))
  expect_true(all(avail$size_mb > 0))
  expect_true(is.logical(avail$cached))

  # tags follow the {corpus}-{date} convention
  expect_true(all(grepl("^[a-z]+-\\d{4}-\\d{2}-\\d{2}$", avail$tag)))
}, "mock")

test_that("papers_available reflects cache state", {
  tmp <- withr::local_tempdir()
  testthat::local_mocked_bindings(.papers_cache_dir = function() tmp, .package = "metacheck")

  avail <- papers_available("scienceverse/papers")
  expect_true(all(!avail$cached))

  file.create(file.path(tmp, "jdm.rds"))
  avail2 <- papers_available("scienceverse/papers")
  expect_true(avail2$cached[avail2$name == "jdm"])
  expect_false(avail2$cached[avail2$name == "bmcmed"])
}, "mock")

test_that(".papers_release_assets resolves immutable re-tags to the newest release per corpus family", {
  httptest2::with_mock_dir("apis_papers_retag", {
    assets <- metacheck:::.papers_release_assets("scienceverse/retagtest")
  })

  # demo-2026-06-18 was superseded by demo-2026-06-20 (e.g. after fixing a
  # mistake in an older, now-immutable release) -- only the newer one is kept
  expect_equal(nrow(assets[assets$name == "demo.rds", ]), 1)
  expect_equal(assets$tag[assets$name == "demo.rds"], "demo-2026-06-20")
  expect_equal(assets$size[assets$name == "demo.rds"], 2000)

  # other family is unaffected, non-rds assets (manifest.csv) are excluded,
  # and a release with zero assets contributes no rows
  expect_true("other.rds" %in% assets$name)
  expect_false("manifest.csv" %in% assets$name)
  expect_equal(nrow(assets), 2)
})

test_that(".papers_release_assets handles repos with no releases", {
  httptest2::with_mock_dir("apis_papers_empty", {
    assets <- metacheck:::.papers_release_assets("scienceverse/norealeases")
  })
  expect_equal(nrow(assets), 0)
  expect_equal(names(assets), c("name", "tag", "size", "download_url"))
})

test_that(".papers_release_assets errors on unreachable repo", {
  httptest2::with_mock_dir("apis_papers_404", {
    expect_error(
      metacheck:::.papers_release_assets("scienceverse/doesnotexist12345"),
      "GitHub API error"
    )
  })
})

# fakes the binary asset download (httr2::req_perform(req, path = ...)) while
# leaving the releases-list JSON call (which has no `path` arg) untouched --
# delegates to the real (pre-mock) req_perform via httptest2's own mock_api,
# since local_mocked_bindings would otherwise recurse into itself
fake_download <- function(rds_path) {
  real_req_perform <- httr2::req_perform
  function(req, path = NULL, ...) {
    if (is.null(path)) return(real_req_perform(req))
    file.copy(rds_path, path, overwrite = TRUE)
    structure(list(method = "GET", url = req$url, status_code = 200L,
                   headers = structure(list(), class = "httr2_headers"),
                   body = raw(0), cache = new.env(parent = emptyenv())),
              class = "httr2_response")
  }
}

test_that("papers_load downloads and returns a corpus", {
  tmp <- withr::local_tempdir()
  testthat::local_mocked_bindings(.papers_cache_dir = function() tmp, .package = "metacheck")

  fake_paper <- list(info = list(title = "fake paper"))
  class(fake_paper) <- "scivrs_paperlist"

  rds_path <- tempfile(fileext = ".rds")
  saveRDS(fake_paper, rds_path)
  withr::defer(unlink(rds_path))

  httptest2::with_mock_dir("apis_papers_retag", {
    testthat::local_mocked_bindings(req_perform = fake_download(rds_path), .package = "httr2")
    op <- capture_messages(
      result <- papers_load("demo", repo = "scienceverse/retagtest")
    )
  })

  expect_equal(result, fake_paper)
})

test_that("papers_load caches when cache = TRUE", {
  tmp <- withr::local_tempdir()
  testthat::local_mocked_bindings(.papers_cache_dir = function() tmp, .package = "metacheck")

  fake_paper <- list(info = list(title = "fake paper"))
  rds_path <- tempfile(fileext = ".rds")
  saveRDS(fake_paper, rds_path)
  withr::defer(unlink(rds_path))

  httptest2::with_mock_dir("apis_papers_retag", {
    testthat::local_mocked_bindings(req_perform = fake_download(rds_path), .package = "httr2")
    op <- capture_messages(
      result <- papers_load("demo", repo = "scienceverse/retagtest", cache = TRUE)
    )
  })

  cache_path <- file.path(tmp, "demo.rds")
  expect_true(file.exists(cache_path))
  expect_equal(result, fake_paper)

  # second call with cache = TRUE reuses the cached file without re-downloading
  no_download <- function(...) stop("should not be called when reading from cache")
  op <- capture_messages(
    result2 <- with_mocked_bindings(
      papers_load("demo", repo = "scienceverse/retagtest", cache = TRUE),
      req_perform = no_download, .package = "httr2"
    )
  )
  expect_equal(result2, fake_paper)
})

test_that("papers_load errors for an unknown corpus name", {
  expect_error(
    papers_load("not_a_real_corpus", repo = "scienceverse/papers"),
    "not found in releases"
  )
}, "mock")

test_that("papers_remove", {
  tmp <- withr::local_tempdir()
  testthat::local_mocked_bindings(.papers_cache_dir = function() tmp, .package = "metacheck")

  # nothing cached yet
  expect_message(removed <- papers_remove("jdm"), "not cached")
  expect_false(removed)

  cache_path <- file.path(tmp, "jdm.rds")
  file.create(cache_path)
  expect_true(file.exists(cache_path))

  expect_message(removed2 <- papers_remove("jdm"), "Removed cached corpus")
  expect_true(removed2)
  expect_false(file.exists(cache_path))
})

test_that("papers_metadata", {
  meta <- papers_metadata("jdm")

  expect_equal(meta$`@type`, "Dataset")
  expect_true(grepl("Judgment and Decision Making", meta$dc_title))
  expect_equal(meta$dc_coverage, "2006-01-01/2022-12-31")

  # nested dc:provenance object is preserved as a nested list
  expect_true(is.list(meta$dc_provenance))
  expect_equal(meta$dc_provenance$conversion_tool, "GROBID 0.8")

  # list-valued fields (dc:subject, dc:format, dc:relation) stay vectors
  expect_true("judgment" %in% meta$dc_subject)
  expect_gt(length(meta$dc_format), 1)

  # colons in original dc:field names are replaced with underscores
  expect_false(any(grepl(":", names(meta)[-(1:2)], fixed = TRUE)))
}, "mock")

test_that("papers_metadata errors for an unknown corpus name", {
  expect_error(
    papers_metadata("doesnotexist999"),
    "No metadata.json found"
  )
}, "mock")
