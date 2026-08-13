test_that(".zenodo_api", {
  expect_equal(.zenodo_api(TRUE), "https://sandbox.zenodo.org/api")
  expect_equal(.zenodo_api(FALSE), "https://zenodo.org/api")
})


test_that("zenodo_pat", {
  expect_true(is.function(metacheck::zenodo_pat))
  expect_no_error(helplist <- help(zenodo_pat, metacheck))

  withr::local_options(metacheck.zenodo.pat = NULL,
                       metacheck.zenodo.pat.sandbox = NULL)
  withr::local_envvar(ZENODO_PAT = "", ZENODO_SANDBOX_PAT = "")

  # unset
  expect_equal(zenodo_pat(sandbox = TRUE), "")
  expect_equal(zenodo_pat(sandbox = FALSE), "")

  # the sandbox and the real server keep separate tokens, so a sandbox token
  # can never be sent to zenodo.org by accident
  zenodo_pat("sandbox-token", sandbox = TRUE)
  expect_equal(zenodo_pat(sandbox = TRUE), "sandbox-token")
  expect_equal(zenodo_pat(sandbox = FALSE), "")

  zenodo_pat("real-token", sandbox = FALSE)
  expect_equal(zenodo_pat(sandbox = FALSE), "real-token")
  expect_equal(zenodo_pat(sandbox = TRUE), "sandbox-token")

  expect_error(zenodo_pat(123), "single string")
  expect_error(zenodo_pat(c("a", "b")), "single string")
})


test_that("zenodo_pat falls back to the environment variables", {
  withr::local_options(metacheck.zenodo.pat = NULL,
                       metacheck.zenodo.pat.sandbox = NULL)
  withr::local_envvar(ZENODO_PAT = "env-real",
                      ZENODO_SANDBOX_PAT = "env-sandbox")

  expect_equal(zenodo_pat(sandbox = TRUE), "env-sandbox")
  expect_equal(zenodo_pat(sandbox = FALSE), "env-real")
})


test_that(".zenodo_license_id maps every OSF license", {
  # The 19 licenses api.osf.io/v2/licenses/ offers (checked 2026-08-12). Each
  # id on the right returned 200 from zenodo.org/api/vocabularies/licenses/.
  expect_equal(.zenodo_license_id("CC-By Attribution 4.0 International"),
               "cc-by-4.0")
  expect_equal(.zenodo_license_id("CC-BY Attribution-NonCommercial 4.0 International"),
               "cc-by-nc-4.0")
  expect_equal(.zenodo_license_id("CC-BY Attribution-No Derivatives 4.0 International"),
               "cc-by-nd-4.0")
  expect_equal(.zenodo_license_id("CC-BY Attribution-NonCommercial-ShareAlike 4.0 International"),
               "cc-by-nc-sa-4.0")
  expect_equal(.zenodo_license_id("CC0 1.0 Universal"), "cc0-1.0")
  expect_equal(.zenodo_license_id("MIT License"), "mit")
  expect_equal(.zenodo_license_id('BSD 2-Clause "Simplified" License'),
               "bsd-2-clause")
  expect_equal(.zenodo_license_id('BSD 3-Clause "New"/"Revised" License'),
               "bsd-3-clause")
  expect_equal(.zenodo_license_id("Apache License 2.0"), "apache-2.0")
  expect_equal(.zenodo_license_id("Artistic License 2.0"), "artistic-2.0")
  expect_equal(.zenodo_license_id("Academic Free License (AFL) 3.0"), "afl-3.0")
  expect_equal(.zenodo_license_id("Eclipse Public License 1.0"), "epl-1.0")
  expect_equal(.zenodo_license_id("Mozilla Public License 2.0"), "mpl-2.0")
  expect_equal(.zenodo_license_id("GNU General Public License (GPL) 2.0"),
               "gpl-2.0-or-later")
  expect_equal(.zenodo_license_id("GNU General Public License (GPL) 3.0"),
               "gpl-3.0-or-later")
  expect_equal(.zenodo_license_id("GNU Lesser General Public License (LGPL) 2.1"),
               "lgpl-2.1-or-later")
  expect_equal(.zenodo_license_id("GNU Lesser General Public License (LGPL) 3.0"),
               "lgpl-3.0-or-later")

  # "No license" and "Other" name nothing Zenodo accepts, so they fall through
  # to the caller's `license` argument exactly as an unset license does
  expect_equal(.zenodo_license_id("No license"), NA_character_)
  expect_equal(.zenodo_license_id("Other"), NA_character_)

  # matching ignores case and punctuation
  expect_equal(.zenodo_license_id("mit   LICENSE"), "mit")

  # nothing to map
  expect_equal(.zenodo_license_id(NA_character_), NA_character_)
  expect_equal(.zenodo_license_id(""), NA_character_)
  expect_equal(.zenodo_license_id(character(0)), NA_character_)
  expect_equal(.zenodo_license_id(NULL), NA_character_)
  expect_equal(.zenodo_license_id("Some Made Up License"), NA_character_)
})


test_that(".zenodo_regex_escape", {
  # A folder path is pasted into a pattern to strip it off the front of each
  # file path, so metacharacters in project names must not be interpreted.
  strip <- function(fp, folder) {
    sub(paste0("^", .zenodo_regex_escape(folder), "[/\\\\]*"), "", fp)
  }

  expect_equal(strip("/a/b/data.csv", "/a/b"), "data.csv")
  expect_equal(strip("/a/b/sub/data.csv", "/a/b"), "sub/data.csv")
  expect_equal(strip("/a/v1.0/data.csv", "/a/v1.0"), "data.csv")
  expect_equal(strip("/a/proj (old)/d.csv", "/a/proj (old)"), "d.csv")
  expect_equal(strip("/a/c++/d.csv", "/a/c++"), "d.csv")
  expect_equal(strip("C:/x/y/d.csv", "C:/x/y"), "d.csv")
})


test_that(".zenodo_build_metadata from OSF metadata", {
  meta <- list(
    osf_id = "6nt4v",
    title = "My Project",
    description = "A description",
    tags = c("open", "data"),
    license = "CC-By Attribution 4.0 International",
    creators = list(list(name = "DeBruine, Lisa"))
  )

  md <- .zenodo_build_metadata(meta, "/tmp/6nt4v")

  expect_equal(md$title, "My Project")
  expect_equal(md$description, "A description")
  expect_equal(md$upload_type, "dataset")
  expect_equal(md$license, "cc-by-4.0")
  expect_equal(md$keywords, list("open", "data"))
  expect_equal(md$related_identifiers[[1]]$identifier, "https://osf.io/6nt4v/")
  expect_equal(md$related_identifiers[[1]]$relation, "isIdenticalTo")
  expect_false(attr(md, "license_was_default"))
})


test_that(".zenodo_build_metadata falls back when the OSF has no license", {
  meta <- list(osf_id = "6nt4v", title = "T", description = "D",
               license = NA_character_, creators = list(list(name = "A, B")))

  md <- .zenodo_build_metadata(meta, "/tmp/6nt4v", license = "cc0-1.0")

  expect_equal(md$license, "cc0-1.0")
  # flagged so zenodo_upload() can warn that a license was assumed
  expect_true(attr(md, "license_was_default"))
})


test_that(".zenodo_build_metadata with no OSF metadata at all", {
  # uploading a plain folder, not something osf_file_download() produced
  md <- .zenodo_build_metadata(NULL, "/tmp/myfolder")

  expect_equal(md$title, "myfolder")
  expect_equal(md$creators, list(list(name = "Unknown")))
  expect_true(nzchar(md$description))
  expect_null(md$related_identifiers)
  expect_null(md$keywords)
  expect_true(attr(md, "license_was_default"))
})


test_that(".zenodo_build_metadata fills blank title and description", {
  # Zenodo rejects a deposition with an empty title or description
  md <- .zenodo_build_metadata(
    list(osf_id = "abcde", title = "", description = ""), "/tmp/f")

  expect_equal(md$title, "f")
  expect_equal(md$description,
               "Files archived from the OSF project https://osf.io/abcde/")
})


test_that("zenodo_upload validates its input", {
  expect_true(is.function(metacheck::zenodo_upload))
  expect_no_error(helplist <- help(zenodo_upload, metacheck))

  # a data frame that did not come from osf_file_download()
  expect_error(zenodo_upload(data.frame(x = 1), ask = FALSE),
               "download_path")
})


test_that("zenodo_upload needs a token", {
  withr::local_options(metacheck.zenodo.pat = NULL,
                       metacheck.zenodo.pat.sandbox = NULL)
  withr::local_envvar(ZENODO_PAT = "", ZENODO_SANDBOX_PAT = "")

  folder <- withr::local_tempdir()
  writeLines("a,b", file.path(folder, "data.csv"))

  # the message names the variable to set, and which server it is for
  expect_error(zenodo_upload(folder, ask = FALSE),
               "ZENODO_SANDBOX_PAT")
  expect_error(zenodo_upload(folder, sandbox = FALSE, ask = FALSE),
               "ZENODO_PAT")
})


test_that("zenodo_upload skips folders that are not there", {
  withr::local_options(metacheck.zenodo.pat.sandbox = "fake-token")

  expect_warning(res <- zenodo_upload("/no/such/folder", ask = FALSE),
                 "could not be found")
  expect_null(res)
})


test_that("zenodo_upload does nothing when there are no files", {
  withr::local_options(metacheck.zenodo.pat.sandbox = "fake-token")

  folder <- withr::local_tempdir()
  expect_message(res <- zenodo_upload(folder, ask = FALSE), "No files")
  expect_null(res)
})


test_that("zenodo_upload rejects a bad token before uploading anything", {
  skip_if_quick() # contacts the Zenodo sandbox
  skip_if_not(online("sandbox.zenodo.org"))
  skip_on_cran()

  withr::local_options(metacheck.zenodo.pat.sandbox = "definitely-not-a-real-token")

  folder <- withr::local_tempdir()
  writeLines("a,b", file.path(folder, "data.csv"))

  # Zenodo answers a bad token with 500 on the deposition endpoint, which
  # req_retry() treats as transient and backs off on for minutes. The up-front
  # check turns that into an immediate, accurate error.
  expect_error(zenodo_upload(folder, ask = FALSE),
               "did not accept the token")
})
