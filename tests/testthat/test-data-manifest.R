# Tests for the data_check file manifest: provenance metadata and the
# intentional / unintentional split of files that were not downloaded.

test_that("manifest separates intentional and unintentional non-downloads", {
  withr::local_options(metacheck.llm.use = FALSE)

  real <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", real)

  files <- data.frame(
    repo_url  = "https://osf.io/xxxxx",
    file_name = c("gotit.csv", "big.rdata", "stim.mp4",
                  "lost.csv", "nourl.csv", "notes.pdf"),
    file_path = c("gotit.csv", "big.rdata", "stim.mp4",
                  "lost.csv", "nourl.csv", "notes.pdf"),
    file_url  = c(rep("https://osf.io/download/x/", 4), NA, "https://osf.io/download/y/"),
    file_size = c(100, 5e8, 1e6, 100, 100, 100),
    data_type = c("data", "data", "materials", "data", "data", "documentation"),
    data_format = "tabular",
    file_location = c(real, NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
  want <- c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)   # download = "data" semantics
  oversize <- data.frame(repo_url = files$repo_url[2], file_name = "big.rdata",
                         file_size = 5e8)
  failed <- data.frame(repo_url = files$repo_url[4], file_name = "lost.csv",
                       error = "HTTP 429 Too Many Requests.\ndetails")

  path <- withr::local_tempfile(fileext = ".json")
  metacheck:::.data_check_write_manifest(
    path, files, want, gated = NULL, paper_id = "p1", download = "data",
    max_file_size = 100, max_download_size = 500, skip_types = "materials",
    oversize = oversize, failed = failed, model = "test-model")

  m <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  # Split summary: exactly the failed download and the URL-less file are
  # unintentional (the re-run signal); cap/type/mode skips are intentional.
  expect_equal(m$not_downloaded$unintentional_n, 2)
  expect_equal(m$not_downloaded$intentional_n, 3)
  expect_true(m$not_downloaded$rerun_recommended)
  un_names <- vapply(m$not_downloaded$unintentional_files,
                     function(f) f$file_name, character(1))
  expect_setequal(un_names, c("lost.csv", "nourl.csv"))

  # Per-file status and classification.
  by_name <- stats::setNames(m$files, vapply(m$files, `[[`, "", "file_name"))
  expect_equal(by_name$gotit.csv$status, "downloaded")
  expect_null(by_name$gotit.csv$skip_reason)
  expect_equal(by_name$big.rdata$status, "skipped")
  expect_true(by_name$big.rdata$skip_intentional)
  expect_match(by_name$big.rdata$skip_reason, "max_file_size")
  expect_equal(by_name$stim.mp4$status, "skipped")
  expect_match(by_name$stim.mp4$skip_reason, "excluded type 'materials'")
  expect_equal(by_name$lost.csv$status, "failed")
  expect_false(by_name$lost.csv$skip_intentional)
  expect_match(by_name$lost.csv$skip_reason, "429")
  expect_false(grepl("details", by_name$lost.csv$skip_reason))  # first line only
  expect_equal(by_name$nourl.csv$status, "failed")
  expect_equal(by_name$notes.pdf$status, "skipped")
  expect_match(by_name$notes.pdf$skip_reason, "download = \"all\"", fixed = TRUE)
})

test_that("manifest records reproducibility provenance", {
  withr::local_options(metacheck.llm.use = FALSE)
  files <- data.frame(
    repo_url = "https://osf.io/xxxxx", file_name = "a.csv", file_path = "a.csv",
    file_url = "https://osf.io/download/x/", file_size = 10,
    data_type = "data", data_format = "tabular",
    file_location = NA_character_, stringsAsFactors = FALSE
  )
  path <- withr::local_tempfile(fileext = ".json")
  metacheck:::.data_check_write_manifest(
    path, files, want = TRUE, gated = NULL, paper_id = "p1", download = "none",
    max_file_size = 100, max_download_size = 500, skip_types = "materials")

  m <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(m$provenance$software$name, "metacheck")
  expect_true(nzchar(m$provenance$software$version))
  expect_match(m$provenance$r_version, "^R version")
  expect_equal(m$provenance$prod_date, m$generated)
  expect_false(m$provenance$llm$used)
  expect_equal(unlist(m$skip_types), "materials")
  # The DDI-Codebook mapping ships inside the manifest (self-describing).
  expect_match(m$provenance$ddi_mapping[["files[].status"]], "ProcStat")
  # download = "none" is an intentional skip, so no re-run is recommended.
  expect_equal(m$not_downloaded$unintentional_n, 0)
  expect_false(m$not_downloaded$rerun_recommended)
})

test_that("manifest records the LLM model when LLM assistance is on", {
  withr::local_options(metacheck.llm.use = TRUE)
  files <- data.frame(
    repo_url = "https://osf.io/xxxxx", file_name = "a.csv", file_path = "a.csv",
    file_url = "https://osf.io/download/x/", file_size = 10,
    data_type = "data", data_format = "tabular",
    file_location = NA_character_, stringsAsFactors = FALSE
  )
  path <- withr::local_tempfile(fileext = ".json")
  metacheck:::.data_check_write_manifest(
    path, files, want = FALSE, gated = NULL, paper_id = "p1", download = "none",
    max_file_size = 100, max_download_size = 500, model = "groq/test-model")
  m <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_true(m$provenance$llm$used)
  expect_equal(m$provenance$llm$model, "groq/test-model")
})

test_that("manifest_merge creates, overlays, and preserves sections", {
  path <- withr::local_tempfile(fileext = ".json")

  # No file yet -> creates one holding just the patch.
  manifest_merge(path, list(code = list(packages = list("dplyr", "tidyr"))))
  m <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(unlist(m$code$packages), c("dplyr", "tidyr"))

  # A second writer patching a different key must not drop the first's section.
  manifest_merge(path, list(files = list(list(file_name = "a.csv"))))
  m <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(unlist(m$code$packages), c("dplyr", "tidyr"))   # preserved
  expect_equal(m$files[[1]]$file_name, "a.csv")                # added

  # Re-patching a key replaces that key wholesale, others untouched.
  manifest_merge(path, list(code = list(packages = list("readr"))))
  m <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(unlist(m$code$packages), "readr")
  expect_equal(m$files[[1]]$file_name, "a.csv")
})

test_that("data_check manifest write preserves a code section", {
  # A code_check-style `code` section written first must survive a later
  # data_check manifest write (section ownership, not overwrite).
  withr::local_options(metacheck.llm.use = FALSE)
  path <- withr::local_tempfile(fileext = ".json")
  manifest_merge(path, list(code = list(packages = list("dplyr"))))

  files <- data.frame(
    repo_url = "https://osf.io/xxxxx", file_name = "a.csv", file_path = "a.csv",
    file_url = "https://osf.io/download/x/", file_size = 10,
    data_type = "data", data_format = "tabular",
    file_location = NA_character_, stringsAsFactors = FALSE
  )
  metacheck:::.data_check_write_manifest(
    path, files, want = TRUE, gated = NULL, paper_id = "p1", download = "none",
    max_file_size = 100, max_download_size = 500)

  m <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(unlist(m$code$packages), "dplyr")   # code section survived
  expect_equal(m$provenance$software$name, "metacheck")  # data_check section present
})
