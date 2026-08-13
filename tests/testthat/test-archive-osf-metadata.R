# Fixtures for these tests were recorded from OSF project ezcuj ("Reproducibility
# Project: Psychology"), a public project with two wiki pages and 1,324 log
# entries. It is used only for the wiki, log, and metadata endpoints -- it holds
# far too many files to record those as well, so nothing here downloads files.
# 8uqfb is the contrasting case: a project with no wiki at all.

test_that(".osf_download_wikis writes each wiki as Markdown", {
  meta_dir <- withr::local_tempdir()

  wikis <- .osf_download_wikis("ezcuj", meta_dir)

  expect_s3_class(wikis, "data.frame")
  expect_equal(nrow(wikis), 2)
  expect_setequal(wikis$name, c("home", "Replicated Studies"))

  # One file per page, named after the page. A page name can contain spaces
  # and other characters a file system may not accept, so it goes through
  # path_sanitize() -- "Replicated Studies" becomes "Replicated_Studies".
  expect_setequal(wikis$file,
                  c("wiki_home.md", "wiki_Replicated_Studies.md"))
  for (f in wikis$file) {
    expect_true(file.exists(file.path(meta_dir, f)))
  }

  # Saved as .md, not .txt: the OSF serves wikis as text/markdown, so the
  # headings, links and tables in a page are meaningful and worth keeping.
  body <- readLines(file.path(meta_dir, "wiki_home.md"), warn = FALSE)
  expect_gt(length(body), 1)
  expect_true(any(grepl("\\*\\*|^#|\\[", body)))
}, "mock")


test_that(".osf_download_wikis returns NULL for a project with no wiki", {
  meta_dir <- withr::local_tempdir()

  # 8uqfb has no wiki; the recorded response is an empty listing
  expect_null(.osf_download_wikis("8uqfb", meta_dir))
  expect_equal(list.files(meta_dir), character(0))
}, "mock")


test_that(".osf_download_logs writes the activity log as a CSV", {
  meta_dir <- withr::local_tempdir()

  # ezcuj has 1,324 log entries, far more than the 100 the OSF returns per
  # page. Only page 1 is in the fixtures: httptest2 does not capture
  # req_perform_parallel(), which is what fetches the later pages (see the note
  # at the top of utils.R), so the replayed listing is legitimately short and
  # osf_get_all_pages() correctly says so. The warning is expected here, and is
  # itself worth asserting -- an incomplete listing must never pass silently.
  expect_warning(logs <- .osf_download_logs("ezcuj", meta_dir),
                 "listed only 100 of the 1324")

  expect_s3_class(logs, "data.frame")
  expect_gt(nrow(logs), 0)
  expect_true(all(c("date", "action") %in% names(logs)))

  # A log is tabular, so it is written as a CSV that can be sorted and
  # filtered rather than as prose.
  path <- file.path(meta_dir, "logs.csv")
  expect_true(file.exists(path))

  from_disk <- readr::read_csv(path, show_col_types = FALSE)
  expect_equal(nrow(from_disk), nrow(logs))
  expect_true(all(c("date", "action") %in% names(from_disk)))

  # every entry says when it happened and what happened
  expect_false(any(is.na(logs$action)))
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}", logs$date)))
}, "mock")


test_that(".osf_node_metadata collects the project's descriptive metadata", {
  meta <- .osf_node_metadata("ezcuj")

  expect_type(meta, "list")
  expect_equal(meta$osf_id, "ezcuj")
  expect_equal(meta$osf_url, "https://osf.io/ezcuj")
  expect_equal(meta$title, "Reproducibility Project: Psychology")
  expect_true(isTRUE(meta$public))

  # contributors are people, with a name and possibly an ORCID
  expect_gt(length(meta$contributors), 0)
  expect_true(all(vapply(meta$contributors,
                         \(c) nzchar(c$name %||% ""), logical(1))))

  expect_true(all(c("date_created", "date_modified", "retrieved") %in%
                    names(meta)))
}, "mock")


test_that(".osf_metadata_download writes the whole metadata folder", {
  download_to <- withr::local_tempdir()

  # See the log test above for why an incomplete-listing warning is expected.
  expect_warning(meta_dir <- .osf_metadata_download("ezcuj", download_to),
                 "listed only 100 of the 1324")

  expect_equal(basename(meta_dir), "_osf_metadata")
  expect_setequal(list.files(meta_dir),
                  c("wiki_home.md", "wiki_Replicated_Studies.md",
                    "logs.csv", "metadata.json", "README.md"))

  # metadata.json is the complete record, readable back as a list
  meta <- jsonlite::read_json(file.path(meta_dir, "metadata.json"))
  expect_equal(meta$osf_id, "ezcuj")
  expect_equal(meta$title, "Reproducibility Project: Psychology")

  # README.md is what a person finds when they open the folder, so it names
  # the project and says what else is in there
  readme <- readLines(file.path(meta_dir, "README.md"), warn = FALSE)
  expect_true(any(grepl("Reproducibility Project", readme)))
  expect_true(any(grepl("wiki_home.md", readme)))
  expect_true(any(grepl("logs.csv", readme)))
  expect_true(any(grepl("metadata.json", readme)))

  # both wiki pages are counted, not just the first
  expect_true(any(grepl("2 wiki pages", readme)))
}, "mock")


test_that(".osf_metadata_download states when a project has no wiki", {
  download_to <- withr::local_tempdir()

  expect_warning(meta_dir <- .osf_metadata_download("8uqfb", download_to),
                 "listed only 100 of the 137")

  # logs.csv is written even when empty, so a missing file always means
  # something went wrong rather than "this project had none"
  expect_true(file.exists(file.path(meta_dir, "logs.csv")))
  expect_true(file.exists(file.path(meta_dir, "metadata.json")))

  # and the README says so in words, so "had no wiki" is never confused with
  # "the wiki was not retrieved"
  readme <- readLines(file.path(meta_dir, "README.md"), warn = FALSE)
  expect_true(any(grepl("none \\(this project has no wiki\\)", readme)))
}, "mock")


test_that(".osf_metadata_download handles an unreachable project", {
  download_to <- withr::local_tempdir()

  # An ID that is not a valid OSF GUID never reaches the API
  expect_null(suppressWarnings(
    .osf_metadata_download("notanid", download_to)))
})
