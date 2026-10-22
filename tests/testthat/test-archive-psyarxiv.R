# PsyArXiv's own API (R/archive-psyarxiv.R). The API answers are recorded in
# apis/next.psyarxiv.org/ (record again with mode "capture").
mode <- "mock"

test_that("exists", {
  for (f in c("psyarxiv_search", "psyarxiv_info", "psyarxiv_versions",
              "psyarxiv_author", "psyarxiv_download", "psyarxiv_dumps",
              "psyarxiv_dump", "psyarxiv_subjects", "psyarxiv_licenses")) {
    expect_true(is.function(get(f, asNamespace("metacheck"))))
  }
  expect_equal(getOption("metacheck.psyarxiv.api"),
               "https://next.psyarxiv.org/api/v1")
})

test_that(".psyarxiv_id", {
  ids <- c("28j7s", " 28j7s ",
           "https://next.psyarxiv.org/preprint/28j7s",
           "https://osf.io/preprints/psyarxiv/28j7s",
           "https://osf.io/preprints/psyarxiv/28j7s_v2/",
           "https://psyarxiv.com/28j7s",
           "https://psyarxiv.com/28j7s/download",
           "10.31234/osf.io/28j7s",
           "10.31234/osf.io/28j7s_v3",
           "https://doi.org/10.31234/osf.io/28j7s_v3")
  expect_equal(.psyarxiv_id(ids), rep("28j7s", length(ids)))
})

test_that(".psyarxiv_abs", {
  expect_equal(.psyarxiv_abs(c("/preprint/28j7s", "https://osf.io/x", NA)),
               c("https://next.psyarxiv.org/preprint/28j7s", "https://osf.io/x", NA))
})

test_that(".psyarxiv_row with missing fields", {
  row <- .psyarxiv_row(list(id = "abcde", title = "T", authors = list(),
                            subjects = list(), tags = list()))
  expect_equal(nrow(row), 1)
  expect_equal(row$psyarxiv_id, "abcde")
  expect_true(is.na(row$authors))
  expect_true(is.na(row$data_status))
  expect_equal(nrow(.psyarxiv_rows(list())), 0)
  expect_equal(names(.psyarxiv_rows(list())), names(row))
})

test_that("psyarxiv_search", {
  pp <- psyarxiv_search("title:replication AND priming", per_page = 2)
  expect_equal(nrow(pp), 2)
  expect_true(attr(pp, "total") > 2)
  expect_equal(attr(pp, "interpreted"), 'title : "replication" AND "priming"')
  expect_true(all(grepl("^https://next.psyarxiv.org/preprint/", pp$url)))
  expect_true(all(is.na(pp$data_status)))  # only in a single record

  # paging follows links.next
  pp2 <- psyarxiv_search("title:replication AND priming", per_page = 2, page_end = 2)
  expect_equal(nrow(pp2), 4)
  expect_equal(pp2$psyarxiv_id[1:2], pp$psyarxiv_id)

  # filters
  pd <- psyarxiv_search(has_data = TRUE, year_from = 2026, per_page = 3)
  expect_true(all(pd$has_data))
  expect_true(all(substr(pd$first_posted, 1, 4) == "2026"))

  expect_error(psyarxiv_search(sort = "bogus"))
  expect_error(psyarxiv_search(state = "bogus"))
}, mode)

test_that("psyarxiv_info", {
  pp <- psyarxiv_info(c("pr4u6", "https://osf.io/preprints/psyarxiv/28j7s",
                        "10.31234/osf.io/pr4u6_v7", "zzzzz"))
  expect_equal(pp$psyarxiv_id, c("pr4u6", "28j7s", "pr4u6", "zzzzz"))
  expect_equal(pp$data_status[1], "available")
  expect_equal(pp$supplements_url[1], "https://osf.io/e57y4/")
  expect_equal(pp$supplements_status[1], "public")
  expect_true(grepl("^https://next.psyarxiv.org/files/", pp$file_url[1]))
  expect_true(pp$n_versions[1] >= 7)
  expect_true(all(is.na(pp$error[1:3])))
  expect_match(pp$error[4], "No such preprint")
}, mode)

test_that("psyarxiv_versions", {
  v <- psyarxiv_versions("pr4u6")
  expect_true(nrow(v) >= 7)
  expect_true(all(v$psyarxiv_id == "pr4u6"))
  # version 1 keeps the DOI without "_v1"
  expect_true(all(grepl("^10.31234/osf.io/pr4u6(_v\\d+)?$", v$doi)))
  expect_true(all(c("file_url", "osf_file_url", "file_revisions") %in% names(v)))
}, mode)

test_that("psyarxiv_author", {
  a <- psyarxiv_author("osf:xu9vw")
  expect_equal(a$author_id, "osf:xu9vw")
  expect_equal(a$name, "Yung-Jung Chen")
  # a bare OSF guid is prefixed with osf:
  expect_equal(psyarxiv_author("xu9vw")$author_id, "osf:xu9vw")
  expect_error(psyarxiv_author("osf:nonexistent"), "No such author")
}, mode)

test_that("psyarxiv_subjects, psyarxiv_licenses, psyarxiv_dumps", {
  s <- psyarxiv_subjects()
  expect_true(nrow(s) > 200)
  expect_equal(sort(unique(s$depth)), 0:2)
  expect_true(all(is.na(s$parent_id[s$depth == 0])))

  l <- psyarxiv_licenses()
  expect_true("CC BY 4.0" %in% l$short_name)

  d <- psyarxiv_dumps()
  expect_true(2016 %in% d$year)
  expect_true(all(grepl("^https://next.psyarxiv.org/api/v1/dumps/", d$url)))
}, mode)

test_that("psyarxiv_dump and psyarxiv_download (live)", {
  skip_on_cran()
  skip_if_offline("next.psyarxiv.org")
  td <- withr::local_tempdir()

  pp <- psyarxiv_dump(2016, save_path = td)
  expect_equal(nrow(pp), psyarxiv_dumps()$records[psyarxiv_dumps()$year == 2016])
  expect_true(file.exists(file.path(td, "psyarxiv-2016.jsonl.gz")))
  expect_equal(names(pp), names(psyarxiv_search(per_page = 1)))

  f <- psyarxiv_download(c("pr4u6", "10.31234/osf.io/pr4u6_v1"), save_path = td)
  expect_match(basename(f[1]), "^pr4u6_v\\d+\\.pdf$")
  expect_equal(basename(f[2]), "pr4u6_v1.pdf")
  expect_equal(rawToChar(readBin(f[1], "raw", 5)), "%PDF-")
  expect_warning(psyarxiv_download("zzzzz", save_path = td), "No such preprint")
})
