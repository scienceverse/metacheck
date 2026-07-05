# Tests for corpus-level archive reporting: write_file_manifest() (file
# inventory from per-paper manifests) and capture/collect_check_results()
# (check outcomes rolled up across papers). All offline.

test_that("write_file_manifest flattens per-paper manifests to one inventory", {
  withr::local_options(metacheck.llm.use = FALSE)
  man_dir <- withr::local_tempdir()

  # Two per-paper manifests: one with files (some downloaded, some skipped),
  # one for a paper with no repo. Written by the real manifest writer.
  f1 <- data.frame(
    repo_url = "https://osf.io/aaaaa",
    file_name = c("got.csv", "big.rdata"),
    file_path = c("got.csv", "big.rdata"),
    file_url = "https://osf.io/download/x/",
    file_size = c(100, 5e8), data_type = "data", data_format = "tabular",
    file_location = c(withr::local_tempfile(fileext = ".csv"), NA))
  writeLines("a,b", f1$file_location[1])
  metacheck:::.data_check_write_manifest(
    man_dir, f1, want = c(TRUE, TRUE), gated = NULL, paper_id = "p1",
    download = "all", max_file_size = 100, max_download_size = 500,
    oversize = data.frame(repo_url = "https://osf.io/aaaaa",
                          file_name = "big.rdata", file_size = 5e8))
  metacheck:::.data_check_write_manifest(
    man_dir, f1[0, ], want = logical(0), gated = NULL, paper_id = "p2",
    download = "all", max_file_size = 100, max_download_size = 500)

  inv <- write_file_manifest(man_dir, out = NA)   # return only, no write
  expect_s3_class(inv, "data.frame")
  expect_setequal(unique(inv$paper_id), c("p1", "p2"))
  expect_true(all(c("paper_id", "file_name", "downloaded", "status",
                    "skip_reason") %in% names(inv)))
  # p1 has one downloaded + one skipped; p2 is the no-repo placeholder row.
  expect_equal(sum(inv$paper_id == "p1"), 2)
  expect_equal(inv$status[inv$paper_id == "p2"], "no_repo")
  expect_true(any(grepl("max_file_size", inv$skip_reason %||% "")))

  # Default writes _all_files.csv into the manifest dir.
  suppressMessages(write_file_manifest(man_dir))
  expect_true(file.exists(file.path(man_dir, "_all_files.csv")))
})

test_that("capture_check_results + collect_check_results round-trip", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- withr::local_tempdir()
  dir.create(file.path(d, "data"), showWarnings = FALSE)
  utils::write.csv(
    data.frame(id = 1:6, score = c(2, 3, 2, 99, 3, 2)),
    file.path(d, "data", "s.csv"), row.names = FALSE)

  chain <- suppressWarnings(report_module_run(
    test_paper("cr1"), c("repo_check", "data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE))))

  rdir <- withr::local_tempdir()
  path <- capture_check_results(chain, rdir, paper_id = "cr1")
  expect_true(file.exists(path))
  expect_match(basename(path), "^cr1\\.checks\\.json$")

  res <- collect_check_results(rdir)
  # One row per module in the chain.
  expect_setequal(res$checks$module,
                  c("repo_check", "data_check", "data_validate"))
  # Single, consistent paper id everywhere (not a per-module hash).
  expect_equal(unique(res$checks$paper_id), "cr1")
  expect_true(all(res$findings$paper_id == "cr1"))
  # Count columns are unpacked from the summary_table (e.g. data_check counts).
  expect_true("data_file_n" %in% names(res$checks))
  # traffic lights are carried through.
  expect_true(all(res$checks$traffic_light %in%
                    c("green","yellow","red","info","na","fail")))
  # CSVs written.
  expect_true(file.exists(file.path(rdir, "_all_checks.csv")))
  expect_true(file.exists(file.path(rdir, "_all_findings.csv")))
})

test_that("collect_check_results aggregates several papers", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- withr::local_tempdir()
  dir.create(file.path(d, "data"), showWarnings = FALSE)
  utils::write.csv(data.frame(id = 1:5, x = c(1, 2, 3, 4, 5)),
                   file.path(d, "data", "s.csv"), row.names = FALSE)
  rdir <- withr::local_tempdir()
  for (pid in c("a1", "a2", "a3")) {
    chain <- suppressWarnings(report_module_run(
      test_paper(pid), c("repo_check", "data_check"),
      args = list(data_check = list(local_path = d, local_only = TRUE))))
    capture_check_results(chain, rdir, paper_id = pid)
  }
  res <- collect_check_results(rdir)
  expect_setequal(unique(res$checks$paper_id), c("a1", "a2", "a3"))
  expect_equal(nrow(res$checks), 6)   # 3 papers x 2 modules
})

test_that("the corpus functions warn (not error) on an empty directory", {
  empty <- withr::local_tempdir()
  expect_warning(inv <- write_file_manifest(empty), "No .*manifest")
  expect_equal(nrow(inv), 0)
  expect_warning(res <- collect_check_results(empty), "No .*checks")
  expect_equal(nrow(res$checks), 0)
})
