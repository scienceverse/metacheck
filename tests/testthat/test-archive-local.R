fixture_dir <- normalizePath("_fixtures/code_files")

# no_paper() ----

test_that("no_paper", {
  expect_true(is.function(metacheck::no_paper))
  expect_no_error(help(no_paper, metacheck))

  p <- no_paper()
  expect_s3_class(p, "scivrs_paper")
  expect_equal(p$paper_id, "local")

  p2 <- no_paper(id = "my_project")
  expect_equal(p2$paper_id, "my_project")

  # paper_id() must return a length-1 string, not character(0)
  expect_equal(paper_id(p), "local")
})


# local_files() ----

test_that("local_files exists", {
  expect_true(is.function(metacheck::local_files))
  expect_no_error(help(local_files, metacheck))
})

test_that("local_files errors", {
  # undefined variable triggers R error before function runs
  expect_error(local_files(bad_arg))

  # path does not exist
  expect_error(local_files("/no/such/path/exists/anywhere"),
               regexp = "No such file|cannot find|not a directory",
               ignore.case = TRUE)

  # path exists but is a file, not a directory
  tmp_file <- withr::local_tempfile()
  writeLines("x", tmp_file)
  expect_error(local_files(tmp_file), regexp = "not a directory",
               ignore.case = TRUE)
})

test_that("local_files empty directory", {
  tmp <- withr::local_tempdir()
  result <- local_files(tmp)

  expect_s3_class(result, "data.frame")
  expect_equal(names(result),
               c("repo_url", "file_name", "file_url",
                 "file_location", "file_size", "file_type"))
  expect_equal(nrow(result), 0)
})

test_that("local_files column structure", {
  result <- local_files(fixture_dir)

  expect_s3_class(result, "data.frame")
  expect_equal(names(result),
               c("repo_url", "file_name", "file_url",
                 "file_location", "file_size", "file_type"))

  # repo_url is the normalised root path for every row
  expect_true(all(result$repo_url == normalizePath(fixture_dir)))

  # file_name is the basename of file_location
  expect_equal(result$file_name, basename(result$file_location))

  # file_url is always NA (no remote URL for local files)
  expect_true(all(is.na(result$file_url)))

  # every file_location path actually exists on disk
  expect_true(all(file.exists(result$file_location)))

  # file_size is numeric and positive for non-empty files
  expect_type(result$file_size, "double")
  expect_true(all(result$file_size > 0))
})

test_that("local_files finds files recursively", {
  result <- local_files(fixture_dir)

  # top-level files
  expect_true("analysis.R" %in% result$file_name)
  expect_true("analysis_no_comments.R" %in% result$file_name)
  expect_true("data.csv" %in% result$file_name)
  expect_true("README.md" %in% result$file_name)

  # file in subdirectory
  expect_true("helper.R" %in% result$file_name)

  expect_equal(nrow(result), 7)
})

test_that("local_files file type detection", {
  result <- local_files(fixture_dir)

  r_row     <- result[result$file_name == "analysis.R", ]
  csv_row   <- result[result$file_name == "data.csv", ]
  readme_row <- result[result$file_name == "README.md", ]

  expect_equal(r_row$file_type,      "code")
  expect_equal(csv_row$file_type,    "data")
  expect_equal(readme_row$file_type, "readme")
})

test_that("local_files vectorised input", {
  tmp1 <- withr::local_tempdir()
  tmp2 <- withr::local_tempdir()
  writeLines("# script\nlibrary(dplyr)", file.path(tmp1, "script1.R"))
  writeLines("x,y\n1,2",               file.path(tmp2, "data.csv"))

  result <- local_files(c(tmp1, tmp2))

  expect_equal(nrow(result), 2)
  expect_true("script1.R" %in% result$file_name)
  expect_true("data.csv"  %in% result$file_name)

  # each file carries its own repo_url
  expect_equal(result$repo_url[result$file_name == "script1.R"], normalizePath(tmp1))
  expect_equal(result$repo_url[result$file_name == "data.csv"],  normalizePath(tmp2))
})

test_that("repo_check vector of local paths", {
  tmp1 <- withr::local_tempdir()
  tmp2 <- withr::local_tempdir()
  writeLines("# script\nlibrary(dplyr)\n", file.path(tmp1, "analysis.R"))
  writeLines("# Code Files\n",             file.path(tmp2, "README.md"))

  mo <- module_run(no_paper(), "repo_check", local_path = c(tmp1, tmp2))

  expect_equal(mo$summary_table$repo_n, 2)
  expect_true("analysis.R" %in% mo$table$file_name)
  expect_true("README.md"  %in% mo$table$file_name)
})

test_that("local_files file without extension gets NA type", {
  tmp <- withr::local_tempdir()
  writeLines("content", file.path(tmp, "Makefile"))

  result <- local_files(tmp)
  make_row <- result[result$file_name == "Makefile", ]
  expect_true(is.na(make_row$file_type))
})


# repo_check() + local_path ----

test_that("repo_check local_path only", {
  mo <- module_run(no_paper(), "repo_check", local_path = fixture_dir)

  # one repo: the local folder
  expect_equal(mo$summary_table$repo_n, 1)
  expect_equal(mo$summary_table$files_n, 7)
  expect_equal(mo$summary_table$files_code, 3)  # .do is "stats" type, not "code"
  expect_equal(mo$summary_table$files_data, 1)
  expect_equal(mo$summary_table$files_readme, 1)
  expect_equal(mo$summary_table$files_zip, 0)

  # repo type is "local"
  expect_true("analysis.R" %in% mo$table$file_name)
  expect_true("README.md" %in% mo$table$file_name)

  # README present, no zip: green
  expect_equal(mo$traffic_light, "green")
})

httptest2::use_mock_api()

test_that("repo_check paper + local_path", {
  # osf/629bx mock: 4 files (2 code, 1 data, 0 readme, 1 zip)
  # fixture_dir:    5 files (3 code, 1 data, 1 readme, 0 zip)
  paper <- test_paper(url = "https://osf.io/629bx")
  mo <- module_run(paper, "repo_check", local_path = fixture_dir)

  # two repos: OSF + local
  expect_equal(mo$summary_table$repo_n, 2)
  expect_equal(mo$summary_table$files_n, 11)

  # combined counts
  expect_equal(mo$summary_table$files_code, 5)  # .do is "stats" type, not "code"
  expect_equal(mo$summary_table$files_readme, 1)  # only local has README
  expect_equal(mo$summary_table$files_zip, 1)      # only OSF has zip

  # files from both sources present in table
  expect_true("README.md" %in% mo$table$file_name)   # local
  expect_true("analysis.R" %in% mo$table$file_name)  # local
  expect_true(any(grepl("bad\\.R$", mo$table$file_name, ignore.case = TRUE))) # OSF

  # OSF still has no README → repo_no_readme > 0 → yellow
  expect_equal(mo$traffic_light, "yellow")
})

test_that("code_check paper + local_path", {
  # OSF 629bx has 2 code files; fixture_dir has 5 (3 R + 2 Stata) → total 7
  paper <- test_paper(url = "https://osf.io/629bx")
  mo <- module_run(paper, "code_check", local_path = fixture_dir)

  expect_equal(mo$summary_table$code_n, 7)

  # code files from both repos in the table
  expect_true("analysis.R" %in% mo$table$file_name)  # local
  expect_true("bad.R" %in% mo$table$file_name)        # OSF

  # two distinct repo_url values
  expect_setequal(
    unique(mo$table$repo_url),
    c("https://osf.io/629bx", normalizePath(fixture_dir))
  )
})

httptest2::stop_mocking()


# code_check() + local_path ----

test_that("code_check reads non-UTF-8 encoded files without NA", {
  # stata_latin1.do: Windows-1252 encoded (non-ASCII bytes invalid in UTF-8)
  # stata_utf16.do:  UTF-16 LE encoded (NUL bytes after every ASCII char)
  # Both previously produced code_lines=0 and percentage_comment=NA
  mo <- module_run(no_paper(), "code_check", local_path = fixture_dir)

  for (fname in c("stata_latin1.do", "stata_utf16.do")) {
    row <- mo$table[mo$table$file_name == fname, ]
    expect_true(nrow(row) == 1, label = paste(fname, "found in table"))
    expect_true(row$code_lines > 0, label = paste(fname, "code_lines > 0"))
    expect_false(is.na(row$percentage_comment), label = paste(fname, "percentage_comment not NA"))
  }
})

test_that("code_check local_path errors", {
  # non-existent path propagates as an error
  expect_error(
    module_run(no_paper(), "code_check", local_path = "/no/such/path/exists")
  )
})

test_that("code_check local_path no code files", {
  tmp <- withr::local_tempdir()
  writeLines("x,y\n1,2", file.path(tmp, "data.csv"))

  mo <- module_run(no_paper(), "code_check", local_path = tmp)

  expect_equal(mo$traffic_light, "na")
  expect_equal(mo$summary_table$code_file_n, 0)
})

test_that("code_check local_path finds code files", {
  mo <- module_run(no_paper(), "code_check", local_path = fixture_dir)

  expect_equal(mo$traffic_light, "yellow")
  # fixture has analysis.R, analysis_no_comments.R, subdir/helper.R, stata_latin1.do, stata_utf16.do
  expect_equal(mo$summary_table$code_n, 5)
  expect_true(all(c("analysis.R", "analysis_no_comments.R", "helper.R",
                    "stata_latin1.do", "stata_utf16.do") %in% mo$table$file_name))
})

test_that("code_check local_path: present files are not flagged missing", {
  mo <- module_run(no_paper(), "code_check", local_path = fixture_dir)

  # analysis.R loads data.csv, which IS in the fixture dir
  analysis_row <- mo$table[mo$table$file_name == "analysis.R", ]
  expect_equal(analysis_row$loaded_files_missing, 0)
})

test_that("code_check local_path: absent files are flagged missing", {
  mo <- module_run(no_paper(), "code_check", local_path = fixture_dir)

  # analysis_no_comments.R loads missing_file.csv, which is NOT in the fixture dir
  no_comments_row <- mo$table[mo$table$file_name == "analysis_no_comments.R", ]
  expect_equal(no_comments_row$loaded_files_missing, 1)
  expect_match(no_comments_row$loaded_files_missing_names, "missing_file.csv")
})

test_that("code_check local_path: files without comments are flagged", {
  mo <- module_run(no_paper(), "code_check", local_path = fixture_dir)

  no_comments_row <- mo$table[mo$table$file_name == "analysis_no_comments.R", ]
  expect_equal(no_comments_row$percentage_comment, 0)
})
