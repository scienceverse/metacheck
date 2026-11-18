test_that("github_tree_files encodes each part of a file's path in its address (#667)", local({
  # a GitHub answer with paths that have a space, "#", "?", "%" and "+"
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      body <- if (grepl("/git/trees/", req$url)) {
        list(truncated = FALSE, tree = list(
          list(path = "data/screened/henklain et al 2019/Script.R", type = "blob", size = 10),
          list(path = "a#b/c?d%e+f.csv", type = "blob", size = 10),
          list(path = "plain/file.R", type = "blob", size = 10)))
      } else list(default_branch = "main")
      httr2::response(status_code = 200, headers = list(`Content-Type` = "application/json"),
                      body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE)))
    },
    .package = "httr2")
  f <- github_tree_files("owner/repo")$files
  base <- "https://raw.githubusercontent.com/owner/repo/main/"
  expect_equal(f$download_url, paste0(base, c("data/screened/henklain%20et%20al%202019/Script.R",
                                              "a%23b/c%3Fd%25e%2Bf.csv", "plain/file.R")))
  # the path and name stay as they are: the cache and the code's paths use them
  expect_equal(f$path[1], "data/screened/henklain et al 2019/Script.R")
}))

test_that("a GitHub file whose path has spaces downloads, also from an address cached before the fix (#667)", {
  skip_if_quick()
  skip_api("https://github.com/ianhussey/repository-of-published-irap-data")
  # an address with spaces, as listings built them before 0.4.4.12 and as
  # the lasting listing cache may still hold them
  url <- "https://raw.githubusercontent.com/ianhussey/repository-of-published-irap-data/main/data/screened/henklain et al 2019/Script.R"
  d <- withr::local_tempfile(fileext = ".R")
  .download_one(url, d)
  expect_true(file.exists(d))
  expect_gt(file.size(d), 0)
})

test_that("files whose download failed are named in the report and counted (#667)", {
  f <- data.frame(repo_url = "https://osf.io/abcde", file_name = c("statcheck output.csv", "notes.txt"),
                  error = c("Failed to parse URL: Malformed input to a URL function\nmore", "HTTP 503"))
  x <- .download_failed_text(f)
  expect_equal(x$report[1], "#### Files that could not be downloaded")
  expect_match(x$report[2], "2 files could not be downloaded, so the checks did not see them", fixed = TRUE)
  expect_match(x$report[2], "Malformed input to a URL function (1)", fixed = TRUE)
  expect_null(.download_failed_text(NULL)$report)
  # code_check with a code file at an address that refuses connections
  p <- test_paper()
  tbl <- data.frame(paper_id = p$paper_id, repo_url = "https://github.com/owner/repo",
                    repo_type = "github", file_name = "analysis.R", file_path = "analysis.R",
                    file_url = "http://127.0.0.1:9/analysis.R", file_size = 100, file_type = "code",
                    file_location = NA_character_)
  prev <- structure(list(paper = p, module = "repo_check", table = tbl,
                         summary_table = data.frame(paper_id = p$paper_id)),
                    class = "metacheck_module_output")
  mo <- module_run(prev, "code_check")
  expect_equal(mo$summary_table$code_files_failed, 1L)
  expect_match(paste(unlist(mo$report), collapse = " "), "Files that could not be downloaded", fixed = TRUE)
})

test_that("every request made with .mc_request() encodes its address, and leaves an encoded one as it is (#667)", {
  expect_equal(.mc_request("https://example.org/data raw.zip")$url,
               "https://example.org/data%20raw.zip")
  u <- "https://api.osf.io/v2/nodes/abcde/files/?page[size]=100&filter[name]=a"
  expect_equal(.mc_request(u)$url, u)
  expect_equal(.mc_request("https://example.org/data%20raw.zip")$url,
               "https://example.org/data%20raw.zip")
})

test_that("members of a zip whose address has spaces are fetched by byte range (#667)", {
  skip_if_quick()
  skip_api("https://github.com/ianhussey/repository-of-published-irap-data")
  # the zip of metapsych.mp20234008 whose 432 members all failed in 0.4.4.13:
  # .http_range_bytes() sent the address without encoding it
  arc <- "https://raw.githubusercontent.com/ianhussey/repository-of-published-irap-data/main/data/screened/van der kaap et al - 2018/other/data raw.zip"
  cd <- zip_peek(arc)
  d <- withr::local_tempdir()
  r <- .zip_fetch_members(arc, names = cd$name[1:2], dest = d)
  expect_true(all(r$ok))
  expect_length(list.files(d, recursive = TRUE), 2)
})
