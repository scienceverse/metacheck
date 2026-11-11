# skip online checking in aspredicted_* functions
testthat::local_mocked_bindings(
  online = \(...) TRUE
)

test_that("errors", {
  expect_error(github_info(bad_arg))
  expect_error(github_repo(bad_arg))
  expect_error(github_readme(bad_arg))
  expect_error(github_languages(bad_arg))
  expect_error(github_files(bad_arg))

  repo <- "scienceverse/norepo"
  expect_null(github_repo(repo))
  expect_null(github_info(repo))
  expect_equal(github_readme(repo), "")
  expect_null(github_languages(repo))
  expect_null(github_files(repo))
}, "mock")

test_that("github_links", {
  expect_true(is.function(metacheck::github_links))
  expect_no_error(helplist <- help(github_links, metacheck))

  expect_error(github_links(bad_arg))

  exp <- c("github.com/a/b1",
           "http://github.com/a/b2",
           "https://github.com/a/b3",
           "https://github.com/a/b4.git",
           "https://github.com/a/b5/file",
           "https://github.com/a/b6",
           "https://github.com/a/b7",
           "https://github.com/a/b8",
           "a/b9")
  text <- c("The github repo is a/b9")
  paper <- test_paper(text)
  paper$url <- data.frame(
    href = exp[1:8],
    text_id = 2:9
  )
  obs <- github_links(paper)
  expect_setequal(obs$href, exp)
}, "mock")


test_that(".github_config", {
  expect_true(is.function(metacheck:::.github_config))
  expect_no_error(helplist <- help(.github_config, metacheck))

  h <- .github_config(httr2::request("https://api.github.com"))
  expect_s3_class(h, "httr2_request")
}, "mock")

test_that("github_repo", {
  expect_true(is.function(metacheck::github_repo))
  expect_no_error(helplist <- help(github_repo, metacheck))

  urls <- c(
    "scienceverse/metacheck",
    "https://github.com/scienceverse/metacheck",
    "http://github.com/scienceverse/metacheck.git",
    "https://github.com/scienceverse/metacheck/index"
  )

  for (url in urls) {
    repo <- github_repo(url)
    expect_equal(repo, "scienceverse/metacheck")
  }

  # vectorised
  url <- c("scienceverse/metacheck", "scienceverse/faux")
  repo <- github_repo(url)
  expect_equal(url, repo, ignore_attr = TRUE)

  # vectorised multiple
  url <- c("scienceverse/metacheck",
           "scienceverse/faux",
           "scienceverse/metacheck.git")
  repo <- github_repo(url)
  exp <- c("scienceverse/metacheck" = "scienceverse/metacheck",
           "scienceverse/faux" = "scienceverse/faux",
           "scienceverse/metacheck.git" = "scienceverse/metacheck")
  expect_equal(exp, repo)
}, "mock")

test_that("github_readme", {
  expect_true(is.function(metacheck::github_readme))
  expect_no_error(helplist <- help(github_readme, metacheck))

  readme <- github_readme("scienceverse/metacheck")
  search <- "# metacheck\n\n"
  expect_true(grepl(search, readme, fixed = TRUE))

  repo <- c("scienceverse/metacheck",
            "scienceverse/typo",
            "scienceverse/faux")
  readmes <- github_readme(repo)
  expect_equal(readme, readmes[[1]])
  expect_equal(length(readmes), 3)
  expect_equal(readmes[[2]], "")
}, "mock")

test_that("github_languages", {
  lang <- github_languages("scienceverse/metacheck")
  expect_true("R" %in% lang$language)
  expect_equal(names(lang), c("repo", "language", "bytes"))

  repo <- c("scienceverse/metacheck",
            "scienceverse/typo",
            "scienceverse/faux")
  langs <- github_languages(repo)
  expect_equal(names(langs), c("repo", "language", "bytes"))
  expect_true(repo[[1]] %in% langs$repo)
  expect_false(repo[[2]] %in% langs$repo)
  expect_true(repo[[3]] %in% langs$repo)
}, "mock")

test_that("github_files", {
  expect_true(is.function(metacheck::github_files))
  expect_no_error(helplist <- help(github_files, metacheck))

  # repo name already clean
  repo <- "scienceverse/metacheck"
  files <- github_files(repo)
  expect_equal(names(files), c("repo", "clean_repo", "name", "path", "download_url", "size", "ext", "type"))
  expect_true("README.md" %in% files$name)
  expect_in(files$repo, repo)
  expect_in(files$clean_repo, repo)

  # repo name not clean
  repo <- "https://scienceverse/metacheck"
  files <- github_files(repo)
  expect_in(files$repo, repo)
  expect_in(files$clean_repo, "scienceverse/metacheck")

  # set dir
  tests <- github_files("scienceverse/metacheck", "tests")
  expect_equal(tests$path, c("tests/testthat",
                             "tests/testthat.R"))

  # recursive
  files_f <- github_files("scienceverse/metacheck",
                          ".github",
                          recursive = FALSE)
  files_t <- github_files("scienceverse/metacheck",
                          ".github",
                         recursive = TRUE)
  expect_true(nrow(files_f) < nrow(files_t))
  expect_true("pkgdown.yaml" %in% files_t$name)
  expect_false("pkgdown.yaml" %in% files_f$name)

  # vectorised
  repo <- c("scienceverse/metacheck", "scienceverse/demo")
  files <- github_files(repo)
  expect_in(files$repo, repo)
  expect_in(files$clean_repo, repo)
}, "mock")

test_that("github_info", {
  expect_true(is.function(metacheck::github_info))
  expect_no_error(helplist <- help(github_info, metacheck))

  repo <- "scienceverse/metacheck"
  info <- github_info(repo)

  # files
  exp <- c("repo", "clean_repo", "name", "path", "download_url", "size", "ext", "type")
  expect_equal(names(info$files), exp)
  expect_true("README.md" %in% info$files$name)

  # readme
  search <- "# metacheck"
  expect_true(grepl(search, info$readme, fixed = TRUE))

  # languages
  expect_true("R" %in% info$languages$language)
  expect_equal(names(info$languages), c("repo", "language", "bytes"))
  expect_true(all(repo == info$languages$repo))
}, "mock")


test_that("github_tree_files lists a repo with API requests only, no github.com HEAD", local({
  # Regression test (2026-10-13 survey): github_tree_files() first sent a
  # HEAD request to the github.com page (via github_repo()) although the
  # /repos/{owner}/{repo} request that follows answers the same question.
  # Requests are answered by a mock, so this records every URL and method.
  seen <- character(0)
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      seen <<- c(seen, paste(req$method %||% "GET", req$url))
      if (grepl("/git/trees/", req$url)) {
        body <- list(truncated = FALSE, tree = list(
          list(path = "README.md", type = "blob", size = 12),
          list(path = "R", type = "tree"),
          list(path = "R/a.R", type = "blob", size = 34)))
      } else {
        body <- list(default_branch = "main", license = list(spdx_id = "MIT"))
      }
      httr2::response(status_code = 200,
                      headers = list(`Content-Type` = "application/json"),
                      body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE)))
    },
    .package = "httr2"
  )

  res <- github_tree_files("https://github.com/owner/repo")
  expect_false(res$gated)
  expect_equal(res$license, "MIT")
  expect_equal(res$files$path, c("README.md", "R/a.R"))
  expect_equal(seen, c(
    "GET https://api.github.com/repos/owner/repo",
    "GET https://api.github.com/repos/owner/repo/git/trees/main?recursive=1"))
}))


test_that("github_tree_files reports a 404 from the API as an invalid repository", local({
  seen <- character(0)
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      seen <<- c(seen, req$url)
      httr2::response(status_code = 404)
    },
    .package = "httr2"
  )

  res <- github_tree_files("owner/does-not-exist")
  expect_true(res$gated)
  expect_equal(res$reason, "invalid or inaccessible GitHub repository")
  expect_null(res$files)
  # one API request, and no fallback crawl through github_files()
  expect_equal(seen, "https://api.github.com/repos/owner/does-not-exist")

  # text that is not shaped like owner/repo never reaches the network at all
  seen <- character(0)
  res <- github_tree_files("not a repository")
  expect_true(res$gated)
  expect_length(seen, 0)
}))

test_that("a GitHub account address is reported as an account, not as invalid (#485)", local({
  # 10.1111/2041-210X.13766: "freely downloadable from https://github.com/OneZoom/."
  seen <- character(0)
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      seen <<- c(seen, req$url)
      httr2::response(status_code = 404)
    },
    .package = "httr2"
  )
  for (u in c("https://github.com/OneZoom/", "https://github.com/OneZoom",
              "github.com/OneZoom/.", "https://github.com/orgs/OneZoom")) {
    expect_equal(.github_account_name(u), "OneZoom")
    res <- github_tree_files(u)
    expect_true(res$gated)
    expect_equal(res$reason, "links to a GitHub account, not to a specific repository")
  }
  # no request is needed to tell
  expect_length(seen, 0)

  # a repository named "." does not exist on GitHub
  expect_null(.github_repo_name("https://github.com/OneZoom/."))
  expect_equal(.github_repo_name("https://github.com/a/.github"), "a/.github")
  expect_null(.github_account_name("https://github.com/OneZoom/OZtree"))
  expect_null(.github_account_name("https://github.com/features"))
}))

test_that("github_links keeps a linked GitHub account address (#485)", {
  paper <- test_paper("The code is on GitHub.")
  paper$url <- data.frame(
    href = c("https://github.com/OneZoom/", "https://github.com/a/b", "https://github.com/features"),
    text_id = 1:3
  )
  obs <- github_links(paper)
  expect_setequal(obs$href, c("https://github.com/OneZoom/", "https://github.com/a/b"))
})

test_that("github_files: empty folder and single file; github_languages on an error (U48)", {
  answer <- NULL; status <- 200
  local_mocked_bindings(github_repo = function(repo, ...) "owner/repo")
  local_mocked_bindings(req_perform = function(req, ...) httr2::response_json(status, body = answer),
                        .package = "httr2")
  answer <- list()
  expect_null(github_files("owner/repo"))
  answer <- list(name = "a.csv", path = "a.csv", download_url = "u", type = "file", size = 3)
  expect_equal(github_files("owner/repo", dir = "a.csv")$name, "a.csv")
  answer <- list(message = "Not Found", documentation_url = "x"); status <- 404
  expect_true(is.na(github_languages("owner/repo")$language))
})

test_that("github_links finds hyperlinks in a bibr 12.x paper (U196)", {
  # text_search() searched the url table's first column, the integer url_id
  p <- suppressWarnings(.grobid_to_bibr(demofile("xml"), schema_version = "12.0"))
  expect_true(any(grepl("github.com", github_links(p)$href, fixed = TRUE)))
})

test_that("github_links finds a full link written in the text, once", {
  # a link the PDF did not make clickable is not in the url table; a sentence
  # with a full link was also left out of the search near the word "github"
  paper <- test_paper(c(
    "The code is at https://github.com/owner/repo.",
    "See also github.com/owner/repo/tree/main for the figures.",
    "Analyses: https://github.com/other/analysis."
  ))
  paper$url <- data.frame(href = "https://github.com/other/analysis", text_id = 3L)
  obs <- github_links(paper)
  expect_setequal(obs$href, c("https://github.com/owner/repo",
                              "https://github.com/other/analysis"))
})

test_that("github_links leaves out a text link that repeats a found one wrongly", {
  # the text has the url table's repository with its end repeated, a word of
  # the next sentence attached, or a hyphen lost; a found link cut off at a
  # hyphen is not a whole name, so its full form in the text is kept
  paper <- test_paper(c(
    "Code: https://github.com/owner/Vaccine-PolicyPolicy and more.",
    "Data: https://github.com/owner/throwinglight.The end.",
    "Also https://github.com/owner/attentionclusters here.",
    "Glass: https://github.com/group/ionic-glass-formers here.",
    "New: https://github.com/someone/else here."
  ))
  paper$url <- data.frame(
    href = c("https://github.com/owner/Vaccine-Policy",
             "https://github.com/owner/throwinglight",
             "https://github.com/owner/attention-clusters",
             "https://github.com/group/ionic-"),
    text_id = 1:4)
  obs <- github_links(paper)
  expect_setequal(obs$href, c(paper$url$href,
                              "https://github.com/group/ionic-glass-formers",
                              "https://github.com/someone/else"))
})
