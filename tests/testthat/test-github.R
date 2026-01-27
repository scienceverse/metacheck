# httptest::start_capturing()
# httptest::use_mock_api()

test_that("errors", {
  expect_error(github_info(bad_arg))
  expect_error(github_repo(bad_arg))
  expect_error(github_readme(bad_arg))
  expect_error(github_languages(bad_arg))
  expect_error(github_files(bad_arg))

  skip_if_offline("github.com")

  repo <- "scienceverse/norepo"
  expect_null(github_repo(repo))
  expect_null(github_info(repo))
  expect_equal(github_readme(repo), "")
  expect_null(github_languages(repo))
  expect_null(github_files(repo))
})

test_that("github_links", {
  expect_true(is.function(metacheck::github_links))
  expect_no_error(helplist <- help(github_links, metacheck))

  expect_error(github_links(bad_arg))

  paper <- paper()
  paper$full_text <- data.frame(
    id = paper$id,
    text = c("No https:  github.com/a/b1",
             "Oops: http://github.com/a/b2/",
             "It's at https://github.com/a/b3/.",
             "check out https://github.com/a/b4.git",
             "I named it https://github.com/a/b5/file. which is dumb.",
             "markdown: [GitHub](https://github.com/a/b6)",
             "md: <https://github.com/a/b7>",
             "html: <a href=\"https://github.com/a/b8\"",
             "The github repo is a/b9")
  )
  obs <- github_links(paper)
  exp <- c("github.com/a/b1",
           "http://github.com/a/b2",
           "https://github.com/a/b3",
           "https://github.com/a/b4.git",
           "https://github.com/a/b5/file.",
           "https://github.com/a/b6",
           "https://github.com/a/b7",
           "https://github.com/a/b8",
           "a/b9")
  expect_setequal(obs$text, exp)
})


test_that("github_config", {
  expect_true(is.function(metacheck::github_config))
  expect_no_error(helplist <- help(github_config, metacheck))

  h <- github_config()
  expect_equal(class(h), "request")
})

test_that("github_repo", {
  expect_true(is.function(metacheck::github_repo))
  expect_no_error(helplist <- help(github_repo, metacheck))

  skip_if_offline("github.com")

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
})

test_that("github_readme", {
  skip_if_offline("github.com")

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
})

test_that("github_languages", {
  skip_if_offline("github.com")

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
})

test_that("github_files", {
  skip_if_offline("github.com")

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
})

test_that("github_info", {
  skip_if_offline("github.com")
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
})

# httptest::stop_mocking()
# httptest::stop_capturing()
