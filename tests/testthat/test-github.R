test_that("exists", {
  expect_true(is.function(metacheck::github_info))
  expect_no_error(helplist <- help(github_info, metacheck))

  expect_true(is.function(metacheck::github_repo))
  expect_no_error(helplist <- help(github_repo, metacheck))

  expect_true(is.function(metacheck::github_readme))
  expect_no_error(helplist <- help(github_readme, metacheck))

  expect_true(is.function(metacheck::github_files))
  expect_no_error(helplist <- help(github_files, metacheck))

  expect_true(is.function(metacheck::github_config))
  expect_no_error(helplist <- help(github_config, metacheck))
})

test_that("errors", {
  expect_error(github_info(bad_arg))
  expect_error(github_repo(bad_arg))
  expect_error(github_readme(bad_arg))
  expect_error(github_languages(bad_arg))
  expect_error(github_files(bad_arg))

  skip_on_cran()
  skip_if_offline("github.com")

  repo <- "scienceverse/norepo"
  expect_null(github_repo(repo))
  expect_null(github_info(repo))
  expect_null(github_readme(repo))
  expect_null(github_languages(repo))
  expect_null(github_files(repo))
})

test_that("github_repo", {
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
})

test_that("github_config", {
  h <- github_config()
  expect_equal(class(h), "request")
})

test_that("github_readme", {
  skip_if_offline("github.com")

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

  files <- github_files("scienceverse/metacheck")
  expect_equal(names(files), c("name", "path", "size", "ext", "type"))
  expect_true("metacheck.Rproj" %in% files$name)

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
})

test_that("github_info", {
  skip_if_offline("github.com")

  info <- github_info("scienceverse/metacheck")

  # files
  expect_equal(names(info$files), c("name", "path", "size", "ext", "type"))
  expect_true("metacheck.Rproj" %in% info$files$name)

  # readme
  search <- "# metacheck"
  expect_true(grepl(search, info$readme, fixed = TRUE))

  # languages
  expect_true("R" %in% info$languages$language)
  expect_equal(names(info$languages), c("language", "bytes"))
})
