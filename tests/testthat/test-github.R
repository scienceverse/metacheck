test_that("exists", {
  expect_true(is.function(papercheck::github_info))
  expect_no_error(helplist <- help(github_info, papercheck))

  expect_true(is.function(papercheck::github_repo))
  expect_no_error(helplist <- help(github_repo, papercheck))

  expect_true(is.function(papercheck::github_readme))
  expect_no_error(helplist <- help(github_readme, papercheck))

  expect_true(is.function(papercheck::github_files))
  expect_no_error(helplist <- help(github_files, papercheck))

  expect_true(is.function(papercheck::github_config))
  expect_no_error(helplist <- help(github_config, papercheck))
})

test_that("errors", {
  expect_error(github_info(bad_arg))
  expect_error(github_repo(bad_arg))
  expect_error(github_readme(bad_arg))
  expect_error(github_languages(bad_arg))
  expect_error(github_files(bad_arg))

  skip_on_cran()
  skip_if_offline("github.com")

  # expect_error(github_repo("scienceverse/norepo"))
  # expect_error(github_info("scienceverse/norepo"))
  # expect_error(github_readme("scienceverse/norepo"))
  # expect_error(github_languages("scienceverse/norepo"))
  # expect_error(github_files("scienceverse/norepo"))
})

test_that("github_repo", {
  skip_if_offline("github.com")

  urls <- c(
    "scienceverse/papercheck",
    "https://github.com/scienceverse/papercheck",
    "http://github.com/scienceverse/papercheck.git",
    "https://github.com/scienceverse/papercheck/index"
  )

  for (url in urls) {
    repo <- github_repo(url)
    expect_equal(repo, "scienceverse/papercheck")
  }
})

test_that("github_config", {
  h <- github_config()
  expect_equal(class(h), "request")
})

test_that("github_readme", {
  skip_if_offline("github.com")

  readme <- github_readme("scienceverse/papercheck")
  search <- "# papercheck\n\n"
  expect_true(grepl(search, readme, fixed = TRUE))
})

test_that("github_languages", {
  skip_if_offline("github.com")

  lang <- github_languages("scienceverse/papercheck")
  expect_true("R" %in% lang$language)
  expect_equal(names(lang), c("language", "bytes"))
})

test_that("github_files", {
  skip_if_offline("github.com")

  files <- github_files("scienceverse/papercheck")
  expect_equal(names(files), c("name", "path", "size", "ext", "type"))
  expect_true("papercheck.Rproj" %in% files$name)

  # set dir
  tests <- github_files("scienceverse/papercheck", "tests")
  expect_equal(tests$path, c("tests/testthat",
                             "tests/testthat.R"))

  # recursive
  files_f <- github_files("scienceverse/papercheck",
                          ".github",
                          recursive = FALSE)
  files_t <- github_files("scienceverse/papercheck",
                          ".github",
                         recursive = TRUE)
  expect_true(nrow(files_f) < nrow(files_t))
  expect_true("pkgdown.yaml" %in% files_t$name)
  expect_false("pkgdown.yaml" %in% files_f$name)
})

test_that("github_info", {
  skip_if_offline("github.com")

  info <- github_info("scienceverse/papercheck")

  # files
  expect_equal(names(info$files), c("name", "path", "size", "ext", "type"))
  expect_true("papercheck.Rproj" %in% info$files$name)

  # readme
  search <- "# papercheck"
  expect_true(grepl(search, info$readme, fixed = TRUE))

  # languages
  expect_true("R" %in% info$languages$language)
  expect_equal(names(info$languages), c("language", "bytes"))
})
