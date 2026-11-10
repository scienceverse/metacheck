# helpers ----

# The text of a small module file; `extra` adds roxygen lines to its header.
module_text <- function(func, extra = "#'") {
  c("#' External Test Module",
    "#'",
    "#' @description",
    paste("#' A module for testing module_install():", func),
    "#'",
    "#' @details",
    "#' Finds the word significant.",
    extra,
    "#' @keywords results",
    "#'",
    "#' @param paper a paper object or paperlist object",
    "#'",
    "#' @returns a list",
    paste0(func, " <- function(paper) {"),
    "  table <- text_search(paper, \"significant\")",
    "  list(table = table, traffic_light = \"info\", summary_text = \"done\")",
    "}")
}

# Write module files into a folder that stands in for a GitHub repository.
# `files` are paths relative to the folder, such as "ext_a.R" or
# "modules/ext_b.R"; the module function is named after the file.
write_modules <- function(dir, files, extra = "#'") {
  for (f in files) {
    dir.create(dirname(file.path(dir, f)), FALSE, TRUE)
    func <- basename(f) |> sub("\\.R$", "", x = _)
    writeLines(module_text(func, extra), file.path(dir, f))
  }
  invisible(dir)
}

# Replace GitHub with a local folder: .module_resolve() returns the fixed
# commit `sha`, and .module_download() returns `repo_dir`.
local_fake_github <- function(repo_dir, repo = "owner/repo",
                              sha = "abc1234def5678", env = parent.frame()) {
  testthat::local_mocked_bindings(
    .module_resolve = \(repo_arg, ref = NULL) {
      list(repo = repo, ref = ref %||% "main", sha = sha)
    },
    .module_download = \(src, dir) repo_dir,
    .package = "metacheck",
    .env = env
  )
}

# tests ----

test_that("exists", {
  expect_true(is.function(metacheck::module_install))
  expect_true(is.function(metacheck::module_update))
  expect_true(is.function(metacheck::module_remove))
  expect_true(is.function(metacheck::module_installed))
  expect_no_error(helplist <- help(module_install, metacheck))
  expect_no_error(helplist <- help(module_update, metacheck))
  expect_no_error(helplist <- help(module_remove, metacheck))
  expect_no_error(helplist <- help(module_installed, metacheck))
})

test_that("module_installed with nothing installed", {
  mod_dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(.modules_dir = \() mod_dir,
                                  .package = "metacheck")

  inst <- module_installed()
  expect_equal(nrow(inst), 0)
  expect_equal(names(inst),
               c("repo", "ref", "sha", "installed", "modules", "path"))
  expect_equal(metacheck:::.module_installed_files(), character(0))
})

test_that("METACHECK_MODULES_DIR", {
  shared <- withr::local_tempdir()
  withr::local_envvar(METACHECK_MODULES_DIR = shared)
  expect_equal(metacheck:::.modules_dir(), shared)

  repo_dir <- withr::local_tempdir()
  write_modules(repo_dir, "ext_shared.R")
  local_fake_github(repo_dir)
  expect_message(module_install("owner/repo"), "Installed 1 module")
  expect_true(file.exists(file.path(shared, "owner", "repo", "ext_shared.R")))
  expect_equal(normalizePath(module_find("ext_shared")),
               normalizePath(file.path(shared, "owner", "repo", "ext_shared.R")))

  # an installed module comes before a file of the same name in the working
  # directory or its modules/ folder (#512), which is still found when no
  # module of that name is installed
  wd <- withr::local_tempdir()
  write_modules(wd, c("modules/ext_shared.R", "modules/ext_local_only.R"))
  withr::with_dir(wd, {
    expect_equal(normalizePath(module_find("ext_shared")),
                 normalizePath(file.path(shared, "owner", "repo", "ext_shared.R")))
    expect_equal(module_find("ext_local_only"), "modules/ext_local_only.R")
  })

  # unset: the user data folder
  withr::local_envvar(METACHECK_MODULES_DIR = NA)
  expect_equal(metacheck:::.modules_dir(),
               file.path(tools::R_user_dir("metacheck", "data"),
                         "modules"))
})

test_that("module_install", {
  mod_dir <- withr::local_tempdir()
  repo_dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(.modules_dir = \() mod_dir,
                                  .package = "metacheck")
  write_modules(repo_dir, c("ext_count.R", "modules/ext_other.R"))
  # helper code without a roxygen header, and other files, are kept but
  # are not modules
  dir.create(file.path(repo_dir, "R"))
  writeLines("helper <- function() 1", file.path(repo_dir, "R", "helper.R"))
  writeLines("helper2 <- function() 1", file.path(repo_dir, "helper2.R"))
  writeLines("# Read me", file.path(repo_dir, "README.md"))
  local_fake_github(repo_dir)

  expect_error(module_install(bad_arg))
  expect_error(module_install(c("owner/a", "owner/b")), "owner/repo")
  expect_error(module_install("notarepo"), "owner/repo")

  expect_message(
    inst <- module_install("owner/repo"),
    "Installed 2 module(s) from owner/repo (commit abc1234): ext_count, ext_other",
    fixed = TRUE
  )
  expect_equal(inst$repo, "owner/repo")
  expect_equal(inst$ref, "main")
  expect_equal(inst$sha, "abc1234def5678")
  expect_equal(inst$modules, "ext_count, ext_other")
  expect_true(file.exists(file.path(mod_dir, "owner", "repo", "R", "helper.R")))
  expect_true(file.exists(file.path(mod_dir, "owner", "repo", "README.md")))

  # found by name, like a built-in module
  path <- module_find("ext_count")
  expect_equal(normalizePath(path),
               normalizePath(file.path(mod_dir, "owner", "repo", "ext_count.R")))
  path <- module_find("ext_other")
  expect_equal(normalizePath(path),
               normalizePath(file.path(mod_dir, "owner", "repo", "modules",
                                       "ext_other.R")))
  expect_error(module_find("helper2"), "no modules that matched helper2")

  mods <- module_list()
  expect_true(all(c("ext_count", "ext_other") %in% mods$name))
  expect_equal(mods$source[mods$name == "ext_count"], "owner/repo")
  expect_true(all(is.na(mods$source[mods$name == "marginal"])))
  builtin <- module_list(external = FALSE)
  expect_false("ext_count" %in% builtin$name)
  expect_true(all(is.na(builtin$source)))

  op <- capture_output(print(mods))
  expect_true(grepl("* ext_count: A module for testing module_install(): ext_count [from owner/repo]",
                    op, fixed = TRUE))

  info <- module_info("ext_count")
  expect_equal(info$source$repo, "owner/repo")
  expect_equal(info$source$sha, "abc1234def5678")
  expect_null(module_info("marginal")$source)

  # runs, and the report names the repository instead of a manual chapter
  mo <- module_run(demopaper(), "ext_count")
  expect_equal(mo$traffic_light, "info")
  expect_equal(mo$summary_text, "done")
  rep <- module_report(mo)
  expect_true(grepl("This module was installed from [owner/repo](https://github.com/owner/repo/tree/abc1234def5678) (commit abc1234).",
                    rep, fixed = TRUE))
  expect_false(grepl("metacheck manual", rep, fixed = TRUE))
  # built-in modules keep their chapter link
  rep <- module_run(demopaper(), "marginal") |> module_report()
  expect_true(grepl("metacheck manual", rep, fixed = TRUE))

  # installing again
  expect_error(module_install("owner/repo"), "already installed")
  expect_error(module_install("OWNER/REPO"), "already installed")
  expect_message(module_install("owner/repo", overwrite = TRUE),
                 "already up to date")
})

test_that("module_install refuses a name that is already used", {
  mod_dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(.modules_dir = \() mod_dir,
                                  .package = "metacheck")

  # the name of a built-in module
  repo_dir <- withr::local_tempdir()
  write_modules(repo_dir, c("marginal.R", "ext_a.R"))
  local_fake_github(repo_dir)
  expect_error(module_install("owner/repo"),
               "Nothing was installed: owner/repo has modules with names that are already used: marginal (built-in)",
               fixed = TRUE)
  expect_equal(nrow(module_installed()), 0)
  expect_false(dir.exists(file.path(mod_dir, "owner", "repo")))

  # two modules with one name in the same repository
  repo_dir <- withr::local_tempdir()
  write_modules(repo_dir, c("ext_a.R", "modules/ext_a.R"))
  local_fake_github(repo_dir)
  expect_error(module_install("owner/repo"),
               "owner/repo has more than one module named ext_a",
               fixed = TRUE)
  expect_equal(nrow(module_installed()), 0)

  # a module of another installed repository
  repo_one <- withr::local_tempdir()
  write_modules(repo_one, c("ext_a.R", "ext_b.R"))
  local_fake_github(repo_one, repo = "owner/one")
  expect_message(module_install("owner/one"), "Installed 2 module")

  repo_two <- withr::local_tempdir()
  write_modules(repo_two, c("ext_b.R", "ext_c.R"))
  local_fake_github(repo_two, repo = "owner/two")
  expect_error(module_install("owner/two"),
               "already used: ext_b (from owner/one)",
               fixed = TRUE)
  inst <- module_installed()
  expect_equal(inst$repo, "owner/one")
  expect_error(module_find("ext_c"), "no modules that matched ext_c")

  # a new commit of the same repository does not clash with itself
  write_modules(repo_one, "ext_d.R")
  local_fake_github(repo_one, repo = "owner/one", sha = "newsha9876543")
  expect_message(module_install("owner/one", overwrite = TRUE),
                 "Installed 3 module(s) from owner/one (commit newsha9): ext_a, ext_b, ext_d",
                 fixed = TRUE)
  expect_equal(module_installed()$sha, "newsha9876543")
})

test_that("module_install with no modules", {
  mod_dir <- withr::local_tempdir()
  repo_dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(.modules_dir = \() mod_dir,
                                  .package = "metacheck")
  writeLines("# Read me", file.path(repo_dir, "README.md"))
  writeLines("helper <- function() 1", file.path(repo_dir, "helper.R"))
  local_fake_github(repo_dir)

  expect_error(module_install("owner/repo"), "No modules found in owner/repo")
  expect_equal(nrow(module_installed()), 0)
})

test_that("module_install checks the metacheck version", {
  mod_dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(.modules_dir = \() mod_dir,
                                  .package = "metacheck")

  repo_dir <- withr::local_tempdir()
  write_modules(repo_dir, "ext_new.R", extra = "#' @metacheck 99.0.0")
  local_fake_github(repo_dir)
  expect_error(module_install("owner/repo"),
               "The module 'ext_new' needs metacheck 99.0.0 or later")
  expect_equal(nrow(module_installed()), 0)

  repo_dir <- withr::local_tempdir()
  write_modules(repo_dir, "ext_bad.R", extra = "#' @metacheck notaversion")
  local_fake_github(repo_dir)
  expect_error(module_install("owner/repo"), "is not a version number")

  repo_dir <- withr::local_tempdir()
  write_modules(repo_dir, "ext_old.R", extra = "#' @metacheck (>= 0.0.1)")
  local_fake_github(repo_dir)
  expect_message(module_install("owner/repo"), "Installed 1 module")
  expect_equal(module_info("ext_old")$metacheck, "(>= 0.0.1)")
  expect_no_error(module_run(demopaper(), "ext_old"))

  # module_run() checks it too
  path <- module_find("ext_old")
  writeLines(module_text("ext_old", "#' @metacheck 99.0.0"), path)
  expect_error(module_run(demopaper(), "ext_old"),
               "The module 'ext_old' needs metacheck 99.0.0 or later")
})

test_that("module_install names missing packages", {
  mod_dir <- withr::local_tempdir()
  repo_dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(.modules_dir = \() mod_dir,
                                  .package = "metacheck")
  write_modules(repo_dir, "ext_pkg.R", extra = "#' @import notapkgxyz")
  local_fake_github(repo_dir)

  expect_warning(
    expect_message(module_install("owner/repo"), "Installed 1 module"),
    "These modules need packages that are not installed: notapkgxyz"
  )
  # it is saved all the same
  expect_equal(module_installed()$repo, "owner/repo")
})

test_that("module_install names how to install packages only on GitHub (#223)", {
  mod_dir <- withr::local_tempdir()
  repo_dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(.modules_dir = \() mod_dir,
                                  .package = "metacheck")
  write_modules(repo_dir, "ext_gh.R",
                extra = "#' @import notapkgxyz someone/notapkggh")
  local_fake_github(repo_dir)

  expect_warning(
    expect_message(module_install("owner/repo"), "Installed 1 module"),
    paste0("These modules need packages that are not installed: notapkgxyz, notapkggh. ",
           "Install them with install.packages(c(\"notapkgxyz\")) and ",
           "remotes::install_github(c(\"someone/notapkggh\"))"),
    fixed = TRUE
  )
})

test_that("module_update", {
  mod_dir <- withr::local_tempdir()
  repo_dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(.modules_dir = \() mod_dir,
                                  .package = "metacheck")
  write_modules(repo_dir, "ext_a.R")
  local_fake_github(repo_dir)

  expect_error(module_update("owner/repo"), "is not installed")

  expect_message(module_install("owner/repo", ref = "dev"), "Installed 1")
  expect_message(module_update(), "already up to date")

  # a new commit on the same branch
  write_modules(repo_dir, "ext_b.R")
  local_fake_github(repo_dir, sha = "newsha9876543")
  expect_message(inst <- module_update("OWNER/repo"),
                 "Installed 2 module(s) from owner/repo (commit newsha9)",
                 fixed = TRUE)
  expect_equal(inst$ref, "dev")
  expect_equal(inst$sha, "newsha9876543")
  expect_equal(inst$modules, "ext_a, ext_b")
})

test_that("module_remove", {
  mod_dir <- withr::local_tempdir()
  repo_dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(.modules_dir = \() mod_dir,
                                  .package = "metacheck")
  write_modules(repo_dir, "ext_a.R")
  local_fake_github(repo_dir)
  expect_message(module_install("owner/repo"), "Installed 1")

  expect_error(module_remove(bad_arg))
  expect_message(res <- module_remove("owner/other"), "is not installed")
  expect_false(res)

  expect_message(res <- module_remove("owner/repo"),
                 "Removed modules from owner/repo")
  expect_true(res)
  expect_false(dir.exists(file.path(mod_dir, "owner")))
  expect_equal(nrow(module_installed()), 0)
  expect_error(module_find("ext_a"), "no modules that matched ext_a")
})

test_that("module_install from GitHub", {
  # the repository's name and default branch (one spelling of the name only:
  # the saved answers are files, and Windows file names ignore case)
  src <- metacheck:::.module_resolve("https://github.com/scienceverse/metacheck")
  expect_equal(src$repo, "scienceverse/metacheck")
  expect_equal(src$ref, "main")
  expect_true(grepl("^[0-9a-f]{40}$", src$sha))

  expect_error(metacheck:::.module_resolve("scienceverse/metacheck", "nobranchxyz"),
               "has no branch, tag or commit called 'nobranchxyz'")
  expect_error(metacheck:::.module_resolve("scienceverse/notarepoxyz"),
               "does not exist or is private")
}, "mock")
