# Tests for the repository check app (inst/app/shiny_app_repocheck).

# The app's own functions, without starting the app: app.R sources its helper
# files by relative path, so it is read from inside its own folder.
.repocheck_fun <- function(name) {
  appdir <- system.file("app", "shiny_app_repocheck", package = "metacheck")
  testthat::skip_if(appdir == "", "repository check app not installed")
  src <- readLines(file.path(appdir, "app.R"))
  s <- grep(paste0("^", name, " <- function"), src)
  e <- s + which(src[(s + 1):length(src)] == "}")[1]
  env <- new.env(parent = globalenv())
  eval(parse(text = src[s:e]), envir = env)
  list(fun = get(name, envir = env), appdir = appdir)
}

test_that("a report on an uploaded .zip does not show the server's folders (#500)", {
  skip_if_quick()
  skip_shiny()
  skip_if_not_installed("quarto")
  skip_if(is.null(quarto::quarto_path()), "Quarto not available")
  llm_use(FALSE)
  extract_zip <- .repocheck_fun("extract_zip")$fun
  job <- .repocheck_fun("repocheck_job")
  # the job puts the app's progress bar in metacheck's place
  # (use_shiny_progress()); put metacheck's own back afterwards, or every
  # later test in this session runs with the app's bar
  orig_pb <- metacheck:::pb
  withr::defer(utils::assignInNamespace("pb", orig_pb, ns = "metacheck"))
  # the job also sets metacheck.repo_cache.dir to its own cache folder; put
  # the old value back afterwards, or every later test downloads into it
  withr::local_options(metacheck.repo_cache.dir = getOption("metacheck.repo_cache.dir"))

  for (wrapped in c(TRUE, FALSE)) {
    workdir <- withr::local_tempdir()
    src <- withr::local_tempdir()
    inner <- if (wrapped) file.path(src, "my_study") else src
    dir.create(inner, showWarnings = FALSE)
    writeLines(c("id,score", "1,3", "2,5"), file.path(inner, "data.csv"))
    writeLines("d <- read.csv('data.csv')", file.path(inner, "analysis.R"))
    zip_path <- file.path(withr::local_tempdir(), "my_upload.zip")
    withr::with_dir(src, utils::zip(zip_path, list.files(".", recursive = TRUE), flags = "-q"))
    skip_if_not(file.exists(zip_path), "zip utility unavailable")

    # what the app's .zip upload handler passes on
    repo_dir <- extract_zip(zip_path, file.path(workdir, "repo", "my_upload"))
    args <- list(repo_check = list(local_path = basename(repo_dir), local_only = TRUE))
    html <- withr::local_tempfile(fileext = ".html")
    # the job itself, started from the app's folder as the background process
    # starts it (bg_start()); it changes to work_dir after sourcing its files
    out <- withr::with_dir(job$appdir, job$fun(
      test_paper(), c("repo_check", "code_check"), NULL, args, html,
      file.path(workdir, "cache"), work_dir = dirname(repo_dir)))
    txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
    expect_false(grepl(workdir, txt, fixed = TRUE))
    expect_false(grepl(normalizePath(workdir, winslash = "/"), txt, fixed = TRUE))
    expect_match(txt, if (wrapped) "my_study" else "my_upload", fixed = TRUE)
    expect_match(txt, "analysis.R", fixed = TRUE)
  }
})

test_that("an empty .zip is called empty, not damaged (Lakens/tue_datamanager_check#4, item 8)", {
  check_zip <- .repocheck_fun("check_zip")$fun
  explain_error <- .repocheck_fun("explain_error")$fun
  # a .zip without entries is only the 22-byte end-of-archive record
  empty <- withr::local_tempfile(fileext = ".zip")
  writeBin(as.raw(c(0x50, 0x4b, 0x05, 0x06, rep(0, 18))), empty)
  msg <- tryCatch(check_zip(empty), error = conditionMessage)
  expect_equal(msg, "The .zip holds no files.")
  expect_true(startsWith(explain_error(msg), "The .zip holds no files."))
  # a file that is not a .zip is still called damaged
  broken <- withr::local_tempfile(fileext = ".zip")
  writeLines("not a zip", broken)
  msg <- tryCatch(check_zip(broken), error = conditionMessage)
  expect_match(explain_error(msg), "could not be opened as a .zip archive")
  # a .zip of an empty folder holds only the folder's entry
  src <- withr::local_tempdir()
  dir.create(file.path(src, "my_study"))
  folders <- file.path(withr::local_tempdir(), "folders.zip")
  withr::with_dir(src, utils::zip(folders, "my_study", flags = "-q"))
  skip_if_not(file.exists(folders), "zip utility unavailable")
  expect_error(check_zip(folders), "holds no files")
})
