test_that("repo_check offline", {
  module <- "repo_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no relevant text
  paper <- test_paper("No repos")
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)
  exp <- data.frame(paper_id = paper$paper_id,
                    repo_n = 0,
                    files_n = NA,
                    files_data = NA,
                    files_code = NA,
                    files_readme = NA,
                    files_zip = NA)
  expect_equal(mod_output$summary_table, exp)
  exp <- "We found no links to repositories on the Open Science Framework, Github, ResearchBox, DSpace, Zenodo, Dataverse, Figshare, Dryad, ReShare, 4TU.ResearchData, Mendeley Data, DataONE, or LDbase."
  expect_equal(mod_output$summary_text, exp)
  expect_equal(mod_output$report, exp)
})


test_that("OSF view_only link", {
  osf_url <- "https://osf.io/t9j8e/? view_only=f171281f212f4435917b16a9e581a73b"
  paper <- test_paper(url = osf_url)
  obs <- module_run(paper, "repo_check")

  expect_in(obs$table$repo_url, osf_url)
}, "mock")

test_that("OSF no files", {
  # OSF but no R files

  module <- "repo_check"
  paper <- test_paper()
  paper$url <- data.frame(href = "https://osf.io/y6a34", text_id = 1)
  mod_output <- module_run(paper, module)

  expect_equal(mod_output$traffic_light, "yellow")
  exp <- data.frame(paper_id = paper$paper_id,
                    repo_n = 1,
                    repo_unchecked = 0L,
                    files_n = 0,
                    files_data = 0,
                    files_code = 0,
                    files_readme = 0,
                    files_zip = 0,
                    files_unknown = 0L,
                    naming_issues = 0L,
                    roster_mismatch = FALSE)
  expect_equal(mod_output$summary_table, exp)
  exp <- " 0 files "
  expect_true(grepl(exp, mod_output$summary_text))
}, "mock")

test_that("no code files", {
  module <- "repo_check"
  paper <- test_paper()
  paper$url <- data.frame(href = "https://osf.io/m4nbv", text_id = 1)
  mod_output <- module_run(paper, module)

  expect_true(grepl("We found 2 files ", mod_output$summary_text))
  # Both files ARE classifiable from their names alone: README.md is the
  # readme, and the archive is named "code.zip", which the name-based rule in
  # data_classify_files() types as data_type "code" (see R/data_check_helpers.R
  # — an archive's EXTENSION crosswalks to "unknown", but an explicit "code" in
  # the file name overrides that). Nothing is left unclassifiable, so
  # files_unknown is 0 and the naming check finds no issue either. files_code
  # counts files whose file_type is code, which an archive's is not — hence 0
  # there while data_type is "code".
  exp <- data.frame(paper_id = paper$paper_id,
                    repo_n = 1,
                    repo_unchecked = 0L,
                    files_n = 2,
                    files_data = 0,
                    files_code = 0,
                    files_readme = 1,
                    files_zip = 1,
                    files_unknown = 0L,
                    naming_issues = 0L,
                    roster_mismatch = FALSE)
  expect_equal(mod_output$summary_table, exp)
}, "mock")

test_that("OSF", {
  module <- "repo_check"
  paper <- test_paper()
  paper$url <- data.frame(href = "https://osf.io/629bx", text_id = 1)
  mod_output <- module_run(paper, module)

  expect_true(grepl("We found 4 files ", mod_output$summary_text))
  # The fixture's Archive.zip is not classifiable by name/extension without
  # opening it (repo_check never downloads), and test_paper()'s empty
  # manuscript text names no studies at all, so roster_check$roster is empty —
  # nothing for the file-derived "ex1" group to disagree with, hence FALSE
  # (not a real mismatch, just an absence of roster evidence to compare).
  exp <- data.frame(paper_id = paper$paper_id,
                    repo_n = 1,
                    repo_unchecked = 0L,
                    files_n = 4,
                    files_data = 1,
                    files_code = 2,
                    files_readme = 0,
                    files_zip = 1,
                    files_unknown = 1L,
                    naming_issues = 1L,
                    roster_mismatch = FALSE)
  expect_equal(mod_output$summary_table, exp)
}, "mock")

test_that("OSF and github", {
  # relevant text - info
  module <- "repo_check"
  text <- c("osf.io/629bx",
            "github.com/scienceverse/demo")
  paper <- test_paper()
  paper$url <- data.frame(href = text, text_id = 1:2)
  mod_output <- module_run(paper, module)

  expect_equal(mod_output$traffic_light, "yellow")
  exp <- c("bad.R", "bad.Rmd", "good-example.R")
  expect_contains(mod_output$table$file_name, exp)
  expect_equal(mod_output$summary_table$paper_id, paper$paper_id)
  expect_equal(mod_output$summary_table$repo_n, 2)
  expect_equal(mod_output$summary_table$files_n, nrow(mod_output$table))
  expect_gte(mod_output$summary_table$files_data, 1)
  expect_gte(mod_output$summary_table$files_code, 1)
  expect_gte(mod_output$summary_table$files_zip, 1)
}, "mock")

test_that("ResearchBox", {
  # Not mocked, unlike the rest of this file. ResearchBox builds its zip from a
  # POST to download_files.php, and httptest2 records that binary body as a
  # separate .R-FILE payload referenced by find_mock_file(), which is internal
  # to httptest2 and not in scope when the fixture is evaluated -- so a recorded
  # response cannot be replayed. (The varying `reference` token in the POST body
  # is handled by the redactor in helper.R, but that only fixes the file NAME,
  # not this.) So this one goes to the live site.
  skip_if_not(online("researchbox.org"))
  skip_on_cran()
  # ResearchBox does not serve GitHub's test machines: box 4377 failed there
  # (2026-10-20) while it downloads from elsewhere
  skip_on_ci()

  module <- "repo_check"
  paper <- test_paper()
  paper$url <- data.frame(href = "https://researchbox.org/4377", text_id = 1)
  mod_output <- suppressWarnings(module_run(paper, module))
  # after several downloads from one computer ResearchBox refuses more for a while
  skip_if(any(grepl("asks for a captcha", unlist(mod_output$report), fixed = TRUE)),
          "ResearchBox asks for a captcha")

  # `file_name` holds the file's NAME; its position inside the repository (or
  # inside an expanded archive) lives in `file_path`. The ResearchBox archive
  # stores this script at "Code/Study 1.r", so the subfolder is asserted
  # against file_path and the bare name against file_name.
  expect_contains(mod_output$table$file_name, "Study 1.r")
  expect_contains(mod_output$table$file_path, "Code/Study 1.r")
  expect_equal(mod_output$summary_table$repo_n, 1)
  expect_gte(mod_output$summary_table$files_data, 1)
  expect_gte(mod_output$summary_table$files_code, 1)
})

test_that("Zenodo", {
  paper <- test_paper(url = "https://zenodo.org/records/17754445")
  module <- "repo_check"
  mod_output <- module_run(paper, module)

  expect_equal(mod_output$table$file_name, "ResearchBox_4377.zip")
  expect_equal(mod_output$summary_table$files_zip, 1)

  paper <- paperlist(
    test_paper(url = "https://zenodo.org/records/17754445"),
    test_paper(url = "https://zenodo.org/records/123456789")
  )
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$summary_table$files_n, c(1, 1))
  expect_equal(mod_output$summary_table$files_zip, c(1, 0))
}, "mock")

# repo_check() + local_path ----

test_that("repo_check tolerates files with an unknown (NA) size", {
  # Regression: OSF sometimes returns no size for a file. The size column was
  # formatted with utils:::format.object_size(), whose `if (x <= 0)` errors on
  # NA ("missing value where TRUE/FALSE needed"), crashing repo_check for the
  # whole paper. Stub file.size() to NA so a local file flows the missing-size
  # path (archive_local() sets file_size from file.size()).
  d <- withr::local_tempdir()
  writeLines("x <- 1", file.path(d, "analysis.R"))
  paper <- test_paper()

  local_mocked_bindings(file.size = function(...) NA_real_, .package = "base")
  expect_no_error(mo <- module_run(paper, "repo_check", local_path = d))

  # The unknown size flows through and renders as the placeholder, not a crash.
  expect_true(is.na(mo$table$file_size))
  expect_true(any(grepl("—", mo$report)))
})

test_that("repo_check vector of local paths", {
  local_path <- c(
    test_path("fixtures", "code_files", "analysis.R"),
    test_path("fixtures", "code_files", "README.md")
  )
  paper <- test_paper()
  module <- "repo_check"
  mo <- module_run(paper, module, local_path = local_path)

  expect_equal(mo$summary_table$repo_n, 2)
  expect_true("analysis.R" %in% mo$table$file_name)
  expect_true("README.md"  %in% mo$table$file_name)
})

test_that("repo_check local_path only", {
  local_path <- test_path("fixtures", "code_files")
  mo <- module_run(test_paper(), "repo_check", local_path = local_path)

  # one repo: the local folder
  expect_equal(mo$summary_table$repo_n, 1)
  expect_equal(mo$summary_table$files_n, 7)
  expect_equal(mo$summary_table$files_code, 5)  # .do is "stats" type, not "code"
  expect_equal(mo$summary_table$files_data, 1)
  expect_equal(mo$summary_table$files_readme, 1)
  expect_equal(mo$summary_table$files_zip, 0)

  # repo type is "local"
  expect_true("analysis.R" %in% mo$table$file_name)
  expect_true("README.md" %in% mo$table$file_name)

  # README present, no zip: green
  expect_equal(mo$traffic_light, "green")
})

test_that("repo_check paper + local_path", {
  # osf/629bx mock: 4 files (2 code, 1 data, 0 readme, 1 zip)
  # fixture_dir:    5 files (3 code, 1 data, 1 readme, 0 zip)
  paper <- test_paper(url = "https://osf.io/629bx")
  local_path <- test_path("fixtures", "code_files")
  mo <- module_run(paper, "repo_check", local_path = local_path)

  # two repos: OSF + local
  expect_equal(mo$summary_table$repo_n, 2)
  expect_equal(mo$summary_table$files_n, 11)

  # combined counts
  expect_equal(mo$summary_table$files_code, 7)  # .do is "stats" type, not "code"
  expect_equal(mo$summary_table$files_readme, 1)  # only local has README
  expect_equal(mo$summary_table$files_zip, 1)      # only OSF has zip

  # files from both sources present in table
  expect_true("README.md" %in% mo$table$file_name)   # local
  expect_true("analysis.R" %in% mo$table$file_name)  # local
  expect_true(any(grepl("bad\\.R$", mo$table$file_name, ignore.case = TRUE))) # OSF

  # OSF still has no README → repo_no_readme > 0 → yellow
  expect_equal(mo$traffic_light, "yellow")
}, "mock")


# repo_check() + R package exclusion ----

test_that("repo_check excludes an R package's own source tree", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "R"))
  dir.create(file.path(d, "man"))
  writeLines("Package: mypkg\nVersion: 0.1.0", file.path(d, "DESCRIPTION"))
  writeLines("export(foo)", file.path(d, "NAMESPACE"))
  writeLines("foo <- function() 1", file.path(d, "R", "foo.R"))
  writeLines("\\name{foo}", file.path(d, "man", "foo.Rd"))
  writeLines("x <- 1", file.path(d, "analysis.R"))
  writeLines("a,b\n1,2", file.path(d, "data.csv"))

  mo <- module_run(test_paper(), "repo_check", local_path = d)

  # package files gone entirely, non-package files still listed
  expect_false(any(grepl("^DESCRIPTION$|^NAMESPACE$|foo\\.R$|foo\\.Rd$",
                         mo$table$file_name)))
  expect_true("analysis.R" %in% mo$table$file_name)
  expect_true("data.csv" %in% mo$table$file_name)
  expect_equal(mo$summary_table$files_n, 2)
})

test_that("repo_check keeps files when DESCRIPTION has no matching NAMESPACE", {
  d <- withr::local_tempdir()
  writeLines("Package: mypkg\nVersion: 0.1.0", file.path(d, "DESCRIPTION"))
  writeLines("x <- 1", file.path(d, "analysis.R"))

  mo <- module_run(test_paper(), "repo_check", local_path = d)

  expect_true("DESCRIPTION" %in% mo$table$file_name)
  expect_true("analysis.R" %in% mo$table$file_name)
})

test_that("repo_check keeps a root-level package's own files when it IS the deposit", {
  # a study that shares its own data and analysis code packaged as an R
  # package (e.g. a study-specific GitHub repo like wx-ecology/BaBA): only
  # packaging scaffolding (.Rbuildignore, .Rproj, LICENSE, README) sits
  # outside the package's R/, man/, data/ -- nothing data/code-shaped, so
  # there is no "outside" for the study's content to live in and the
  # exclusion must not strip the package's own real data/code.
  d <- withr::local_tempdir()
  dir.create(file.path(d, "R"))
  dir.create(file.path(d, "man"))
  dir.create(file.path(d, "data"))
  writeLines("Package: mypkg\nVersion: 0.1.0", file.path(d, "DESCRIPTION"))
  writeLines("export(foo)", file.path(d, "NAMESPACE"))
  writeLines("foo <- function() 1", file.path(d, "R", "foo.R"))
  writeLines("\\name{foo}", file.path(d, "man", "foo.Rd"))
  save(list = "x", file = file.path(d, "data", "study_data.rda"), envir = list2env(list(x = 1)))
  writeLines("", file.path(d, ".Rbuildignore"))
  writeLines("MIT", file.path(d, "LICENSE"))
  writeLines("# mypkg", file.path(d, "README.md"))

  mo <- module_run(test_paper(), "repo_check", local_path = d)

  expect_true("DESCRIPTION" %in% mo$table$file_name)
  expect_true("NAMESPACE" %in% mo$table$file_name)
  expect_true("foo.R" %in% mo$table$file_name)
  expect_true("study_data.rda" %in% mo$table$file_name)
})

test_that("a root-level package that IS the deposit is kept despite its CI config files (#469)", {
  # .travis.yml / codecov.yml / GitHub Actions workflows classify as "code"
  # by their .yml extension, which made the package look vendored and
  # removed all of its files. They are build/test configuration, not analysis
  # code, and stay listed themselves.
  d <- withr::local_tempdir()
  dir.create(file.path(d, "R"))
  dir.create(file.path(d, ".github", "workflows"), recursive = TRUE)
  writeLines("Package: mypkg\nVersion: 0.1.0", file.path(d, "DESCRIPTION"))
  writeLines("export(foo)", file.path(d, "NAMESPACE"))
  writeLines("foo <- function() 1", file.path(d, "R", "foo.R"))
  writeLines("language: r", file.path(d, ".travis.yml"))
  writeLines("comment: false", file.path(d, "codecov.yml"))
  writeLines("on: push", file.path(d, ".github", "workflows", "R-CMD-check.yaml"))

  mo <- module_run(test_paper(), "repo_check", local_path = d)

  expect_true("foo.R" %in% mo$table$file_name)
  expect_true("DESCRIPTION" %in% mo$table$file_name)
  expect_true("codecov.yml" %in% mo$table$file_name)
})

test_that("a paper's own config.yml still counts as content next to a root-level package", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "R"))
  writeLines("Package: mypkg\nVersion: 0.1.0", file.path(d, "DESCRIPTION"))
  writeLines("export(foo)", file.path(d, "NAMESPACE"))
  writeLines("foo <- function() 1", file.path(d, "R", "foo.R"))
  writeLines("alpha: 0.05", file.path(d, "config.yml"))

  mo <- module_run(test_paper(), "repo_check", local_path = d)

  expect_false("foo.R" %in% mo$table$file_name)
  expect_true("config.yml" %in% mo$table$file_name)
})

# files that do not belong in a repository ----

test_that(".useless_file_kind recognises every kind of useless file", {
  os <- "operating system"
  expected <- c(
    # macOS: __MACOSX/ folder (also as the folder itself), "._" copies,
    # .DS_Store, .localized, Icon\r, volume folders
    "LAMP.zip/__MACOSX/LAMP/._data.csv"     = os,
    "__MACOSX"                              = os,
    "study/._analysis.R"                    = os,
    "study/.DS_Store"                       = os,
    "study/.localized"                      = os,
    "study/Icon\r"                          = os,
    ".AppleDouble/data.csv"                 = os,
    ".Spotlight-V100/store.db"              = os,
    ".Trashes/501/x"                        = os,
    ".fseventsd/0001"                       = os,
    # Windows (backslashes, any case) and Linux desktops
    "img\\Thumbs.db"                        = os,
    "img/ehthumbs.db"                       = os,
    "Desktop.ini"                           = os,
    "$RECYCLE.BIN/S-1-5/x.csv"              = os,
    ".Trash-1000/files/x.csv"               = os,
    "study/.directory"                      = os,
    # RStudio, Jupyter, Python, open documents
    "LAMP/.Rproj.user/shared/notebooks/x"   = "RStudio session files",
    "nb/.ipynb_checkpoints/a-checkpoint.ipynb" = "Jupyter checkpoints",
    "code/__pycache__/utils.cpython-311.pyc" = "compiled Python files",
    "code/utils.pyc"                        = "compiled Python files",
    "docs/~$report.docx"                    = "open-document lock files",
    # at the top level: basename() would expand the leading "~"
    "~$report.docx"                         = "open-document lock files",
    "data/~$scores.xlsx"                    = "open-document lock files",
    "slides/~$talk.pptx"                    = "open-document lock files",
    "docs/.~lock.report.docx#"              = "open-document lock files",
    "~$notes.txt"                           = "open-document lock files",
    # the junk rules of TU/e's data managers (RAPS data manager check)
    ".TemporaryItems/folders.501/x"         = os,
    ".Rhistory"                             = "R history",
    "code/.Rhistory"                        = "R history",
    "analysis~"                             = "editor backups",
    "analysis.R.bak"                        = "editor backups",
    "code/.analysis.R.swp"                  = "editor backups",
    "code/.analysis.R.swo"                  = "editor backups",
    "notes.tmp"                             = "temporary files",
    "export.TEMP"                           = "temporary files",
    ".git/config"                           = "version control",
    "sub/.git/objects/ab/cdef"              = "version control",
    ".svn/entries"                          = "version control",
    ".hg/store/data"                        = "version control",
    ".idea/workspace.xml"                   = "editor settings",
    ".vscode/settings.json"                 = "editor settings"
  )
  expect_equal(.useless_file_kind(names(expected)), unname(expected))
  expect_true(all(.is_useless_file(names(expected))))
})

test_that(".useless_file_severity", {
  kinds <- c("operating system", "R history", "editor backups", "temporary files",
             "open-document lock files", "version control", "editor settings", NA)
  expect_equal(.useless_file_severity(kinds),
               c(rep("problem", 5), "suggestion", "suggestion", NA))
})

test_that(".useless_file_kind keeps every file that may matter", {
  keep <- c("data.csv", "study/a_.csv", "MACOSX_notes.txt", "analysis.R",
            # may matter to someone, deliberately not treated as useless
            ".RData", "report_cache/html/chunk.rdb", "renv.lock", ".gitignore",
            # names that only look like useless files
            "my.Rproj", ".Rproj.user.txt", "pycache_notes.txt", "utils.py",
            "Thumbs.db.csv", "lock.docx", ".github/workflows/check.yml",
            "git/notes.txt", "temperature.csv", "data.backup.csv",
            "Rhistory_notes.txt", "tmp/data.csv")
  expect_true(all(is.na(.useless_file_kind(keep))))
  expect_false(any(.is_useless_file(keep)))
  expect_true(is.na(.useless_file_kind(NA_character_)))
  expect_equal(.useless_file_kind(character(0)), character(0))
  expect_equal(.is_useless_file(character(0)), logical(0))
})

test_that("repo_check leaves out and reports files that do not belong in a repository", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "__MACOSX"))
  dir.create(file.path(d, "__pycache__"))
  writeLines("a,b\n1,2", file.path(d, "data.csv"))
  writeLines("x <- 1", file.path(d, "analysis.R"))
  writeLines("junk", file.path(d, "__MACOSX", "data.csv"))
  writeLines("junk", file.path(d, "Thumbs.db"))
  writeLines("junk", file.path(d, "__pycache__", "utils.cpython-311.pyc"))
  writeLines("junk", file.path(d, "~$report.docx"))

  mo <- module_run(test_paper(), "repo_check", local_path = d)

  # only the real files are listed and counted
  expect_setequal(mo$table$file_name, c("data.csv", "analysis.R"))
  expect_equal(mo$summary_table$files_n, 2)
  # the useless files are reported, by kind
  expect_match(mo$summary_text, "left out 4 files that do not belong in a repository", fixed = TRUE)
  expect_match(mo$summary_text, "operating system (2)", fixed = TRUE)
  expect_match(mo$summary_text, "compiled Python files (1)", fixed = TRUE)
  expect_match(mo$summary_text, "open-document lock files (1)", fixed = TRUE)
  report <- paste(mo$report, collapse = "\n")
  expect_match(report, "Files that do not belong in the repository", fixed = TRUE)
  expect_match(report, "Thumbs.db", fixed = TRUE)
  # and they are not flagged for naming
  expect_false(any(grepl("Thumbs|pycache|~\\$", mo$naming_issues$file_name %||% character(0))))
  # they are something to fix, and returned for other modules
  expect_equal(mo$traffic_light, "red")
  expect_equal(nrow(mo$useless_files), 4)
  expect_true(all(c("repo_url", "file_path", "kind", "severity") %in% names(mo$useless_files)))
  expect_true(all(mo$useless_files$severity == "problem"))
})

test_that("repo_check finds hidden useless files in a local folder", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "code"))
  writeLines("a,b\n1,2", file.path(d, "data.csv"))
  writeLines("x <- 1", file.path(d, "code", "analysis.R"))
  writeLines("junk", file.path(d, ".DS_Store"))
  writeLines("q()", file.path(d, "code", ".Rhistory"))
  # a hidden file that is not useless stays out of the listing, as before
  writeLines("data/", file.path(d, ".gitignore"))

  mo <- module_run(test_paper(), "repo_check", local_path = d)

  expect_setequal(mo$table$file_name, c("data.csv", "analysis.R"))
  expect_setequal(mo$useless_files$kind, c("operating system", "R history"))
  expect_equal(mo$traffic_light, "red")
  expect_match(mo$summary_text, "left out 2 files that do not belong", fixed = TRUE)
})

test_that("repo_check: version control and editor settings are suggestions", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, ".git"))
  dir.create(file.path(d, ".vscode"))
  writeLines("a,b\n1,2", file.path(d, "data.csv"))
  writeLines("[core]", file.path(d, ".git", "config"))
  writeLines("{}", file.path(d, ".vscode", "settings.json"))

  mo <- module_run(test_paper(), "repo_check", local_path = d)

  expect_equal(mo$table$file_name, "data.csv")
  expect_equal(sort(mo$useless_files$kind), c("editor settings", "version control"))
  expect_true(all(mo$useless_files$severity == "suggestion"))
  expect_equal(mo$traffic_light, "yellow")
  expect_match(mo$summary_text, "Keep them only if you mean to share them", fixed = TRUE)
  expect_no_match(mo$summary_text, "Remove", fixed = TRUE)
})

test_that("repo_check reports nothing when there are no useless files", {
  d <- withr::local_tempdir()
  writeLines("a,b\n1,2", file.path(d, "data.csv"))
  mo <- module_run(test_paper(), "repo_check", local_path = d)
  expect_no_match(mo$summary_text, "do not belong", fixed = TRUE)
  expect_no_match(paste(mo$report, collapse = "\n"), "do not belong", fixed = TRUE)
  expect_equal(nrow(mo$useless_files), 0)
  expect_true(mo$traffic_light != "red")
})

test_that("data_check leaves useless files out of a zip it unpacks", {
  skip_if_not_installed("zip")
  llm_use(FALSE)
  src <- withr::local_tempdir()
  dir.create(file.path(src, "__MACOSX"))
  write.csv(data.frame(id = 1:5, score = c(2.5, 3.1, 4.0, 3.3, 2.9)),
            file.path(src, "scores.csv"), row.names = FALSE)
  writeLines("junk", file.path(src, "__MACOSX", "._scores.csv"))
  d <- withr::local_tempdir()
  zip::zip(file.path(d, "study.zip"), c("scores.csv", "__MACOSX/._scores.csv"), root = src)

  mo <- module_run(test_paper(), "data_check", local_path = d, local_only = TRUE)

  expect_true("scores.csv" %in% mo$structure$file_name)
  expect_false(any(.is_useless_file(mo$structure$file_path %||% mo$structure$file_name)))
  expect_false("._scores.csv" %in% mo$structure$file_name)
})

test_that("repo_check excludes a nested vendored package but keeps real sibling content", {
  # a study's repo that bundles a vendored copy of someone else's general-
  # purpose package (its own DESCRIPTION+NAMESPACE nested in a subfolder)
  # alongside the study's OWN real analysis files at the repo root -- only
  # the vendored package's files should be excluded.
  d <- withr::local_tempdir()
  dir.create(file.path(d, "vendored_pkg", "R"), recursive = TRUE)
  writeLines("Package: vendoredpkg\nVersion: 0.1.0", file.path(d, "vendored_pkg", "DESCRIPTION"))
  writeLines("export(bar)", file.path(d, "vendored_pkg", "NAMESPACE"))
  writeLines("bar <- function() 1", file.path(d, "vendored_pkg", "R", "bar.R"))
  writeLines("x <- 1", file.path(d, "analysis.R"))
  writeLines("a,b\n1,2", file.path(d, "data.csv"))

  mo <- module_run(test_paper(), "repo_check", local_path = d)

  expect_false(any(grepl("^DESCRIPTION$|^NAMESPACE$|bar\\.R$", mo$table$file_name)))
  expect_true("analysis.R" %in% mo$table$file_name)
  expect_true("data.csv" %in% mo$table$file_name)
})


# repo_check() + local_only ----

test_that("repo_check local_only = TRUE ignores online repos, checks local only", {
  # paper has OSF link that would normally return 4 files
  paper     <- test_paper(url = "https://osf.io/629bx")
  local_path <- test_path("fixtures", "code_files")
  mo <- module_run(paper, "repo_check", local_path = local_path, local_only = TRUE)

  # only 1 repo: the local folder — OSF link silently skipped
  expect_equal(mo$summary_table$repo_n, 1)
  # all files come from the local fixture (7 files)
  expect_equal(mo$summary_table$files_n, 7)
  # no OSF URLs anywhere in the table
  expect_false(any(grepl("osf\\.io", mo$table$repo_url, ignore.case = TRUE)))
  # local files present
  expect_true("analysis.R" %in% mo$table$file_name)
  expect_true("README.md"  %in% mo$table$file_name)
})

test_that("repo_check local_only = TRUE with no local_path returns na", {
  # nothing to check: online skipped, no local path provided
  mo <- module_run(test_paper(), "repo_check", local_only = TRUE)

  expect_equal(mo$traffic_light, "na")
  expect_equal(mo$summary_table$repo_n, 0)
})

test_that("repo_check local_only = TRUE with online URLs but no local_path returns na", {
  # paper has an OSF link, but local_only suppresses online lookup; no local_path given
  paper <- test_paper(url = "https://osf.io/629bx")
  mo <- module_run(paper, "repo_check", local_only = TRUE)

  expect_equal(mo$traffic_light, "na")
  expect_equal(mo$summary_table$repo_n, 0)
})

test_that("repo_check local_only = FALSE is the same as the default", {
  # passing local_only = FALSE should produce identical output to omitting it
  local_path <- test_path("fixtures", "code_files")
  paper      <- test_paper()

  mo_default  <- module_run(paper, "repo_check", local_path = local_path)
  mo_explicit <- module_run(paper, "repo_check", local_path = local_path, local_only = FALSE)

  expect_equal(mo_default$summary_table, mo_explicit$summary_table)
  expect_equal(mo_default$table,         mo_explicit$table)
  expect_equal(mo_default$traffic_light, mo_explicit$traffic_light)
}, "mock")

test_that("repo_check local_only = TRUE without local_path ignores multiple online repo types", {
  # paper has OSF, GitHub, and ResearchBox links — all should be skipped
  text  <- c("osf.io/629bx", "github.com/scienceverse/demo", "https://researchbox.org/4377")
  paper <- test_paper()
  paper$url <- data.frame(href = text, text_id = 1:3)
  mo <- module_run(paper, "repo_check", local_only = TRUE)

  expect_equal(mo$traffic_light, "na")
  expect_equal(mo$summary_table$repo_n, 0)
})

# repo_check() + a zip inside a ResearchBox ----
# A ResearchBox row carries the box's web page as file_url and the copy
# unpacked from the box's zip as file_location. A zip inside the box used to be
# peeked at the web page (which answers a Range request with 406, verified
# 2026-10-13); it is now listed from the local copy, with no request.
.real_rbox_file_download <- get("rbox_file_download", envir = asNamespace("metacheck"),
                                inherits = FALSE)
test_that("a zip inside a ResearchBox is listed from its local copy", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "Data"))
  writeLines("id,x\n1,2", file.path(d, "Data", "study1.csv"))
  writeLines("x <- 1", file.path(d, "analysis.R"))
  withr::with_dir(d, utils::zip("inner.zip", c("Data/study1.csv", "analysis.R"),
                                flags = "-q"))
  skip_if_not(file.exists(file.path(d, "inner.zip")), "zip utility unavailable")
  box <- "https://researchbox.org/99999"
  local_mocked_bindings(rbox_file_download = function(rb_url, pb = NULL, ...) {
    data.frame(rb_url = box, name = "ResearchBox 99999/inner.zip",
               file_location = file.path(d, "inner.zip"),
               size = file.size(file.path(d, "inner.zip")), type = "archive",
               isdir = FALSE, stringsAsFactors = FALSE)
  }, .rbox_license = \(...) NA_character_)
  httr2::local_mocked_responses(function(req) stop("no request expected: ", req$url))

  paper <- test_paper(url = box)
  mo <- module_run(paper, "repo_check")
  expect_setequal(mo$table$file_name, c("study1.csv", "analysis.R"))
  expect_true(all(mo$table$archive_url == file.path(d, "inner.zip")))
  expect_true(all(is.na(mo$table$file_location)))
})
assignInNamespace("rbox_file_download", .real_rbox_file_download, ns = "metacheck")

test_that("repo_check reads the licence of the project a cited registration was made from (#486)", {
  # 10.1002/2688-8319.12402 cites registration TP78A, made from project
  # qw685, whose files repo_check lists.
  osf_info_mock <- function(osf_url, recursive = TRUE, pb = NULL, cache = FALSE, ...) {
    if (grepl("tp78a", osf_url, ignore.case = TRUE))
      return(data.frame(osf_url = osf_url, osf_type = "registrations", parent = "qw685",
                        kind = NA_character_, public = TRUE, name = NA_character_,
                        path = NA_character_, download_url = NA_character_, size = NA_real_,
                        filetype = NA_character_, provider = NA_character_))
    data.frame(osf_url = osf_url, osf_type = c("nodes", "files"), parent = NA_character_,
               kind = c(NA, "file"), public = TRUE, name = c(NA, "data.csv"),
               path = c(NA, "/data.csv"), download_url = c(NA, "https://osf.io/download/abcde"),
               size = c(NA, 10), filetype = c(NA, "data"), provider = c(NA, "osfstorage"))
  }
  paper <- test_paper(text = "Data: https://doi.org/10.17605/OSF.IO/TP78A")

  local_mocked_bindings(
    online = \(...) TRUE, osf_info = osf_info_mock,
    .osf_license = \(osf_id) if (osf_id == "qw685") "CC-By Attribution 4.0 International" else NA_character_
  )
  mo <- module_run(paper, "repo_check", peek_zips = FALSE, osf_license = TRUE)
  expect_equal(mo$repo_metadata$repo_url, "https://osf.io/qw685")
  expect_equal(mo$repo_metadata$license, "CC-By Attribution 4.0 International")
  expect_false(is.na(mo$repo_metadata$paper_id))

  # a parent without a licence takes the registration's
  local_mocked_bindings(
    .osf_license = \(osf_id) if (osf_id == "tp78a") "CC-By Attribution 4.0 International" else NA_character_
  )
  mo <- module_run(paper, "repo_check", peek_zips = FALSE, osf_license = TRUE)
  expect_equal(mo$repo_metadata$license, "CC-By Attribution 4.0 International")
})

test_that("repo_check reports an expired Dryad sharing link with its reason (#484)", {
  local_mocked_bindings(
    online = \(...) TRUE,
    .dryad_share_doi = \(url) list(doi = NA_character_, error = "private or expired Dryad sharing link")
  )
  paper <- test_paper(text = "Data: https://datadryad.org/stash/share/NOTAREALTOKEN123")
  mo <- module_run(paper, "repo_check", peek_zips = FALSE)
  expect_equal(mo$gated_repos$repo_error, "private or expired Dryad sharing link")
})

test_that("repo_check reports each link's own reason when every link of a platform fails (#476)", {
  # When every Figshare link failed, figshare_info() returned no doi/license
  # columns, and building the metadata table stopped with "arguments imply
  # differing number of rows", which then replaced each link's reason.
  local_mocked_bindings(
    online = \(...) TRUE,
    figshare_info = function(figshare_url, ...) data.frame(
      figshare_url = figshare_url, figshare_id = NA_character_, error = "unfound")
  )
  paper <- test_paper(text = "Data: https://figshare.com/articles/dataset/x/99999999")
  mo <- module_run(paper, "repo_check")
  expect_equal(mo$gated_repos$repo_error, "unfound")
  expect_false(any(grepl("differing number of rows", mo$gated_repos$repo_error)))
})

test_that("repo_check reports why a ResearchBox box could not be downloaded", {
  # rbox_file_download() lists each box that failed in its "failed" attribute;
  # repo_check used to give every box the same reason when all of them failed
  box <- "https://researchbox.org/99999997"
  local_mocked_bindings(
    online = \(...) TRUE,
    rbox_file_download = function(rb_url, ...) .rbox_no_files(data.frame(
      rb_url = box, name = NA_character_, error = "unfound")),
    # a box that could not be downloaded is not asked about again
    .rbox_license = \(...) stop("no licence request expected")
  )
  paper <- test_paper(url = box)
  mo <- suppressWarnings(module_run(paper, "repo_check"))
  expect_equal(mo$gated_repos$repo_error, "ResearchBox did not serve the box: unfound")
  expect_match(mo$summary_text, "We could not check 1 repository", fixed = TRUE)
  expect_equal(nrow(mo$repo_metadata), 0)
})

test_that("repo_check gives a ResearchBox box's licence", {
  d <- withr::local_tempdir()
  writeLines("id,x\n1,2", file.path(d, "study1.csv"))
  box <- "https://researchbox.org/99999"
  local_mocked_bindings(
    online = \(...) TRUE,
    rbox_file_download = function(rb_url, ...) data.frame(
      rb_url = box, name = "Data/study1.csv", file_location = file.path(d, "study1.csv"),
      size = 9, type = "data", isdir = FALSE),
    .rbox_license = \(rb_url, cache = FALSE) "CC-BY-4.0"
  )
  mo <- module_run(test_paper(url = box), "repo_check", peek_zips = FALSE)
  expect_equal(mo$repo_metadata$repo_url, box)
  expect_equal(mo$repo_metadata$license, "CC-BY-4.0")
  expect_true(is.na(mo$repo_metadata$doi))
})

test_that("repo_check reports OSF links it could not check, instead of dropping them (#501)", {
  # During an OSF outage (HTTP 503) every OSF link was removed with the
  # registrations, so the paper read as linking no repository at all.
  paper <- test_paper(text = "Data and code: https://doi.org/10.17605/OSF.IO/CKZVE")
  for (status in c(503L, 404L)) {
    osf_cache_clear()
    local_mocked_bindings(
      online = \(...) TRUE,
      .batch_query = function(urls, ...) lapply(urls, \(u) httr2::response(status_code = status)))
    mo <- suppressWarnings(module_run(paper, "repo_check", cache = FALSE))
    expect_equal(nrow(mo$gated_repos), 1)
    expect_equal(mo$gated_repos$repo_error,
                 if (status == 503L) "the OSF API did not answer (HTTP 503)"
                 else "not found on the OSF (HTTP 404)")
    expect_equal(mo$summary_table$repo_n, 1)
    expect_equal(mo$summary_table$repo_unchecked, 1)
    # not counted as empty, nor as lacking a README
    expect_match(mo$summary_text, "We could not check 1 repository", fixed = TRUE)
    expect_false(grepl("empty repositor", mo$summary_text))
    expect_match(mo$summary_text, "0 repositories without READMEs", fixed = TRUE)
    expect_equal(mo$traffic_light, "yellow")
  }
  # the failed lookups were not kept for the session: the 404 run above asked
  # the OSF again instead of replaying the 503
  osf_cache_clear()
})

test_that("an invalid OSF link is reported, and a nameless file does not stop the naming check (U122)", {
  # with only invalid OSF ids, filter(osf_type ...) found the osf_type()
  # function and dplyr's error text became the reason; beside valid links the
  # invalid one vanished
  llm_use(FALSE)
  mo <- suppressWarnings(suppressMessages(module_run(test_paper(url = "https://osf.io/ab"), "repo_check")))
  expect_equal(mo$gated_repos$repo_error, "not a valid OSF link")
  # check_file_naming(NA) stopped the module
  out <- check_file_naming(c(NA, "my file.csv"), c(NA, "my file.csv"))
  expect_equal(out$file_name, "my file.csv")
})

test_that("repo_check leaves out private files row by row, and keeps distinct files with repeated fields (U121, U123)", {
  # !isFALSE(public) tested the whole column; and duplicated(url) &
  # duplicated(path), tested separately, dropped one project's readme.txt
  # because another project had a file with the same (missing) URL and path
  osf_info_mock <- function(osf_url, recursive = TRUE, pb = NULL, cache = FALSE, ...) {
    data.frame(osf_url = osf_url, osf_type = c("nodes", "files", "files"),
               parent = NA_character_, kind = c(NA, "file", "file"),
               public = c(TRUE, TRUE, FALSE), name = c(NA, "readme.txt", "secret.csv"),
               path = c(NA, "/readme.txt", "/secret.csv"),
               download_url = NA_character_, size = c(NA, 10, 10),
               filetype = c(NA, "text", "data"), provider = c(NA, "osfstorage", "osfstorage"))
  }
  local_mocked_bindings(online = \(...) TRUE, osf_info = osf_info_mock)
  paper <- test_paper(text = "Data: https://osf.io/aaaaa and https://osf.io/bbbbb")
  mo <- module_run(paper, "repo_check", peek_zips = FALSE)
  expect_false("secret.csv" %in% mo$table$file_name)
  expect_equal(sum(mo$table$file_name == "readme.txt"), 2)
})
