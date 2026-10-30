# FAIR_repo() on a small folder written to a temporary folder: no network, no
# LLM.

fair_fixture <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(d, "data"))
  dir.create(file.path(d, "Code"))
  utils::write.csv(data.frame(id = 1:5, score = c(2.1, 3.4, 1.9, 4.0, 2.8)),
                   file.path(d, "data", "scores.csv"), row.names = FALSE)
  # a byte order mark, which the copy drops
  con <- file(file.path(d, "data", "bom.csv"), "wb")
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("id,x\n1,2\n")), con)
  close(con)
  writeLines("A readme.", file.path(d, "README.txt"))
  writeLines(c('setwd("C:/Users/me/project")',
               'd <- read.csv("../data/scores.csv")',
               'e <- read.csv("old_scores.csv")',
               "library(dplyr)"),
             file.path(d, "Code", "analysis.R"))
  writeLines(c("var,description", "id,participant id", "score,outcome measure"),
             file.path(d, "codebook.csv"))
  d
}

# Every @id in an RO-Crate graph, and the ones it refers to.
crate_ids <- function(crate) {
  flat <- unlist(crate[["@graph"]])
  list(described = vapply(crate[["@graph"]], `[[`, character(1), "@id"),
       referenced = unique(unname(flat[grepl("@id$", names(flat))])))
}

fair_run <- function(d, ...) {
  out <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "out")
  suppressMessages(FAIR_repo(d, output_dir = out, ...))
}

fair_paper <- function() {
  p <- test_paper("x")
  p$info$title <- "A test paper"
  p$info$doi <- "10.1234/abc"
  p$author <- data.frame(author_id = 1:2, given = c("Ann", "Bob"),
                         family = c("Smith", "Jones"),
                         email = c("ann@x.org", NA), corresponding = c(TRUE, FALSE),
                         orcid = c("0000-0001-2345-6789", NA),
                         affiliation = c("TU/e", NA))
  p$author$credit_roles <- list(c("Conceptualization", "Writing - original draft"),
                                character(0))
  p
}

test_that("FAIR_repo copies every file to its place in the new layout", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  res <- fair_run(d)
  out <- res$output_dir
  expect_true(file.exists(file.path(out, "data", "study-scores_data.csv")))
  expect_true(file.exists(file.path(out, "analysis", "analysis.R")))
  expect_true("README.txt" %in% basename(list.files(out, recursive = TRUE)))
  expect_equal(res$n_files_copied, sum(!is.na(res$plan$written)))
  expect_length(res$skipped, 0)

  # a data CSV is copied without its byte order mark
  con <- file(file.path(out, "data", "study-bom_data.csv"), "rb")
  b <- readBin(con, "raw", 3); close(con)
  expect_false(identical(b, as.raw(c(0xEF, 0xBB, 0xBF))))
})

test_that("FAIR_repo leaves the original folder unchanged", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  before <- tools::md5sum(list.files(d, recursive = TRUE, full.names = TRUE))
  fair_run(d)
  after <- tools::md5sum(list.files(d, recursive = TRUE, full.names = TRUE))
  expect_equal(after, before)
})

test_that("FAIR_repo goes online nowhere, also when the paper links a repository", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  p <- fair_paper()
  p$text$text <- "Data and code are at https://osf.io/abcde and https://github.com/a/b."
  online <- character(0)
  httr2::local_mocked_responses(function(req) {
    online <<- c(online, req$url)
    stop("went online: ", req$url)
  })
  local_mocked_bindings(
    curl_fetch_memory = function(url, ...) { online <<- c(online, url); stop("went online") },
    curl_fetch_disk = function(url, ...) { online <<- c(online, url); stop("went online") },
    curl_download = function(url, ...) { online <<- c(online, url); stop("went online") },
    .package = "curl")
  res <- fair_run(d, paper = p)
  expect_equal(online, character(0))
  expect_gt(res$n_files_copied, 0)
})

test_that("FAIR_repo changes a script's file paths and setwd() and says so", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  out <- file.path(withr::local_tempdir(), "out")
  expect_message(res <- FAIR_repo(d, output_dir = out), "setwd\\(\\) was removed")
  code <- readLines(file.path(out, "analysis", "analysis.R"))
  expect_true(any(grepl('read.csv("data/study-scores_data.csv")', code, fixed = TRUE)))
  # a longer name that ends in the same file name is left alone
  expect_true(any(grepl('read.csv("old_scores.csv")', code, fixed = TRUE)))
  expect_true(grepl("^invisible\\(NULL\\)  # \\[metacheck FAIR_repo removed setwd\\]", code[1]))
  expect_equal(res$scripts$setwd_removed, 1)
  expect_equal(res$scripts$setwd_paths, "C:/Users/me/project")
  expect_equal(res$scripts$paths_changed, 1)
})

test_that("FAIR_repo copies a script with nothing to change byte for byte", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  con <- file(file.path(d, "Code", "helper.R"), "wb")
  writeBin(charToRaw("x <- 1\r\ny <- 2\r\n"), con)
  close(con)
  res <- fair_run(d)
  copy <- file.path(res$output_dir, "analysis", "helper.R")
  expect_equal(unname(tools::md5sum(copy)),
               unname(tools::md5sum(file.path(d, "Code", "helper.R"))))
})

test_that("FAIR_repo writes a data file that is not a CSV as a real CSV and keeps the original", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  openxlsx::write.xlsx(data.frame(id = 1:3, cond = c("a", "b", "a")),
                       file.path(d, "data", "survey.xlsx"))
  res <- fair_run(d)
  data_dir <- file.path(res$output_dir, "data")
  back <- utils::read.csv(file.path(data_dir, "study-survey_data.csv"))
  expect_equal(nrow(back), 3)
  expect_equal(names(back), c("id", "cond"))
  expect_true(file.exists(file.path(data_dir, "survey.xlsx")))
})

test_that("FAIR_repo keeps a data file that holds no table as it is, and two originals with one name apart", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  # a list is no table (and no model): kept as it is, not reported as failed
  saveRDS(list(a = 1, b = letters), file.path(d, "data", "lookup.rds"))
  # two different R tables with one name in one folder of the copy
  dir.create(file.path(d, "data", "a"))
  dir.create(file.path(d, "data", "a", "b"))
  saveRDS(data.frame(x = 1:3), file.path(d, "data", "a", "tab.rds"))
  file.copy(file.path(d, "data", "a", "tab.rds"), file.path(d, "data", "a", "b"))
  saveRDS(data.frame(x = 4:6), file.path(d, "data", "a", "b", "tab.rds"))
  res <- fair_run(d)
  data_dir <- file.path(res$output_dir, "data")
  expect_length(res$copy_failed, 0)
  expect_true(file.exists(file.path(data_dir, "lookup.rds")))
  expect_false(file.exists(file.path(data_dir, "study-lookup_data.csv")))
  # each R table is written as CSV, with its own original next to it
  expect_equal(readRDS(file.path(data_dir, "a", "tab.rds"))$x, 1:3)
  expect_equal(readRDS(file.path(data_dir, "a", "b", "tab.rds"))$x, 4:6)
  expect_true(file.exists(file.path(data_dir, "a", "study-tab_data.csv")))
  expect_true(file.exists(file.path(data_dir, "a", "b", "study-tab_data.csv")))
})

test_that(".FAIR_repo_copy keeps two different originals with one place apart", {
  # with folders kept this is rare, but a converted original placed where
  # another original already is got overwritten
  d <- withr::local_tempdir()
  saveRDS(data.frame(x = 1:3), file.path(d, "one.rds"))
  saveRDS(data.frame(x = 4:6), file.path(d, "two.rds"))
  plan <- data.frame(file_name = c("tab.rds", "tab.rds"), data_type = "data",
                     group = NA_character_,
                     target_path = c("data/study-tab_data.csv", "data/study-tab_data.csv"),
                     convert = TRUE, original_target = c("data/tab.rds", "data/tab.rds"))
  out <- withr::local_tempdir()
  copied <- .FAIR_repo_copy(plan, data.frame(file_location = file.path(d, c("one.rds", "two.rds"))),
                            out)
  originals <- list.files(file.path(out, "data"), "\\.rds$", full.names = TRUE)
  expect_length(originals, 2)
  expect_setequal(lapply(originals, function(f) readRDS(f)$x), list(1:3, 4:6))
})

test_that("FAIR_repo gives the second of two different files with one place a numbered name", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  # x.csv and x.tsv in one folder both become study-x_data.csv
  writeLines(c("a,b", "1,2"), file.path(d, "data", "x.csv"))
  writeLines(c("a\tb", "3\t4"), file.path(d, "data", "x.tsv"))
  # files with one name in different folders stay apart in their folders
  dir.create(file.path(d, "extra"))
  utils::write.csv(data.frame(id = 9), file.path(d, "extra", "scores.csv"),
                   row.names = FALSE)
  res <- fair_run(d)
  expect_equal(nrow(res$renamed), 1)
  expect_equal(res$renamed$written, "data/study-x_data_2.csv")
  expect_true(file.exists(file.path(res$output_dir, res$renamed$written)))
  expect_true(file.exists(file.path(res$output_dir, "data", "extra", "study-scores_data.csv")))
})

test_that("FAIR_repo leaves out a file from a zip that is also next to it, and leaves the folder unchanged", {
  skip_if_not_installed("zip")
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  # the folder holds a zip of its own data folder, next to that folder
  zip::zip(file.path(d, "data.zip"), files = "data", root = d)
  before <- tools::md5sum(list.files(d, recursive = TRUE, full.names = TRUE))
  res <- fair_run(d)
  after <- tools::md5sum(list.files(d, recursive = TRUE, full.names = TRUE))
  # the zip is unpacked elsewhere, never into the researcher's folder
  expect_equal(after, before)
  # each data file is in the copy once
  csvs <- list.files(res$output_dir, "scores", recursive = TRUE)
  expect_equal(csvs, "data/study-scores_data.csv")
  expect_gt(nrow(res$identical), 0)
  expect_true(all(res$identical$same_as %in% res$plan$written))
  changes <- readLines(file.path(res$output_dir, "CHANGES.md"))
  expect_true(any(grepl("identical file", changes)))
  # a left-out file is not listed as missing
  crate <- jsonlite::fromJSON(file.path(res$output_dir, "ro-crate-metadata.json"),
                              simplifyVector = FALSE)
  expect_null(crate[["@graph"]][[2]][["metacheck:notIncluded"]])
})

test_that("FAIR_repo keeps a README in a subfolder in documentation/, and an identical one once", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  dir.create(file.path(d, "nest-1"))
  dir.create(file.path(d, "copy of top"))
  writeLines("About the files in nest-1.", file.path(d, "nest-1", "README"))
  writeLines("A readme.", file.path(d, "copy of top", "README.txt"))
  res <- fair_run(d)
  out <- res$output_dir
  expect_equal(readLines(file.path(out, "README.txt")), "A readme.")
  expect_equal(readLines(file.path(out, "documentation", "nest-1_README")),
               "About the files in nest-1.")
  expect_false(file.exists(file.path(out, "documentation", "copy-of-top_README.txt")))
  expect_equal(res$identical$same_as[grepl("copy of top", res$identical$path)],
               "README.txt")
  expect_false(any(grepl("^README_[0-9]", list.files(out))))
})

test_that("FAIR_repo leaves an existing folder alone unless overwrite = TRUE", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  out <- withr::local_tempdir()
  writeLines("keep", file.path(out, "mine.txt"))
  expect_message(res <- FAIR_repo(d, output_dir = out), "already exists")
  expect_true(isTRUE(res$existed))
  expect_true(file.exists(file.path(out, "mine.txt")))

  suppressMessages(FAIR_repo(d, output_dir = out, overwrite = TRUE))
  expect_false(file.exists(file.path(out, "mine.txt")))
})

test_that("FAIR_repo names the copy after the folder by default", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  withr::local_dir(withr::local_tempdir())
  res <- suppressMessages(FAIR_repo(d))
  expect_equal(res$output_dir, paste0(basename(d), "_FAIR"))
  expect_true(dir.exists(res$output_dir))
})

test_that("FAIR_repo stops for a path that is not a folder, or a copy inside it", {
  expect_error(FAIR_repo(file.path(tempdir(), "no_such_folder")), "No folder found")
  expect_error(FAIR_repo(c("a", "b")), "single path")
  d <- withr::local_tempdir()
  expect_error(FAIR_repo(d, output_dir = file.path(d, "copy")), "outside the folder")
  expect_error(FAIR_repo(d, paper = list(1)), "paper object")
})

test_that("FAIR_repo writes the metadata files", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  res <- fair_run(d)
  out <- res$output_dir

  desc <- jsonlite::fromJSON(file.path(out, "dataset_description.json"),
                             simplifyVector = FALSE)
  vars <- desc$variableMeasured
  names_ <- vapply(vars, `[[`, character(1), "name")
  expect_true(all(c("id", "score") %in% names_))
  # the codebook's label is the variable's description
  expect_equal(vars[[which(names_ == "score")[1]]]$description, "outcome measure")

  # R packages are not put in requirements.txt, which pip reads
  expect_false(file.exists(file.path(out, "requirements.txt")))
  expect_true(file.exists(file.path(out, "logs", "log.md")))
  expect_false(file.exists(file.path(out, "logs", "README.md")))
  # the paper's full text is not copied
  expect_false(dir.exists(file.path(out, "documentation")) &&
                 any(grepl("fulltext", list.files(file.path(out, "documentation")))))
  expect_length(list.files(file.path(out, "logs"), "files\\.json$"), 1)
  expect_length(list.files(file.path(out, "logs"), "\\.rds$"), 1)

  # CHANGES.md tells the user about the removed setwd()
  changes <- readLines(file.path(out, "CHANGES.md"))
  expect_true(any(grepl("setwd", changes)))
  expect_true(any(grepl("C:/Users/me/project", changes, fixed = TRUE)))

  # the RO-Crate describes every file it refers to, once
  crate <- jsonlite::fromJSON(file.path(out, "ro-crate-metadata.json"),
                              simplifyVector = FALSE)
  ids <- crate_ids(crate)
  expect_false(any(duplicated(ids$described)))
  local <- ids$referenced[!grepl("^https?://", ids$referenced)]
  expect_equal(setdiff(local, ids$described), character(0))
  expect_true(all(c("data/study-scores_data.csv", "analysis/analysis.R",
                    "CHANGES.md") %in% ids$described))
})

test_that("FAIR_repo's metadata holds no path on the computer that ran it", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  res <- fair_run(d)
  # the name of the folder holding the folder, which is the same in a short
  # and a long spelling of its path (the folder's own name is its name, and
  # may be shown)
  top <- basename(dirname(normalizePath(d)))
  for (f in c("ro-crate-metadata.json", "dataset_description.json",
              list.files(file.path(res$output_dir, "logs"), "json$", full.names = TRUE))) {
    path <- if (file.exists(f)) f else file.path(res$output_dir, f)
    txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_false(grepl(top, txt, fixed = TRUE), info = f)
  }
  logs <- readRDS(list.files(file.path(res$output_dir, "logs"), "\\.rds$",
                             full.names = TRUE))
  expect_false(any(grepl(top, unlist(logs), fixed = TRUE)))
})

test_that("FAIR_repo records which files each script reads", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  res <- fair_run(d)
  crate <- jsonlite::fromJSON(file.path(res$output_dir, "ro-crate-metadata.json"),
                              simplifyVector = FALSE)
  runs <- Filter(function(e) identical(e$eventType, "script run"), crate[["@graph"]])
  expect_length(runs, 1)
  expect_equal(runs[[1]]$instrument[["@id"]], "analysis/analysis.R")
  expect_equal(runs[[1]]$object[[1]][["@id"]], "data/study-scores_data.csv")
})

test_that("FAIR_repo gives each study its own folder and metadata", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- withr::local_tempdir()
  for (s in c("study1", "study2")) {
    dir.create(file.path(d, s))
    utils::write.csv(data.frame(id = 1:3, x = c(1.5, 2.5, 3.5)),
                     file.path(d, s, paste0(s, "_scores.csv")), row.names = FALSE)
  }
  res <- fair_run(d, paper = fair_paper())
  out <- res$output_dir
  expect_equal(res$n_studies, 2)
  for (s in c("study-ex1", "study-ex2"))
    for (f in c("dataset_description.json", "README.md", "CHANGES.md"))
      expect_true(file.exists(file.path(out, s, f)), info = file.path(s, f))

  crate <- jsonlite::fromJSON(file.path(out, "ro-crate-metadata.json"),
                              simplifyVector = FALSE)
  ids <- crate_ids(crate)
  expect_true(all(c("study-ex1/", "study-ex2/", "#study-design-ex1",
                    "#study-design-ex2") %in% ids$described))
  # CRediT roles are one entry per author, not a set per study
  roles <- Filter(function(e) identical(e[["@type"]], "Role"), crate[["@graph"]])
  people <- Filter(function(e) identical(e[["@type"]], "Person"), crate[["@graph"]])
  expect_equal(length(roles), 2)
  expect_equal(length(people), 2)
  expect_false(any(duplicated(ids$described)))
})

test_that("the study design is filled from a single preregistration only", {
  pr <- function(n) list(prereg_check = list(table = data.frame(
    paper_id = "p", research_questions = paste("Q", seq_len(n)),
    study_design_overview = "between", data_exclusion_criteria = NA,
    sample_size = "100")))
  one <- .FAIR_repo_study_design_entity(pr(1), "p")
  expect_equal(one$purpose, list("Q 1"))
  expect_equal(one$targetSampleSize, list("100"))
  expect_equal(one$dataCollectionMethodology, list("between"))
  # a field not found is left out, except how the data were collected
  expect_null(one$populationOfConcern)
  two <- .FAIR_repo_study_design_entity(pr(2), "p")
  expect_null(two$purpose)
  expect_equal(two$dataCollectionMethodology, list("unknown"))
  none <- .FAIR_repo_study_design_entity(list(), "p")
  expect_null(none$purpose)
  expect_equal(none$dataCollectionMethodology, list("unknown"))
})

test_that("FAIR_repo writes the metadata for a folder with no files", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- withr::local_tempdir()
  out <- file.path(withr::local_tempdir(), "out")
  expect_message(res <- FAIR_repo(d, output_dir = out), "No files")
  expect_true(res$empty_plan)
  expect_equal(res$n_files_copied, 0)
  expect_true(file.exists(file.path(out, "ro-crate-metadata.json")))
  expect_false(file.exists(file.path(out, "dataset_description.json")))
})

test_that("the RO-Crate has the paper, authors, CRediT roles and contact, and few unknowns", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  res <- fair_run(d, paper = fair_paper())
  crate <- jsonlite::fromJSON(file.path(res$output_dir, "ro-crate-metadata.json"),
                              simplifyVector = FALSE)
  g <- crate[["@graph"]]
  by_id <- stats::setNames(g, vapply(g, `[[`, character(1), "@id"))
  root <- by_id[["./"]]
  # the paper is cited, not given as the dataset's own identifier
  expect_equal(root$citation[["@id"]], "https://doi.org/10.1234/abc")
  expect_null(root$identifier)
  expect_equal(root$name, "A test paper")
  ann <- by_id[["https://orcid.org/0000-0001-2345-6789"]]
  expect_equal(ann$email, "ann@x.org")
  expect_equal(by_id[["mailto:ann@x.org"]]$contactType, "corresponding author")
  roles <- Filter(function(e) identical(e[["@type"]], "Role"), g)
  expect_length(roles, 2)
  expect_equal(vapply(roles[[1]]$roleName, `[[`, character(1), "@id"),
               c("https://credit.niso.org/contributor-roles/conceptualization/",
                 "https://credit.niso.org/contributor-roles/writing-original-draft/"))
  expect_equal(roles[[2]]$roleName, "unknown")
  # unknown only for the licence, Bob's roles, and when and how the data were
  # collected and processed
  expect_equal(sum(unlist(g) == "unknown"), 5)
  # the language and package the code uses
  expect_true(all(c("#language-r", "#package-r-dplyr") %in% names(by_id)))
  expect_equal(by_id[["#package-r-dplyr"]]$programmingLanguage[["@id"]], "#language-r")
})

test_that("without a paper, the copy is named after the folder and the README asks for the paper", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  unlink(file.path(d, "README.txt"))
  res <- fair_run(d)
  crate <- jsonlite::fromJSON(file.path(res$output_dir, "ro-crate-metadata.json"),
                              simplifyVector = FALSE)
  expect_equal(crate[["@graph"]][[2]]$name, basename(d))
  expect_false(any(vapply(crate[["@graph"]], function(e)
    identical(e[["@type"]], "Person"), logical(1))))
  # the authors are a field to fill in
  expect_equal(crate[["@graph"]][[2]]$author, "unknown")
  readme <- readLines(file.path(res$output_dir, "README.md"))
  expect_equal(readme[1], paste("#", basename(d)))
  expect_true(all(c("- The title", "- The authors", "- The paper that uses these files")
                  %in% readme))
})

test_that("without a paper, the title comes from the folder's own README", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  writeLines(c("# Water quality at the festival", "", "Some text."),
             file.path(d, "README.txt"))
  res <- fair_run(d)
  crate <- jsonlite::fromJSON(file.path(res$output_dir, "ro-crate-metadata.json"),
                              simplifyVector = FALSE)
  expect_equal(crate[["@graph"]][[2]]$name, "Water quality at the festival")
  desc <- jsonlite::fromJSON(file.path(res$output_dir, "dataset_description.json"))
  expect_equal(desc$name, "Water quality at the festival")
  # a plain first line is no title
  expect_null(.FAIR_repo_readme_title("README.txt", {
    o <- withr::local_tempdir()
    writeLines("This readme.text file was generated on 2024-01-01 by me", file.path(o, "README.txt"))
    o
  }))
})

test_that("FAIR_repo treats a top folder that holds every file as the top", {
  withr::local_options(metacheck.llm.use = FALSE)
  base <- fair_fixture()
  # a repository downloaded from GitHub sits in "<name>-<commit>/"
  d <- withr::local_tempdir()
  file.copy(base, d, recursive = TRUE)
  file.rename(file.path(d, basename(base)), file.path(d, "repo-1a2b3c4"))
  res <- fair_run(d)
  out <- res$output_dir
  expect_equal(readLines(file.path(out, "README.txt")), "A readme.")
  expect_false(file.exists(file.path(out, "README.md")))
  expect_true(file.exists(file.path(out, "data", "study-scores_data.csv")))
})

test_that("FAIR_repo keeps the folders inside data/, so files with one name stay apart", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  for (s in c("scenario_lb", "scenario_ub")) {
    dir.create(file.path(d, "data", s))
    utils::write.csv(data.frame(t = 1:2, v = if (s == "scenario_lb") 1:2 else 3:4),
                     file.path(d, "data", s, "forcing.csv"), row.names = FALSE)
    # identical in both scenarios, and still kept in both
    writeLines(c("a,b", "1,2"), file.path(d, "data", s, "soil.csv"))
  }
  res <- fair_run(d)
  data_dir <- file.path(res$output_dir, "data")
  for (s in c("scenario_lb", "scenario_ub"))
    expect_setequal(list.files(file.path(data_dir, s)),
                    c("study-forcing_data.csv", "study-soil_data.csv"))
  expect_equal(nrow(res$renamed), 0)
  expect_equal(nrow(res$identical), 0)
})

test_that("FAIR_repo puts results in outputs/: files only scripts write, and fitted models", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  writeLines(c('d <- read.csv("../data/scores.csv")',
               'write.csv(summary(d), "results_table.csv")'),
             file.path(d, "Code", "summarise.R"))
  # the results the script wrote, shared next to the code
  writeLines(c("x,y", "1,2"), file.path(d, "Code", "results_table.csv"))
  saveRDS(stats::lm(dist ~ speed, datasets::cars), file.path(d, "data", "fit.rds"))
  # an R table with a CSV of the same name next to it is not converted again
  saveRDS(data.frame(id = 1:5), file.path(d, "data", "scores.rds"))
  res <- fair_run(d)
  out <- res$output_dir
  expect_true(file.exists(file.path(out, "outputs", "results_table.csv")))
  expect_true(file.exists(file.path(out, "outputs", "fit.rds")))
  expect_false(file.exists(file.path(out, "data", "study-fit_data.csv")))
  expect_true(file.exists(file.path(out, "data", "scores.rds")))
  expect_false(any(grepl("scores_data_2", list.files(file.path(out, "data")))))
  # the script now writes its results into outputs/, where they are shared
  code <- readLines(file.path(out, "analysis", "summarise.R"))
  expect_true(any(grepl('"outputs/results_table.csv"', code, fixed = TRUE)))
})

test_that("requirements.txt lists the Python packages pip can install, nothing else", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  writeLines(c("import os", "import json", "import pandas as pd",
               "from sklearn.linear_model import LinearRegression"),
             file.path(d, "Code", "model.py"))
  unlink(file.path(d, "README.txt"))
  res <- fair_run(d)
  req <- readLines(file.path(res$output_dir, "requirements.txt"))
  pkgs <- req[!startsWith(req, "#")]
  expect_setequal(pkgs, c("pandas", "scikit-learn"))
  # the R packages are named in the README instead
  readme <- readLines(file.path(res$output_dir, "README.md"))
  expect_true(any(grepl("R packages: dplyr", readme, fixed = TRUE)))
  expect_true(any(grepl("Python packages: pandas, scikit-learn", readme, fixed = TRUE)))
})

test_that("FAIR_repo writes no dataset_description.json for a folder of code only", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- withr::local_tempdir()
  writeLines("x <- 1", file.path(d, "analysis.R"))
  res <- fair_run(d)
  expect_false(file.exists(file.path(res$output_dir, "dataset_description.json")))
  expect_true(file.exists(file.path(res$output_dir, "ro-crate-metadata.json")))
})

test_that("FAIR_repo writes a README from the RO-Crate when the folder has none", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  unlink(file.path(d, "README.txt"))
  res <- fair_run(d, paper = fair_paper())
  readme <- readLines(file.path(res$output_dir, "README.md"))
  expect_equal(readme[1], "# A test paper")
  for (h in c("## Authors", "## Paper", "## Licence", "## Files", "## File formats",
              "## Software", "## Scripts", "## Variables", "## Still to be filled in"))
    expect_true(h %in% readme, info = h)
  expect_true(any(grepl("Contact: Ann Smith <ann@x.org>", readme, fixed = TRUE)))
  expect_true(any(grepl("The CRediT roles of Bob Jones", readme, fixed = TRUE)))
  expect_true(any(grepl("| score | outcome measure |", readme, fixed = TRUE)))
  # the README is listed in the RO-Crate
  crate <- jsonlite::fromJSON(file.path(res$output_dir, "ro-crate-metadata.json"),
                              simplifyVector = FALSE)
  expect_true("README.md" %in% crate_ids(crate)$described)
})

test_that("FAIR_repo leaves the folder's own README alone", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  res <- fair_run(d)
  expect_equal(readLines(file.path(res$output_dir, "README.txt")), "A readme.")
  expect_false(file.exists(file.path(res$output_dir, "README.md")))
})

test_that("FAIR_repo writes the funding it found into the RO-Crate", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  local_mocked_bindings(.FAIR_repo_funding = function(paper, ops)
    list(statement = "Funded by NWO grant 123.", funders = "NWO", awards = "123"))
  res <- fair_run(d, paper = fair_paper())
  crate <- jsonlite::fromJSON(file.path(res$output_dir, "ro-crate-metadata.json"),
                              simplifyVector = FALSE)
  g <- crate[["@graph"]]
  by_id <- stats::setNames(g, vapply(g, `[[`, character(1), "@id"))
  expect_equal(by_id[["./"]]$funding[["@id"]], "#funding")
  expect_equal(by_id[["#funding"]]$description, "Funded by NWO grant 123.")
  expect_equal(by_id[["#funder-1"]]$name, "NWO")
})

test_that(".FAIR_repo_funding reads funding_check's sentences and bibr's funders", {
  p <- test_paper("x")
  expect_null(.FAIR_repo_funding(p, list()))
  ops <- list(funding_check = list(table = data.frame(paper_id = "x",
                                                      text = "Funded by NWO.")))
  expect_equal(.FAIR_repo_funding(p, ops)$statement, "Funded by NWO.")
  p$info$funding_statement <- "Supported by ERC."
  p$funding <- data.frame(funding_id = 1, funder = "ERC")
  p$funding$award_ids <- list("A1")
  f <- .FAIR_repo_funding(p, ops)
  expect_equal(f$statement, "Supported by ERC.")
  expect_equal(f$funders, "ERC")
  expect_equal(f$awards, "A1")
})

test_that(".FAIR_repo_credit_slug recognises CRediT roles as papers write them", {
  expect_equal(.FAIR_repo_credit_slug(c("Writing \u2013 review & editing",
                                        "Formal Analysis", "Cooking")),
               c("writing-review-editing", "formal-analysis", NA))
})
