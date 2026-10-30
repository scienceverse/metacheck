# FAIR_repo() on a small repository written to a temporary folder: no
# network, no LLM.

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
  res <- suppressMessages(FAIR_repo(test_paper("x"), output_dir = out,
                                    local_path = d, local_only = TRUE, ...))
  res
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

test_that("FAIR_repo changes a script's file paths and setwd() and says so", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  out <- file.path(withr::local_tempdir(), "out")
  expect_message(
    res <- FAIR_repo(test_paper("x"), output_dir = out,
                     local_path = d, local_only = TRUE),
    "setwd\\(\\) was removed")
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

test_that("FAIR_repo gives the second of two different files with one place a numbered name", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  dir.create(file.path(d, "extra"))
  dir.create(file.path(d, "copy"))
  # a different file with the same name: renamed
  utils::write.csv(data.frame(id = 9), file.path(d, "extra", "scores.csv"),
                   row.names = FALSE)
  # an identical file with the same name: left out
  file.copy(file.path(d, "data", "bom.csv"), file.path(d, "copy", "bom.csv"))
  res <- fair_run(d)
  expect_equal(nrow(res$renamed), 1)
  expect_equal(res$renamed$written, "data/study-scores_data_2.csv")
  expect_true(file.exists(file.path(res$output_dir, res$renamed$written)))
  expect_equal(sum(grepl("bom", list.files(file.path(res$output_dir, "data")))), 1)
})

test_that("FAIR_repo leaves an existing folder alone unless overwrite = TRUE", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  out <- withr::local_tempdir()
  writeLines("keep", file.path(out, "mine.txt"))
  expect_message(
    res <- FAIR_repo(test_paper("x"), output_dir = out,
                     local_path = d, local_only = TRUE),
    "already exists")
  expect_true(isTRUE(res$existed))
  expect_true(file.exists(file.path(out, "mine.txt")))

  suppressMessages(FAIR_repo(test_paper("x"), output_dir = out, overwrite = TRUE,
                             local_path = d, local_only = TRUE))
  expect_false(file.exists(file.path(out, "mine.txt")))
})

test_that("FAIR_repo reuses the checks in a captured result and runs only the missing ones", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  partial <- suppressMessages(report_module_run(
    test_paper("x"), "data_check",
    args = list(data_check = list(local_path = d, local_only = TRUE,
                                  download = "all"))))
  # only the checks the result lacks are run, on top of the reused data_check
  ran <- character(0)
  local_mocked_bindings(module_run = function(paper, module, ...) {
    ran <<- c(ran, module)
    structure(list(module = module, paper = paper$paper,
                   prev_outputs = c(paper$prev_outputs,
                                    stats::setNames(list(paper), paper$module)),
                   summary_table = data.frame(paper_id = "x"),
                   traffic_light = "info"),
              class = "metacheck_module_output")
  })
  out <- file.path(withr::local_tempdir(), "out")
  res <- suppressMessages(FAIR_repo(partial, output_dir = out))
  expect_equal(ran, c("codebook_check", "code_check", "reproducibility_check",
                      "funding_check"))
  expect_true(file.exists(file.path(out, "data", "study-scores_data.csv")))
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

  expect_true(file.exists(file.path(out, "requirements.txt")))
  expect_true("dplyr" %in% readLines(file.path(out, "requirements.txt")))
  expect_true(file.exists(file.path(out, "logs", "log.md")))
  expect_false(file.exists(file.path(out, "logs", "README.md")))
  # the paper's full text is not copied
  expect_false(dir.exists(file.path(out, "documentation")) &&
                 any(grepl("fulltext", list.files(file.path(out, "documentation")))))
  expect_length(list.files(file.path(out, "logs"), "manifest\\.json$"), 1)
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
                    "requirements.txt", "CHANGES.md") %in% ids$described))
})

test_that("FAIR_repo's metadata holds no path on the computer that ran it", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  res <- fair_run(d)
  # the name of the folder holding the repository, which is the same in a
  # short and a long spelling of its path (the repository's own folder name
  # is its name, and may be shown)
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

test_that("FAIR_repo copies data_check's own manifest without local paths", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  top <- basename(dirname(normalizePath(d)))
  manifest_dir <- withr::local_tempdir()
  captured <- suppressMessages(report_module_run(
    test_paper("x"), "data_check",
    args = list(data_check = list(local_path = d, local_only = TRUE,
                                  download = "all", manifest = manifest_dir))))
  expect_length(list.files(manifest_dir, "manifest\\.json$"), 1)
  out <- file.path(withr::local_tempdir(), "out")
  suppressMessages(FAIR_repo(captured, output_dir = out, local_path = d,
                             local_only = TRUE))
  logs <- list.files(file.path(out, "logs"), "manifest\\.json$", full.names = TRUE)
  expect_length(logs, 1)
  doc <- jsonlite::fromJSON(logs, simplifyVector = FALSE)
  # it is data_check's own manifest, with its reasons per file
  expect_false(is.null(doc$download))
  expect_false(grepl(top, paste(readLines(logs), collapse = "\n"), fixed = TRUE))
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
  res <- fair_run(d)
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
  expect_equal(length(roles), length(people))
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

test_that("FAIR_repo writes the metadata for a repository with no files", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- withr::local_tempdir()
  out <- file.path(withr::local_tempdir(), "out")
  expect_message(
    res <- FAIR_repo(test_paper("x"), output_dir = out,
                     local_path = d, local_only = TRUE),
    "No repository files")
  expect_true(res$empty_plan)
  expect_equal(res$n_files_copied, 0)
  expect_true(file.exists(file.path(out, "ro-crate-metadata.json")))
  expect_false(file.exists(file.path(out, "dataset_description.json")))
})

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

test_that("the RO-Crate has the paper, authors, CRediT roles and contact, and few unknowns", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  out <- file.path(withr::local_tempdir(), "out")
  suppressMessages(FAIR_repo(fair_paper(), output_dir = out, local_path = d,
                             local_only = TRUE))
  crate <- jsonlite::fromJSON(file.path(out, "ro-crate-metadata.json"),
                              simplifyVector = FALSE)
  g <- crate[["@graph"]]
  by_id <- stats::setNames(g, vapply(g, `[[`, character(1), "@id"))
  root <- by_id[["./"]]
  # the paper is cited, not given as the dataset's own identifier
  expect_equal(root$citation[["@id"]], "https://doi.org/10.1234/abc")
  expect_null(root$identifier)
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
  expect_true(all(c("#language-r", "#package-dplyr") %in% names(by_id)))
})

test_that("FAIR_repo writes a README from the RO-Crate when the repository has none", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  unlink(file.path(d, "README.txt"))
  out <- file.path(withr::local_tempdir(), "out")
  suppressMessages(FAIR_repo(fair_paper(), output_dir = out, local_path = d,
                             local_only = TRUE))
  readme <- readLines(file.path(out, "README.md"))
  expect_equal(readme[1], "# A test paper")
  for (h in c("## Authors", "## Paper", "## Licence", "## Files", "## File formats",
              "## Software", "## Scripts", "## Variables", "## Still to be filled in"))
    expect_true(h %in% readme, info = h)
  expect_true(any(grepl("Contact: Ann Smith <ann@x.org>", readme, fixed = TRUE)))
  expect_true(any(grepl("The CRediT roles of Bob Jones", readme, fixed = TRUE)))
  expect_true(any(grepl("| score | outcome measure |", readme, fixed = TRUE)))
  # the README is listed in the RO-Crate
  crate <- jsonlite::fromJSON(file.path(out, "ro-crate-metadata.json"),
                              simplifyVector = FALSE)
  expect_true("README.md" %in% crate_ids(crate)$described)
})

test_that("FAIR_repo leaves the repository's own README alone", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  res <- fair_run(d)
  expect_equal(readLines(file.path(res$output_dir, "README.txt")), "A readme.")
  expect_false(file.exists(file.path(res$output_dir, "README.md")))
})

test_that("FAIR_repo writes the funding it found into the RO-Crate", {
  withr::local_options(metacheck.llm.use = FALSE)
  d <- fair_fixture()
  p <- fair_paper()
  out <- file.path(withr::local_tempdir(), "out")
  local_mocked_bindings(.FAIR_repo_funding = function(paper, ops)
    list(statement = "Funded by NWO grant 123.", funders = "NWO", awards = "123"))
  suppressMessages(FAIR_repo(p, output_dir = out, local_path = d, local_only = TRUE))
  crate <- jsonlite::fromJSON(file.path(out, "ro-crate-metadata.json"),
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
  expect_equal(.FAIR_repo_credit_slug(c("Writing – review & editing",
                                        "Formal Analysis", "Cooking")),
               c("writing-review-editing", "formal-analysis", NA))
})

test_that("FAIR_repo converts one paper at a time", {
  expect_error(FAIR_repo(paperlist(test_paper("a"), test_paper("b"))),
               "one paper at a time")
})
