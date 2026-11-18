# Provenance kept for a TRO (R/provenance.R; metacheck #404)

test_that(".sha256_file hashes files, gives NA for what it cannot hash", {
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a.txt")
  writeLines("hello", a)
  expected <- unname(tools::sha256sum(a))
  expect_equal(metacheck:::.sha256_file(a), expected)
  # missing file, folder, NA: NA, and the others keep their hash
  h <- metacheck:::.sha256_file(c(a, file.path(dir, "missing.txt"), dir, NA))
  expect_equal(h, c(expected, NA, NA, NA))
  expect_identical(metacheck:::.sha256_file(character(0)), character(0))
  # the old name used by the bibr import and the Dryad check gives the same
  expect_equal(metacheck:::.bibr12_sha256(a), expected)
  # a known value: the SHA-256 of the empty file
  e <- file.path(dir, "empty")
  file.create(e)
  expect_equal(metacheck:::.sha256_file(e),
               "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
})

test_that(".sha256_file streams a large file (same hash as digest)", {
  skip_if_not_installed("digest")
  dir <- withr::local_tempdir()
  big <- file.path(dir, "big.bin")
  con <- file(big, "wb")
  for (i in 1:40) writeBin(as.raw(rep(i %% 256, 1024^2)), con)   # 40 MB
  close(con)
  expect_equal(metacheck:::.sha256_file(big),
               digest::digest(big, algo = "sha256", file = TRUE))
})

test_that(".sha256_file gives NA for an unreadable file", {
  skip_on_os("windows")   # chmod 000 does not stop the owner reading on Windows
  skip_if(Sys.info()[["user"]] == "root")
  dir <- withr::local_tempdir()
  f <- file.path(dir, "secret.txt")
  writeLines("x", f)
  Sys.chmod(f, "000")
  on.exit(Sys.chmod(f, "644"), add = TRUE)
  expect_true(is.na(metacheck:::.sha256_file(f)))
})

test_that(".repro_iso_utc writes UTC times with milliseconds", {
  t <- as.POSIXct("2026-11-18 21:05:03.25", tz = "Europe/Amsterdam")
  expect_equal(metacheck:::.repro_iso_utc(t), "2026-11-18T20:05:03.250Z")
  expect_true(is.na(metacheck:::.repro_iso_utc(NULL)))
  expect_true(is.na(metacheck:::.repro_iso_utc(as.POSIXct(NA))))
})

test_that(".repro_add_run_clock adds the columns, NA without a clock", {
  df <- data.frame(file_name = c("a.R", "b.R"), outcome = "ran_ok")
  clock <- new.env()
  clock$started <- as.POSIXct("2026-11-18 10:00:00", tz = "UTC")
  clock$ended <- as.POSIXct("2026-11-18 10:00:05", tz = "UTC")
  clock$status <- 0L
  out <- metacheck:::.repro_add_run_clock(df, clock)
  expect_equal(out$started_at, rep("2026-11-18T10:00:00.000Z", 2))
  expect_equal(out$ended_at, rep("2026-11-18T10:00:05.000Z", 2))
  expect_equal(out$exit_status, c(0L, 0L))
  # a skipped script (no clock), and a row that already has the columns
  none <- metacheck:::.repro_add_run_clock(df)
  expect_true(all(is.na(none$started_at)) && all(is.na(none$exit_status)))
  expect_equal(metacheck:::.repro_add_run_clock(out), out)
  # an empty result gets the columns too, so rows of every pass bind
  empty <- metacheck:::.repro_add_run_clock(df[0, ])
  expect_true(all(c("started_at", "ended_at", "exit_status") %in% names(empty)))
  expect_equal(nrow(dplyr::bind_rows(empty, out, none)), 4)
})

test_that(".repro_exit_status reads processx, callr and failures", {
  expect_equal(metacheck:::.repro_exit_status(list(status = 1L)), 1L)
  expect_equal(metacheck:::.repro_exit_status(list(status = 0)), 0L)
  expect_true(is.na(metacheck:::.repro_exit_status(NULL)))
  expect_true(is.na(metacheck:::.repro_exit_status(simpleError("docker failed"))))
  e <- structure(class = c("callr_status_error", "error", "condition"),
                 list(message = "exited", call = NULL, status = 2L))
  expect_equal(metacheck:::.repro_exit_status(e), 2L)
  # stopped at the time limit: the stopped docker command's status is not the script's
  expect_true(is.na(metacheck:::.repro_exit_status(list(status = 2L, timeout = TRUE))))
})

test_that(".repro_artifacts lists every file with hash, size and type", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "data"))
  dir.create(file.path(dir, "_lib", "pkg"), recursive = TRUE)
  dir.create(file.path(dir, "_pylib"))
  writeLines("x,y\n1,2", file.path(dir, "data", "d.csv"))
  writeLines("print(1)", file.path(dir, "analysis.R"))
  writeLines("hidden", file.path(dir, ".Rprofile"))
  writeLines("lib", file.path(dir, "_lib", "pkg", "DESCRIPTION"))
  writeLines("lib", file.path(dir, "_pylib", "m.py"))
  writeLines("runner", file.path(dir, ".runner_analysis.R"))
  a <- metacheck:::.repro_artifacts(dir, "before_run")
  expect_setequal(a$path, c("data/d.csv", "analysis.R", ".Rprofile"))
  expect_true(all(a$stage == "before_run"))
  expect_equal(a$sha256[a$path == "data/d.csv"],
               unname(tools::sha256sum(file.path(dir, "data", "d.csv"))))
  expect_equal(a$size[a$path == "analysis.R"], file.size(file.path(dir, "analysis.R")))
  expect_equal(a$mime_type[a$path == "data/d.csv"], "text/csv")
  expect_equal(a$mime_type[a$path == "analysis.R"], "text/x-r-source")
  expect_true(is.na(a$mime_type[a$path == ".Rprofile"]))
  # an empty or missing folder: no rows, same columns
  e <- metacheck:::.repro_artifacts(withr::local_tempdir(), "before_run")
  expect_equal(nrow(e), 0)
  expect_equal(names(e), names(a))
  expect_equal(nrow(metacheck:::.repro_artifacts(file.path(dir, "nope"), "x")), 0)
})

test_that(".repro_artifacts keeps file names in other encodings as valid UTF-8", {
  skip_on_os("windows")   # Windows file names are UTF-16; no invalid bytes there
  dir <- withr::local_tempdir()
  # a CP437/Latin-1 byte in a name, as in old zips
  name <- rawToChar(as.raw(c(0x64, 0x61, 0x74, 0x61, 0x82, 0x2e, 0x63, 0x73, 0x76)))
  Encoding(name) <- "bytes"
  ok <- tryCatch(file.create(file.path(dir, name)), error = function(e) FALSE,
                 warning = function(w) FALSE)
  skip_if_not(isTRUE(ok))
  a <- metacheck:::.repro_artifacts(dir, "before_run")
  expect_equal(nrow(a), 1)
  expect_true(validUTF8(a$path))
  expect_match(a$path, "<82>", fixed = TRUE)
  expect_false(is.na(a$sha256))
  expect_silent(jsonlite::toJSON(a))
})

test_that(".repro_artifacts_after keeps new, changed and removed files", {
  dir <- withr::local_tempdir()
  writeLines("a", file.path(dir, "same.txt"))
  writeLines("b", file.path(dir, "changes.txt"))
  writeLines("c", file.path(dir, "goes.txt"))
  before <- metacheck:::.repro_artifacts(dir, "before_run")
  writeLines("B", file.path(dir, "changes.txt"))
  unlink(file.path(dir, "goes.txt"))
  dir.create(file.path(dir, "output"))
  writeLines("result", file.path(dir, "output", "table.csv"))
  after <- metacheck:::.repro_artifacts_after(before, dir)
  expect_true(all(after$stage == "after_run"))
  st <- stats::setNames(after$status, after$path)
  expect_equal(st[["changes.txt"]], "changed")
  expect_equal(st[["output/table.csv"]], "new")
  expect_equal(st[["goes.txt"]], "removed")
  expect_false("same.txt" %in% after$path)
  # a removed file keeps its hash from before the run
  expect_equal(after$sha256[after$path == "goes.txt"], before$sha256[before$path == "goes.txt"])
  expect_equal(after$sha256[after$path == "changes.txt"],
               unname(tools::sha256sum(file.path(dir, "changes.txt"))))
  # nothing listed before: every file is new
  all_new <- metacheck:::.repro_artifacts_after(NULL, dir)
  expect_true(all(all_new$status == "new"))
  # nothing changed: no rows
  expect_equal(nrow(metacheck:::.repro_artifacts_after(
    metacheck:::.repro_artifacts(dir, "before_run"), dir)), 0)
})

test_that(".repro_deposit_artifacts hashes the downloaded files with their address", {
  dir <- withr::local_tempdir()
  src <- file.path(dir, "cache_x.csv")
  writeLines("1,2", src)
  m <- data.frame(target_path = c("data/x.csv", "data/y.csv", "z.csv"),
                  source = c(src, file.path(dir, "gone.csv"), NA), ok = c(TRUE, TRUE, FALSE))
  sdf <- data.frame(file_name = "x.csv", file_location = src,
                    file_url = "https://osf.io/download/abc/", repo_url = "https://osf.io/xyz/")
  d <- metacheck:::.repro_deposit_artifacts(m, sdf)
  expect_equal(d$path, c("data/x.csv", "data/y.csv"))
  expect_equal(d$stage, c("deposit", "deposit"))
  expect_equal(d$sha256[1], unname(tools::sha256sum(src)))
  expect_true(is.na(d$sha256[2]))   # vanished before it was hashed
  expect_equal(d$source[1], "https://osf.io/download/abc/")
  # no file_url: the repository address
  sdf$file_url <- NA
  expect_equal(metacheck:::.repro_deposit_artifacts(m[1, ], sdf)$source, "https://osf.io/xyz/")
  expect_equal(nrow(metacheck:::.repro_deposit_artifacts(NULL)), 0)
  expect_equal(nrow(metacheck:::.repro_deposit_artifacts(m[0, ])), 0)
})

test_that(".repro_modification_hashes matches scripts by their path", {
  arts <- data.frame(stage = c("deposit", "before_run", "before_run"),
                     path = c("code/a.R", "code/a.R", "b.R"),
                     size = 1, sha256 = c("old", "new", "bnew"), mime_type = NA,
                     source = NA, status = NA)
  mods <- data.frame(file_name = c("a.R", "b.R", "c.R"), change_type = "setwd_removed",
                     detail = "")
  scripts <- data.frame(file_name = c("a.R", "b.R"), path = c("code/a.R", "b.R"))
  out <- metacheck:::.repro_modification_hashes(mods, scripts, arts)
  expect_equal(out$sha256_before, c("old", NA, NA))
  expect_equal(out$sha256_after, c("new", "bnew", NA))
  # no modifications: the columns are still there
  none <- metacheck:::.repro_modification_hashes(mods[0, ], scripts, arts)
  expect_true(all(c("sha256_before", "sha256_after") %in% names(none)))
})

test_that(".repro_prov_scripts gives paths relative to the run folder", {
  root <- withr::local_tempdir()
  rt <- data.frame(file_name = c("a.R", "b.R"),
                   script_path = c(file.path(root, "code", "a.R"), "/elsewhere/b.R"))
  s <- metacheck:::.repro_prov_scripts(rt, root)
  expect_equal(s$path, c("code/a.R", NA))
  expect_equal(nrow(metacheck:::.repro_prov_scripts(NULL, root)), 0)
})

test_that(".repro_environment describes the system without Docker", {
  metacheck:::.repro_prov_reset()
  env <- metacheck:::.repro_environment("process", Sys.time() - 5, Sys.time(),
                                        timeout = 600, total_timeout = 3600)
  expect_equal(env$sandbox, "process")
  expect_false(env$network_isolated)
  expect_false(env$internet)
  expect_null(env$docker)
  expect_equal(env$metacheck_version, as.character(utils::packageVersion("metacheck")))
  expect_match(env$started_at, "Z$")
  expect_true(env$started_at < env$ended_at)
  expect_equal(env$host$r_version, R.version.string)
  expect_silent(jsonlite::toJSON(env, auto_unbox = TRUE, null = "null"))
  # a clock time as total_timeout is written as a time
  env2 <- metacheck:::.repro_environment("process", Sys.time(), Sys.time(),
                                         total_timeout = as.POSIXct("2026-11-18 12:00:00", tz = "UTC"))
  expect_equal(env2$total_timeout, "2026-11-18T12:00:00.000Z")
})

test_that("docker runs are noted once per kind and image; isolation needs --network none", {
  metacheck:::.repro_prov_reset()
  args <- c("run", "--rm", "--name", "repro_x1", "--network", "none", "--read-only",
            "--cap-drop", "ALL", "img:1", "Rscript", "/sandbox/a.R")
  metacheck:::.repro_prov_note_docker("r_run", args, "img:1")
  metacheck:::.repro_prov_note_docker("r_run", sub("x1", "x2", args), "img:1")   # same kind: kept once
  metacheck:::.repro_prov_note_docker("r_install", c("run", "--name", "i", "img:1"), "img:1")
  d <- metacheck:::.repro_prov$docker
  expect_length(d, 2)
  expect_false("--name" %in% d[["r_run|img:1"]]$args)
  expect_false("repro_x1" %in% d[["r_run|img:1"]]$args)
  # no Docker here (or no such image): the image fields are NA, not an error
  local_mocked_bindings(.repro_docker_image_info = function(image) list(
    image = image, id = "sha256:abc", repo_digests = "img@sha256:def", created = NA,
    architecture = "amd64", r_version = "4.5.1", python_version = NA), .package = "metacheck")
  env <- metacheck:::.repro_environment("docker", Sys.time(), Sys.time())
  expect_true(env$network_isolated)
  expect_equal(env$docker$images[[1]]$id, "sha256:abc")
  expect_length(env$docker$runs, 2)
  # a run with the network on is not isolated
  metacheck:::.repro_prov_note_docker("python_run", c("run", "img:2", "python", "x.py"), "img:2")
  expect_false(metacheck:::.repro_environment("docker", Sys.time(), Sys.time())$network_isolated)
  # internet on: not isolated, whatever the arguments
  metacheck:::.repro_prov_reset()
  metacheck:::.repro_prov_note_docker("r_run", args, "img:1")
  expect_false(metacheck:::.repro_environment("docker", Sys.time(), Sys.time(), internet = TRUE)$network_isolated)
  # only an install run noted: no claim of isolation
  metacheck:::.repro_prov_reset()
  metacheck:::.repro_prov_note_docker("r_install", args, "img:1")
  expect_false(metacheck:::.repro_environment("docker", Sys.time(), Sys.time())$network_isolated)
  metacheck:::.repro_prov_reset()
})

test_that(".repro_docker_image_info gives NA fields when Docker cannot tell", {
  info <- metacheck:::.repro_docker_image_info("metacheck-no-such-image:0")
  expect_true(is.na(info$id))
  expect_length(info$repo_digests, 0)
  expect_true(is.na(metacheck:::.repro_docker_image_info(NA)$id))
})

test_that(".repro_prov_utf8 keeps other encodings' bytes as <xx>", {
  x <- c("plain.csv", rawToChar(as.raw(c(0x64, 0x82, 0x2e, 0x52))), NA)
  out <- metacheck:::.repro_prov_utf8(x)
  expect_equal(out[1], "plain.csv")
  expect_true(validUTF8(out[2]))
  expect_equal(out[2], "d<82>.R")
  expect_true(is.na(out[3]))
  expect_silent(jsonlite::toJSON(out))
})

test_that(".repro_prov_files leaves out metacheck's own files, keeps the authors'", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "statistical_output"))
  for (f in c("statistical_output/results_long.csv", "statistical_output/a.R.statistical_output.json",
              "statistical_output/authors_table.csv", ".capture.rds", ".repro_received",
              "install.R", "sub/.runner_x.R"))
    { dir.create(dirname(file.path(dir, f)), showWarnings = FALSE, recursive = TRUE); writeLines("x", file.path(dir, f)) }
  expect_setequal(metacheck:::.repro_prov_files(dir),
                  c("statistical_output/authors_table.csv", "install.R"))
})

# a deposit with data, an R script that reads it and writes a file (with a
# setwd() metacheck removes), and an R script that fails
provenance_deposit <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(d, "data")); dir.create(file.path(d, "code"))
  utils::write.csv(data.frame(g = rep(c("a", "b"), 10), y = c(1:10, 3:12)),
                   file.path(d, "data", "scores.csv"), row.names = FALSE)
  writeLines(c('setwd("C:/Users/someone/project")', 'x <- read.csv("data/scores.csv")',
               'write.csv(aggregate(y ~ g, x, mean), "means.csv", row.names = FALSE)'),
             file.path(d, "code", "analysis.R"))
  writeLines('stop("this script fails on purpose")', file.path(d, "fails.R"))
  d
}

test_that("reproducibility_check keeps run times, file hashes and the environment", {
  skip_if_quick()
  skip_if_not_installed("callr")
  d <- provenance_deposit()
  original <- unname(tools::sha256sum(file.path(d, "code", "analysis.R")))
  t0 <- Sys.time()
  mo <- suppressWarnings(suppressMessages(module_run(test_paper(), "reproducibility_check",
                                                     local_path = d, execute = TRUE, timeout = 60)))
  rr <- mo$run_results
  expect_true(all(c("started_at", "ended_at", "exit_status") %in% names(rr)))
  expect_equal(rr$exit_status[rr$file_name == "analysis.R"], 0L)
  expect_false(isTRUE(rr$exit_status[rr$file_name == "fails.R"] == 0L))
  st <- as.POSIXct(rr$started_at, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  en <- as.POSIXct(rr$ended_at, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  expect_true(all(st <= en) && all(st >= t0 - 1))

  a <- mo$artifacts
  dep <- a[a$stage == "deposit", ]
  expect_setequal(dep$path, c("data/scores.csv", "code/analysis.R", "fails.R"))
  expect_equal(dep$sha256[dep$path == "code/analysis.R"], original)
  bef <- a[a$stage == "before_run", ]
  expect_equal(bef$sha256[bef$path == "data/scores.csv"], dep$sha256[dep$path == "data/scores.csv"])
  expect_false(identical(bef$sha256[bef$path == "code/analysis.R"], original))   # setwd() removed
  aft <- a[a$stage == "after_run", ]
  expect_true(any(grepl("means.csv$", aft$path) & aft$status == "new"))
  expect_false(any(grepl("^statistical_output/", a$path)))

  sw <- mo$modifications[mo$modifications$change_type == "setwd_removed", ]
  expect_equal(sw$sha256_before[1], original)
  expect_equal(sw$sha256_after[1], bef$sha256[bef$path == "code/analysis.R"])

  e <- mo$environment
  expect_equal(e$sandbox, "process")
  expect_false(e$network_isolated)
  expect_equal(e$metacheck_version, as.character(utils::packageVersion("metacheck")))
  expect_silent(jsonlite::toJSON(list(a, e), auto_unbox = TRUE, null = "null", na = "null"))
})

test_that("without running code there is no provenance", {
  skip_if_quick()
  d <- provenance_deposit()
  mo <- suppressWarnings(suppressMessages(module_run(test_paper(), "reproducibility_check",
                                                     local_path = d, execute = FALSE)))
  expect_null(mo$artifacts)
  expect_null(mo$environment)
  expect_false("started_at" %in% names(mo$run_results %||% data.frame()))
})
