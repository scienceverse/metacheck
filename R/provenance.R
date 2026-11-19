# Provenance of a reproducibility check (metacheck #404)
#
# What a TRACE Research Object (TRO) declaration needs, kept in the output of
# reproducibility_check() so that one can be written later from a stored
# result, without running the code again:
#
# - per script run (run_results): when it started and ended (started_at,
#   ended_at, UTC) and the exit status of the process (exit_status);
# - per file (artifacts): its SHA-256 hash, size and type at three moments
#   (deposit = as downloaded from the repository; before_run = the run
#   folder just before the first script ran, after metacheck's own changes;
#   after_run = the files the scripts made, changed or removed);
# - per change metacheck made to a script (modifications): the script's hash
#   before and after the change;
# - the system that ran the code (environment): metacheck's version, the
#   sandbox, the Docker image (its ID and digest), the R and Python versions,
#   the docker run arguments (network, read-only file system, capabilities),
#   the time limits and whether the internet was on.
#
# A TRO maps these onto trov:ResearchArtifact (hash, type), trov:hasArrangement
# (the files at each moment), trov:TransparentResearchPerformance (the runs,
# with their times and the arrangements they read and wrote) and
# trov:TransparentResearchSystem (the environment, whose docker arguments back
# capabilities such as network isolation). See
# https://transparency-certified.github.io/trace-specification/

#' SHA-256 hashes of files
#'
#' Streams each file (R's `tools::sha256sum()` from R 4.5, else the digest
#' package), so a large file is never read into memory whole.
#'
#' @param path file paths
#' @returns a character vector as long as `path`: 64 lowercase hex characters,
#'   or `NA` for a path that is missing, a folder, or cannot be read, and
#'   when neither R >= 4.5 nor the digest package is available
#' @noRd
.sha256_file <- function(path) {
  path <- as.character(path)
  out <- rep(NA_character_, length(path))
  ok <- !is.na(path) & file.exists(path) & !dir.exists(path)
  if (!any(ok)) return(out)
  tools_ns <- asNamespace("tools")
  if (exists("sha256sum", envir = tools_ns, inherits = FALSE)) {
    h <- tryCatch(suppressWarnings(unname(get("sha256sum", envir = tools_ns)(path[ok]))),
                  error = function(e) NULL)
    # one unreadable file must not lose the others' hashes
    if (is.null(h)) h <- vapply(path[ok], function(p) tryCatch(
      suppressWarnings(unname(get("sha256sum", envir = tools_ns)(p))),
      error = function(e) NA_character_), character(1), USE.NAMES = FALSE)
    out[ok] <- h
  } else if (requireNamespace("digest", quietly = TRUE)) {
    out[ok] <- vapply(path[ok], function(p) tryCatch(
      digest::digest(p, algo = "sha256", file = TRUE),
      error = function(e) NA_character_), character(1), USE.NAMES = FALSE)
  }
  out
}

# A time as ISO 8601 in UTC with milliseconds ("2026-11-18T21:05:03.123Z");
# NA for a missing time.
.repro_iso_utc <- function(t) {
  if (is.null(t) || !length(t)) return(NA_character_)
  t <- as.POSIXct(t)
  out <- format(t, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  out[is.na(t)] <- NA_character_
  out
}

# Adds started_at, ended_at and exit_status to rows of run_results. `clock` is
# the environment a script runner fills while a script runs (started, ended,
# status); without it, or for a script that did not run (skipped, not parsed),
# the columns are NA, so rows of every kind and of every pass bind together.
.repro_add_run_clock <- function(df, clock = NULL) {
  if (is.null(df)) return(df)
  n <- nrow(df)
  started <- if (!is.null(clock)) clock$started else NULL
  ended <- if (!is.null(clock)) clock$ended else NULL
  status <- if (!is.null(clock) && length(clock$status)) clock$status else NA_integer_
  if (!"started_at" %in% names(df))
    df$started_at <- rep(if (is.null(started)) NA_character_ else .repro_iso_utc(started), n)
  if (!"ended_at" %in% names(df))
    df$ended_at <- rep(if (is.null(ended)) NA_character_ else .repro_iso_utc(ended), n)
  if (!"exit_status" %in% names(df))
    df$exit_status <- rep(suppressWarnings(as.integer(status))[1], n)
  df
}

# The exit status of a finished process run: processx's list ($status), a
# callr error that carries one ($status), 0 for a callr result; NA when the
# process did not report one (docker itself failing) or was stopped at its time
# limit, where processx gives the status of the stopped `docker` command (2),
# not the script's.
.repro_exit_status <- function(res) {
  if (is.null(res)) return(NA_integer_)
  if (is.list(res) && isTRUE(res$timeout)) return(NA_integer_)
  if (inherits(res, "condition")) {
    s <- tryCatch(res$status, error = function(e) NULL)
    return(if (is.numeric(s) && length(s) == 1) as.integer(s) else NA_integer_)
  }
  if (is.list(res) && is.numeric(res$status) && length(res$status) == 1) return(as.integer(res$status))
  NA_integer_
}

# The files in a run folder, relative to it, with forward slashes; hidden files
# included. Left out, as they are neither the authors' files nor made by their
# code: the package libraries metacheck installs there (see
# .repro_sandbox_names()), metacheck's own short-lived files (a script's
# runner, its captured results, what a container received over the network)
# and the statistical output metacheck reads from the runs and writes there
# (stat_output_write(): statistical_output/results_long.csv and
# <script>.statistical_output.json). The packages are installed in another
# folder, so an install.R of the authors' is kept.
.repro_prov_files <- function(root) {
  if (is.null(root) || !dir.exists(root)) return(character(0))
  f <- gsub("\\\\", "/", list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  f[!grepl("^(_r?lib|_pylib2?)/", f) &
      !grepl("(^|/)\\.runner_[^/]*$", f) &
      !f %in% c(".capture.rds", ".repro_received") &
      !grepl("^statistical_output/(results_long\\.csv|[^/]+\\.statistical_output\\.json)$", f)]
}

# A path as valid UTF-8 text: a name in another encoding (an old zip's CP437
# name, a Latin-1 name) keeps its bytes as <xx>, so the result can be written as
# JSON and the original bytes can still be read from it.
.repro_prov_utf8 <- function(x) {
  x <- as.character(x)
  bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) x[bad] <- iconv(x[bad], "UTF-8", "UTF-8", sub = "byte")
  x
}

# The MIME type of a file, by its extension, from metacheck's registry of file
# types (.ext_registry, R/data_check_helpers.R); NA when it has none.
.repro_file_mime <- function(path) {
  ext <- tolower(tools::file_ext(path))
  .ext_registry$mime[match(ext, .ext_registry$ext)]
}

.repro_artifact_cols <- function() {
  data.frame(stage = character(0), path = character(0), size = numeric(0),
             sha256 = character(0), mime_type = character(0), source = character(0),
             status = character(0), stringsAsFactors = FALSE)
}

# One row per file in a run folder at one moment: stage, path (relative to the
# folder), size (bytes), sha256, mime_type, source (NA here) and status (NA
# here; see .repro_artifacts_after()). A file that vanishes while it is
# listed gets NA for size and hash.
.repro_artifacts <- function(root, stage) {
  f <- .repro_prov_files(root)
  if (!length(f)) return(.repro_artifact_cols())
  full <- file.path(root, f)
  data.frame(stage = stage, path = .repro_prov_utf8(f),
             size = suppressWarnings(file.size(full)),
             sha256 = .sha256_file(full), mime_type = .repro_file_mime(f),
             source = NA_character_, status = NA_character_, stringsAsFactors = FALSE)
}

# The files as downloaded from the repository (stage "deposit"): one row per
# file metacheck copied into the run folder (repro_materialize_layout()'s
# "materialised" attribute: target_path, source, ok), hashed from the
# downloaded file itself, so a later change by metacheck or by the code shows
# as a different hash in the next stage. source: the file's address in the
# repository (file_url, else repo_url, from structure_df), else NA.
.repro_deposit_artifacts <- function(materialised, structure_df = NULL) {
  if (is.null(materialised) || !is.data.frame(materialised) || !nrow(materialised) ||
      !all(c("target_path", "source") %in% names(materialised)))
    return(.repro_artifact_cols())
  m <- materialised[!is.na(materialised$source) & nzchar(materialised$source), , drop = FALSE]
  if ("ok" %in% names(m)) m <- m[m$ok %in% TRUE, , drop = FALSE]
  if (!nrow(m)) return(.repro_artifact_cols())
  address <- rep(NA_character_, nrow(m))
  if (is.data.frame(structure_df) && "file_location" %in% names(structure_df)) {
    url <- if ("file_url" %in% names(structure_df)) as.character(structure_df$file_url) else rep(NA_character_, nrow(structure_df))
    repo <- if ("repo_url" %in% names(structure_df)) as.character(structure_df$repo_url) else rep(NA_character_, nrow(structure_df))
    url[is.na(url) | !nzchar(url)] <- repo[is.na(url) | !nzchar(url)]
    hit <- match(m$source, structure_df$file_location)
    address <- url[hit]
  }
  path <- sub("^(\\./|/)+", "", gsub("\\\\", "/", m$target_path))
  data.frame(stage = "deposit", path = .repro_prov_utf8(path),
             size = suppressWarnings(file.size(m$source)),
             sha256 = .sha256_file(m$source), mime_type = .repro_file_mime(path),
             source = address, status = NA_character_, stringsAsFactors = FALSE)
}

# The run folder after the scripts ran (stage "after_run"), compared with the
# rows of the "before_run" stage: only the files that are new, changed (another
# hash) or removed are listed, with that status, since an unchanged file is
# the one already listed before the run. A removed file keeps its before-run
# hash and size.
.repro_artifacts_after <- function(before, root) {
  now <- .repro_artifacts(root, "after_run")
  if (is.null(before) || !nrow(before)) {
    if (nrow(now)) now$status <- "new"
    return(now)
  }
  old <- before$sha256[match(now$path, before$path)]
  now$status <- ifelse(!now$path %in% before$path, "new",
                       ifelse(is.na(old) | is.na(now$sha256) | old != now$sha256, "changed", "unchanged"))
  gone <- before[!before$path %in% now$path, , drop = FALSE]
  if (nrow(gone)) {
    gone$stage <- "after_run"
    gone$status <- "removed"
  }
  rbind(now[now$status != "unchanged", , drop = FALSE], gone)
}

# ── The system that ran the code (environment) ───────────────────────────────
# The docker run arguments and images actually used are noted by the script
# runners and installers while they run (.repro_prov_note_docker()), once per
# kind of run, and read back by .repro_environment(). Reset at the start of
# each check (.repro_prov_reset()); a batch of papers runs each paper in its
# own R process (see .reproducibility_check_batch()).
.repro_prov <- new.env(parent = emptyenv())

.repro_prov_reset <- function() {
  rm(list = ls(.repro_prov, all.names = TRUE), envir = .repro_prov)
  .repro_prov$docker <- list()
  invisible(NULL)
}

# kind: "r_run", "r_install", "python_run", "python_install"; noted once per
# kind and image (Python 2 scripts run in another image than Python 3 ones).
# The container's random name is left out; the rest is kept as given.
.repro_prov_note_docker <- function(kind, args, image) {
  if (!exists("docker", envir = .repro_prov, inherits = FALSE)) .repro_prov$docker <- list()
  image <- as.character(image)[1]
  key <- paste0(kind, "|", image)
  if (!is.null(.repro_prov$docker[[key]])) return(invisible(NULL))
  args <- as.character(args)
  i <- which(args == "--name")
  if (length(i)) args <- args[-c(i[1], i[1] + 1L)]
  .repro_prov$docker[[key]] <- list(kind = kind, image = image, args = args)
  invisible(NULL)
}

# What `docker image inspect` says about an image: its ID, digests (as pulled
# from a registry), creation time, architecture, and the R and Python versions
# its environment declares (R_VERSION in the rocker images and those built on
# them, PYTHON_VERSION in the official Python images), read without starting a
# container. NULL fields when Docker cannot tell.
.repro_docker_image_info <- function(image) {
  # repo_digests is a list, so it is a JSON array also with one digest
  info <- list(image = image, id = NA_character_, repo_digests = list(),
               created = NA_character_, architecture = NA_character_,
               r_version = NA_character_, python_version = NA_character_)
  if (is.null(image) || is.na(image) || !nzchar(image) || !requireNamespace("processx", quietly = TRUE))
    return(info)
  res <- tryCatch(processx::run("docker", c("image", "inspect", image),
                                error_on_status = FALSE, timeout = 30),
                  error = function(e) NULL)
  if (is.null(res) || !identical(res$status, 0L)) return(info)
  j <- tryCatch(jsonlite::fromJSON(res$stdout, simplifyVector = FALSE)[[1]], error = function(e) NULL)
  if (is.null(j)) return(info)
  env <- unlist(j$Config$Env %||% list())
  env_of <- function(name) {
    hit <- grep(paste0("^", name, "="), env, value = TRUE)
    if (length(hit)) sub(paste0("^", name, "="), "", hit[1]) else NA_character_
  }
  info$id <- j$Id %||% NA_character_
  info$repo_digests <- as.list(as.character(unlist(j$RepoDigests %||% list())))
  info$created <- j$Created %||% NA_character_
  info$architecture <- j$Architecture %||% NA_character_
  info$r_version <- env_of("R_VERSION")
  info$python_version <- env_of("PYTHON_VERSION")
  info
}

# The version of a Python executable on this computer ("Python 3.12.4"); NA
# when it cannot be run.
.repro_python_version <- function(bin) {
  if (is.null(bin) || !nzchar(bin) || !requireNamespace("processx", quietly = TRUE)) return(NA_character_)
  res <- tryCatch(processx::run(bin, "--version", error_on_status = FALSE, timeout = 20),
                  error = function(e) NULL)
  if (is.null(res)) return(NA_character_)
  v <- trimws(paste(res$stdout, res$stderr))
  if (nzchar(v)) v else NA_character_
}

# The environment element of reproducibility_check()'s output. sandbox:
# "docker" or "process"; internet: whether the code could use the internet;
# timeout: seconds per script; total_timeout: seconds for all scripts, or a
# clock time; python_bin: the Python run outside Docker, when Python ran there.
.repro_environment <- function(sandbox, started, ended, timeout = NA, total_timeout = NA,
                               internet = FALSE, python_bin = NULL) {
  docker <- if (exists("docker", envir = .repro_prov, inherits = FALSE)) .repro_prov$docker else list()
  images <- unique(unlist(lapply(docker, function(d) d$image)))
  images <- images[!is.na(images) & nzchar(images)]
  inspected <- stats::setNames(lapply(images, .repro_docker_image_info), images)
  runs <- docker[vapply(docker, function(d) grepl("_run$", d$kind %||% ""), logical(1))]
  isolated <- identical(sandbox, "docker") && !isTRUE(internet) && length(runs) > 0 &&
    all(vapply(runs, function(d)
      any(d$args == "--network") && "none" %in% d$args[which(d$args == "--network") + 1L], logical(1)))
  tl <- if (inherits(total_timeout, "POSIXt")) .repro_iso_utc(total_timeout) else total_timeout
  list(
    metacheck_version = tryCatch(as.character(utils::packageVersion("metacheck")),
                                 error = function(e) NA_character_),
    started_at = .repro_iso_utc(started),
    ended_at = .repro_iso_utc(ended),
    host = list(r_version = R.version.string, platform = R.version$platform,
                os = paste(Sys.info()[["sysname"]], Sys.info()[["release"]])),
    sandbox = sandbox,
    # whether the code was run with the network switched off by the sandbox
    # itself (docker's --network none); the process sandbox does not stop it
    network_isolated = isTRUE(isolated),
    internet = isTRUE(internet),
    timeout_per_script = timeout,
    total_timeout = tl,
    python_local = if (!is.null(python_bin) && nzchar(python_bin))
      list(executable = python_bin, version = .repro_python_version(python_bin)) else NULL,
    docker = if (length(docker)) list(runs = unname(docker), images = unname(inspected)) else NULL
  )
}

# Adds sha256_before and sha256_after to the modifications table: the hash of
# the script as downloaded (stage "deposit") and as it ran (stage
# "before_run"), matched by the script's path in the run folder (scripts:
# file_name and path). NA where a stage has no file at that path, e.g. a
# script made from an R Markdown file.
.repro_modification_hashes <- function(modifications, scripts, artifacts) {
  if (is.null(modifications) || !is.data.frame(modifications)) return(modifications)
  modifications$sha256_before <- rep(NA_character_, nrow(modifications))
  modifications$sha256_after <- rep(NA_character_, nrow(modifications))
  if (!nrow(modifications) || is.null(scripts) || !nrow(scripts) ||
      is.null(artifacts) || !nrow(artifacts)) return(modifications)
  path <- scripts$path[match(modifications$file_name, scripts$file_name)]
  dep <- artifacts[artifacts$stage == "deposit", , drop = FALSE]
  bef <- artifacts[artifacts$stage == "before_run", , drop = FALSE]
  modifications$sha256_before <- dep$sha256[match(path, dep$path)]
  modifications$sha256_after <- bef$sha256[match(path, bef$path)]
  modifications
}

# The scripts of a run table as file_name and path relative to the run folder.
.repro_prov_scripts <- function(run_tbl, root) {
  if (is.null(run_tbl) || !nrow(run_tbl) || !"script_path" %in% names(run_tbl) || is.null(root))
    return(data.frame(file_name = character(0), path = character(0)))
  r <- gsub("\\\\", "/", normalizePath(root, mustWork = FALSE))
  p <- gsub("\\\\", "/", normalizePath(run_tbl$script_path, mustWork = FALSE))
  rel <- ifelse(startsWith(p, paste0(r, "/")), substring(p, nchar(r) + 2L), NA_character_)
  data.frame(file_name = run_tbl$file_name, path = .repro_prov_utf8(rel), stringsAsFactors = FALSE)
}
