# The Docker execution backend for Python code, for
# reproducibility_check(execute = TRUE, sandbox = "docker") against a
# `language == "Python"` file. Same two-phase install-then-run design as
# reproducibility_check_docker.R's R backend (see that file's own header
# comment for the full rationale -- install phase has network + a
# bind-mounted throwaway library so pip can reach PyPI; run phase has
# --network none + --read-only so the script cannot reach the network or
# touch anything outside the mounted sandbox). The container-lifecycle
# helpers (.repro_docker_uid, .repro_docker_container_name(),
# .repro_docker_stop(), .repro_docker_resource_args(),
# .repro_docker_container_path()) are GENERIC -- not R-specific at all (they
# only ever touch docker/processx and host/container path strings) -- so
# this file reuses them from reproducibility_check_docker.R directly rather
# than duplicating them.
#
# The library is mounted at /pylib, not into the image's own site-packages:
# same reasoning as the R backend's /rlib (not /lib) choice -- an empty host
# directory bind-mounted over a path the base image's OWN packages already
# occupy would shadow them, not just add to them. /pylib is not a path any
# Python base image reserves. PYTHONPATH (not a `pip install --target`-less
# default search path) is how the run phase's `python` finds packages
# installed there, mirroring the R run phase's explicit .libPaths(c("/rlib",
# ...)) addition inside its capture-runner wrapper for the identical reason
# (there is no docker-level equivalent that makes a bind-mounted library
# findable on its own).

#' The Docker base image to use for a Python run
#'
#' Unlike R's [.repro_docker_image_for()] (which can match the paper's own
#' DECLARED R version via `rocker/r-ver:<version>`), there is no equivalent
#' Python version-pin detection built yet (`code_check`'s
#' `.code_version_pin_check()` is R-specific — an `renv.lock`/`sessionInfo()`
#' concept, with no Python counterpart reading a `runtime.txt`/`python_requires`
#' pin), so this always returns the one pre-built image.
#'
#' Defaults to the SMALL image (45 packages seen in >=2 files of a 486-file
#' corpus scan: numpy/pandas/scipy/matplotlib/seaborn/scikit-learn/
#' statsmodels and the rest of the common data-science/psychology-research
#' stack) rather than the large one — same "smaller, more conservative
#' default" choice the R side's README documents for
#' `metacheck_r_small`/`metacheck_r_large`. The large image
#' (`metacheck_py_large`, ~8.3GB) additionally bundles CPU-only torch and
#' tensorflow (deliberately excluded from the small image despite being
#' common in the scan — measured at ~700MB and ~450MB respectively, on top
#' of several hundred MB more of their own transitive dependencies) plus ~70
#' more less-common packages; a caller whose paper needs one of those passes
#' `docker_image = "ghcr.io/scienceverse/metacheck_py_large:latest"`
#' explicitly, or simply lets a paper needing an uncommon package install it
#' at run time against the small image instead (slower, needs network, same
#' tradeoff the R side's install phase already makes for an uncommon CRAN
#' package).
#'
#' psychopy was deliberately EXCLUDED from both images despite appearing in
#' 18 of 486 scanned files (tied with statsmodels for the most common
#' package after the core data-science set): it is a live-experiment
#' stimulus-presentation library (GUI display, audio/video playback, serial/
#' parallel port hardware I/O), and `reproducibility_check` only ever runs
#' ANALYSIS code against already-collected data — a paper's psychopy-based
#' experiment SCRIPT is not something this module runs, let alone
#' "reproduces" (it requires a live display/participant). Its own install
#' pulls in PyQt6 (261MB), ffmpeg/moviepy/pyglet (another ~250MB+), and
#' more — confirmed as a real, measured ~700MB+ cost for a package this
#' module's own execution path has no legitimate use for.
#'
#' @returns the pre-built Python sandbox image reference,
#'   `"ghcr.io/scienceverse/metacheck_py_small:latest"` (see
#'   https://github.com/scienceverse/metacheck_docker_reproducibility for the
#'   Dockerfile/package list/build instructions — the same repo that builds
#'   the R images)
#' @keywords internal
.repro_docker_default_image_py <- "ghcr.io/scienceverse/metacheck_py_small:latest"

#' Install Python dependencies inside a throwaway Docker container
#'
#' The Docker-backed twin of [repro_install_deps_py()]: same return contract,
#' same throwaway-library semantics, but the actual `pip install` runs INSIDE
#' a container (default networking, since PyPI must be reachable), writing
#' into `lib_dir` via a bind mount — so the run phase
#' ([repro_run_scripts_py_docker()]), which mounts the same `lib_dir` with the
#' network off, finds the packages already there.
#'
#' @param install_deps a data frame as produced inside `reproducibility_check()`
#'   (`package`, `source`, `ref`, `base`) — the non-base rows of
#'   [repro_dependencies_py()]'s output
#' @param lib_dir throwaway `pip install --target` directory ON THE HOST
#'   (created if absent); bind-mounted into the container at `/pylib`
#' @param image the Docker image to install into (see
#'   [.repro_docker_default_image_py])
#' @param timeout timeout in seconds for the whole install container run
#'   (default 600)
#'
#' @returns a data frame with `package`, `source`, `installed` (logical),
#'   `message` (pip's stderr on failure, else ""), `via_archive` (always
#'   FALSE — pip has no CRAN-Archive-style fallback), and `category` (always
#'   NA) — same columns as [repro_install_deps_py()] so callers do not need
#'   to branch on which backend ran.
#' @export
repro_install_deps_py_docker <- function(install_deps, lib_dir,
                                         image = .repro_docker_default_image_py,
                                         timeout = 600) {
  empty <- data.frame(package = character(0), source = character(0),
                      installed = logical(0), message = character(0),
                      via_archive = logical(0), category = character(0))
  if (is.null(install_deps) || nrow(install_deps) == 0) return(empty)
  if (!requireNamespace("processx", quietly = TRUE))
    stop("the 'processx' package is required for sandbox = \"docker\".", call. = FALSE)
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)

  # One pip invocation covering the whole batch (not one docker run per
  # package), same reasoning as the R backend's single install.R script: a
  # container start's own overhead is paid once, and `pip install` already
  # reports per-package success/failure in its own output if one line fails
  # -- but since `pip install a b c` aborts the WHOLE command on the first
  # failing requirement (unlike install.packages(), which tries every
  # package and reports per-package), each requirement is installed with
  # its OWN pip invocation inside the one container/shell session, so one
  # bad requirement does not block every other package in the batch.
  reqs <- ifelse(!is.na(install_deps$ref) & nzchar(install_deps$ref %||% ""),
                 install_deps$ref, install_deps$package)
  sandbox_dir <- tempfile("repro_docker_install_py_")
  dir.create(sandbox_dir, recursive = TRUE)
  on.exit(unlink(sandbox_dir, recursive = TRUE), add = TRUE)

  results_path <- file.path(sandbox_dir, ".install_results.jsonl")
  # A tiny shell driver, not a Python script: pip itself is the thing being
  # invoked per requirement, and a results line is appended after each one
  # (same incremental-write reasoning as the R backend's per-package
  # saveRDS() -- a batch that times out partway through still reports every
  # package that finished before the cutoff). JSON Lines (one compact JSON
  # object per line) rather than a single JSON array, specifically so a
  # truncated file (the container killed mid-write) still parses every
  # COMPLETE line that was flushed before the cutoff -- a single top-level
  # array left unclosed by a mid-write kill would fail to parse at all.
  driver_lines <- c(
    "#!/bin/sh",
    "set -u",
    paste0('OUT="', .repro_docker_container_path(results_path, sandbox_dir), '"'),
    ': > "$OUT"',
    unlist(lapply(seq_along(reqs), function(i) {
      pkg <- install_deps$package[i]; req <- reqs[i]; src <- install_deps$source[i]
      c(sprintf('if pip install --quiet --target /pylib %s 2> /tmp/err_%d.txt; then',
               shQuote(req), i),
        sprintf('  printf \'{"package":"%s","source":"%s","installed":true,"message":""}\\n\' >> "$OUT"',
               pkg, src),
        'else',
        sprintf('  ERR=$(tr -d \'\\n\' < /tmp/err_%d.txt | sed \'s/"/\\\\"/g\')', i),
        sprintf('  printf \'{"package":"%s","source":"%s","installed":false,"message":"%%s"}\\n\' "$ERR" >> "$OUT"',
               pkg, src),
        'fi')
    }))
  )
  driver_path <- file.path(sandbox_dir, "install.sh")
  con <- file(driver_path, open = "wb", encoding = "UTF-8")
  writeLines(driver_lines, con, useBytes = TRUE)
  close(con)
  Sys.chmod(driver_path, "0755")
  container_driver <- .repro_docker_container_path(driver_path, sandbox_dir)

  container_name <- .repro_docker_container_name()
  args <- c("run", "--rm", "--name", container_name,
           "--user", .repro_docker_uid,
           "--cap-drop", "ALL",
           "--security-opt", "no-new-privileges",
           "--pids-limit", "512",
           .repro_docker_resource_args(),
           "-v", paste0(sandbox_dir, ":/sandbox"),
           "-v", paste0(normalizePath(lib_dir, mustWork = FALSE), ":/pylib"),
           image, "sh", container_driver)

  out_file <- tempfile(fileext = ".out")
  res <- tryCatch(
    processx::run("docker", args, error_on_status = FALSE, timeout = timeout,
                 stdout = out_file, stderr = out_file),
    error = function(e) NULL)

  # Same timeout-leaves-the-container-running risk the R backend's own
  # comment documents (GitHub issue #417) -- processx's timeout only kills
  # the host-side `docker run` CLI, never the container itself.
  if (is.null(res) || isTRUE(res$timeout)) .repro_docker_stop(container_name)

  parsed <- if (file.exists(results_path)) {
    ln <- tryCatch(readLines(results_path, warn = FALSE), error = function(e) character(0))
    ln <- ln[nzchar(trimws(ln))]
    if (length(ln) == 0) NULL else
      dplyr::bind_rows(lapply(ln, function(l) {
        tryCatch(as.data.frame(jsonlite::fromJSON(l)), error = function(e) NULL)
      }))
  } else NULL

  msg <- if (!is.null(res)) {
    txt <- tryCatch(paste(readLines(out_file, warn = FALSE), collapse = "\n"),
                    error = function(e) "")
    if (isTRUE(res$timeout)) paste0("docker install timed out after ", timeout, "s")
    else paste0("docker install failed (status ", res$status, "): ", txt)
  } else "docker run could not be started"
  unlink(out_file)

  missing_pkgs <- setdiff(install_deps$package, parsed$package %||% character(0))
  padding <- if (length(missing_pkgs) > 0) {
    src_of <- stats::setNames(install_deps$source, install_deps$package)
    data.frame(package = missing_pkgs, source = unname(src_of[missing_pkgs]),
              installed = FALSE, message = msg)
  } else NULL

  out <- dplyr::bind_rows(parsed, padding)
  out$via_archive <- FALSE
  out$category <- NA_character_
  out
}

#' Run Python scripts, in order, each in an isolated Docker container
#'
#' The Docker-backed twin of [repro_run_scripts_py()]: same return contract,
#' same per-script semantics (skip on missing inputs / parse failure,
#' timeout, error classification), but each script runs via `docker run
#' --network none --read-only --user <non-root>` instead of a `processx`
#' subprocess on the host — so the code cannot reach the network or touch
#' anything outside the mounted sandbox and library, even deliberately.
#'
#' @param run_tbl `repro_write_scripts()` output (`file_name`, `script_path`,
#'   `run_dir`) — `run_dir` must be the SAME directory as (or a subdirectory
#'   of) `sandbox_root`
#' @param order a vector of `file_name`s in the order to run
#' @param sandbox_root the materialised layout root on the HOST — mounted
#'   read-write at `/sandbox`
#' @param lib_dir throwaway `pip install --target` library ON THE HOST (or
#'   NULL) — mounted read-only at `/pylib` when supplied
#' @param image the Docker image to run scripts in (see
#'   [.repro_docker_default_image_py])
#' @param timeout per-script timeout in seconds
#' @param skip character vector of `file_name`s to record as
#'   `skipped_missing_inputs` instead of running
#' @param parses named logical (by file_name); a file that will not parse is
#'   recorded `not_parsed` and not run
#' @param failed_deps character vector of package names
#'   [repro_install_deps_py_docker()] could not install
#'
#' @returns a data frame — identical columns to [repro_run_scripts_py()]'s
#'   return: `file_name`, `outcome`, `error`, `error_type`, `undefined_var`,
#'   `stdout`, `stderr`, `elapsed`, `script_lines`, `captures` (always `NULL`)
#' @export
repro_run_scripts_py_docker <- function(run_tbl, order, sandbox_root, lib_dir = NULL,
                                        image = .repro_docker_default_image_py,
                                        timeout = 600, skip = character(0),
                                        parses = NULL, failed_deps = character(0)) {
  if (is.null(run_tbl) || nrow(run_tbl) == 0)
    return(data.frame(file_name = character(0), outcome = character(0),
                      error = character(0), error_type = character(0),
                      undefined_var = character(0), stdout = character(0),
                      stderr = character(0), elapsed = numeric(0)) |>
             dplyr::mutate(script_lines = list(), captures = list()))
  if (!requireNamespace("processx", quietly = TRUE))
    stop("the 'processx' package is required for sandbox = \"docker\".", call. = FALSE)

  ordered_names <- c(order[order %in% run_tbl$file_name],
                     setdiff(run_tbl$file_name, order))

  sandbox_root <- normalizePath(sandbox_root, mustWork = TRUE)
  lib_dir_norm <- if (!is.null(lib_dir) && dir.exists(lib_dir))
    normalizePath(lib_dir, mustWork = TRUE) else NULL

  pb_run <- pb(length(ordered_names), ":what [:bar] :current/:total")
  pb_run$tick(0, list(what = ""))
  on.exit(pb_run$terminate())

  rows <- lapply(ordered_names, function(fn) {
    pb_run$tick(1, list(what = fn))
    row <- run_tbl[run_tbl$file_name == fn, ][1, ]

    no_lines <- function(df) dplyr::mutate(df, script_lines = list(character(0)),
                                           captures = list(NULL))
    if (!is.null(parses) && fn %in% names(parses) && !isTRUE(parses[[fn]]))
      return(no_lines(data.frame(file_name = fn, outcome = "not_parsed", error = "",
                        error_type = NA_character_, undefined_var = NA_character_,
                        stdout = "", stderr = "", elapsed = 0)))
    if (fn %in% skip)
      return(no_lines(data.frame(file_name = fn, outcome = "skipped_missing_inputs",
                        error = "", error_type = NA_character_,
                        undefined_var = NA_character_,
                        stdout = "", stderr = "", elapsed = 0)))

    exec_lines <- tryCatch(readLines(row$script_path, warn = FALSE),
                           error = function(e) character(0))
    message("[repro/py-docker]   -> running '", fn, "' (timeout ", timeout, "s) ...")

    container_script <- .repro_docker_container_path(row$script_path, sandbox_root)
    container_name <- .repro_docker_container_name()
    # PYTHONPATH is set via -e, not baked into the image: the mounted
    # /pylib is only populated once repro_install_deps_py_docker() has run,
    # so this must be an environment variable the container sees at RUN
    # time, not something the (shared, reused-across-papers) image itself
    # hardcodes.
    args <- c("run", "--rm", "--name", container_name,
             "--network", "none", "--read-only",
             "--tmpfs", "/tmp",
             "--user", .repro_docker_uid,
             "--cap-drop", "ALL",
             "--security-opt", "no-new-privileges",
             "--pids-limit", "512",
             .repro_docker_resource_args(),
             "-w", "/sandbox",
             "-v", paste0(sandbox_root, ":/sandbox"))
    if (!is.null(lib_dir_norm))
      args <- c(args, "-v", paste0(lib_dir_norm, ":/pylib:ro"),
               "-e", "PYTHONPATH=/pylib")
    args <- c(args, image, "python", container_script)

    out_file <- tempfile(fileext = ".out")
    err_file <- tempfile(fileext = ".err")
    t0 <- Sys.time()
    res <- tryCatch(
      processx::run("docker", args, error_on_status = FALSE, timeout = timeout,
                    stdout = out_file, stderr = err_file),
      error = function(e) e)
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    is_timeout <- (inherits(res, "condition") &&
      grepl("timed? ?out", conditionMessage(res), ignore.case = TRUE)) ||
      (is.list(res) && isTRUE(res$timeout))
    if (is_timeout) .repro_docker_stop(container_name)

    read_cap <- function(f) if (file.exists(f))
      paste(readLines(f, warn = FALSE), collapse = "\n") else ""
    so <- read_cap(out_file); se <- read_cap(err_file)
    unlink(c(out_file, err_file))

    message("[repro/py-docker]   <- '", fn, "' done in ", round(elapsed, 1), "s")

    if (inherits(res, "condition")) {
      msg <- conditionMessage(res)
      etype <- if (is_timeout) "timeout" else "runtime"
      outc <- if (is_timeout) "timed_out" else "errored"
      return(data.frame(file_name = fn, outcome = outc, error = msg,
                        error_type = etype, undefined_var = NA_character_,
                        stdout = so, stderr = se, elapsed = elapsed) |>
               dplyr::mutate(script_lines = list(exec_lines), captures = list(NULL)))
    }
    if (isTRUE(res$timeout)) {
      return(data.frame(file_name = fn, outcome = "timed_out",
                        error = paste0("timed out after ", timeout, "s"),
                        error_type = "timeout", undefined_var = NA_character_,
                        stdout = so, stderr = se, elapsed = elapsed) |>
               dplyr::mutate(script_lines = list(exec_lines), captures = list(NULL)))
    }
    if (res$status != 0) {
      err_lines <- strsplit(se, "\n", fixed = TRUE)[[1]]
      err_lines <- err_lines[nzchar(trimws(err_lines))]
      last_line <- if (length(err_lines)) err_lines[length(err_lines)] else se
      error_type <- sub("^([A-Za-z_][A-Za-z0-9_.]*Error)\\s*:.*$", "\\1",
                        last_line, perl = TRUE)
      if (identical(error_type, last_line)) error_type <- NA_character_
      undefined_var <- if (!is.na(error_type) && error_type == "NameError")
        sub(".*name ['\"]([^'\"]+)['\"] is not defined.*", "\\1", last_line,
           perl = TRUE) else NA_character_
      if (identical(undefined_var, last_line)) undefined_var <- NA_character_

      nomod_pat <- "No module named ['\"]([^'\"]+)['\"]"
      nomod_src <- if (grepl(nomod_pat, se)) se else NA_character_
      nomod_var <- if (!is.na(nomod_src))
        sub(paste0(".*", nomod_pat, ".*"), "\\1",
            regmatches(nomod_src, regexpr(nomod_pat, nomod_src))) else NA_character_
      dep_unavailable <- !is.na(nomod_var) && nomod_var %in% failed_deps

      etype <- if (dep_unavailable) "dependency_unavailable"
               else if (!is.na(undefined_var)) "undefined_variable"
               else error_type %||% "runtime"
      outc <- if (dep_unavailable) "dependency_unavailable" else "errored"
      return(data.frame(file_name = fn, outcome = outc, error = last_line,
                        error_type = etype, undefined_var = undefined_var,
                        stdout = so, stderr = se, elapsed = elapsed) |>
               dplyr::mutate(script_lines = list(exec_lines), captures = list(NULL)))
    }
    data.frame(file_name = fn, outcome = "ran_ok", error = "",
              error_type = NA_character_, undefined_var = NA_character_,
              stdout = so, stderr = se, elapsed = elapsed) |>
      dplyr::mutate(script_lines = list(exec_lines), captures = list(NULL))
  })
  dplyr::bind_rows(rows)
}
