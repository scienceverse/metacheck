# The Docker execution backend for reproducibility_check(execute = TRUE,
# sandbox = "docker"). Same idea as repro_install_deps()/repro_run_scripts()
# (R/reproducibility_check.R) -- install the paper's declared dependencies,
# then run each script in order -- but INSIDE a container instead of a
# callr subprocess on the host, so the code cannot touch the host filesystem
# or network. See those functions' own roxygen for what callr isolates (a
# crash, not the filesystem/network) and why that is not enough for running
# untrusted downloaded code; this backend is the actual sandbox.
#
# Two-phase container use, because a single always-network-off container
# cannot install.packages() from CRAN:
#   1. INSTALL phase: `docker run` with default networking (so CRAN/GitHub
#      are reachable) but still non-root + dropped capabilities (a package's
#      configure/.onLoad code is as much "running someone else's code" as the
#      run phase is), writing into a library on a MOUNTED HOST DIRECTORY so
#      it survives into phase 2 (the container itself is --rm'd after each
#      run).
#   2. RUN phase: `docker run --network none --read-only --user <non-root>`
#      per script, with the materialised sandbox and the (already-populated)
#      library both mounted, so the script can read/write only inside the
#      sandbox and cannot reach the network at all.
#
# The library is mounted at /rlib, NOT /lib: an empty host directory bind-
# mounted over the container's own /lib shadows its real shared libraries
# (ld.so, libc, ...), so even `Rscript` itself fails to start with a
# misleading "exec .../Rscript: no such file or directory" -- confirmed
# against a real Docker Desktop run before this was caught. /rlib is not a
# path any Linux base image reserves.
#
# Both phases run as a fixed non-root uid/gid (1000:1000, arbitrary but
# consistent across every run in this package): the image itself is root by
# default (confirmed against rocker/r-ver:latest), and a container escape as
# root is a strictly worse outcome than as an unprivileged uid.

#' Is Docker available and running?
#'
#' Checks `docker info` succeeds, so a caller can fail early and clearly
#' (before materialising anything) rather than partway through a batch of
#' papers with a cryptic `docker run` error. Used as the guard for
#' `sandbox = "docker"` the same way `requireNamespace("callr", ...)` guards
#' the process backend.
#'
#' @returns a list `ok` (logical) and `msg` (character, empty on success —
#'   otherwise a human-readable reason, e.g. Docker not installed or the
#'   daemon not running)
#' @export
repro_docker_available <- function() {
  bin <- tryCatch(Sys.which("docker"), error = function(e) "")
  if (!nzchar(bin))
    return(list(ok = FALSE, msg = "the 'docker' command was not found on PATH. Install Docker Desktop (https://www.docker.com/products/docker-desktop/) and ensure it is running."))
  if (!requireNamespace("processx", quietly = TRUE))
    return(list(ok = FALSE, msg = "the 'processx' package is required for sandbox = \"docker\"."))
  res <- tryCatch(
    processx::run("docker", c("info"), error_on_status = FALSE, timeout = 15),
    error = function(e) NULL)
  if (is.null(res) || res$status != 0)
    return(list(ok = FALSE, msg = "Docker does not appear to be running. Start Docker Desktop and try again."))
  list(ok = TRUE, msg = "")
}

# Fixed non-root uid:gid every docker run in this backend uses. Arbitrary but
# constant, so files written into a bind-mounted host directory (the
# materialised sandbox, the throwaway library) are owned by a predictable,
# non-root id on both sides of the mount.
.repro_docker_uid <- "1000:1000"

# A HOST path known to be under sandbox_root -> its path INSIDE the container
# (mounted at /sandbox). Uses substring() on the known prefix length, not a
# regex strip: sandbox_root is a real filesystem path that can contain
# regex-special characters (parens, brackets, braces -- a temp dir name is
# not guaranteed free of them), and escaping every one of those correctly is
# more failure-prone than just slicing off a known-length prefix (confirmed:
# an early regex-based version of this broke on a Windows temp path).
.repro_docker_container_path <- function(host_path, sandbox_root) {
  host_norm <- normalizePath(host_path, mustWork = FALSE)
  root_norm <- normalizePath(sandbox_root, mustWork = FALSE)
  root_norm <- gsub("\\\\", "/", root_norm)
  host_norm <- gsub("\\\\", "/", host_norm)
  root_prefix <- paste0(root_norm, "/")
  rel <- if (startsWith(host_norm, root_prefix))
    substring(host_norm, nchar(root_prefix) + 1L) else
    if (identical(host_norm, root_norm)) "" else basename(host_norm)
  paste0("/sandbox/", rel)
}

#' The pre-built metacheck Docker image
#'
#' `ghcr.io/scienceverse/metacheck_r` has the ~750 most common packages
#' found across a corpus scan of real papers' R code already installed (see
#' https://github.com/scienceverse/metacheck_docker_reproducibility for the
#' Dockerfile/package list/build instructions), so a run against it skips
#' most of the install phase entirely instead of reinstalling every
#' dependency from scratch. This is the DEFAULT image (see
#' `.repro_docker_image_for()`'s `use_declared_version` argument) -- pinned
#' to whatever R version it was built against, NOT the paper's own declared
#' version, which is the tradeoff `docker_use_declared_version` on
#' `reproducibility_check()` exists to let a caller opt out of.
#' @keywords internal
.repro_docker_default_image <- "ghcr.io/scienceverse/metacheck_r:latest"

#' The Docker base image to use for a run
#'
#' By default returns the pre-built [.repro_docker_default_image] regardless
#' of what R version the paper declared -- fast (most dependencies are
#' already installed), but runs against whatever R version that image was
#' built with, not necessarily the paper's own. When `use_declared_version`
#' is TRUE, picks the `rocker/r-ver` tag matching the R version
#' [.code_version_pin_check()] found declared in the paper's repository (an
#' `renv.lock`'s `R.Version`, or a `sessionInfo()` dump's first line — see
#' that function) instead — a bare image with nothing pre-installed, so
#' every dependency is installed from scratch, but it matches the paper's
#' own declared environment. Falls back to bare `rocker/r-ver:latest` (not
#' the pre-built image) when `use_declared_version = TRUE` but nothing was
#' declared, since there is no version to match in that case either way, and
#' silently reusing the pre-built image would defeat the point of asking for
#' version-matching in the first place.
#'
#' @param r_versions character vector of declared R version strings (e.g.
#'   from `.code_version_pin_check()$r_versions`), possibly empty
#' @param use_declared_version if TRUE, use `r_versions` (see above) instead
#'   of the pre-built image. Default FALSE.
#'
#' @returns a single image reference, e.g.
#'   `"ghcr.io/scienceverse/metacheck_r:latest"`, `"rocker/r-ver:4.3.1"`, or
#'   `"rocker/r-ver:latest"`
#' @keywords internal
.repro_docker_image_for <- function(r_versions = character(0),
                                    use_declared_version = FALSE) {
  if (!isTRUE(use_declared_version)) return(.repro_docker_default_image)

  # Only a clean X.Y[.Z] shape is trusted as a tag -- anything else (embedded
  # whitespace/newlines from a malformed extraction upstream, a version with
  # extra text still attached, an empty string after trimming) falls back to
  # "latest" rather than being spliced into the image reference as-is.
  # Confirmed as a REAL failure mode, not a hypothetical: a corrupted
  # extraction upstream (see .code_version_pin_check()'s sessionInfo scan)
  # once produced the version string "\n4.5.0", which propagated all the way
  # into `docker run`'s image argument as "rocker/r-ver:\n4.5.0" -- an
  # embedded newline inside a single CLI argument, silently breaking EVERY
  # docker run call in the batch with "invalid reference format", with no
  # indication of why. That upstream bug is fixed at its source too, but this
  # function is the last point before the value becomes a shell/Docker
  # argument, so it validates independently rather than trusting any caller.
  clean_pat <- "^[0-9]+\\.[0-9]+(\\.[0-9]+)?$"
  v <- r_versions[!is.na(r_versions) & grepl(clean_pat, trimws(r_versions %||% ""))]
  v <- trimws(v)
  tag <- if (length(v) > 0) v[[1]] else "latest"
  paste0("rocker/r-ver:", tag)
}

#' Install R dependencies inside a throwaway Docker container
#'
#' The Docker-backed twin of [repro_install_deps()]: same return contract,
#' same throwaway-library semantics, but the actual `install.packages()`/
#' `remotes::install_github()` call runs INSIDE a container (default
#' networking, since CRAN/GitHub must be reachable), writing into `lib_dir`
#' via a bind mount — so the run phase ([repro_run_scripts_docker()]), which
#' mounts the same `lib_dir` with the network off, finds the packages already
#' there.
#'
#' Unlike [repro_install_deps()], there is no `cran_to_main_lib` option: a
#' container has no access to (and must not be given access to) the host's
#' real R library, so every install always goes into the throwaway `lib_dir`.
#'
#' @param install_deps a data frame as produced inside `reproducibility_check()`
#'   (`package`, `source`, `ref`, `base`) — the non-base rows of
#'   [repro_dependencies()]'s output
#' @param lib_dir throwaway library directory ON THE HOST (created if absent);
#'   bind-mounted into the container at `/rlib`
#' @param image the Docker image to install into (see [.repro_docker_image_for()])
#' @param timeout per-package-batch timeout in seconds for the whole install
#'   container run (default 600 — installs can be slow, esp. compiling from
#'   source)
#'
#' @returns a data frame with `package`, `source`, `installed` (logical),
#'   `message` (error text on failure, else ""), `via_archive` (always FALSE
#'   — the CRAN Archive retry [repro_install_deps()] does is not duplicated
#'   here; a package unavailable on live CRAN is simply recorded as failed) —
#'   same columns as [repro_install_deps()] so callers do not need to branch
#'   on which backend ran.
#' @export
repro_install_deps_docker <- function(install_deps, lib_dir, image = "rocker/r-ver:latest",
                                      timeout = 600) {
  empty <- data.frame(package = character(0), source = character(0),
                      installed = logical(0), message = character(0),
                      via_archive = logical(0))
  if (is.null(install_deps) || nrow(install_deps) == 0) return(empty)
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)

  # One install SCRIPT per call (not one docker run per package): a single
  # container start pays Docker's own startup overhead once for the whole
  # batch, and install.packages() already reports per-package success/failure
  # on its own -- so the script below installs every package in one process,
  # then writes a small per-package result table the host reads back.
  results_file <- tempfile(fileext = ".rds")
  results_file_container <- "/sandbox/.install_results.rds"

  # Results are written back to the host INCREMENTALLY (one saveRDS() after
  # each package, not one at the very end) so a batch that times out partway
  # through still reports every package that finished before the cutoff,
  # rather than losing the whole batch to whichever package was slowest or
  # got cut off. This matters in practice: a real paper's dependency list can
  # be ~20 packages, several needing to compile from source (car, lme4,
  # DHARMa, simr, ...) against a rocker/r-ver image with no pre-built
  # binaries -- a single docker run covering the WHOLE batch can legitimately
  # take 30-40+ minutes, and losing every already-installed result to one
  # slow package near the end (confirmed as the original all-or-nothing
  # shape before this) makes the timeout choice far more failure-prone than
  # it needs to be.
  script_lines <- c(
    'lib <- "/rlib"',
    'dir.create(lib, recursive = TRUE, showWarnings = FALSE)',
    '.libPaths(c(lib, .libPaths()))',
    'repos <- getOption("repos")',
    'if (is.null(repos) || !length(repos) || any(!nzchar(repos)) || any(repos == "@CRAN@"))',
    '  options(repos = c(CRAN = "https://cloud.r-project.org"))',
    'gh_avail <- requireNamespace("remotes", quietly = TRUE)',
    'pkgs   <- .repro_docker_pkgs',
    'srcs   <- .repro_docker_srcs',
    'refs   <- .repro_docker_refs',
    'out <- list()',
    'for (i in seq_along(pkgs)) {',
    '  pkg <- pkgs[i]; src <- srcs[i]; ref <- refs[i]',
    '  res <- tryCatch({',
    '    if (identical(src, "github")) {',
    '      if (!gh_avail) stop("the remotes package is not available in this image")',
    '      remotes::install_github(ref, lib = lib, upgrade = "never", quiet = FALSE)',
    '    } else if (identical(src, "url")) {',
    '      install.packages(ref, lib = lib, repos = NULL, quiet = FALSE)',
    '    } else {',
    '      install.packages(pkg, lib = lib, quiet = FALSE)',
    '    }',
    '    if (!requireNamespace(pkg, quietly = TRUE, lib.loc = lib))',
    '      stop("installed but package is not loadable")',
    '    list(ok = TRUE, msg = "")',
    '  }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))',
    '  out[[i]] <- data.frame(package = pkg, source = src, installed = res$ok, message = res$msg)',
    '  saveRDS(do.call(rbind, out), "/sandbox/.install_results.rds")',
    '}'
  )

  sandbox_dir <- tempfile("repro_docker_install_")
  dir.create(sandbox_dir, recursive = TRUE)
  on.exit(unlink(sandbox_dir, recursive = TRUE), add = TRUE)

  # The package/source/ref vectors are spliced in as literal R code (not
  # passed as docker run args) so they survive shell quoting untouched --
  # same reasoning code_read()/others use elsewhere for anything that must
  # round-trip exactly. dput() on a character vector is safe here: every
  # value originates from repro_dependencies()'s own regex extraction
  # (package/source names, GitHub refs), never from the paper's arbitrary
  # file content.
  header <- c(
    paste0(".repro_docker_pkgs <- ", deparse(install_deps$package)),
    paste0(".repro_docker_srcs <- ", deparse(install_deps$source)),
    paste0(".repro_docker_refs <- ", deparse(install_deps$ref))
  )
  script_path <- file.path(sandbox_dir, "install.R")
  # No BOM: writeLines()/UTF-8 without a BOM is R's own default, but Docker
  # Desktop path handling on Windows makes this worth being explicit about --
  # a BOM here breaks the container's R parser (confirmed).
  con <- file(script_path, open = "wb", encoding = "UTF-8")
  writeLines(c(header, script_lines), con, useBytes = TRUE)
  close(con)

  # Network stays on (install.packages()/remotes need CRAN/GitHub reachable),
  # but everything else is still hardened: install.packages() runs each
  # package's configure/.onLoad code, which is as much "running someone
  # else's code" as the run phase is, just from CRAN/GitHub rather than the
  # paper's own repo. Not --read-only here (R build tooling writes temp files
  # in more places than just /sandbox and /rlib), but non-root + dropped
  # capabilities cost nothing and narrow what a malicious build script could
  # do even with network access.
  args <- c("run", "--rm",
           "--user", .repro_docker_uid,
           "--cap-drop", "ALL",
           "--security-opt", "no-new-privileges",
           "--pids-limit", "512",
           "-v", paste0(sandbox_dir, ":/sandbox"),
           "-v", paste0(normalizePath(lib_dir, mustWork = FALSE), ":/rlib"),
           image, "Rscript", "/sandbox/install.R")

  out_file <- tempfile(fileext = ".out")
  res <- tryCatch(
    processx::run("docker", args, error_on_status = FALSE, timeout = timeout,
                  stdout = out_file, stderr = out_file),
    error = function(e) NULL)

  # A results file with FEWER rows than install_deps is a real, expected
  # outcome now that results are written incrementally (see script_lines'
  # own comment above): a timeout partway through the batch leaves rows for
  # every package that finished before the cutoff, but nothing at all for
  # the ones after it. Those missing packages must still be represented in
  # the returned table -- silently dropping them (an early `return(tbl)`
  # with no padding) would make failed_deps miss them entirely, so a run
  # phase script needing one of those UNLISTED packages would be
  # misclassified `errored` instead of `dependency_unavailable`.
  results_path <- file.path(sandbox_dir, ".install_results.rds")
  tbl <- if (!is.null(res) && file.exists(results_path))
    tryCatch(readRDS(results_path), error = function(e) NULL) else NULL

  msg <- if (!is.null(res)) {
    txt <- tryCatch(paste(readLines(out_file, warn = FALSE), collapse = "\n"),
                    error = function(e) "")
    if (isTRUE(res$timeout)) paste0("docker install timed out after ", timeout, "s")
    else paste0("docker install failed (status ", res$status, "): ", txt)
  } else "docker run could not be started"
  unlink(out_file)

  missing_pkgs <- setdiff(install_deps$package, tbl$package %||% character(0))
  padding <- if (length(missing_pkgs) > 0) {
    src_of <- stats::setNames(install_deps$source, install_deps$package)
    data.frame(package = missing_pkgs, source = unname(src_of[missing_pkgs]),
              installed = FALSE, message = msg)
  } else NULL

  out <- dplyr::bind_rows(tbl, padding)
  out$via_archive <- FALSE   # the CRAN Archive retry repro_install_deps() does is not duplicated here
  out
}

# Render .r_capture_runner()/.r_capture_helpers()'s logic (R/r-capture.R) as
# a STANDALONE script fragment, for a container that has no shared R session
# to serialise a closure from the way callr::r() does. deparse()-ing each
# helper function reproduces exactly what callr's own serialisation already
# does implicitly (confirmed: every helper is a closure over nothing but the
# inlined .STATO_* tables, so its deparsed source is fully self-contained --
# see this file's own header comment). Returns character lines to prepend to
# the per-script runner, not a callable object.
.repro_docker_capture_preamble <- function() {
  helpers <- .r_capture_helpers()
  out <- character(0)
  for (nm in names(helpers)) {
    fn_lines <- deparse(helpers[[nm]])
    nm_lit <- if (grepl("^[.a-zA-Z][.a-zA-Z0-9_]*$", nm)) nm else paste0("`", nm, "`")
    out <- c(out, paste0(nm_lit, " <- ", fn_lines[1]), fn_lines[-1])
  }
  out
}

#' Run the paper's scripts, in order, each in an isolated Docker container
#'
#' The Docker-backed twin of [repro_run_scripts()]: same return contract, same
#' per-script semantics (skip on missing inputs / parse failure, timeout,
#' undefined-variable / dependency-unavailable classification, captured
#' result objects), but each script runs via `docker run --network none
#' --read-only --user <non-root>` instead of a `callr` subprocess — so the
#' code cannot reach the network or touch anything outside the mounted
#' sandbox and library, even deliberately. See this file's header comment for
#' the two-phase (install-then-run) design and why the library is mounted at
#' `/rlib`, not `/lib`.
#'
#' @param run_tbl `repro_write_scripts()` output (`file_name`, `script_path`,
#'   `run_dir`) — `run_dir` must be the SAME directory as (or a subdirectory
#'   of) `sandbox_root`, so it can be bind-mounted as a whole
#' @param order a vector of `file_name`s in the order to run
#' @param sandbox_root the materialised layout root on the HOST (from
#'   [repro_materialize_layout()]) — mounted read-write at `/sandbox`
#' @param lib_dir throwaway library ON THE HOST (or NULL) — mounted
#'   read-only at `/rlib` when supplied
#' @param image the Docker image to run scripts in (see [.repro_docker_image_for()])
#' @param timeout per-script timeout in seconds
#' @param skip character vector of `file_name`s to record as
#'   `skipped_missing_inputs` instead of running
#' @param parses named logical (by file_name); a file that will not parse is
#'   recorded `not_parsed` and not run
#' @param failed_deps character vector of package names
#'   [repro_install_deps_docker()] could not install
#'
#' @returns a data frame — identical columns to [repro_run_scripts()]'s
#'   return: `file_name`, `outcome`, `error`, `error_type`, `undefined_var`,
#'   `stdout`, `stderr`, `elapsed`, `script_lines`, `captures`
#' @export
repro_run_scripts_docker <- function(run_tbl, order, sandbox_root, lib_dir = NULL,
                                     image = "rocker/r-ver:latest", timeout = 600,
                                     skip = character(0), parses = NULL,
                                     failed_deps = character(0)) {
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

  capture_preamble <- .repro_docker_capture_preamble()

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

    message("[repro/docker]   -> running '", fn, "' (timeout ", timeout, "s) ...")

    # script_path is a HOST path under sandbox_root; translate it to the
    # container path (/sandbox/<relative part>) since the container never
    # sees the host's own path.
    container_script <- .repro_docker_container_path(row$script_path, sandbox_root)

    # The capture-runner wrapper: same parse/eval/echo/capture loop as
    # .r_capture_runner() (R/r-capture.R), rendered as a standalone script
    # since there is no shared R session to serialise a closure from. Written
    # fresh per script (not reused) so its own capture_file path is unique.
    #
    # The whole body is wrapped in a FUNCTION that is then called (not left
    # as top-level statements): on.exit() registered at the true top level of
    # an Rscript-executed file does not fire at interpreter shutdown -- R's
    # on.exit() semantics are scoped to a function's own call frame, and
    # Rscript's top level is the C driver loop, not a function call.
    # Confirmed directly: a minimal top-level `on.exit(saveRDS(...))` in an
    # Rscript file never wrote its file, while the identical on.exit() call
    # placed inside a function that is then invoked worked correctly. This is
    # exactly why .r_capture_runner() itself is a function in the first place
    # (called by callr::r()) -- this wrapper now matches that shape.
    cap_file_container <- "/sandbox/.capture.rds"
    # /rlib must be added to .libPaths() explicitly: unlike repro_run_scripts()'s
    # callr::r(libpath = ...), there is no Docker-level equivalent that makes an
    # installed package findable -- the script's own library()/require() calls
    # only see it if this wrapper puts it on the search path first. Confirmed as
    # a real bug: a package installed by repro_install_deps_docker() into the
    # SAME mounted /rlib was still reported "there is no package called" by the
    # run phase before this line was added.
    libpath_setup <- if (!is.null(lib_dir_norm))
      '  .libPaths(c("/rlib", .libPaths()))' else NULL
    wrapper_lines <- c(
      capture_preamble,
      '.repro_docker_run <- function() {',
      libpath_setup,
      '  captures <- list()',
      paste0('  on.exit(try(saveRDS(captures, "', cap_file_container, '"), silent = TRUE), add = TRUE)'),
      paste0('  exprs <- parse("', container_script, '", keep.source = TRUE)'),
      '  srcrefs <- attr(exprs, "srcref")',
      '  env <- globalenv()',
      '  for (i in seq_along(exprs)) {',
      '    e <- exprs[[i]]',
      '    sr <- if (!is.null(srcrefs) && length(srcrefs) >= i) srcrefs[[i]] else NULL',
      '    txt <- if (!is.null(sr)) as.character(sr) else deparse(e)',
      '    cat(paste0("> ", txt, collapse = "\\n"), "\\n", sep = "")',
      '    res <- withVisible(eval(e, envir = env))',
      '    if (res$visible) print(res$value)',
      '    call_txt <- paste(txt, collapse = " ")',
      '    rec <- tryCatch(.r_capture_value(res$value, call_txt), error = function(e) NULL)',
      '    if (!is.null(rec)) {',
      '      rec$line <- if (!is.null(sr)) as.integer(sr[1L]) else NA_integer_',
      '      rec$call_text <- call_txt',
      '      captures[[length(captures) + 1L]] <- rec',
      '    }',
      '  }',
      '  invisible(NULL)',
      '}',
      '.repro_docker_run()'
    )
    wrapper_path <- file.path(sandbox_root, paste0(".runner_", make.names(fn), ".R"))
    con <- file(wrapper_path, open = "wb", encoding = "UTF-8")
    writeLines(wrapper_lines, con, useBytes = TRUE)
    close(con)
    on.exit(unlink(wrapper_path), add = TRUE)
    container_wrapper <- .repro_docker_container_path(wrapper_path, sandbox_root)

    # Working directory is always /sandbox itself: repro_write_scripts()'s own
    # contract is "run_dir is always root" (every script's relative paths are
    # rewritten against the sandbox ROOT, not its own subdirectory -- see that
    # function's roxygen), so there is no per-script subdirectory to cd into.
    args <- c("run", "--rm",
             "--network", "none", "--read-only",
             "--tmpfs", "/tmp",
             "--user", .repro_docker_uid,
             "--cap-drop", "ALL",
             "--security-opt", "no-new-privileges",
             "--pids-limit", "512",
             "-w", "/sandbox",
             "-v", paste0(sandbox_root, ":/sandbox"))
    if (!is.null(lib_dir_norm))
      args <- c(args, "-v", paste0(lib_dir_norm, ":/rlib:ro"))
    args <- c(args, image, "Rscript", container_wrapper)

    out_file <- tempfile(fileext = ".out")
    err_file <- tempfile(fileext = ".err")
    t0 <- Sys.time()
    res <- tryCatch(
      processx::run("docker", args, error_on_status = FALSE, timeout = timeout,
                    stdout = out_file, stderr = err_file),
      error = function(e) e)
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    cap_file_host <- file.path(sandbox_root, ".capture.rds")
    captures <- if (file.exists(cap_file_host))
      tryCatch(readRDS(cap_file_host), error = function(e) NULL) else NULL
    unlink(cap_file_host)

    read_cap <- function(f) if (file.exists(f))
      paste(readLines(f, warn = FALSE), collapse = "\n") else ""
    so <- read_cap(out_file); se <- read_cap(err_file)
    unlink(c(out_file, err_file))

    message("[repro/docker]   <- '", fn, "' done in ", round(elapsed, 1), "s")

    is_timeout <- inherits(res, "condition") && isTRUE(attr(res, "timeout") %||%
      grepl("timed? ?out", conditionMessage(res), ignore.case = TRUE))
    # processx::run(error_on_status = FALSE) never throws for a nonzero exit;
    # it returns normally with $status != 0. A THROWN condition here means
    # docker itself could not be started/completed (not the script's fault).
    if (inherits(res, "condition")) {
      msg <- conditionMessage(res)
      etype <- if (is_timeout) "timeout" else "runtime"
      outc <- if (is_timeout) "timed_out" else "errored"
      return(data.frame(file_name = fn, outcome = outc, error = msg,
                        error_type = etype, undefined_var = NA_character_,
                        stdout = so, stderr = se, elapsed = elapsed) |>
               dplyr::mutate(script_lines = list(exec_lines), captures = list(captures)))
    }
    if (isTRUE(res$timeout)) {
      return(data.frame(file_name = fn, outcome = "timed_out",
                        error = paste0("timed out after ", timeout, "s"),
                        error_type = "timeout", undefined_var = NA_character_,
                        stdout = so, stderr = se, elapsed = elapsed) |>
               dplyr::mutate(script_lines = list(exec_lines), captures = list(captures)))
    }
    if (res$status != 0) {
      undef_pat <- "object ['\"]([^'\"]+)['\"] not found"
      undef_src <- if (grepl(undef_pat, se)) se else NA_character_
      undef_var <- if (!is.na(undef_src))
        sub(paste0(".*", undef_pat, ".*"), "\\1",
            regmatches(undef_src, regexpr(undef_pat, undef_src))) else NA_character_

      nopkg_pat <- "there is no package called ['\"]([^'\"]+)['\"]"
      nopkg_src <- if (grepl(nopkg_pat, se)) se else NA_character_
      nopkg_var <- if (!is.na(nopkg_src))
        sub(paste0(".*", nopkg_pat, ".*"), "\\1",
            regmatches(nopkg_src, regexpr(nopkg_pat, nopkg_src))) else NA_character_
      dep_unavailable <- !is.na(nopkg_var) && nopkg_var %in% failed_deps

      etype <- if (dep_unavailable) "dependency_unavailable"
               else if (!is.na(undef_var)) "undefined_variable"
               else "runtime"
      outc <- if (dep_unavailable) "dependency_unavailable" else "errored"
      return(data.frame(file_name = fn, outcome = outc,
                        error = se, error_type = etype, undefined_var = undef_var,
                        stdout = so, stderr = se, elapsed = elapsed) |>
               dplyr::mutate(script_lines = list(exec_lines), captures = list(captures)))
    }
    data.frame(file_name = fn, outcome = "ran_ok", error = "",
              error_type = NA_character_, undefined_var = NA_character_,
              stdout = so, stderr = se, elapsed = elapsed) |>
      dplyr::mutate(script_lines = list(exec_lines), captures = list(captures))
  })
  dplyr::bind_rows(rows)
}
