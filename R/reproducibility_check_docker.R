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
#'   otherwise a human-readable reason, e.g. Docker not installed, the
#'   daemon not running, or the daemon too busy to answer)
#' @export
repro_docker_available <- function() {
  bin <- tryCatch(Sys.which("docker"), error = function(e) "")
  if (!nzchar(bin))
    return(list(ok = FALSE, msg = "the 'docker' command was not found on PATH. Install Docker Desktop (https://www.docker.com/products/docker-desktop/) and ensure it is running."))
  if (!requireNamespace("processx", quietly = TRUE))
    return(list(ok = FALSE, msg = "the 'processx' package is required for sandbox = \"docker\"."))
  # A running Docker can be slow to answer while other containers are busy, so
  # only a late answer earns another, longer try (GitHub issue #560).
  for (wait in c(15, 30, 60)) {
    res <- tryCatch(
      processx::run("docker", c("info"), error_on_status = FALSE, timeout = wait),
      error = function(e) NULL)
    if (is.null(res) || !isTRUE(res$timeout)) break
  }
  if (!is.null(res) && isTRUE(res$timeout))
    return(list(ok = FALSE, msg = "Docker did not answer within 60 seconds, after two shorter tries. It may be busy; try again when it has less to do."))
  if (is.null(res) || res$status != 0)
    return(list(ok = FALSE, msg = "Docker does not appear to be running. Start Docker Desktop and try again."))
  list(ok = TRUE, msg = "")
}

# Fixed non-root uid:gid every docker run in this backend uses. Arbitrary but
# constant, so files written into a bind-mounted host directory (the
# materialised sandbox, the throwaway library) are owned by a predictable,
# non-root id on both sides of the mount.
.repro_docker_uid <- "1000:1000"

# A unique container name for one docker run, so it can be `docker stop`ped
# by name afterwards if needed (see .repro_docker_stop() and its call sites
# below -- GitHub issue #417). Built from tempfile()'s own randomness
# (already used throughout this file for sandbox/results paths) rather than
# adding a uuid dependency; "docker" itself only allows
# [a-zA-Z0-9][a-zA-Z0-9_.-]* in a container name, so non-alphanumeric
# characters from the temp path are stripped rather than escaped.
.repro_docker_container_name <- function() {
  raw <- basename(tempfile("repro_"))
  paste0("repro_", gsub("[^a-zA-Z0-9_.-]", "", raw))
}

# Best-effort `docker stop` on a container BY NAME, for the case
# processx::run(..., timeout = ...) reports a timeout: that only kills the
# host-side `docker run` CLI process (confirmed directly: a `docker run`
# killed via a processx timeout leaves its container `Up` and burning CPU
# indefinitely -- the CLI dying is not something the container's own process
# tree is listening for), never the container itself. `--rm` alone does not
# help either: it removes the container on ITS OWN exit, which a
# `docker stop` still triggers -- see this function's own use immediately
# after a detected timeout, below. Errors here are swallowed (best-effort,
# not resignalled as the run's own failure): the run has already been
# recorded as `timed_out` regardless of whether the stop itself succeeds,
# and a stop failing (e.g. the container happened to exit on its own between
# the timeout firing and this call) must not mask that already-correct
# result with a second, unrelated error.
.repro_docker_stop <- function(name, timeout = 30) {
  tryCatch(
    processx::run("docker", c("stop", "--time", "5", name),
                  error_on_status = FALSE, timeout = timeout),
    error = function(e) NULL)
  invisible(NULL)
}

# `--cpus`/`--memory` args for a docker run, from whatever
# reproducibility_check()'s batch dispatcher (workers > 1 — see
# .reproducibility_check_batch() in inst/modules/reproducibility_check.R) set
# via the `metacheck.docker_resource_limits` option, so N concurrent
# containers cannot collectively exceed the host. A single-paper,
# sequential call (workers = 1, the default, or reproducibility_check()
# called directly rather than through the batch path) never sets this option,
# so there is no limit on the processors or memory of the container.
#
# `--ulimit core=1` is always there: when R crashes in a container on Windows,
# WSL writes a file of about 2.5 GB to the user's temp folder. Ten crashing
# scripts filled a disk. A limit of 0 does not stop WSL (confirmed directly);
# the kernel only leaves out the file for a limit of 1.
.repro_docker_resource_args <- function() {
  no_crash_file <- c("--ulimit", "core=1")
  limits <- getOption("metacheck.docker_resource_limits")
  if (is.null(limits)) return(no_crash_file)
  c(no_crash_file, "--cpus", as.character(limits$cpus),
    "--memory", paste0(limits$memory_gb, "g"))
}

# The limits on what scripts download when the internet is on during a run (#568,
# item 21): the module's own max_file_size and max_download_size, in MB, as for the
# files of the repository, and what the paper's scripts received so far, which all
# of them share. NULL when the internet is off.
.repro_download_limits <- function(internet = FALSE, max_file_size = 100, max_download_size = 500) {
  if (!isTRUE(internet)) return(NULL)
  used <- new.env()
  used$bytes <- 0
  list(max_file = (max_file_size %||% Inf) * 1024^2,
       max_total = (max_download_size %||% Inf) * 1024^2, used = used)
}

# `docker run` arguments for the network: off, unless the internet is on.
.repro_docker_network_args <- function(net) {
  if (is.null(net)) c("--network", "none") else character(0)
}

# The command a container runs, with the internet on: it first marks its start in
# /tmp, so a file is known to be new, and at the end writes what it received to the
# sandbox, so a script that ends between two checks is still counted. The command's
# own exit status is kept.
.repro_docker_counted_cmd <- function(cmd, net) {
  if (is.null(net)) return(cmd)
  c("sh", "-c", paste0('touch /tmp/.repro_t0; "$@"; s=$?; ',
                       'cat /sys/class/net/eth0/statistics/rx_bytes > /sandbox/.repro_received 2>/dev/null; exit $s'),
    "sh", cmd)
}

# What a running container has received over the network, in bytes, and, once that
# is at least `max_file`, the first file of at least that size that appeared in the
# sandbox or /tmp since the script started. NA when the container could not be asked.
.repro_docker_received <- function(name, max_file = Inf) {
  find <- if (is.finite(max_file))
    sprintf(paste0('; [ "$r" -ge %.0f ] && find /sandbox /tmp -type f -newer /tmp/.repro_t0 ',
                   '-size +%.0fk -printf "%%s %%p\\n" 2>/dev/null | head -n 1'),
            max_file, floor(max_file / 1024)) else ""
  res <- tryCatch(processx::run("docker", c("exec", name, "sh", "-c",
                    paste0("r=$(cat /sys/class/net/eth0/statistics/rx_bytes); echo $r", find)),
                    error_on_status = FALSE, timeout = 20),
                  error = function(e) NULL)
  out <- if (is.null(res)) character(0) else strsplit(trimws(res$stdout), "\n", fixed = TRUE)[[1]]
  bytes <- suppressWarnings(as.numeric(out[1]))
  big <- if (length(out) > 1) regmatches(out[2], regexec("^(\\d+) (.+)$", out[2]))[[1]] else character(0)
  list(bytes = if (length(bytes) && !is.na(bytes)) bytes else NA_real_,
       file = if (length(big) == 3) big[3] else NA_character_,
       file_bytes = if (length(big) == 3) as.numeric(big[2]) else NA_real_)
}

.repro_mb <- function(bytes) format(round(bytes / 1024^2, 1), big.mark = ",")

# The error of a script that ran out of time, which names a download that was still
# coming in (see .repro_docker_run_watched()).
.repro_timeout_text <- function(timeout, watched = NULL) {
  paste0("timed out after ", round(timeout), "s",
         if (isTRUE(watched$arriving))
           sprintf(" while a download was still arriving (%s MB received)", .repro_mb(watched$received)))
}

# Run a `docker run` and wait for it. With the internet off (`net` NULL) this is
# processx::run(). With it on, the container is checked every two seconds (a check
# takes about 0.15 seconds), and
# stopped when the paper's scripts together have downloaded more than
# max_download_size, or a file larger than max_file_size has appeared (#568, item
# 21), as the repository's own files are limited. A download that ends between two
# checks is still counted towards the total, so a later script is stopped.
# `arriving` says whether data was still coming in when the time ran out, so a slow
# download is named as the reason.
.repro_docker_run_watched <- function(args, name, timeout, stdout, stderr, sandbox_root, net = NULL) {
  if (is.null(net))
    return(list(res = tryCatch(
      processx::run("docker", args, error_on_status = FALSE, timeout = timeout,
                    stdout = stdout, stderr = stderr),
      error = function(e) e), stopped = NA_character_, arriving = FALSE))

  received_file <- file.path(sandbox_root, ".repro_received")
  unlink(received_file)
  p <- tryCatch(processx::process$new("docker", args, stdout = stdout, stderr = stderr),
                error = function(e) e)
  if (inherits(p, "condition")) return(list(res = p, stopped = NA_character_, arriving = FALSE))
  t0 <- Sys.time()
  before <- net$used$bytes
  rx <- 0
  rx_prev <- 0
  stopped <- NA_character_
  timed_out <- FALSE
  repeat {
    left <- timeout - as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (left <= 0) { timed_out <- TRUE; break }
    p$wait(as.integer(ceiling(min(2, left) * 1000)))
    if (!p$is_alive()) break
    seen <- .repro_docker_received(name, net$max_file)
    if (is.na(seen$bytes)) next
    rx_prev <- rx
    rx <- seen$bytes
    if (before + rx > net$max_total) {
      stopped <- sprintf(paste0("stopped: the paper's scripts downloaded %s MB, more than the limit ",
                                "of %s MB (max_download_size)"),
                         .repro_mb(before + rx), .repro_mb(net$max_total))
      break
    }
    if (!is.na(seen$file)) {
      stopped <- sprintf(paste0("stopped: it downloaded %s, a file of %s MB, more than the limit ",
                                "of %s MB per file (max_file_size)"),
                         sub("^/sandbox/", "", seen$file), .repro_mb(seen$file_bytes),
                         .repro_mb(net$max_file))
      break
    }
  }
  if (timed_out || !is.na(stopped)) {
    .repro_docker_stop(name)
    p$wait(10000)
    if (p$is_alive()) p$kill()
  }
  if (file.exists(received_file)) {
    final <- suppressWarnings(as.numeric(readLines(received_file, warn = FALSE)[1]))
    if (length(final) && !is.na(final)) rx <- max(rx, final)
    unlink(received_file)
  }
  net$used$bytes <- before + rx
  res <- if (timed_out) list(status = NA_integer_, timeout = TRUE) else
    list(status = p$get_exit_status() %||% NA_integer_, timeout = FALSE)
  # more than 64 KB since the check before: data was still coming in
  list(res = res, stopped = stopped, received = rx,
       arriving = timed_out && rx - rx_prev > 64 * 1024)
}

# Whether a run's message comes from Docker itself rather than from the script: a
# script cannot be judged when its container never started. While Docker Desktop
# was down, every script of a paper was recorded as a failing script (#568, item 27).
.repro_docker_failed <- function(txt) {
  grepl(paste0("Error response from daemon|Docker Desktop is unable to start|",
               "Cannot connect to the Docker daemon|failed to connect to the docker API|",
               "error during connect|dockerDesktopLinuxEngine"), txt %||% "", ignore.case = TRUE)
}

# Where packages live that are on neither CRAN nor Bioconductor (#568, item 12): a
# GitHub owner/repository, or the address of a repository of packages.
.repro_known_package_places <- function() {
  c(INLA = "https://inla.r-inla-download.org/R/stable",
    NicheMapR = "mrke/NicheMapR", glmmADMB = "bbolker/glmmadmb", rethinking = "rmcelreath/rethinking",
    cmdstanr = "stan-dev/cmdstanr", TwGeos = "SLisovski/TwGeos", GeoLocTools = "SLisovski/GeoLocTools",
    SGAT = "SWotherspoon/SGAT", microclima = "ilyamaclean/microclima", V.PhyloMaker = "jinyizju/V.PhyloMaker",
    metabaR = "metabaRfactory/metabaR", calecopal = "an-bui/calecopal", easyGgplot2 = "kassambara/easyGgplot2",
    ggthemr = "Mikata-Project/ggthemr", BuenColors = "caleblareau/BuenColors", coefplot2 = "palday/coefplot2",
    tess3r = "bcm-uga/TESS3_encho_sen", PAMLr = "KiranLDA/PAMLr", rcoalescence = "thompsonsed/rcoalescence",
    datamodelr = "bergant/datamodelr", biomonitoR = "alexology/biomonitoR", scampr = "ElliotDovers/scampr",
    mapppdr = "CCheCastaldo/mapppdr", riojaPlot = "nsj3/riojaPlot", PCAviz = "NovembreLab/PCAviz",
    methClust = "kkdey/methClust", remake = "richfitz/remake", ascr = "b-steve/ascr",
    dentist = "phylotastic/dentist", Hmsc = "hmsc-r/HMSC", HMSC = "hmsc-r/HMSC")
}

#' The part of the install script for packages that are not on CRAN
#'
#' A second pass after every package has had its turn, since one time limit
#' covers the whole install: first Bioconductor, because the fixed list of
#' [.repro_bioc_packages()] misses many names and some packages moved there,
#' keeping only an old copy in the CRAN Archive; then the latest version in the
#' CRAN Archive, as [.repro_cran_archive_install()] does outside Docker. Two rounds,
#' because a package can need another archived one (#568, items 9 and 11).
#' Runs inside the container, without this package.
#'
#' @returns lines of R code that follow the install loop
#' @keywords internal
.repro_docker_elsewhere_lines <- function() {
  c(
    # This R treats a warning old packages cause as an error, and has dropped
    # Calloc() and the like, so maptools did not compile. Only for this pass.
    r"(makevars <- file.path(tempdir(), "Makevars"))",
    r"(writeLines(c("CFLAGS += -Wno-error=format-security -DCalloc=R_Calloc -DRealloc=R_Realloc -DFree=R_Free", paste0(c("CXXFLAGS", "CXX11FLAGS", "CXX14FLAGS", "CXX17FLAGS", "CXX20FLAGS"), " += -Wno-error=format-security")), makevars))",
    r"(Sys.setenv(R_MAKEVARS_USER = makevars, R_LIBS = paste(.libPaths(), collapse = ":")))",
    r"(retry_rows <- function() which(vapply(out, function(x) !x$installed && identical(x$source, "cran") && grepl("is not available|there is no package called", x$message), logical(1))))",
    r"(bioc_known <- NULL)",
    r"(in_elsewhere <- function(pkg) {)",
    r"(  if (is.null(bioc_known)) bioc_known <<- tryCatch({)",
    r"(    if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager", lib = lib, quiet = TRUE))",
    r"(    rownames(utils::available.packages(repos = BiocManager::repositories())))",
    r"(  }, error = function(e) character(0)))",
    # A package on CRAN can need one from Bioconductor, which install.packages()
    # does not find: speaq needs MassSpecWavelet.
    r"(  bioc_needed <- regmatches(paste(out[[match(pkg, pkgs)]]$message), regexpr("(?<=there is no package called .)[A-Za-z0-9.]+", paste(out[[match(pkg, pkgs)]]$message), perl = TRUE)))",
    r"(  if (length(bioc_needed) && bioc_needed %in% bioc_known) {)",
    r"(    BiocManager::install(bioc_needed, lib = lib, update = FALSE, ask = FALSE))",
    r"(    return("bioc"))",
    r"(  })",
    r"(  if (pkg %in% bioc_known) {)",
    r"(    BiocManager::install(pkg, lib = lib, update = FALSE, ask = FALSE))",
    r"(    return("bioc"))",
    r"(  })",
    r"(  listing <- suppressWarnings(tryCatch(readLines(paste0("https://cran.r-project.org/src/contrib/Archive/", pkg, "/"), warn = FALSE), error = function(e) character(0))))",
    # rgdal and rgeos compile against sp's header files, which sp dropped in
    # version 2.2 after they left CRAN; 2.1-4 is the last version that has them
    # (#568). The run then loads this sp, from the temporary library.
    r"(  if (pkg %in% c("rgdal", "rgeos") && !file.exists(file.path(lib, "sp", "include", "sp.h"))) {)",
    r"(    sp_src <- file.path(tempdir(), "sp_2.1-4.tar.gz"))",
    r"(    try(utils::download.file("https://cran.r-project.org/src/contrib/Archive/sp/sp_2.1-4.tar.gz", sp_src, quiet = TRUE), silent = TRUE))",
    r"(    if (file.exists(sp_src)) try(install.packages(sp_src, lib = lib, repos = NULL, type = "source", quiet = TRUE), silent = TRUE))",
    r"(  })",
    r"(  hits <- grep(paste0(pkg, "_[0-9][^\"']*\\.tar\\.gz"), listing, value = TRUE))",
    r"(  if (!length(hits)) return(NA_character_))",
    r"(  dates <- sub(".*(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}).*", "\\1", hits))",
    r"(  latest <- hits[order(dates, decreasing = TRUE)][1])",
    r"(  tarball <- regmatches(latest, regexpr(paste0(pkg, "_[0-9][^\"']*\\.tar\\.gz"), latest)))",
    r"(  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes", lib = lib, quiet = TRUE))",
    r"(  remotes::install_url(paste0("https://cran.r-project.org/src/contrib/Archive/", pkg, "/", tarball), lib = lib, upgrade = "never", dependencies = TRUE, quiet = TRUE))",
    r"(  "archive")",
    r"(})",
    # Packages only elsewhere (#568, item 12): a table of known places, then
    # R-universe's search for that exact name. A GitHub tarball needs no token.
    paste0("known_places <- ", deparse1(.repro_known_package_places())),
    r"(in_known_place <- function(pkg) {)",
    r"(  place <- unname(known_places[pkg]))",
    r"(  if (!is.na(place) && startsWith(place, "https://")) {)",
    r"(    install.packages(pkg, lib = lib, repos = c(place, getOption("repos")), quiet = TRUE))",
    r"(    return(place))",
    r"(  })",
    r"(  if (!is.na(place)) {)",
    r"(    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes", lib = lib, quiet = TRUE))",
    r"(    remotes::install_url(paste0("https://github.com/", place, "/archive/HEAD.tar.gz"), lib = lib, upgrade = "never", dependencies = TRUE, quiet = TRUE))",
    r"(    return(paste0("github.com/", place)))",
    r"(  })",
    # a search that fails is not a package R-universe lacks (#389); a bare
    # rocker/r-ver image has no jsonlite, so the search always failed there
    r"(  if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite", lib = lib, quiet = TRUE))",
    r"(  found <- tryCatch(jsonlite::fromJSON(paste0("https://r-universe.dev/api/search?q=", pkg, "&limit=20"))$results, error = function(e) e))",
    r"(  if (inherits(found, "error")) return(paste("the search of R-universe failed:", conditionMessage(found))))",
    r"(  if (is.null(found) || !NROW(found)) return(NA_character_))",
    r"(  who <- found[["_user"]][found$Package == pkg])",
    r"(  if (!length(who)) return(NA_character_))",
    r"(  repo <- paste0("https://", who[1], ".r-universe.dev"))",
    r"(  install.packages(pkg, lib = lib, repos = c(repo, getOption("repos")), quiet = TRUE))",
    r"(  repo)",
    r"(})",
    r"(for (round in 1:2) for (i in retry_rows()) {)",
    r"(  pkg <- pkgs[i])",
    r"(  writeLines(as.character(i), "/sandbox/.install_status"))",
    r"(  from <- tryCatch(in_elsewhere(pkg), error = function(e) conditionMessage(e)))",
    r"(  usable <- function() file.exists(file.path(lib, pkg, "DESCRIPTION")) && requireNamespace(pkg, quietly = TRUE, lib.loc = lib))",
    # what stopped it is kept, next to the first pass's "not available"
    r"(  tried <- from)",
    r"(  if (!usable()) from <- tryCatch(in_known_place(pkg), error = function(e) conditionMessage(e)))",
    r"(  stopped <- c(tried, from)[!is.na(c(tried, from)) & !c(tried, from) %in% c("bioc", "archive") & !grepl("^https://|^github", c(tried, from))])",
    r"(  if (!usable() && round == 2 && length(stopped)) out[[i]]$message <- paste0(out[[i]]$message, "; also tried elsewhere: ", substr(stopped[1], 1, 300)))",
    # Found in none of the places: the code does not say where the package comes
    # from, which authors can act on (#389). See .repro_classify_install_message().
    r"(  if (!usable() && round == 2 && is.na(tried) && is.na(from)) out[[i]]$message <- paste0(out[[i]]$message, "; found nowhere else: not on Bioconductor, in the CRAN Archive, in our list of known places or on R-universe"))",
    r"(  if (usable()) {)",
    r"(    out[[i]]$installed <- TRUE)",
    r"(    out[[i]]$message <- "")",
    r"(    out[[i]]$version_installed <- have_version(pkg))",
    r"(    out[[i]]$via_archive <- identical(from, "archive"))",
    r"(    if (identical(from, "bioc")) out[[i]]$source <- "bioc")",
    r"(    if (!from %in% c("bioc", "archive")) { out[[i]]$source <- "elsewhere"; out[[i]]$version_note <- paste0("installed from ", from) })",
    r"(  })",
    r"(  saveRDS(do.call(rbind, out), "/sandbox/.install_results.rds"))",
    r"(})"
  )
}

# The container's working directory for a script's run_dir: /sandbox itself, also
# when run_dir is missing, or a folder below it.
.repro_docker_workdir <- function(run_dir, sandbox_root) {
  if (is.null(run_dir) || is.na(run_dir) || !nzchar(run_dir)) return("/sandbox")
  if (normalizePath(run_dir, mustWork = FALSE) == normalizePath(sandbox_root, mustWork = FALSE)) return("/sandbox")
  .repro_docker_container_path(run_dir, sandbox_root)
}

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
#' `ghcr.io/scienceverse/metacheck_r_large` has the ~750 most common
#' packages found across a corpus scan of real papers' R code already
#' installed, plus JAGS, a JDK, and the GDAL/GEOS/PROJ geospatial stack
#' (see https://github.com/scienceverse/metacheck_docker_reproducibility
#' for the Dockerfile/package list/build instructions -- that repo also
#' publishes a smaller `metacheck_r_small` variant, ~390 packages, seen in
#' at least 3 corpus files, also with JAGS/a JDK but without the geospatial
#' stack), so a run against it needs no install for most papers. (With
#' `install_missing = TRUE` every declared package is still installed.) This is the
#' DEFAULT image (see `.repro_docker_image_for()`'s `use_declared_version`
#' argument) -- pinned to whatever R version it was built against, NOT the
#' paper's own declared version, which is the tradeoff
#' `docker_use_declared_version` on `reproducibility_check()` exists to let
#' a caller opt out of.
#'
#' History: this constant pointed at a single, un-split
#' `ghcr.io/scienceverse/metacheck_r` image until 2026-10, when the
#' `_large`/`_small` split described in the build repo's own README/
#' Dockerfiles was ACTUALLY built and pushed under those names for the
#' first time (confirmed live via `docker pull`, not assumed from the
#' README) -- a prior attempt to point this constant at `metacheck_r_large`
#' earlier that same session had to be reverted because the name did not
#' yet exist on the registry at that time; it does now. If this constant
#' is ever repointed again, confirm with `docker pull` first, not from this
#' file's own README alone -- see that README's own naming-status note for
#' why that verification step matters here specifically.
#' @keywords internal
.repro_docker_default_image <- "ghcr.io/scienceverse/metacheck_r_large:latest"

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
#'   `"ghcr.io/scienceverse/metacheck_r_large:latest"`, `"rocker/r-ver:4.3.1"`, or
#'   `"rocker/r-ver:latest"`
#' @keywords internal
.repro_docker_image_for <- function(r_versions = character(0),
                                    use_declared_version = FALSE) {
  # the option `metacheck.docker_image` names another image, to try one before it is published
  if (!isTRUE(use_declared_version)) return(getOption("metacheck.docker_image", .repro_docker_default_image))

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
#'   `message` (error text on failure, else ""), `via_archive` (`TRUE` when the
#'   package was not on CRAN and came from the CRAN Archive; a package not on
#'   CRAN is first looked for on Bioconductor, see
#'   [.repro_docker_elsewhere_lines()]),
#'   `category` (see [.repro_classify_install_message()]; `NA` for a
#'   successful install), `version_recorded`, `version_installed` and
#'   `version_note` — same columns as [repro_install_deps()] so callers
#'   do not need to branch on which backend ran. A row with a `version` is
#'   treated as there (see [repro_install_deps()]).
#' @export
repro_install_deps_docker <- function(install_deps, lib_dir, image = "rocker/r-ver:latest",
                                      timeout = 600) {
  empty <- data.frame(package = character(0), source = character(0),
                      installed = logical(0), message = character(0),
                      via_archive = logical(0), category = character(0),
                      version_recorded = character(0),
                      version_installed = character(0),
                      version_note = character(0))
  if (is.null(install_deps) || nrow(install_deps) == 0) return(empty)
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  versions <- install_deps$version %||% rep(NA_character_, nrow(install_deps))

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
    .repro_repos_lines(),
    'pkgs   <- .repro_docker_pkgs',
    'srcs   <- .repro_docker_srcs',
    'refs   <- .repro_docker_refs',
    'vers   <- .repro_docker_vers',
    # A bare rocker/r-ver image does not have remotes, and without it no
    # recorded version and no GitHub package can be installed.
    'gh_avail <- requireNamespace("remotes", quietly = TRUE)',
    'if (!gh_avail && (any(!is.na(vers)) || any(srcs == "github"))) {',
    '  try(install.packages("remotes", lib = lib, quiet = TRUE))',
    '  gh_avail <- requireNamespace("remotes", quietly = TRUE)',
    '}',
    # The same rules as .repro_installed_version() and .repro_same_version()
    # (R/reproducibility_check.R), inlined because this script runs without
    # this package.
    'have_version <- function(pkg) {',
    '  v <- suppressWarnings(tryCatch(utils::packageDescription(pkg, lib.loc = .libPaths(), fields = "Version"),',
    '                                 error = function(e) NA_character_))',
    '  if (length(v) != 1 || is.na(v)) NA_character_ else as.character(v)',
    '}',
    'same_version <- function(a, b) {',
    '  if (is.na(a) || is.na(b)) return(FALSE)',
    '  isTRUE(tryCatch(package_version(a) == package_version(b), error = function(e) FALSE))',
    '}',
    # The same rule as .repro_load_error() (R/reproducibility_check.R): a bare
    # image often lacks a system library, which only this message tells.
    'load_error <- function(pkg) {',
    '  msg <- tryCatch({ loadNamespace(pkg); "" }, error = function(e) conditionMessage(e))',
    '  substr(gsub("[[:space:]]+", " ", msg), 1, 300)',
    '}',
    'out <- list()',
    'for (i in seq_along(pkgs)) {',
    '  pkg <- pkgs[i]; src <- srcs[i]; ref <- refs[i]; ver <- vers[i]',
    # Read by the host while this runs, for the progress display.
    '  writeLines(as.character(i), "/sandbox/.install_status")',
    '  warns <- character(0)',
    '  note <- ""',
    '  version_done <- same_version(have_version(pkg), ver)',
    '  if (!version_done && !is.na(ver) && identical(src, "bioc")) {',
    '    note <- "Bioconductor versions follow the R version, so the current version was installed"',
    '  } else if (!version_done && !is.na(ver) && identical(src, "cran")) {',
    '    if (gh_avail) {',
    '      version_done <- tryCatch({',
    '        remotes::install_version(pkg, version = ver, lib = lib, upgrade = "never",',
    '                                 dependencies = NA, force = TRUE, quiet = FALSE)',
    '        same_version(have_version(pkg), ver)',
    '      }, error = function(e) { note <<- conditionMessage(e); FALSE })',
    '      if (!version_done && !nzchar(note)) note <- paste0("the recorded version could not be installed on R ", getRversion())',
    '    } else note <- "the remotes package is not available in this image"',
    '  }',
    '  res <- tryCatch({',
    '    withCallingHandlers({',
    '    if (version_done) {',
    '    } else if (identical(src, "github")) {',
    '      if (!gh_avail) stop("the remotes package is not available in this image")',
    '      remotes::install_github(ref, lib = lib, upgrade = "never", quiet = FALSE)',
    '    } else if (identical(src, "url")) {',
    '      install.packages(ref, lib = lib, repos = NULL, quiet = FALSE)',
    '    } else if (identical(src, "bioc")) {',
    '      if (!requireNamespace("BiocManager", quietly = TRUE))',
    '        install.packages("BiocManager", lib = lib, quiet = FALSE)',
    '      BiocManager::install(pkg, lib = lib, update = FALSE, ask = FALSE)',
    '    } else {',
    '      install.packages(pkg, lib = lib, quiet = FALSE)',
    '    }',
    '    }, warning = function(w) warns <<- c(warns, conditionMessage(w)))',
    # Same rule as .repro_not_loadable_msg() (R/reproducibility_check.R),
    # inlined because this script runs without this package: a package
    # install.packages() could not find only produces a "not available"
    # warning, which is the real reason (issue #421).
    # A recorded version already in the image is not in lib. Being in lib is
    # checked as well, since the load check also passes for a namespace an
    # earlier package loaded, and a failed install_github() does not stop. A
    # package that is gone from CRAN may still be in the image.
    '    in_lib <- file.exists(file.path(lib, pkg, "DESCRIPTION"))',
    '    in_image <- !in_lib && identical(src, "cran") && is.na(ver) && !is.na(have_version(pkg))',
    '    if (!(version_done || in_lib || in_image) ||',
    '        !requireNamespace(pkg, quietly = TRUE, lib.loc = if (in_lib && !version_done) lib else .libPaths())) {',
    '      na <- unique(grep("is not available|non-zero exit status", warns, value = TRUE))',
    '      stop(if (length(na) > 0) paste(na, collapse = "; ")',
    '           else if (!(version_done || in_lib)) "the install did not put the package in the library"',
    '           else paste0("installed but package is not loadable: ", load_error(pkg)))',
    '    }',
    '    list(ok = TRUE, msg = "")',
    '  }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))',
    '  v <- have_version(pkg)',
    '  if (res$ok && !is.na(ver) && !same_version(v, ver) && !nzchar(note))',
    '    note <- "the source gave another version than the recorded one"',
    # stringsAsFactors: the default is TRUE before R 4.0, and the image may be that old
    '  out[[i]] <- data.frame(package = pkg, source = src, installed = res$ok, message = res$msg,',
    '                         version_installed = v, version_note = note, via_archive = FALSE, stringsAsFactors = FALSE)',
    '  saveRDS(do.call(rbind, out), "/sandbox/.install_results.rds")',
    '}',
    .repro_docker_elsewhere_lines()
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
  # deparse1(), not deparse(): deparse() returns a character VECTOR of lines
  # once the deparsed value is long enough to wrap (R's default
  # width.cutoff), so paste0(prefix, deparse(x)) recycles the prefix against
  # each line instead of prefixing the whole literal -- header ends up with
  # the prefix attached only to the first line, and the remaining lines
  # spliced in bare, unassigned, later in the same c(). A paper with enough
  # dependencies (18 was enough in a real corpus paper) then produces a
  # broken install.R like:
  #   .repro_docker_pkgs <- c("a", "b", ...,
  #   "c", "d", ...
  #   .repro_docker_pkgs <- "e", "f", ...
  #   .repro_docker_pkgs"
  # which fails to parse, silently zeroing out every install in the batch.
  # deparse1() (base R, since R 4.0) always returns a single string, so the
  # prefix is never recycled. See GitHub issue #418.
  header <- c(
    paste0(".repro_docker_pkgs <- ", deparse1(install_deps$package)),
    paste0(".repro_docker_srcs <- ", deparse1(install_deps$source)),
    paste0(".repro_docker_refs <- ", deparse1(install_deps$ref)),
    paste0(".repro_docker_vers <- ", deparse1(as.character(versions)))
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
  container_name <- .repro_docker_container_name()
  args <- c("run", "--rm", "--name", container_name,
           "--user", .repro_docker_uid,
           "--cap-drop", "ALL",
           "--security-opt", "no-new-privileges",
           "--pids-limit", "512",
           .repro_docker_resource_args(),
           "-v", paste0(sandbox_dir, ":/sandbox"),
           "-v", paste0(normalizePath(lib_dir, mustWork = FALSE), ":/rlib"),
           image, "Rscript", "/sandbox/install.R")

  out_file <- tempfile(fileext = ".out")
  res <- tryCatch(
    .repro_docker_run_install("docker", args, timeout, out_file,
                              file.path(sandbox_dir, ".install_status"), install_deps, versions),
    error = function(e) NULL)

  # Stopping the `docker run` CLI at the timeout does not stop the container
  # that CLI started (confirmed directly: a container survives its own
  # `docker run` wrapper being killed on timeout, still consuming CPU
  # indefinitely afterwards -- see GitHub issue #417). `--name` above makes the
  # container reachable by name, so any non-clean outcome here gets a stop
  # attempt; a container that already exited on its own before this runs is
  # simply a harmless no-op (.repro_docker_stop() swallows the "no such
  # container" error).
  if (is.null(res) || isTRUE(res$timeout)) .repro_docker_stop(container_name)

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
    if (isTRUE(res$timeout)) paste0("docker install timed out after ", round(timeout), "s")
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
  out$version_recorded <- as.character(versions)[match(out$package, install_deps$package)]
  if (!"version_installed" %in% names(out)) out$version_installed <- NA_character_
  if (!"version_note" %in% names(out)) out$version_note <- ""
  out$version_note[is.na(out$version_note)] <- ""
  out$via_archive <- if ("via_archive" %in% names(out)) out$via_archive %in% TRUE else rep(FALSE, nrow(out))
  # Classified on the HOST, not inside the container script: the container has
  # no access to this package's own R functions (it runs install.R as a
  # standalone Rscript with no library() of this package available), so each
  # row's message is categorised here once results are back, same classifier
  # repro_install_deps() uses.
  out$category <- ifelse(out$installed, NA_character_,
                         vapply(out$message, .repro_classify_install_message,
                                character(1)))
  out
}

# Run the install container and show what it is doing. The container writes the
# number of the package it has started to `status_file`, on a mounted folder;
# this reads it every second, and shows the package, the count and the time so far
# (see .repro_install_what()). Returns a list `status`, `timeout`. A `docker run`
# in the foreground showed nothing until the whole install had ended.
.repro_docker_run_install <- function(command, args, timeout, out_file, status_file,
                                      install_deps, versions) {
  n <- nrow(install_deps)
  bar <- pb(NA, "(:spin) :what")
  on.exit(bar$terminate(), add = TRUE)
  started <- Sys.time()
  p <- processx::process$new(command, args, stdout = out_file, stderr = "2>&1")
  on.exit(if (p$is_alive()) p$kill(), add = TRUE)
  current <- 0L
  repeat {
    p$wait(1000)
    i <- suppressWarnings(tryCatch(as.integer(readLines(status_file, n = 1, warn = FALSE)),
                                   error = function(e) NA_integer_))
    if (length(i) == 1 && !is.na(i) && i >= 1L && i <= n) {
      what <- .repro_install_what(i, n, install_deps$package[i], versions[i], started)
      if (i != current) message("[repro/docker]   ", what)
      bar$tick(max(0L, i - current), list(what = what))
      current <- i
    }
    if (!p$is_alive()) return(list(status = p$get_exit_status(), timeout = FALSE))
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) >= timeout)
      return(list(status = NA_integer_, timeout = TRUE))
  }
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
  # stato_type_column() reads the vocabulary from its own surroundings, which
  # deparse() leaves behind, so the vocabulary is written out first; without
  # it every call to it failed, and aggregate()/tapply() results were never
  # captured in a container (found with Lakens/tue_datamanager_check#5, item 4).
  tbl_lines <- deparse(environment(helpers$stato_type_column)$stato_tables)
  out <- c(paste0("stato_tables <- ", tbl_lines[1]), tbl_lines[-1])
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
#' code cannot reach the network (unless `net` is given) or touch anything
#' outside the mounted sandbox and library, even deliberately. See this file's header comment for
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
#' @param deadline the time ([Sys.time()] plus the time for all scripts
#'   together) after which no script is started, recorded as
#'   `skipped_time_limit`; a running script is stopped then. `NULL` (the
#'   default) sets no such limit.
#' @param net `NULL` (the default) runs with the network off. With the limits from
#'   `.repro_download_limits()`, scripts can use the internet, and a script is
#'   stopped (outcome `download_too_large`) when the scripts together download
#'   more than the total limit, or a file larger than the limit per file appears.
#' @export
repro_run_scripts_docker <- function(run_tbl, order, sandbox_root, lib_dir = NULL,
                                     image = "rocker/r-ver:latest", timeout = 600,
                                     skip = character(0), parses = NULL,
                                     failed_deps = character(0), deadline = NULL,
                                     net = NULL) {
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
  run_started <- Sys.time()

  capture_preamble <- .repro_docker_capture_preamble()

  rows <- lapply(ordered_names, function(fn) {
    pb_run$tick(1, list(what = sprintf("Running %s (%s so far)", fn, .repro_duration(
      as.numeric(difftime(Sys.time(), run_started, units = "secs"))))))
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
    # Once the time for all scripts together has run out, a script is not
    # started; a script that is started stops when that time runs out
    # (Lakens/tue_datamanager_check#4, item 6)
    left <- .repro_time_left(deadline)
    if (left <= 0)
      return(no_lines(data.frame(file_name = fn, outcome = "skipped_time_limit",
                        error = "not started: the time for all scripts together had run out",
                        error_type = NA_character_, undefined_var = NA_character_,
                        stdout = "", stderr = "", elapsed = 0)))
    timeout <- min(timeout, ceiling(left))

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
    # The container is read-only apart from /tmp, its home folder too, so a
    # package that keeps files in the user's cache, configuration or data
    # folder stopped: renv::restore() with "failed to create directory at
    # path '/home/rcheck/.cache/R/renv'" (#523, item 4). Those folders are
    # put in /tmp. rmarkdown::render() looks for pandoc, which the image has
    # only inside Quarto (in bin/tools/<arch>/ on Linux, see
    # .report_pandoc_dir()), and stopped with "pandoc version 2.8 or higher
    # is required and was not found" (#523, item 5); RSTUDIO_PANDOC points
    # it there.
    env_setup <- c(
      '  Sys.setenv(XDG_CACHE_HOME = "/tmp/.cache", XDG_CONFIG_HOME = "/tmp/.config",',
      '             XDG_DATA_HOME = "/tmp/.local/share", R_USER_CACHE_DIR = "/tmp/.cache",',
      '             R_USER_CONFIG_DIR = "/tmp/.config", R_USER_DATA_DIR = "/tmp/.local/share")',
      '  quarto <- Sys.which("quarto")',
      '  if (nzchar(quarto) && !nzchar(Sys.getenv("RSTUDIO_PANDOC"))) {',
      '    tools <- file.path(dirname(normalizePath(quarto)), "tools")',
      '    pandoc <- c(file.path(tools, "pandoc"), Sys.glob(file.path(tools, "*", "pandoc")))',
      '    pandoc <- pandoc[file.exists(pandoc)]',
      '    if (length(pandoc)) Sys.setenv(RSTUDIO_PANDOC = dirname(pandoc[1]))',
      '  }',
      # as in .r_capture_runner(): what a person\'s computer has and a check does not
      '  options(knitr.graphics.error = FALSE)',
      '  assign("View", function(...) invisible(NULL), envir = globalenv())',
      '  assign("windowsFonts", function(...) invisible(NULL), envir = globalenv())',
      '  assign("windowsFont", function(...) "sans", envir = globalenv())',
      '  for (ask in c("file.choose", "choose.files", "choose.dir", "tk_choose.files", "tk_choose.dir"))',
      '    assign(ask, function(...) stop("file choice cancelled", call. = FALSE), envir = globalenv())'
    )
    wrapper_lines <- c(
      capture_preamble,
      '.repro_docker_run <- function() {',
      env_setup,
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

    # The working directory is the script's run_dir (see repro_write_scripts()),
    # which is the sandbox root unless the package layout chose a folder below it.
    container_name <- .repro_docker_container_name()
    args <- c("run", "--rm", "--name", container_name,
             .repro_docker_network_args(net), "--read-only",
             "--tmpfs", "/tmp",
             "--user", .repro_docker_uid,
             "--cap-drop", "ALL",
             "--security-opt", "no-new-privileges",
             "--pids-limit", "512",
             .repro_docker_resource_args(),
             "-w", .repro_docker_workdir(row$run_dir, sandbox_root),
             "-v", paste0(sandbox_root, ":/sandbox"))
    if (!is.null(lib_dir_norm))
      args <- c(args, "-v", paste0(lib_dir_norm, ":/rlib:ro"))
    args <- c(args, image, .repro_docker_counted_cmd(c("Rscript", container_wrapper), net))

    out_file <- tempfile(fileext = ".out")
    err_file <- tempfile(fileext = ".err")
    t0 <- Sys.time()
    watched <- .repro_docker_run_watched(args, container_name, timeout, out_file, err_file,
                                         sandbox_root, net)
    res <- watched$res
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    # is_timeout also drives error_type/outcome further below (moved up from
    # there, unchanged) -- computed once and reused here as well, for the
    # container-stop call: a timed-out `docker run` CLI does NOT stop the
    # container it started (confirmed directly, see .repro_docker_stop()'s
    # own comment) -- a real corpus run left a container running a
    # ~490-combination caret grid search for 80+ minutes past its own
    # recorded timeout, undetected, burning CPU that later papers'/scripts'
    # containers were competing for (GitHub issue #417). `--name` above
    # makes it reachable by either of processx's two timeout shapes (a
    # thrown condition, or a normal return with $timeout == TRUE); a
    # container that already exited cleanly makes the stop call a harmless
    # no-op.
    is_timeout <- (inherits(res, "condition") && isTRUE(attr(res, "timeout") %||%
      grepl("timed? ?out", conditionMessage(res), ignore.case = TRUE))) ||
      (is.list(res) && isTRUE(res$timeout))
    if (is_timeout) .repro_docker_stop(container_name)

    cap_file_host <- file.path(sandbox_root, ".capture.rds")
    captures <- if (file.exists(cap_file_host))
      tryCatch(readRDS(cap_file_host), error = function(e) NULL) else NULL
    unlink(cap_file_host)

    read_cap <- function(f) if (file.exists(f))
      paste(readLines(f, warn = FALSE), collapse = "\n") else ""
    so <- read_cap(out_file); se <- read_cap(err_file)
    unlink(c(out_file, err_file))

    message("[repro/docker]   <- '", fn, "' done in ", round(elapsed, 1), "s")

    # stopped for what it downloaded, with the internet on
    if (!is.na(watched$stopped))
      return(data.frame(file_name = fn, outcome = "download_too_large", error = watched$stopped,
                        error_type = NA_character_, undefined_var = NA_character_,
                        stdout = so, stderr = se, elapsed = elapsed) |>
               dplyr::mutate(script_lines = list(exec_lines), captures = list(captures)))

    # is_timeout was already computed above (right after `res`, so the
    # container-stop call could reuse it too) -- not recomputed here.
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
                        error = .repro_timeout_text(timeout, watched),
                        error_type = "timeout", undefined_var = NA_character_,
                        stdout = so, stderr = se, elapsed = elapsed) |>
               dplyr::mutate(script_lines = list(exec_lines), captures = list(captures)))
    }
    if (res$status != 0) {
      # See repro_run_scripts()'s twin of this block (R/reproducibility_check.R)
      # for why a missing FUNCTION ("could not find function ...") is matched
      # alongside a missing variable ("object ... not found") into the same
      # undefined_var column.
      undef_pat <- "object ['\"]([^'\"]+)['\"] not found"
      fn_pat    <- "could not find function ['\"]([^'\"]+)['\"]"
      undef_src <- if (grepl(undef_pat, se)) se else
        if (grepl(fn_pat, se)) se else NA_character_
      undef_var <- if (!is.na(undef_src) && grepl(undef_pat, undef_src))
        sub(paste0(".*", undef_pat, ".*"), "\\1",
            regmatches(undef_src, regexpr(undef_pat, undef_src)))
        else if (!is.na(undef_src) && grepl(fn_pat, undef_src))
        sub(paste0(".*", fn_pat, ".*"), "\\1",
            regmatches(undef_src, regexpr(fn_pat, undef_src)))
        else NA_character_

      # curly quotes too: in a UTF-8 locale R writes them around the name,
      # which the ASCII-only quotes missed, so the script was "errored", not
      # dependency_unavailable (U132)
      nopkg_pat <- "there is no package called ['\"\u2018]([^'\"\u2019]+)['\"\u2019]"
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
