# Python-specific helpers for the reproducibility_check module, mirroring
# the R helpers in R/reproducibility_check.R (STATIC: repro_dependencies(),
# repro_rewrite_paths(), repro_file_io(), repro_run_order(),
# repro_write_scripts() — those four already accept a `lang` argument and
# are shared, not duplicated here; EXECUTION: repro_install_deps(),
# repro_run_scripts()). This file holds the pieces that have no shared,
# language-parametrised home because the underlying ecosystem concept is
# genuinely different: PyPI/pip has no CRAN-vs-Bioconductor-vs-base split
# (base packages here means "the Python standard library", not an
# R-installation query), dependency pins commonly live in a SEPARATE
# manifest file (requirements.txt/pyproject.toml) rather than only in the
# code's own import statements, and the install/run tooling is `pip`/
# `python`, not `install.packages()`/`callr`.
#
# Process-sandbox execution (repro_run_scripts_py(), this file) runs each
# script as a `python` subprocess via `processx::run()` — the same crash-
# only isolation caveat as repro_run_scripts()'s callr backend applies here
# too (see that function's own roxygen): this isolates a crash, NOT the
# filesystem or network. The Docker-backed twin
# (repro_run_scripts_py_docker(), R/reproducibility_check_python_docker.R)
# is the actual sandbox, mirroring reproducibility_check_docker.R's
# two-phase install/run design exactly.
#
# Per the module's own execution contract (confirmed with the project
# before this was built): a Python script runs WHOLE, as one subprocess,
# capturing only its stdout/stderr/exit status — there is no Python
# analogue to R/r-capture.R's per-statement "capture the statistical
# object, not just what it printed" scheme built here. A richer
# Python-side capture (e.g. via `ast`, executing top-level statements one
# at a time) is future work, not attempted in this pass.

#' Collect the package dependencies a set of Python files declare
#'
#' The Python sibling of [repro_dependencies()]: walks each file's `import`/
#' `from ... import` statements via [code_library_names()] (lang = "Python"),
#' and additionally reads any `requirements.txt`/`pyproject.toml` the
#' repository supplies for version pins — static analysis of the code alone
#' cannot see those (a script that does `import numpy` says nothing about
#' which version), so when a manifest is present its pin is attached to the
#' matching imported package instead of being reported separately. A top-
#' level package name is tagged `base` (the Python standard library; a run
#' never needs to `pip install` these) or `pypi` — unlike R's CRAN/
#' Bioconductor/GitHub/URL split, this does not attempt to tell a PyPI
#' install apart from a `pip install git+...`/local-path install named in a
#' `requirements.txt` line: both are reported `source = "pypi"`, with `ref`
#' carrying the raw requirement line verbatim when it is not a bare
#' `package==version` pin, so the install step can still attempt it.
#'
#' @param code_text the code text for a single file (character vector), OR a
#'   list of such vectors (one per file) to pool across files
#' @param manifest_text optional character vector: the text of a
#'   `requirements.txt` (one requirement per line) or the `[project]`/
#'   `[tool.poetry]` dependency lines of a `pyproject.toml`, if the
#'   repository has one. `NULL` (default) when none was found — dependencies
#'   are then name-only, exactly like a code-only R scan.
#'
#' @returns a data frame with columns `package`, `source` (`pypi` or `base`),
#'   `ref` (the manifest's own pin/requirement text for that package, or NA
#'   when the package was seen only via `import`), and `base` (logical, TRUE
#'   for a standard-library module). One row per distinct package. Empty
#'   frame (same columns) when none are found.
#' @export
#'
#' @examples
#' code_text <- c("import numpy as np", "from sklearn.linear_model import LinearRegression")
#' repro_dependencies_py(code_text)
repro_dependencies_py <- function(code_text, manifest_text = NULL) {
  empty <- data.frame(package = character(0), source = character(0),
                      ref = character(0), base = logical(0))
  if (is.null(code_text)) return(empty)

  if (is.list(code_text)) {
    parts <- lapply(code_text, repro_dependencies_py, manifest_text = manifest_text)
    parts <- Filter(function(d) is.data.frame(d) && nrow(d) > 0, parts)
    if (length(parts) == 0) return(empty)
    out <- dplyr::bind_rows(parts)
    # A manifest pin (non-NA ref) beats a bare import-only row for the same
    # package, same "keep the row that carries the real source" rule
    # repro_dependencies() applies to a github-vs-bare duplicate.
    out <- out[order(out$package, is.na(out$ref)), ]
    return(out[!duplicated(out$package), , drop = FALSE])
  }

  pkgs <- unique(code_library_names(code_text, "Python")$package)
  if (length(pkgs) == 0) return(empty)

  pins <- .repro_py_manifest_pins(manifest_text)
  base_pkgs <- .repro_py_stdlib_modules()

  src <- rep("pypi", length(pkgs))
  ref <- rep(NA_character_, length(pkgs))
  src[pkgs %in% base_pkgs] <- "base"
  m <- match(pkgs, pins$package)
  has_pin <- !is.na(m) & !pkgs %in% base_pkgs
  ref[has_pin] <- pins$requirement[m[has_pin]]

  data.frame(package = pkgs, source = src, ref = ref, base = pkgs %in% base_pkgs)
}

#' Parse a requirements.txt / pyproject.toml dependency block into per-package pins
#'
#' A `requirements.txt` line is `package`, `package==1.2.3`, `package>=1.2`,
#' a VCS/URL install (`git+https://...#egg=package`, `package @ https://...`),
#' or a comment/blank line (skipped, along with `-r other.txt`/`-e .`/`--...`
#' option lines, which name no installable package at all). A
#' `pyproject.toml`'s dependency ARRAY entries (`dependencies = [...]` or
#' Poetry's `[tool.poetry.dependencies]` table) are each one requirement
#' STRING in the same `package==1.2.3`-style shape, just TOML-quoted — this
#' only reads the quoted strings themselves, not the surrounding TOML
#' structure, so it works for either file without a TOML parser dependency.
#'
#' @param manifest_text character vector of the manifest file's lines, or NULL
#'
#' @returns a data frame with `package` (lowercased, as PyPI names are
#'   case-insensitive) and `requirement` (the raw requirement text, for
#'   `repro_install_deps_py()`/`repro_install_deps_py_docker()` to pass to
#'   `pip install` verbatim). Empty frame when `manifest_text` is NULL/empty.
#' @keywords internal
.repro_py_manifest_pins <- function(manifest_text) {
  empty <- data.frame(package = character(0), requirement = character(0))
  if (is.null(manifest_text) || length(manifest_text) == 0) return(empty)

  lines <- trimws(manifest_text)
  lines <- lines[nzchar(lines) & !grepl("^#", lines)]
  # pyproject.toml: pull the quoted requirement strings out of a
  # `dependencies = [...]` array or a Poetry table line (`numpy = "^1.26"` ->
  # treated as `numpy^1.26`, close enough for pip to still resolve a name
  # from — Poetry's own caret/tilde operators are not pip syntax, but the
  # bare package NAME, which is all repro_dependencies_py() actually keys
  # on, is still recovered correctly either way).
  is_toml <- any(grepl("^\\[tool\\.poetry|^dependencies\\s*=|^\\[project\\]", lines))
  if (is_toml) {
    quoted <- regmatches(lines, gregexpr("(['\"])((?:[^'\"\\\\]|\\\\.)*)\\1",
                                         lines, perl = TRUE))
    reqs <- unlist(quoted)
    reqs <- gsub("^['\"]|['\"]$", "", reqs)
    poetry_kv <- regmatches(lines, regexec(
      "^([A-Za-z0-9_.-]+)\\s*=\\s*[\"']([^\"']+)[\"']", lines, perl = TRUE))
    poetry_kv <- Filter(function(x) length(x) == 3, poetry_kv)
    poetry_reqs <- vapply(poetry_kv, function(x) paste0(x[2], x[3]), character(1))
    reqs <- unique(c(reqs[nzchar(reqs)], poetry_reqs))
  } else {
    # requirements.txt: drop option lines (-r/-e/--...) and VCS/URL installs'
    # leading flags; keep the rest as one requirement per (non-empty,
    # non-comment) line, already filtered above.
    reqs <- lines[!grepl("^(-r|-e|--)", lines)]
  }
  if (length(reqs) == 0) return(empty)

  # The package name is the leading identifier before any version
  # specifier (==, >=, <=, ~=, !=, <, >), extras (`pkg[extra]`), or an
  # `@`/`#egg=`-style VCS/URL marker.
  name_pat <- "^([A-Za-z0-9][A-Za-z0-9._-]*)"
  m <- regmatches(reqs, regexpr(name_pat, reqs, perl = TRUE))
  ok <- nzchar(m)
  data.frame(package = tolower(m[ok]), requirement = reqs[ok]) |> unique()
}

# The Python standard library's top-level module names (3.10+), analogous to
# .repro_base_packages() -- a run never needs to pip install these. Unlike
# R's base/recommended set (resolved LIVE from the running installation),
# Python's stdlib is not queryable from R without actually invoking a
# `python` interpreter (`sys.stdlib_module_names`, 3.10+ only) -- deferred to
# a static list here for the same reason repro_dependencies_py() never shells
# out to Python for STATIC analysis at all (it must work even when no Python
# interpreter is installed on the machine running metacheck itself). A module
# missing from this list is simply reported `source = "pypi"` and a
# `pip install` is attempted for it at run time, which fails harmlessly
# (recorded `dependency_unavailable`, not a crash) if it was in fact a stdlib
# name this list missed -- same "no worse than before" tolerance
# .repro_bioc_packages() documents for an unlisted Bioconductor package.
.repro_py_stdlib_modules <- function() {
  c(
    "__future__", "_thread", "abc", "aifc", "argparse", "array", "ast",
    "asyncio", "atexit", "base64", "bdb", "binascii", "bisect", "builtins",
    "bz2", "calendar", "cgi", "cgitb", "chunk", "cmath", "cmd", "code",
    "codecs", "codeop", "collections", "colorsys", "compileall",
    "concurrent", "configparser", "contextlib", "contextvars", "copy",
    "copyreg", "cProfile", "csv", "ctypes", "curses", "dataclasses",
    "datetime", "dbm", "decimal", "difflib", "dis", "distutils", "doctest",
    "email", "encodings", "ensurepip", "enum", "errno", "faulthandler",
    "fcntl", "filecmp", "fileinput", "fnmatch", "fractions", "ftplib",
    "functools", "gc", "getopt", "getpass", "gettext", "glob", "graphlib",
    "grp", "gzip", "hashlib", "heapq", "hmac", "html", "http", "idlelib",
    "imaplib", "imghdr", "imp", "importlib", "inspect", "io", "ipaddress",
    "itertools", "json", "keyword", "lib2to3", "linecache", "locale",
    "logging", "lzma", "mailbox", "mailcap", "marshal", "math", "mimetypes",
    "mmap", "modulefinder", "msilib", "msvcrt", "multiprocessing",
    "netrc", "nis", "nntplib", "numbers", "operator", "optparse", "os",
    "ossaudiodev", "pathlib", "pdb", "pickle", "pickletools", "pipes",
    "pkgutil", "platform", "plistlib", "poplib", "posix", "posixpath",
    "pprint", "profile", "pstats", "pty", "pwd", "py_compile", "pyclbr",
    "pydoc", "queue", "quopri", "random", "re", "readline", "reprlib",
    "resource", "rlcompleter", "runpy", "sched", "secrets", "select",
    "selectors", "shelve", "shlex", "shutil", "signal", "site", "smtpd",
    "smtplib", "sndhdr", "socket", "socketserver", "spwd", "sqlite3",
    "ssl", "stat", "statistics", "string", "stringprep", "struct",
    "subprocess", "sunau", "symtable", "sys", "sysconfig", "syslog",
    "tabnanny", "tarfile", "telnetlib", "tempfile", "termios", "test",
    "textwrap", "this", "threading", "time", "timeit", "tkinter", "token",
    "tokenize", "tomllib", "trace", "traceback", "tracemalloc", "tty",
    "turtle", "turtledemo", "types", "typing", "unicodedata", "unittest",
    "urllib", "uu", "uuid", "venv", "warnings", "wave", "weakref",
    "webbrowser", "winreg", "winsound", "wsgiref", "xdrlib", "xml",
    "xmlrpc", "zipapp", "zipfile", "zipimport", "zlib", "zoneinfo"
  )
}

#' Install a paper's declared Python dependencies into a throwaway environment
#'
#' The process-sandbox twin of [repro_install_deps()]: installs each
#' non-stdlib dependency via `pip install --target <lib_dir>`, so a run does
#' not touch the host's own site-packages. A manifest-pinned requirement
#' (`ref` non-NA — see [repro_dependencies_py()]) is installed by that exact
#' requirement text (honouring the pin); an import-only package (`ref` NA) is
#' installed by bare name, latest version. Installing runs a package's own
#' build/setup code, so this is part of the gated execute phase, same as its
#' R counterpart.
#'
#' @param install_deps the module's Python `install_deps` frame (`package`,
#'   `source`, `ref`), base/stdlib rows already excluded
#' @param lib_dir the throwaway install target (created if absent); passed to
#'   `pip install --target`
#' @param python_bin the `python` executable to invoke `-m pip` with
#'   (default `"python"`, resolved via `Sys.which()` at call time so a
#'   missing interpreter fails with a clear message rather than a cryptic
#'   `pip` error)
#' @param timeout per-`pip install` timeout in seconds (default 300)
#'
#' @returns a data frame with `package`, `source`, `installed` (logical),
#'   `message` (error text/pip stderr on failure, else ""), `via_archive`
#'   (always FALSE — pip has no CRAN-Archive-style fallback registry to retry
#'   against), and `category` (always NA — pip's own error text is not
#'   classified into categories the way [.repro_classify_install_message()]
#'   does for R's install.packages()/BiocManager error shapes) — same column
#'   SET as [repro_install_deps()] so callers do not need to branch on which
#'   language ran.
#' @export
repro_install_deps_py <- function(install_deps, lib_dir, python_bin = "python",
                                  timeout = 300) {
  empty <- data.frame(package = character(0), source = character(0),
                      installed = logical(0), message = character(0),
                      via_archive = logical(0), category = character(0))
  if (is.null(install_deps) || nrow(install_deps) == 0) return(empty)
  if (!requireNamespace("processx", quietly = TRUE))
    stop("the 'processx' package is required to install Python dependencies.",
         call. = FALSE)

  bin <- Sys.which(python_bin)
  if (!nzchar(bin))
    stop("Python executable '", python_bin, "' was not found on PATH.", call. = FALSE)
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)

  rows <- lapply(seq_len(nrow(install_deps)), function(i) {
    pkg <- install_deps$package[i]
    req <- if (!is.na(install_deps$ref[i]) && nzchar(install_deps$ref[i]))
      install_deps$ref[i] else pkg
    res <- tryCatch(
      processx::run(bin, c("-m", "pip", "install", "--quiet",
                          "--target", lib_dir, req),
                   error_on_status = FALSE, timeout = timeout),
      error = function(e) list(status = 1L, stderr = conditionMessage(e)))
    ok <- identical(res$status, 0L)
    data.frame(package = pkg, source = install_deps$source[i], installed = ok,
              message = if (ok) "" else trimws(res$stderr %||% ""),
              via_archive = FALSE, category = NA_character_)
  })
  dplyr::bind_rows(rows)
}

#' Run Python scripts, in order, each in an isolated subprocess
#'
#' The Python sibling of [repro_run_scripts()]: runs each script WHOLE, as a
#' `python <script>` subprocess via `processx::run()` (not `callr`, which is
#' R-specific), with the working directory set to the materialised sandbox.
#' Unlike [repro_run_scripts()]'s `callr`-based runner, there is no per-
#' statement capture of result objects here (see this file's header for why)
#' — only the whole script's stdout/stderr/exit status/elapsed time are
#' recorded, so `captures` is always `NULL` for every row (kept as a column,
#' not dropped, so the module's downstream code that expects the same shape
#' [repro_run_scripts()] returns does not need to branch by language).
#'
#' This isolates a CRASH, not the filesystem or network — the same caveat
#' [repro_run_scripts()]'s own roxygen states for its callr backend applies
#' identically here: the script can read/write/delete anywhere this process
#' can, and reach the network freely. Use `sandbox = "docker"`
#' ([repro_run_scripts_py_docker()]) for code you do not trust.
#'
#' @param run_tbl `repro_write_scripts()` output (`file_name`, `script_path`,
#'   `run_dir`)
#' @param order a vector of `file_name`s in the order to run
#' @param lib_dir throwaway `pip install --target` directory (or NULL) —
#'   prepended to `PYTHONPATH` so an installed dependency is importable
#' @param python_bin the `python` executable to run scripts with (default
#'   `"python"`)
#' @param timeout per-script timeout in seconds
#' @param skip character vector of `file_name`s to record as
#'   `skipped_missing_inputs` instead of running
#' @param parses named logical (by file_name); a file that will not parse
#'   (see `code_parse_py()`/the module's own syntax check) is recorded
#'   `not_parsed` and not run
#' @param failed_deps character vector of package names
#'   [repro_install_deps_py()] could not install
#'
#' @returns a data frame — same columns as [repro_run_scripts()]'s return:
#'   `file_name`, `outcome`, `error`, `error_type`, `undefined_var`,
#'   `stdout`, `stderr`, `elapsed`, `script_lines`, `captures` (always `NULL`
#'   per row — see above)
#' @export
repro_run_scripts_py <- function(run_tbl, order, lib_dir = NULL,
                                 python_bin = "python", timeout = 600,
                                 skip = character(0), parses = NULL,
                                 failed_deps = character(0)) {
  empty_cols <- function() data.frame(
    file_name = character(0), outcome = character(0), error = character(0),
    error_type = character(0), undefined_var = character(0),
    stdout = character(0), stderr = character(0), elapsed = numeric(0)) |>
    dplyr::mutate(script_lines = list(), captures = list())
  if (is.null(run_tbl) || nrow(run_tbl) == 0) return(empty_cols())
  if (!requireNamespace("processx", quietly = TRUE))
    stop("the 'processx' package is required to execute code (execute = TRUE).",
         call. = FALSE)

  bin <- Sys.which(python_bin)
  if (!nzchar(bin))
    stop("Python executable '", python_bin, "' was not found on PATH.", call. = FALSE)

  ordered_names <- c(order[order %in% run_tbl$file_name],
                     setdiff(run_tbl$file_name, order))

  env <- Sys.getenv("PYTHONPATH")
  pythonpath <- if (!is.null(lib_dir) && dir.exists(lib_dir))
    paste(c(lib_dir, if (nzchar(env)) env), collapse = .Platform$path.sep) else env

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

    message("[repro/py]   -> running '", fn, "' (timeout ", timeout, "s) ...")
    t0 <- Sys.time()
    res <- tryCatch(
      processx::run(bin, row$script_path, wd = row$run_dir,
                   env = c("current", PYTHONPATH = pythonpath),
                   error_on_status = FALSE, timeout = timeout),
      error = function(e) e)
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    if (inherits(res, "condition")) {
      is_timeout <- grepl("timed? ?out", conditionMessage(res), ignore.case = TRUE)
      msg <- conditionMessage(res)
      message("[repro/py]   <- '", fn, "' done in ", round(elapsed, 1), "s (",
              if (is_timeout) "timed out" else "condition/error", ")")
      return(no_lines(data.frame(
        file_name = fn, outcome = if (is_timeout) "timed_out" else "errored",
        error = msg, error_type = NA_character_, undefined_var = NA_character_,
        stdout = "", stderr = "", elapsed = elapsed)))
    }

    so <- trimws(res$stdout %||% "")
    se <- trimws(res$stderr %||% "")
    message("[repro/py]   <- '", fn, "' done in ", round(elapsed, 1), "s (",
            if (res$status == 0L) "ok" else "errored",
            "; stdout ", nchar(so), " chars, stderr ", nchar(se), " chars)")

    if (res$status != 0L) {
      # Python's own traceback ends with "<ExceptionType>: <message>" on the
      # LAST non-empty line -- the same "classify from the interpreter's own
      # final line" approach repro_run_scripts()'s R classification takes
      # against an R error's own condition message, just against Python's
      # traceback shape instead. `undefined_var` is populated only for a
      # NameError ("name 'x' is not defined") -- the module's corrective
      # re-run (R-only, see this file's header) does not act on it, but it
      # is still useful information for the report.
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

      # error_type is normalized to "undefined_variable" / "dependency_unavailable"
      # here (not the raw "NameError" string, and a new dependency_unavailable
      # check mirroring the docker backend) so the module's corrective re-run
      # logic can check ONE consistent string regardless of which backend ran
      # -- see repro_run_scripts_py_docker()'s identical classification for
      # the twin of this logic.
      nomod_pat <- "No module named ['\"]([^'\"]+)['\"]"
      nomod_src <- if (grepl(nomod_pat, se)) se else NA_character_
      nomod_var <- if (!is.na(nomod_src))
        sub(paste0(".*", nomod_pat, ".*"), "\\1",
            regmatches(nomod_src, regexpr(nomod_pat, nomod_src))) else NA_character_
      dep_unavailable <- !is.na(nomod_var) && nomod_var %in% failed_deps

      error_type <- if (dep_unavailable) "dependency_unavailable"
                    else if (!is.na(undefined_var)) "undefined_variable"
                    else error_type
      outcome <- if (dep_unavailable) "dependency_unavailable" else "errored"
      return(data.frame(file_name = fn, outcome = outcome, error = last_line,
                        error_type = error_type, undefined_var = undefined_var,
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

#' Find the names each Python script defines at top level
#'
#' The Python sibling of [repro_defined_vars()], for the module's own Python
#' corrective re-run (a `NameError` -- `error_type == "undefined_variable"`,
#' see [repro_run_scripts_py()]'s classification -- usually means the script
#' expects a name another script in the same repository defines, rather than
#' being a standalone program). Scans each file for **top-level** variable
#' assignments (`x = ...`, not `==`) and top-level `def`/`class` statements, so
#' a missing name can be matched to the file that would supply it. Only a
#' statement at the start of a (comment-free) line is taken, which
#' approximates "top level": an assignment/def indented inside a function body
#' or control-flow block is not a module-level name the next script would see
#' if it only imports or runs this one, and taking it would create false
#' ordering edges. Unlike [repro_defined_vars()], there is no `assign("x",
#' ...)`-style function-call form to also match -- Python has no equivalent
#' builtin that creates a module-level name by string.
#'
#' @param code_text_list a named list of Python code-text character vectors,
#'   one per file (names are the file_names)
#'
#' @returns a data frame with `file_name` and a list-column `defines`
#'   (character vector of names each file defines at top level -- variables,
#'   functions, and classes together, since a missing name can be any of the
#'   three).
#' @export
.repro_py_defined_vars <- function(code_text_list) {
  if (is.null(code_text_list) || length(code_text_list) == 0)
    return(data.frame(file_name = character(0)))
  fname <- names(code_text_list) %||% as.character(seq_along(code_text_list))

  # A top-level assignment: line start (no indentation => not inside a
  # block/function body), a valid Python identifier, then = (not ==, !=, <=,
  # >=). A top-level def/class: line start, the keyword, then the name.
  assign_pat <- "^([a-zA-Z_][a-zA-Z0-9_]*)\\s*=(?!=)"
  defclass_pat <- "^(?:def|class)\\s+([a-zA-Z_][a-zA-Z0-9_]*)"

  rows <- lapply(seq_along(code_text_list), function(k) {
    nc <- code_remove_comments(code_text_list[[k]], "Python")
    m1 <- regmatches(nc, regexpr(assign_pat, nc, perl = TRUE))
    v1 <- sub(paste0(assign_pat, ".*"), "\\1", m1, perl = TRUE)
    m2 <- regmatches(nc, regexpr(defclass_pat, nc, perl = TRUE))
    v2 <- sub(paste0(defclass_pat, ".*"), "\\1", m2, perl = TRUE)
    defines <- unique(c(v1[nzchar(v1)], v2[nzchar(v2)]))
    data.frame(file_name = fname[k], stringsAsFactors = FALSE) |>
      (\(d) { d$defines <- list(defines); d })()
  })
  dplyr::bind_rows(rows)
}
