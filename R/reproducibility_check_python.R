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
  base_pkgs <- c(.repro_py_stdlib_modules(), .repro_py2_modules())
  is_base <- pkgs %in% base_pkgs
  # The import name is not always the name pip installs (`import sklearn`
  # comes from `scikit-learn`), so the pip name is what is reported, installed
  # and matched against the manifest's pins.
  pkgs[!is_base] <- .repro_py_pip_name(pkgs[!is_base])

  src <- rep("pypi", length(pkgs))
  ref <- rep(NA_character_, length(pkgs))
  src[is_base] <- "base"
  m <- match(tolower(pkgs), pins$package)
  has_pin <- !is.na(m) & !is_base
  ref[has_pin] <- pins$requirement[m[has_pin]]

  out <- data.frame(package = pkgs, source = src, ref = ref, base = is_base)
  out[!duplicated(out$package), , drop = FALSE]
}

# Import names whose pip package has a different name. Python imports are
# case-sensitive, so the keys are matched exactly. A name not listed here is
# installed under its import name, as before.
.repro_py_pip_names <- function() {
  c(
    sklearn = "scikit-learn", skimage = "scikit-image",
    cv2 = "opencv-python", PIL = "Pillow", yaml = "PyYAML",
    bs4 = "beautifulsoup4", dateutil = "python-dateutil", Bio = "biopython",
    docx = "python-docx", pptx = "python-pptx", umap = "umap-learn",
    mpl_toolkits = "matplotlib", fitz = "PyMuPDF", Crypto = "pycryptodome",
    serial = "pyserial", dotenv = "python-dotenv", community = "python-louvain",
    wx = "wxPython", zmq = "pyzmq", attr = "attrs", jwt = "PyJWT",
    OpenSSL = "pyOpenSSL",
    pywt = "PyWavelets", ruamel = "ruamel.yaml", osgeo = "GDAL", pkg_resources = "setuptools",
    pylab = "matplotlib", skbio = "scikit-bio", gi = "PyGObject", git = "GitPython",
    MySQLdb = "mysqlclient", psycopg2 = "psycopg2-binary", argon2 = "argon2-cffi",
    elftools = "pyelftools", paho = "paho-mqtt", past = "future", vcf = "PyVCF3",
    rapidjson = "python-rapidjson", gitlab = "python-gitlab", primer3 = "primer3-py",
    hydra = "hydra-core", skbuild = "scikit-build", pytorch_grad_cam = "grad-cam",
    rest_framework = "djangorestframework", rest_framework_api_key = "djangorestframework-api-key",
    corsheaders = "django-cors-headers", django_filters = "django-filter",
    allauth = "django-allauth", dirtyfields = "django-dirtyfields", silk = "django-silk",
    crum = "django-crum", django_auth_ldap = "django-auth-ldap",
    logstash_async = "python-logstash-async"
  )
}

# Modules of Python 2's standard library, which have other names in Python 3 and
# cannot be installed: importing one shows that the code is Python 2 (#568, item 13).
.repro_py2_modules <- function() {
  c("cPickle", "cStringIO", "StringIO", "Queue", "BaseHTTPServer", "SocketServer",
    "SimpleHTTPServer", "CGIHTTPServer", "Tkinter", "tkFileDialog", "tkMessageBox",
    "ttk", "urlparse", "urllib2", "httplib", "ConfigParser", "commands", "HTMLParser",
    "cookielib", "exceptions", "__builtin__", "sets", "thread", "dummy_thread", "anydbm",
    "dumbdbm", "gdbm", "whichdb", "copy_reg", "repr", "xmlrpclib", "htmlentitydefs")
}

# The pip name for each import name (only the top-level module counts, so
# "sklearn.linear_model" gives "scikit-learn").
.repro_py_pip_name <- function(module) {
  top <- sub("\\..*$", "", module)
  map <- .repro_py_pip_names()
  ifelse(top %in% names(map), unname(map[top]), top)
}

# The Python interpreter to run: `python_bin` when it is on the path and
# works, else `python3` (the only name on many macOS and Linux machines).
# "Works" is checked by running it, because on Windows `python3` can be a
# placeholder that only opens the Microsoft Store. Returns "" when neither
# works.
.repro_find_python <- function(python_bin = "python") {
  # Running a candidate needs processx (in Suggests); without it every
  # candidate would fail and look like a missing interpreter -- callers say
  # which of the two it is (see .repro_python_missing_reason()).
  if (!requireNamespace("processx", quietly = TRUE)) return("")
  for (cand in unique(c(python_bin, "python3"))) {
    bin <- Sys.which(cand)
    if (!nzchar(bin)) next
    ok <- tryCatch(
      identical(processx::run(bin, c("-c", "import sys"),
                              error_on_status = FALSE, timeout = 30)$status, 0L),
      error = function(e) FALSE)
    if (ok) return(unname(bin))
  }
  ""
}

# Why no Python interpreter can be used, for the report: the processx package
# is missing (it runs Python), or neither `python` nor `python3` works.
.repro_python_missing_reason <- function() {
  if (!requireNamespace("processx", quietly = TRUE))
    "the 'processx' package, which metacheck needs to run Python, is not installed"
  else "no Python interpreter (`python` or `python3`) was found on this computer"
}

# Check whether Python files parse: the Python sibling of code_check's R parse
# check. Each file's text is parsed with Python's own ast.parse(), which reads
# the code without running it and writes no files (`python -m py_compile`
# would write a __pycache__ folder). Returns a named logical vector (by file),
# FALSE for a file with a syntax error; every file is TRUE when no working
# Python interpreter was found, since nothing could be checked. When a file fails
# because it is Python 2 code, attribute "python2" marks which (see
# .repro_py2_error() for how Python 3 words that).
.repro_py_parses <- function(code_text_list, python_bin = "python") {
  out <- stats::setNames(rep(TRUE, length(code_text_list)), names(code_text_list))
  if (!length(code_text_list) || !requireNamespace("processx", quietly = TRUE))
    return(out)
  bin <- .repro_find_python(python_bin)
  if (!nzchar(bin)) return(out)

  # One Python process for all files (starting Python once per file is slow
  # for a repository with many scripts): it prints one line per file, "1"
  # when it parses, "2" when it does not because it is Python 2, and "0" when
  # it does not for another reason.
  tfs <- vapply(seq_along(code_text_list), function(k) {
    tf <- tempfile(fileext = ".py")
    writeLines(enc2utf8(code_text_list[[k]]), tf, useBytes = TRUE)
    tf
  }, character(1))
  # The paths go in a list file, not on the command line, which has a length
  # limit on Windows.
  list_file <- tempfile(fileext = ".txt")
  writeLines(enc2utf8(tfs), list_file, useBytes = TRUE)
  on.exit(unlink(c(tfs, list_file)), add = TRUE)
  script <- paste(
    "import ast, sys",
    "for f in open(sys.argv[1], encoding='utf-8').read().splitlines():",
    "    try:",
    "        ast.parse(open(f, encoding='utf-8', errors='replace').read())",
    "        print(1)",
    "    except Exception as e:",
    "        words = ('Missing parentheses in call to', 'multiple exception types must be parenthesized')",
    "        print(2 if isinstance(e, TabError) or any(w in str(e) for w in words) else 0)",
    sep = "\n")
  res <- tryCatch(
    processx::run(bin, c("-c", script, list_file), error_on_status = FALSE,
                  timeout = 60 + 2 * length(tfs)),
    error = function(e) NULL)
  ok <- if (!is.null(res)) trimws(strsplit(res$stdout, "\n", fixed = TRUE)[[1]])
  # Anything unexpected (a timeout, a crash, missing lines) leaves every file
  # counted as parsing, the same as having no interpreter.
  if (length(ok) == length(tfs) && all(ok %in% c("0", "1", "2"))) {
    out[] <- ok == "1"
    if (any(ok == "2")) attr(out, "python2") <- ok == "2"
  }
  out
}

# Whether a run's error output shows Python 2 code: a print or exec statement, the
# form `except E, e`, or tabs mixed with spaces, which Python 2 accepted. The image
# only has Python 3, and converting the code could change results without a word
# (`/` between whole numbers divides differently), so these are only named.
.repro_py2_error <- function(txt) {
  grepl(paste0("SyntaxError: (Missing parentheses in call to|multiple exception types must be parenthesized)",
               "|TabError:|No module named '(", paste(.repro_py2_modules(), collapse = "|"), ")'"),
        txt %||% "", perl = TRUE)
}

# The folders Python must look in to find the repository's own modules that a script
# imports. Python only looks beside the script, so `from maad import sound` stopped
# with "No module named 'maad'" while maad/ is in the repository: 365 scripts in 23
# papers (#568, item 10). For each imported name that is no standard module, the
# folder holding a file or folder of that name, nearest to the script first. A folder
# is left out when it would hide a standard module the script uses, such as one with
# its own json.py, since these folders come before the standard library. `files` are
# the paths in the sandbox from its top; returns folders from the top.
.repro_py_module_folders <- function(code_text, script_path, files) {
  txt <- code_text[!grepl("^\\s*#", code_text)]
  m1 <- regmatches(txt, regexec("^\\s*import\\s+([A-Za-z0-9_., ]+)", txt))
  m2 <- regmatches(txt, regexec("^\\s*from\\s+([A-Za-z0-9_]+)[A-Za-z0-9_.]*\\s+import\\s", txt))
  names1 <- unlist(lapply(m1[lengths(m1) > 1], function(m) trimws(sub("\\s+as\\s+.*$", "", strsplit(m[[2]], ",")[[1]]))))
  names2 <- vapply(m2[lengths(m2) > 1], `[`, character(1), 2)
  tops <- unique(sub("\\..*$", "", c(names1, names2)))
  stdlib <- .repro_py_stdlib_modules()
  own <- setdiff(tops, stdlib)
  if (!length(own) || !length(files)) return(character(0))
  files <- gsub("\\\\", "/", files)
  script_dir <- dirname(gsub("\\\\", "/", script_path))
  depth_shared <- function(a, b) {
    x <- strsplit(a, "/", fixed = TRUE)[[1]]; y <- strsplit(b, "/", fixed = TRUE)[[1]]
    n <- min(length(x), length(y))
    if (n == 0) return(0L)
    same <- x[seq_len(n)] == y[seq_len(n)]
    if (all(same)) n else which(!same)[1] - 1L
  }
  used_std <- intersect(tops, stdlib)
  hides <- function(dir) {
    prefix <- if (dir == ".") "" else paste0(dir, "/")
    any(paste0(prefix, used_std, ".py") %in% files) ||
      any(vapply(used_std, function(s) any(startsWith(files, paste0(prefix, s, "/"))), logical(1)))
  }
  out <- character(0)
  for (nm in own) {
    hit <- grep(paste0("(^|/)", nm, "(\\.py$|/)"), files, value = TRUE)
    if (!length(hit)) next
    # the folder that holds the module, not one further down
    parents <- unique(vapply(hit, function(f) {
      parts <- strsplit(f, "/", fixed = TRUE)[[1]]
      k <- which(parts == nm | parts == paste0(nm, ".py"))[1]
      if (k <= 1) "." else paste(parts[seq_len(k - 1)], collapse = "/")
    }, character(1), USE.NAMES = FALSE))
    parents <- parents[order(-vapply(parents, depth_shared, integer(1), b = script_dir))]
    parents <- parents[!vapply(parents, hides, logical(1))]
    if (length(parents) && parents[1] != script_dir) out <- c(out, parents[1])
  }
  unique(out)
}

# A stand-in for get_ipython(), which only exists inside IPython: without it a
# notebook's `if "google.colab" in str(get_ipython()):` stops with a NameError. Its
# methods (run_line_magic, system) do nothing, as the commands are left out too.
# Placed after leading comments (an encoding line must stay on top) and after
# `from __future__` imports (which must come first).
.repro_py_get_ipython <- function(txt) {
  if (!any(grepl("\\bget_ipython\\s*\\(", txt)) || any(grepl("\\bget_ipython\\s*=|def\\s+get_ipython\\b", txt)))
    return(txt)
  stub <- paste0('get_ipython = lambda: type("ipython", (), {"__getattr__": lambda self, name: ',
                 '(lambda *a, **k: None)})()  # [reproducibility_check added]')
  future <- which(grepl("^\\s*from\\s+__future__\\s+import", txt))
  comments <- cumprod(grepl("^\\s*(#.*)?$", txt))
  append(txt, stub, after = if (length(future)) max(future) else sum(comments))
}

#' Parse a requirements.txt / pyproject.toml dependency block into per-package pins
#'
#' A `requirements.txt` line is `package`, `package==1.2.3`, `package>=1.2`,
#' a VCS/URL install (`git+https://...#egg=package`, `package @ https://...`),
#' or a comment/blank line (skipped, along with `-r other.txt`/`-e .`/`--...`
#' option lines, which name no installable package at all); an inline comment
#' (`numpy==1.26  # pinned`) and per-requirement options (`--hash=...`) are
#' dropped. In a `pyproject.toml` only the dependency declarations are read:
#' the strings of `[project]`'s `dependencies = [...]` array, and the
#' `name = "spec"` / `name = { version = "spec", ... }` lines of
#' `[tool.poetry.dependencies]` (not `python`). Every other quoted string of
#' the file (`name = "analysis"`, the version, the description) was read as a
#' requirement before, and a file whose strings did not all start like a
#' package name stopped the function (U236). Poetry's `^` and `~` versions are
#' written as pip ranges (`^1.26` -> `>=1.26,<2.0`, `~1.11` -> `>=1.11,<1.12`),
#' a bare version as `==`, and `*` as no version, since pip refuses Poetry's
#' own syntax.
#'
#' @param manifest_text character vector of the manifest file's lines, or NULL
#'
#' @returns a data frame with `package` (lowercased, as PyPI names are
#'   case-insensitive) and `requirement` (the requirement text, for
#'   `repro_install_deps_py()`/`repro_install_deps_py_docker()` to pass to
#'   `pip install` verbatim). Empty frame when `manifest_text` is NULL/empty.
#' @keywords internal
.repro_py_manifest_pins <- function(manifest_text) {
  empty <- data.frame(package = character(0), requirement = character(0))
  if (is.null(manifest_text) || length(manifest_text) == 0) return(empty)

  lines <- trimws(manifest_text)
  lines <- lines[nzchar(lines) & !grepl("^#", lines)]
  is_toml <- any(grepl("^\\[(project|tool\\.poetry)", lines))
  reqs <- if (is_toml) .py_toml_requirements(lines) else {
    # requirements.txt: drop option lines (-r/-e/--...), then each line's
    # inline comment and per-requirement options (" --hash=...")
    r <- lines[!grepl("^(-r|-e|--)", lines)]
    r <- sub("\\s+#.*$", "", r)
    trimws(sub("\\s+--\\S.*$", "", r))
  }
  reqs <- reqs[nzchar(reqs)]
  if (length(reqs) == 0) return(empty)

  # The package name is the leading identifier before any version
  # specifier (==, >=, <=, ~=, !=, <, >), extras (`pkg[extra]`), or an
  # `@`/`#egg=`-style VCS/URL marker. regexpr() keeps one entry per
  # requirement (regmatches() dropped non-matches, which misaligned the two
  # columns and stopped data.frame()).
  name_pat <- "^([A-Za-z0-9][A-Za-z0-9._-]*)"
  hit <- regexpr(name_pat, reqs, perl = TRUE)
  ok <- hit > 0
  if (!any(ok)) return(empty)
  name <- substr(reqs[ok], 1, attr(hit, "match.length")[ok])
  data.frame(package = tolower(name), requirement = reqs[ok]) |> unique()
}

# The requirement strings a pyproject.toml declares (see
# .repro_py_manifest_pins()): [project] dependencies and the Poetry
# dependency table, read section by section from `lines` (trimmed, without
# blank or comment lines).
.py_toml_requirements <- function(lines) {
  out <- character(0)
  section <- ""
  in_array <- FALSE
  unquote <- function(x) {
    m <- regmatches(x, gregexpr("(['\"])((?:[^'\"\\\\]|\\\\.)*)\\1", x, perl = TRUE))[[1]]
    gsub("^['\"]|['\"]$", "", m)
  }
  for (ln in lines) {
    if (in_array) {
      out <- c(out, unquote(sub("\\s+#.*$", "", ln)))
      if (grepl("\\]", ln)) in_array <- FALSE
      next
    }
    if (grepl("^\\[", ln)) {
      section <- sub("^\\[+\\s*([^]]*?)\\s*\\]+.*$", "\\1", ln)
      next
    }
    if (section == "project" && grepl("^dependencies\\s*=", ln)) {
      rest <- sub("^dependencies\\s*=\\s*", "", ln)
      out <- c(out, unquote(rest))
      in_array <- !grepl("\\]", rest)
    } else if (grepl("^tool\\.poetry(\\.group\\.[^.]+)?\\.dependencies$", section)) {
      m <- regmatches(ln, regexec("^([A-Za-z0-9_.-]+)\\s*=\\s*(.*)$", ln, perl = TRUE))[[1]]
      if (length(m) != 3 || tolower(m[2]) == "python") next
      value <- m[3]
      spec <- if (grepl("^\\{", value)) {
        v <- regmatches(value, regexec("version\\s*=\\s*['\"]([^'\"]*)['\"]", value))[[1]]
        if (length(v) == 2) v[2] else ""
      } else unquote(value)[1]
      out <- c(out, paste0(m[2], .poetry_to_pip(spec %||% "")))
    }
  }
  trimws(out)
}

# A Poetry version constraint as a pip one: "^1.26" -> ">=1.26,<2.0",
# "^0.3" -> ">=0.3,<0.4", "~1.11" -> ">=1.11,<1.12", "1.2.3" -> "==1.2.3",
# "*" or "" -> "" (any version); pip operators (">=1.0") are kept.
.poetry_to_pip <- function(spec) {
  spec <- trimws(spec)
  if (is.na(spec) || !nzchar(spec) || spec == "*") return("")
  if (grepl("^[<>=!~]=|^[<>]", spec)) return(spec)
  op <- substr(spec, 1, 1)
  ver <- if (op %in% c("^", "~")) substring(spec, 2) else spec
  parts <- suppressWarnings(as.integer(strsplit(ver, ".", fixed = TRUE)[[1]]))
  if (!length(parts) || anyNA(parts)) return(if (op %in% c("^", "~")) "" else paste0("==", spec))
  if (op == "^") {
    # the first non-zero part may not change (only the last one given, if all zero)
    i <- which(parts != 0)[1]
    if (is.na(i)) i <- length(parts)
  } else if (op == "~") {
    i <- if (length(parts) >= 2) 2 else 1
  } else {
    return(paste0("==", ver))
  }
  upper <- parts[seq_len(i)]
  upper[i] <- upper[i] + 1L
  if (length(upper) < 2) upper <- c(upper, 0L)
  paste0(">=", ver, ",<", paste(upper, collapse = "."))
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

  bin <- .repro_find_python(python_bin)
  if (!nzchar(bin))
    stop("Python executable '", python_bin, "' (or 'python3') was not found on PATH.",
         call. = FALSE)
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)

  rows <- lapply(seq_len(nrow(install_deps)), function(i) {
    pkg <- install_deps$package[i]
    req <- if (!is.na(install_deps$ref[i]) && nzchar(install_deps$ref[i]))
      install_deps$ref[i] else pkg
    pip <- function(r) tryCatch(
      processx::run(bin, c("-m", "pip", "install", "--quiet", "--prefer-binary",
                          "--target", lib_dir, r),
                   error_on_status = FALSE, timeout = timeout),
      error = function(e) list(status = 1L, stderr = conditionMessage(e)))
    res <- pip(req)
    # a pinned version that does not install is replaced by the current one
    if (!identical(res$status, 0L) && !identical(req, pkg)) res <- pip(pkg)
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
#' @param deadline the time ([Sys.time()] plus the time for all scripts
#'   together) after which no script is started, recorded as
#'   `skipped_time_limit`; a running script is stopped then. `NULL` (the
#'   default) sets no such limit.
#' @export
repro_run_scripts_py <- function(run_tbl, order, lib_dir = NULL,
                                 python_bin = "python", timeout = 600,
                                 skip = character(0), parses = NULL,
                                 failed_deps = character(0), deadline = NULL) {
  empty_cols <- function() data.frame(
    file_name = character(0), outcome = character(0), error = character(0),
    error_type = character(0), undefined_var = character(0),
    stdout = character(0), stderr = character(0), elapsed = numeric(0)) |>
    dplyr::mutate(script_lines = list(), captures = list())
  if (is.null(run_tbl) || nrow(run_tbl) == 0) return(empty_cols())
  if (!requireNamespace("processx", quietly = TRUE))
    stop("the 'processx' package is required to execute code (execute = TRUE).",
         call. = FALSE)

  bin <- .repro_find_python(python_bin)
  if (!nzchar(bin))
    stop("Python executable '", python_bin, "' (or 'python3') was not found on PATH.",
         call. = FALSE)

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

    message("[repro/py]   -> running '", fn, "' (timeout ", timeout, "s) ...")
    # the repository's own modules first (see .repro_py_module_folders()); the
    # folders count from the sandbox's top, which `module_root` names
    own_modules <- if ("module_folders" %in% names(row) && !is.na(row$module_folders) && nzchar(row$module_folders))
      file.path(row$module_root, strsplit(row$module_folders, "\n", fixed = TRUE)[[1]]) else character(0)
    t0 <- Sys.time()
    res <- tryCatch(
      processx::run(bin, row$script_path, wd = row$run_dir,
                   env = c("current", PYTHONPATH = paste(c(own_modules, if (nzchar(pythonpath)) pythonpath),
                                                         collapse = .Platform$path.sep),
                           MPLBACKEND = "Agg", QT_QPA_PLATFORM = "offscreen"),
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
      dep_unavailable <- !is.na(nomod_var) && .repro_py_pip_name(nomod_var) %in% failed_deps

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
#' The Python sibling of [repro_defined_vars()], used by the module's report
#' to name the file that defines a missing name (a `NameError` --
#' `error_type == "undefined_variable"`, see [repro_run_scripts_py()]'s
#' classification -- usually means the script expects a name another script
#' in the same repository defines, rather than being a standalone program).
#' Unlike R, there is no re-run in a corrected order: every Python script runs
#' in its own `python` process, so a name another script defines is never
#' available to it, whatever the order. Scans each file for **top-level** variable
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
#' @keywords internal
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
