# Helpers for the reproducibility_check module. This module answers "can the
# paper's code be run on its data?" in two phases.
#
# STATIC helpers (never run downloaded code): collect the dependencies a run
# would need, rewrite each script's file paths to the Psych-DS layout the release
# uses, work out the order the scripts must run in, and diagnose why a referenced
# input is unavailable. Built on the static analysis already in code_check.R
# (code_library_names, code_file_refs, code_parse_r) and the file→location plan
# from psychds_check / data_check.
#
# EXECUTION helpers (run only when the module is called with execute = TRUE):
# repro_materialize_layout / repro_write_scripts build a throwaway copy of the
# Psych-DS layout, repro_install_deps installs the declared packages into a
# throwaway library, and repro_run_scripts runs each script in an isolated
# subprocess (callr) with a per-script timeout, capturing its outcome and output.
# Running downloaded code is a deliberate, opt-in action; see those functions.

#' Collect the package dependencies a set of code files declare
#'
#' Walks each code file with [code_library_names()] and returns the distinct
#' packages the paper's code loads, tagged with how each was referenced and
#' whether it is installable from CRAN, from a named GitHub/URL source, or is a
#' base/recommended package already shipped with R (so a run need not install
#' it). This is the input list a run would install; it is deliberately
#' name-only, because static analysis cannot know which *version* was used —
#' running against the latest packages and observing a break is itself a
#' finding (it argues the authors should have pinned versions).
#'
#' GitHub/URL sources are read from the code text, not the package name: a call
#' `remotes::install_github("user/repo")` names the source `user/repo`, which is
#' matched back to the package by the repo's basename. When a script pins a ref
#' (`user/repo@v1.2`), the ref is kept.
#'
#' @param code_text the code text for a single file (character vector), OR a list
#'   of such vectors (one per file) to pool across files
#' @param lang the language (only R declares installable packages here)
#'
#' @returns a data frame with columns `package`, `source` (`cran`, `github`,
#'   `url`, or `base`), `ref` (the source path/ref for github/url, else NA), and
#'   `base` (logical, TRUE for a base/recommended package). One row per distinct
#'   package. Empty frame (same columns) when none are found.
#' @export
#'
#' @examples
#' code_text <- c(
#'   "library(dplyr)",
#'   "remotes::install_github('tidyverse/ggplot2@v3.4.0')",
#'   "x <- stats::sd(1:10)"
#' )
#' repro_dependencies(code_text)
repro_dependencies <- function(code_text, lang = "R") {
  empty <- data.frame(package = character(0), source = character(0),
                      ref = character(0), base = logical(0))
  if (is.null(code_text)) return(empty)

  # Pool a list of per-file texts into one search, keeping the union of names.
  if (is.list(code_text)) {
    parts <- lapply(code_text, repro_dependencies, lang = lang)
    parts <- Filter(function(d) is.data.frame(d) && nrow(d) > 0, parts)
    if (length(parts) == 0) return(empty)
    out <- dplyr::bind_rows(parts)
    # A package named both bare (library) and via a github source: keep the
    # github row (it carries the real install source), drop the duplicate.
    out <- out[order(out$package, match(out$source, c("github", "url", "cran", "base"))), ]
    out[!duplicated(out$package), , drop = FALSE]
  } else {
    if (!identical(lang, "R")) return(empty)

    # Package names loaded/installed, from the shared static extractor.
    names_df <- code_library_names(code_text, "R")
    pkgs <- unique(names_df$package)
    if (length(pkgs) == 0) return(empty)

    # GitHub / URL install sources named in the text, matched to a package by
    # the repo basename. install_github("user/repo@ref") -> package "repo".
    joined <- paste(code_text, collapse = "\n")
    gh_pat  <- "install_github\\s*\\(\\s*['\"]([^'\"]+)['\"]"
    url_pat <- "install\\.packages\\s*\\(\\s*['\"](https?://[^'\"]+)['\"]"
    gh  <- regmatches(joined, gregexpr(gh_pat,  joined, perl = TRUE))[[1]]
    url <- regmatches(joined, gregexpr(url_pat, joined, perl = TRUE))[[1]]
    gh_refs  <- sub(gh_pat,  "\\1", gh,  perl = TRUE)
    url_refs <- sub(url_pat, "\\1", url, perl = TRUE)
    # repo basename (drop owner and @ref) is the installed package name
    gh_pkg  <- sub("@.*$", "", basename(gh_refs))
    url_pkg <- sub("[_.].*$", "", basename(url_refs))   # pkg_1.0.tar.gz -> pkg

    base_pkgs <- .repro_base_packages()

    src <- rep("cran", length(pkgs))
    ref <- rep(NA_character_, length(pkgs))
    src[pkgs %in% base_pkgs] <- "base"
    for (k in seq_along(gh_pkg)) {
      hit <- pkgs == gh_pkg[k]
      if (any(hit)) { src[hit] <- "github"; ref[hit] <- gh_refs[k] }
    }
    for (k in seq_along(url_pkg)) {
      hit <- pkgs == url_pkg[k]
      if (any(hit)) { src[hit] <- "url"; ref[hit] <- url_refs[k] }
    }

    data.frame(package = pkgs, source = src, ref = ref,
               base = pkgs %in% base_pkgs)
  }
}

# The base + recommended packages shipped with R. A run never needs to install
# these, so they are tagged `base` and excluded from the install set. Resolved
# from the running R installation so it stays correct across R versions.
.repro_base_packages <- function() {
  ip <- tryCatch(utils::installed.packages(priority = c("base", "recommended")),
                 error = function(e) NULL)
  if (is.null(ip)) c("base", "methods", "utils", "stats", "graphics",
                     "grDevices", "datasets", "tools")
  else rownames(ip)
}

#' Rewrite a code file's data-file paths to the Psych-DS layout
#'
#' A script reads and writes data by relative path (`read_csv("raw/x.csv")`,
#' `saveRDS(m, "../out/model.rds")`). When the release is re-laid-out to
#' Psych-DS (data under `data/`, per-study `study-<group>/data/`), those paths no
#' longer resolve. This rewrites each referenced path to where the file now
#' lives, using the file→target plan `psychds_check` produced.
#'
#' Matching is by **basename** (a script's `../data/x.csv` and the repo's
#' `raw/x.csv` are the same file seen from different working directories, so the
#' prefix is ignored — the same choice [code_file_refs()] makes). When several
#' plan files share a basename (a `demographics.csv` in more than one study), the
#' ambiguity is resolved by **study group**: the candidate whose target path is
#' in the same study as the script (derived from the paths via
#' [.data_group_from_path()]) is chosen. Only when that still leaves more than
#' one candidate is the reference left unrewritten and flagged.
#'
#' @param code_text the code text for a single file (character vector)
#' @param file_name the script's own name/path (used to derive its study group)
#' @param plan the `psychds_check` table: one row per file with `file_name`,
#'   `target_path`, and `current_path`
#' @param lang the language (only R is rewritten here)
#'
#' @returns a data frame with one row per referenced path, columns `ref` (the
#'   path as written), `basename`, `matched` (logical, a plan file was found),
#'   `target` (its Psych-DS path, or NA), `ambiguous` (logical, several plan
#'   files matched and the study could not disambiguate), and `n_candidates`.
#'   Empty frame when the script references no files.
#' @export
#'
#' @examples
#' plan <- data.frame(
#'   file_name = c("demographics.csv", "scores.csv"),
#'   target_path = c("study-ex1/data/source-demographics_data.csv",
#'                   "study-ex1/data/source-scores_data.csv"),
#'   current_path = c("ex1/demographics.csv", "ex1/scores.csv")
#' )
#' code_text <- c('d <- read.csv("data/demographics.csv")')
#' repro_rewrite_paths(code_text, "ex1/analysis.R", plan)
repro_rewrite_paths <- function(code_text, file_name, plan, lang = "R") {
  empty <- data.frame(ref = character(0), basename = character(0),
                      matched = logical(0), target = character(0),
                      ambiguous = logical(0), n_candidates = integer(0))
  if (!identical(lang, "R") || is.null(code_text)) return(empty)
  if (is.null(plan) || nrow(plan) == 0 ||
      !all(c("file_name", "target_path") %in% names(plan))) return(empty)

  refs <- code_file_refs(code_text, "R")
  if (length(refs) == 0) return(empty)
  ref_base <- tolower(basename(gsub("\\\\", "/", refs)))

  # Candidate plan files by basename. Only rows with a real (non-NA) target are
  # reachable in the release; NA-target rows (consumed archives) are not.
  plan_base <- tolower(basename(gsub("\\\\", "/", plan$file_name)))
  has_target <- !is.na(plan$target_path) & nzchar(plan$target_path %||% "")

  # The script's own study group, for disambiguation.
  script_grp <- .data_group_from_path(file_name)

  rows <- lapply(seq_along(refs), function(i) {
    cand <- which(plan_base == ref_base[i] & has_target)
    n <- length(cand)
    if (n == 0) {
      return(data.frame(ref = refs[i], basename = ref_base[i], matched = FALSE,
                        target = NA_character_, ambiguous = FALSE,
                        n_candidates = 0L))
    }
    if (n == 1) {
      return(data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                        target = plan$target_path[cand], ambiguous = FALSE,
                        n_candidates = 1L))
    }
    # Several candidates share the basename: disambiguate by study group. Prefer
    # the candidate whose target path is in the script's study; failing that,
    # whose target is in the study the reference path itself names.
    cand_grp <- .data_group_from_path(plan$target_path[cand])
    ref_grp <- .data_group_from_path(refs[i])
    pick <- integer(0)
    if (!is.na(script_grp)) pick <- cand[cand_grp == script_grp & !is.na(cand_grp)]
    if (length(pick) != 1 && !is.na(ref_grp))
      pick <- cand[cand_grp == ref_grp & !is.na(cand_grp)]
    if (length(pick) == 1) {
      data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                 target = plan$target_path[pick], ambiguous = FALSE,
                 n_candidates = n)
    } else {
      # Still ambiguous: do not guess. Flagged for the report; the script is run
      # unmodified for this reference.
      data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                 target = NA_character_, ambiguous = TRUE, n_candidates = n)
    }
  })
  dplyr::bind_rows(rows)
}

#' Determine the order code files must run in
#'
#' Scripts in a repository usually depend on each other: a preparation script
#' writes a file a later analysis script reads, or one `source()`s another.
#' Running them in the wrong order fails for reasons that have nothing to do with
#' reproducibility, so a run must order them first. This derives an order from
#' three signals, in decreasing strength:
#'
#' 1. **read-after-write** — if file A's code writes `x.csv` and file B's code
#'    reads `x.csv`, A must precede B (hard data dependency);
#' 2. **`source()` edges** — if A `source()`s B, B must precede A;
#' 3. **numeric filename prefixes** — `0_prep.R` before `1_analysis.R` (a weak
#'    tie-breaker, not a dependency).
#'
#' The read/write and source references come from the shared static analysis
#' ([code_file_refs()] and a source scan); matching is by basename (as
#' elsewhere). The dependency graph is topologically sorted; a cycle (A reads a
#' file B writes and vice versa) cannot be ordered and is reported.
#'
#' @param files a data frame with one row per code file, columns `file_name`,
#'   and `reads` / `writes` / `sources` list-columns of basenames (as produced
#'   by [repro_file_io()]). When `reads`/`writes`/`sources` are absent they are
#'   derived from a `code_text` list-column if present.
#'
#' @returns a data frame with columns `file_name`, `order` (1-based run
#'   position, NA when unplaceable), `depends_on` (comma-joined file_names that
#'   must run earlier), and `order_basis` (`dependency`, `numeric`, or `none`).
#'   Attribute `"cycle"` is a character vector of file_names in a dependency
#'   cycle (empty when acyclic); attribute `"ambiguous"` is TRUE when no ordering
#'   signal placed a multi-file set (they share `order = NA`, basis `none`).
#' @export
#'
#' @examples
#' files <- data.frame(file_name = c("1_analysis.R", "0_prep.R"))
#' files$reads   <- list("clean.csv", character(0))
#' files$writes  <- list(character(0), "clean.csv")
#' files$sources <- list(character(0), character(0))
#' repro_run_order(files)
repro_run_order <- function(files, extra_edges = NULL) {
  if (is.null(files) || nrow(files) == 0)
    return(data.frame(file_name = character(0), order = integer(0),
                      depends_on = character(0), order_basis = character(0)))

  n <- nrow(files)
  fname <- files$file_name
  base_name <- tolower(basename(gsub("\\\\", "/", fname)))

  io_col <- function(col) {
    if (col %in% names(files)) files[[col]]
    else replicate(n, character(0), simplify = FALSE)
  }
  reads   <- io_col("reads")
  writes  <- io_col("writes")
  sources <- io_col("sources")

  # Edge j -> i means j must run BEFORE i. Built from read-after-write and
  # source(), keyed on basename.
  before <- vector("list", n)   # before[[i]] = indices that must precede i
  for (i in seq_len(n)) before[[i]] <- integer(0)

  # read-after-write: writer of a basename precedes its readers.
  writer_of <- list()
  for (j in seq_len(n)) for (w in tolower(writes[[j]]))
    writer_of[[w]] <- c(writer_of[[w]], j)
  for (i in seq_len(n)) for (r in tolower(reads[[i]])) {
    w <- setdiff(writer_of[[r]] %||% integer(0), i)   # not self
    if (length(w)) before[[i]] <- union(before[[i]], w)
  }
  # source(): if i sources B, the file whose basename is B precedes i.
  for (i in seq_len(n)) for (s in tolower(sources[[i]])) {
    j <- setdiff(which(base_name == s), i)
    if (length(j)) before[[i]] <- union(before[[i]], j)
  }

  # extra_edges: soft ordering edges inferred from execution (e.g. an
  # undefined-variable error implies the file defining that variable must run
  # first). A list of c(from_file_name, to_file_name) pairs; each adds a
  # "from precedes to" edge, matched to file positions by file_name.
  for (e in extra_edges %||% list()) {
    from <- which(fname == e[[1]]); to <- which(fname == e[[2]])
    from <- setdiff(from, to)   # never self
    if (length(from) && length(to))
      before[[to[[1]]]] <- union(before[[to[[1]]]], from)
  }

  # Kahn topological sort. `before[[i]]` are i's predecessors; build the forward
  # adjacency (`after[[j]]` = successors of j) and the in-degree, then repeatedly
  # emit a zero-in-degree node and decrement its successors. Any node never
  # reaching in-degree 0 sits in a cycle.
  after <- vector("list", n)
  for (i in seq_len(n)) after[[i]] <- integer(0)
  for (i in seq_len(n)) for (j in before[[i]]) after[[j]] <- c(after[[j]], i)
  indeg <- vapply(before, length, integer(1))

  # Deterministic tie-break among ready nodes. Real scripts carry their sequence
  # as digit runs anywhere in the name, not just a leading prefix: "0_prep.R",
  # but also "Exp1_02_preprocessing.R", "study2_1_import.R". So the numeric key
  # is the VECTOR of all digit runs in the name (Exp1_02 -> c(1, 2)), padded to a
  # common length and compared component-wise, which sorts Exp1_02 < Exp2_1
  # correctly. Files with no digits sort last, then alphabetically.
  digit_runs <- lapply(base_name, function(b)
    as.integer(regmatches(b, gregexpr("[0-9]+", b))[[1]]))
  has_num <- vapply(digit_runs, length, integer(1)) > 0
  maxk <- max(0L, vapply(digit_runs, length, integer(1)))
  # A matrix of padded numeric keys (NA-padded so no-digit names go last).
  num_key <- if (maxk > 0)
    do.call(rbind, lapply(digit_runs, function(v)
      c(v, rep(NA_integer_, maxk - length(v))))) else
    matrix(NA_integer_, nrow = n, ncol = 1)
  tie_key <- function(v) {
    args <- c(lapply(seq_len(ncol(num_key)), function(k) num_key[v, k]),
              list(base_name[v]))
    v[do.call(order, args)]
  }

  order_idx <- integer(0)
  placed <- rep(FALSE, n)
  ready <- tie_key(which(indeg == 0))
  while (length(ready) > 0) {
    node <- ready[[1]]; ready <- ready[-1]
    order_idx <- c(order_idx, node); placed[node] <- TRUE
    newly <- integer(0)
    for (i in after[[node]]) {
      indeg[i] <- indeg[i] - 1L
      if (indeg[i] == 0L) newly <- c(newly, i)
    }
    if (length(newly)) ready <- tie_key(c(ready, newly))
  }
  cycle_nodes <- which(!placed)

  # order_basis per file: `dependency` when it had an incoming edge (a real data
  # or source() dependency decided its position); else `numeric` when its name
  # carries digits that fed the ordering; else `none` (nothing distinguished it).
  had_edge <- vapply(before, function(x) length(x) > 0, logical(1))
  any_dependency <- any(had_edge)
  basis <- ifelse(had_edge, "dependency",
                  ifelse(has_num, "numeric", "none"))

  ord <- rep(NA_integer_, n)
  ord[order_idx] <- seq_along(order_idx)

  depends_on <- vapply(seq_len(n), function(i)
    paste(fname[before[[i]]], collapse = ", "), character(1))

  out <- data.frame(file_name = fname, order = ord,
                    depends_on = depends_on, order_basis = basis)
  attr(out, "cycle") <- fname[cycle_nodes]
  # Ambiguous when more than one file exists but nothing (no dependency edge and
  # no numbering in any filename) distinguishes their order.
  attr(out, "ambiguous") <- n > 1 && !any_dependency && !any(has_num)
  out
}

#' Extract the files a script reads, writes, and sources
#'
#' A thin wrapper that runs the static reference scans over each code file and
#' returns the per-file read / write / source basenames [repro_run_order()]
#' needs. Reads/writes come from [code_file_refs()] split by call type; sources
#' from a scan for `source()`. All returned as lowercased basenames.
#'
#' @param code_text_list a named list of code-text character vectors, one per
#'   file (names are the file_names)
#'
#' @returns a data frame with `file_name` and list-columns `reads`, `writes`,
#'   `sources` (each a character vector of basenames).
#' @export
repro_file_io <- function(code_text_list) {
  if (is.null(code_text_list) || length(code_text_list) == 0)
    return(data.frame(file_name = character(0)))

  fname <- names(code_text_list) %||% as.character(seq_along(code_text_list))

  # A read call vs a write call, from the call name preceding the quoted path.
  write_fns <- "write[._]|save(RDS|_rds)?|write_(csv2?|tsv|delim|xlsx|sav|dta|feather|parquet)"
  src_pat <- "source\\s*\\(\\s*['\"]([^'\"]+)['\"]"

  rows <- lapply(seq_along(code_text_list), function(k) {
    ct <- code_text_list[[k]]
    nc <- code_remove_comments(ct, "R")
    all_refs <- code_file_refs(nc, "R")
    joined <- paste(nc, collapse = "\n")

    # Classify each ref as write when it sits in a write-call line, else read.
    is_write <- vapply(all_refs, function(r) {
      # A line that both writes and names this file.
      lines <- grep(r, nc, fixed = TRUE, value = TRUE)
      any(grepl(write_fns, lines, perl = TRUE, ignore.case = TRUE))
    }, logical(1))
    reads  <- tolower(basename(gsub("\\\\", "/", all_refs[!is_write])))
    writes <- tolower(basename(gsub("\\\\", "/", all_refs[is_write])))

    srcs <- regmatches(joined, gregexpr(src_pat, joined, perl = TRUE))[[1]]
    srcs <- sub(src_pat, "\\1", srcs, perl = TRUE)
    srcs <- tolower(basename(gsub("\\\\", "/", srcs)))

    # code_file_refs() lists source() among its "read" calls, so a sourced
    # script leaks into `reads`. A source() is a CODE dependency (handled by the
    # source-edge ordering), not a data input, so drop sourced files from reads —
    # otherwise a present, sourced .R file is later mis-reported as a missing
    # data input.
    reads <- setdiff(reads, srcs)

    data.frame(file_name = fname[k], stringsAsFactors = FALSE) |>
      (\(d) { d$reads <- list(unique(reads)); d$writes <- list(unique(writes))
              d$sources <- list(unique(srcs)); d })()
  })
  dplyr::bind_rows(rows)
}

#' Find the variables each script defines at top level
#'
#' When a script errors with `object 'X' not found`, the fix is usually to run
#' the script that *defines* `X` first. This scans each file for **top-level**
#' variable assignments — `x <- ...`, `x = ...`, `x <<- ...`, and
#' `assign("x", ...)` — so an undefined-variable error can be matched to the file
#' that would supply it (see the module's corrective re-run). Only assignments at
#' the start of a (comment-free) line are taken, which approximates "top level":
#' an assignment indented inside a `function`/`for` body is not a global the next
#' sourced script would see, and taking it would create false ordering edges.
#'
#' @param code_text_list a named list of code-text character vectors, one per
#'   file (names are the file_names)
#'
#' @returns a data frame with `file_name` and a list-column `defines` (character
#'   vector of variable names each file assigns at top level).
#' @export
repro_defined_vars <- function(code_text_list) {
  if (is.null(code_text_list) || length(code_text_list) == 0)
    return(data.frame(file_name = character(0)))
  fname <- names(code_text_list) %||% as.character(seq_along(code_text_list))

  # A top-level assignment: line start (no indentation => not inside a block),
  # a valid R name, then <-, <<-, or = (not ==). Also assign("name", ...).
  assign_pat <- "^([.a-zA-Z][.a-zA-Z0-9_]*)\\s*(<<-|<-|=)(?!=)"
  assign_fn  <- "^\\s*assign\\s*\\(\\s*['\"]([.a-zA-Z][.a-zA-Z0-9_]*)['\"]"

  rows <- lapply(seq_along(code_text_list), function(k) {
    nc <- code_remove_comments(code_text_list[[k]], "R")
    m1 <- regmatches(nc, regexpr(assign_pat, nc, perl = TRUE))
    v1 <- sub(paste0(assign_pat, ".*"), "\\1", m1, perl = TRUE)
    m2 <- regmatches(nc, regexpr(assign_fn, nc, perl = TRUE))
    v2 <- sub(paste0(assign_fn, ".*"), "\\1", m2, perl = TRUE)
    defines <- unique(c(v1[nzchar(v1)], v2[nzchar(v2)]))
    data.frame(file_name = fname[k], stringsAsFactors = FALSE) |>
      (\(d) { d$defines <- list(defines); d })()
  })
  dplyr::bind_rows(rows)
}

#' Diagnose why a referenced input file is unavailable
#'
#' When a script reads a file that is not present, the reason matters: a file
#' deliberately withheld because it was too large to download is a different
#' finding from a file simply absent from the repository. This cross-references
#' the referenced basenames against the file plan and the download-skip records
#' `download_repo_files()` attaches, and classifies each unresolved reference.
#'
#' @param refs a character vector of referenced file basenames (unresolved reads)
#' @param plan the `psychds_check` table (file_name + target_path)
#' @param structure_df the `data_check` structure table (file_name +
#'   file_location + file_size)
#' @param skipped optional data frame of size-skipped files (the
#'   `oversize_skipped` / `gated` attributes of [download_repo_files()], with a
#'   `file_name` column)
#'
#' @returns a data frame with `basename`, `status` (one of `in_repo_not_downloaded`,
#'   `withheld_size`, `absent`), and `detail` (a human-readable reason).
#' @export
repro_missing_inputs <- function(refs, plan, structure_df, skipped = NULL) {
  refs <- unique(tolower(basename(gsub("\\\\", "/", refs))))
  empty <- data.frame(basename = character(0), status = character(0),
                      detail = character(0))
  if (length(refs) == 0) return(empty)

  struct_base <- if (!is.null(structure_df) && "file_name" %in% names(structure_df))
    tolower(basename(gsub("\\\\", "/", structure_df$file_name))) else character(0)
  skip_base <- if (!is.null(skipped) && "file_name" %in% names(skipped))
    tolower(basename(gsub("\\\\", "/", skipped$file_name))) else character(0)

  rows <- lapply(refs, function(b) {
    if (b %in% skip_base) {
      i <- which(skip_base == b)[1]
      sz <- suppressWarnings(as.numeric(skipped$file_size[i]))
      mb <- if (is.finite(sz)) sprintf(" (%.0f MB)", sz / (1024 * 1024)) else ""
      return(data.frame(basename = b, status = "withheld_size",
                        detail = sprintf(
                          "referenced file is in the repository but was not downloaded%s: over the size cap",
                          mb)))
    }
    if (b %in% struct_base) {
      i <- which(struct_base == b)[1]
      loc <- if ("file_location" %in% names(structure_df))
        structure_df$file_location[i] else NA_character_
      downloaded <- !is.na(loc) && nzchar(loc) && file.exists(loc %||% "")
      if (!downloaded)
        return(data.frame(basename = b, status = "in_repo_not_downloaded",
                          detail = "referenced file is listed in the repository but was not downloaded"))
      # Present and downloaded — not actually missing; caller filters these out.
      return(data.frame(basename = b, status = "present",
                        detail = "referenced file is available"))
    }
    data.frame(basename = b, status = "absent",
               detail = "referenced file is not present in the repository")
  })
  out <- dplyr::bind_rows(rows)
  out[out$status != "present", , drop = FALSE]
}

# ─────────────────────────────────────────────────────────────────────────────
# EXECUTION helpers (the `execute = TRUE` phase). Everything below actually runs
# downloaded code, so it is reached only when the caller explicitly opts in via
# reproducibility_check(execute = TRUE). Each script runs in an isolated
# subprocess (callr) so a crash cannot take down the R session; note this
# isolates crashes, NOT the filesystem or network — running untrusted code is a
# deliberate, gated action. The layout is materialised into a throwaway temp dir
# so the real psychds output and the download cache are never touched.
# ─────────────────────────────────────────────────────────────────────────────

#' Materialise the Psych-DS layout for a paper into a throwaway directory
#'
#' The execute phase runs the paper's code against the Psych-DS layout, but the
#' module has only the *plan* (`psychds_check`'s file → `target_path`) and the
#' current on-disk location of each source file (`data_check`'s `structure`).
#' This builds the tree the plan describes inside a fresh temp directory: for
#' each planned file with a real target and an available source copy, the source
#' is copied to `<root>/<target_path>`. Nothing outside `root` is written, and
#' `root` is the caller's to delete.
#'
#' @param plan the `psychds_check` table (`file_name`, `target_path`)
#' @param structure_df the `data_check` structure table (`file_name`,
#'   `file_location`)
#' @param root the temp directory to build into (created if absent)
#'
#' @returns `root`, invisibly, with attribute `"materialised"`: a data frame of
#'   `target_path` / `source` / `ok` (whether the copy succeeded) rows.
#' @export
repro_materialize_layout <- function(plan, structure_df, root) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  done <- data.frame(target_path = character(0), source = character(0),
                     ok = logical(0))
  if (is.null(plan) || nrow(plan) == 0 ||
      !all(c("file_name", "target_path") %in% names(plan)))
    return(invisible(structure(root, materialised = done)))

  loc_lookup <- if (!is.null(structure_df) &&
                    all(c("file_name", "file_location") %in% names(structure_df)))
    stats::setNames(structure_df$file_location, structure_df$file_name) else
    character(0)
  # basename index, for when plan file_name and structure file_name differ by dir
  loc_base <- if (length(loc_lookup))
    stats::setNames(loc_lookup, tolower(basename(names(loc_lookup)))) else
    character(0)

  find_source <- function(fn) {
    if (fn %in% names(loc_lookup)) {
      l <- loc_lookup[[fn]]
      if (!is.na(l) && nzchar(l) && file.exists(l)) return(l)
    }
    b <- tolower(basename(fn))
    if (b %in% names(loc_base)) {
      l <- loc_base[[b]]
      if (!is.na(l) && nzchar(l) && file.exists(l)) return(l)
    }
    NA_character_
  }

  has_target <- !is.na(plan$target_path) & nzchar(plan$target_path %||% "")
  rows <- lapply(which(has_target), function(i) {
    tgt <- plan$target_path[i]
    src <- find_source(plan$file_name[i])
    if (is.na(src))
      return(data.frame(target_path = tgt, source = NA_character_, ok = FALSE))
    dest <- file.path(root, tgt)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    ok <- isTRUE(file.copy(src, dest, overwrite = TRUE))
    data.frame(target_path = tgt, source = src, ok = ok)
  })
  done <- if (length(rows)) dplyr::bind_rows(rows) else done
  invisible(structure(root, materialised = done))
}

#' Write path-rewritten scripts into the materialised layout
#'
#' Each script's referenced data paths are rewritten to their Psych-DS target
#' (via the per-file `repro_rewrite_paths()` result) so that, run from `root`,
#' the reads/writes resolve against the materialised tree. The rewritten script
#' text is written to `root/<script target_path>` (or, when the plan has no
#' target for the script itself, to `root/<basename>` at the top level so it is
#' still runnable). A reference left ambiguous or unmatched is written unchanged
#' — the static report already flagged it.
#'
#' @param code_text_list named list of per-file code text (names are file_names)
#' @param rewrite_list named list of `repro_rewrite_paths()` results, same order
#' @param plan the `psychds_check` table (to find each script's own target_path)
#' @param root the materialised layout root
#'
#' @details
#' A `setwd()` call in analysis code is bad practice: it hardcodes an assumption
#' about where the code runs (often an absolute path on the author's own machine,
#' e.g. `setwd("D:/Dropbox/...")`), and it overrides the working directory the
#' runner sets to the materialised layout — so the script's relative reads then
#' resolve against a directory that does not exist here, and the reproduction
#' fails for a reason that has nothing to do with the analysis. To run the code
#' we therefore **comment out every `setwd()` call** (recorded per file in
#' `setwd_removed` and flagged in the report). The transform is in the same
#' spirit as the path rewriting: it makes the author's script runnable in its new
#' location without changing the analysis. A script that only reproduces after a
#' `setwd()` is removed is reported as "reproducible after ignoring a `setwd()`
#' that should not have been in the code and needs to be fixed".
#'
#' @returns a data frame with `file_name`, `script_path` (absolute path written),
#'   `run_dir` (the working directory a run should use — always `root`),
#'   `setwd_removed` (count of `setwd()` lines commented out), and
#'   `setwd_paths` (comma-joined paths those calls named, for the warning).
#' @export
repro_write_scripts <- function(code_text_list, rewrite_list, plan, root) {
  fnames <- names(code_text_list)
  plan_base <- if (!is.null(plan) && "file_name" %in% names(plan))
    tolower(basename(plan$file_name)) else character(0)

  script_target <- function(fn) {
    # A script's own place in the tree: match its basename in the plan; else put
    # it at the tree root under its basename so it is still runnable.
    if (length(plan_base)) {
      m <- which(plan_base == tolower(basename(fn)) &
                   !is.na(plan$target_path) & nzchar(plan$target_path %||% ""))
      if (length(m) >= 1) return(plan$target_path[m[[1]]])
    }
    basename(fn)
  }

  # A line that calls setwd(). Matches setwd( at a statement start (allowing
  # leading whitespace), so setwd inside a longer expression is still caught by
  # the call token. The quoted argument (if any) is captured for the warning.
  setwd_line <- "^(\\s*)(setwd\\s*\\(.*)$"
  setwd_arg  <- "setwd\\s*\\(\\s*['\"]([^'\"]+)['\"]"

  rows <- lapply(seq_along(code_text_list), function(i) {
    fn  <- fnames[[i]]
    txt <- code_text_list[[i]]
    rw  <- rewrite_list[[fn]]
    # Apply each resolved (matched, non-ambiguous, real target) rewrite as a
    # literal replacement of the path as written. Fixed (non-regex) to avoid
    # metacharacter surprises in file paths.
    if (!is.null(rw) && nrow(rw)) {
      good <- rw$matched & !rw$ambiguous & !is.na(rw$target) & nzchar(rw$target)
      for (k in which(good))
        txt <- gsub(rw$ref[k], rw$target[k], txt, fixed = TRUE)
    }

    # Comment out setwd() lines so they cannot override the sandbox wd. Record
    # how many, and the path each named, for the report's warning.
    is_setwd <- grepl(setwd_line, txt, perl = TRUE)
    setwd_n <- sum(is_setwd)
    setwd_paths <- character(0)
    if (setwd_n > 0) {
      hit_lines <- txt[is_setwd]
      m <- regmatches(hit_lines, regexpr(setwd_arg, hit_lines, perl = TRUE))
      setwd_paths <- sub(paste0(".*", setwd_arg, ".*"), "\\1",
                         m[nzchar(m)], perl = TRUE)
      # Comment the offending lines (keep them visible, but inert), tagging why.
      txt[is_setwd] <- sub(setwd_line,
        "\\1# [reproducibility_check removed setwd] \\2", txt[is_setwd],
        perl = TRUE)
    }

    tgt  <- script_target(fn)
    dest <- file.path(root, tgt)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    writeLines(txt, dest)
    data.frame(file_name = fn, script_path = dest, run_dir = root,
               setwd_removed = setwd_n,
               setwd_paths = paste(setwd_paths, collapse = ", "))
  })
  dplyr::bind_rows(rows)
}

#' Install a paper's declared dependencies into a throwaway library
#'
#' Installs each non-base dependency into `lib_dir` (a temp library, never the
#' user's), so a run does not mutate the user's installed packages. CRAN packages
#' come from `install.packages()`; GitHub/URL sources from the ref the code named
#' (`remotes::install_github()` / `install.packages(url)`), honouring a pinned
#' `@ref` when present. Every install's outcome is recorded; installing runs
#' package build/configure scripts, so this is part of the gated execute phase.
#'
#' @param install_deps the module's `install_deps` frame (`package`, `source`,
#'   `ref`), base packages already excluded
#' @param lib_dir the throwaway library path (created if absent)
#'
#' @returns a data frame with `package`, `source`, `installed` (logical), and
#'   `message` (error text on failure, else "").
#' @export
repro_install_deps <- function(install_deps, lib_dir) {
  empty <- data.frame(package = character(0), source = character(0),
                      installed = logical(0), message = character(0))
  if (is.null(install_deps) || nrow(install_deps) == 0) return(empty)
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  old_lib <- .libPaths(); on.exit(.libPaths(old_lib), add = TRUE)
  .libPaths(c(lib_dir, old_lib))

  gh_avail <- requireNamespace("remotes", quietly = TRUE)

  rows <- lapply(seq_len(nrow(install_deps)), function(i) {
    pkg <- install_deps$package[i]
    src <- install_deps$source[i]
    ref <- install_deps$ref[i]
    # DEBUG (TEMPORARY — remove before release): quiet = FALSE so compilation
    # progress is visible; a slow install then reads as work, not a hang.
    message("[repro]     installing '", pkg, "' (source: ", src, ") ...")
    res <- tryCatch({
      if (identical(src, "github")) {
        if (!gh_avail) stop("the 'remotes' package is needed to install GitHub sources")
        remotes::install_github(ref, lib = lib_dir, upgrade = "never",
                                quiet = FALSE)
      } else if (identical(src, "url")) {
        utils::install.packages(ref, lib = lib_dir, repos = NULL, quiet = FALSE)
      } else {
        utils::install.packages(pkg, lib = lib_dir, quiet = FALSE)
      }
      # Confirm it can actually be loaded from the temp library.
      if (!requireNamespace(pkg, quietly = TRUE, lib.loc = lib_dir) &&
          !requireNamespace(pkg, quietly = TRUE))
        stop("installed but package '", pkg, "' is not loadable")
      list(ok = TRUE, msg = "")
    }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    data.frame(package = pkg, source = src, installed = res$ok, message = res$msg)
  })
  dplyr::bind_rows(rows)
}

#' Run the paper's scripts, in order, in isolated subprocesses
#'
#' Runs each script with [callr::r()] from `run_dir`, with `lib_dir` prepended to
#' the subprocess `.libPaths()` so the throwaway library is seen. Each run is
#' bounded by `timeout` seconds; its exit, error, stdout and stderr are captured.
#' A script whose inputs are known-missing (in `skip`) is not run — it is
#' recorded `skipped_missing_inputs`, since it cannot succeed and would only
#' produce a confusing error. callr isolates a crash to the subprocess; it does
#' not sandbox the filesystem or network, so this only runs on explicit opt-in.
#'
#' @param run_tbl `repro_write_scripts()` output (`file_name`, `script_path`,
#'   `run_dir`)
#' @param order a vector of `file_name`s in the order to run (unplaceable files
#'   are appended at the end in input order)
#' @param lib_dir throwaway library to expose to each subprocess (or NULL)
#' @param timeout per-script timeout in seconds
#' @param skip character vector of `file_name`s to record as
#'   `skipped_missing_inputs` instead of running
#' @param parses named logical (by file_name); a file that will not parse is
#'   recorded `not_parsed` and not run
#'
#' @returns a data frame with `file_name`, `outcome` (one of `ran_ok`,
#'   `errored`, `timed_out`, `not_parsed`, `skipped_missing_inputs`), `error`
#'   (message or ""), `error_type` (`undefined_variable`, `timeout`, `runtime`,
#'   or NA), `undefined_var` (the missing variable name when
#'   `error_type == "undefined_variable"`, else NA), `stdout`, `stderr`, and
#'   `elapsed` (seconds).
#' @export
repro_run_scripts <- function(run_tbl, order, lib_dir = NULL, timeout = 600,
                              skip = character(0), parses = NULL) {
  if (is.null(run_tbl) || nrow(run_tbl) == 0)
    return(data.frame(file_name = character(0), outcome = character(0),
                      error = character(0), error_type = character(0),
                      undefined_var = character(0), stdout = character(0),
                      stderr = character(0), elapsed = numeric(0)))
  if (!requireNamespace("callr", quietly = TRUE))
    stop("the 'callr' package is required to execute code (execute = TRUE).",
         call. = FALSE)

  # Order the files: placeable ones by the given order, the rest appended.
  ordered_names <- c(order[order %in% run_tbl$file_name],
                     setdiff(run_tbl$file_name, order))
  libs <- if (!is.null(lib_dir)) c(lib_dir, .libPaths()) else .libPaths()

  pb_run <- pb(length(ordered_names), ":what [:bar] :current/:total")
  pb_run$tick(0, list(what = ""))
  on.exit(pb_run$terminate())

  rows <- lapply(ordered_names, function(fn) {
    pb_run$tick(1, list(what = fn))
    row <- run_tbl[run_tbl$file_name == fn, ][1, ]

    if (!is.null(parses) && fn %in% names(parses) && !isTRUE(parses[[fn]]))
      return(data.frame(file_name = fn, outcome = "not_parsed", error = "",
                        error_type = NA_character_, undefined_var = NA_character_,
                        stdout = "", stderr = "", elapsed = 0))
    if (fn %in% skip)
      return(data.frame(file_name = fn, outcome = "skipped_missing_inputs",
                        error = "", error_type = NA_character_,
                        undefined_var = NA_character_,
                        stdout = "", stderr = "", elapsed = 0))

    # DEBUG (TEMPORARY — remove before release): announce each script as it starts
    # and finishes, so a hang is pinned to the exact script and its wall time.
    message("[repro]   -> running '", fn, "' (timeout ", timeout, "s) ...")

    # Capture the child's stdout/stderr to FILES, not OS pipes. With stdout = "|"
    # callr routes output through a pipe the parent only drains after the call
    # returns; a chatty script fills the pipe buffer and the child blocks on
    # write while the parent blocks on wait — a deadlock (this is the hang).
    # Redirecting to files removes the buffer entirely, so no script can wedge
    # the run, and the output is still captured (read back below).
    out_file <- tempfile(fileext = ".out")
    err_file <- tempfile(fileext = ".err")
    t0 <- Sys.time()
    out <- tryCatch(
      callr::r(
        # callr::r() has no working-directory argument, so set it INSIDE the
        # subprocess before sourcing, so the script's relative paths resolve
        # against the materialised layout root.
        function(script, wd) { setwd(wd); source(script, echo = FALSE) },
        args = list(script = row$script_path, wd = row$run_dir),
        libpath = libs,
        timeout = timeout, error = "error",
        stdout = out_file, stderr = err_file),
      error = function(e) e)
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    # Read whatever the child wrote before it finished / errored / timed out.
    read_cap <- function(f) if (file.exists(f))
      paste(readLines(f, warn = FALSE), collapse = "\n") else ""
    so <- read_cap(out_file); se <- read_cap(err_file)
    unlink(c(out_file, err_file))

    message("[repro]   <- '", fn, "' done in ", round(elapsed, 1), "s (",
            if (inherits(out, "condition")) "condition/error" else "ok",
            "; stdout ", nchar(so), " chars, stderr ", nchar(se), " chars)")

    # callr signals a timeout with a classed condition; a script error arrives as
    # a callr "error in processx"/"callr_error" carrying the child's message.
    if (inherits(out, "condition")) {
      is_timeout <- inherits(out, "callr_timeout_error") ||
        grepl("timed? ?out", conditionMessage(out), ignore.case = TRUE)
      msg <- conditionMessage(out)
      # Classify the error. An "object 'X' not found" is special: it usually
      # means the script expected a variable an earlier (unrun / separately-run)
      # script defined — the classic symptom of a snippet meant to be source()d
      # into a larger session, not run standalone. We capture the variable name;
      # a later phase uses it as a run-order signal (the file defining X should
      # precede this one). Check both the callr message and the captured stderr,
      # since the child's own error text lands in stderr.
      undef_pat <- "object ['\"]([^'\"]+)['\"] not found"
      undef_src <- if (grepl(undef_pat, msg)) msg else
        if (grepl(undef_pat, se)) se else NA_character_
      undef_var <- if (!is.na(undef_src))
        sub(paste0(".*", undef_pat, ".*"), "\\1",
            regmatches(undef_src, regexpr(undef_pat, undef_src))) else NA_character_
      etype <- if (is_timeout) "timeout"
               else if (!is.na(undef_var)) "undefined_variable"
               else "runtime"
      data.frame(file_name = fn,
                 outcome = if (is_timeout) "timed_out" else "errored",
                 error = msg, error_type = etype, undefined_var = undef_var,
                 stdout = so, stderr = se, elapsed = elapsed)
    } else {
      data.frame(file_name = fn, outcome = "ran_ok", error = "",
                 error_type = NA_character_, undefined_var = NA_character_,
                 stdout = so, stderr = se, elapsed = elapsed)
    }
  })
  dplyr::bind_rows(rows)
}
