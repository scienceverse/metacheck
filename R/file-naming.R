# File-naming convention checks, per the metacheck "machine readable FAIR data
# and code" guide: no spaces or special characters, avoid CamelCase, actually
# be classifiable, use zero-padded numbering for lexicographic sort, and stay
# within path-length budgets a typical download would need. Used by
# repo_check() to surface naming problems alongside its other warnings.
#
# A "date-format" rule (flagging an 8-digit run that doesn't parse as a valid
# YYYYMMDD date) was removed: there is no way to tell, from the filename
# alone, whether an 8-digit run was ever intended as a date rather than some
# other numeric ID (a DOI suffix, an accession number, a participant code),
# so any such heuristic produces false positives with no reliable signal to
# avoid them -- confirmed with the user rather than patched further (see
# issue #462, a 16-digit ID sliced into two bogus "invalid date" violations).
#
# Deliberately does NOT check case (lowercase-vs-uppercase is acceptable — the
# user confirmed this is not worth flagging even as a suggestion) or
# underscore-vs-dash separator consistency: that convention (from the
# manuscript) is aimed at generic tools that parse a filename positionally.
# metacheck's own classifiers (file_category(), data_classify_files(),
# .data_group_from_path()) never do that — they match keywords ANYWHERE in a
# name (grepl("code", nm)) with an optional, unconstrained separator
# ([ ._-]?), so "codePowerSim.R", "power-sim_code.R", and "power_sim-code.R"
# all classify identically. Nothing in metacheck keys off separator
# consistency, so checking for it would flag a real convention with no actual
# consequence in this tool — checked and confirmed with the user rather than
# assumed.

# Severity per rule, decided with the user: "bad" rules break something real
# (unreadable names, or a file so unclassifiable it cannot be typed at all);
# "suggestion" rules are conventions metacheck's own classifiers do not
# actually depend on today (case is ignored, and no code relies on
# lexicographic sort order — see repro_run_order()'s numeric, not string,
# tie-break) — real, worth following, but not load-bearing, so they never
# affect the traffic light on their own.
# Files that do not belong in a repository because software creates them by
# itself, never the authors, and they hold nothing a reader can use. repo_check
# and data_check leave them out of the file list, and repo_check lists them so
# the authors can remove them. Kept, they were counted as files, flagged for
# naming, and the "._" copies of .csv files were read as (unreadable) data: in
# one LDbase zip they were 993 of 1,826 listed files.
#
# The list follows GitHub's ignore-file templates (github.com/github/gitignore:
# Global/macOS, Windows, Linux, MicrosoftOffice, LibreOffice, Python, R,
# Jupyter), each also seen in real repositories (metacheck's corpus cache,
# 2026-10-15), and the junk rules TU/e's data managers check before a package
# is archived (the RAPS data manager check, as in pytacheck's files_junk.json):
# editor backups, temporary files and .Rhistory (which can hold file paths and
# passwords) are on it for that reason. Version-control folders and editor
# settings are "suggestions" (see .useless_file_severity()): they can be kept
# on purpose. Files that may matter to someone (.RData, knitr caches) are
# deliberately not included.
#
# Returns, per path, the kind of useless file, or NA for any other file.
.useless_file_kind <- function(path) {
  if (!length(path)) return(character(0))
  p <- gsub("\\\\", "/", as.character(path))
  # Not basename(): it expands a leading "~" to the home folder first, so
  # "~$report.docx" became "Documents$report.docx".
  base <- sub("^.*/", "", p)
  in_dir <- function(dir) grepl(paste0("(^|/)", dir, "(/|$)"), p, perl = TRUE)
  kind <- rep(NA_character_, length(p))
  # Files the operating system adds: macOS's __MACOSX/ folder and "._" copies
  # (resource forks a Mac adds to every file it zips), Finder's .DS_Store and
  # other volume files, Windows's thumbnail caches, folder settings and
  # recycle bin, and the Linux desktop's equivalents.
  os <- in_dir("__MACOSX") | startsWith(base, "._") |
    tolower(base) %in% c(".ds_store", ".localized", "icon\r", "thumbs.db",
                         "ehthumbs.db", "ehthumbs_vista.db", "desktop.ini",
                         ".directory") |
    in_dir("\\.AppleDouble") | in_dir("\\.Spotlight-V100") | in_dir("\\.Trashes") |
    in_dir("\\.fseventsd") | in_dir("\\.TemporaryItems") | in_dir("\\$RECYCLE\\.BIN") |
    in_dir("\\.Trash-[0-9]+")
  kind[os] <- "operating system"
  # The history of a project kept by Git, Subversion or Mercurial: it can hold
  # earlier versions of files, including data that was later removed.
  kind[is.na(kind) & (in_dir("\\.git") | in_dir("\\.svn") | in_dir("\\.hg"))] <-
    "version control"
  # Editor settings (JetBrains, Visual Studio Code), which can hold paths on
  # the author's own computer.
  kind[is.na(kind) & (in_dir("\\.idea") | in_dir("\\.vscode"))] <- "editor settings"
  # RStudio's per-user session state (open tabs, unsaved edits).
  kind[is.na(kind) & in_dir("\\.Rproj\\.user")] <- "RStudio session files"
  # Jupyter's automatic duplicate copies of notebooks.
  kind[is.na(kind) & in_dir("\\.ipynb_checkpoints")] <- "Jupyter checkpoints"
  # Compiled Python files, recreated whenever the code runs.
  kind[is.na(kind) & (in_dir("__pycache__") | grepl("\\.py[co]$", base))] <-
    "compiled Python files"
  # Lock files Word, Excel, PowerPoint and LibreOffice leave while a document
  # is open.
  kind[is.na(kind) & (startsWith(base, "~$") | grepl("^\\.~lock\\..*#$", base))] <-
    "open-document lock files"
  # The commands typed in an R session: they can hold file paths and passwords.
  kind[is.na(kind) & base == ".Rhistory"] <- "R history"
  # Copies an editor makes by itself: "file~", .bak, and Vim's swap files.
  kind[is.na(kind) & grepl("(~|\\.bak|\\.swp|\\.swo)$", base, ignore.case = TRUE)] <-
    "editor backups"
  # Temporary files a program forgot to remove.
  kind[is.na(kind) & grepl("\\.(tmp|temp)$", base, ignore.case = TRUE)] <-
    "temporary files"
  kind[is.na(p)] <- NA_character_
  kind
}

# How serious each kind of useless file is: "problem" (remove it; repo_check's
# light turns red) or "suggestion" (version control and editor settings, which
# can be kept on purpose; the light turns yellow). NA for NA.
.useless_file_severity <- function(kind) {
  ifelse(is.na(kind), NA_character_,
         ifelse(kind %in% c("version control", "editor settings"),
                "suggestion", "problem"))
}

.is_useless_file <- function(path) !is.na(.useless_file_kind(path))

.file_naming_severity <- c(
  "spaces"               = "bad",
  "special-characters"   = "bad",
  "diacritics"           = "bad",
  "unclassifiable"       = "bad",
  "zero-padding"         = "suggestion",
  "path-length-255"      = "bad",
  "path-length-228"      = "suggestion",
  "directory-length-100" = "suggestion",
  "filename-length-50"   = "suggestion"
)

# Check ONE filename (basename only, no directory) against the naming-
# convention rules that apply to a single file in isolation (i.e. everything
# except zero-padded numbering, which needs the sibling files in its family —
# see .file_naming_check_padding()). Returns a data.frame of violations (0
# rows when clean), one row per rule broken: `rule`, `detail`.
.file_naming_check_one <- function(file_name, data_type = NA_character_) {
  base <- basename(file_name)
  stem <- tools::file_path_sans_ext(base)
  issues <- list()

  add <- function(rule, detail) {
    issues[[length(issues) + 1L]] <<- data.frame(
      rule = rule, detail = detail, stringsAsFactors = FALSE)
  }

  # Spaces: never allowed in file/folder names.
  if (grepl(" ", base, fixed = TRUE))
    add("spaces", "contains a space")

  # Special characters: only letters, digits, underscore, dash, and the
  # extension's leading dot are allowed. Spaces are EXCLUDED from this class
  # (not `[^A-Za-z0-9._ -]`) even though they aren't in the allowed set,
  # because a bare space is already fully reported by its own "spaces" rule
  # above — including it here too would report the same single problem
  # twice under two different rule names. Diacritics are similarly covered by
  # their own dedicated rule below, but those bytes ALSO fall outside
  # A-Za-z0-9._-, so an accented filename correctly gets both "special-
  # characters" and "diacritics" (they are genuinely two different kinds of
  # character, unlike a space, which is only ever one thing).
  if (grepl("[^A-Za-z0-9._ -]", base))
    add("special-characters", "contains a character other than letters, digits, underscore, or dash")

  # Non-ASCII / diacritics.
  if (grepl("[^\x01-\x7F]", base))
    add("diacritics", "contains non-ASCII characters (e.g. accented letters)")

  # Unclassifiable: the file could not be classified at all — no recognised
  # keyword (see file_category()) or extension (see .file_type_crosswalk /
  # .fixed_ext_type) told data_classify_files() what kind of file this is.
  # This is NOT a check on filename FORM (metacheck's classifiers match a
  # keyword anywhere in the name, with any/no separator — see
  # data_classify_files()), only on whether classification actually
  # succeeded, which is what a human or machine reading the FAIR guide's
  # "allow classification by machines" goal actually needs.
  if (!is.na(data_type) && identical(data_type, "unknown"))
    add("unclassifiable", "could not be classified by name or extension (data_type is 'unknown'); add a recognisable keyword (data, code, materials, ...) or a known extension")

  if (length(issues)) do.call(rbind, issues) else
    data.frame(rule = character(0), detail = character(0), stringsAsFactors = FALSE)
}

# Zero-padded numbering: within a family of files sharing the same stem prefix
# and suffix around a numeric run (e.g. "data-2.csv", "data-11.csv" -> family
# "data-<N>.csv"), the numeric run should be zero-padded to the width the
# family's largest number needs, so lexicographic sort matches numeric sort.
# Returns a data.frame (file_name, rule, detail) for members that break this,
# or 0 rows when every family already sorts correctly (including families of
# size 1, which have nothing to be inconsistent with).
.file_naming_check_padding <- function(file_names) {
  if (!length(file_names)) return(data.frame(
    file_name = character(0), rule = character(0), detail = character(0),
    stringsAsFactors = FALSE))

  base <- basename(file_names)
  # Split each name into (prefix, number, suffix) around its LAST run of
  # digits (the common place a sequence index sits, e.g. "pp01.csv").
  m <- regexec("^(.*?)([0-9]+)([^0-9]*)$", base)
  parts <- regmatches(base, m)
  has_num <- lengths(parts) == 4L

  out <- list()
  if (any(has_num)) {
    prefix <- vapply(parts[has_num], `[[`, character(1), 2)
    numstr <- vapply(parts[has_num], `[[`, character(1), 3)
    suffix <- vapply(parts[has_num], `[[`, character(1), 4)
    family_key <- paste(prefix, suffix, sep = "\r")
    idx <- which(has_num)

    for (key in unique(family_key)) {
      members <- idx[family_key == key]
      if (length(members) < 2L) next   # nothing to be inconsistent with
      widths <- nchar(numstr[family_key == key])
      target <- max(widths)
      bad <- members[widths < target]
      if (length(bad)) {
        out[[length(out) + 1L]] <- data.frame(
          file_name = file_names[bad],
          rule = "zero-padding",
          detail = sprintf(
            "numbered inconsistently with sibling files (pad to %d digit%s so names sort in numeric order)",
            target, if (target == 1) "" else "s"),
          stringsAsFactors = FALSE)
      }
    }
  }
  if (length(out)) do.call(rbind, out) else data.frame(
    file_name = character(0), rule = character(0), detail = character(0),
    stringsAsFactors = FALSE)
}

# Path-length budgets: the four thresholds from the FAIR data/code guide —
# 255 chars for the full relative path, 228 accounting for a typical Downloads
# folder prefix, 100 for the directory portion, 50 for the base filename.
# Returns a data.frame (file_name, rule, detail) for files that exceed any
# budget; 0 rows when everything fits.
.file_naming_check_length <- function(file_path) {
  file_path <- gsub("\\\\", "/", file_path)
  dir_part  <- dirname(file_path)
  dir_part[dir_part == "."] <- ""
  base_part <- basename(file_path)

  out <- list()
  # detail: a sprintf() template for the length, in words a reader knows
  # (the rule's id is in `rule`)
  add_rows <- function(hit, rule, detail, actual) {
    if (!any(hit)) return(invisible())
    out[[length(out) + 1L]] <<- data.frame(
      file_name = file_path[hit], rule = rule,
      detail = sprintf(detail, actual[hit]),
      stringsAsFactors = FALSE)
  }
  add_rows(nchar(file_path) > 255L, "path-length-255",
           "The path is %d characters, over the 255-character limit", nchar(file_path))
  add_rows(nchar(file_path) > 228L, "path-length-228",
           "The path is %d characters, over the recommended 228, which leaves room for a download folder",
           nchar(file_path))
  add_rows(nchar(dir_part) > 100L, "directory-length-100",
           "The folder path is %d characters, over the recommended 100", nchar(dir_part))
  add_rows(nchar(base_part) > 50L, "filename-length-50",
           "The file name is %d characters, over the recommended 50", nchar(base_part))

  if (length(out)) do.call(rbind, out) else data.frame(
    file_name = character(0), rule = character(0), detail = character(0),
    stringsAsFactors = FALSE)
}

#' Check repository files against the FAIR file-naming conventions
#'
#' Checks every file's name against the naming conventions from the metacheck
#' "machine readable FAIR data and code" guide: no spaces, special characters
#' or diacritics, an actually-classifiable name
#' (not `data_type == "unknown"`), zero-padded numbering across sibling files,
#' and four path-length budgets (255/228/100/50 characters).
#'
#' @param file_name a character vector of file basenames
#' @param file_path a character vector of repo-relative paths (defaults to
#'   `file_name` when not supplied — length-checks need the full path,
#'   everything else only needs the basename)
#' @param data_type an optional character vector (same length as `file_name`)
#'   of `data_check` semantic types (see `data_classify_files()`), used only
#'   for the classifiability rule
#'
#' @returns a data.frame with one row per naming violation: `file_name`,
#'   `rule`, `severity` (`"bad"` or `"suggestion"` — see Details), `detail`.
#'   0 rows when every file is clean.
#'
#' @details
#' Rules split into two severities. `"bad"`: spaces, special characters,
#' diacritics, `data_type == "unknown"` (the file could not be classified at
#' all), and exceeding the hard 255-character path limit — these break
#' something real. `"suggestion"`: unpadded sibling numbering and the softer
#' 228/100/50 path-length budgets — real conventions worth following, but nothing in
#' metacheck's own classification or ordering logic actually depends on them
#' (classification matches a keyword anywhere in the name with any/no
#' separator — see `data_classify_files()`; nothing sorts files
#' lexicographically for correctness — see `repro_run_order()`'s numeric, not
#' string, tie-break). Letter case (uppercase, CamelCase) and
#' underscore-vs-dash separator consistency are deliberately NOT checked at
#' all, for the same reason: metacheck's classifiers never depend on them.
#' @export
#' @keywords internal
#' @examples
#' check_file_naming(c("data final (2).csv", "analysis.R", "My Data.xlsx"))
check_file_naming <- function(file_name, file_path = file_name,
                              data_type = NA_character_) {
  n <- length(file_name)
  if (n == 0) return(data.frame(
    file_name = character(0), rule = character(0), severity = character(0),
    detail = character(0), stringsAsFactors = FALSE))

  data_type <- rep_len(data_type, n)

  # A name that is not valid UTF-8 (a zip's old Windows name) stopped the
  # checks below ("input string 1 is invalid", U76). Check a valid copy, and
  # give the result the names as they came in, so callers can match them.
  given <- c(file_name, file_path)
  file_name <- .utf8_lines(as.character(file_name))
  file_path <- .utf8_lines(as.character(file_path))
  as_given <- function(x) {
    i <- match(x, c(file_name, file_path))
    ifelse(is.na(i), x, given[i])
  }

  # a file without a name (a platform listing can have one) has nothing to
  # check; it stopped the module ("missing value where TRUE/FALSE needed", U122)
  per_file <- lapply(which(!is.na(file_name)), function(i) {
    df <- .file_naming_check_one(file_name[[i]], data_type[[i]])
    # Add file_name even when 0 rows, so rbind()-ing an all-clean batch still
    # produces a data.frame with a file_name column to select below (a bare
    # 0-row rule/detail frame, with no file_name column at all, otherwise made
    # the later `per_file[, c("file_name", ...)]` error with "undefined
    # columns selected" whenever every checked file was clean).
    df$file_name <- if (nrow(df)) file_name[[i]] else character(0)
    df
  })
  per_file <- do.call(rbind, per_file)
  if (is.null(per_file)) per_file <- data.frame(
    file_name = character(0), rule = character(0), detail = character(0),
    stringsAsFactors = FALSE)
  per_file <- per_file[, c("file_name", "rule", "detail")]

  padding <- .file_naming_check_padding(file_name[!is.na(file_name)])
  length_issues <- .file_naming_check_length(file_path[!is.na(file_path)])

  out <- rbind(per_file, padding, length_issues)
  out$severity <- unname(.file_naming_severity[out$rule])
  out <- out[, c("file_name", "rule", "severity", "detail")]
  if (nrow(out)) out$file_name <- as_given(out$file_name)
  rownames(out) <- NULL
  out
}
