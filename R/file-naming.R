# File-naming convention checks, per the metacheck "machine readable FAIR data
# and code" guide: no spaces or special characters, avoid CamelCase, actually
# be classifiable, use zero-padded numbering for lexicographic sort, use
# YYYYMMDD dates, and stay within path-length budgets a typical download would
# need. Used by repo_check() to surface naming problems alongside its other
# warnings.
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
.file_naming_severity <- c(
  "spaces"               = "bad",
  "special-characters"   = "bad",
  "diacritics"           = "bad",
  "date-format"          = "bad",
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

  # Dates: a run of exactly 8 digits should be YYYYMMDD, not some other
  # 8-digit number (heuristic: treat any 8-digit run as a claimed date and
  # validate it, since 8 consecutive digits in a filename is otherwise rare).
  date_hits <- regmatches(stem, gregexpr("[0-9]{8}", stem))[[1]]
  for (d in date_hits) {
    ok <- tryCatch(!is.na(as.Date(d, format = "%Y%m%d")), error = function(e) FALSE)
    if (!ok) add("date-format", sprintf("'%s' is not a valid YYYYMMDD date", d))
  }

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
  add_rows <- function(hit, rule, budget, actual) {
    if (!any(hit)) return(invisible())
    out[[length(out) + 1L]] <<- data.frame(
      file_name = file_path[hit], rule = rule,
      detail = sprintf("%s is %d characters, over the %d-character budget",
                       rule, actual[hit], budget),
      stringsAsFactors = FALSE)
  }
  add_rows(nchar(file_path) > 255L, "path-length-255", 255L, nchar(file_path))
  add_rows(nchar(file_path) > 228L, "path-length-228", 228L, nchar(file_path))
  add_rows(nchar(dir_part) > 100L, "directory-length-100", 100L, nchar(dir_part))
  add_rows(nchar(base_part) > 50L, "filename-length-50", 50L, nchar(base_part))

  if (length(out)) do.call(rbind, out) else data.frame(
    file_name = character(0), rule = character(0), detail = character(0),
    stringsAsFactors = FALSE)
}

#' Check repository files against the FAIR file-naming conventions
#'
#' Checks every file's name against the naming conventions from the metacheck
#' "machine readable FAIR data and code" guide: no spaces or special
#' characters, lowercase only, no CamelCase, an actually-classifiable name
#' (not `data_type == "unknown"`), zero-padded numbering across sibling files,
#' YYYYMMDD dates, and four path-length budgets (255/228/100/50 characters).
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
#' diacritics, invalid YYYYMMDD dates, `data_type == "unknown"` (the file
#' could not be classified at all), and exceeding the hard 255-character path
#' limit — these break something real. `"suggestion"`: uppercase letters,
#' CamelCase, unpadded sibling numbering, and the softer 228/100/50
#' path-length budgets — real conventions worth following, but nothing in
#' metacheck's own classification or ordering logic actually depends on them
#' (classification matches a keyword anywhere in the name with any/no
#' separator — see `data_classify_files()`; nothing sorts files
#' lexicographically for correctness — see `repro_run_order()`'s numeric, not
#' string, tie-break). Underscore-vs-dash separator consistency is
#' deliberately NOT checked at all, for the same reason: metacheck's
#' classifiers never depend on it.
#' @export
#' @keywords internal
check_file_naming <- function(file_name, file_path = file_name,
                              data_type = NA_character_) {
  n <- length(file_name)
  if (n == 0) return(data.frame(
    file_name = character(0), rule = character(0), severity = character(0),
    detail = character(0), stringsAsFactors = FALSE))

  data_type <- rep_len(data_type, n)

  per_file <- lapply(seq_len(n), function(i) {
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

  padding <- .file_naming_check_padding(file_name)
  length_issues <- .file_naming_check_length(file_path)

  out <- rbind(per_file, padding, length_issues)
  out$severity <- unname(.file_naming_severity[out$rule])
  out <- out[, c("file_name", "rule", "severity", "detail")]
  rownames(out) <- NULL
  out
}
