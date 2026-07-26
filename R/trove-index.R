# trove-index.R — Build a searchable index of metacheck Psych-DS collections.
#
# The trove app (see trove_app()) browses a corpus of metacheck-generated
# Psych-DS collections. Each paper lives in its own root folder containing a
# `collection.json` and one `study-*/` subfolder per study, each with a
# `dataset_description.json`. These functions walk such a tree and flatten it
# into tidy data frames the Shiny app searches over: one row per paper, one per
# study, one per variable, and one per identified scale/task.
#
# This is the R port of the discovery/indexer logic from the PsychTrove viewer
# (https://github.com/levibaruch/PsychTrove), adapted to the field names
# metacheck actually writes (metacheck:scale, metacheck:concept,
# metacheck:measurementLevel, measurementTechnique, metacheck:statistics, ...).

#' Discover metacheck collection roots
#'
#' Scan a directory tree for metacheck Psych-DS collection roots. A collection
#' root is any folder that directly contains a `collection.json` file.
#'
#' @param root directory to scan (recursively) for collection roots
#' @param max_depth maximum directory depth to descend when searching. Scanning
#'   is limited to avoid walking into large `data/` and `raw/` subtrees;
#'   `collection.json` always sits at a paper root near the top of the tree.
#'
#' @return character vector of absolute paths to collection root folders
#' @keywords internal
trove_find_collections <- function(root, max_depth = 4L) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (!dir.exists(root)) {
    stop("`root` is not an existing directory: ", root)
  }

  # Depth-limited walk. list.files(recursive = TRUE) would descend into every
  # data/ and raw/ folder in the corpus (tens of thousands of files); a manual
  # breadth-first walk that stops at max_depth is far cheaper.
  found <- character(0)
  frontier <- root
  depth <- 0L
  while (length(frontier) && depth <= max_depth) {
    next_frontier <- character(0)
    for (d in frontier) {
      entries <- list.files(d, full.names = TRUE, no.. = TRUE)
      if (file.exists(file.path(d, "collection.json"))) {
        found <- c(found, d)
        # A collection root's children are study-*/ dirs, not more collections,
        # so we do not descend further into it.
        next
      }
      subdirs <- entries[dir.exists(entries)]
      next_frontier <- c(next_frontier, subdirs)
    }
    frontier <- next_frontier
    depth <- depth + 1L
  }
  unique(normalizePath(found, winslash = "/", mustWork = FALSE))
}

# `a %||% b` returns `a` unless it is NULL, then `b`. Defined locally so the
# package works on R 4.3 (base R only gained %||% in 4.4).
`%||%` <- function(a, b) if (is.null(a)) b else a

# Read a JSON file, returning NULL (with a warning suppressed) on any failure so
# one malformed file cannot abort a whole corpus index.
.trove_read_json <- function(path) {
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

# Pull a scalar out of a parsed-JSON list, coercing to character; NA if absent.
.trove_chr <- function(x, ...) {
  keys <- c(...)
  for (k in keys) {
    if (!is.null(x[[k]])) {
      v <- x[[k]]
      if (length(v) == 0) return(NA_character_)
      return(as.character(v[[1]]))
    }
  }
  NA_character_
}

# Collapse a list of author objects ({name: ...}) into "A, B, C".
.trove_authors <- function(x) {
  au <- x[["author"]]
  if (is.null(au) || length(au) == 0) return(NA_character_)
  nms <- vapply(au, function(a) {
    if (is.list(a)) as.character(a[["name"]] %||% NA_character_) else as.character(a)
  }, character(1))
  nms <- nms[!is.na(nms) & nzchar(nms)]
  if (!length(nms)) return(NA_character_)
  paste(nms, collapse = ", ")
}

# Collapse a keyword array into "a; b; c".
.trove_keywords <- function(x) {
  kw <- x[["keywords"]]
  if (is.null(kw) || length(kw) == 0) return(NA_character_)
  kw <- unlist(kw, use.names = FALSE)
  paste(kw, collapse = "; ")
}

#' Index a single metacheck collection root
#'
#' Parse one paper folder (containing `collection.json` and `study-*/`
#' subfolders) into per-paper, per-study, per-variable, and per-scale rows.
#'
#' @param path path to a collection root folder
#'
#' @return a list with elements `paper` (1-row data frame), `studies`,
#'   `variables`, and `scales` (data frames, possibly with 0 rows)
#' @keywords internal
trove_index_collection <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  paper_id <- basename(path)

  coll <- .trove_read_json(file.path(path, "collection.json"))
  if (is.null(coll)) coll <- list()

  paper <- data.frame(
    paper_id    = paper_id,
    path        = path,
    title       = .trove_chr(coll, "name"),
    description = .trove_chr(coll, "description"),
    authors     = .trove_authors(coll),
    doi         = .trove_chr(coll, "identifier"),
    keywords    = .trove_keywords(coll),
    date        = .trove_chr(coll, "dateCreated", "metacheck:generated")
  )

  # Study folders: subdirectories named study-* that carry a
  # dataset_description.json. Fall back to a glob if none match the prefix.
  study_dirs <- list.dirs(path, full.names = TRUE, recursive = FALSE)
  study_dirs <- study_dirs[grepl("(^|/)study-", study_dirs)]
  study_dirs <- study_dirs[file.exists(file.path(study_dirs, "dataset_description.json"))]

  studies_list  <- list()
  variables_list <- list()
  scales_list    <- list()

  for (sd in study_dirs) {
    study_group <- sub("^study-", "", basename(sd))
    dd <- .trove_read_json(file.path(sd, "dataset_description.json"))
    if (is.null(dd)) next

    vm <- dd[["variableMeasured"]]
    n_vars <- if (is.null(vm)) 0L else length(vm)

    studies_list[[length(studies_list) + 1L]] <- data.frame(
      paper_id      = paper_id,
      study_group   = study_group,
      study_dir     = basename(sd),
      title         = .trove_chr(dd, "name"),
      description   = .trove_chr(dd, "description"),
      schema_version = .trove_chr(dd, "schemaVersion"),
      n_variables   = n_vars
    )

    if (n_vars == 0L) next

    # Flatten each variable entry. Duplicate (name, source_file) rows are common
    # in these files (a variable repeated once per file it appears in); we keep
    # them here and de-duplicate at the variable-search layer so counts stay
    # honest per study.
    for (v in vm) {
      stats <- v[["metacheck:statistics"]]
      scale <- v[["metacheck:scale"]]
      variables_list[[length(variables_list) + 1L]] <- data.frame(
        paper_id      = paper_id,
        study_group   = study_group,
        name          = .trove_chr(v, "name"),
        label         = .trove_chr(v, "description"),
        concept       = .trove_chr(v, "metacheck:concept"),
        level         = .trove_chr(v, "metacheck:measurementLevel"),
        role          = .trove_chr(v, "metacheck:role"),
        representation = .trove_chr(v, "metacheck:representation"),
        source_file   = .trove_chr(v, "metacheck:sourceFile"),
        scale         = .trove_chr(scale, "name"),
        scale_code    = .trove_chr(scale, "code"),
        technique     = .trove_chr(v, "measurementTechnique"),
        value_pattern = .trove_chr(v, "valuePattern"),
        n             = suppressWarnings(as.numeric(.trove_chr(stats, "n"))),
        mean          = suppressWarnings(as.numeric(.trove_chr(stats, "mean"))),
        sd            = suppressWarnings(as.numeric(.trove_chr(stats, "sd"))),
        min_value     = suppressWarnings(as.numeric(.trove_chr(v, "minValue"))),
        max_value     = suppressWarnings(as.numeric(.trove_chr(v, "maxValue")))
      )
    }
  }

  # Scales/tasks: read the exported OSD definitions under scales/. These are the
  # named instruments metacheck identified — the "scales and tasks we can now
  # code". Each .osd is one instrument; we also fold in scale names seen only in
  # the variable annotations, in case an OSD file was not written.
  osd_dir <- file.path(path, "scales")
  if (dir.exists(osd_dir)) {
    for (f in list.files(osd_dir, pattern = "\\.osd$", full.names = TRUE)) {
      osd <- .trove_read_json(f)
      if (is.null(osd)) next
      def  <- osd[["definition"]] %||% list()
      info <- def[["scale_info"]] %||% list()
      mc   <- def[["metacheck"]] %||% list()
      items <- def[["items"]]
      lik  <- def[["likert_options"]] %||% list()
      scales_list[[length(scales_list) + 1L]] <- data.frame(
        paper_id     = paper_id,
        scale        = .trove_chr(info, "name"),
        code         = .trove_chr(info, "code"),
        abbreviation = .trove_chr(info, "abbreviation"),
        n_items      = if (is.null(items)) NA_integer_ else length(items),
        likert_points = suppressWarnings(as.integer(.trove_chr(lik, "points"))),
        source       = .trove_chr(mc, "scale_source"),
        confidence   = .trove_chr(mc, "confidence"),
        osd_file     = basename(f)
      )
    }
  }

  studies   <- if (length(studies_list))  do.call(rbind, studies_list)  else .trove_empty_studies()
  variables <- if (length(variables_list)) do.call(rbind, variables_list) else .trove_empty_variables()
  scales    <- if (length(scales_list))   do.call(rbind, scales_list)   else .trove_empty_scales()

  list(paper = paper, studies = studies, variables = variables, scales = scales)
}

#' Build a searchable trove index from a corpus root
#'
#' Discover every metacheck collection under `root` and flatten them into four
#' tidy data frames: `papers`, `studies`, `variables`, and `scales`. This is the
#' backing store the trove app searches.
#'
#' @param root directory containing one or more metacheck collection roots
#'   (folders with a `collection.json`). Defaults to the current directory.
#' @param max_depth passed to [trove_find_collections()]
#' @param quiet whether to suppress the progress message
#'
#' @return a list of class `trove_index` with `papers`, `studies`, `variables`,
#'   `scales` data frames plus a `root` attribute
#' @export
#'
#' @examples
#' \dontrun{
#' idx <- trove_index(".")
#' idx$papers
#' }
trove_index <- function(root = ".", max_depth = 4L, quiet = FALSE) {
  roots <- trove_find_collections(root, max_depth = max_depth)
  if (!length(roots)) {
    if (!quiet) message("No collection.json roots found under ", root)
    return(structure(
      list(papers = .trove_empty_papers(), studies = .trove_empty_studies(),
           variables = .trove_empty_variables(), scales = .trove_empty_scales(),
           root = normalizePath(root, winslash = "/", mustWork = FALSE)),
      class = "trove_index"
    ))
  }

  if (!quiet) message("Indexing ", length(roots), " collection",
                      plural(length(roots)), " under ", root, " ...")

  papers <- vector("list", length(roots))
  studies <- vector("list", length(roots))
  variables <- vector("list", length(roots))
  scales <- vector("list", length(roots))
  for (i in seq_along(roots)) {
    one <- tryCatch(trove_index_collection(roots[[i]]), error = function(e) NULL)
    if (is.null(one)) next
    papers[[i]]    <- one$paper
    studies[[i]]   <- one$studies
    variables[[i]] <- one$variables
    scales[[i]]    <- one$scales
  }

  idx <- list(
    papers    = dplyr::bind_rows(papers),
    studies   = dplyr::bind_rows(studies),
    variables = dplyr::bind_rows(variables),
    scales    = dplyr::bind_rows(scales),
    root      = normalizePath(root, winslash = "/", mustWork = FALSE)
  )
  if (!quiet) {
    message("  ", nrow(idx$papers), " papers, ",
            nrow(idx$studies), " studies, ",
            nrow(idx$variables), " variables, ",
            nrow(idx$scales), " scale definitions.")
  }
  structure(idx, class = "trove_index")
}

# Empty-frame templates, so downstream code always sees the same columns even
# when a paper/study/corpus has nothing of a given kind.
.trove_empty_papers <- function() {
  data.frame(paper_id = character(), path = character(), title = character(),
             description = character(), authors = character(), doi = character(),
             keywords = character(), date = character())
}
.trove_empty_studies <- function() {
  data.frame(paper_id = character(), study_group = character(),
             study_dir = character(), title = character(),
             description = character(), schema_version = character(),
             n_variables = integer())
}
.trove_empty_variables <- function() {
  data.frame(paper_id = character(), study_group = character(), name = character(),
             label = character(), concept = character(), level = character(),
             role = character(), representation = character(),
             source_file = character(), scale = character(),
             scale_code = character(), technique = character(),
             value_pattern = character(), n = numeric(), mean = numeric(),
             sd = numeric(), min_value = numeric(), max_value = numeric())
}
.trove_empty_scales <- function() {
  data.frame(paper_id = character(), scale = character(), code = character(),
             abbreviation = character(), n_items = integer(),
             likert_points = integer(), source = character(),
             confidence = character(), osd_file = character())
}

#' Parse a faceted search query
#'
#' Split a search string into field-scoped terms and free terms. A token of the
#' form `field:term` (e.g. `scale:panas`, `level:ordinal`) restricts that term
#' to the named field; a bare token (e.g. `trust`) is a free term matched across
#' all searchable fields. Quote a value to include spaces: `scale:"just world"`.
#' All terms are AND-combined.
#'
#' @param query the raw query string
#' @param fields character vector of valid field names. A `field:` token whose
#'   field is not in this list is treated as a free term (so a stray colon does
#'   not silently drop the term).
#'
#' @return a list with `field_terms` (named list: field -> character vector of
#'   terms) and `free_terms` (character vector)
#' @keywords internal
trove_parse_query <- function(query, fields = character()) {
  query <- if (is.null(query)) "" else trimws(query)
  out <- list(field_terms = list(), free_terms = character())
  if (!nzchar(query)) return(out)

  # Tokenise on whitespace, but keep quoted spans together.
  toks <- regmatches(query, gregexpr('(?:[^\\s"]|"[^"]*")+', query, perl = TRUE))[[1]]
  for (tok in toks) {
    m <- regmatches(tok, regexec('^([A-Za-z_]+):(.*)$', tok))[[1]]
    if (length(m) == 3 && m[2] %in% fields) {
      fld <- m[2]
      val <- gsub('^"|"$', '', m[3])
      if (nzchar(val)) {
        out$field_terms[[fld]] <- c(out$field_terms[[fld]], val)
      }
    } else {
      val <- gsub('^"|"$', '', tok)
      if (nzchar(val)) out$free_terms <- c(out$free_terms, val)
    }
  }
  out
}

#' Apply a parsed query to a data frame
#'
#' Return a logical vector marking rows that match a parsed query (see
#' [trove_parse_query()]). Field terms are matched against their own column;
#' free terms are matched against every column in `free_cols`. Matching is
#' case-insensitive substring. All terms must match (AND).
#'
#' @param df a data frame to search
#' @param parsed a parsed query from [trove_parse_query()]
#' @param free_cols columns a free (unscoped) term may match against
#'
#' @return logical vector of length `nrow(df)`
#' @keywords internal
trove_match <- function(df, parsed, free_cols) {
  n <- nrow(df)
  keep <- rep(TRUE, n)

  contains <- function(col, term) {
    if (is.null(col)) return(rep(FALSE, n))
    hit <- grepl(term, col, ignore.case = TRUE, fixed = FALSE)
    hit[is.na(hit)] <- FALSE
    hit
  }

  # Field-scoped terms: each restricted to its own column.
  for (fld in names(parsed$field_terms)) {
    col <- if (fld %in% names(df)) df[[fld]] else NULL
    for (term in parsed$field_terms[[fld]]) {
      keep <- keep & contains(col, term)
    }
  }

  # Free terms: must appear in at least one of the free columns.
  free_cols <- intersect(free_cols, names(df))
  for (term in parsed$free_terms) {
    any_hit <- rep(FALSE, n)
    for (fc in free_cols) any_hit <- any_hit | contains(df[[fc]], term)
    keep <- keep & any_hit
  }

  keep
}

#' @export
print.trove_index <- function(x, ...) {
  cat("<trove_index>\n")
  cat("  root:      ", x$root, "\n", sep = "")
  cat("  papers:    ", nrow(x$papers), "\n", sep = "")
  cat("  studies:   ", nrow(x$studies), "\n", sep = "")
  cat("  variables: ", nrow(x$variables), "\n", sep = "")
  cat("  scales:    ", nrow(x$scales), "\n", sep = "")
  invisible(x)
}
