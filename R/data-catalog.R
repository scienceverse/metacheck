# Archive-level catalog for a built data release. A per-paper Psych-DS
# `dataset_description.json` describes ONE study's variables (the DDI-Codebook
# equivalent); a data release also needs a top-level object describing the whole
# collection (the DDI study-group / DDI-Lifecycle "Group" equivalent). The
# web-native standard for "a collection of datasets" is schema.org DataCatalog,
# which Google Dataset Search harvests and which composes with the per-paper
# schema.org Dataset objects the archive already contains. This builds that
# catalog by scanning the archive directory.

# Read a paper's title / authors / DOI / keywords for a catalog entry, preferring
# the paper object (when a paperlist is supplied) and falling back to the paper's
# own dataset_description.json written by convert_psychds().
.catalog_entry_meta <- function(pid, paper_dir, paper = NULL) {
  meta <- list(title = NULL, authors = character(0), doi = NULL,
               keywords = character(0))

  if (!is.null(paper)) {
    info <- paper$info %||% list()
    ival <- function(f) { v <- if (f %in% names(info)) info[[f]] else NULL
                          if (length(v) == 0) NULL else v[[1]] }
    meta$title    <- ival("title")
    meta$doi      <- ival("doi")
    kw <- if ("keywords" %in% names(info)) info$keywords else NULL
    if (length(kw)) meta$keywords <- as.character(kw)
    if (!is.null(paper$author) && nrow(paper$author) > 0) {
      nm <- trimws(paste(paper$author$given %||% "", paper$author$family %||% ""))
      meta$authors <- nm[nzchar(nm)]
    }
  }

  # Fall back to any dataset_description.json in the paper's archive folder.
  if (is.null(meta$title)) {
    dd <- list.files(paper_dir, pattern = "dataset_description[.]json$",
                     recursive = TRUE, full.names = TRUE)
    if (length(dd) > 0) {
      j <- tryCatch(jsonlite::fromJSON(dd[[1]], simplifyVector = FALSE),
                    error = function(e) NULL)
      if (!is.null(j)) {
        meta$title <- meta$title %||% j$name
        if (is.null(meta$doi) && !is.null(j$identifier))
          meta$doi <- sub("^https?://doi.org/", "", as.character(j$identifier))
        if (length(meta$keywords) == 0 && !is.null(j$keywords))
          meta$keywords <- as.character(unlist(j$keywords))
      }
    }
  }
  meta
}

#' Build an archive-level catalog for a data release
#'
#' Scans a built Psych-DS archive directory (as produced by [convert_psychds()]
#' per paper) and writes a top-level `catalog.json`: a schema.org
#' [`DataCatalog`](https://schema.org/DataCatalog) listing every paper's dataset,
#' with its title, authors, DOI, and a pointer to the paper's own
#' `dataset_description.json`. This is the object that makes the release a single
#' discoverable archive rather than a set of unconnected folders; it is harvested
#' by Google Dataset Search and is the schema.org equivalent of a DDI study
#' group (each entry a study, the per-paper descriptor its DDI-Codebook).
#'
#' A companion `catalog.csv` (one row per dataset) is written alongside for quick
#' inspection.
#'
#' @param archive_dir the directory holding the per-paper archive folders (each a
#'   `<paper_id>/` with a Psych-DS `dataset_description.json`)
#' @param papers optional paperlist (see [papers_load()]) used to enrich each
#'   entry with title / authors / DOI when a folder's descriptor lacks them
#' @param name a name for the catalog (e.g. `"Open Mind data release"`)
#' @param description a one-line description of the catalog
#' @param manifest_dir optional directory of per-paper manifests (see
#'   `data_check(manifest=)`); when given, each entry records its file count and
#'   total size from the manifest
#'
#' @returns (invisibly) the path to the written `catalog.json`
#' @export
#' @examples
#' \dontrun{
#' papers <- papers_load("openmind")
#' data_catalog("output/openmind", papers = papers,
#'              name = "Open Mind data release",
#'              manifest_dir = "output/openmind/_manifests")
#' }
data_catalog <- function(archive_dir, papers = NULL,
                         name = "metacheck data release",
                         description = "A collection of Psych-DS datasets built by metacheck.",
                         manifest_dir = NULL) {
  if (!dir.exists(archive_dir))
    stop("archive_dir does not exist: ", archive_dir, call. = FALSE)

  # Per-paper archive folders: subdirectories that contain a dataset_description.
  subdirs <- list.dirs(archive_dir, recursive = FALSE)
  subdirs <- subdirs[!grepl("(^|/)_", basename(subdirs))]  # skip _manifests etc.
  has_desc <- vapply(subdirs, function(d)
    length(list.files(d, pattern = "dataset_description[.]json$",
                      recursive = TRUE)) > 0, logical(1))
  subdirs <- subdirs[has_desc]

  paper_of <- function(pid) if (!is.null(papers) && pid %in% names(papers))
    papers[[pid]] else NULL

  # Optional manifest lookup for file count + total size per paper.
  manifest_stats <- function(pid) {
    if (is.null(manifest_dir)) return(NULL)
    mf <- file.path(manifest_dir, paste0(pid, ".manifest.json"))
    if (!file.exists(mf)) return(NULL)
    j <- tryCatch(jsonlite::fromJSON(mf, simplifyVector = TRUE),
                  error = function(e) NULL)
    if (is.null(j)) return(NULL)
    total <- if (!is.null(j$files) && length(j$files) &&
                 "file_size" %in% names(j$files))
      sum(as.numeric(j$files$file_size), na.rm = TRUE) else NA_real_
    list(n_files = j$n_files %||% NA_integer_, total_bytes = total)
  }

  entries <- list()
  csv_rows <- list()
  for (d in subdirs) {
    pid  <- basename(d)
    meta <- .catalog_entry_meta(pid, d, paper_of(pid))
    ms   <- manifest_stats(pid)

    ds <- list(
      `@type`      = "Dataset",
      name         = meta$title %||% pid,
      identifier   = pid,
      distribution = list(list(
        `@type`         = "DataDownload",
        encodingFormat  = "application/ld+json",
        contentUrl      = file.path(pid,
          list.files(d, pattern = "dataset_description[.]json$",
                     recursive = TRUE)[[1]])))
    )
    if (length(meta$authors) > 0)
      ds$author <- lapply(meta$authors, function(a) list(`@type` = "Person", name = a))
    if (!is.null(meta$doi) && nzchar(meta$doi))
      ds$sameAs <- paste0("https://doi.org/", sub("^https?://doi.org/", "", meta$doi))
    if (length(meta$keywords) > 0) ds$keywords <- as.list(meta$keywords)
    if (!is.null(ms)) {
      ds[["metacheck:fileCount"]]  <- ms$n_files
      ds[["metacheck:totalBytes"]] <- ms$total_bytes
    }
    entries[[length(entries) + 1L]] <- Filter(Negate(is.null), ds)

    csv_rows[[length(csv_rows) + 1L]] <- data.frame(
      paper_id  = pid,
      title     = meta$title %||% "",
      doi       = meta$doi %||% "",
      n_authors = length(meta$authors),
      n_files   = if (!is.null(ms)) ms$n_files else NA_integer_,
      total_mb  = if (!is.null(ms)) round(ms$total_bytes / 1024^2, 1) else NA_real_,
      stringsAsFactors = FALSE)
  }

  catalog <- Filter(Negate(is.null), list(
    `@context`   = "https://schema.org/",
    `@type`      = "DataCatalog",
    name         = name,
    description  = description,
    dateCreated  = format(Sys.Date(), "%Y-%m-%d"),
    `metacheck:generatedBy` = "metacheck::data_catalog()",
    `metacheck:datasetCount` = length(entries),
    dataset      = entries
  ))

  json_path <- file.path(archive_dir, "catalog.json")
  writeLines(jsonlite::toJSON(catalog, auto_unbox = TRUE, pretty = TRUE,
                              na = "null"), json_path, useBytes = TRUE)

  csv_path <- file.path(archive_dir, "catalog.csv")
  utils::write.csv(do.call(rbind, csv_rows), csv_path, row.names = FALSE, na = "")

  message("Wrote catalog for ", length(entries), " datasets:\n  ",
          normalizePath(json_path, mustWork = FALSE), "\n  ",
          normalizePath(csv_path, mustWork = FALSE))
  invisible(json_path)
}
