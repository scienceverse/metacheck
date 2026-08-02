# Peek inside a remote ZIP without downloading it. A ZIP stores its file listing
# (the "central directory") at the END of the archive, so an HTTP range request
# for the tail is enough to read every entry's name and uncompressed size. This
# lets the downloader decide whether a zip is worth fetching (does it hold data,
# or only stimuli/assets?) before spending the bandwidth. Falls back to NULL when
# the host doesn't honour ranges or the central directory isn't in the tail, so
# the caller can download-and-inspect instead.

# Little-endian integer from a raw vector slice (1-based, `n` bytes).
.le_int <- function(raw, at, n) {
  bytes <- as.integer(raw[at:(at + n - 1)])
  sum(bytes * 256^(seq_len(n) - 1))
}

# Fetch the last `n` bytes of a URL via an HTTP Range request. Returns the raw
# bytes (possibly fewer than n if the file is smaller), or NULL on failure / if
# the server ignored the range (returned 200 with the whole body).
.http_range_tail <- function(url, n, total = NULL) {
  tryCatch({
    if (is.null(total)) {
      h <- httr2::request(url) |> httr2::req_method("HEAD") |>
        httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform()
      total <- suppressWarnings(as.numeric(
        httr2::resp_header(h, "content-length")))
    }
    if (is.null(total) || is.na(total) || total <= 0) return(NULL)
    start <- max(0, total - n)
    r <- httr2::request(url) |>
      httr2::req_headers(Range = sprintf("bytes=%.0f-%.0f", start, total - 1)) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    # 206 = partial content (range honoured). 200 = whole file (range ignored):
    # only usable if it's small enough that we got the tail anyway.
    if (httr2::resp_status(r) == 206) return(httr2::resp_body_raw(r))
    if (httr2::resp_status(r) == 200) {
      body <- httr2::resp_body_raw(r)
      return(utils::tail(body, n))
    }
    NULL
  }, error = function(e) NULL)
}

# Parse ZIP central-directory entries from a raw tail that ends at the true end
# of the file. Returns a data.frame(name, size) of uncompressed sizes, or NULL if
# the End-Of-Central-Directory record isn't present in the tail.
.parse_zip_central_dir <- function(raw) {
  n <- length(raw)
  if (n < 22) return(NULL)
  eocd_sig <- as.raw(c(0x50, 0x4b, 0x05, 0x06))   # "PK\05\06"
  cd_sig   <- as.raw(c(0x50, 0x4b, 0x01, 0x02))   # "PK\01\02" central-dir header

  # Find the EOCD signature, scanning from the end (comment may follow it).
  eocd <- NA_integer_
  for (i in (n - 21):1) {
    if (identical(raw[i:(i + 3)], eocd_sig)) { eocd <- i; break }
    if (i < n - 21 - 65536) break   # comment can't exceed 64KB; stop early
  }
  if (is.na(eocd)) return(NULL)

  # EOCD fields (offsets relative to the signature start):
  #   +10 (2) total entries, +12 (4) central-dir size, +16 (4) central-dir offset
  n_entries <- .le_int(raw, eocd + 10, 2)
  cd_size   <- .le_int(raw, eocd + 12, 4)

  # The central directory ends right before the EOCD record and is cd_size bytes
  # long, so within our tail it starts at (eocd - cd_size) when fully present.
  cd_start <- eocd - cd_size
  if (cd_start < 1) return(NULL)   # central directory not fully in the tail

  names <- character(0); sizes <- numeric(0)
  p <- cd_start
  for (e in seq_len(n_entries)) {
    if (p + 46 > n) break
    if (!identical(raw[p:(p + 3)], cd_sig)) break
    usize   <- .le_int(raw, p + 24, 4)   # uncompressed size
    name_len <- .le_int(raw, p + 28, 2)
    extra_len <- .le_int(raw, p + 30, 2)
    comm_len  <- .le_int(raw, p + 32, 2)
    nm <- rawToChar(raw[(p + 46):(p + 46 + name_len - 1)])
    Encoding(nm) <- "UTF-8"
    names <- c(names, nm); sizes <- c(sizes, usize)
    p <- p + 46 + name_len + extra_len + comm_len
  }
  if (length(names) == 0) return(NULL)
  data.frame(name = names, size = sizes, stringsAsFactors = FALSE)
}

#' Peek at the contents of a remote ZIP file without downloading it
#'
#' Uses an HTTP range request to fetch only the tail of a ZIP archive and read
#' its central directory, returning the name and uncompressed size of every entry
#' inside. This lets a downloader decide whether a zip is worth fetching (does it
#' contain data, or only stimuli/assets?) before spending the bandwidth.
#'
#' @param url the download URL of a ZIP file
#' @param tail_bytes how many bytes of the tail to fetch (default 128 KB; raised
#'   automatically on retry when the central directory is larger)
#'
#' @returns a data.frame with columns `name` (entry path inside the zip) and
#'   `size` (uncompressed bytes), excluding directory entries; or `NULL` when the
#'   host does not support range requests or the listing cannot be read (the
#'   caller should then download-and-inspect).
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' zip_peek("https://osf.io/download/abcde/")
#' }
zip_peek <- function(url, tail_bytes = 131072) {
  # HEAD once for the total size (also lets us grab a bigger tail if needed).
  total <- tryCatch({
    h <- httr2::request(url) |> httr2::req_method("HEAD") |>
      httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform()
    suppressWarnings(as.numeric(httr2::resp_header(h, "content-length")))
  }, error = function(e) NA_real_)

  for (nb in unique(c(tail_bytes, 1048576))) {   # retry once with 1 MB tail
    raw <- .http_range_tail(url, nb, total = total)
    if (is.null(raw)) return(NULL)
    cd <- .parse_zip_central_dir(raw)
    if (!is.null(cd)) {
      cd <- cd[!grepl("/$", cd$name), , drop = FALSE]   # drop directory entries
      return(cd)
    }
    if (!is.null(total) && !is.na(total) && nb >= total) break  # whole file seen
  }
  NULL
}

# ── Archive-format classification ────────────────────────────────────────────
# Which archive/compression formats metacheck can OPEN with base R alone:
#   * zip            → utils::unzip                (also peekable without download)
#   * tar family     → utils::untar                (.tar[.gz|.bz2|.xz], .tgz, ...)
#   * single-file gz → gz/bz2/xz connection        (one compressed file, no index)
# Everything else archive-shaped (.7z, .rar, .cab, .arj, .lzma, ...) needs an
# external binary or the libarchive-backed `archive` package, which metacheck
# does NOT depend on — those are left as `other` (never fetched) and repo_check
# warns the author to re-upload as .zip. `untar` FAILS on a bare .gz that is not
# a tarball ("Missing type keyword"), so single-file compressions are a distinct
# class handled by connection, not by untar.
.is_zip <- function(name)
  grepl("[.]zip$", name, ignore.case = TRUE)
.is_tar_archive <- function(name)
  grepl("[.](tar|tar[.]gz|tgz|tar[.]bz2|tbz2?|tar[.]xz|txz)$", name,
        ignore.case = TRUE)
# Bare single-file compressions (NOT .tar.*): one file inside, classified by the
# name with the compression suffix stripped (results.csv.gz → results.csv).
.is_single_compress <- function(name)
  grepl("[.](gz|bz2|xz)$", name, ignore.case = TRUE) & !.is_tar_archive(name)
# Archives base R can read (and therefore worth downloading despite `unknown`).
.is_readable_archive <- function(name)
  .is_zip(name) | .is_tar_archive(name) | .is_single_compress(name)

# Shared back end for the three .expand_* functions. Called AFTER extraction: it
# walks what is actually on disk under `dest` (rather than trusting the archive's
# listed member names, which tar may rewrite — stripping drive letters or leading
# "/"/"./" — so listed paths need not match the extracted ones), classifies each
# file, keeps only data or documentation (readme/codebook, by doc_role) content
# (never skip_types), and builds rows inheriting `archive_row`'s repo/paper/group
# fields. `label` prefixes file_path for provenance (the container name).
# Returns a 0-row frame when nothing worth keeping is inside — which is how an
# archive of only materials ends up contributing nothing, exactly as if it had
# not been downloaded.
.archive_rows <- function(dest, archive_row, label, skip_types) {
  empty <- archive_row[0, , drop = FALSE]
  if (!dir.exists(dest)) return(empty)

  # Paths relative to the extraction root (no full.names → no prefix to strip,
  # so no fragile path-regex). loc is rebuilt with file.path for the real files.
  rel <- list.files(dest, recursive = TRUE, all.files = TRUE)
  rel <- rel[!grepl("(^|/)__MACOSX/", rel)]           # drop mac resource forks
  if (length(rel) == 0) return(empty)
  loc <- file.path(dest, rel)

  types <- data_classify_files(basename(loc))
  roles <- .data_doc_role(basename(loc))
  fmt   <- data_format(tolower(tools::file_ext(loc)))
  keep  <- (types == "data" |
              (types == "documentation" & !is.na(roles) & roles %in% c("codebook", "readme"))) &
    !(types %in% skip_types)
  if (!any(keep)) return(empty)

  loc <- loc[keep]; rel <- rel[keep]; types <- types[keep]
  roles <- roles[keep]; fmt <- fmt[keep]

  rows <- archive_row[rep(1, length(loc)), , drop = FALSE]
  rows$file_name     <- basename(loc)
  rows$file_path     <- file.path(label, rel)   # inner path, prefixed for context
  rows$file_location <- loc
  rows$file_size     <- suppressWarnings(file.size(loc))
  if ("data_type" %in% names(rows))   rows$data_type   <- types
  if ("doc_role" %in% names(rows))    rows$doc_role    <- roles
  if ("data_format" %in% names(rows)) rows$data_format <- fmt
  # Inner files came from a local extraction, not a remote URL of their own.
  if ("file_url" %in% names(rows))    rows$file_url    <- NA_character_
  rows
}

# Expand a downloaded zip and return rows for its DATA-type inner files, ready to
# bind into data_check's `all_files`. The zip is extracted once to a cache dir
# beside the zip; each inner file is classified, and only files whose type is
# data or documentation with doc_role readme/codebook (and not in skip_types)
# are returned — materials and other inner content are left in the extraction
# dir but not added to the archive (the original zip remains the link for
# them). `zip_row` is the zip's own row in all_files, whose repo/paper/group
# fields the inner rows inherit.
#
# Returns a data.frame with the same columns as `zip_row` (one per kept inner
# file, file_location pointing at the extracted copy), or a 0-row frame.
.expand_zip <- function(zip_path, zip_row, skip_types = "materials") {
  empty <- zip_row[0, , drop = FALSE]
  if (is.null(zip_path) || is.na(zip_path) || !file.exists(zip_path)) return(empty)

  # Extract into "<zip>.contents/" beside the cached zip (idempotent: reuse).
  dest <- paste0(zip_path, ".contents")
  entries <- tryCatch(utils::unzip(zip_path, list = TRUE), error = function(e) NULL)
  if (is.null(entries) || nrow(entries) == 0) return(empty)
  if (!dir.exists(dest))
    tryCatch(utils::unzip(zip_path, exdir = dest),
             error = function(e) NULL)

  .archive_rows(dest, zip_row, basename(zip_path), skip_types)
}

# Expand a downloaded tar-family archive (.tar / .tar.gz / .tgz / .tar.bz2 /
# .tar.xz / ...). Mirror of .expand_zip using base R's utils::untar (no extra
# dependency). Peeking is impossible for tar (no tail index), so the caller must
# have downloaded it first; the size caps still bound what gets fetched. A tar we
# cannot read (corrupt, or an unusual variant) degrades to a 0-row frame rather
# than erroring the run.
.expand_tar <- function(tar_path, tar_row, skip_types = "materials") {
  empty <- tar_row[0, , drop = FALSE]
  if (is.null(tar_path) || is.na(tar_path) || !file.exists(tar_path)) return(empty)

  dest <- paste0(tar_path, ".contents")
  entries <- tryCatch(utils::untar(tar_path, list = TRUE), error = function(e) NULL,
                      warning = function(w) NULL)
  if (is.null(entries) || length(entries) == 0) return(empty)
  if (!dir.exists(dest))
    tryCatch(utils::untar(tar_path, exdir = dest),
             error = function(e) NULL, warning = function(w) NULL)

  .archive_rows(dest, tar_row, basename(tar_path), skip_types)
}

# Expand a downloaded single-file compression (.gz / .bz2 / .xz that is NOT a
# .tar.*). These wrap exactly ONE file; there is no listing, so we decompress the
# stream to the name with the compression suffix stripped (results.csv.gz →
# results.csv) and classify that one file. Uses base R connections only.
.expand_compressed <- function(gz_path, gz_row, skip_types = "materials") {
  empty <- gz_row[0, , drop = FALSE]
  if (is.null(gz_path) || is.na(gz_path) || !file.exists(gz_path)) return(empty)

  ext <- tolower(tools::file_ext(gz_path))
  open_con <- switch(ext,
    gz  = gzfile, bz2 = bzfile, xz = xzfile, NULL)
  if (is.null(open_con)) return(empty)

  dest <- paste0(gz_path, ".contents")
  inner_name <- sub("[.](gz|bz2|xz)$", "", basename(gz_path), ignore.case = TRUE)
  out <- file.path(dest, inner_name)
  if (!file.exists(out)) {
    ok <- tryCatch({
      if (!dir.exists(dest)) dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      con <- open_con(gz_path, "rb"); on.exit(close(con), add = TRUE)
      oc  <- file(out, "wb"); on.exit(close(oc), add = TRUE)
      repeat {
        chunk <- readBin(con, "raw", n = 1048576L)
        if (length(chunk) == 0) break
        writeBin(chunk, oc)
      }
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!isTRUE(ok)) return(empty)
  }

  .archive_rows(dest, gz_row, basename(gz_path), skip_types)
}

#' Decide whether a remote ZIP is worth downloading for a data archive
#'
#' Peeks inside the zip (see [zip_peek()]) and classifies its entries. A zip is
#' worth downloading if it contains actual data or a codebook; a zip of only
#' stimuli/software is better linked than mirrored.
#'
#' @param url the zip's download URL
#' @param skip_types data_type(s) that don't count as worth-downloading content
#'   (e.g. `"materials"`)
#'
#' @returns a list with `worth` (`TRUE`/`FALSE`, or `NA` when the peek failed so
#'   the caller can fall back to downloading), `reason`, `n_entries`, `types`
#'   (table of inner types), and `contents` (the peeked data.frame or `NULL`).
#' @export
#' @keywords internal
zip_decision <- function(url, skip_types = "materials") {
  peek <- zip_peek(url)
  if (is.null(peek))
    return(list(worth = NA, reason = "could not peek (download to inspect)",
                n_entries = NA_integer_, types = NULL, contents = NULL))
  types <- data_classify_files(basename(peek$name))
  roles <- .data_doc_role(basename(peek$name))
  # "worth it" = contains actual research data or a codebook (not just stimuli,
  # instructions, or media). A zip of only materials/readmes is linked, not
  # mirrored, matching the archive intent "we host data, link to materials".
  worth <- any(types == "data" |
                (types == "documentation" & !is.na(roles) & roles == "codebook")) &&
    !all(types %in% skip_types)
  list(
    worth     = worth,
    reason    = if (worth) "contains archivable content"
                else "only skipped types inside (link, do not mirror)",
    n_entries = nrow(peek),
    types     = table(types),
    contents  = peek)
}
