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

# Expand a downloaded zip and return rows for its DATA-type inner files, ready to
# bind into data_check's `all_files`. The zip is extracted once to a cache dir
# beside the zip; each inner file is classified, and only files whose type is
# data/codebook/readme (and not in skip_types) are returned — assets and other
# inner content are left in the extraction dir but not added to the archive (the
# original zip remains the link for them). `zip_row` is the zip's own row in
# all_files, whose repo/paper/group fields the inner rows inherit.
#
# Returns a data.frame with the same columns as `zip_row` (one per kept inner
# file, file_location pointing at the extracted copy), or a 0-row frame.
.expand_zip <- function(zip_path, zip_row, skip_types = "asset") {
  empty <- zip_row[0, , drop = FALSE]
  if (is.null(zip_path) || is.na(zip_path) || !file.exists(zip_path)) return(empty)

  # Extract into "<zip>.contents/" beside the cached zip (idempotent: reuse).
  dest <- paste0(zip_path, ".contents")
  entries <- tryCatch(utils::unzip(zip_path, list = TRUE), error = function(e) NULL)
  if (is.null(entries) || nrow(entries) == 0) return(empty)
  if (!dir.exists(dest))
    tryCatch(utils::unzip(zip_path, exdir = dest),
             error = function(e) NULL)

  inner <- entries$Name
  inner <- inner[!grepl("/$", inner)]                 # drop directory entries
  inner <- inner[!grepl("(^|/)__MACOSX/", inner)]     # drop mac resource forks
  if (length(inner) == 0) return(empty)

  types  <- data_classify_files(basename(inner))
  fmt    <- data_format(tolower(tools::file_ext(inner)))
  # Keep only genuinely archivable data/codebook/readme content; never keep
  # skip_types (e.g. asset) inner files.
  keep <- types %in% c("data", "codebook", "readme") & !(types %in% skip_types)
  if (!any(keep)) return(empty)

  inner <- inner[keep]; types <- types[keep]; fmt <- fmt[keep]
  loc <- file.path(dest, inner)
  ok  <- file.exists(loc)
  inner <- inner[ok]; types <- types[ok]; fmt <- fmt[ok]; loc <- loc[ok]
  if (length(inner) == 0) return(empty)

  # Build rows inheriting the zip's repo/paper/group, with the inner path.
  rows <- zip_row[rep(1, length(inner)), , drop = FALSE]
  rows$file_name     <- basename(inner)
  # Record the inner path within the zip, prefixed with the zip name for context.
  rows$file_path     <- file.path(paste0(basename(zip_path)), inner)
  rows$file_location <- loc
  rows$file_size     <- suppressWarnings(file.size(loc))
  if ("data_type" %in% names(rows))   rows$data_type   <- types
  if ("data_format" %in% names(rows)) rows$data_format <- fmt
  # These inner files came from a local extraction, not a remote URL of their own.
  if ("file_url" %in% names(rows))    rows$file_url    <- NA_character_
  rows
}

#' Decide whether a remote ZIP is worth downloading for a data archive
#'
#' Peeks inside the zip (see [zip_peek()]) and classifies its entries. A zip is
#' worth downloading if it contains actual data or a codebook; a zip of only
#' stimuli/materials is better linked than mirrored.
#'
#' @param url the zip's download URL
#' @param skip_types data_type(s) that don't count as worth-downloading content
#'   (e.g. `"asset"`)
#'
#' @returns a list with `worth` (`TRUE`/`FALSE`, or `NA` when the peek failed so
#'   the caller can fall back to downloading), `reason`, `n_entries`, `types`
#'   (table of inner types), and `contents` (the peeked data.frame or `NULL`).
#' @export
#' @keywords internal
zip_decision <- function(url, skip_types = "asset") {
  peek <- zip_peek(url)
  if (is.null(peek))
    return(list(worth = NA, reason = "could not peek (download to inspect)",
                n_entries = NA_integer_, types = NULL, contents = NULL))
  types <- data_classify_files(basename(peek$name))
  # "worth it" = contains actual research data or a codebook (not just stimuli,
  # instructions, or media). A zip of only assets/readmes is linked, not
  # mirrored, matching the archive intent "we host data, link to materials".
  worth <- any(types %in% c("data", "codebook")) &&
    !all(types %in% skip_types)
  list(
    worth     = worth,
    reason    = if (worth) "contains archivable content"
                else "only skipped types inside (link, do not mirror)",
    n_entries = nrow(peek),
    types     = table(types),
    contents  = peek)
}
