# Listing a plain (uncompressed) .tar archive. Unlike a zip, a tar has no index
# at its end (see zip_peek()): every member is preceded by its own 512-byte
# header holding its name and size, so the names are spread through the whole
# archive. Reading them one header at a time costs one request per member
# (134 requests for a 5 MB Zenodo archive of 133 files, after which Zenodo
# answered 429; measured 2026-10-16), so a tar is instead downloaded ONCE,
# whole, when its size is known and at most `max_bytes`, and its headers are
# read from that copy. The copy is kept for the rest of the session, so taking
# members out of it later (.tar_fetch_members(), download_repo_files()) costs
# no further request. A compressed tar (.tar.gz, ...) is not listed here: its
# headers are compressed with everything else.

# url -> listing (or NULL), for this session
.tar_peek_cache <- new.env(parent = emptyenv())
# url -> path of the archive's copy downloaded while listing it this session
.tar_peek_copies <- new.env(parent = emptyenv())

# The largest .tar that is downloaded to list it (50 MB).
.tar_peek_max_bytes <- 50 * 1024^2

# Text of a NUL-padded header field.
.tar_str <- function(r) {
  z <- which(r == as.raw(0))
  if (length(z)) r <- r[seq_len(z[[1]] - 1L)]
  if (!length(r)) return("")
  x <- rawToChar(r)
  Encoding(x) <- "UTF-8"
  x
}

# A numeric header field: octal digits, or base-256 when the first byte's high
# bit is set (GNU tar's form for sizes of 8 GB and more). Computed in doubles,
# since strtoi() stops at 2^31.
.tar_num <- function(r) {
  if (length(r) && bitwAnd(as.integer(r[[1]]), 0x80L) != 0L) {
    v <- as.integer(r)
    v[[1]] <- bitwAnd(v[[1]], 0x7FL)
    return(sum(v * 256^(rev(seq_along(v)) - 1)))
  }
  s <- gsub("[^0-7]", "", .tar_str(r))
  if (!nzchar(s)) return(0)
  d <- as.integer(strsplit(s, "", fixed = TRUE)[[1]])
  sum(d * 8^(rev(seq_along(d)) - 1))
}

# Whether a 512-byte block is a valid tar header: its checksum (the sum of its
# bytes, with the checksum field itself counted as spaces) matches the stored
# value. Old tar programs summed signed bytes, so that sum is accepted too.
.tar_checksum_ok <- function(h) {
  stored <- .tar_num(h[149:156])
  v <- as.integer(h)
  v[149:156] <- 32L
  signed <- ifelse(v > 127L, v - 256L, v)
  stored == sum(v) || stored == sum(signed)
}

# The records of a pax extended header ("<length> <key>=<value>\n" ...) as a
# named list.
.tar_pax <- function(r) {
  out <- list()
  p <- 1L
  n <- length(r)
  while (p <= n) {
    sp <- which(r[p:n] == as.raw(0x20))
    if (!length(sp)) break
    len <- suppressWarnings(as.integer(rawToChar(r[p:(p + sp[[1]] - 2L)])))
    if (is.na(len) || len <= 0 || p + len - 1L > n) break
    rec <- r[(p + sp[[1]]):(p + len - 2L)]      # without the trailing newline
    eq <- which(rec == as.raw(0x3D))
    # a record with a null byte is binary, such as a file's extended
    # attributes (SCHILY.xattr.*, written by tar on a Mac): only path and size
    # are used, and rawToChar() stopped on it ("embedded nul in string",
    # PsyArXiv preprint 4z75t_v1)
    if (length(eq) && any(rec == as.raw(0))) eq <- integer(0)
    if (length(eq)) {
      key <- rawToChar(rec[seq_len(eq[[1]] - 1L)])
      val <- if (eq[[1]] < length(rec)) rawToChar(rec[(eq[[1]] + 1L):length(rec)]) else ""
      Encoding(val) <- "UTF-8"
      out[[key]] <- val
    }
    p <- p + len
  }
  out
}

# List the regular files of a .tar on disk from its headers, with the columns
# `name` (the path inside the archive, without a leading "./"), `size` (bytes)
# and `member` (the name exactly as stored, which utils::untar(files = ) needs).
# Long names are read from the POSIX "ustar" prefix field, from a pax extended
# header ("path", also "size") and from GNU tar's long-name entries; pax
# global headers, directories and links are skipped. NULL when the file is not
# a tar (its first header fails the checksum) or a later header is damaged.
.tar_list_file <- function(path) {
  total <- file.size(path)
  if (is.na(total) || total < 512) return(NULL)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  name <- character(0); size <- numeric(0)
  pos <- 0
  pax <- list(); long_name <- NULL
  while (pos + 512 <= total) {
    seek(con, pos)
    h <- readBin(con, "raw", 512L)
    if (length(h) < 512L || all(h == as.raw(0))) break   # end of the archive
    if (!.tar_checksum_ok(h)) return(NULL)
    type <- if (h[[157]] == as.raw(0)) "0" else rawToChar(h[157])
    len <- .tar_num(h[125:136])
    content <- function() if (len > 0) readBin(con, "raw", len) else raw(0)

    if (type == "x") {
      pax <- .tar_pax(content())
    } else if (type == "L") {
      long_name <- .tar_str(content())
    } else if (type %in% c("0", "7")) {
      nm <- .tar_str(h[1:100])
      # POSIX ustar ("ustar\0" + "00") keeps the start of a long path in the
      # prefix field; GNU tar ("ustar  ") uses those bytes for other things.
      if (identical(h[258:263], c(charToRaw("ustar"), as.raw(0)))) {
        prefix <- .tar_str(h[346:500])
        if (nzchar(prefix)) nm <- paste0(prefix, "/", nm)
      }
      if (!is.null(long_name)) nm <- long_name
      if (!is.null(pax$path)) nm <- pax$path
      sz <- suppressWarnings(as.numeric(pax$size %||% NA))
      name <- c(name, nm)
      size <- c(size, if (is.na(sz)) len else sz)
      if (!is.na(sz)) len <- sz
      pax <- list(); long_name <- NULL
    } else if (type != "g") {
      # a directory, link or device: the extended name was for it
      pax <- list(); long_name <- NULL
    }
    pos <- pos + 512 + ceiling(len / 512) * 512
  }
  data.frame(name = sub("^(\\./)+", "", name), size = size, member = name,
             stringsAsFactors = FALSE)
}

#' List the files inside a remote plain .tar archive
#'
#' Downloads the archive once, whole, when its size is known and at most
#' `max_bytes`, and reads its file names and sizes from that copy (a tar has
#' no index that a partial download could read, see the top of this file).
#' The copy is kept for the session, so members taken from it later need no
#' further request.
#'
#' @param url the download URL of a `.tar` file, or the path of one on disk
#'   (then read directly, without any request)
#' @param size the archive's size in bytes, from the repository's listing.
#'   An archive whose size is unknown or over `max_bytes` is not downloaded,
#'   and `NULL` is returned.
#' @param max_bytes the largest archive that is downloaded to list it
#' @param cache if `TRUE`, keep the listing on disk for later sessions (see
#'   [zip_peek_cache_clear()])
#' @param skip_on_api_limit see [download_repo_files()]
#'
#' @returns a data.frame with columns `name`, `size` and `member` (see
#'   `.tar_list_file()`), or `NULL` when the archive was not listed
#' @keywords internal
.tar_peek <- function(url, size = NULL, max_bytes = .tar_peek_max_bytes,
                      cache = FALSE, skip_on_api_limit = FALSE) {
  if (exists(url, envir = .tar_peek_cache, inherits = FALSE))
    return(get(url, envir = .tar_peek_cache, inherits = FALSE))
  if (.is_local_file(url)) return(.tar_list_file(url))

  # Keyed apart from zip_peek()'s entries for the same URL.
  key <- paste0("tar:", url)
  if (isTRUE(cache) && .zip_peek_cache_has(key)) {
    listing <- .zip_peek_cache_get(key)
    assign(url, listing, envir = .tar_peek_cache)
    return(listing)
  }

  known <- suppressWarnings(as.numeric(size %||% NA_real_))[1]
  if (!isTRUE(known > 0 && known <= max_bytes)) return(NULL)

  copy <- tempfile(fileext = ".tar")
  err <- .download_one(url, copy, skip_on_api_limit = skip_on_api_limit,
                       expected_bytes = known)
  # A short file is a broken transfer, not a short archive.
  complete <- is.na(err) && isTRUE(file.size(copy) == known)
  listing <- if (complete) .tar_list_file(copy) else NULL
  if (is.null(listing)) unlink(copy) else assign(url, copy, envir = .tar_peek_copies)
  assign(url, listing, envir = .tar_peek_cache)
  # A failed download may succeed later, so only what a complete download
  # showed is kept on disk.
  if (isTRUE(cache) && complete) .zip_peek_cache_put(key, listing)
  listing
}

# Take the members `names` (as stored, the `member` column of .tar_peek()) out
# of a plain .tar into `dest`, with the same return value as
# .zip_fetch_members(): a data.frame of name, path, size, ok and error. The
# archive is read from, in order: `url` itself when it is a path on disk; the
# copy .tar_peek() downloaded this session; `archive_path` when an earlier run
# left a readable copy there; or one new download of the whole archive to
# `archive_path` (a temporary file when NULL), when its size is at most
# `max_whole_bytes`.
.tar_fetch_members <- function(url, names, dest, archive_path = NULL, size = NULL,
                               max_whole_bytes = Inf, skip_on_api_limit = FALSE) {
  fail <- function(msg) data.frame(
    name = names, path = NA_character_, size = NA_real_, ok = FALSE,
    error = msg, stringsAsFactors = FALSE)

  copy <- if (exists(url, envir = .tar_peek_copies, inherits = FALSE))
    get(url, envir = .tar_peek_copies, inherits = FALSE) else NA_character_
  src <- if (.is_local_file(url)) url
    else if (!is.na(copy) && file.exists(copy)) copy
    else if (!is.null(archive_path) && file.exists(archive_path) &&
             !is.null(.tar_list_file(archive_path))) archive_path
    else NA_character_

  if (is.na(src)) {
    known <- suppressWarnings(as.numeric(size %||% NA_real_))[1]
    if (isTRUE(known > max_whole_bytes))
      return(fail("the archive is larger than the download budget"))
    if (is.null(archive_path)) {
      archive_path <- tempfile(fileext = ".tar")
      on.exit(unlink(archive_path), add = TRUE)
    }
    unlink(archive_path)
    err <- .download_one(url, archive_path, skip_on_api_limit = skip_on_api_limit,
                         expected_bytes = known)
    if (!is.na(err)) return(fail(err))
    src <- archive_path
  }

  listing <- .tar_list_file(src)
  if (is.null(listing)) return(fail("could not read the archive"))
  want <- listing[listing$member %in% names, , drop = FALSE]
  if (nrow(want) == 0) return(fail("not found in the archive"))

  targets <- vapply(want$name, .zip_member_target, character(1), dest = dest,
                    USE.NAMES = FALSE)
  exdir <- tempfile("tarwhole")
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  safe <- !is.na(targets)
  if (any(safe))
    tryCatch(suppressWarnings(utils::untar(src, files = want$member[safe],
                                           exdir = exdir, tar = "internal")),
             error = function(e) NULL)

  path <- rep(NA_character_, nrow(want))
  error <- ifelse(safe, "could not extract the member from the archive",
                  "entry path escapes the archive (path traversal)")
  for (i in which(safe)) {
    from <- file.path(exdir, want$member[i])
    if (!file.exists(from) || file.size(from) != want$size[i]) next
    dir.create(dirname(targets[i]), showWarnings = FALSE, recursive = TRUE)
    if (isTRUE(file.copy(from, targets[i], overwrite = TRUE))) {
      path[i] <- targets[i]
      error[i] <- NA_character_
    }
  }
  data.frame(name = want$member, path = path, size = want$size, ok = !is.na(path),
             error = error, stringsAsFactors = FALSE)
}
