# manually add batch queries to mocking - req_perform_parallel doesn't add them
# httptest2::start_capturing()
# for (url in urls) {
#   httr2::request(url) |>
#     httr2::req_headers(Accept = "application/json") |>
#     httr2::req_perform()
# }
# httptest2::stop_capturing()


# Decision log (throttle_capacity/throttle_fill_time_s, added after PR #326):
# PR #326 hardcoded req_throttle(capacity = 8, fill_time_s = 1) here for every
# caller, to fix CrossRef 429s. But .batch_query() is shared by CrossRef,
# DataCite, OSF, Zenodo and doi.org, each with different (or undocumented)
# limits, so one hardcoded number was CrossRef-specific logic silently applied
# to hosts it was never derived for. Checked each host's published limit
# (Aug 2026): CrossRef polite pool is 10 req/s for single-record lookups, 3
# req/s for list/search queries (changed 2025-12-01, so the PR's blended 8/s no
# longer matches either figure); DataCite is ~3.3 req/s (1000/5min,
# identified); OSF is 10,000/day authenticated; Zenodo's only published number
# (30/min) is for its search endpoint, not the single-record GET used here;
# doi.org has no confirmed public number. Only CrossRef's volume/limit ratio
# makes proactive throttling worth its cost, so throttling is now opt-in
# (default NULL = off) and only the CrossRef call sites in R/db-crossref.R
# pass a value, split by endpoint type. Every other host relies on the
# existing req_retry() backoff for the rare 429 rather than a guessed number.
#
# `delay` (the flat Sys.sleep between batches, below) stays unconditional
# rather than being skipped when a throttle is set. For throttled calls it's a
# small extra courtesy gap on top of req_throttle's per-request pacing
# (negligible next to network round-trip time); for unthrottled calls it is
# the only pacing they have. Making it conditional on throttle_capacity would
# silently change delay's effect for any future caller that sets both, which
# is a worse trade than the redundancy it would remove.
#' Batch query
#'
#' @param urls A vector of URLs
#' @param batch_size Size of each batch
#' @param msg Message to show in progress bar - set to NULL to omit progressbar
#' @param delay Courtesy delay between batches (in seconds)
#' @param accept The header type to accept
#' @param throttle_capacity requests per `throttle_fill_time_s` to allow, or
#'   NULL (default) for no throttling. Only set this for a host known to
#'   rate-limit (e.g. CrossRef); for others, req_retry's backoff already
#'   handles the rare 429 and a proactive throttle would just slow things down
#'   for no benefit.
#' @param throttle_fill_time_s time window (seconds) `throttle_capacity`
#'   applies to. Ignored when `throttle_capacity` is NULL.
#'
#' @returns a list of responses
#' @keywords internal
.batch_query <- function(urls,
                         batch_size = 5,
                         msg = "Batch Query",
                         delay = 0.5,
                         accept = "application/json",
                         req_func = \(req) {req},
                         throttle_capacity = NULL,
                         throttle_fill_time_s = 1) {
  if (length(urls) == 0) return(list())

  # set up requests from urls
  reqs <- lapply(urls, \(url) {
    tryCatch({
      req <- httr2::request(url) |>
        httr2::req_headers(Accept = accept) |>
        req_func()
      if (!is.null(throttle_capacity)) {
        # Throttle to stay under the host's rate limit; exceeding it returns
        # 429 and, in bursts, the retries get 429'd too. httr2 shares one
        # token bucket across requests (by host), so this paces the whole
        # batch. req_perform_sequential (below) is what makes this effective:
        # req_perform_parallel does not honour req_throttle.
        req <- req |> httr2::req_throttle(capacity = throttle_capacity,
                                          fill_time_s = throttle_fill_time_s)
      }
      req |>
        # retry transient statuses and connection-level failures (timeouts,
        # dropped connections). retry_on_failure covers the latter, which a
        # status-only is_transient would otherwise miss.
        httr2::req_retry(
          max_tries = 5,
          retry_on_failure = TRUE,
          is_transient = \(resp) {
            status <- httr2::resp_status(resp)
            status %in% c(429, 500, 502, 503, 504)
          }
        ) |>
        httr2::req_error(is_error = \(resp) FALSE)
    }, error = \(e) {
      warning("Bad URL: ", url, call. = FALSE)
      return(NULL)
    })
  })

  # batch to avoid rate limiting
  n <- length(reqs)
  resps <- vector("list", n)

  batches <- split(seq_len(n), ceiling(seq_len(n) / batch_size))

  if (!is.null(msg)) {
    format <- sprintf("%s [:bar] :current/:total", msg)
    pb <- pb(n, format = format)
  }

  # set to false during mocking capture
  mocking <- isTRUE(Sys.getenv("MOCK_CAPTURE") == "TRUE")
  if (!mocking) {
    for (b in seq_along(batches)) {
      idx <- batches[[b]]
      valid_idx <- !sapply(reqs[idx], is.null) # skip errors

      # sequential rather than parallel: req_perform_parallel does not honour
      # req_throttle, so parallel bursts exceed the API rate limit and get 429s.
      # req_perform_sequential respects the throttle, keeping us under the limit.
      resps[idx][valid_idx] <- httr2::req_perform_sequential(
        reqs[idx][valid_idx],
        on_error = "continue",
        progress = FALSE
      )
      if (!is.null(msg)) { pb$tick(length(idx)) }

      # courtesy delay
      Sys.sleep(delay)
    }
  } else { # non-parallel for mocking - workaround
    for (idx in seq_along(reqs)) {
      if (is.null(reqs[[idx]])) break # skip errors

      resps[[idx]] <- httr2::req_perform(
        reqs[[idx]]
      )
      if (!is.null(msg)) { pb$tick(1) }

      # courtesy delay
      Sys.sleep(delay)
    }
  }

  resps
}




#' Set or get email
#'
#' Get or set the contact email metacheck sends with requests to external
#' APIs (e.g. Crossref's polite pool), so a host that needs to reach you
#' about your usage can do so. Call with no argument to read the current
#' value; call with a valid email address to set it for the rest of the
#' session. Defaults to `"metacheck@scienceverse.org"` when never set.
#'
#' @param email if a string, sets the email
#'
#' @returns the current option value (character)
#' @export
#'
#' @examples
#' email()
email <- function(email = NULL) {
  if (is.null(email)) {
    email <- getOption("metacheck.email") %||% "metacheck@scienceverse.org"
    return(email)
  } else if (is.character(email) && grepl(".+@.+\\..+$", email)) {
    options(metacheck.email = email)
    invisible(getOption("metacheck.email"))
  } else {
    stop("Set email with a valid email address")
  }
}



#' Sanitize File Path
#'
#' Make sure user-input file names are not problematic.
#'
#' @param path the path to sanitize (can be a vector of paths)
#' @param replacement the character to replace invalid characters with
#' @param remove_whitespace whether to include whitespace as a problem
#' @param keep_sep whether to keep the path separator /
#'
#' @returns the sanitized vector
#' @export
#'
#' @examples
#' path <- "/My Files/x><y.pdf"
#' path_sanitize(path)
#' path_sanitize(path, replacement = "~")
#' path_sanitize(path, remove_whitespace = FALSE)
#' path_sanitize(path, keep_sep = FALSE)
path_sanitize <- function(path, replacement = "_",
                          remove_whitespace = TRUE,
                          keep_sep = TRUE) {
  rep_plus <- paste0(replacement, "+")
  invalid <- '[\\:*?"<>|]'
  sep <- ifelse(keep_sep, replacement, "\\/")
  ws <- ifelse(remove_whitespace, "\\s", replacement)

  path |>
    trimws() |>
    gsub("[[:cntrl:]]", "", x = _) |>      # remove control chars
    gsub(invalid, replacement, x = _) |>   # replace invalid chars
    gsub(sep, replacement, x = _) |>       # replace sep
    gsub(ws, replacement, x = _) |>        # replace whitespace
    gsub(rep_plus, replacement, x = _) |>  # condense replacements
    trimws()                               # trim spaces
}



# bind_rows <- function(..., .id = NULL) {
#   # try to bind and try to fix if error
#   df <- tryCatch(
#     dplyr::bind_rows(..., .id = .id),
#     error = \(e) {}
#   )
#
#   if (!is.null(df)) return(df)
#
#   dfs <- list(...)
#
#   if (length(dfs > 2)) stop("Data frames are imcompatible for binding.")
#   x <- dfs[[1]]
#   y <- dfs[[2]]
#
#   # find data types for all shared columns
#   x_types <- flatten(x) |> sapply(typeof)
#   y_types <- flatten(y) |> sapply(typeof)
#   share <- intersect(names(x_types), names(y_types))
#   mismatch <- x_types[share] != y_types[share]
#
#   find_var <- function(df, flat_name) {
#     path <- strsplit(flat_name, "\\.")[[1]]
#     Reduce(function(acc, name) acc[[name]], path, init = df)
#   }
#
#   change_type <- function(df, flat_name, type) {
#     fname <- paste0("as.", type)
#     if (!exists(fname, mode = "function")) {
#       warning(type)
#       return(df)
#     }
#     f <- get(fname, mode = "function")
#
#     new_var <- find_var(df, flat_name) |> f()
#
#     path <- strsplit(flat_name, "\\.")[[1]]
#
#     if (length(path) == 1) {
#       df[[path[[1]]]] <- new_var
#     } else if (length(path) == 2) {
#       df[[path[[1]]]][[path[[2]]]] <- new_var
#     } else if (length(path) == 3) {
#       df[[path[[1]]]][[path[[2]]]][[path[[3]]]] <- new_var
#     } else if (length(path) == 4) {
#       df[[path[[1]]]][[path[[2]]]][[path[[3]]]][[path[[4]]]] <- new_var
#     } else if (length(path) == 5) {
#       df[[path[[1]]]][[path[[2]]]][[path[[3]]]][[path[[4]]]][[path[[5]]]] <- new_var
#     } else {
#       warning("Path too long:", length(path))
#     }
#
#     df
#   }
#
#   for (flat_name in names(mismatch[mismatch])) {
#     #flat_name <- names(mismatch[mismatch])[[1]]
#     x_var <- find_var(x, flat_name)
#     y_var <- find_var(y, flat_name)
#     newtype <- typeof(c(x_var, y_var))
#
#     x <- change_type(x, flat_name, newtype)
#     y <- change_type(y, flat_name, newtype)
#   }
#
#   dplyr::bind_rows(x, y, .id = .id)
# }

# Conservative maximum ABSOLUTE path length metacheck will write to. Windows'
# classic MAX_PATH is 260; long-path support is unreliable to detect, so we keep
# a safe margin. On other platforms the effective limit is far higher (per-
# component 255), so we only guard the total on Windows but still cap absurd
# single components everywhere.
.max_path_chars <- if (.Platform$OS.type == "windows") 250L else 4000L
.max_component_chars <- 255L    # per-directory / filename limit on all platforms

# Make a path safe to write: if its ABSOLUTE form (or any single component)
# exceeds the filesystem limit, shorten the over-long component(s) by truncating
# and appending a short hash of the original, so the result stays unique and
# collisions between two long names cannot happen. Warns once per shortened path,
# naming the original so the change is visible. Directories/filename are shortened
# but the parent structure is preserved; the extension is kept on the leaf.
#
# Returns the (possibly shortened) path. Callers should use the RETURNED path for
# both dir.create and the write, and record it as the file's location, so the
# on-disk name and the recorded name always agree.
.safe_write_path <- function(path) {
  if (is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path))
    return(path)

  # Short deterministic hash (dependency-free), 8 hex chars, so two different
  # long names never collide after truncation. A polynomial rolling hash kept in
  # double precision (exact for integers < 2^53), reduced modulo a large prime so
  # it never overflows R's numeric range.
  short_hash <- function(s) {
    b <- as.numeric(charToRaw(s))
    m <- 2147483647                       # 2^31 - 1 (Mersenne prime)
    h <- 0
    for (x in b) h <- (h * 131 + x) %% m
    sprintf("%08x", as.integer(h))
  }

  shorten_component <- function(comp, budget) {
    # budget = max chars allowed for this component. Keep an extension on files.
    if (nchar(comp) <= budget) return(comp)
    ext <- sub("^.*(\\.[A-Za-z0-9]{1,8})$", "\\1", comp)
    if (identical(ext, comp)) ext <- ""            # no extension
    h <- paste0("-", short_hash(comp))
    keep <- max(1L, budget - nchar(h) - nchar(ext))
    paste0(substr(comp, 1, keep), h, ext)
  }

  parts <- strsplit(path, "[/\\\\]")[[1]]
  changed <- FALSE

  # Per-component cap first (cheap, platform-wide): shorten any single directory
  # or filename that is itself too long.
  parts <- vapply(parts, function(p) {
    if (nzchar(p) && nchar(p) > .max_component_chars) { changed <<- TRUE
      shorten_component(p, .max_component_chars) } else p
  }, character(1), USE.NAMES = FALSE)

  # Total absolute-length cap. When the absolute path is over the limit, shorten
  # over-long components (directory or leaf) — several deep components can each
  # contribute (the scale case has a long directory AND a long filename). We cap
  # every component longer than `long_comp` first, then, if still over, keep
  # tightening the longest one. A floor keeps a component from collapsing below a
  # usable stub+hash.
  long_comp <- 40L
  min_comp  <- 16L
  abs_len <- function() tryCatch(
    nchar(normalizePath(paste(parts, collapse = "/"), winslash = "/", mustWork = FALSE)),
    error = function(e) nchar(paste(parts, collapse = "/")))
  if (abs_len() > .max_path_chars) {
    for (j in seq_along(parts)) {
      if (nchar(parts[j]) > long_comp) {
        parts[j] <- shorten_component(parts[j], long_comp); changed <- TRUE
      }
    }
    # Still over (very deep tree of moderate components): tighten the longest.
    while (abs_len() > .max_path_chars) {
      j <- which.max(nchar(parts))
      if (nchar(parts[j]) <= min_comp) break
      parts[j] <- shorten_component(parts[j], nchar(parts[j]) - 1L)
      changed <- TRUE
    }
  }
  new_path <- paste(parts, collapse = "/")

  still_over <- abs_len() > .max_path_chars
  if (still_over) {
    # Could not get under the limit (e.g. a very deeply nested tree of moderate
    # names). Warn strongly — the write may fail — so the cause is not a cryptic
    # "cannot open file" error. Callers keep the shortened path (best effort).
    warning("Path is still too long after shortening and the write may fail: ",
            new_path, " (absolute length ", abs_len(), " > ", .max_path_chars,
            "). This usually means a very deeply nested output/repo tree; ",
            "use a shorter output directory or a path outside OneDrive.",
            call. = FALSE)
  } else if (changed) {
    warning("Path too long for the filesystem; shortened it (a hash keeps it ",
            "unique). Original: ", path, " -> ", new_path,
            ". Deeply nested repos or very long names on OneDrive/Windows hit ",
            "the ~260-character limit.", call. = FALSE)
  }
  new_path
}
