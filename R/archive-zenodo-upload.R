# Uploading to Zenodo. The read-only Zenodo functions live in archive-zenodo.R;
# everything here WRITES to a Zenodo account, so the defaults are deliberately
# cautious: uploads go to the sandbox unless asked otherwise, depositions are
# left as unpublished drafts, and an interactive run confirms before the first
# byte is sent. Publishing on the real zenodo.org mints a permanent DOI and
# cannot be undone, which is why it is never the default.

#' Write a line to a progress bar without leaving the old one behind
#'
#' `progress_bar` overwrites its line but does not clear it, so a message
#' shorter than the one before leaves the tail of the previous message on
#' screen. Padding to a fixed width overwrites those characters with spaces.
#'
#' @param pb the progress bar
#' @param text the message
#'
#' @returns NULL, invisibly
#' @keywords internal
.pb_say <- function(pb, text) {
  width <- max(getOption("width", 80) - 12, 40)
  if (nchar(text) > width) {
    text <- paste0(substr(text, 1, width - 1), "…")
  }
  pb$tick(0, tokens = list(what = formatC(text, width = -width)))
  invisible(NULL)
}

#' Zenodo API base URL
#'
#' @param sandbox whether to use the sandbox server
#'
#' @returns the base API URL
#' @keywords internal
.zenodo_api <- function(sandbox = TRUE) {
  if (isTRUE(sandbox)) "https://sandbox.zenodo.org/api" else "https://zenodo.org/api"
}

#' Set or get the Zenodo personal access token
#'
#' Use `zenodo_pat()` to get the token used to authorise Zenodo uploads, or
#' `zenodo_pat("your-token")` to set it for the rest of the session. The sandbox
#' and the real Zenodo are separate services with separate accounts, so they
#' need separate tokens and are stored separately here.
#'
#' To create a token, sign in and go to Applications, then "Personal access
#' tokens", then "New token". Give it a name and tick the `deposit:write` scope
#' (to create depositions and add files) and `deposit:actions` (to publish).
#' Copy the token immediately, as Zenodo does not show it again.
#'
#' * sandbox token: <https://sandbox.zenodo.org/account/settings/applications/tokens/new/>
#' * real token: <https://zenodo.org/account/settings/applications/tokens/new/>
#'
#' The sandbox needs its own account, registered at
#' <https://sandbox.zenodo.org>; a real zenodo.org login does not work there.
#'
#' Store them as lines in your `.Renviron` file (open it with
#' `usethis::edit_r_environ()`) so they are read every time R starts:
#'
#' `ZENODO_SANDBOX_PAT="replace-with-your-sandbox-token"`
#'
#' `ZENODO_PAT="replace-with-your-real-token"`
#'
#' @param pat the token to set, or NULL to get the current token
#' @param sandbox whether the token is for the sandbox server
#'
#' @returns the current token (character; `""` when none is set)
#' @export
#'
#' @examples
#' zenodo_pat() # returns "" unless a token is set
zenodo_pat <- function(pat = NULL, sandbox = TRUE) {
  opt <- if (isTRUE(sandbox)) "metacheck.zenodo.pat.sandbox" else "metacheck.zenodo.pat"
  env <- if (isTRUE(sandbox)) "ZENODO_SANDBOX_PAT" else "ZENODO_PAT"

  if (is.null(pat)) {
    return(getOption(opt) %||% Sys.getenv(env))
  }
  if (!is.character(pat) || length(pat) != 1 || is.na(pat)) {
    stop("Set zenodo_pat with a single string containing your Zenodo token",
         call. = FALSE)
  }
  args <- list(pat)
  names(args) <- opt
  do.call(options, args)
  invisible(pat)
}

#' Add Zenodo auth to a request
#'
#' @param req an httr2 request object
#' @param token the Zenodo personal access token
#'
#' @returns the modified request
#' @keywords internal
.zenodo_auth <- function(req, token) {
  req |>
    httr2::req_headers(
      `User-Agent` = "metacheck",
      Authorization = sprintf("Bearer %s", token)
    ) |>
    httr2::req_retry(
      max_tries = 3,
      retry_on_failure = TRUE,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
    ) |>
    httr2::req_error(is_error = \(resp) FALSE)
}

#' Check a Zenodo token works before uploading anything
#'
#' Zenodo does not answer a bad token with 401: it returns HTTP 500, which is
#' indistinguishable from a genuine server fault and so gets retried with
#' backoff. Left unchecked, a mistyped or wrong-server token therefore costs
#' minutes of retries per folder before failing. One cheap authenticated
#' request up front turns that into an immediate, accurate message.
#'
#' @param api the Zenodo API base URL
#' @param token the personal access token
#' @param sandbox whether this is the sandbox server (for the message)
#'
#' @returns TRUE invisibly; stops when the token is not accepted
#' @keywords internal
.zenodo_check_token <- function(api, token, sandbox = TRUE) {
  resp <- tryCatch(
    httr2::request(paste0(api, "/deposit/depositions")) |>
      httr2::req_url_query(size = 1) |>
      httr2::req_headers(`User-Agent` = "metacheck",
                         Authorization = sprintf("Bearer %s", token)) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform(),
    error = \(e) e
  )

  if (inherits(resp, "error")) {
    stop(sprintf("Could not reach %s: %s", api, conditionMessage(resp)),
         call. = FALSE)
  }

  status <- httr2::resp_status(resp)
  if (status < 400) return(invisible(TRUE))

  stop(sprintf(
    "Zenodo did not accept the token (HTTP %d). Check that it is a %s token with the deposit:write scope; a %s token will not work here. See ?zenodo_pat",
    status,
    if (isTRUE(sandbox)) "sandbox.zenodo.org" else "zenodo.org",
    if (isTRUE(sandbox)) "zenodo.org" else "sandbox.zenodo.org"),
    call. = FALSE)
}

#' Report a Zenodo API error in a readable way
#'
#' Zenodo returns the reason a deposition was rejected in a nested `errors`
#' list (e.g. which metadata field is invalid), which is far more useful than
#' the bare status code.
#'
#' @param resp an httr2 response
#' @param what what was being attempted, for the message
#'
#' @returns the parsed body, invisibly; stops on an error status
#' @keywords internal
.zenodo_check_resp <- function(resp, what) {
  status <- httr2::resp_status(resp)
  body <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)

  if (status < 400) return(invisible(body))

  detail <- body$message %||% httr2::resp_status_desc(resp)
  fields <- ""
  if (!is.null(body$errors) && length(body$errors) > 0) {
    fields <- vapply(body$errors, \(e) {
      sprintf("%s: %s", e$field %||% "?", e$message %||% "invalid")
    }, character(1)) |>
      paste(collapse = "; ") |>
      paste0(" (", ... = _, ")")
  }

  if (status %in% c(401, 403)) {
    stop(sprintf(
      "%s failed: Zenodo rejected the token (HTTP %d). Check the token has the deposit:write scope, and that a sandbox token is used for the sandbox. See ?zenodo_pat",
      what, status), call. = FALSE)
  }

  stop(sprintf("%s failed (HTTP %d): %s%s", what, status, detail, fields),
       call. = FALSE)
}

#' Map an OSF license name to a Zenodo license identifier
#'
#' Zenodo wants an SPDX-style identifier (e.g. `"cc-by-4.0"`), while the OSF
#' stores a display name (e.g. `"CC-By Attribution 4.0 International"`). Only
#' the licenses the OSF actually offers are mapped; anything else returns NA so
#' the caller can fall back to its `license` argument rather than send Zenodo a
#' value it will reject.
#'
#' @param osf_license the OSF license name
#'
#' @returns an SPDX-style Zenodo license id, or NA_character_
#' @keywords internal
.zenodo_license_id <- function(osf_license) {
  if (length(osf_license) == 0) return(NA_character_)
  if (is.na(osf_license) || !nzchar(osf_license)) return(NA_character_)

  # All 19 licenses the OSF offers, taken verbatim from
  # https://api.osf.io/v2/licenses/ (checked 2026-08-12), mapped to the SPDX
  # identifiers Zenodo accepts. Every id on the right was checked against
  # https://zenodo.org/api/vocabularies/licenses/<id> and returned 200 on the
  # same date. Matching is on a lowercased, punctuation-free form of the name,
  # so differences in spacing, hyphens, or the quotation marks in the BSD names
  # do not matter.
  #
  # Two details Zenodo's vocabulary insists on, both of which fail as a plain
  # 400 at upload time if guessed: CC0 is "cc0-1.0" (not "cc-zero"), and every
  # GPL/LGPL id must say whether later versions are allowed. The OSF names do
  # not record that choice, so "-or-later" is used, matching how the Free
  # Software Foundation intends those licenses to be applied by default.
  #
  # "No license" and "Other" are deliberately absent: neither names a license
  # Zenodo could accept, so both fall through to NA and the caller's `license`
  # argument, exactly as an unset license does.
  map <- c(
    "ccbyattribution40international"                        = "cc-by-4.0",
    "ccbyattributionnoncommercial40international"           = "cc-by-nc-4.0",
    "ccbyattributionnoderivatives40international"           = "cc-by-nd-4.0",
    "ccbyattributionnoncommercialsharealike40international"  = "cc-by-nc-sa-4.0",
    "cc010universal"                                        = "cc0-1.0",
    "mitlicense"                                            = "mit",
    "bsd2clausesimplifiedlicense"                           = "bsd-2-clause",
    "bsd3clausenewrevisedlicense"                           = "bsd-3-clause",
    "apachelicense20"                                       = "apache-2.0",
    "artisticlicense20"                                     = "artistic-2.0",
    "academicfreelicenseafl30"                              = "afl-3.0",
    "eclipsepubliclicense10"                                = "epl-1.0",
    "mozillapubliclicense20"                                = "mpl-2.0",
    "gnugeneralpubliclicensegpl20"                          = "gpl-2.0-or-later",
    "gnugeneralpubliclicensegpl30"                          = "gpl-3.0-or-later",
    "gnulessergeneralpubliclicenselgpl21"                   = "lgpl-2.1-or-later",
    "gnulessergeneralpubliclicenselgpl30"                   = "lgpl-3.0-or-later"
  )

  key <- tolower(osf_license) |> gsub("[^a-z0-9]", "", x = _)
  if (!key %in% names(map)) return(NA_character_)
  unname(map[[key]])
}

#' Get OSF project metadata for a Zenodo deposition
#'
#' One API call per project, using `?embed=license&embed=bibliographic_contributors`
#' so the title, description, tags, license, and every contributor's name and
#' ORCID arrive together instead of as three separate requests per project.
#' The calls for all projects are batched through [.batch_query()].
#'
#' @param osf_id a vector of OSF project IDs
#' @param pb a progress bar passed from another function
#'
#' @returns a list of metadata lists, named by OSF ID
#' @keywords internal
.osf_zenodo_metadata <- function(osf_id, pb = NULL) {
  osf_id <- stats::na.omit(unique(osf_id))
  if (length(osf_id) == 0) return(list())

  osf_api <- getOption("metacheck.osf.api")
  urls <- sprintf(
    "%s/nodes/%s/?embed=license&embed=bibliographic_contributors&page[size]=100",
    osf_api, osf_id
  )
  resps <- .batch_query(urls, msg = "OSF Metadata", req_func = .osf_headers)

  out <- lapply(seq_along(osf_id), \(i) {
    resp <- resps[[i]]
    if (is.null(resp) || inherits(resp, "error")) return(NULL)
    if (httr2::resp_status(resp) != 200) return(NULL)

    content <- tryCatch(httr2::resp_body_json(resp, simplifyVector = TRUE),
                        error = \(e) NULL)
    if (is.null(content)) return(NULL)

    att <- content$data$attributes
    users <- content$data$embeds$bibliographic_contributors$data$embeds$users$data

    # OSF stores given_name and family_name separately, so no name splitting is
    # needed. full_name is only the fallback for a profile that left them blank,
    # and Zenodo accepts a single-string name in that case.
    creators <- list()
    if (!is.null(users) && length(users$attributes$full_name) > 0) {
      ua <- users$attributes
      n <- length(ua$full_name)
      creators <- lapply(seq_len(n), \(j) {
        family <- ua$family_name[[j]] %||% ""
        given <- ua$given_name[[j]] %||% ""
        name <- if (nzchar(family) && nzchar(given)) {
          sprintf("%s, %s", family, given)
        } else {
          ua$full_name[[j]]
        }
        cr <- list(name = name)
        orcid <- ua$social$orcid[[j]] %||% NA_character_
        if (!is.na(orcid) && nzchar(orcid)) cr$orcid <- orcid
        cr
      })
    }

    list(
      osf_id = osf_id[[i]],
      title = att$title %||% NA_character_,
      description = att$description %||% NA_character_,
      tags = att$tags %||% character(0),
      license = content$data$embeds$license$data$attributes$name %||% NA_character_,
      creators = creators,
      date_created = att$date_created %||% NA_character_
    )
  })

  names(out) <- osf_id
  out[!vapply(out, is.null, logical(1))]
}

#' Classify files the way the check modules do
#'
#' `repo_check()`, `code_check()` and `data_check()` all classify a
#' repository's files with `data_classify_files()`, into six categories:
#' `data`, `code`, `materials`, `documentation`, `output`, and `unknown`.
#' Splitting an upload calls the very same function, so a file cannot end up in
#' one category when it is checked and another when it is archived.
#'
#' That function reads names and paths only, never file contents, in three
#' steps: a format-locked extension (a `.sav` is code, a `.csv` is data), then
#' a category word used as a whole token in the path (`Materials/`,
#' `results.docx`), then a crosswalk through the coarse file type, where
#' `image`, `audio`, `video`, `3D`, `exec`, `config` and `font` all become
#' `materials`. Because nothing is opened, classifying costs no time even for a
#' folder of very large files.
#'
#' @param files full paths to the files
#'
#' @returns a character vector of categories, one per file
#' @keywords internal
.zenodo_classify <- function(files) {
  if (length(files) == 0) return(character(0))
  data_classify_files(basename(files), file_path = files)
}

#' Work out the name each file will have on Zenodo
#'
#' Zenodo has no folders. Its own documentation is explicit: "Zenodo does not
#' support uploading and organising files into folders/directories. Instead,
#' you can create a ZIP archive and upload it, in which case Zenodo will
#' display the file structure inside the ZIP." The API bears this out -- the
#' bucket refuses a name containing "/" with a 404, the older form endpoint
#' silently rewrites `code/02.R` to `code_02.R`, and a file object carries no
#' path field at all.
#'
#' So a name has to be flat. Using the full relative path for every file makes
#' them needlessly long, when most files in a project have a unique name
#' anyway. This keeps the bare file name wherever it is unambiguous, and only
#' prefixes the ones that would otherwise collide -- so `analysis.R` stays
#' `analysis.R`, while two `README.md` files become `github__README.md` and
#' `data__README.md`.
#'
#' @param files full paths to the files being uploaded
#' @param folder the folder they are inside, stripped from the front
#'
#' @returns a character vector of names, one per file, all distinct
#' @keywords internal
.zenodo_flat_names <- function(files, folder) {
  if (length(files) == 0) return(character(0))

  rel <- sub(paste0("^", .zenodo_regex_escape(folder), "[/\\\\]*"), "", files)
  rel <- gsub("\\\\", "/", rel)

  base <- basename(rel)
  out <- base

  # Only the names that appear more than once need disambiguating. Each one
  # takes as much of its path as it takes to become unique, so a file two
  # folders deep is only prefixed twice if one prefix is not enough.
  for (nm in unique(base[duplicated(base)])) {
    idx <- which(base == nm)
    parts <- strsplit(rel[idx], "/", fixed = TRUE)
    for (depth in seq_len(max(lengths(parts)))) {
      cand <- vapply(parts, \(p) {
        keep <- utils::tail(p, depth + 1)
        paste(keep, collapse = "__")
      }, character(1))
      if (!any(duplicated(cand))) break
    }
    out[idx] <- cand
  }

  # A name can still clash with a different file's full path form, so anything
  # left over gets a number rather than silently overwriting on Zenodo.
  dup <- duplicated(out)
  if (any(dup)) {
    for (i in which(dup)) {
      ext <- sub(".*(\\.[^.]+)$", "\\1", out[[i]])
      stem <- sub("\\.[^.]+$", "", out[[i]])
      out[[i]] <- sprintf("%s_%d%s", stem, i, if (ext == out[[i]]) "" else ext)
    }
  }

  out
}

#' Read OSF metadata that was saved alongside a downloaded folder
#'
#' `osf_file_download(metadata = TRUE)` writes `_osf_metadata/metadata.json`
#' into each project folder. Reading it back means a folder can be uploaded
#' with its real title, description, tags, licence and contributors without
#' asking the OSF again -- and still works for a project that has since been
#' deleted there, which is the reason for archiving it in the first place.
#'
#' @param folder a folder that may contain `_osf_metadata/metadata.json`
#'
#' @returns a metadata list in the same shape [.osf_zenodo_metadata()] returns,
#'   or NULL when the folder has no such file
#' @keywords internal
.zenodo_meta_from_folder <- function(folder) {
  path <- file.path(folder, .osf_meta_dir, "metadata.json")
  if (!file.exists(path)) return(NULL)

  m <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
                error = \(e) NULL)
  if (is.null(m) || is.null(m$osf_id)) return(NULL)

  # The saved file keeps each contributor as a list with name, given_name,
  # family_name and orcid; Zenodo wants "Family, Given" plus an optional orcid,
  # which is what .zenodo_build_metadata() expects to receive here.
  creators <- lapply(m$contributors %||% list(), \(c) {
    family <- c$family_name %||% ""
    given <- c$given_name %||% ""
    nm <- if (nzchar(family) && nzchar(given)) {
      sprintf("%s, %s", family, given)
    } else {
      c$name %||% ""
    }
    out <- list(name = nm)
    if (!is.null(c$orcid) && !is.na(c$orcid) && nzchar(c$orcid)) {
      out$orcid <- c$orcid
    }
    out
  })
  creators <- creators[vapply(creators, \(c) nzchar(c$name), logical(1))]

  list(
    osf_id = m$osf_id,
    title = m$title %||% NA_character_,
    description = m$description %||% NA_character_,
    tags = unlist(m$tags %||% list(), use.names = FALSE) %||% character(0),
    license = m$license %||% NA_character_,
    creators = creators
  )
}

#' Build the metadata Zenodo expects for one deposition
#'
#' @param meta a metadata list from [.osf_zenodo_metadata()], or NULL
#' @param folder the folder being uploaded (used for the title as a last resort)
#' @param license the fallback Zenodo license id
#' @param upload_type the Zenodo resource type
#'
#' @returns a list ready to send as the deposition's `metadata`
#' @keywords internal
.zenodo_build_metadata <- function(meta, folder, license = "cc-by-4.0",
                                   upload_type = "dataset") {
  title <- meta$title %||% NA_character_
  if (is.na(title) || !nzchar(title)) title <- basename(folder)

  description <- meta$description %||% NA_character_
  if (is.na(description) || !nzchar(description)) {
    # Zenodo requires a non-empty description, so say plainly where the files
    # came from rather than inventing a summary of content we have not read.
    description <- if (!is.null(meta$osf_id)) {
      sprintf("Files archived from the OSF project https://osf.io/%s/", meta$osf_id)
    } else {
      sprintf("Files archived from %s", basename(folder))
    }
  }

  creators <- meta$creators
  if (is.null(creators) || length(creators) == 0) {
    creators <- list(list(name = "Unknown"))
  }

  osf_license <- .zenodo_license_id(meta$license %||% NA_character_)
  used_license <- osf_license
  if (is.na(used_license)) used_license <- license

  md <- list(
    title = title,
    upload_type = upload_type,
    description = description,
    creators = creators,
    license = used_license
  )

  if (length(meta$tags %||% character(0)) > 0) {
    md$keywords <- as.list(meta$tags)
  }

  if (!is.null(meta$osf_id)) {
    md$related_identifiers <- list(list(
      identifier = sprintf("https://osf.io/%s/", meta$osf_id),
      relation = "isIdenticalTo",
      scheme = "url"
    ))
  }

  attr(md, "license_was_default") <- is.na(osf_license)
  md
}

#' Upload Folders to Zenodo
#'
#' Creates a Zenodo deposition for each folder, uploads every file in it, and
#' attaches metadata. Designed to take the output of [osf_file_download()]
#' directly, so a whole OSF account can be archived on Zenodo in two steps:
#'
#' ```
#' osf_file_download("4i578", download_to = "my_osf") |>
#'   zenodo_upload()
#' ```
#'
#' # Safety
#'
#' Uploads go to the Zenodo **sandbox** by default. The sandbox is a complete
#' separate copy of Zenodo, run for testing at <https://sandbox.zenodo.org>. It
#' has its own accounts and its own tokens, and the DOIs it mints use a test
#' prefix that never resolves publicly, so nothing you do there is permanent.
#' Set `sandbox = FALSE` only once a sandbox run looks right.
#'
#' Depositions are left as unpublished **drafts** (`publish = FALSE`). A draft
#' can be edited or deleted freely. Review each draft in your Zenodo account,
#' then press Publish there when you are satisfied. Publishing on the real
#' zenodo.org is irreversible: the record and its DOI are permanent and cannot
#' be deleted.
#'
#' @param folders a data frame returned by [osf_file_download()], or a character
#'   vector of paths to folders to upload (one deposition per folder)
#' @param sandbox whether to upload to the Zenodo sandbox (the default) rather
#'   than the real zenodo.org
#' @param zenodo_pat a Zenodo personal access token. Defaults to whatever
#'   [zenodo_pat()] returns for this server. See [zenodo_pat()] for how to
#'   create one.
#' @param publish whether to publish each deposition. `FALSE` (the default)
#'   leaves it as a draft for you to check and publish by hand.
#' @param license the Zenodo license identifier to use when the OSF project has
#'   no license Zenodo recognises
#' @param upload_type the Zenodo resource type (e.g. `"dataset"`,
#'   `"publication"`, `"software"`)
#' @param as_zip whether to upload each folder as a single ZIP archive instead
#'   of as individual files.
#'
#'   Zenodo has no folders. Its documentation says so directly -- "Zenodo does
#'   not support uploading and organising files into folders/directories.
#'   Instead, you can create a ZIP archive and upload it, in which case Zenodo
#'   will display the file structure inside the ZIP" -- and the API agrees: a
#'   file name containing "/" is rejected, and a file record has no path field.
#'
#'   So there is a choice. `TRUE` (the default) keeps the folder structure
#'   exactly, which is what an archive is for: a reader gets the project as the
#'   author arranged it, rather than a heap of files they must sort out. The
#'   usual objection -- that reaching one small file means downloading
#'   everything -- is what `split_materials` answers, by putting the bulky part
#'   in a separate archive.
#'
#'   `FALSE` uploads the files separately, which keeps each one visible,
#'   previewable and downloadable on its own; the folder structure is lost,
#'   though file names are kept short and only prefixed where two files would
#'   otherwise collide. That suits a small flat folder, where the structure
#'   carries no meaning worth preserving.
#' @param split_materials which CATEGORIES of file to put in their own archive
#'   when `as_zip = TRUE`. Defaults to `"materials"`, which is usually the bulk
#'   of a repository's size and the part a reader is least likely to need.
#'
#'   Categories are the same six `repo_check()` reports -- `data`, `code`,
#'   `materials`, `documentation`, `output`, `unknown` -- decided by the same
#'   classifier, so a file lands in the same category whether it is being
#'   checked or archived. Pass more than one to separate them further, for
#'   example `c("materials", "output")`.
#'
#'   Every archive stores the same project folder at its root, so unzipping all
#'   of them rebuilds the original tree exactly, while unzipping only the main
#'   one gives everything except the split categories, each file still in its
#'   right place. That lets somebody take the data and code without a
#'   multi-gigabyte stimulus set.
#'
#'   Set `NULL` or `FALSE` for a single archive holding everything. Ignored
#'   unless `as_zip = TRUE`.
#' @param upload_osf_metadata whether to upload the `_osf_metadata` folder that
#'   [osf_file_download()] saves inside each project -- the wiki pages, the
#'   activity log, and the project record. `TRUE` by default, so the archive on
#'   Zenodo describes itself. Set `FALSE` to deposit only the data files; the
#'   metadata is still read either way, since that is where the deposition's
#'   title, licence and authors come from.
#' @param metadata a list of extra metadata fields to add to every deposition,
#'   overriding what was taken from the OSF
#' @param max_file_size largest file to upload, in MB; larger files are skipped
#'   and reported. Set to NULL for no limit.
#' @param ask whether to show a summary and ask for confirmation before
#'   uploading anything. Ignored in a non-interactive session, where the upload
#'   proceeds without asking.
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame with one row per folder: the folder path, the Zenodo
#'   deposition id, its DOI, the URL to review it, how many files were uploaded
#'   and skipped, and whether it was published
#' @export
#'
#' @examples
#' \dontrun{
#' # download one OSF project and put it on the sandbox as a draft
#' osf_file_download("6nt4v", download_to = "archive") |>
#'   zenodo_upload()
#'
#' # upload a folder you already have
#' zenodo_upload("archive/6nt4v")
#'
#' # once the sandbox run looks right, upload for real (still a draft)
#' zenodo_upload("archive/6nt4v", sandbox = FALSE)
#' }
zenodo_upload <- function(folders,
                          sandbox = TRUE,
                          zenodo_pat = NULL,
                          publish = FALSE,
                          license = "cc-by-4.0",
                          upload_type = "dataset",
                          metadata = NULL,
                          as_zip = TRUE,
                          split_materials = "materials",
                          upload_osf_metadata = TRUE,
                          max_file_size = NULL,
                          ask = TRUE,
                          pb = NULL) {
  ## work out what to upload, and which OSF project each folder came from ----
  osf_ids <- NULL
  if (is.data.frame(folders)) {
    if (!"download_path" %in% names(folders)) {
      stop("`folders` is a data frame without a `download_path` column. ",
           "Pass the result of osf_file_download(), or a vector of folder paths.",
           call. = FALSE)
    }
    # One row per FILE comes in; one deposition per FOLDER goes out.
    paths <- unique(folders$download_path[!is.na(folders$download_path)])
    if ("osf_project" %in% names(folders)) {
      lookup <- unique(folders[!is.na(folders$download_path),
                               c("download_path", "osf_project")])
      osf_ids <- lookup$osf_project[match(paths, lookup$download_path)]
    }
  } else {
    paths <- as.character(folders)
  }

  paths <- paths[!is.na(paths)]
  exists <- dir.exists(paths)
  if (any(!exists)) {
    warning(sum(!exists), " folder", plural(sum(!exists)),
            " could not be found and will be skipped: ",
            paste(utils::head(paths[!exists], 3), collapse = ", "),
            call. = FALSE)
    if (!is.null(osf_ids)) osf_ids <- osf_ids[exists]
    paths <- paths[exists]
  }
  if (length(paths) == 0) {
    message("No folders to upload")
    return(invisible(NULL))
  }

  ## token ----
  # The argument `zenodo_pat` shadows the zenodo_pat() function inside this
  # body, so the function is fetched explicitly. `::` would fail while the
  # package is loaded with load_all() before NAMESPACE is regenerated, so the
  # lookup is by mode instead, which finds the function whatever its export
  # status.
  set_pat <- get("zenodo_pat", mode = "function")
  if (!is.null(zenodo_pat)) set_pat(zenodo_pat, sandbox = sandbox)
  token <- set_pat(sandbox = sandbox)
  if (!nzchar(token)) {
    stop(sprintf(
      "No token found for %s. Set one with zenodo_pat(\"your-token\", sandbox = %s), or put %s in your .Renviron. See ?zenodo_pat for how to create one.",
      if (isTRUE(sandbox)) "the Zenodo sandbox" else "Zenodo",
      if (isTRUE(sandbox)) "TRUE" else "FALSE",
      if (isTRUE(sandbox)) "ZENODO_SANDBOX_PAT" else "ZENODO_PAT"), call. = FALSE)
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Zenodo Upload"))
    on.exit(pb$terminate())
  }

  ## list the files in each folder ----
  mb <- 1024 * 1024
  file_lists <- lapply(paths, \(p) {
    f <- list.files(p, recursive = TRUE, full.names = TRUE, all.files = FALSE)
    f <- f[!dir.exists(f)]
    # The `_osf_metadata` folder holds the wiki pages, activity log and project
    # record that osf_file_download() saved. Uploading it by default keeps the
    # archive self-describing on Zenodo, but it is not data, so it can be left
    # out. The metadata is still READ either way -- that is what gives the
    # deposition its title, licence and authors.
    if (!isTRUE(upload_osf_metadata)) {
      f <- f[!grepl(sprintf("(^|[/\\\\])%s([/\\\\]|$)", .osf_meta_dir), f)]
    }
    f
  })
  sizes <- lapply(file_lists, \(f) if (length(f)) file.size(f) else numeric(0))

  skipped <- rep(0L, length(paths))
  if (!is.null(max_file_size) && is.finite(max_file_size) && max_file_size > 0) {
    for (i in seq_along(file_lists)) {
      too_big <- which(sizes[[i]] > max_file_size * mb)
      if (length(too_big) > 0) {
        message(sprintf(
          "%d file%s in %s exceed the %s MB limit and will not be uploaded (largest: %s)",
          length(too_big), plural(length(too_big)), basename(paths[[i]]),
          format(max_file_size),
          basename(file_lists[[i]][too_big][order(-sizes[[i]][too_big])][1])))
        file_lists[[i]] <- file_lists[[i]][-too_big]
        sizes[[i]] <- sizes[[i]][-too_big]
        skipped[[i]] <- length(too_big)
      }
    }
  }

  n_files <- vapply(file_lists, length, integer(1))
  total_size <- sum(unlist(sizes), na.rm = TRUE)

  if (sum(n_files) == 0) {
    message("No files to upload in ", length(paths), " folder",
            plural(length(paths)))
    return(invisible(NULL))
  }

  ## check the token before asking anything or sending anything ----
  api <- .zenodo_api(sandbox)
  .zenodo_check_token(api, token, sandbox = sandbox)

  ## confirm before sending anything ----
  server <- if (isTRUE(sandbox)) "the Zenodo SANDBOX (sandbox.zenodo.org)" else "the REAL Zenodo (zenodo.org)"
  # Starts on a new line: the progress bar leaves the cursor part way along its
  # own line, so without this the summary is spliced onto the spinner. The
  # blank line *after* it belongs to the menu title below, not here -- see the
  # note there.
  # Say which of the two shapes the files will arrive in, because it is the
  # difference between a reader seeing your folders and seeing a flat list, and
  # it is easier to change now than after the deposition exists.
  how <- if (isTRUE(as_zip)) {
    if (length(split_materials) > 0 && !isFALSE(split_materials)) {
      sprintf("\nFiles will be packed into ZIP archives, keeping their folders, with %s in separate archives.",
              paste(split_materials, collapse = " and "))
    } else {
      "\nFiles will be packed into one ZIP archive per folder, keeping their folders."
    }
  } else {
    "\nFiles will be uploaded individually; their folder structure will be lost."
  }
  summary_msg <- sprintf(
    "\nAbout to upload %d folder%s (%d file%s, %s) to %s.%s\nDepositions will be %s.",
    length(paths), plural(length(paths)),
    sum(n_files), plural(sum(n_files)),
    .cap_size_str(total_size), server, how,
    if (isTRUE(publish)) "PUBLISHED immediately" else "left as unpublished drafts"
  )
  message(summary_msg)

  if (isTRUE(publish) && !isTRUE(sandbox)) {
    message("Publishing on the real Zenodo cannot be undone: each record and ",
            "its DOI become permanent and cannot be deleted.\n")
  }

  if (isTRUE(ask) && interactive()) {
    # The blank line belongs to the title rather than the message before it:
    # message() writes to stderr and menu() to stdout, and the two are not
    # guaranteed to interleave, so a trailing newline on the message can be
    # flushed after the prompt has already been printed -- which is how
    # "drafts." and "Continue?" ended up on the same line.
    choice <- utils::menu(c("Yes, upload", "No, cancel"),
                          title = "\nContinue?")
    if (choice != 1) {
      message("Cancelled; nothing was uploaded")
      return(invisible(NULL))
    }
  }

  ## metadata for each folder ----
  # A folder downloaded by osf_file_download(metadata = TRUE) already carries
  # everything Zenodo needs, in `_osf_metadata/metadata.json`: title,
  # description, tags, licence, and contributors with their ORCIDs. Reading it
  # means uploading a folder gets the same rich deposition as uploading a
  # download result, and it works for a project that has since been deleted
  # from the OSF -- which is the whole point of having archived it.
  #
  # Where there is no such file (a folder that did not come from the OSF, or
  # one downloaded with metadata = FALSE), the OSF is queried as before.
  meta_by_id <- list()
  if (!is.null(osf_ids) && any(!is.na(osf_ids))) {
    meta_by_id <- .osf_zenodo_metadata(osf_ids, pb = pb)
  }

  meta_by_folder <- lapply(paths, .zenodo_meta_from_folder)
  names(meta_by_folder) <- paths
  n_local <- sum(!vapply(meta_by_folder, is.null, logical(1)))
  if (n_local > 0) {
    message(sprintf(
      "Read the OSF metadata already saved in %d folder%s.",
      n_local, plural(n_local)))
  }

  # An OSF project whose metadata could not be fetched (deleted, made private,
  # or a failed request) still gets a deposition, but titled after its folder
  # and credited to "Unknown". That is recoverable while it is a draft, so it
  # warns rather than stopping -- but it must not pass silently, or a record
  # could be published with no real author on it.
  if (!is.null(osf_ids)) {
    no_meta <- setdiff(stats::na.omit(osf_ids), names(meta_by_id))
    if (length(no_meta) > 0) {
      warning(sprintf(
        "No OSF metadata could be retrieved for %d project%s (%s), so the deposition%s will be titled after the folder with \"Unknown\" as the creator. Check and correct %s before publishing.",
        length(no_meta), plural(length(no_meta)),
        paste(utils::head(no_meta, 5), collapse = ", "),
        plural(length(no_meta)),
        if (length(no_meta) == 1) "it" else "them"), call. = FALSE)
    }
  }

  results <- vector("list", length(paths))
  default_license_used <- character(0)

  for (i in seq_along(paths)) {
    folder <- paths[[i]]
    # Padded to a fixed width. The progress bar overwrites its line without
    # clearing it, so a shorter message leaves the tail of a longer one behind
    # -- a folder named after a long project title would otherwise stay on
    # screen, spliced onto whatever came next.
    .pb_say(pb, sprintf("Uploading %s (%d of %d)",
                        basename(folder), i, length(paths)))

    osf_id <- if (!is.null(osf_ids)) osf_ids[[i]] else NA_character_
    # The folder's own saved metadata wins: it is what was archived, it needs
    # no request, and it survives the project being deleted from the OSF.
    # Falling back to a fresh query covers folders downloaded before this
    # existed, or with metadata = FALSE.
    meta <- meta_by_folder[[folder]]
    if (is.null(meta) && !is.na(osf_id)) meta <- meta_by_id[[osf_id]]
    md <- .zenodo_build_metadata(meta, folder, license = license,
                                 upload_type = upload_type)
    if (isTRUE(attr(md, "license_was_default"))) {
      default_license_used <- c(default_license_used, basename(folder))
    }
    if (!is.null(metadata)) md[names(metadata)] <- metadata

    dep <- tryCatch({
      ## create an empty deposition ----
      resp <- httr2::request(paste0(api, "/deposit/depositions")) |>
        .zenodo_auth(token) |>
        # An empty JSON OBJECT, "{}", not an empty array. R has one type for
        # both, and req_body_json(list()) serialises it as "[]", which Zenodo
        # answers with HTTP 500 -- an unhelpful status that reads like a fault
        # at their end rather than a malformed request from ours (verified
        # 2026-08-13: "{}" returns 201 and creates the deposition, "[]" returns
        # 500). The body is written literally to avoid the ambiguity.
        httr2::req_body_raw("{}", type = "application/json") |>
        httr2::req_perform()
      .zenodo_check_resp(resp, sprintf("Creating a deposition for %s",
                                       basename(folder)))
    }, error = \(e) e)

    if (inherits(dep, "error")) {
      warning(conditionMessage(dep), call. = FALSE)
      results[[i]] <- data.frame(
        folder = folder, osf_project = osf_id,
        deposition_id = NA_character_, doi = NA_character_, url = NA_character_,
        files_uploaded = 0L, files_skipped = skipped[[i]],
        published = FALSE, error = conditionMessage(dep)
      )
      next
    }

    dep_id <- dep$id
    bucket <- dep$links$bucket

    ## upload each file ----
    uploaded <- 0L
    upload_err <- NA_character_
    # Worked out for the whole folder at once, because a name can only be
    # shortened safely if nothing else in the same deposition would collide
    # with it.
    # One zip preserving the real folder structure, or the files individually
    # with flattened names.
    #
    # Zenodo's own answer to "Can I upload folders/directories?" is: "Zenodo
    # does not support uploading and organising files into folders/directories.
    # Instead, you can create a ZIP archive and upload it, in which case Zenodo
    # will display the file structure inside the ZIP." So `as_zip = TRUE` is
    # the only way to keep nesting -- at the cost of nobody being able to
    # download or preview a single file without taking the whole archive.
    if (isTRUE(as_zip)) {
      stem <- path_sanitize(basename(folder), keep_sep = FALSE)
      root <- basename(folder)

      # Both archives store paths relative to the folder's PARENT, so each
      # carries the same "<project>/" prefix inside it. Unzipping both into one
      # place therefore rebuilds the original tree exactly, and unzipping only
      # one gives everything except the omitted part, still correctly nested.
      # That is what makes splitting safe: a reader who skips a multi-gigabyte
      # stimulus set still gets a working copy of everything else.
      parent <- dirname(folder)
      # list.files(root) returns paths relative to `root` ("Data/b.csv"), not
      # including it, so the prefix has to be added back -- zipping from the
      # parent needs "Study/Data/b.csv" or it finds nothing to do.
      rels <- list.files(folder, recursive = TRUE, all.files = FALSE)
      rels <- rels[!dir.exists(file.path(folder, rels))]
      rels <- file.path(root, rels)
      abs <- file.path(parent, rels)

      # Split by the SAME categories repo_check reports, so a file goes to the
      # same place whether it is being checked or archived.
      split_into <- if (is.null(split_materials) || isFALSE(split_materials)) {
        character(0)
      } else {
        split_materials
      }
      cats <- if (length(split_into)) .zenodo_classify(abs) else
        rep(NA_character_, length(rels))
      is_mat <- cats %in% split_into

      built <- character(0)
      # Each folder gets its own temporary directory, because the archive is
      # named after the folder and two folders can easily share a name -- two
      # OSF projects each holding a "Study" folder, say. Writing them all to
      # one directory means the second overwrites the first, so anything
      # reading an archive back after the loop gets another folder's files.
      # The names inside stay short and meaningful, which is what a reader
      # sees on Zenodo.
      zip_dir <- file.path(tempdir(), paste0("metacheck_zip_", i))
      dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
      make_zip <- function(which_rels, suffix) {
        if (length(which_rels) == 0) return(NULL)
        zf <- file.path(zip_dir, paste0(stem, suffix, ".zip"))
        unlink(zf)
        withr::with_dir(parent, utils::zip(zf, which_rels, flags = "-q"))
        if (file.exists(zf) && file.size(zf) > 0) zf else NULL
      }

      if (any(is_mat) && !all(is_mat)) {
        # One archive per split category, plus one for everything else, so
        # "materials" and "output" can be separated independently.
        built <- make_zip(rels[!is_mat], "")
        parts <- character(0)
        for (ct in intersect(split_into, unique(cats[is_mat]))) {
          zf <- make_zip(rels[cats %in% ct], paste0("_", ct))
          if (!is.null(zf)) {
            built <- c(built, zf)
            n_ct <- sum(cats %in% ct)
            parts <- c(parts, sprintf("%s (%d %s file%s)",
                                      basename(zf), n_ct, ct, plural(n_ct)))
          }
        }
        message(sprintf(
          "%s: split into %s (%d file%s) and %s. Unzipping all of them rebuilds the original folders; unzipping only the first gives everything except those categories.",
          basename(folder), basename(built[[1]]), sum(!is_mat),
          plural(sum(!is_mat)), paste(parts, collapse = ", ")))
      } else {
        built <- make_zip(rels, "")
      }

      if (length(built) > 0) {
        file_lists[[i]] <- built
      } else {
        message(sprintf(
          "Could not build a zip for %s; its files are uploaded individually.",
          basename(folder)))
      }
    }

    flat_names <- .zenodo_flat_names(file_lists[[i]], folder)
    for (j in seq_along(file_lists[[i]])) {
      fp <- file_lists[[i]][[j]]
      rel <- flat_names[[j]]

      ok <- tryCatch({
        if (!is.null(bucket)) {
          # The bucket API takes the file's bytes as the request body and is
          # the route Zenodo recommends; it has no 100MB form limit.
          resp <- httr2::request(sprintf("%s/%s", bucket, curl::curl_escape(rel))) |>
            .zenodo_auth(token) |>
            httr2::req_method("PUT") |>
            httr2::req_body_file(fp) |>
            httr2::req_perform()
        } else {
          # Older depositions report no bucket; fall back to the form endpoint.
          resp <- httr2::request(sprintf("%s/deposit/depositions/%s/files", api, dep_id)) |>
            .zenodo_auth(token) |>
            httr2::req_body_multipart(name = rel,
                                      file = curl::form_file(fp)) |>
            httr2::req_perform()
        }
        .zenodo_check_resp(resp, sprintf("Uploading %s", rel))
        TRUE
      }, error = \(e) {
        logger("zenodo_upload", list(error = conditionMessage(e), file = fp))
        upload_err <<- conditionMessage(e)
        FALSE
      })

      if (isTRUE(ok)) uploaded <- uploaded + 1L
      .pb_say(pb, sprintf("%s: uploaded %d of %d files", basename(folder),
                          uploaded, length(file_lists[[i]])))
    }

    if (uploaded < length(file_lists[[i]])) {
      warning(sprintf("%d of %d file%s in %s failed to upload (e.g. %s)",
                      length(file_lists[[i]]) - uploaded, length(file_lists[[i]]),
                      plural(length(file_lists[[i]])), basename(folder),
                      upload_err), call. = FALSE)
    }

    ## attach metadata ----
    meta_ok <- tryCatch({
      resp <- httr2::request(sprintf("%s/deposit/depositions/%s", api, dep_id)) |>
        .zenodo_auth(token) |>
        httr2::req_method("PUT") |>
        httr2::req_body_json(list(metadata = md)) |>
        httr2::req_perform()
      .zenodo_check_resp(resp, sprintf("Adding metadata to %s", basename(folder)))
      TRUE
    }, error = \(e) {
      warning(conditionMessage(e), call. = FALSE)
      FALSE
    })

    ## publish, only when explicitly asked ----
    doi <- dep$metadata$prereserve_doi$doi %||% NA_character_
    published <- FALSE
    if (isTRUE(publish) && isTRUE(meta_ok)) {
      pub <- tryCatch({
        resp <- httr2::request(sprintf("%s/deposit/depositions/%s/actions/publish",
                                       api, dep_id)) |>
          .zenodo_auth(token) |>
          httr2::req_method("POST") |>
          httr2::req_perform()
        .zenodo_check_resp(resp, sprintf("Publishing %s", basename(folder)))
      }, error = \(e) e)
      if (!inherits(pub, "error")) {
        published <- TRUE
        doi <- pub$doi %||% doi
      } else {
        warning(conditionMessage(pub), call. = FALSE)
      }
    }

    results[[i]] <- data.frame(
      folder = folder,
      osf_project = osf_id,
      deposition_id = as.character(dep_id),
      doi = doi,
      url = dep$links$html %||% NA_character_,
      files_uploaded = uploaded,
      files_skipped = skipped[[i]],
      published = published,
      error = NA_character_
    )
  }

  out <- dplyr::bind_rows(results)

  if (length(default_license_used) > 0) {
    # Says what to type, not just which argument exists. A reader who does not
    # already know Zenodo's licence identifiers cannot act on "set the license
    # argument"; the two commonest choices and where to find the rest close
    # that gap.
    # Names each folder in a full sentence rather than appending a list to a
    # count: "1 folder ...: one_project" made the reader work out which folder
    # was meant, and read as a tally rather than as something to act on.
    n_lic <- length(default_license_used)
    shown <- utils::head(default_license_used, 5)
    quoted <- paste(sprintf("\"%s\"", shown), collapse = ", ")
    opening <- if (n_lic == 1) {
      sprintf(
        "The folder %s comes from an OSF project without specified license information.",
        quoted)
    } else {
      sprintf(
        "%d folders come from OSF projects without specified license information: %s%s.",
        n_lic, quoted,
        if (n_lic > length(shown)) ", and others" else "")
    }

    warning(sprintf(
      paste0(
        "%s The license `%s` was added to Zenodo. You can change it on Zenodo ",
        "if needed, or choose a different one when uploading:\n",
        "    zenodo_upload(result, license = \"cc0-1.0\")   # public domain\n",
        "    zenodo_upload(result, license = \"cc-by-4.0\")  # attribution\n",
        "Other identifiers are listed at ",
        "https://zenodo.org/api/vocabularies/licenses"),
      opening, license),
      call. = FALSE)
  }

  n_ok <- sum(!is.na(out$deposition_id))
  message(sprintf(
    "\nCreated %d deposition%s on %s with %d file%s.",
    n_ok, plural(n_ok),
    if (isTRUE(sandbox)) "the Zenodo sandbox" else "Zenodo",
    sum(out$files_uploaded), plural(sum(out$files_uploaded))
  ))

  # The urls are the point of the whole run, so they are printed rather than
  # left in the returned table -- which is invisible, so a caller who does not
  # assign the result would otherwise be told to "open each url below" and
  # shown none.
  urls <- out$url[!is.na(out$url)]
  if (length(urls) > 0) {
    if (isTRUE(publish)) {
      message("Published. The records are at:")
    } else {
      message("They are unpublished drafts. Check each one, then press ",
              "Publish on Zenodo:")
    }
    for (k in seq_along(urls)) {
      message(sprintf("  %s  (%s)", urls[[k]], basename(out$folder[!is.na(out$url)][[k]])))
    }

    # Opening the pages saves the user copying each url by hand, but only in an
    # interactive session: a script, R CMD check, or the test suite must never
    # have browser windows opened on it.
    if (interactive()) {
      for (url in urls) utils::browseURL(url)
    }
  }

  invisible(out)
}

#' Escape a string for use in a regular expression
#'
#' Folder paths routinely contain characters that are regex metacharacters (`.`
#' and `(` are common in project names), so a path cannot be pasted into a
#' pattern unescaped.
#'
#' @param x a character vector
#'
#' @returns the escaped vector
#' @keywords internal
.zenodo_regex_escape <- function(x) {
  gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", x, perl = TRUE)
}
