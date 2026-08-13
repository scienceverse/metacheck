# Retrieving the parts of an OSF project that are not files. A project is often
# used as a record as much as a store: the wiki holds the protocol or the
# reading of the results, and the activity log is the only account of when
# things changed. Downloading the files alone throws that away, so
# osf_file_download(metadata = TRUE) writes it alongside them.
#
# Formats follow what each thing actually is. Wikis are served as Markdown, so
# they are saved as .md and keep their headings. Logs are tabular, so they are
# a .csv that can be sorted and filtered. Everything structured is written as
# .json exactly as the API returned it, with a short README.md summarising it
# for a reader who just opens the folder.

#' Name of the folder metadata is written to inside a project's download folder
#' @keywords internal
.osf_meta_dir <- "_osf_metadata"

#' Retrieve one OSF API listing as a parsed table
#'
#' A thin wrapper so every metadata call reports a failure the same way, and so
#' one unavailable endpoint (a private wiki, say) never stops the rest.
#'
#' @param url the API URL
#'
#' @returns the parsed `data` element, or NULL when it could not be read
#' @keywords internal
.osf_meta_get <- function(url) {
  out <- tryCatch(osf_get_all_pages(url), error = \(e) NULL)
  if (is.null(out) || !is.null(attr(out, "osf_error"))) return(NULL)
  if (length(out) == 0) return(NULL)
  out
}

#' Download an OSF project's wikis
#'
#' Each wiki page is written as its own Markdown file, named after the page.
#' Only the current version is downloaded: the OSF keeps every revision, but
#' fetching them all is one request per revision and produces a directory of
#' near-identical files, which is rarely what an archive needs.
#'
#' @param osf_id the OSF node ID
#' @param meta_dir the folder to write into
#'
#' @returns a data frame of the wikis written, or NULL when the node has none
#' @keywords internal
.osf_download_wikis <- function(osf_id, meta_dir) {
  osf_api <- getOption("metacheck.osf.api")
  wikis <- .osf_meta_get(sprintf("%s/nodes/%s/wikis/", osf_api, osf_id))
  if (is.null(wikis) || !is.data.frame(wikis) || nrow(wikis) == 0) return(NULL)

  att <- wikis$attributes
  name <- att$name %||% rep(NA_character_, nrow(wikis))
  url <- wikis$links$download %||% rep(NA_character_, nrow(wikis))

  written <- character(nrow(wikis))
  for (i in seq_len(nrow(wikis))) {
    if (is.na(url[[i]]) || !nzchar(url[[i]])) next
    body <- tryCatch({
      # Wiki content is served as text/markdown, not as a JSON:API document.
      # .osf_headers() asks for application/vnd.api+json, which this endpoint
      # answers with HTTP 406, so the Accept header is overridden here while
      # the authorisation header it adds is kept.
      resp <- httr2::request(url[[i]]) |>
        .osf_headers() |>
        httr2::req_headers(Accept = "text/markdown, text/plain, */*") |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform()
      if (httr2::resp_status(resp) != 200) NULL else httr2::resp_body_string(resp)
    }, error = \(e) NULL)
    if (is.null(body)) next

    # The wiki page name becomes a file name, so it goes through the same
    # sanitising as any other OSF name.
    fname <- paste0("wiki_", path_sanitize(name[[i]] %||% paste0("page", i),
                                           keep_sep = FALSE), ".md")
    writeLines(body, file.path(meta_dir, fname), useBytes = TRUE)
    written[[i]] <- fname
  }

  data.frame(
    wiki_id = wikis$id %||% NA_character_,
    name = name,
    size = att$size %||% NA_integer_,
    date_modified = att$date_modified %||% NA_character_,
    file = written
  )
}

#' Download an OSF project's activity log
#'
#' Written as a CSV, because a log is tabular and is read by sorting and
#' filtering it. Each entry's `params` field is a nested structure with up to
#' 14 members; the ones that are simple values become columns, and the few that
#' are themselves nested (`contributors`, `urls`, `params_node`) are dropped,
#' because a CSV cell cannot hold them. Every entry's date and action are
#' always kept, so the record of what happened and when is complete.
#'
#' @param osf_id the OSF node ID
#' @param meta_dir the folder to write into
#'
#' @returns the log as a data frame, or NULL when it could not be read
#' @keywords internal
.osf_download_logs <- function(osf_id, meta_dir) {
  osf_api <- getOption("metacheck.osf.api")
  logs <- .osf_meta_get(sprintf("%s/nodes/%s/logs/", osf_api, osf_id))
  if (is.null(logs) || !is.data.frame(logs) || nrow(logs) == 0) return(NULL)

  att <- logs$attributes
  out <- data.frame(
    date = att$date %||% NA_character_,
    action = att$action %||% NA_character_
  )

  # Flatten whichever params are simple vectors; anything nested (contributors,
  # urls, params_node) stays in the JSON rather than being mangled into a cell.
  params <- att$params
  if (is.data.frame(params)) {
    for (nm in names(params)) {
      v <- params[[nm]]
      if (is.atomic(v) && length(v) == nrow(out)) out[[nm]] <- v
    }
  }

  readr::write_csv(out, file.path(meta_dir, "logs.csv"))
  out
}

#' Collect an OSF project's structured metadata
#'
#' Everything that is a record rather than a file: the node's own attributes,
#' its contributors, licence, citation, tags, and any registrations or forks of
#' it. Returned as a list so it can be written verbatim as JSON.
#'
#' @param osf_id the OSF node ID
#'
#' @returns a named list of metadata
#' @keywords internal
.osf_node_metadata <- function(osf_id) {
  osf_api <- getOption("metacheck.osf.api")

  node <- tryCatch({
    resp <- httr2::request(sprintf(
      "%s/nodes/%s/?embed=license&embed=bibliographic_contributors",
      osf_api, osf_id)) |>
      .osf_headers() |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200) NULL else
      httr2::resp_body_json(resp, simplifyVector = TRUE)$data
  }, error = \(e) NULL)

  att <- node$attributes
  users <- node$embeds$bibliographic_contributors$data$embeds$users$data

  contributors <- list()
  if (!is.null(users) && length(users$attributes$full_name) > 0) {
    ua <- users$attributes
    contributors <- lapply(seq_along(ua$full_name), \(j) {
      list(name = ua$full_name[[j]],
           given_name = ua$given_name[[j]] %||% NA_character_,
           family_name = ua$family_name[[j]] %||% NA_character_,
           orcid = ua$social$orcid[[j]] %||% NA_character_)
    })
  }

  citation <- .osf_meta_get(sprintf("%s/nodes/%s/citation/", osf_api, osf_id))
  regs <- .osf_meta_get(sprintf("%s/nodes/%s/registrations/", osf_api, osf_id))
  forks <- .osf_meta_get(sprintf("%s/nodes/%s/forks/", osf_api, osf_id))

  list(
    osf_id = osf_id,
    osf_url = paste0("https://osf.io/", osf_id),
    title = att$title %||% NA_character_,
    description = att$description %||% NA_character_,
    category = att$category %||% NA_character_,
    public = att$public %||% NA,
    date_created = att$date_created %||% NA_character_,
    date_modified = att$date_modified %||% NA_character_,
    tags = att$tags %||% character(0),
    license = node$embeds$license$data$attributes$name %||% NA_character_,
    node_license = att$node_license %||% NULL,
    contributors = contributors,
    citation = if (is.null(citation)) NULL else citation$attributes,
    registrations = if (is.data.frame(regs)) regs$id else NULL,
    forks = if (is.data.frame(forks)) forks$id else NULL,
    retrieved = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

#' Write a readable summary of what was archived
#'
#' `metadata.json` holds everything exactly, but nothing about it is readable
#' by opening the folder. This is the file a person finds first.
#'
#' @param meta the list from [.osf_node_metadata()]
#' @param wikis the table from [.osf_download_wikis()], or NULL
#' @param logs the table from [.osf_download_logs()], or NULL
#' @param meta_dir the folder to write into
#'
#' @returns the path written, invisibly
#' @keywords internal
.osf_write_readme <- function(meta, wikis, logs, meta_dir) {
  # `%||%` only catches NULL; the API returns NA for a field it has no value
  # for, which would otherwise print as the literal "NA".
  said <- function(x, fallback) {
    if (is.null(x) || length(x) == 0 || is.na(x[[1]]) || !nzchar(x[[1]])) {
      fallback
    } else {
      as.character(x[[1]])
    }
  }

  ln <- c(
    paste("#", said(meta$title, meta$osf_id)),
    "",
    paste0("Archived from <", meta$osf_url, "> on ", meta$retrieved, "."),
    ""
  )

  desc <- said(meta$description, "")
  if (nzchar(desc)) {
    ln <- c(ln, "## Description", "", desc, "")
  }

  details <- c(
    sprintf("- OSF ID: %s", meta$osf_id),
    sprintf("- Category: %s", said(meta$category, "unknown")),
    sprintf("- Visibility: %s", if (isTRUE(meta$public)) "public" else "private"),
    sprintf("- Created: %s", said(meta$date_created, "unknown")),
    sprintf("- Last modified: %s", said(meta$date_modified, "unknown")),
    sprintf("- License: %s", said(meta$license, "none recorded on the OSF"))
  )
  if (length(meta$tags) > 0) {
    details <- c(details, sprintf("- Tags: %s", paste(meta$tags, collapse = ", ")))
  }
  ln <- c(ln, "## Project", "", details, "")

  if (length(meta$contributors) > 0) {
    who <- vapply(meta$contributors, \(c) {
      if (!is.na(c$orcid) && nzchar(c$orcid)) {
        sprintf("- %s (ORCID %s)", c$name, c$orcid)
      } else sprintf("- %s", c$name)
    }, character(1))
    ln <- c(ln, "## Contributors", "", who, "")
  }

  # Every item is described either way, so "no wiki" is stated rather than left
  # to be inferred from a missing line -- the reader can always tell the
  # difference between "this project had none" and "this was never retrieved".
  got_wikis <- !is.null(wikis) && nrow(wikis) > 0 && any(nzchar(wikis$file))
  contents <- if (got_wikis) {
    sprintf("- %d wiki page%s: %s", sum(nzchar(wikis$file)),
            plural(sum(nzchar(wikis$file))),
            paste(wikis$file[nzchar(wikis$file)], collapse = ", "))
  } else {
    "- Wiki pages: none (this project has no wiki)"
  }

  contents <- c(contents, if (!is.null(logs) && nrow(logs) > 0) {
    sprintf("- logs.csv: %d activity log entr%s, %s to %s",
            nrow(logs), if (nrow(logs) == 1) "y" else "ies",
            substr(min(logs$date, na.rm = TRUE), 1, 10),
            substr(max(logs$date, na.rm = TRUE), 1, 10))
  } else {
    "- logs.csv: empty (no activity log could be retrieved)"
  })

  # Say what is actually in the file. It holds the project's own record --
  # title, description, tags, licence, contributors, dates, citation -- and a
  # list of the wiki pages, but not their text (that is in the .md files) and
  # not the activity log (that is logs.csv). Claiming "everything above" was
  # wrong, and wrong in the direction that matters: someone reading only the
  # JSON would think they had the log.
  contents <- c(contents, paste(
    "- metadata.json: the project's title, description, tags, licence,",
    "contributors, dates and citation, as the OSF API returned them"))
  ln <- c(ln, "## This folder", "", contents, "")

  path <- file.path(meta_dir, "README.md")
  writeLines(ln, path, useBytes = TRUE)
  invisible(path)
}

#' Download an OSF Project's Metadata
#'
#' Retrieves the parts of an OSF project that are not files -- its wiki pages,
#' its activity log, and its descriptive metadata -- and writes them into an
#' `_osf_metadata` folder inside `download_to`. Called by [osf_file_download()]
#' when `metadata = TRUE`, and usable on its own.
#'
#' A project is often a record as much as a store: the wiki may hold the
#' protocol or the interpretation of the results, and the activity log is the
#' only account of when things changed. An archive of the files alone loses
#' that.
#'
#' Each thing is written in the form it actually takes:
#'
#' * `wiki_<name>.md` -- one file per wiki page. The OSF serves wikis as
#'   Markdown, so headings and lists survive. Only the current version is
#'   downloaded.
#' * `logs.csv` -- the activity log, one row per entry. Nested fields are left
#'   to `metadata.json`.
#' * `metadata.json` -- title, description, tags, licence, contributors with
#'   ORCIDs, dates, citation, registrations and forks, as the API returned them.
#' * `README.md` -- a readable summary of all of the above.
#'
#' Every project gets the same four kinds of file, whether or not it has
#' anything to put in them, so a folder's structure never depends on what the
#' project happened to contain.
#'
#' @param osf_id an OSF node ID
#' @param download_to the project's download folder; the metadata folder is
#'   created inside it
#' @param pb a progress bar passed from another function
#'
#' @returns the path to the metadata folder, invisibly
#' @keywords internal
.osf_metadata_download <- function(osf_id, download_to, pb = NULL) {
  osf_id <- osf_check_id(osf_id)[[1]]
  if (is.na(osf_id)) return(invisible(NULL))

  meta_dir <- file.path(download_to, .osf_meta_dir)
  dir.create(meta_dir, showWarnings = FALSE, recursive = TRUE)

  if (!is.null(pb)) {
    sprintf("Retrieving metadata for %s", osf_id) |>
      list(what = _) |>
      pb$tick(0, tokens = _)
  }

  wikis <- .osf_download_wikis(osf_id, meta_dir)
  logs <- .osf_download_logs(osf_id, meta_dir)
  meta <- .osf_node_metadata(osf_id)

  # An index of what else was written, so the JSON says where the parts it does
  # not contain have gone: wiki TEXT is in the .md files, the activity log is
  # in logs.csv. Without this a reader of the JSON alone has no way to know a
  # log was retrieved at all.
  meta$wikis <- if (is.null(wikis)) list() else wikis
  meta$files_written <- list(
    wiki_pages = if (is.null(wikis)) character(0) else
      wikis$file[nzchar(wikis$file)],
    logs = if (is.null(logs)) NULL else
      list(file = "logs.csv", entries = nrow(logs),
           first = min(logs$date, na.rm = TRUE),
           last = max(logs$date, na.rm = TRUE))
  )
  jsonlite::write_json(meta, file.path(meta_dir, "metadata.json"),
                       auto_unbox = TRUE, pretty = TRUE, null = "null")

  # Written even when the project has no log, so every archived project has the
  # same files and a missing one always means something went wrong rather than
  # "this project had none".
  if (is.null(logs)) {
    readr::write_csv(data.frame(date = character(0), action = character(0)),
                     file.path(meta_dir, "logs.csv"))
  }

  .osf_write_readme(meta, wikis, logs, meta_dir)

  invisible(meta_dir)
}
