#' Repository Check
#'
#' @description
#' This module retrieves information from repositories.
#'
#' @details
#' The Repository Check module lists files on the OSF, GitHub, ResearchBox, PsychArchives, Zenodo, Dataverse, Figshare, Dryad, ReShare, and 4TU.ResearchData based on links in the manuscript.
#'
#' When a linked OSF page is a registration, its `registered_from` project (the
#' one it was registered from, which the manuscript itself may never link
#' directly) is also checked: if that project is public its files are listed
#' under its own URL, and if it is closed or otherwise inaccessible the report
#' flags it explicitly, since some registrations only describe their content
#' rather than mirroring it.
#'
#' If you want to extend the package to be able to download files from additional data repositories reach out to the Metacheck development team.
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param peek_zips if TRUE (default), read each `.zip`'s file listing over HTTP
#'   without downloading it (one Range request per archive, see [zip_peek()]) and
#'   list its contents in place of the archive row. Set FALSE to leave archives
#'   as single opaque entries and make no per-archive request. Only `.zip` can be
#'   inspected this way; other archive formats have no tail index.
#' @param model the LLM model name (see `llm_model_list()`), used only when
#'   `llm_use(TRUE)` for study grouping the deterministic passes cannot place
#' @param params a named list passed to `llm()`, used only when `llm_use(TRUE)`
#'
#' @returns a list
repo_check <- function(paper, local_path = NULL, local_only = FALSE,
                       peek_zips = TRUE,
                       model = llm_model(),
                       params = list()) {
  # get repository links ----
  # paper <- demopaper()
  pb <- pb(NA, "(:spin) :what")
  pb$tick(0, list(what = "Starting Repo Check"))
  # if (!is.null(local_path)) {
  #   pb$message("If folders are stored online, the check might be slow as all files need to be downloaded.")
  # }
  on.exit({
    pb$tick(0, list(what = "Repo Check Complete"))
    pb$terminate()
  })

  ## get links ----
  if (isTRUE(local_only)) {
    empty_links        <- dplyr::tibble(paper_id = character(), href = character(), repo_type = character())
    osf_links_found    <- empty_links
    github_links_found <- empty_links
    rb_links_found     <- empty_links
    pa_links_found     <- empty_links
    zenodo_links_found <- empty_links
    dataverse_links_found <- empty_links
    figshare_links_found <- empty_links
    dryad_links_found <- empty_links
    reshare_links_found <- empty_links
    researchdata4tu_links_found <- empty_links
  } else {
    osf_links_found <- osf_links(paper)
    # exclude psychsci badges
    if ("href" %in% names(osf_links_found)) {
      osf_links_found <- osf_links_found |>
        dplyr::filter(!grepl("tvyxz", href))
    }
    osf_links_found$repo_type    <- "osf"
    github_links_found <- github_links(paper)
    github_links_found$repo_type <- "github"
    rb_links_found     <- rbox_links(paper)
    rb_links_found$repo_type     <- "researchbox"
    pa_links_found     <- psycharchives_links(paper)
    pa_links_found$repo_type     <- "psycharchives"
    zenodo_links_found <- zenodo_links(paper)
    zenodo_links_found$repo_type <- "zenodo"
    dataverse_links_found <- dataverse_links(paper)
    dataverse_links_found$repo_type <- "dataverse"
    figshare_links_found <- figshare_links(paper)
    figshare_links_found$repo_type <- "figshare"
    dryad_links_found <- dryad_links(paper)
    dryad_links_found$repo_type <- "dryad"
    reshare_links_found <- reshare_links(paper)
    reshare_links_found$repo_type <- "reshare"
    researchdata4tu_links_found <- researchdata4tu_links(paper)
    researchdata4tu_links_found$repo_type <- "researchdata4tu"
  }

  ## organise repos in a table
  cols <- c("paper_id", "href", "repo_type")
  repos <- dplyr::bind_rows(
    osf_links_found[, cols],
    github_links_found[, cols],
    rb_links_found[, cols],
    pa_links_found[, cols],
    zenodo_links_found[, cols],
    dataverse_links_found[, cols],
    figshare_links_found[, cols],
    dryad_links_found[, cols],
    reshare_links_found[, cols],
    researchdata4tu_links_found[, cols]
  ) |> dplyr::distinct()
  names(repos)[2] <- "repo_url"
  repos$repo_error <- NA_character_

  # get files ----

  ## OSF ----
  osf_urls <- repos |>
    dplyr::filter(repo_type == "osf") |>
    _$repo_url |>
    unique()
  # BUG FIX: captured before any row removal below. A paper whose ONLY OSF
  # links are registrations (e.g. a Registered Report citing "Open-Ended
  # Registration" entries) has every one of those rows stripped out of
  # `repos` by the registration-removal filter further down (`osf_to_remove`)
  # — that filter's intent is correct (a registration is never the real
  # storage location, see the comments below), but it used to leave `repos`
  # completely empty with nothing further downstream to add rows back onto.
  # Reading `repos$paper_id[[1]]` at that point crashed with "subscript out
  # of bounds", and before this fix existed, an emptied `repos` also tripped
  # the module's early "no repos found" return further down, silently
  # discarding every file repo_check HAD found (confirmed via a real paper
  # with 4 registration links and 12 real files: `repo_check` reported
  # traffic_light "na" / "no repositories found" even though the files were
  # right there in osf_files_df). Caching paper_id here, before the filter
  # runs, means new rows (parent projects, orphaned registrations) can always
  # be added back later even when `repos` was emptied in between.
  #
  # The `[[1]]` must be guarded, not assumed: `repos` having no OSF rows AT
  # ALL is an ordinary case, not an edge case, and indexing an empty vector
  # throws the very "subscript out of bounds" this block exists to prevent.
  # It happens for a paper with no repository links whatsoever (every
  # test_paper() in the suite), and — the case that matters in production —
  # for any real paper whose links are all GitHub/Zenodo/ResearchBox/
  # PsychArchives with no OSF among them. Fall back to the paper's own id,
  # which is what this value is for anyway (it labels rows added back later)
  # and is exactly what every other row in `repos` carries.
  osf_paper_ids <- repos$paper_id[repos$repo_type == "osf"]
  osf_paper_id <- if (length(osf_paper_ids) > 0) osf_paper_ids[[1]] else
    paper_id(paper)[[1]]

  osf_files_df <- data.frame(repo_name = character(0))
  if (length(osf_urls) > 0) {
    tryCatch({
      suppressWarnings({
        osf_info <- lapply(osf_urls, \(x) {
          osf_files <- osf_info(x, recursive = TRUE, pb = pb)
          osf_files$repo_name <- x
          osf_files
        }) |> dplyr::bind_rows()
      })

      # A registration is never itself the place files "live": either it
      # mirrors files into its own storage (in which case those files really
      # belong to the project it was registered from — see below), or it only
      # describes what was shared and the real files live in that project.
      # `parent` is `registered_from`, captured by .osf_reg_data(); a
      # registration without one (should not normally happen) is left keyed
      # to its own URL below so its files are never silently dropped.
      reg_url_to_parent <- character(0)
      if ("parent" %in% names(osf_info)) {
        reg_rows <- osf_info |>
          dplyr::filter(osf_type %in% "registrations", !is.na(parent), !is.na(osf_url))
        if (nrow(reg_rows) > 0) {
          reg_url_to_parent <- stats::setNames(
            sprintf("https://osf.io/%s", reg_rows$parent), reg_rows$osf_url)
        }
      }

      # "kind" only in table if there are files
      if ("kind" %in% names(osf_info)) {
        osf_file_list <- osf_info |>
          dplyr::filter(kind == "file", !isFALSE(public))

        # Files found via a registration are attributed to the project it was
        # registered from (the real, reusable location), not the registration
        # URL the manuscript happened to cite.
        file_repo_url <- osf_file_list$repo_name
        remapped <- reg_url_to_parent[file_repo_url]
        has_parent <- !is.na(remapped)
        file_repo_url[has_parent] <- remapped[has_parent]

        osf_files_df <- data.frame(
          repo_url = file_repo_url,
          file_name = osf_file_list$name,
          file_path = gsub("^/+", "", osf_file_list$path),
          file_url = osf_file_list$download_url,
          file_location = rep(NA_character_, nrow(osf_file_list)),
          file_size = osf_file_list$size,
          file_type = osf_file_list$filetype,
          # provider (osfstorage / dropbox / github / ...) drives the
          # Waterbutler-zip decision in download_repo_files(): only osfstorage is
          # coverable by the node's ?zip= endpoint. NULL when osf_info did not
          # return the column, which download_repo_files() falls back on gracefully.
          provider = osf_file_list$provider %||% NA_character_
        )
      }

      # remove registrations (and other non-node/file types) from repos list —
      # a registration is never the real storage location (see above); its
      # parent project row is added below instead, public or closed.
      #
      # BUG THIS USED TO CAUSE: this filter (pre-existing, not new) drops
      # EVERY registration URL from `repos`, including ones whose files WERE
      # successfully listed into osf_files_df above. For a paper whose
      # manuscript links are ALL registrations (a Registered Report citing
      # OSF "Open-Ended Registration" entries for data/materials/scripts —
      # the common case, since PsychSci-style badges link the registration,
      # not the underlying project), this used to leave `repos` with ZERO
      # rows. That tripped the module's early "no repos found" branch further
      # down, so the whole module reported traffic_light "na" and "we found
      # no links to repositories" — discarding files repo_check had actually
      # already found. The block below (following `parent`/`registered_from`)
      # exists specifically to give every removed registration's files a new
      # home in `repos` (the project it was registered from) so they are
      # never orphaned by this removal.
      osf_to_remove <- osf_info |>
        dplyr::filter(!osf_type %in% c("nodes", "files", "private")) |>
        _$osf_url
      repos <- repos[!repos$repo_url %in% osf_to_remove, ]

      # note private repos
      private_repos <- osf_info |>
        dplyr::filter(osf_type %in% "private") |>
        _$osf_url
      if (length(private_repos)) {
        repos$repo_error[repos$repo_url %in% private_repos] <- "private"
      }

      # follow registrations back to their origin project ----
      # A registration is a locked snapshot; many "Open-Ended Registration"
      # entries (esp. older OSF schemas) hold only a DESCRIPTION of what was
      # shared, with the actual files living in the project it was registered
      # from (`registered_from`, captured as `parent` by .osf_reg_data() and
      # `reg_url_to_parent` above). That project was never linked in the
      # manuscript, so osf_links() cannot find it — but if it is public, its
      # files are exactly what the author meant to share, and if it is closed,
      # the author should be told their registration points at content nobody
      # can reach. Query only the parents not already directly linked (those
      # already have a row and were already queried as part of osf_urls).
      parent_urls_all <- unique(reg_url_to_parent)
      parent_urls_new <- setdiff(parent_urls_all, osf_urls)

      closed_parent_urls <- character(0)
      if (length(parent_urls_new) > 0) {
        parent_info <- suppressWarnings(
          lapply(parent_urls_new, \(x) {
            pf <- osf_info(x, recursive = TRUE, pb = pb)
            pf$repo_name <- x
            pf
          }) |> dplyr::bind_rows()
        )

        # public parent: its files belong under the PARENT's real URL (the
        # place a reader would actually need to look). Any file also reached
        # directly via a registration is deduplicated later by the caller
        # (same repo_url + file_url + file_path).
        if ("kind" %in% names(parent_info)) {
          parent_file_list <- parent_info |>
            dplyr::filter(kind == "file", !isFALSE(public))
          if (nrow(parent_file_list) > 0) {
            osf_files_df <- dplyr::bind_rows(
              osf_files_df,
              data.frame(
                repo_url = parent_file_list$repo_name,
                file_name = parent_file_list$name,
                file_path = gsub("^/+", "", parent_file_list$path),
                file_url = parent_file_list$download_url,
                file_location = rep(NA_character_, nrow(parent_file_list)),
                file_size = parent_file_list$size,
                file_type = parent_file_list$filetype,
                provider = parent_file_list$provider %||% NA_character_
              )
            )
          }
        }

        # The parent's OWN row (osf_url == its own URL, not a child/file row
        # picked up by the recursive walk) tells us whether it is reachable.
        closed_parent_urls <- parent_info |>
          dplyr::filter(osf_url %in% parent_urls_new,
                        is.na(osf_type) | osf_type %in% "private") |>
          _$osf_url |>
          unique()
        # A parent osf_info() could not resolve at all (e.g. request failed)
        # never gets its own row in parent_info; still flag it as closed
        # rather than silently listing nothing for it.
        closed_parent_urls <- union(closed_parent_urls,
          setdiff(parent_urls_new, parent_info$osf_url))
      }

      # Every parent needs exactly one repos row: closed ones flagged, public
      # ones plain (their files were just added above, or already existed if
      # directly linked in the manuscript). A closed parent whose files were
      # STILL found (mirrored into the registration's own storage — see
      # reg_url_to_parent remapping above) is a milder note than one with no
      # files anywhere: the content is not actually lost, just its permanent
      # (parent-project) home is inaccessible.
      if (length(parent_urls_all) > 0) {
        missing_parent_rows <- setdiff(parent_urls_all, repos$repo_url)
        if (length(missing_parent_rows) > 0) {
          has_files <- missing_parent_rows %in% osf_files_df$repo_url
          repo_err <- rep(NA_character_, length(missing_parent_rows))
          is_closed <- missing_parent_rows %in% closed_parent_urls
          # For the "files mirrored" case the report needs to name WHICH
          # registration the files actually came from (reg_url_to_parent is
          # keyed the other way round: registration URL -> parent URL), so
          # invert it and fold the registration URL(s) into the error string
          # itself — `repos` has no spare column to carry it separately
          # without rippling into repo_tbl/summary_table construction below.
          reg_for_parent <- vapply(missing_parent_rows, function(p) {
            regs <- names(reg_url_to_parent)[reg_url_to_parent == p]
            paste(regs, collapse = ", ")
          }, character(1))
          repo_err[is_closed & has_files]  <- paste0(
            "closed registration source (files retrieved from registration: ",
            reg_for_parent[is_closed & has_files], ")")
          repo_err[is_closed & !has_files] <- "closed registration source"
          repos <- dplyr::bind_rows(
            repos,
            data.frame(
              paper_id = osf_paper_id,
              repo_url = missing_parent_rows,
              repo_type = "osf",
              repo_error = repo_err
            )
          )
        }
      }

      # Registrations with no `parent` at all (should not normally happen):
      # their files were left keyed to the registration's own URL above, so
      # give that URL back a repos row rather than losing it to the removal
      # filter above with nowhere for its files to land.
      orphan_reg_urls <- setdiff(
        unique(osf_files_df$repo_url[osf_files_df$repo_url %in% osf_urls]),
        repos$repo_url)
      if (length(orphan_reg_urls) > 0) {
        repos <- dplyr::bind_rows(
          repos,
          data.frame(paper_id = osf_paper_id, repo_url = orphan_reg_urls,
                    repo_type = "osf", repo_error = NA_character_)
        )
      }
    }, error = \(e) {
      # A failure anywhere in this block (network error, auth failure, rate
      # limit, ...) aborts before any OSF url below it gets a chance to be
      # flagged, so on error every OSF url still lacking a repo_error is
      # attributed to this failure -- otherwise it silently looks like an
      # empty repository (0 files, no error) instead of a failed listing, and
      # the repos-filter below (`in_files`) would drop it from the report
      # entirely since it has neither files nor a repo_error.
      unflagged <- osf_urls[is.na(repos$repo_error[match(osf_urls, repos$repo_url)])]
      repos$repo_error[repos$repo_url %in% unflagged] <<- conditionMessage(e)
    })
  }

  ## GitHub ----
  github_urls <- repos |>
    dplyr::filter(repo_type == "github") |>
    _$repo_url |>
    unique()
  github_files_df <- data.frame(repo_name = character(0))
  if (length(github_urls) > 0) {
    # github_tree_files() fetches repo metadata + full tree in 2 API requests
    # (vs. N recursive /contents/ calls), so a repository is listed in full
    # whatever its size — the same treatment every other source gets. Only a
    # tree GitHub's own API reports as truncated (>100,000 items) comes back
    # unlisted. How much of a repository is DOWNLOADED is capped separately,
    # by download_repo_files() in data_check.
    gh_results <- lapply(github_urls, function(url) {
      tryCatch(
        github_tree_files(url),
        error = \(e) list(gated = TRUE, reason = conditionMessage(e),
                          files = NULL, default_branch = NA_character_))
    })
    names(gh_results) <- github_urls

    for (url in github_urls) {
      r <- gh_results[[url]]
      if (isTRUE(r$gated)) {
        repos$repo_error[repos$repo_url == url] <- r$reason
        warning(sprintf("Repository %s was not listed: %s.", url, r$reason),
                call. = FALSE)
        paste0("Skipping GitHub repo (", r$reason, "): ", url) |>
          list(what = _) |>
          pb$tick(0, tokens = _)
      }
    }

    good_files <- Filter(Negate(is.null),
                         lapply(gh_results, \(r) if (!isTRUE(r$gated)) r$files else NULL))
    if (length(good_files) > 0) {
      github_file_list <- dplyr::bind_rows(good_files)
      github_file_list <- github_file_list[
        !is.na(github_file_list$type) & github_file_list$type != "dir", , drop = FALSE]
      if (nrow(github_file_list) > 0) {
        github_files_df <- dplyr::tibble(
          repo_url      = github_file_list$repo,
          file_name     = github_file_list$name,
          file_path     = github_file_list$path,
          file_url      = github_file_list$download_url,
          file_location = NA_character_,
          file_size     = github_file_list$size,
          file_type     = github_file_list$type
        )
      }
    }
  }

  ## ResearchBox ----
  rb_urls <- repos |>
    dplyr::filter(repo_type == "researchbox") |>
    _$repo_url |>
    unique()
  rb_files_df <- data.frame(repo_name = character(0))
  if (length(rb_urls) > 0) {
    tryCatch({
      rb_file_list <- rbox_file_download(rb_urls, pb = pb) |>
        dplyr::filter(!isdir)
      rb_files_df <- data.frame(
        repo_url = rb_file_list$rb_url,
        # rb_file_list$name is the RELATIVE PATH within the unzipped
        # ResearchBox archive (e.g. "ResearchBox 801/Materials/foo.pdf" —
        # list.files(..., recursive = TRUE) in rbox_file_download()), not a
        # bare basename. Every OTHER source (OSF, GitHub, PsychArchives)
        # keeps file_name as the basename and file_path as the full relative
        # path as two DIFFERENT values; ResearchBox previously set both to
        # this same full path, which made file_name/file_path (and any
        # report table showing both, e.g. repo_check's own "File
        # Classification" section) look duplicated even though the data was
        # correct — every row really was one file, just with two identically-
        # valued columns.
        file_name = basename(rb_file_list$name),
        file_path = rb_file_list$name,
        file_url = rb_file_list$rb_url,
        file_location = rb_file_list$file_location,
        file_size = rb_file_list$size,
        file_type = rb_file_list$type
      )
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      repos$repo_error[repos$repo_url %in% rb_urls] <<- conditionMessage(e)
    })
  }

  ## PsychArchives ----
  # Unlike ResearchBox, PsychArchives lists public files via its DSpace REST API
  # without downloading them, so this only fills file_url / file_size and leaves
  # file_location = NA; download_repo_files() fetches the bytes later (deferred,
  # like Zenodo/OSF), which keeps the per-file/per-repo size caps in force.
  pa_urls <- repos |>
    dplyr::filter(repo_type == "psycharchives") |>
    _$repo_url |>
    unique()
  pa_files_df <- data.frame(repo_name = character(0))
  if (length(pa_urls) > 0) {
    tryCatch({
      pa_file_list <- psycharchives_file_download(pa_urls, pb = pb)

      # Flag items whose DSpace rights are restricted/embargoed. The REST API
      # only lists publicly retrievable bitstreams, so restricted files never
      # appear in pa_files_df — the item's rights flag is the only signal that
      # some files are machine-inaccessible. Reuses the rights attribute
      # psycharchives_file_download() carries out (no extra API call).
      pa_rights <- attr(pa_file_list, "rights")
      if (length(pa_rights) > 0) {
        restricted <- names(pa_rights)[
          !is.na(pa_rights) &
            grepl("restricted|embargo", pa_rights, ignore.case = TRUE)]
        if (length(restricted) > 0) {
          repos$repo_error[repos$repo_url %in% restricted] <-
            "restricted access"
        }
      }

      if (!is.null(pa_file_list) && nrow(pa_file_list) > 0) {
        pa_file_list <- pa_file_list |> dplyr::filter(!isdir)
        pa_files_df <- data.frame(
          repo_url = pa_file_list$pa_url,
          file_name = pa_file_list$name,
          file_path = pa_file_list$name,
          file_url = pa_file_list$file_url,
          file_location = pa_file_list$file_location,
          file_size = pa_file_list$size,
          file_type = pa_file_list$type
        )
      }
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      # `restricted` (if the block got that far before failing) already has
      # its own repo_error, so only backfill urls still unflagged.
      unflagged <- pa_urls[is.na(repos$repo_error[match(pa_urls, repos$repo_url)])]
      repos$repo_error[repos$repo_url %in% unflagged] <<- conditionMessage(e)
    })
  }

  ## Zenodo ----
  zenodo_urls <- repos |>
    dplyr::filter(repo_type == "zenodo") |>
    _$repo_url |>
    unique()
  zenodo_files_df <- data.frame(repo_name = character(0))
  if (length(zenodo_urls) > 0) {
    tryCatch({
      .zenodo_info <- suppressMessages(zenodo_info(zenodo_urls, pb = pb))

      if (nrow(.zenodo_info) > 0 && "files" %in% names(.zenodo_info)) {
        file_rows <- lapply(seq_len(nrow(.zenodo_info)), function(i) {
          files_i <- .zenodo_info$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            file_url <- NA_character_
            if (!is.null(f$links) && !is.null(f$links$self)) {
              file_url <- as.character(f$links$self)
            }

            data.frame(
              repo_url = as.character(.zenodo_info$zenodo_url[[i]]),
              file_name = as.character(f$key %||% NA_character_),
              file_path = as.character(f$key %||% NA_character_),
              file_url = file_url,
              file_location = NA_character_,
              file_size = as.numeric(f$size %||% NA_real_)
            )
          })

          dplyr::bind_rows(rows_i)
        })

        zenodo_files_df <- dplyr::bind_rows(file_rows)

        if (nrow(zenodo_files_df) > 0) {
          zenodo_files_df$ext <- tolower(sub("^.*\\.", "", basename(zenodo_files_df$file_name)))
          no_ext <- !is.na(zenodo_files_df$file_name) &
            !grepl("\\.", basename(zenodo_files_df$file_name))
          zenodo_files_df$ext[no_ext] <- NA_character_

          zenodo_files_df <- zenodo_files_df |>
            dplyr::left_join(metacheck::file_types, by = "ext") |>
            dplyr::rename(file_type = type)

          zenodo_files_df$ext <- NULL
        } else {
          zenodo_files_df$file_type <- character(0)
        }
      }
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      repos$repo_error[repos$repo_url %in% zenodo_urls] <<- conditionMessage(e)
    })
  }

  ## Dataverse ----
  # Like PsychArchives, Dataverse's REST API lists a dataset's files (name,
  # size, checksum, per-file download URL) without downloading them, so this
  # only fills file_url / file_size and leaves file_location = NA;
  # download_repo_files() fetches the bytes later (deferred, like Zenodo/OSF).
  dv_urls <- repos |>
    dplyr::filter(repo_type == "dataverse") |>
    _$repo_url |>
    unique()
  dv_files_df <- data.frame(repo_name = character(0))
  if (length(dv_urls) > 0) {
    tryCatch({
      .dv_info <- suppressMessages(dataverse_info(dv_urls))

      if (nrow(.dv_info) > 0 && "files" %in% names(.dv_info)) {
        file_rows <- lapply(seq_len(nrow(.dv_info)), function(i) {
          files_i <- .dv_info$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            df <- f$dataFile %||% list()
            file_url <- if (!is.null(df$id))
              sprintf("https://%s/api/access/datafile/%s",
                     .dv_info$dataverse_host[[i]], df$id)
            else NA_character_

            data.frame(
              repo_url = as.character(.dv_info$dataverse_url[[i]]),
              file_name = as.character(f$label %||% df$filename %||% NA_character_),
              file_path = as.character(f$label %||% df$filename %||% NA_character_),
              file_url = file_url,
              file_location = NA_character_,
              file_size = as.numeric(df$filesize %||% NA_real_)
            )
          })

          dplyr::bind_rows(rows_i)
        })

        dv_files_df <- dplyr::bind_rows(file_rows)

        if (nrow(dv_files_df) > 0) {
          dv_files_df$ext <- tolower(sub("^.*\\.", "", basename(dv_files_df$file_name)))
          no_ext <- !is.na(dv_files_df$file_name) &
            !grepl("\\.", basename(dv_files_df$file_name))
          dv_files_df$ext[no_ext] <- NA_character_

          dv_files_df <- dv_files_df |>
            dplyr::left_join(metacheck::file_types, by = "ext") |>
            dplyr::rename(file_type = type)

          dv_files_df$ext <- NULL
        } else {
          dv_files_df$file_type <- character(0)
        }
      }
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      repos$repo_error[repos$repo_url %in% dv_urls] <<- conditionMessage(e)
    })
  }

  ## Figshare ----
  # Like Dataverse/PsychArchives, Figshare's REST API lists an article's files
  # (name, size, checksum, per-file download URL) without downloading them, so
  # this only fills file_url / file_size and leaves file_location = NA;
  # download_repo_files() fetches the bytes later (deferred, like Zenodo/OSF).
  fs_urls <- repos |>
    dplyr::filter(repo_type == "figshare") |>
    _$repo_url |>
    unique()
  fs_files_df <- data.frame(repo_name = character(0))
  if (length(fs_urls) > 0) {
    tryCatch({
      .fs_info <- suppressMessages(figshare_info(fs_urls))

      if (nrow(.fs_info) > 0 && "files" %in% names(.fs_info)) {
        file_rows <- lapply(seq_len(nrow(.fs_info)), function(i) {
          files_i <- .fs_info$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            data.frame(
              repo_url = as.character(.fs_info$figshare_url[[i]]),
              file_name = as.character(f$name %||% NA_character_),
              file_path = as.character(f$name %||% NA_character_),
              file_url = as.character(f$download_url %||% NA_character_),
              file_location = NA_character_,
              file_size = as.numeric(f$size %||% NA_real_)
            )
          })

          dplyr::bind_rows(rows_i)
        })

        fs_files_df <- dplyr::bind_rows(file_rows)

        if (nrow(fs_files_df) > 0) {
          fs_files_df$ext <- tolower(sub("^.*\\.", "", basename(fs_files_df$file_name)))
          no_ext <- !is.na(fs_files_df$file_name) &
            !grepl("\\.", basename(fs_files_df$file_name))
          fs_files_df$ext[no_ext] <- NA_character_

          fs_files_df <- fs_files_df |>
            dplyr::left_join(metacheck::file_types, by = "ext") |>
            dplyr::rename(file_type = type)

          fs_files_df$ext <- NULL
        } else {
          fs_files_df$file_type <- character(0)
        }
      }
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      repos$repo_error[repos$repo_url %in% fs_urls] <<- conditionMessage(e)
    })
  }

  ## Dryad ----
  # Like Figshare/Dataverse, Dryad's REST API lists a dataset's files (path,
  # size, digest, per-file download URL) without downloading them, so this
  # only fills file_url / file_size and leaves file_location = NA;
  # download_repo_files() fetches the bytes later (deferred, like Zenodo/OSF).
  dryad_urls <- repos |>
    dplyr::filter(repo_type == "dryad") |>
    _$repo_url |>
    unique()
  dryad_files_df <- data.frame(repo_name = character(0))
  if (length(dryad_urls) > 0) {
    tryCatch({
      .dryad_info_tbl <- suppressMessages(dryad_info(dryad_urls))

      if (nrow(.dryad_info_tbl) > 0 && "files" %in% names(.dryad_info_tbl)) {
        file_rows <- lapply(seq_len(nrow(.dryad_info_tbl)), function(i) {
          files_i <- .dryad_info_tbl$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            dl_href <- f$`_links`$`stash:download`$href %||% NA_character_
            file_url <- if (!is.na(dl_href)) paste0("https://datadryad.org", dl_href)
                        else NA_character_

            data.frame(
              repo_url = as.character(.dryad_info_tbl$dryad_url[[i]]),
              file_name = as.character(f$path %||% NA_character_),
              file_path = as.character(f$path %||% NA_character_),
              file_url = file_url,
              file_location = NA_character_,
              file_size = as.numeric(f$size %||% NA_real_)
            )
          })

          dplyr::bind_rows(rows_i)
        })

        dryad_files_df <- dplyr::bind_rows(file_rows)

        if (nrow(dryad_files_df) > 0) {
          dryad_files_df$ext <- tolower(sub("^.*\\.", "", basename(dryad_files_df$file_name)))
          no_ext <- !is.na(dryad_files_df$file_name) &
            !grepl("\\.", basename(dryad_files_df$file_name))
          dryad_files_df$ext[no_ext] <- NA_character_

          dryad_files_df <- dryad_files_df |>
            dplyr::left_join(metacheck::file_types, by = "ext") |>
            dplyr::rename(file_type = type)

          dryad_files_df$ext <- NULL
        } else {
          dryad_files_df$file_type <- character(0)
        }
      }
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      repos$repo_error[repos$repo_url %in% dryad_urls] <<- conditionMessage(e)
    })
  }

  ## ReShare ----
  # Like Dataverse/Figshare/Dryad, ReShare's EPrints REST API lists a
  # deposit's files (name, size, hash, per-file download URL) without
  # downloading them, so this only fills file_url / file_size and leaves
  # file_location = NA; download_repo_files() fetches the bytes later
  # (deferred, like Zenodo/OSF).
  reshare_urls <- repos |>
    dplyr::filter(repo_type == "reshare") |>
    _$repo_url |>
    unique()
  reshare_files_df <- data.frame(repo_name = character(0))
  if (length(reshare_urls) > 0) {
    tryCatch({
      .reshare_info_tbl <- suppressMessages(reshare_info(reshare_urls))

      if (nrow(.reshare_info_tbl) > 0 && "files" %in% names(.reshare_info_tbl)) {
        file_rows <- lapply(seq_len(nrow(.reshare_info_tbl)), function(i) {
          files_i <- .reshare_info_tbl$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            file_url <- f$uri %||% NA_character_
            if (!is.na(file_url)) file_url <- sub("^http://", "https://", file_url)

            data.frame(
              repo_url = as.character(.reshare_info_tbl$reshare_url[[i]]),
              file_name = as.character(f$filename %||% NA_character_),
              file_path = as.character(f$filename %||% NA_character_),
              file_url = file_url,
              file_location = NA_character_,
              file_size = as.numeric(f$filesize %||% NA_real_)
            )
          })

          dplyr::bind_rows(rows_i)
        })

        reshare_files_df <- dplyr::bind_rows(file_rows)

        if (nrow(reshare_files_df) > 0) {
          reshare_files_df$ext <- tolower(sub("^.*\\.", "", basename(reshare_files_df$file_name)))
          no_ext <- !is.na(reshare_files_df$file_name) &
            !grepl("\\.", basename(reshare_files_df$file_name))
          reshare_files_df$ext[no_ext] <- NA_character_

          reshare_files_df <- reshare_files_df |>
            dplyr::left_join(metacheck::file_types, by = "ext") |>
            dplyr::rename(file_type = type)

          reshare_files_df$ext <- NULL
        } else {
          reshare_files_df$file_type <- character(0)
        }
      }
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      repos$repo_error[repos$repo_url %in% reshare_urls] <<- conditionMessage(e)
    })
  }

  ## 4TU.ResearchData ----
  # Djehuty (4TU.ResearchData's platform) implements the same Figshare v2 API
  # as figshare_info()/figshare_file_download() use, so this block is
  # otherwise identical to the Figshare block above -- see archive-4tu.R.
  fourtu_urls <- repos |>
    dplyr::filter(repo_type == "researchdata4tu") |>
    _$repo_url |>
    unique()
  fourtu_files_df <- data.frame(repo_name = character(0))
  if (length(fourtu_urls) > 0) {
    tryCatch({
      .fourtu_info <- suppressMessages(researchdata4tu_info(fourtu_urls))

      if (nrow(.fourtu_info) > 0 && "files" %in% names(.fourtu_info)) {
        file_rows <- lapply(seq_len(nrow(.fourtu_info)), function(i) {
          files_i <- .fourtu_info$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            data.frame(
              repo_url = as.character(.fourtu_info$researchdata4tu_url[[i]]),
              file_name = as.character(f$name %||% NA_character_),
              file_path = as.character(f$name %||% NA_character_),
              file_url = as.character(f$download_url %||% NA_character_),
              file_location = NA_character_,
              file_size = as.numeric(f$size %||% NA_real_)
            )
          })

          dplyr::bind_rows(rows_i)
        })

        fourtu_files_df <- dplyr::bind_rows(file_rows)

        if (nrow(fourtu_files_df) > 0) {
          fourtu_files_df$ext <- tolower(sub("^.*\\.", "", basename(fourtu_files_df$file_name)))
          no_ext <- !is.na(fourtu_files_df$file_name) &
            !grepl("\\.", basename(fourtu_files_df$file_name))
          fourtu_files_df$ext[no_ext] <- NA_character_

          fourtu_files_df <- fourtu_files_df |>
            dplyr::left_join(metacheck::file_types, by = "ext") |>
            dplyr::rename(file_type = type)

          fourtu_files_df$ext <- NULL
        } else {
          fourtu_files_df$file_type <- character(0)
        }
      }
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      repos$repo_error[repos$repo_url %in% fourtu_urls] <<- conditionMessage(e)
    })
  }

  ## Local files ----
  local_files_df <- data.frame(repo_name = character(0))
  if (!is.null(local_path)) {
    local_files_df <- local_files(local_path, recursive = TRUE)
    if (nrow(local_files_df) > 0) {
      local_files_df$file_path <- vapply(seq_len(nrow(local_files_df)), function(i) {
        loc <- local_files_df$file_location[[i]]
        root <- local_files_df$repo_url[[i]]
        if (is.na(loc) || !nzchar(loc) || is.na(root) || !nzchar(root)) {
          return(local_files_df$file_name[[i]])
        }
        loc_norm <- normalizePath(loc, winslash = "/", mustWork = FALSE)
        root_norm <- normalizePath(root, winslash = "/", mustWork = FALSE)
        prefix <- paste0(root_norm, "/")
        rel <- if (startsWith(loc_norm, prefix)) {
          substr(loc_norm, nchar(prefix) + 1L, nchar(loc_norm))
        } else if (identical(loc_norm, root_norm)) {
          basename(loc_norm)
        } else {
          local_files_df$file_name[[i]]
        }
        if (!nzchar(rel)) local_files_df$file_name[[i]] else rel
      }, character(1))
    }
    local_repo <- data.frame(
      paper_id = paper_id(paper)[[1]],
      repo_url = local_path,
      repo_type = "local",
      repo_error = NA_character_
    )
    repos <- dplyr::bind_rows(repos, local_repo)
  }

  ## no repos found ----
  if (nrow(repos) == 0) {
    info <- list(
      traffic_light = "na",
      summary_text = "We found no links to repositories on the Open Science Framework, Github, ResearchBox, PsychArchives, Zenodo, Dataverse, Figshare, Dryad, ReShare, or 4TU.ResearchData.",
      summary_table = data.frame(
        paper_id = paper_id(paper),
        repo_n = 0,
        files_n = NA,
        files_data = NA,
        files_code = NA,
        files_readme = NA,
        files_zip = NA
      )
    )

    return(info)
  }

  ## file numbers and types ----
  all_files <- dplyr::bind_rows(osf_files_df, github_files_df, rb_files_df, pa_files_df, zenodo_files_df, dv_files_df, fs_files_df, dryad_files_df, reshare_files_df, fourtu_files_df, local_files_df)

  # remove duplicate links
  # (can happen when same repo is referenced different ways)
  #
  # Matched on file_url AND file_path (not file_name alone): file_name is only
  # the basename, so two DIFFERENT files that happen to share a name (e.g.
  # study1/analysis.R and study2/analysis.R, both local with no file_url — an
  # ordinary multi-study repository) were previously treated as duplicates and
  # silently dropped, since duplicated(NA) is TRUE for the second NA just like
  # any other repeated value. file_path carries the distinguishing directory,
  # so genuinely identical files (same URL, same path) are still deduped while
  # same-named files in different folders are both kept.
  if (nrow(all_files) > 0) {
    file_path_dedup <- if ("file_path" %in% names(all_files))
      all_files$file_path else all_files$file_name
    dupes <- duplicated(all_files$file_url) &
      duplicated(file_path_dedup)
    all_files <- all_files[!dupes, ]
    # keep repos with explicit errors (e.g. gated/private) in summary/reporting
    in_files <- repos$repo_url %in% all_files$repo_url | !is.na(repos$repo_error)
    repos <- repos[in_files, ]
  }

  if (nrow(all_files) == 0) {
    all_files$repo_url <- character(0)
    all_files$file_name <- character(0)
    all_files$file_path <- character(0)
    all_files$file_url <- character(0)
    all_files$file_location <- character(0)
    all_files$file_size <- numeric(0)
    all_files$file_type <- character(0)
    is_readme <- logical(0)
  } else {
    if (!"file_path" %in% names(all_files)) {
      all_files$file_path <- all_files$file_name
    }
    all_files$file_path[is.na(all_files$file_path) | !nzchar(all_files$file_path)] <- all_files$file_name[is.na(all_files$file_path) | !nzchar(all_files$file_path)]
    is_readme <- grepl(
      "readme|read[_ ]me",
      all_files$file_name,
      ignore.case = TRUE
    )
    all_files$file_type[is_readme] <- "readme"
  }
  all_files$repo_name <- basename(all_files$repo_url)

  ## look inside .zip archives ----
  # An archive is otherwise a dead end in a file listing: "this repository
  # contains 3 archive files" says nothing about what a reader would actually
  # get. A ZIP stores its central directory at the END of the file, so ONE HTTP
  # Range request for the tail recovers every entry's name and uncompressed size
  # without downloading the archive (see zip_peek()) — which is exactly the
  # "list, don't fetch" operation this module is built around.
  #
  # Only .zip can be inspected this way. A .7z/.rar/.tar.gz has no tail index,
  # so its contents stay unknown until data_check downloads it — which is why
  # the report below still recommends .zip over those formats.
  #
  # The archive row is REPLACED by its contents when the peek succeeds, so the
  # listing shows the files a reader gets rather than the container. A failed
  # peek (host ignores ranges, directory not in the tail) leaves the archive row
  # untouched, and data_check opens it after download as before.
  if (isTRUE(peek_zips) && nrow(all_files) > 0) {
    is_zip <- .is_zip(all_files$file_name) &
      !is.na(all_files$file_url) & nzchar(all_files$file_url %||% "")
    if (any(is_zip)) {
      zpb <- pb(sum(is_zip), "Reading zip contents [:bar] :current/:total")
      on.exit(zpb$terminate(), add = TRUE)
      expanded <- list(); consumed <- integer(0)
      for (i in which(is_zip)) {
        peek <- tryCatch(zip_peek(all_files$file_url[i]), error = \(e) NULL)
        zpb$tick()
        if (is.null(peek) || nrow(peek) == 0) next
        rows <- all_files[rep(i, nrow(peek)), , drop = FALSE]
        rows$file_name <- basename(peek$name)
        # The inner path is prefixed with the archive's own name so a reader can
        # see which archive a file came from, and so two archives holding a
        # `data.csv` do not collide in the listing.
        rows$file_path <- file.path(all_files$file_name[i], peek$name)
        rows$file_size <- peek$size
        # No URL of its own: an entry inside an archive has no address a plain
        # download can request, so nothing downstream should try one.
        #
        # A member CAN now be retrieved on its own, by asking the archive's URL
        # for just that member's byte range (.zip_member_fetch()), but that needs
        # the archive URL plus the entry's position within it rather than a URL
        # for the entry, so this column stays NA. Wiring data_check up to fetch
        # single members that way is a separate change.
        rows$file_url  <- NA_character_
        # file_type must be re-derived from the INNER file's extension. Left
        # inherited it would still say "archive" for every entry, so a zip of 40
        # CSVs would report 40 archives and no data files in the summary counts
        # (files_data / files_code / files_zip all key on this column).
        if ("file_type" %in% names(rows)) {
          ext <- tolower(tools::file_ext(rows$file_name))
          ft  <- metacheck::file_types$type[
            match(ext, metacheck::file_types$ext)]
          rows$file_type <- ifelse(is.na(ft), "file", ft)
        }
        expanded[[length(expanded) + 1L]] <- rows
        consumed <- c(consumed, i)
      }
      if (length(consumed) > 0) {
        all_files <- dplyr::bind_rows(all_files[-consumed, , drop = FALSE],
                                      dplyr::bind_rows(expanded))
      }
    }
  }

  ## preliminary classification + study grouping ----
  # A PRELIMINARY, name/path-only pass: repo_check does not download files, so
  # it classifies from names/extensions (data_classify_files()) and groups from
  # paths/repo-splits/the manuscript roster (data_group_llm()) — the same
  # deterministic-first machinery data_check uses, run independently here (not
  # shared as a frozen seed) because data_check operates on a materially
  # different, POST-download file set: it opens the archives .zip peeking cannot
  # reach (.tar.gz, .7z) and reclassifies .txt by content, neither of which is
  # visible from a listing. This preliminary pass powers repo_check's OWN report
  # (warnings, naming check, dropdown below); data_check's later classification
  # is authoritative for placement.
  all_files$data_type <- data_classify_files(all_files$file_name,
                                             all_files$file_path)
  all_files$doc_role  <- .data_doc_role(all_files$file_name)

  # Root readme / ro-crate-metadata.json are collection-level and are never
  # assigned a study (see data_check.R for the identical exclusion rule).
  # path_for_group is coalesced PER ELEMENT (not just when the whole column is
  # NULL, which %||% alone would miss): a mixed multi-source file list (e.g.
  # OSF + local) can have individual rows where file_path is NA even though
  # the column itself exists, and an NA here would make is_root_readme itself
  # NA for that row, tripping `if (any(!is_root_readme))` below.
  path_for_group <- ifelse(
    is.na(all_files$file_path) | !nzchar(all_files$file_path %||% ""),
    all_files$file_name, all_files$file_path)
  # A LICENSE is collection-level exactly like the readme (one licence for the
  # whole deposit, never one per study), so it is excluded from per-study
  # grouping the same way — see the "license" doc_role in .data_doc_role().
  is_root_readme <- !is.na(all_files$doc_role) &
    all_files$doc_role %in% c("readme", "license") &
    is.na(.data_group_from_path(path_for_group)) &
    !grepl("study[-_]?[0-9]|/ex[0-9]|/pilot[0-9]", tolower(path_for_group))
  is_root_readme[is.na(is_root_readme)] <- FALSE

  all_files$group <- rep(NA_character_, nrow(all_files))
  roster_check <- NULL
  group_no_evidence <- FALSE
  if (nrow(all_files) > 0 && any(!is_root_readme)) {
    grp <- data_group_llm(all_files[!is_root_readme, , drop = FALSE],
                          model = model, params = params, paper = paper)
    if (!is.null(grp)) {
      all_files$group[!is_root_readme] <- grp$group
      roster_check <- attr(grp, "roster_check")
      group_no_evidence <- isTRUE(attr(grp, "no_evidence"))
    }
  }

  # attach paper_id so downstream modules (e.g., code_check) know which
  # paper each file belongs to, even when a repo is shared across papers
  all_files <- dplyr::left_join(
    all_files,
    repos[, c("paper_id", "repo_url")],
    by = "repo_url",
    relationship = "many-to-many"
  )

  repos <- dplyr::full_join(repos, all_files, by = c("paper_id", "repo_url"), relationship = "many-to-many") |>
    dplyr::summarise(
      files_n = sum(!is.na(file_name)),
      files_data = sum(file_type %in% "data"),
      files_code = sum(file_type %in% "code"),
      files_readme = sum(file_type %in% "readme"),
      files_zip = sum(file_type %in% "archive"),
      .by = c(paper_id, repo_url, repo_type, repo_error)
    )


  summary_files <- sprintf(
    "We found %d file%s in %d %s.",
    sum(repos$files_n),
    plural(sum(repos$files_n)),
    nrow(repos),
    plural(nrow(repos), "repository", "repositories")
  )

  ## empty repos ----
  repo_no_files <- sum(repos$files_n == 0)
  summary_repo <- NULL
  report_repo <- NULL

  if (repo_no_files > 0) {
    summary_repo <- sprintf(
      "We found %d empty %s.",
      repo_no_files,
      plural(repo_no_files, "repository", "repositories")
    )

    report_repo <- c(
      "Double-check permissions on repositories with no detectable files."
    )
  }

  ## missing READMEs ----
  readme_n <- sum(is_readme)
  repo_no_readme <- sum(repos$files_readme == 0)
  summary_readme <- sprintf(
    "We found %d README file%s and %d %s without READMEs.",
    readme_n,
    plural(readme_n),
    repo_no_readme,
    plural(repo_no_readme, "repository", "repositories")
  )

  if (repo_no_readme > 0) {
    report_readme <- "#### README Files\n\nREADME files are a way to document the contents and structure of a folder, helping users locate the information they need. You can use a README to document changes to a repository, and explain how files are named. Please consider adding a README to each repository or including 'README' in the name of your overview document."
  } else {
    report_readme <- "README files were found in all repositories."
  }

  ## zip files ----
  # What reaches here is the archives that could NOT be listed. A .zip whose
  # central directory was read above has been replaced by its contents, so it is
  # no longer an "archive" row and is not counted or warned about — its files
  # are simply in the listing, which is the point.
  #
  # What remains is a .7z/.rar/.tar.gz (no tail index, so nothing to read
  # without downloading the whole file — and .7z/.rar metacheck cannot read at
  # all), or a .zip whose host ignored the Range request. Both are opaque from a
  # listing, which is exactly what the recommendation below is about.
  zip_n <- sum(repos$files_zip)
  if (zip_n > 0) {
    zip_files <- all_files$file_name[!is.na(all_files$file_type) & all_files$file_type == "archive"]
    nonzip_files <- zip_files[!grepl("[.]zip$", zip_files, ignore.case = TRUE)]
    if (length(nonzip_files) > 0) {
      report_zip <- sprintf(
        "#### Archive Files\n\nThe following files are not ZIP archives: %s. We recommend the `.zip` format for archives: only ZIP stores its file listing in a way that lets a tool inspect the contents without downloading the whole archive, so a `.zip` is more discoverable and re-usable than a `.7z`, `.rar`, or `.tar.gz`.",
        paste(nonzip_files, collapse = ", ")
      )
    } else {
      report_zip <- NULL
    }
    summary_zip <- sprintf(
      "We found %d archive file%s.",
      zip_n,
      plural(zip_n)
    )
  } else {
    summary_zip <- NULL
    report_zip <- NULL
  }

  ## proprietary E-Prime binary files ----
  # E-Prime .edat/.edat2 (and the .emrg/.emrg2 merge files) are proprietary
  # BINARY formats that metacheck cannot read and does not download — only the
  # experiment software can open them. The analysable data is in E-Prime's plain
  # .txt export. Warn when the repo has .edat but is MISSING the matching .txt, so
  # authors know to also upload the readable export.
  edat_files <- all_files$file_name[
    !is.na(all_files$file_name) &
      grepl("[.](edat2?|emrg2?)$", all_files$file_name, ignore.case = TRUE)]
  if (length(edat_files) > 0) {
    txt_stems <- tolower(tools::file_path_sans_ext(
      all_files$file_name[grepl("[.]txt$", all_files$file_name, ignore.case = TRUE)]))
    edat_stems <- tolower(tools::file_path_sans_ext(edat_files))
    missing_txt <- edat_files[!(edat_stems %in% txt_stems)]
    report_edat <- sprintf(
      "#### Proprietary E-Prime Files\n\nThe repository contains %d E-Prime file%s (%s). These are proprietary **binary** formats that only the E-Prime software can open, so metacheck does not download or read them and other researchers cannot reuse them directly.",
      length(edat_files), plural(length(edat_files)),
      paste(utils::head(edat_files, 8), collapse = ", "))
    report_edat <- if (length(missing_txt) > 0) paste0(
      report_edat,
      sprintf(" %d of them ha%s no matching plain-text export. Please also upload the E-Prime **.txt export** (File → Export in E-Prime) for each, so the trial-level data is readable without the proprietary software.",
              length(missing_txt), if (length(missing_txt) == 1) "s" else "ve"))
    else paste0(report_edat,
      " A matching .txt export was found for each, which is the readable form metacheck uses — good. Keep including the .txt export alongside any .edat file.")
    summary_edat <- sprintf("We found %d proprietary E-Prime file%s.",
                            length(edat_files), plural(length(edat_files)))
  } else {
    report_edat <- NULL
    summary_edat <- NULL
  }

  ## restricted-access repositories ----
  # A PsychArchives item flagged restrictedAccess/embargoedAccess hides its
  # protected bitstreams behind institutional (SSO) login, so those files cannot
  # be fetched by a machine. Warn that this limits reuse. repo_error was set to
  # "restricted access" in the PsychArchives block above.
  restricted_repos <- repos$repo_url[
    !is.na(repos$repo_error) & repos$repo_error == "restricted access"]
  if (length(restricted_repos) > 0) {
    report_restricted <- sprintf(
      "#### Restricted-Access Files\n\n%d %s (%s) %s files behind restricted or embargoed access. These files require a login and cannot be downloaded programmatically, so metacheck cannot examine them and other researchers cannot reuse them without requesting access. Consider making the files openly available to improve their reusability.",
      length(restricted_repos),
      plural(length(restricted_repos), "repository", "repositories"),
      paste(restricted_repos, collapse = ", "),
      if (length(restricted_repos) == 1) "contains" else "contain")
    summary_restricted <- sprintf(
      "We found %d %s with restricted-access files.",
      length(restricted_repos),
      plural(length(restricted_repos), "repository", "repositories"))
  } else {
    report_restricted <- NULL
    summary_restricted <- NULL
  }

  ## registrations pointing to a closed source project ----
  # An OSF registration is a locked snapshot; several "Open-Ended Registration"
  # entries hold only a description of what was shared, with the actual files
  # living in the project it was registered from. repo_error was set in the
  # OSF block above when that origin project could not be reached (private,
  # or the request failed): "...(files retrieved from registration: <url>)"
  # when the registration itself still held a working copy of the files (the
  # files came from the REGISTRATION's own storage, not the closed project —
  # nothing was actually lost, just the project's permanent home is
  # inaccessible), or plain "closed registration source" when no copy could
  # be found anywhere, including the registration.
  mirror_rows <- !is.na(repos$repo_error) &
    grepl("^closed registration source \\(files retrieved from registration: ",
         repos$repo_error)
  closed_reg_mirrored <- repos$repo_url[mirror_rows]
  # Registration URL(s) the files actually came from, per closed parent —
  # extracted back out of the repo_error string built in the OSF block above.
  closed_reg_mirrored_source <- sub(
    "^closed registration source \\(files retrieved from registration: (.+)\\)$",
    "\\1", repos$repo_error[mirror_rows])
  closed_reg_unreachable <- repos$repo_url[
    !is.na(repos$repo_error) & repos$repo_error == "closed registration source"]

  report_closed_reg <- NULL
  summary_closed_reg <- NULL
  if (length(closed_reg_unreachable) > 0) {
    report_closed_reg <- c(report_closed_reg, sprintf(
      "#### Registration Points to a Closed Project\n\n%d OSF %s (%s) %s referenced by a registration in this manuscript, but %s closed or inaccessible, and no copy of %s files could be found anywhere else. Some OSF registrations only describe what was shared, while the actual files live in the project they were registered from — if that project is closed and the registration holds no copy, metacheck cannot list or download its files, and other researchers cannot access them either. Consider making the source project openly available, or re-uploading its files directly into the registration.",
      length(closed_reg_unreachable),
      plural(length(closed_reg_unreachable), "project", "projects"),
      paste(closed_reg_unreachable, collapse = ", "),
      if (length(closed_reg_unreachable) == 1) "is" else "are",
      if (length(closed_reg_unreachable) == 1) "it is" else "they are",
      if (length(closed_reg_unreachable) == 1) "its" else "their"))
    summary_closed_reg <- c(summary_closed_reg, sprintf(
      "We found %d registration-linked OSF %s that %s closed with no files reachable anywhere.",
      length(closed_reg_unreachable),
      plural(length(closed_reg_unreachable), "project", "projects"),
      if (length(closed_reg_unreachable) == 1) "is" else "are"))
  }
  if (length(closed_reg_mirrored) > 0) {
    # One line per closed parent, explicitly naming the parent project (closed)
    # and the registration its files were actually retrieved from — so the
    # report never implies the closed project itself was read.
    parent_lines <- sprintf(
      "- **%s** is closed; its files were retrieved from the OSF registration %s.",
      closed_reg_mirrored, closed_reg_mirrored_source)
    report_closed_reg <- c(report_closed_reg, sprintf(
      "#### Registration's Source Project Is Closed\n\n%d OSF source %s %s closed. The files metacheck lists below were not read from %s — they were retrieved from the OSF registration's own copy, which is separate from and unaffected by the project's access setting. Nothing is currently lost, but this is still worth fixing: a closed source project means the files only survive for as long as the registration's copy does, and anyone who follows the project link itself (rather than the registration) will find it inaccessible.\n\n%s\n\nConsider making the source project openly available.",
      length(closed_reg_mirrored),
      plural(length(closed_reg_mirrored), "project", "projects"),
      if (length(closed_reg_mirrored) == 1) "is" else "are",
      if (length(closed_reg_mirrored) == 1) "it" else "them",
      paste(parent_lines, collapse = "\n")))
    summary_closed_reg <- c(summary_closed_reg, sprintf(
      "We found %d registration-linked OSF %s that %s closed; %s files were retrieved from the OSF registration instead.",
      length(closed_reg_mirrored),
      plural(length(closed_reg_mirrored), "project", "projects"),
      if (length(closed_reg_mirrored) == 1) "is" else "are",
      if (length(closed_reg_mirrored) == 1) "its" else "their"))
  }

  ## unclassifiable files ----
  # Files data_classify_files() could not place at all (data_type == "unknown"),
  # each with the same concrete rename suggestion check_file_naming() gives for
  # the "unclassifiable" rule.
  unknown_files <- all_files$file_name[
    !is.na(all_files$data_type) & all_files$data_type == "unknown"]
  n_unknown <- length(unknown_files)
  if (n_unknown > 0) {
    report_unknown <- sprintf(
      "#### Unclassified Files\n\nWe could not classify %d file%s by name or extension: %s. Add a recognisable keyword (`data`, `code`, `materials`, `documentation`, `output`) to the file name, or use a common extension, so both humans and machines can tell what kind of file it is.",
      n_unknown, plural(n_unknown), paste(utils::head(unknown_files, 10), collapse = ", "))
    summary_unknown <- sprintf("We could not classify %d file%s.",
                               n_unknown, plural(n_unknown))
  } else {
    report_unknown <- NULL
    summary_unknown <- NULL
  }

  ## study roster mismatch ----
  # roster_check compares the studies the MANUSCRIPT names against the studies
  # the FILES actually separate into (see .data_group_check_roster()). A
  # mismatch means the repository structure and the paper disagree — worth
  # surfacing rather than silently building a layout that contradicts the text.
  # Only meaningful when the manuscript names at least one study: an empty
  # roster (most single-study papers never say "Study 1" explicitly) has
  # nothing to disagree with, so every file-derived group would trivially show
  # up as "extra" — that is an absence of evidence, not a real mismatch.
  if (!is.null(roster_check) && length(roster_check$roster) > 0 &&
      (length(roster_check$missing) > 0 || length(roster_check$extra) > 0)) {
    parts <- character(0)
    if (length(roster_check$missing) > 0)
      parts <- c(parts, sprintf(
        "the manuscript names %s (%s) with no matching files in the repository",
        plural(length(roster_check$missing), "a study", "studies"),
        paste(roster_check$missing, collapse = ", ")))
    if (length(roster_check$extra) > 0)
      parts <- c(parts, sprintf(
        "the repository separates out %s (%s) not named in the manuscript",
        plural(length(roster_check$extra), "a study", "studies"),
        paste(roster_check$extra, collapse = ", ")))
    report_roster <- sprintf(
      "#### Study/Repository Mismatch\n\nThe studies named in the manuscript do not match how the repository's files are grouped: %s. Check that every study has its own clearly-named folder or file prefix, and that the folder names match how the manuscript refers to each study.",
      paste(parts, collapse = "; "))
    summary_roster <- "The manuscript's studies and the repository's file groups do not match."
  } else {
    report_roster <- NULL
    summary_roster <- NULL
  }

  ## file-naming conventions ----
  naming_issues <- check_file_naming(
    all_files$file_name,
    file_path = all_files$file_path %||% all_files$file_name,
    data_type = all_files$data_type)
  naming_bad <- naming_issues[naming_issues$severity == "bad", , drop = FALSE]
  naming_suggest <- naming_issues[naming_issues$severity == "suggestion", , drop = FALSE]
  n_naming_bad <- nrow(naming_bad)
  n_naming_suggest <- length(unique(naming_suggest$file_name))

  if (nrow(naming_issues) > 0) {
    naming_tbl <- naming_issues
    names(naming_tbl) <- c("File", "Rule", "Severity", "Detail")
    report_naming <- c(
      "#### File Naming",
      sprintf(
        "%s We found %d naming problem%s that %s, and %d file%s with a naming suggestion (not required, but a good habit).",
        if (n_naming_bad > 0)
          "File names should be machine-parseable: no spaces or special characters, and every file should be classifiable by name or extension."
        else
          "File names are broadly machine-parseable.",
        n_naming_bad, plural(n_naming_bad),
        if (n_naming_bad == 1) "should be fixed" else "should be fixed",
        n_naming_suggest, plural(n_naming_suggest)),
      scroll_table(naming_tbl, maxrows = 10)
    )
    summary_naming <- if (n_naming_bad > 0) sprintf(
      "We found %d file naming problem%s to fix.", n_naming_bad, plural(n_naming_bad)
    ) else if (n_naming_suggest > 0) sprintf(
      "We found %d file%s with a naming suggestion.", n_naming_suggest, plural(n_naming_suggest)
    ) else NULL
  } else {
    report_naming <- NULL
    summary_naming <- NULL
  }

  ## classification dropdown ----
  # One row per data_type, listing every file assigned to it (name, resolved
  # study group, path) — a full audit view of how repo_check classified and
  # grouped every file in the repository.
  class_rows <- all_files[!is.na(all_files$data_type), , drop = FALSE]
  if (nrow(class_rows) > 0) {
    by_type <- split(class_rows, class_rows$data_type)
    class_sections <- lapply(names(by_type), function(dt) {
      grp <- by_type[[dt]]
      tbl <- data.frame(
        File  = grp$file_name,
        Group = ifelse(is.na(grp$group), "—", grp$group),
        Path  = grp$file_path %||% grp$file_name
      )
      sprintf("**%s** (%d file%s)\n\n%s", dt, nrow(grp), plural(nrow(grp)),
              scroll_table(tbl, maxrows = 10))
    })
    report_classification <- c(
      "#### File Classification",
      collapse_section(
        unlist(class_sections),
        title = "See how every file was classified and grouped",
        callout = "note")
    )
  } else {
    report_classification <- NULL
  }

  report_tbl <- all_files |>
    dplyr::mutate(file = dplyr::coalesce(link(file_url, file_name), file_name)) |>
    dplyr::select(Repository = repo_url,
                  File = file,
                  Size = file_size,
                  Type = file_type)

  # human-readable sizes. Some repos (e.g. OSF) return no size for a file, so
  # guard against NA/non-positive values: format.object_size() does `if (x <= 0)`
  # and errors on NA. Unknown sizes are shown as "—".
  report_tbl$Size <- vapply(report_tbl$Size, function(x) {
    if (is.na(x) || !is.finite(x) || x < 0) return("—")
    utils:::format.object_size(x, units = "auto", standard = "SI", digits = 1)
  }, character(1))

  # set up summary table of repositories
  repo_tbl <- repos
  repo_tbl$paper_id <- NULL
  repo_tbl$repo_url <- link(repo_tbl$repo_url)
  names(repo_tbl) <- c("Repository", "Platform", "Error",
                       "All Files", "Data Files",
                       "Code Files", "READMEs",
                       "Archives")
  if (all(is.na(repo_tbl$Error))) {
    repo_tbl$Error <- NULL
  }

  report <- c(
    report_repo,
    if(nrow(repo_tbl)) "#### Repositories" else NULL,
    scroll_table(repo_tbl, maxrows = 10),
    if (nrow(report_tbl)) "#### Files" else NULL,
    scroll_table(report_tbl, maxrows = 10),
    report_readme,
    report_zip,
    report_edat,
    report_restricted,
    report_closed_reg,
    report_unknown,
    report_roster,
    report_naming,
    report_classification
  )

  # traffic_light ----
  # A repository where metacheck cannot tell how many studies exist, has files
  # it cannot classify at all, or has real (non-suggestion) naming problems is
  # exactly the kind of ambiguity this traffic light exists to flag, alongside
  # the existing zip/README criteria.
  if (zip_n == 0 && repo_no_readme == 0 && n_unknown == 0 &&
      is.null(report_roster) && n_naming_bad == 0 &&
      length(closed_reg_unreachable) == 0) {
    tl <- "green"
  } else {
    tl <- "yellow"
  }

  # summary_table ----
  summary_table <- repos |>
    dplyr::summarise(
      repo_n = dplyr::n(),
      dplyr::across(files_n:files_zip, sum),
      .by = c(paper_id)
    )
  summary_table$files_unknown <- n_unknown
  summary_table$naming_issues <- n_naming_bad
  summary_table$roster_mismatch <- !is.null(roster_check) &&
    length(roster_check$roster) > 0 &&
    (length(roster_check$missing) > 0 || length(roster_check$extra) > 0)

  # summary_text ----
  summary_text <- c(
    summary_repo,
    summary_files,
    summary_readme,
    summary_zip,
    summary_edat,
    summary_restricted,
    summary_closed_reg,
    summary_unknown,
    summary_roster,
    summary_naming
  ) |>
    paste("\n- ", x = _, collapse = "")

  # Repositories that were found but could not be listed (e.g. a GitHub repo
  # over the size gate, or a private OSF component). Carried out of the module
  # so downstream converters can explain *why* a paper produced no files,
  # instead of reporting "no repository".
  gated_repos <- repos[!is.na(repos$repo_error),
                       c("repo_url", "repo_type", "repo_error"), drop = FALSE]

  # return a list ----
  list(
    table = all_files,
    summary_table = summary_table,
    gated_repos = gated_repos,
    naming_issues = naming_issues,
    roster_check = roster_check,
    group_no_evidence = group_no_evidence,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}
