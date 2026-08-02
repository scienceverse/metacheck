#' Repository Check
#'
#' @description
#' This module retrieves information from repositories.
#'
#' @details
#' The Repository Check module lists files on the OSF, GitHub, ResearchBox, PsychArchives, and Zenodo based on links in the manuscript.
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
#' @param github_gate if TRUE, gate large GitHub repos before recursive listing
#' @param github_max_repo_size_mb gate threshold for GitHub repository size (MB)
#' @param github_max_files gate threshold for GitHub repository file count
#' @param model the LLM model name (see `llm_model_list()`), used only when
#'   `llm_use(TRUE)` for study grouping the deterministic passes cannot place
#' @param params a named list passed to `llm()`, used only when `llm_use(TRUE)`
#'
#' @returns a list
repo_check <- function(paper, local_path = NULL, local_only = FALSE,
                       github_gate = TRUE,
                       github_max_repo_size_mb = 500,
                       github_max_files = 1000,
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
  }

  ## organise repos in a table
  cols <- c("paper_id", "href", "repo_type")
  repos <- dplyr::bind_rows(
    osf_links_found[, cols],
    github_links_found[, cols],
    rb_links_found[, cols],
    pa_links_found[, cols],
    zenodo_links_found[, cols]
  ) |> dplyr::distinct()
  names(repos)[2] <- "repo_url"
  repos$repo_error <- NA_character_

  # get files ----

  ## OSF ----
  osf_urls <- repos |>
    dplyr::filter(repo_type == "osf") |>
    _$repo_url |>
    unique()

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

      # "kind" only in table if there are files
      if ("kind" %in% names(osf_info)) {
        osf_file_list <- osf_info |>
          dplyr::filter(kind == "file", !isFALSE(public))

        osf_files_df <- data.frame(
          repo_url = osf_file_list$repo_name,
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

      # remove e.g., registrations from repos list
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
    }, error = \(e) {
      # TODO: communicate errors to repos table
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
    # (vs. N recursive /contents/ calls) and gates large repos before any file
    # listing happens.
    gh_results <- lapply(github_urls, function(url) {
      tryCatch(
        github_tree_files(
          url,
          max_repo_size_mb = github_max_repo_size_mb,
          max_files = github_max_files,
          gate = github_gate
        ),
        error = \(e) list(gated = TRUE, reason = conditionMessage(e),
                          files = NULL, default_branch = NA_character_))
    })
    names(gh_results) <- github_urls

    for (url in github_urls) {
      r <- gh_results[[url]]
      if (isTRUE(r$gated)) {
        repos$repo_error[repos$repo_url == url] <- r$reason
        warning(sprintf(
          paste0("Repository %s was not listed: %s. ",
                 "Set `github_gate = FALSE` to force full recursive listing."),
          url, r$reason
        ), call. = FALSE)
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
      # TODO: communicate errors to repos table
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
      # TODO: communicate errors to repos table
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
      # TODO: communicate errors to repos table
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
      summary_text = "We found no links to repositories on the Open Science Framework, Github, ResearchBox, PsychArchives, or Zenodo.",
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
  all_files <- dplyr::bind_rows(osf_files_df, github_files_df, rb_files_df, pa_files_df, zenodo_files_df, local_files_df)

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

  ## preliminary classification + study grouping ----
  # A PRELIMINARY, name/path-only pass: repo_check never downloads files, so it
  # can only classify from names/extensions (data_classify_files()) and group
  # from paths/repo-splits/the manuscript roster (data_group_llm()) — the same
  # deterministic-first machinery data_check uses, run independently here (not
  # shared as a frozen seed) because data_check operates on a materially
  # different, POST-download file set: it downloads, expands archives (adding
  # rows that don't exist yet at this point), and reclassifies .txt content,
  # none of which repo_check can see. This preliminary pass exists purely to
  # power repo_check's OWN report (warnings, naming check, dropdown below);
  # data_check's later classification is authoritative for placement.
  all_files$data_type <- data_classify_files(all_files$file_name)
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
  # repo_check() only LISTS archive files here — it never opens one (peeking a
  # .zip via HTTP range request, or downloading any archive, is data_check's
  # job when peek_zips/download is enabled). So this report makes no claim
  # about whether an archive's content was or wasn't examined; it only warns
  # about the one thing repo_check itself can know from the file name alone:
  # non-.zip archive FORMAT. Only .zip stores its file listing in a tail index
  # reachable by an HTTP range request, so only .zip can be inspected without a
  # full download; .7z/.rar/.tar.gz must be downloaded whole, and some formats
  # (.7z/.rar) metacheck cannot read at all.
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
      is.null(report_roster) && n_naming_bad == 0) {
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
