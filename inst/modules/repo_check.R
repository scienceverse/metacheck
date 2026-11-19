#' Repository Check
#'
#' @description
#' This module retrieves information from repositories.
#'
#' @details
#' The Repository Check module lists files on the OSF, GitHub, ResearchBox, DSpace (PsychArchives and other legacy- and DSpace 7-based installations), Zenodo, Dataverse, Figshare, Dryad, ReShare, 4TU.ResearchData, Mendeley Data, DataONE (Arctic Data Center, KNB, and other member nodes), and LDbase based on links in the manuscript.
#'
#' A link to a file-sharing service (Google Drive, Dropbox, OneDrive, Box,
#' WeTransfer, MEGA, iCloud Drive, Baidu Netdisk, SURFdrive; see
#' [file_share_links()]) gets a warning and makes the light yellow: such
#' services are not repositories, as files can be changed or deleted at any
#' time, get no DOI, and the link stops working when the account is closed.
#' The report advises a stable repository such as Zenodo instead. Their files are not
#' read, and `summary_table$file_share_n` counts the links.
#'
#' Deposits in archives for sequence and expression data (GEO, the INSDC
#' archives BioProject, SRA, ENA and DDBJ, and ArrayExpress; see
#' [seq_archive_links()]), mostly named by accession number, are read for
#' whose deposit they are. The authors' own are listed with what they hold
#' ([seq_archive_size()]) and are not downloaded or checked as data and code;
#' `summary_table$archive_own_n` counts them.
#'
#' R packages on CRAN ([cran_links()], [cran_info()]) are the paper's own when
#' a paper author is a package author and the paper presents the package;
#' the paper's own are listed with their version and not checked
#' (`summary_table$cran_own_n`). A Handle link (`hdl.handle.net/...`) that
#' leads to a repository read here is checked as a link to that repository
#' ([handle_links()]).
#'
#' Listing a repository's files never requires an API key, but some hosts
#' answer faster or more completely with one: OSF allows 100 requests/hour
#' unauthenticated versus 10,000/day with a token (see [osf_pat()]), and
#' Dryad enforces a strict per-IP daily quota on repeated listing (see
#' `cache` below). Dryad is a special case beyond rate limits: listing a
#' Dryad dataset's files works without a key, but actually downloading file
#' CONTENTS from Dryad fails with HTTP 401 unless a key is set, even for a
#' fully public dataset (see [dryad_pat()] or [dryad_auth()]). Since this
#' module only lists files and never downloads their contents itself, that
#' limitation applies to `data_check` and `code_check` (which do download
#' content), not here -- see their own documentation.
#'
#' When a linked OSF page is a registration, its `registered_from` project (the
#' one it was registered from, which the manuscript itself may never link
#' directly) is also checked: if that project is public its files are listed
#' under its own URL, and if it is closed or otherwise inaccessible the report
#' flags it explicitly, since some registrations only describe their content
#' rather than mirroring it.
#'
#' Whose deposit is each link? The `ownership` table gives, for every
#' repository linked in the text and (with `references = TRUE`) every deposit
#' cited in the reference list, one of six labels: `own_this` (made by the
#' authors for this paper), `own_earlier_data` or `own_earlier_code` (the
#' authors' own, from earlier work), `others_data` or `others_code` (made by
#' other people, reused here), or `not_data` (a publication such as a
#' preprint, not a deposit), with `ownership_group` ("own", "others",
#' "not_data") and the reason. It is decided without a language model, from
#' the sentence or reference that cites the link, the paper's authors (names,
#' ORCID iDs, affiliations), and what the deposit's own record and files say
#' (who made it, what it names, when, whether it is a software package; for
#' an OSF project cited only in the reference list, its record alone). On a
#' round of labelled links the rules had not been written from, the three
#' groups were right for 98% of links in the main text (93 of 95) and 90% in
#' reference lists (74 of 82); the labels were made by Claude agents from
#' each link's evidence, and disputed ones checked against the deposits.
#' Telling this paper's own deposit from the authors' earlier one is less
#' certain.
#'
#' Only the deposits that are this paper's own are checked. A deposit linked
#' in the text that holds other people's work (`others_data`, `others_code`)
#' or the authors' earlier work (`own_earlier_data`, `own_earlier_code`) is
#' listed in the report, with the reason, but its files are left out of
#' `table` and kept in `table_not_checked`: they count toward nothing here
#' (files that do not belong in a repository, naming, READMEs, studies, the
#' traffic light), and code_check, data_check and reproducibility_check, which
#' read `table`, do not check them either. reproducibility_check still puts the
#' files of theirs that the paper's code names in the folder the code runs in,
#' as a re-analysis needs the data it re-analyses. When every linked deposit
#' is set aside, the traffic light is "na". A deposit that is the paper's own
#' is counted as such when its README, title or description names the paper
#' (its title or DOI).
#'
#' If you want to extend the package to be able to download files from additional data repositories reach out to the Metacheck development team.
#'
#' <validation>This module has not been validated. All checks in the repo_check module have unknown error rates. Carefully evaluate the output of this module. You can help improve this module by reporting an issue on GitHub.</validation>
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
#'   inspected this way; other archive formats have no tail index. A plain
#'   (uncompressed) `.tar` of known size up to 50 MB is listed too, by
#'   downloading it once; its files are later taken from that copy.
#' @param osf_license if TRUE, make one extra lightweight API request
#'   (`?embed=license`, not the full four-call project-metadata lookup) per
#'   OSF project, to include OSF's licence in `repo_metadata`. A cited
#'   registration is looked up as the project it was registered from, which
#'   is where its files are listed from; when that project is closed or has
#'   no licence, the registration's own licence is used.
#'   FALSE (the default) skips this, since it is one additional request per
#'   OSF project on top of what this module otherwise makes -- meaningful
#'   added latency across a large corpus. Every other supported repository
#'   platform (Dryad, Zenodo, Dataverse, Figshare, ReShare, GitHub, GitLab,
#'   PsychArchives) already fetches its own licence as part of a request
#'   this module makes anyway, so `repo_metadata` has their licence
#'   populated regardless of this parameter. ResearchBox's licence costs one
#'   extra request per downloaded box, which is always made.
#' @param cache if TRUE, reuse a previously cached Dryad/Zenodo listing (see
#'   [repo_info_cache_clear()]) instead of re-querying the host's API for a
#'   repository already looked up. FALSE (the default) always queries fresh.
#'   This is a SEPARATE cache from the downloaded-file-bytes cache
#'   ([repo_cache_dir()]) -- listing a repository and downloading its files
#'   are different costs. Worth enabling for a corpus build likely to be
#'   interrupted and restarted: without it, a host with a strict per-IP
#'   quota (Dryad, confirmed live 2026-09-08) has its quota re-spent on every
#'   restart re-listing repositories nothing new needs fetching for, rather
#'   than on the actual (usually much smaller) download workload.
#' @param model the LLM model name (see `llm_model_list()`), used only when
#'   `llm_use(TRUE)` for study grouping the deterministic passes cannot place
#' @param params a named list passed to `llm()`, used only when `llm_use(TRUE)`
#' @param skip_on_api_limit if TRUE, a confirmed exhausted rate-limit bucket
#'   hit while peeking a zip's contents (`peek_zips = TRUE`) skips that
#'   archive instead of waiting out the host's own reset. Default FALSE
#'   (always wait for a confirmed reset) -- see [download_repo_files()]'s
#'   parameter of the same name, which this matches for the listing-only
#'   requests `zip_peek()` makes here.
#' @param references if TRUE (default), also look up the data and code
#'   deposits cited only in the reference list (one lookup each, as for a
#'   link in the text; a preprint server's record needs none), to tell
#'   whose deposit each one is. A deposit that is the authors' own for this
#'   paper (`own_this`) is then checked like a link in the text: its files
#'   are added to `table`, so data_check, code_check and
#'   reproducibility_check check them too, and `summary_table$ref_own` counts
#'   them. The files of the other deposits cited there are never added.
#'   FALSE leaves them out.
#' @param osf_max_requests the largest number of requests to the OSF for
#'   listing the components and folders of one OSF project (default 50; `Inf`
#'   for no limit). The OSF needs one request per folder, and a project with
#'   many components or folders, or one the OSF answers slowly for, can take
#'   an hour to list in full. A project that reaches the limit is listed in
#'   part: it is named in `partial_repos` and in the report, and its row in
#'   `ownership` has `listing_cut` TRUE. With `cache = TRUE`, the next run
#'   goes on where the listing stopped (see [osf_info()]). A project part of
#'   whose listing the OSF refused, also when asked again, is named there too
#'   (`reason` "refused by the OSF" in `partial_repos`, `listing_short` TRUE
#'   in `ownership`).
#'
#' @returns a list
repo_check <- function(paper, local_path = NULL, local_only = FALSE,
                       peek_zips = TRUE,
                       osf_license = FALSE,
                       cache = FALSE,
                       osf_max_requests = 50,
                       model = llm_model(),
                       params = list(),
                       skip_on_api_limit = FALSE,
                       references = TRUE) {
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
  # Set only by the lookup of the deposits cited in the reference list (see
  # own_references() below): an OSF project is read as its record alone,
  # not its components, file lists and folders, and no README is read. Whose
  # deposit a reference is did not depend on either (#634; 2026-11-13, on
  # the 148 reference-list deposits of rounds 7-8: without the OSF file
  # facts of 90 OSF deposits one class changed, to the right one, and
  # without the READMEs of 20 GitHub repositories none), and they were more
  # than half of that lookup's requests (58 of 102 on 50 papers).
  records_only <- isTRUE(getOption("metacheck.repo_check.records_only", FALSE))

  ## get links ----
  if (isTRUE(local_only)) {
    empty_links        <- dplyr::tibble(paper_id = character(), href = character(), repo_type = character())
    osf_links_found    <- empty_links
    github_links_found <- empty_links
    gitlab_links_found <- empty_links
    rb_links_found     <- empty_links
    pa_links_found     <- empty_links
    zenodo_links_found <- empty_links
    dataverse_links_found <- empty_links
    figshare_links_found <- empty_links
    dryad_links_found <- empty_links
    reshare_links_found <- empty_links
    researchdata4tu_links_found <- empty_links
    dspace7_links_found <- empty_links
    mendeley_links_found <- empty_links
    dataone_links_found <- empty_links
    ldbase_links_found <- empty_links
    ckan_links_found <- empty_links
    seq_links_found <- empty_links
    handle_links_found <- empty_links
    cran_links_found <- empty_links
    icpsr_links_found <- empty_links
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
    gitlab_links_found <- gitlab_links(paper)
    gitlab_links_found$repo_type <- "gitlab"
    rb_links_found     <- rbox_links(paper)
    rb_links_found$repo_type     <- "researchbox"
    pa_links_found     <- dspace_links(paper)
    pa_links_found$repo_type     <- "dspace"
    dspace7_links_found <- dspace7_links(paper)
    dspace7_links_found$repo_type <- "dspace7"
    zenodo_links_found <- invenio_links(paper)  # Zenodo and the other Invenio installations (#636)
    zenodo_links_found$repo_type <- "zenodo"
    dataverse_links_found <- dataverse_links(paper)
    dataverse_links_found$repo_type <- "dataverse"
    figshare_links_found <- figshare_links(paper)
    figshare_links_found$repo_type <- "figshare"
    dryad_links_found <- dryad_links(paper)
    dryad_links_found$repo_type <- "dryad"
    reshare_links_found <- eprints_links(paper)  # ReShare and the other EPrints repositories (#637)
    reshare_links_found$repo_type <- "reshare"
    researchdata4tu_links_found <- researchdata4tu_links(paper)
    researchdata4tu_links_found$repo_type <- "researchdata4tu"
    mendeley_links_found <- mendeley_links(paper)
    mendeley_links_found$repo_type <- "mendeley"
    dataone_links_found <- dataone_links(paper)
    dataone_links_found$repo_type <- "dataone"
    ldbase_links_found <- ldbase_links(paper)
    ldbase_links_found$repo_type <- "ldbase"
    ckan_links_found <- ckan_links(paper)  # CKAN data portals (#637)
    ckan_links_found$repo_type <- "ckan"
    # GEO, the INSDC archives (BioProject, SRA, ENA, DDBJ) and ArrayExpress,
    # mostly named by accession number (#660); each row names its archive
    seq_links_found <- tryCatch(seq_archive_links(paper), error = \(e) empty_links)
    # a deposit cited by its Handle (hdl.handle.net/...), as the address the
    # Handle leads to, when that is a repository read here (#660, #466)
    handle_links_found <- tryCatch(handle_links(paper), error = \(e) empty_links)
    # R packages on CRAN, the paper's own or ones it uses (#478, #660)
    cran_links_found <- tryCatch(cran_links(paper), error = \(e) empty_links)
    # replication packages at ICPSR and openICPSR, with the AEA Data and Code
    # Repository (economics journals)
    icpsr_links_found <- tryCatch(icpsr_links(paper), error = \(e) empty_links)
  }

  ## links to file-sharing services (#660) ----
  # Google Drive, Dropbox, OneDrive and the like are not repositories: every
  # such link gets a warning and makes the light yellow. Their files are not
  # read (see file_share_links(), R/file-share.R).
  file_share <- if (isTRUE(local_only)) NULL else tryCatch(file_share_links(paper), error = \(e) NULL)
  if (!is.data.frame(file_share) || !nrow(file_share))
    file_share <- data.frame(paper_id = character(0), href = character(0), service = character(0))
  if (!"paper_id" %in% names(file_share)) file_share$paper_id <- rep(paper_id(paper)[1] %||% NA_character_, nrow(file_share))
  n_file_share <- nrow(file_share)
  report_file_share <- NULL
  summary_file_share <- NULL
  if (n_file_share > 0) {
    fs_list <- paste(sprintf("%s: %s", file_share$service, file_share$href), collapse = "; ")
    report_file_share <- c(
      "#### File-sharing services",
      sprintf(paste(
        "The paper links to %s (%s). Such services are not repositories: files can be changed or",
        "deleted at any time, they get no DOI, and the link stops working when the account is closed.",
        "Share data and code in a stable repository such as Zenodo."),
        plural(n_file_share, "a file-sharing service", "file-sharing services"), fs_list))
    summary_file_share <- sprintf(
      "The paper links to %s (%s), which %s; share data and code in a stable repository such as Zenodo.",
      plural(n_file_share, "a file-sharing service", sprintf("%d file-sharing services", n_file_share)),
      paste(unique(file_share$service), collapse = ", "),
      plural(n_file_share, "is not a repository", "are not repositories"))
  }
  file_share_n_of <- function(ids) vapply(ids, \(id) sum(file_share$paper_id %in% id), integer(1), USE.NAMES = FALSE)

  ## organise repos in a table
  cols <- c("paper_id", "href", "repo_type")
  repos <- dplyr::bind_rows(
    osf_links_found[, cols],
    github_links_found[, cols],
    gitlab_links_found[, cols],
    rb_links_found[, cols],
    pa_links_found[, cols],
    zenodo_links_found[, cols],
    dataverse_links_found[, cols],
    figshare_links_found[, cols],
    dryad_links_found[, cols],
    reshare_links_found[, cols],
    researchdata4tu_links_found[, cols],
    dspace7_links_found[, cols],
    mendeley_links_found[, cols],
    dataone_links_found[, cols],
    ldbase_links_found[, cols],
    ckan_links_found[, cols],
    seq_links_found[, cols],
    handle_links_found[, cols],
    cran_links_found[, cols],
    icpsr_links_found[, cols]
  ) |> dplyr::distinct()
  names(repos)[2] <- "repo_url"
  # The punctuation that ends a sentence is not part of an address: taken along
  # ("https://doi.org/10.5063/f1mc8x92."), it made one repository into two, each
  # listing the same files, or one that could not be found.
  repos$repo_url <- sub("[.,;:]+$", "", repos$repo_url)
  repos <- dplyr::distinct(repos)
  # Damage from reading the PDF: a link in a security wrapper, two links run
  # together, a home page alone, a link cut inside a name beside its longer
  # form (.repo_links_tidy(), R/repo-link-versions.R, #634).
  repos <- .repo_links_tidy(repos, paper)
  # A link that a space split, with the rest after it in the text, is
  # replaced by the joined form when only that form exists
  # (.repo_links_joined(), R/repo-link-versions.R).
  if (nrow(repos) > 0) {
    pb$tick(0, list(what = "Checking links that may be split"))
    repos <- .repo_links_joined(repos, paper)
  }
  repos$repo_error <- NA_character_
  # the links as the paper gives them, for telling whose deposit each is
  # (#634) after the lookups below have changed `repos`
  own_links <- unique(repos[, c("paper_id", "repo_url", "repo_type")])
  # Whose deposit each link is (#634; R/repo-ownership.R): the links in the
  # text, with what this check learns about each, and -- when `references`
  # is TRUE -- the deposits cited only in the reference list, looked up with
  # this same check (their files are not added to `table`).
  own_references <- function() {
    ids <- unique(paper_id(paper))
    dplyr::bind_rows(lapply(ids, function(id) {
      p <- if (.is_paper_list(paper)) paper[[id]] else paper
      refl <- tryCatch(.own_reference_links(p, exclude = own_links$repo_url[own_links$paper_id %in% id]),
                       error = \(e) NULL)
      if (is.null(refl) || !nrow(refl)) return(NULL)
      pre <- .own_preprint_link(refl$repo_url, refl$ref_text)
      inner <- NULL
      x <- data.frame(paper_id = id, repo_url = refl$repo_url, repo_type = refl$repo_type)
      if (any(!pre)) {
        pb$tick(0, list(what = sprintf("Looking up %d deposit%s cited in the reference list",
                                         sum(!pre), plural(sum(!pre)))))
        # records only: no OSF file listings or READMEs (see `records_only`)
        old_opt <- options(metacheck.repo_check.records_only = TRUE)
        inner <- tryCatch(
          repo_check(.own_reference_paper(id, refl$repo_url[!pre]), peek_zips = FALSE,
                     osf_license = FALSE, cache = cache, references = FALSE,
                     skip_on_api_limit = skip_on_api_limit),
          error = \(e) NULL, finally = options(old_opt))
        facts <- inner$ownership
        if (is.data.frame(facts) && nrow(facts)) {
          k <- match(.own_key(refl$repo_url), .own_key(facts$repo_url))
          k[pre] <- NA
          for (cc in setdiff(names(facts), c("paper_id", "repo_url", "repo_type", "source", "context",
                                              "sections", "cites_others", "cites_own", "ownership",
                                              "ownership_group", "ownership_reason")))
            x[[cc]] <- facts[[cc]][k]
          # an OSF project read as its record alone: its files are unknown,
          # not absent
          osf_k <- x$repo_type %in% "osf"
          for (cc in intersect(c("n_files", "listed", "has_data", "has_code", "pkg_layout", "file_paper_words"),
                               names(x)))
            x[[cc]][osf_k] <- NA
          im <- inner$repo_metadata
          if (is.data.frame(im) && nrow(im) && "readme" %in% names(im))
            x$readme <- im$readme[match(.own_key(refl$repo_url), .own_key(im$repo_url))]
        }
      }
      x$source <- "reference"
      for (cc in c("bib_id", "ref_text", "ref_authors", "ref_year", "ref_title", "citing", "citing_sections"))
        x[[cc]] <- refl[[cc]]
      # a deposit the inner check ignored as a publication is not a deposit
      inner_pub <- if (is.data.frame(inner$ownership))
        inner$ownership$repo_url[inner$ownership$ownership_reason %in% .own_pub_reason] else character(0)
      .own_mark_publications(.own_classify(paper, x), inner_pub)
    }))
  }
  own_ownership <- function(meta, files, osf_desc) {
    x <- .own_facts(paper, own_links, meta, files, osf_desc)
    x$source <- rep("text", nrow(x))
    if (nrow(x)) {
      ctx <- dplyr::bind_rows(lapply(unique(own_links$paper_id), function(id) {
        p <- if (.is_paper_list(paper)) paper[[id]] else paper
        cbind(paper_id = id, .own_text_context(p, unique(own_links$repo_url[own_links$paper_id %in% id])))
      }))
      x <- dplyr::left_join(x, ctx[!duplicated(ctx[c("paper_id", "repo_url")]), , drop = FALSE],
                            by = c("paper_id", "repo_url"))
    }
    # a link repo_check ignored as a publication (see "Publications") is not
    # a deposit
    out <- .own_mark_publications(.own_classify(paper, x), repo_publications)
    # the deposits cited in the reference list, looked up once below
    # ("deposits cited in the reference list"); a table without rows has
    # empty logical columns, which cannot be joined to the reference rows'
    # text columns
    if (is.data.frame(ref_rows) && nrow(ref_rows))
      out <- if (nrow(out)) dplyr::bind_rows(out, ref_rows) else ref_rows
    first <- c("paper_id", "source", "repo_url", "repo_type", "ownership", "ownership_group", "ownership_reason")
    out <- out[, c(intersect(first, names(out)), setdiff(names(out), c(first, "readme"))), drop = FALSE]
    rownames(out) <- NULL
    out
  }
  ## deposits cited in the reference list (#634, #644) ----
  # A deposit the paper cites only in its reference list ("Data are available
  # from Dryad [75]", with the address in reference 75) and that is the
  # authors' own for this paper (`ownership` "own_this": its record names the
  # paper, the citing sentence is the data statement, ...) is checked like a
  # link in the text: its files are listed, and code_check, data_check and
  # reproducibility_check, which read `table`, check them. Before, such
  # deposits were only classified, and a paper whose data and code were cited
  # this way had "no links to repositories" (#644: two Proc. R. Soc. B papers,
  # where the journal's style puts the data and code in the references).
  # Deposits of others, the authors' earlier deposits and publications are
  # listed in Whose Deposits only, as before.
  ref_rows <- NULL
  ref_checked <- data.frame(paper_id = character(0), repo_url = character(0))
  if (isTRUE(references) && !isTRUE(local_only)) {
    ref_rows <- tryCatch(own_references(), error = \(e) NULL)
    if (is.data.frame(ref_rows) && nrow(ref_rows)) {
      add <- ref_rows[ref_rows$ownership %in% "own_this", c("paper_id", "repo_url", "repo_type"), drop = FALSE]
      add <- add[!paste(add$paper_id, .own_key(add$repo_url)) %in% paste(repos$paper_id, .own_key(repos$repo_url)), ,
                 drop = FALSE]
      add <- add[!duplicated(paste(add$paper_id, .own_key(add$repo_url))), , drop = FALSE]
      if (nrow(add)) {
        add$repo_error <- NA_character_
        repos <- dplyr::bind_rows(repos, add)
        ref_checked <- add[, c("paper_id", "repo_url")]
      }
    }
  }

  # each DSpace and EPrints record's own type, by repo_url, from the requests
  # below: a publication there is not a deposit (#637; see "Publications")
  repo_record_type <- character(0)
  # the links ignored as publications, for the ownership step (#634)
  repo_publications <- character(0)

  # A Figshare private "share link" (figshare.com/s/<hash>) is a real,
  # detected Figshare mention that cannot be resolved any further -- the
  # hash is opaque and Figshare's own resolution page is behind an AWS WAF
  # bot challenge, with no documented plain-HTTP API alternative (see
  # figshare_links()'s own comment). Flagged via repo_error (the same field
  # every other unreachable-repo case in this module uses, e.g. a private
  # OSF project) rather than left silently indistinguishable from "no
  # Figshare link found at all" or from a merely malformed/broken url.
  if ("figshare_unsupported" %in% names(figshare_links_found) &&
      any(figshare_links_found$figshare_unsupported %in% TRUE)) {
    unsupported_urls <- figshare_links_found$href[figshare_links_found$figshare_unsupported %in% TRUE]
    repos$repo_error[repos$repo_url %in% unsupported_urls] <-
      "private share link (figshare.com/s/...) cannot be resolved without a browser"
  }

  # get files ----

  # One platform's metadata rows (repo_url, doi, license) from its *_info()
  # table. .col_chr(), not a bare as.character(): when every link of a
  # platform fails, the table has no doi/license columns, as.character(NULL)
  # is character(0), and data.frame() then failed with "arguments imply
  # differing number of rows: n, 0" -- which the block's tryCatch reported for
  # every link in place of each link's real reason (#476; the Zenodo block
  # below explains the first case found).
  # The deposit's own description is kept too (title, creators and their
  # ORCID iDs, date, description, keywords): the *_info() functions fetch it
  # in the request that lists the files, and it tells the paper's own
  # deposit from one it reuses (#634). A platform whose table lacks a field
  # gets NA there.
  meta_rows <- function(tbl, url_col) data.frame(
    repo_url = .col_chr(tbl, url_col),
    doi = .col_chr(tbl, "doi"),
    license = .col_chr(tbl, "license"),
    title = .meta_col(tbl, c("title", "name"), \(v) .meta_text(v, 300)),
    creators = .meta_col(tbl, c("creators", "authors"), .meta_people),
    creator_orcids = .meta_col(tbl, c("orcids", "creators", "authors"), .meta_orcids),
    published = .meta_col(tbl, c("publication_date", "date_created"),
                          \(v) substr(as.character(v), 1, 10)),
    description = .meta_col(tbl, "description", .meta_text),
    keywords = .meta_col(tbl, c("keywords", "tags"), .meta_people),
    # the record's type ("dataset", "software", "preprint") and the works it
    # names ("isSupplementTo:<the article's DOI>"), from the same answer
    resource_type = .meta_col(tbl, "resource_type", \(v) as.character(v)),
    resource_subtype = .meta_col(tbl, "resource_subtype", \(v) as.character(v)),
    related = .meta_col(tbl, "related", \(v) as.character(v)),
    # the institute that submitted it (sequence archives, #660)
    organisation = .meta_col(tbl, c("organisation", "center_name"), \(v) .meta_text(v, 300))
  )
  # `repo_error` with each link's own reason from its *_info() table
  # ("unfound", "parse_error", ...) filled in, so a link that could not be
  # looked up is reported in gated_repos rather than dropped without a word
  # (#476). A reason already recorded is kept.
  link_errors <- function(repo_error, repo_url, tbl, url_col) {
    err <- .col_chr(tbl, "error")
    urls <- .col_chr(tbl, url_col)[!is.na(err)]
    err <- err[!is.na(err)]
    hit <- match(repo_url, urls)
    fill <- !is.na(hit) & is.na(repo_error)
    repo_error[fill] <- err[hit[fill]]
    repo_error
  }

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
  # An empty paper or paper list has no id at all, and [[1]] stopped with
  # "subscript out of bounds" (U79), so this is NA then.
  first_paper_id <- function() {
    ids <- paper_id(paper)
    if (length(ids) > 0) ids[[1]] else NA_character_
  }
  osf_paper_ids <- repos$paper_id[repos$repo_type == "osf"]
  osf_paper_id <- if (length(osf_paper_ids) > 0) osf_paper_ids[[1]] else
    first_paper_id()

  osf_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                            license = character(0))
  # registration URL -> the project it was registered from; filled below
  reg_url_to_parent <- character(0)

  osf_files_df <- data.frame(repo_name = character(0))
  # OSF projects whose listing stopped at `osf_max_requests` (see below)
  osf_cut_urls <- character(0)
  # OSF projects part of whose listing the OSF refused, also when asked
  # again (`listing_short`, see osf_info(); #681)
  osf_short_urls <- character(0)
  # The links whose listing holds whole storages: the linked item is a
  # project, component or registration (not a single file or folder), listed
  # with its contents and in full. Only their files count towards a storage's
  # size and number of files, which decide whether its archive is fetched
  # (.osf_storage_totals(), #679).
  osf_full_listings <- function(info, cut) {
    if (records_only || !all(c("osf_url", "osf_type", "repo_name") %in% names(info))) return(character(0))
    top <- !is.na(info$osf_url) & info$osf_url == info$repo_name &
      info$osf_type %in% c("nodes", "registrations")
    setdiff(unique(info$repo_name[top]), cut)
  }
  # private projects read through a view-only link in the paper (see below)
  osf_view_only <- data.frame(paper_id = character(0), repo_url = character(0),
                              project = character(0))
  if (length(osf_urls) > 0) {
    tryCatch({
      suppressWarnings({
        osf_info <- lapply(osf_urls, \(x) {
          osf_files <- osf_info(x, recursive = !records_only, pb = pb, cache = cache,
                                max_requests = osf_max_requests)
          osf_files$repo_name <- x
          osf_files
        }) |> dplyr::bind_rows()
      })
      if ("listing_cut" %in% names(osf_info))
        osf_cut_urls <- unique(osf_info$repo_name[osf_info$listing_cut %in% TRUE])
      if ("listing_short" %in% names(osf_info))
        osf_short_urls <- unique(osf_info$repo_name[osf_info$listing_short %in% TRUE])
      osf_full_urls <- osf_full_listings(osf_info, c(osf_cut_urls, osf_short_urls))

      # A registration is never itself the place files "live": either it
      # mirrors files into its own storage (in which case those files really
      # belong to the project it was registered from — see below), or it only
      # describes what was shared and the real files live in that project.
      # `parent` is `registered_from`, captured by .osf_reg_data(); a
      # registration without one (should not normally happen) is left keyed
      # to its own URL below so its files are never silently dropped.
      reg_url_to_parent <- character(0)
      # Opened with a view-only key, a registration does not say which project
      # it was registered from (the OSF hides it, as an anonymous key hides
      # who made the page). A public registration says it without the key, so
      # it is asked once more without it (#648: q7pnw_v3's
      # "osf.io/ujdep/?view_only=..." came from the closed project ke24d,
      # which was reported only while the link was also found without its
      # key).
      if ("parent" %in% names(osf_info)) {
        keyed_reg <- which(osf_info$osf_type %in% "registrations" & is.na(osf_info$parent) &
                             grepl("view_only=", osf_info$osf_id %||% ""))
        for (id in unique(sub("[?/].*$", "", osf_info$osf_id[keyed_reg]))) {
          plain <- tryCatch(suppressWarnings(
            osf_info(sprintf("https://osf.io/%s", id), recursive = FALSE, pb = pb, cache = cache)),
            error = function(e) NULL)
          from <- if (is.data.frame(plain) && "parent" %in% names(plain))
            stats::na.omit(plain$parent)[1] else NA_character_
          if (!is.na(from)) {
            osf_info$parent[keyed_reg[sub("[?/].*$", "", osf_info$osf_id[keyed_reg]) == id]] <- from
          }
        }
      }
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
        # per row: !isFALSE(public) tested the whole column, so a private file
        # was left out only when it was the listing's one row (U123)
        osf_file_list <- osf_info |>
          dplyr::filter(kind == "file", !(public %in% FALSE))

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
          # coverable by the node's ?zip= endpoint. NA when osf_info did not
          # return the column, which download_repo_files() falls back on gracefully.
          # (.col_chr(), not `%||% NA`: a listing without files and without
          # the column gave one NA for no rows, and data.frame() stopped with
          # "differing number of rows: 0, 1")
          provider = .col_chr(osf_file_list, "provider"),
          # the component the file is stored in: its storage is downloaded as
          # one zip per component (#675)
          osf_node = .col_chr(osf_file_list, "project"),
          # whether the file comes from a listing of whole storages (#679);
          # dropped after .osf_storage_totals()
          storage_listed = osf_file_list$repo_name %in% osf_full_urls
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
      #
      # A link the OSF could not look up ("unfound", "error", "too many
      # requests") is kept, with its reason, instead of being removed with the
      # registrations (#501): during an OSF outage (HTTP 503) every OSF link
      # was dropped this way, so the paper read as linking no repository at
      # all, with nothing in gated_repos. Now it is a repository that could
      # not be checked, like a failed link on any other platform (#476).
      osf_failed_types <- c("unfound", "error", "too many requests")
      # A link whose id is not a valid OSF id gets no osf_type: with only
      # such links the column was missing and filter(osf_type ...) below
      # found the exported osf_type() function, so dplyr's error text became
      # the reason; beside valid links it was removed without a word (U122).
      if (!"osf_type" %in% names(osf_info))
        osf_info$osf_type <- rep(NA_character_, nrow(osf_info))
      invalid_osf <- osf_info$osf_url[is.na(osf_info$osf_type) &
                                        osf_info$osf_url %in% osf_urls]
      fill <- repos$repo_url %in% invalid_osf & is.na(repos$repo_error)
      repos$repo_error[fill] <- "not a valid OSF link"
      osf_failed <- osf_info[osf_info$osf_type %in% osf_failed_types &
                               osf_info$osf_url %in% osf_urls, , drop = FALSE]
      if (nrow(osf_failed) > 0) {
        why <- .col_chr(osf_failed, "error")
        why[is.na(why)] <- "the OSF could not look this link up"
        hit <- match(repos$repo_url, osf_failed$osf_url)
        fill <- !is.na(hit) & is.na(repos$repo_error)
        repos$repo_error[fill] <- why[hit[fill]]
      }
      osf_to_remove <- osf_info |>
        dplyr::filter(!is.na(osf_type),
                      !osf_type %in% c("nodes", "files", "private", osf_failed_types)) |>
        _$osf_url
      repos <- repos[!repos$repo_url %in% osf_to_remove, ]

      # note private repos
      private_repos <- osf_info |>
        dplyr::filter(osf_type %in% "private") |>
        _$osf_url
      if (length(private_repos)) {
        repos$repo_error[repos$repo_url %in% private_repos] <- "private"
      }

      # A private project read through a view-only link in the paper: the OSF
      # lists its files through that link, and says the project is not public
      # (public = FALSE on its node). The paper's plain link to the same
      # project is then not a repository that could not be checked, and is
      # left out; the project is named as private instead, as the authors can
      # now make it public (8dn4g_v1 linked osf.io/j9sdn both ways: one copy
      # was read, the other reported as "could not check ... (private)").
      # The same holds for any page the key opens, also a registration (a
      # preregistration shared with a view-only link): the plain link the
      # paper also gives is not a repository that could not be checked
      # (#648: osf.io/kpngh and osf.io/4vtjq in q7pnw_v3 were "private").
      if ("osf_id" %in% names(osf_info)) {
        key_ok <- grepl("view_only=", osf_info$osf_id) & !is.na(osf_info$osf_type) &
          !osf_info$osf_type %in% c("private", osf_failed_types)
        opened <- sub("[?/].*$", "", osf_info$osf_id[key_ok])
        plain <- osf_info$osf_url[osf_info$osf_type %in% "private" &
                                    sub("[?/].*$", "", osf_info$osf_id) %in% opened]
        repos <- repos[!repos$repo_url %in% plain, , drop = FALSE]
      }
      if (all(c("osf_id", "public") %in% names(osf_info))) {
        vo <- osf_info[osf_info$osf_type %in% "nodes" & osf_info$public %in% FALSE &
                         grepl("view_only=", osf_info$osf_id), , drop = FALSE]
        vo <- vo[!duplicated(vo$repo_name) & vo$repo_name %in% repos$repo_url, , drop = FALSE]
        if (nrow(vo) > 0) {
          vo_id <- sub("[?/].*$", "", vo$osf_id)
          plain <- osf_info$osf_url[osf_info$osf_type %in% "private" &
                                      sub("[?/].*$", "", osf_info$osf_id) %in% vo_id]
          repos <- repos[!repos$repo_url %in% plain, , drop = FALSE]
          osf_view_only <- data.frame(
            paper_id = repos$paper_id[match(vo$repo_name, repos$repo_url)],
            repo_url = vo$repo_name,
            project = sprintf("https://osf.io/%s", vo_id))
        }
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
            pf <- osf_info(x, recursive = !records_only, pb = pb, cache = cache,
                           max_requests = osf_max_requests)
            pf$repo_name <- x
            pf
          }) |> dplyr::bind_rows()
        )
        if ("listing_cut" %in% names(parent_info))
          osf_cut_urls <- unique(c(osf_cut_urls,
                                   parent_info$repo_name[parent_info$listing_cut %in% TRUE]))
        if ("listing_short" %in% names(parent_info))
          osf_short_urls <- unique(c(osf_short_urls,
                                     parent_info$repo_name[parent_info$listing_short %in% TRUE]))
        parent_full_urls <- osf_full_listings(parent_info, c(osf_cut_urls, osf_short_urls))

        # public parent: its files belong under the PARENT's real URL (the
        # place a reader would actually need to look). Any file also reached
        # directly via a registration is deduplicated later by the caller
        # (same repo_url + file_url + file_path).
        if ("kind" %in% names(parent_info)) {
          parent_file_list <- parent_info |>
            dplyr::filter(kind == "file", !(public %in% FALSE))
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
                provider = .col_chr(parent_file_list, "provider"),
                osf_node = .col_chr(parent_file_list, "project"),
                storage_listed = parent_file_list$repo_name %in% parent_full_urls
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

  # OSF has no project-level DOI field to surface (see .osf_license()'s own
  # docs) -- only license is fetched here, one extra lightweight request
  # (?embed=license alone, not the full four-call .osf_node_metadata()) per
  # OSF repository. Gated behind osf_license (default FALSE, see its own docs
  # above): unlike every other backend, this is a genuinely extra request this
  # module would not otherwise make, so it is opt-in rather than always-on.
  #
  # Done AFTER the registration -> parent resolution above, for the OSF rows
  # left in `repos` (#486): a cited registration has been replaced there by
  # the project it was registered from, which is where the files are listed
  # from, so that project's licence is the one asked for, and its metadata
  # row joins to the paper below (a row keyed to the removed registration
  # URL had no paper_id). A parent that is closed or has no licence set
  # takes its registration's licence, which covers the same files.
  if (isTRUE(osf_license)) {
    osf_repo_urls <- unique(repos$repo_url[repos$repo_type %in% "osf"])
    lic_of <- function(url) {
      id <- osf_check_id(url)[[1]]
      if (is.na(id)) return(NA_character_)
      tryCatch(.osf_license(id), error = \(e) NA_character_)
    }
    osf_lic <- vapply(osf_repo_urls, \(url) {
      lic <- lic_of(url)
      regs <- names(reg_url_to_parent)[reg_url_to_parent == url]
      for (r in regs) {
        if (!is.na(lic)) break
        lic <- lic_of(r)
      }
      lic
    }, character(1))
    if (length(osf_repo_urls) > 0) {
      osf_meta_df <- data.frame(
        repo_url = osf_repo_urls,
        doi = NA_character_,
        license = unname(osf_lic)
      )
    }
  }

  # Each OSF project's own title, description, category, dates and
  # contributors, from the listing osf_info() already made (no extra request;
  # #634). `osf_desc_all` keeps them for every OSF link the paper gives,
  # registrations included, for telling whose deposit each link is (below).
  osf_repo_urls <- unique(repos$repo_url[repos$repo_type %in% "osf"])
  osf_desc_all <- NULL
  if (length(osf_urls) > 0 && exists("osf_info", inherits = FALSE) &&
      is.data.frame(osf_info) && nrow(osf_info) > 0 && "osf_id" %in% names(osf_info)) {
    own <- osf_info[osf_info$osf_type %in% c("nodes", "registrations", "preprints"), , drop = FALSE]
    own <- own[!duplicated(own$osf_id), , drop = FALSE]
    desc_urls <- unique(c(osf_urls, osf_repo_urls))
    ids <- vapply(desc_urls, \(u) osf_check_id(u)[[1]], character(1), USE.NAMES = FALSE)
    k <- match(tolower(sub("[?/].*$", "", ids)), tolower(own$osf_id))
    osf_desc_all <- data.frame(
      repo_url = desc_urls,
      title = .col_chr(own, "name")[k],
      description = vapply(.col_chr(own, "description")[k],
                           \(v) if (is.na(v)) NA_character_ else .meta_text(v), character(1),
                           USE.NAMES = FALSE),
      category = .col_chr(own, "category")[k],
      osf_type = .col_chr(own, "osf_type")[k],
      creators = .col_chr(own, "contributors")[k],
      creator_orcids = .col_chr(own, "contributor_orcids")[k],
      published = substr(dplyr::coalesce(.col_chr(own, "date_registered")[k],
                                          .col_chr(own, "date_created")[k]), 1, 10),
      updated = substr(.col_chr(own, "date_modified")[k], 1, 10),
      # the contributors' own family names, and how many there are: the OSF
      # names only the first 10 (#682). For the ownership step only, not
      # for repo_metadata (below).
      creator_families = .col_chr(own, "contributor_families")[k],
      n_creators = .col_chr(own, "n_contributors")[k]
    )
  }
  if (length(osf_repo_urls) > 0 && !is.null(osf_desc_all)) {
    osf_desc <- osf_desc_all[match(osf_repo_urls, osf_desc_all$repo_url),
                             setdiff(names(osf_desc_all), c("creator_families", "n_creators")), drop = FALSE]
    osf_desc$repo_url <- osf_repo_urls
    osf_meta_df <- if (nrow(osf_meta_df) > 0) {
      dplyr::left_join(osf_meta_df, osf_desc, by = "repo_url")
    } else {
      cbind(data.frame(doi = NA_character_, license = NA_character_)[rep(1, nrow(osf_desc)), ], osf_desc)
    }
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
      cached <- if (isTRUE(cache)) .repo_info_cache_get("github", url) else NULL
      # a listing stored before the repository's own description was kept
      # (`info`, #634) is asked for again
      if (!is.null(cached) && !is.null(cached$info)) return(cached)
      result <- tryCatch(
        github_tree_files(url),
        error = \(e) list(gated = TRUE, reason = conditionMessage(e),
                          files = NULL, default_branch = NA_character_))
      # A repository that does not exist may have a hyphen a line break
      # added ("Betha-nyL/DeepKoopman"): the form without it is listed
      # instead, and `repo_url` names it (R/repo-link-versions.R).
      if (identical(result$reason, "invalid or inaccessible GitHub repository")) {
        alt <- .repo_link_unhyphenated(url, github_repo)
        if (!is.null(alt)) {
          alt_result <- tryCatch(github_tree_files(alt), error = \(e) NULL)
          if (!is.null(alt_result) && !isTRUE(alt_result$gated)) {
            result <- alt_result
            result$repo_url <- alt
          }
        }
      }
      if (isTRUE(cache) && .repo_info_list_ok(result))
        .repo_info_cache_put("github", url, result)
      result
    })
    # the links found without a hyphen take the place of the links as written
    moved <- vapply(gh_results, \(r) r$repo_url %||% NA_character_, character(1))
    for (i in which(!is.na(moved))) {
      repos$repo_url[repos$repo_url == github_urls[[i]]] <- moved[[i]]
      github_urls[[i]] <- moved[[i]]
    }
    keep <- !duplicated(github_urls)
    gh_results <- gh_results[keep]
    github_urls <- github_urls[keep]
    repos <- dplyr::distinct(repos)
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

    # github_tree_files() already fetches the repo's detected licence (SPDX
    # id) as part of the same metadata request that supplies default_branch
    # -- see archive-github.R. GitHub has no dataset DOI concept, unlike the
    # data-repository backends below, so doi is always NA here.
    # %empty_or% (not %||%) because r$license can come back as a length-zero
    # value rather than NULL; vapply(..., character(1)) requires exactly
    # length 1 from every call, so %||% letting a length-zero value through
    # would error ("values must be length 1").
    gh_license <- vapply(gh_results, \(r) r$license %empty_or% NA_character_, character(1))
    # the repository's own description, from the same metadata request
    # (github_tree_files()'s `info`)
    gh_info <- function(field) unname(vapply(gh_results[github_urls], \(r) {
      v <- r$info[[field]]
      if (is.null(v) || !length(v) || is.na(v[[1]])) NA_character_ else as.character(v[[1]])
    }, character(1)))
    # The README's text, one plain file request per listed repository (not
    # the rate-limited API): its opening names the authors, and it says
    # whether the repository is a tool to install or a study's code and
    # which paper it belongs to (#634). At most 20,000 characters are kept.
    gh_readme <- unname(vapply(gh_results[github_urls], \(r)
      if (isTRUE(r$gated) || records_only) NA_character_ else .repo_readme_from_listing(r$files), character(1)))
    github_meta_df <- data.frame(
      repo_url = github_urls,
      doi = NA_character_,
      license = unname(gh_license[github_urls]),
      readme = gh_readme,
      title = gh_info("full_name"),
      creators = gh_info("owner"),
      published = gh_info("created"),
      updated = gh_info("pushed"),
      description = gh_info("description"),
      keywords = gh_info("topics"),
      homepage = gh_info("homepage"),
      owner_type = gh_info("owner_type"),
      # the name the account gives itself, for whose repository it is (#634)
      owner_name = .github_owner_names(gh_info("owner"), cache = cache),
      stars = suppressWarnings(as.integer(gh_info("stars"))),
      forks = suppressWarnings(as.integer(gh_info("forks"))),
      is_fork = as.logical(gh_info("fork"))
    )
  } else {
    github_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                                 license = character(0))
  }

  ## GitLab ----
  gitlab_urls <- repos |>
    dplyr::filter(repo_type == "gitlab") |>
    _$repo_url |>
    unique()
  gitlab_files_df <- data.frame(repo_name = character(0))
  gitlab_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                               license = character(0))
  if (length(gitlab_urls) > 0) {
    # gitlab_tree_files() fetches project metadata + licence, then the file
    # tree (paginated -- see archive-gitlab.R for why this differs from
    # GitHub's single-request Git Trees API) and backfills file size via a
    # batched GraphQL query. How much of a repository is DOWNLOADED is
    # capped separately, by download_repo_files() in data_check.
    gl_results <- lapply(gitlab_urls, function(url) {
      cached <- if (isTRUE(cache)) .repo_info_cache_get("gitlab", url) else NULL
      if (!is.null(cached)) return(cached)
      result <- tryCatch(
        gitlab_tree_files(url),
        error = \(e) list(gated = TRUE, reason = conditionMessage(e),
                          files = NULL, default_branch = NA_character_,
                          license = NA_character_))
      # a hyphen a line break added, as for GitHub above
      if (identical(result$reason, "invalid or inaccessible GitLab repository")) {
        alt <- .repo_link_unhyphenated(url, gitlab_repo)
        if (!is.null(alt)) {
          alt_result <- tryCatch(gitlab_tree_files(alt), error = \(e) NULL)
          if (!is.null(alt_result) && !isTRUE(alt_result$gated)) {
            result <- alt_result
            result$repo_url <- alt
          }
        }
      }
      if (isTRUE(cache) && .repo_info_list_ok(result))
        .repo_info_cache_put("gitlab", url, result)
      result
    })
    moved <- vapply(gl_results, \(r) r$repo_url %||% NA_character_, character(1))
    for (i in which(!is.na(moved))) {
      repos$repo_url[repos$repo_url == gitlab_urls[[i]]] <- moved[[i]]
      gitlab_urls[[i]] <- moved[[i]]
    }
    keep <- !duplicated(gitlab_urls)
    gl_results <- gl_results[keep]
    gitlab_urls <- gitlab_urls[keep]
    repos <- dplyr::distinct(repos)
    names(gl_results) <- gitlab_urls

    for (url in gitlab_urls) {
      r <- gl_results[[url]]
      if (isTRUE(r$gated)) {
        repos$repo_error[repos$repo_url == url] <- r$reason
        warning(sprintf("Repository %s was not listed: %s.", url, r$reason),
                call. = FALSE)
        paste0("Skipping GitLab repo (", r$reason, "): ", url) |>
          list(what = _) |>
          pb$tick(0, tokens = _)
      }
    }

    good_gl_files <- Filter(Negate(is.null),
                            lapply(gl_results, \(r) if (!isTRUE(r$gated)) r$files else NULL))
    if (length(good_gl_files) > 0) {
      gitlab_file_list <- dplyr::bind_rows(good_gl_files)
      gitlab_file_list <- gitlab_file_list[
        !is.na(gitlab_file_list$type) & gitlab_file_list$type != "dir", , drop = FALSE]
      if (nrow(gitlab_file_list) > 0) {
        gitlab_files_df <- dplyr::tibble(
          repo_url      = gitlab_file_list$repo,
          file_name     = gitlab_file_list$name,
          file_path     = gitlab_file_list$path,
          file_url      = gitlab_file_list$download_url,
          file_location = NA_character_,
          file_size     = gitlab_file_list$size,
          file_type     = gitlab_file_list$type
        )
      }
    }

    # GitLab has no dataset DOI concept either, same as GitHub.
    # %empty_or% (not %||%) for the same reason as gh_license above: vapply
    # requires exactly length 1 from every call.
    gl_license <- vapply(gl_results, \(r) r$license %empty_or% NA_character_, character(1))
    # The README, as for GitHub (one plain file request per listed
    # repository): it can say the repository is a published tool (#634)
    gl_readme <- unname(vapply(gl_results[gitlab_urls], \(r)
      if (isTRUE(r$gated) || records_only) NA_character_ else .repo_readme_from_listing(r$files), character(1)))
    gitlab_meta_df <- data.frame(
      repo_url = gitlab_urls,
      doi = NA_character_,
      license = unname(gl_license[gitlab_urls]),
      readme = gl_readme
    )
  }

  ## ResearchBox ----
  rb_urls <- repos |>
    dplyr::filter(repo_type == "researchbox") |>
    _$repo_url |>
    unique()
  rb_files_df <- data.frame(repo_name = character(0))
  rb_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                           license = character(0))
  # Private boxes (#621): links without a passcode to a box that ResearchBox
  # does not show (it does not exist or is not public), and boxes read through
  # a peer-review link whose box is not public, named like OSF's view-only
  # projects (paper_id, repo_url = the link, project = the box's address)
  rb_not_public <- character(0)
  rb_view_only <- data.frame(paper_id = character(0), repo_url = character(0),
                             project = character(0))
  if (length(rb_urls) > 0) {
    tryCatch({
      rb_file_list <- rbox_file_download(rb_urls, pb = pb, cache = cache)
      # Each box that could not be retrieved (refused, removed, ResearchBox's
      # hourly limit) gets its own reason. Only a failure of every box was
      # reported before; one failed box among several lost its files silently.
      rb_failed <- attr(rb_file_list, "failed") %||%
        data.frame(rb_url = character(0), name = character(0), error = character(0))
      rb_failed <- rb_failed[is.na(rb_failed$name), , drop = FALSE]
      if (nrow(rb_failed) > 0) {
        hit <- match(repos$repo_url, rb_failed$rb_url)
        fill <- !is.na(hit) & is.na(repos$repo_error)
        repos$repo_error[fill] <- paste("ResearchBox did not serve the box:",
                                         rb_failed$error[hit[fill]])
        # the API answers "does not exist or is not public"; the web page asks
        # for a captcha (.rbox_info_page()). A link with its passcode that
        # fails this way is not a box readers were meant to open without it.
        np <- grepl("does not exist or is not public|not public: ResearchBox asks for a captcha",
                    rb_failed$error) & is.na(.rbox_code(rb_failed$rb_url))
        rb_not_public <- unique(rb_failed$rb_url[np])
      }
      rb_file_list <- dplyr::filter(rb_file_list, !isdir)
      rb_files_df <- data.frame(
        repo_url = rb_file_list$rb_url,
        # rb_file_list$name is the RELATIVE PATH within the unzipped
        # ResearchBox archive (e.g. "Materials/foo.pdf" —
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

    # The licence and status of each box that was downloaded (one box_meta
    # request per box; a box that could not be downloaded is not asked about
    # again). ResearchBox puts every box under CC BY 4.0, but names it in each
    # box's metadata since 2026-10.
    rb_ok <- setdiff(rb_urls, repos$repo_url[!is.na(repos$repo_error)])
    if (length(rb_ok) > 0) {
      rb_meta <- lapply(rb_ok, \(u) {
        tryCatch(.rbox_box_meta(u, cache = cache), error = \(e)
          data.frame(RB_license = NA_character_, RB_status = NA_character_))
      }) |> dplyr::bind_rows()
      rb_meta_df <- data.frame(repo_url = rb_ok, doi = NA_character_,
                               license = rb_meta$RB_license)
      # A box read through its peer-review link is private unless ResearchBox
      # says it is public; without a key the status is not known, and the
      # passcode in the link shows it is a link meant for peer review.
      rb_vo <- rb_ok[!is.na(.rbox_code(rb_ok)) & !rb_meta$RB_status %in% "public"]
      if (length(rb_vo) > 0) {
        rb_view_only <- repos[repos$repo_url %in% rb_vo, c("paper_id", "repo_url"), drop = FALSE]
        rb_view_only$project <- paste0("https://researchbox.org/", .rbox_id(rb_view_only$repo_url))
        rb_view_only <- unique(rb_view_only)
      }
    }
  }

  ## DSpace (legacy REST API: PsychArchives and other installations in
  ## .dspace_legacy_hosts(), archive-psycharchives.R) ----
  # Lists public files via the DSpace REST API without downloading them, so
  # this only fills file_url / file_size and leaves file_location = NA;
  # download_repo_files() fetches the bytes later (deferred, like Zenodo/OSF),
  # which keeps the per-file/per-repo size caps in force.
  # psycharchives_file_download() resolves which of .dspace_legacy_hosts() a
  # given url belongs to on its own (see archive-psycharchives.R), so this
  # call is unchanged even though pa_urls may now include non-PsychArchives
  # hosts.
  pa_urls <- repos |>
    dplyr::filter(repo_type == "dspace") |>
    _$repo_url |>
    unique()
  pa_files_df <- data.frame(repo_name = character(0))
  pa_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                           license = character(0))
  if (length(pa_urls) > 0) {
    tryCatch({
      pa_file_list <- psycharchives_file_download(pa_urls, pb = pb, cache = cache)

      # Flag items whose DSpace rights are restricted/embargoed. The REST API
      # only lists publicly retrievable bitstreams, so restricted files never
      # appear in pa_files_df — the item's rights flag is the only signal that
      # some files are machine-inaccessible. Reuses the rights attribute
      # psycharchives_file_download() carries out (no extra API call).
      repo_record_type <- c(repo_record_type, attr(pa_file_list, "record_type"))
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

      # doi/license (dc.identifier.doi / dc.rights.license, dc.rights.uri) are
      # carried as attributes by psycharchives_file_download() (same mechanism
      # as pa_rights above) rather than columns, since the file frame stays
      # file-only -- no extra API call. The licence is its own attribute since
      # 2026-10-13: dc.rights is the ACCESS level ("restrictedAccess"), which
      # used to be reported here as the licence.
      pa_doi <- attr(pa_file_list, "doi")
      pa_license <- attr(pa_file_list, "license")
      if (length(pa_rights) > 0 || length(pa_doi) > 0 || length(pa_license) > 0) {
        pa_meta_df <- data.frame(
          repo_url = pa_urls,
          doi = unname(pa_doi[pa_urls]),
          license = unname(pa_license[pa_urls])
        )
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

  ## DSpace 7+ (archive-dspace7.R -- a differently-shaped REST API from the
  ## legacy one above, so it needs its own download function) ----
  # Same deferred pattern as the legacy-DSpace block: only file_url / file_size
  # are filled here, download_repo_files() fetches bytes later. doi/license are
  # carried as attributes on dspace7_file_download()'s result, the same
  # mechanism psycharchives_file_download() uses for legacy DSpace (see its
  # own doi/rights comment) -- fixed 2026-09-29 (issue #435 follow-up).
  dspace7_urls <- repos |>
    dplyr::filter(repo_type == "dspace7") |>
    _$repo_url |>
    unique()
  dspace7_files_df <- data.frame(repo_name = character(0))
  dspace7_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                                license = character(0))
  if (length(dspace7_urls) > 0) {
    tryCatch({
      dspace7_file_list <- dspace7_file_download(dspace7_urls, pb = pb)

      # doi/license (dc.identifier.doi / dc.rights*) carried as attributes by
      # dspace7_file_download() rather than columns, same as pa_doi/pa_rights
      # above -- no extra API call.
      dspace7_doi <- attr(dspace7_file_list, "doi")
      dspace7_license <- attr(dspace7_file_list, "license")
      repo_record_type <- c(repo_record_type, attr(dspace7_file_list, "record_type"))
      if (length(dspace7_doi) > 0 || length(dspace7_license) > 0) {
        dspace7_meta_df <- data.frame(
          repo_url = dspace7_urls,
          doi = unname(dspace7_doi[dspace7_urls]),
          license = unname(dspace7_license[dspace7_urls])
        )
      }

      if (!is.null(dspace7_file_list) && nrow(dspace7_file_list) > 0) {
        dspace7_file_list <- dspace7_file_list |> dplyr::filter(!isdir)
        dspace7_files_df <- data.frame(
          repo_url = dspace7_file_list$dspace7_url,
          file_name = dspace7_file_list$name,
          file_path = dspace7_file_list$name,
          file_url = dspace7_file_list$file_url,
          file_location = dspace7_file_list$file_location,
          file_size = dspace7_file_list$size,
          file_type = dspace7_file_list$type
        )
      }
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      repos$repo_error[repos$repo_url %in% dspace7_urls] <<- conditionMessage(e)
    })
  }

  ## Zenodo ----
  zenodo_urls <- repos |>
    dplyr::filter(repo_type == "zenodo") |>
    _$repo_url |>
    unique()
  zenodo_files_df <- data.frame(repo_name = character(0))
  zenodo_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                               license = character(0))
  if (length(zenodo_urls) > 0) {
    tryCatch({
      .zenodo_info <- suppressMessages(zenodo_info(zenodo_urls, pb = pb, cache = cache))

      # zenodo_info() already fetches doi/license as part of its normal
      # dataset-record lookup (see archive-zenodo.R) -- this is a plain
      # extraction of columns already in memory, not a new API call.
      #
      # .col_chr(), not a bare as.character(). A paper whose every Zenodo
      # record came back unreadable had no doi/license columns at all, and
      # as.character(NULL) is character(0) -- a zero-length column beside the
      # n-row repo_url column, which data.frame() rejects with "arguments
      # imply differing number of rows: n, 0". That aborted this whole block
      # and flagged every Zenodo URL on the paper. .zenodo_unread() in
      # archive-zenodo.R now guarantees those columns are present, but the
      # guard belongs here too: extracting the metadata never needs the
      # record to have been readable.
      if (nrow(.zenodo_info) > 0) {
        if ("resource_type" %in% names(.zenodo_info))
          repo_record_type <- c(repo_record_type, stats::setNames(
            as.character(.zenodo_info$resource_type), .zenodo_info$zenodo_url))
        zenodo_meta_df <- meta_rows(.zenodo_info, "zenodo_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url,
                                        .zenodo_info, "zenodo_url")
      }

      if (nrow(.zenodo_info) > 0 && "files" %in% names(.zenodo_info)) {
        file_rows <- lapply(seq_len(nrow(.zenodo_info)), function(i) {
          files_i <- .zenodo_info$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            file_url <- NA_character_
            if (!is.null(f$links) && length(f$links$self) > 0) {
              file_url <- as.character(f$links$self)
            }

            # %empty_or% (not %||%) because Zenodo can return a field as a
            # length-zero value (e.g. an empty array) rather than NULL; %||%
            # would let that through and break this data.frame() call with
            # "arguments imply differing number of rows".
            data.frame(
              repo_url = as.character(.zenodo_info$zenodo_url[[i]]),
              file_name = as.character(f$key %empty_or% NA_character_),
              file_path = as.character(f$key %empty_or% NA_character_),
              file_url = file_url,
              file_location = NA_character_,
              file_size = as.numeric(f$size %empty_or% NA_real_)
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
  dv_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                           license = character(0))
  if (length(dv_urls) > 0) {
    tryCatch({
      .dv_info <- suppressMessages(dataverse_info(dv_urls, cache = cache))

      # dataverse_info() already fetches doi/license as part of its normal
      # dataset-version lookup (see archive-dataverse.R) -- plain extraction
      # of columns already in memory, not a new API call.
      if (nrow(.dv_info) > 0) {
        dv_meta_df <- meta_rows(.dv_info, "dataverse_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url,
                                        .dv_info, "dataverse_url")
      }

      if (nrow(.dv_info) > 0 && "files" %in% names(.dv_info)) {
        file_rows <- lapply(seq_len(nrow(.dv_info)), function(i) {
          files_i <- .dv_info$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            # name, size and URL; a file Dataverse converted on upload (a
            # .tab made from a .sav) as it was uploaded (#582), the same as
            # dataverse_file_download()
            fe <- .dataverse_file_entry(f, .dv_info$dataverse_host[[i]])

            # %empty_or% (not %||%) because a field can come back as a
            # length-zero value (e.g. an empty array) rather than NULL; %||%
            # would let that through and break this data.frame() call with
            # "arguments imply differing number of rows".
            data.frame(
              repo_url = as.character(.dv_info$dataverse_url[[i]]),
              file_name = as.character(fe$name),
              file_path = as.character(fe$name),
              file_url = fe$url,
              file_location = NA_character_,
              file_size = fe$size,
              file_restricted = isTRUE(f$restricted)
            )
          })

          dplyr::bind_rows(rows_i)
        })

        dv_files_df <- dplyr::bind_rows(file_rows)

        if (nrow(dv_files_df) > 0) {
          # A restricted file is listed but served only on request (the host
          # answers 403), so download_repo_files() does not ask for it. The
          # column exists only when a file needs it.
          if (!any(dv_files_df$file_restricted)) dv_files_df$file_restricted <- NULL

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
  fs_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                           license = character(0))
  if (length(fs_urls) > 0) {
    tryCatch({
      .fs_info <- suppressMessages(figshare_info(fs_urls, cache = cache))

      # figshare_info() already fetches doi/license as part of its normal
      # article lookup (see archive-figshare.R) -- plain extraction of
      # columns already in memory, not a new API call.
      if (nrow(.fs_info) > 0) {
        fs_meta_df <- meta_rows(.fs_info, "figshare_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url,
                                        .fs_info, "figshare_url")
      }

      if (nrow(.fs_info) > 0 && "files" %in% names(.fs_info)) {
        file_rows <- lapply(seq_len(nrow(.fs_info)), function(i) {
          files_i <- .fs_info$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            # %empty_or% (not %||%) because a field can come back as a
            # length-zero value (e.g. an empty array) rather than NULL; %||%
            # would let that through and break this data.frame() call with
            # "arguments imply differing number of rows".
            data.frame(
              repo_url = as.character(.fs_info$figshare_url[[i]]),
              file_name = as.character(f$name %empty_or% NA_character_),
              # the file's folder within the record, where the listing gives
              # one (Figshare's folder_structure, 2026-10-13), else its name
              file_path = as.character(f$path %empty_or% f$name %empty_or% NA_character_),
              file_url = as.character(f$download_url %empty_or% NA_character_),
              file_location = NA_character_,
              file_size = as.numeric(f$size %empty_or% NA_real_)
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
  dryad_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                              license = character(0))
  if (length(dryad_urls) > 0) {
    tryCatch({
      .dryad_info_tbl <- suppressMessages(dryad_info(dryad_urls, cache = cache))

      # dryad_info() already fetches doi/license as part of its normal
      # dataset-record lookup (see archive-dryad.R) -- plain extraction of
      # columns already in memory, not a new API call.
      if (nrow(.dryad_info_tbl) > 0) {
        # An expired sharing link (#484) is reported with its reason; see
        # meta_rows() and link_errors() above.
        dryad_meta_df <- meta_rows(.dryad_info_tbl, "dryad_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url,
                                        .dryad_info_tbl, "dryad_url")
      }

      if (nrow(.dryad_info_tbl) > 0 && "files" %in% names(.dryad_info_tbl)) {
        file_rows <- lapply(seq_len(nrow(.dryad_info_tbl)), function(i) {
          files_i <- .dryad_info_tbl$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            dl_href <- f$`_links`$`stash:download`$href %empty_or% NA_character_
            file_url <- if (!is.na(dl_href)) paste0("https://datadryad.org", dl_href)
                        else NA_character_

            # %empty_or% (not %||%) because a field can come back as a
            # length-zero value (e.g. an empty array) rather than NULL; %||%
            # would let that through and break this data.frame() call with
            # "arguments imply differing number of rows" (dl_href above uses
            # it too, since a length-zero value would otherwise make
            # `!is.na(dl_href)` error with "argument is of length zero").
            data.frame(
              repo_url = as.character(.dryad_info_tbl$dryad_url[[i]]),
              file_name = as.character(f$path %empty_or% NA_character_),
              file_path = as.character(f$path %empty_or% NA_character_),
              file_url = file_url,
              file_location = NA_character_,
              file_size = as.numeric(f$size %empty_or% NA_real_),
              # Dryad's own total for the dataset (storageSize), which
              # .archive_size_estimate() prefers over summing the listed files
              repo_size = as.numeric((.dryad_info_tbl$storage_size %||%
                                      rep(NA_real_, nrow(.dryad_info_tbl)))[[i]])
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
  reshare_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                                license = character(0))
  if (length(reshare_urls) > 0) {
    tryCatch({
      .reshare_info_tbl <- suppressMessages(reshare_info(reshare_urls, cache = cache))

      # reshare_info() already fetches doi as part of its normal deposit
      # lookup (see archive-reshare.R) -- plain extraction, not a new API
      # call. license is always NA: ReShare's EPrints API does not expose a
      # licence field through this endpoint at all (not a bug, a real gap
      # in what ReShare's own API returns).
      if (nrow(.reshare_info_tbl) > 0) {
        if ("record_type" %in% names(.reshare_info_tbl))
          repo_record_type <- c(repo_record_type, stats::setNames(
            as.character(.reshare_info_tbl$record_type), .reshare_info_tbl$reshare_url))
        reshare_meta_df <- meta_rows(.reshare_info_tbl, "reshare_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url,
                                        .reshare_info_tbl, "reshare_url")
      }

      if (nrow(.reshare_info_tbl) > 0 && "files" %in% names(.reshare_info_tbl)) {
        file_rows <- lapply(seq_len(nrow(.reshare_info_tbl)), function(i) {
          files_i <- .reshare_info_tbl$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            file_url <- f$uri %empty_or% NA_character_
            if (!is.na(file_url)) file_url <- sub("^http://", "https://", file_url)

            # %empty_or% (not %||%) because a field can come back as a
            # length-zero value (e.g. an empty array) rather than NULL; %||%
            # would let that through and break this data.frame() call with
            # "arguments imply differing number of rows" (file_url above uses
            # it too, since a length-zero value would otherwise make
            # `!is.na(file_url)` error with "argument is of length zero").
            data.frame(
              repo_url = as.character(.reshare_info_tbl$reshare_url[[i]]),
              file_name = as.character(f$filename %empty_or% NA_character_),
              file_path = as.character(f$filename %empty_or% NA_character_),
              file_url = file_url,
              file_location = NA_character_,
              file_size = as.numeric(f$filesize %empty_or% NA_real_)
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
  fourtu_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                               license = character(0))
  if (length(fourtu_urls) > 0) {
    tryCatch({
      .fourtu_info <- suppressMessages(researchdata4tu_info(fourtu_urls, cache = cache))

      # researchdata4tu_info() reuses figshare's own info-fetching internally
      # (Djehuty implements the Figshare v2 API), so doi/license are already
      # fetched the same way as the Figshare block above -- plain
      # extraction, not a new API call.
      if (nrow(.fourtu_info) > 0) {
        fourtu_meta_df <- meta_rows(.fourtu_info, "researchdata4tu_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url,
                                        .fourtu_info, "researchdata4tu_url")
      }

      if (nrow(.fourtu_info) > 0 && "files" %in% names(.fourtu_info)) {
        file_rows <- lapply(seq_len(nrow(.fourtu_info)), function(i) {
          files_i <- .fourtu_info$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            # %empty_or% (not %||%) because a field can come back as a
            # length-zero value (e.g. an empty array) rather than NULL; %||%
            # would let that through and break this data.frame() call with
            # "arguments imply differing number of rows".
            data.frame(
              repo_url = as.character(.fourtu_info$researchdata4tu_url[[i]]),
              file_name = as.character(f$name %empty_or% NA_character_),
              # the file's folder within the record, where the listing gives
              # one (Figshare's folder_structure, 2026-10-13), else its name
              file_path = as.character(f$path %empty_or% f$name %empty_or% NA_character_),
              file_url = as.character(f$download_url %empty_or% NA_character_),
              file_location = NA_character_,
              file_size = as.numeric(f$size %empty_or% NA_real_)
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

  ## Mendeley Data ----
  mendeley_urls <- repos |>
    dplyr::filter(repo_type == "mendeley") |>
    _$repo_url |>
    unique()
  mendeley_files_df <- data.frame(repo_name = character(0))
  mendeley_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                                 license = character(0))
  if (length(mendeley_urls) > 0) {
    tryCatch({
      .mendeley_info <- suppressMessages(mendeley_info(mendeley_urls, cache = cache))

      # mendeley_info() already fetches doi/license as part of its normal
      # dataset-record lookup (see archive-mendeley.R) -- this is a plain
      # extraction of columns already in memory, not a new API call.
      if (nrow(.mendeley_info) > 0) {
        mendeley_meta_df <- meta_rows(.mendeley_info, "mendeley_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url,
                                        .mendeley_info, "mendeley_url")
      }

      if (nrow(.mendeley_info) > 0 && "files" %in% names(.mendeley_info)) {
        file_rows <- lapply(seq_len(nrow(.mendeley_info)), function(i) {
          files_i <- .mendeley_info$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          rows_i <- lapply(files_i, function(f) {
            cd <- f$content_details %||% list()

            # %empty_or% (not %||%) because a field can come back as a
            # length-zero value (e.g. an empty array) rather than NULL; %||%
            # would let that through and break this data.frame() call with
            # "arguments imply differing number of rows".
            data.frame(
              repo_url = as.character(.mendeley_info$mendeley_url[[i]]),
              file_name = as.character(f$filename %empty_or% NA_character_),
              file_path = as.character(f$filename %empty_or% NA_character_),
              file_url = as.character(cd$download_url %empty_or% NA_character_),
              file_location = NA_character_,
              file_size = as.numeric(cd$size %empty_or% NA_real_)
            )
          })

          dplyr::bind_rows(rows_i)
        })

        mendeley_files_df <- dplyr::bind_rows(file_rows)

        if (nrow(mendeley_files_df) > 0) {
          mendeley_files_df$ext <- tolower(sub("^.*\\.", "", basename(mendeley_files_df$file_name)))
          no_ext <- !is.na(mendeley_files_df$file_name) &
            !grepl("\\.", basename(mendeley_files_df$file_name))
          mendeley_files_df$ext[no_ext] <- NA_character_

          mendeley_files_df <- mendeley_files_df |>
            dplyr::left_join(metacheck::file_types, by = "ext") |>
            dplyr::rename(file_type = type)

          mendeley_files_df$ext <- NULL
        } else {
          mendeley_files_df$file_type <- character(0)
        }
      }
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      repos$repo_error[repos$repo_url %in% mendeley_urls] <<- conditionMessage(e)
    })
  }

  ## DataONE ----
  # Unlike the other providers above, a DataONE listing needs BOTH the host
  # and the pid to identify a record (see archive-dataone.R) -- dataone_info()
  # takes the repo_url directly rather than a pre-extracted id column, and
  # its own dataone_host/dataone_pid columns (not repo_url) are what actually
  # key the join back to each file's source repo.
  dataone_urls <- repos |>
    dplyr::filter(repo_type == "dataone") |>
    _$repo_url |>
    unique()
  dataone_files_df <- data.frame(repo_name = character(0))
  dataone_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                                license = character(0))
  if (length(dataone_urls) > 0) {
    tryCatch({
      .dataone_info_tbl <- suppressMessages(dataone_info(dataone_urls, cache = cache))

      if (nrow(.dataone_info_tbl) > 0) {
        dataone_meta_df <- meta_rows(.dataone_info_tbl, "dataone_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url,
                                        .dataone_info_tbl, "dataone_url")
      }

      if (nrow(.dataone_info_tbl) > 0 && "files" %in% names(.dataone_info_tbl)) {
        file_rows <- lapply(seq_len(nrow(.dataone_info_tbl)), function(i) {
          files_i <- .dataone_info_tbl$files[[i]]
          if (is.null(files_i) || length(files_i) == 0) {
            return(NULL)
          }

          host <- .dataone_info_tbl$dataone_host[[i]]
          api_base <- NULL
          for (h in .dataone_hosts()) if (identical(h$host, host)) api_base <- h$api_base

          rows_i <- lapply(files_i, function(f) {
            # A file's download url is built from the SAME host's own
            # "object/<pid>" endpoint the dataset's metadata document itself
            # came from -- see the file-level note in archive-dataone.R for
            # why this is used instead of the url the metadata document's
            # <physical> element names directly.
            file_url <- if (!is.null(api_base) && length(f$pid) > 0 && !is.na(f$pid))
              paste0("https://", host, api_base, "object/", utils::URLencode(f$pid, reserved = TRUE))
            else NA_character_

            data.frame(
              repo_url = as.character(.dataone_info_tbl$dataone_url[[i]]),
              file_name = as.character(f$key %empty_or% NA_character_),
              file_path = as.character(f$key %empty_or% NA_character_),
              file_url = file_url,
              file_location = NA_character_,
              file_size = as.numeric(f$size %empty_or% NA_real_)
            )
          })

          dplyr::bind_rows(rows_i)
        })

        dataone_files_df <- dplyr::bind_rows(file_rows)

        if (nrow(dataone_files_df) > 0) {
          dataone_files_df$ext <- tolower(sub("^.*\\.", "", basename(dataone_files_df$file_name)))
          no_ext <- !is.na(dataone_files_df$file_name) &
            !grepl("\\.", basename(dataone_files_df$file_name))
          dataone_files_df$ext[no_ext] <- NA_character_

          dataone_files_df <- dataone_files_df |>
            dplyr::left_join(metacheck::file_types, by = "ext") |>
            dplyr::rename(file_type = type)

          dataone_files_df$ext <- NULL
        } else {
          dataone_files_df$file_type <- character(0)
        }
      }
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      repos$repo_error[repos$repo_url %in% dataone_urls] <<- conditionMessage(e)
    })
  }

  ## LDbase ----
  # No API: ldbase_info() reads the public pages (archive-ldbase.R). A
  # project lists every item in its hierarchy, each item's file in a folder
  # named after the item. Pages give no file sizes; download_repo_files()
  # asks the host for them when it needs them.
  ldbase_urls <- repos |>
    dplyr::filter(repo_type == "ldbase") |>
    _$repo_url |>
    unique()
  ldbase_files_df <- data.frame(repo_name = character(0))
  ldbase_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                               license = character(0))
  if (length(ldbase_urls) > 0) {
    tryCatch({
      .ldbase_info_tbl <- suppressMessages(ldbase_info(ldbase_urls, pb = pb, cache = cache))

      if (nrow(.ldbase_info_tbl) > 0) {
        ldbase_meta_df <- meta_rows(.ldbase_info_tbl, "ldbase_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url,
                                        .ldbase_info_tbl, "ldbase_url")
      }

      if (nrow(.ldbase_info_tbl) > 0 && "files" %in% names(.ldbase_info_tbl)) {
        file_rows <- lapply(seq_len(nrow(.ldbase_info_tbl)), function(i) {
          files_i <- .ldbase_info_tbl$files[[i]]
          if (is.null(files_i) || !nrow(files_i)) return(NULL)
          data.frame(
            repo_url = as.character(.ldbase_info_tbl$ldbase_url[[i]]),
            file_name = files_i$file_name,
            file_path = files_i$file_path,
            file_url = files_i$file_url,
            file_location = NA_character_,
            file_size = NA_real_
          )
        })

        ldbase_files_df <- dplyr::bind_rows(file_rows)

        if (nrow(ldbase_files_df) > 0) {
          ldbase_files_df$ext <- tolower(sub("^.*\\.", "", basename(ldbase_files_df$file_name)))
          no_ext <- !is.na(ldbase_files_df$file_name) &
            !grepl("\\.", basename(ldbase_files_df$file_name))
          ldbase_files_df$ext[no_ext] <- NA_character_

          ldbase_files_df <- ldbase_files_df |>
            dplyr::left_join(metacheck::file_types, by = "ext") |>
            dplyr::rename(file_type = type)

          ldbase_files_df$ext <- NULL
        } else {
          ldbase_files_df$file_type <- character(0)
        }
      }
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      repos$repo_error[repos$repo_url %in% ldbase_urls] <<- conditionMessage(e)
    })
  }

  ## CKAN ----
  # Data portals running CKAN (archive-ckan.R, #637): ckan_info() reads each
  # dataset's record, one request; its files are the resources the portal
  # holds itself, with their sizes (a resource that only links to another
  # website is not listed).
  ckan_urls <- repos |>
    dplyr::filter(repo_type == "ckan") |>
    _$repo_url |>
    unique()
  ckan_files_df <- data.frame(repo_name = character(0))
  ckan_meta_df <- data.frame(repo_url = character(0), doi = character(0),
                             license = character(0))
  if (length(ckan_urls) > 0) {
    tryCatch({
      .ckan_info_tbl <- suppressMessages(ckan_info(ckan_urls, pb = pb, cache = cache))

      if (nrow(.ckan_info_tbl) > 0) {
        ckan_meta_df <- meta_rows(.ckan_info_tbl, "ckan_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url,
                                        .ckan_info_tbl, "ckan_url")
      }

      if (nrow(.ckan_info_tbl) > 0 && "files" %in% names(.ckan_info_tbl)) {
        file_rows <- lapply(seq_len(nrow(.ckan_info_tbl)), function(i) {
          files_i <- .ckan_info_tbl$files[[i]]
          if (is.null(files_i) || !nrow(files_i)) return(NULL)
          data.frame(
            repo_url = as.character(.ckan_info_tbl$ckan_url[[i]]),
            file_name = files_i$file_name,
            file_path = files_i$file_path,
            file_url = files_i$file_url,
            file_location = NA_character_,
            file_size = files_i$file_size
          )
        })

        ckan_files_df <- dplyr::bind_rows(file_rows)

        if (nrow(ckan_files_df) > 0) {
          ckan_files_df$ext <- tolower(sub("^.*\\.", "", basename(ckan_files_df$file_name)))
          no_ext <- !is.na(ckan_files_df$file_name) &
            !grepl("\\.", basename(ckan_files_df$file_name))
          ckan_files_df$ext[no_ext] <- NA_character_

          ckan_files_df <- ckan_files_df |>
            dplyr::left_join(metacheck::file_types, by = "ext") |>
            dplyr::rename(file_type = type)

          ckan_files_df$ext <- NULL
        } else {
          ckan_files_df$file_type <- character(0)
        }
      }
    }, error = \(e) {
      # See the OSF block above for why every url is flagged on failure.
      repos$repo_error[repos$repo_url %in% ckan_urls] <<- conditionMessage(e)
    })
  }

  ## Sequence and expression archives ----
  # GEO, the INSDC archives and ArrayExpress (archive-seq.R, #660): one
  # record each, read for whose deposit it is. Their files are not listed:
  # see "sequence and expression archives" below.
  seq_urls <- repos |>
    dplyr::filter(repo_type %in% c("geo", "ena", "arrayexpress")) |>
    _$repo_url |>
    unique()
  seq_files_df <- data.frame(repo_name = character(0))
  seq_meta_df <- data.frame(repo_url = character(0), doi = character(0), license = character(0))
  seq_info_all <- NULL
  if (length(seq_urls) > 0) {
    tryCatch({
      .seq_info_tbl <- suppressMessages(seq_archive_info(seq_urls, pb = pb, cache = cache))
      seq_info_all <- .seq_info_tbl
      if (nrow(.seq_info_tbl) > 0) {
        seq_meta_df <- meta_rows(.seq_info_tbl, "seq_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url, .seq_info_tbl, "seq_url")
      }
    }, error = \(e) {
      repos$repo_error[repos$repo_url %in% seq_urls] <<- conditionMessage(e)
    })
  }

  ## CRAN ----
  # R packages (archive-cran.R, #478): the record of each, read for whose
  # package it is; the paper's own are listed below, not downloaded
  cran_urls <- unique(repos$repo_url[repos$repo_type %in% "cran"])
  cran_meta_df <- data.frame(repo_url = character(0), doi = character(0), license = character(0))
  cran_info_all <- NULL
  if (length(cran_urls) > 0) {
    tryCatch({
      cran_info_all <- suppressMessages(cran_info(cran_urls, pb = pb, cache = cache))
      if (nrow(cran_info_all) > 0) {
        cran_meta_df <- meta_rows(cran_info_all, "cran_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url, cran_info_all, "cran_url")
      }
    }, error = \(e) {
      repos$repo_error[repos$repo_url %in% cran_urls] <<- conditionMessage(e)
    })
  }

  ## ICPSR and openICPSR ----
  # (archive-icpsr.R): the record of each from DataCite, read for whose
  # deposit it is; the paper's own are listed below. Their files cannot be
  # read: ICPSR's site answers automatic requests with a browser check.
  icpsr_urls <- unique(repos$repo_url[repos$repo_type %in% "icpsr"])
  icpsr_meta_df <- data.frame(repo_url = character(0), doi = character(0), license = character(0))
  icpsr_info_all <- NULL
  if (length(icpsr_urls) > 0) {
    tryCatch({
      icpsr_info_all <- suppressMessages(icpsr_info(icpsr_urls, pb = pb, cache = cache))
      if (nrow(icpsr_info_all) > 0) {
        icpsr_meta_df <- meta_rows(icpsr_info_all, "icpsr_url")
        repos$repo_error <- link_errors(repos$repo_error, repos$repo_url, icpsr_info_all, "icpsr_url")
      }
    }, error = \(e) {
      repos$repo_error[repos$repo_url %in% icpsr_urls] <<- conditionMessage(e)
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
      paper_id = first_paper_id(),
      repo_url = local_path,
      repo_type = "local",
      repo_error = NA_character_
    )
    repos <- dplyr::bind_rows(repos, local_repo)
  }

  ## Publications ----
  # A paper, book or thesis in a repository that also holds them (DSpace,
  # EPrints, Zenodo and the other Invenio installations) is not a deposit,
  # and is ignored with its files (#636, #637): a record whose own type says
  # it is a publication (.repo_publication_type(), R/repo-kind.R) and that
  # holds no data or code file (data, code, a statistics file or an archive).
  # A record that gives no type is ignored only when it lists files and none
  # of them is data or code; a preprint with its data (zenodo.7692046, 185
  # files) is kept. Before the check below, so a paper whose only links are
  # publications has no repositories.
  inst_urls <- unique(repos$repo_url[repos$repo_type %in% c("dspace", "dspace7", "reshare", "zenodo")])
  # the records and files of the publications, kept for the ownership step
  # only, so `ownership` still says what each one is (its title, type and
  # files; #634)
  pub_meta <- NULL; pub_files <- NULL
  if (length(inst_urls)) {
    inst_files <- dplyr::bind_rows(pa_files_df, dspace7_files_df, reshare_files_df, zenodo_files_df)
    is_pub <- .repo_publication_type(unname(repo_record_type[inst_urls]))
    kinds <- lapply(inst_urls, \(u)
      if ("repo_url" %in% names(inst_files)) inst_files$file_type[inst_files$repo_url %in% u] else character(0))
    # A record whose type says publication, and whose only file that may hold
    # data is a zip (three at most), is read inside: a report's zipped
    # citation file is no data (zenodo.7430682, a 1 kB zip of its .bib and
    # .ris beside the report's PDFs). One Range request per zip (zip_peek()),
    # also when `peek_zips` is FALSE, as what the record is depends on it; a
    # zip that cannot be read keeps the record a deposit.
    for (j in which(is_pub %in% TRUE)) {
      k <- kinds[[j]]
      if (!any(k %in% "archive") || any(k %in% c("data", "code", "stats"))) next
      d <- inst_files[inst_files$repo_url %in% inst_urls[[j]] & inst_files$file_type %in% "archive", , drop = FALSE]
      if (!nrow(d) || nrow(d) > 3 || !all(.is_zip(d$file_name)) || !"file_url" %in% names(d)) next
      inner <- lapply(seq_len(nrow(d)), \(i) {
        pk <- tryCatch(zip_peek(d$file_url[[i]], cache = cache, skip_on_api_limit = skip_on_api_limit,
                                size = if ("file_size" %in% names(d)) d$file_size[[i]] else NULL),
                       error = \(e) NULL)
        if (!is.data.frame(pk) || !nrow(pk)) return(NA_character_)
        ft <- metacheck::file_types$type[match(tolower(tools::file_ext(pk$name)), metacheck::file_types$ext)]
        ifelse(is.na(ft), "file", ft)
      })
      inner <- unlist(inner)
      if (length(inner) && !anyNA(inner)) kinds[[j]] <- c(k[!k %in% "archive"], inner)
    }
    no_data <-vapply(kinds, \(k) !any(k %in% c("data", "code", "stats", "archive")), NA)
    listed <- lengths(kinds) > 0
    pub_urls <- inst_urls[(is_pub %in% TRUE & no_data) | (is.na(is_pub) & listed & no_data)]
    repo_publications <- pub_urls
    if (length(pub_urls)) {
      keep_pub <- \(df) if ("repo_url" %in% names(df)) df[df$repo_url %in% pub_urls, , drop = FALSE] else NULL
      pub_meta <- dplyr::bind_rows(lapply(list(pa_meta_df, dspace7_meta_df, reshare_meta_df, zenodo_meta_df), keep_pub))
      pub_files <- keep_pub(inst_files)
      drop_pub <- \(df) if ("repo_url" %in% names(df)) df[!df$repo_url %in% pub_urls, , drop = FALSE] else df
      repos <- drop_pub(repos)
      pa_files_df <- drop_pub(pa_files_df); dspace7_files_df <- drop_pub(dspace7_files_df)
      reshare_files_df <- drop_pub(reshare_files_df); zenodo_files_df <- drop_pub(zenodo_files_df)
      pa_meta_df <- drop_pub(pa_meta_df); dspace7_meta_df <- drop_pub(dspace7_meta_df)
      reshare_meta_df <- drop_pub(reshare_meta_df); zenodo_meta_df <- drop_pub(zenodo_meta_df)
    }
  }

  ## no repos found ----
  if (nrow(repos) == 0) {
    no_repo_text <- "We found no links to repositories on the Open Science Framework, Github, GitLab, ResearchBox, DSpace, EPrints (ReShare and others), Invenio (Zenodo and others), Dataverse, Figshare, Dryad, 4TU.ResearchData, Mendeley Data, DataONE, LDbase, CKAN data portals, GEO, SRA, ENA, DDBJ, ArrayExpress, CRAN, or ICPSR (openICPSR, AEA Data and Code Repository)."
    st <- .per_paper_table(paper,
                repo_n = 0,
        files_n = NA,
        files_data = NA,
        files_code = NA,
        files_readme = NA,
        files_zip = NA
      )
    st$file_share_n <- file_share_n_of(st$paper_id)
    info <- list(
      # a link to a file-sharing service is something to fix (#660)
      traffic_light = if (n_file_share > 0) "yellow" else "na",
      summary_text = if (n_file_share > 0) paste("\n- ", c(no_repo_text, summary_file_share), collapse = "") else no_repo_text,
      summary_table = st,
      ownership = own_ownership(pub_meta, pub_files, if (exists("osf_desc_all", inherits = FALSE)) osf_desc_all else NULL)
    )
    if (n_file_share > 0) info$report <- c(no_repo_text, report_file_share)

    return(info)
  }

  ## file numbers and types ----
  # each OSF storage's listed size and number of files, which decide whether
  # it is downloaded as one archive: the checks pass download_repo_files()
  # only the files they want, so it cannot count the storage itself (#675)
  # (only from listings of whole storages, #679)
  osf_files_df <- .osf_storage_totals(osf_files_df, complete = osf_files_df$storage_listed %in% TRUE)
  osf_files_df$storage_listed <- NULL
  all_files <- dplyr::bind_rows(osf_files_df, github_files_df, gitlab_files_df, rb_files_df, pa_files_df, dspace7_files_df, zenodo_files_df, dv_files_df, fs_files_df, dryad_files_df, reshare_files_df, fourtu_files_df, mendeley_files_df, dataone_files_df, ldbase_files_df, ckan_files_df, seq_files_df, local_files_df)

  # One row per REPOSITORY (not per paper, not per file): a paper can link
  # more than one repository (e.g. one for data, one for code) with
  # different licences, so collapsing to one value per paper here would
  # lose information -- callers that want a single per-paper answer can
  # derive it from this the same way build_master_comparison.R derives
  # mc_repo_types from repo_url. Local paths are excluded: they have no
  # doi/license to surface. ResearchBox gives a licence but no DOI.
  repo_metadata <- dplyr::bind_rows(
    osf_meta_df, github_meta_df, gitlab_meta_df, rb_meta_df, pa_meta_df, dspace7_meta_df,
    zenodo_meta_df, dv_meta_df, fs_meta_df, dryad_meta_df, reshare_meta_df,
    fourtu_meta_df, mendeley_meta_df, dataone_meta_df, ldbase_meta_df, ckan_meta_df, seq_meta_df, cran_meta_df, icpsr_meta_df
  )

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
    # Per file, not two separate tests: duplicated(url) & duplicated(path)
    # dropped a file whose URL and path each appeared on OTHER rows (U121).
    # A remote file is the same file when URL and path are; a local one when
    # its location is.
    loc <- all_files$file_location %||% rep(NA_character_, nrow(all_files))
    dedup_key <- ifelse(!is.na(all_files$file_url),
                        paste("url", all_files$file_url, file_path_dedup, sep = "\x01"),
                 ifelse(!is.na(loc), paste("loc", loc, sep = "\x01"),
                        paste("path", all_files$repo_url, file_path_dedup, sep = "\x01")))
    dupes <- duplicated(dedup_key)
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

    # exclude R package source trees ----
    # An R package (its own DESCRIPTION+NAMESPACE) is reusable software
    # infrastructure, not the study's data or analysis code, so its files are
    # dropped from the listing entirely -- before classification, README
    # detection, or the zip peek below, so they never inflate files_n/
    # files_code or appear in downstream reports. Detected per repo_url (a
    # package's DESCRIPTION/NAMESPACE must belong to the SAME repository) so a
    # package in one OSF component does not wrongly exclude an unrelated
    # sibling component's files.
    is_r_pkg <- stats::ave(
      all_files$file_path, all_files$repo_url,
      FUN = .is_r_package_file
    ) |> as.logical()
    if (any(is_r_pkg)) {
      all_files <- all_files[!is_r_pkg, , drop = FALSE]
    }

    is_readme <- .is_readme_name(all_files$file_name)
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
  # Only .zip can be inspected this way. A plain .tar has no index either, but
  # its headers are not compressed, so one of up to 50 MB is downloaded once,
  # whole, and listed from that copy (see .tar_peek(), #487); the download step
  # later takes its members from the same copy. A .7z/.rar/.tar.gz has no tail
  # index, so its contents stay unknown until data_check downloads it — which
  # is why the report below still recommends .zip over those formats (and over
  # .tar, which can only be listed by downloading it).
  #
  # The archive row is REPLACED by its contents when the peek succeeds, so the
  # listing shows the files a reader gets rather than the container. A failed
  # peek (host ignores ranges, directory not in the tail) leaves the archive row
  # untouched, and data_check opens it after download as before.
  listed_tars <- character(0)
  if (isTRUE(peek_zips) && nrow(all_files) > 0) {
    # A zip that is already on disk is listed from the file, with no request:
    # a ResearchBox box is downloaded and unpacked whole above, so a zip
    # inside it has its copy in file_location and the box's web page as
    # file_url -- peeking at that page only earned a 406 answer and cost about
    # 0.8 s per inner zip (verified 2026-10-13). See .zip_local_source().
    # Only rows with a file_url are expanded, as before: a zip from
    # `local_path` (no file_url) is still opened by data_check after its
    # download step, unchanged.
    has_url <- !is.na(all_files$file_url) & nzchar(all_files$file_url %||% "")
    zip_src <- .zip_local_source(all_files)
    zip_src[is.na(zip_src)] <- all_files$file_url[is.na(zip_src)]
    is_zip <- .is_zip(all_files$file_name) & has_url
    is_tar <- grepl("[.]tar$", all_files$file_name, ignore.case = TRUE) & has_url
    if (any(is_zip | is_tar)) {
      zpb <- pb(sum(is_zip | is_tar), "Reading archive contents [:bar] :current/:total")
      on.exit(zpb$terminate(), add = TRUE)
      expanded <- list(); consumed <- integer(0)
      for (i in which(is_zip | is_tar)) {
        # The listing's own size, where it gave one, saves a HEAD request per
        # archive (see zip_peek()'s `size`); a tar is listed only when its
        # size is known (see .tar_peek()).
        peek <- tryCatch(
          if (is_tar[i]) .tar_peek(zip_src[i], size = all_files$file_size[i],
                                   cache = cache, skip_on_api_limit = skip_on_api_limit)
          else zip_peek(zip_src[i], cache = cache,
                        skip_on_api_limit = skip_on_api_limit,
                        size = all_files$file_size[i]),
          error = \(e) NULL)
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
        rows$file_url  <- NA_character_
        # A member CAN still be retrieved on its own, by asking the archive's own
        # URL for just that member's byte range (.zip_member_fetch(), via
        # .zip_fetch_members()) -- that needs the archive's URL plus the member's
        # exact name within it (zip_peek()'s own listing, not the display
        # file_path above, which is prefixed for readability), not a URL for the
        # member itself. Carried here so a later download step (see
        # download_repo_files(), R/repo-download.R) can fetch it that way instead
        # of needing the whole archive. NA for every non-archive-member row.
        # For a zip already on disk this is the local path instead, which
        # download_repo_files() extracts the member from directly.
        rows$archive_url    <- zip_src[i]
        # A tar member is taken by its name exactly as stored (see
        # .tar_list_file()); `archive_format` tells download_repo_files()
        # which way to fetch it, since an archive's URL need not end in .tar.
        rows$archive_member <- if (is_tar[i]) peek$member else peek$name
        rows$archive_format <- if (is_tar[i]) "tar" else "zip"
        # The archive's own size, so the later member fetch can skip the HEAD
        # request zip_peek() would otherwise make in a new session.
        rows$archive_size   <- as.numeric(all_files$file_size[i] %||% NA_real_)
        # The member has no copy of its own on disk yet (the inherited
        # file_location is the archive's), so the download step fetches it.
        rows$file_location  <- NA_character_
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
      # A listed tar is still not as reusable as a zip (it can only be listed
      # by downloading it), so the report's archive advice keeps naming it.
      listed_tars <- all_files$file_name[consumed[is_tar[consumed]]]
      if (length(consumed) > 0) {
        all_files <- dplyr::bind_rows(all_files[-consumed, , drop = FALSE],
                                      dplyr::bind_rows(expanded))
      }
    }
  }

  ## files that do not belong in a repository ----
  # Files software creates by itself, never the authors, which hold nothing a
  # reader can use (see .useless_file_kind(): operating-system files such as
  # __MACOSX/ and .DS_Store, RStudio's .Rproj.user/, Jupyter checkpoints,
  # compiled Python files, open-document lock files, editor backups, temporary
  # files, .Rhistory, and as suggestions version-control folders and editor
  # settings). Left out here, after zips are opened, so they are never counted,
  # classified or flagged for naming; listed in the report and returned as
  # `useless_files` so the authors can remove them. A "problem" kind turns the
  # light red, as TU/e's data managers require these files removed before a
  # package is archived; a "suggestion" kind turns it yellow.
  useless_files <- if (nrow(all_files) > 0) {
    kind <- .useless_file_kind(dplyr::coalesce(all_files$file_path, all_files$file_name))
    cols <- intersect(c("paper_id", "repo_url", "file_path"), names(all_files))
    useless <- all_files[!is.na(kind), cols, drop = FALSE]
    useless$kind <- kind[!is.na(kind)]
    useless$severity <- .useless_file_severity(useless$kind)
    all_files <- all_files[is.na(kind), , drop = FALSE]
    useless
  } else data.frame(repo_url = character(0), file_path = character(0),
                    kind = character(0), severity = character(0))

  ## preliminary classification ----
  # A PRELIMINARY, name/path-only pass: repo_check does not download files, so
  # it classifies from names/extensions (data_classify_files()) — the same
  # deterministic-first machinery data_check uses, run independently here
  # because data_check operates on a materially different, POST-download file
  # set: it opens the archives .zip peeking cannot reach (.tar.gz, .7z) and
  # reclassifies .txt by content, neither of which is visible from a listing.
  # This preliminary pass powers repo_check's OWN report (warnings, naming
  # check, dropdown below) and the ownership step; data_check's later
  # classification is authoritative for placement.
  all_files$data_type <- data_classify_files(all_files$file_name,
                                             all_files$file_path,
                                             group = all_files$repo_url)
  all_files$doc_role  <- .data_doc_role(all_files$file_name)
  # what each file is for the counts per repository and the archive advice:
  # the same classification, so the two parts of the report agree (#648, K8d)
  all_files$file_kind <- .repo_file_kind(all_files$file_name, all_files$data_type,
                                         all_files$file_size)

  ## whose deposit is it (#634) ----
  # with the records and files of the publications set aside above, so their
  # rows say what they are
  pub_cols <- c("repo_url", "file_name", "file_path", "file_url", "file_type")
  ownership <- own_ownership(
    dplyr::bind_rows(repo_metadata, pub_meta),
    if (is.data.frame(pub_files) && nrow(pub_files))
      dplyr::bind_rows(all_files, pub_files[, intersect(pub_cols, names(pub_files)), drop = FALSE]) else all_files,
    osf_desc_all)
  # OSF projects listed only in part (`osf_max_requests`, `listing_cut`; or
  # part of the listing refused by the OSF, `listing_short`): what their row
  # says about files describes only the folders that were listed. A
  # registration's files are kept under the project it was registered from,
  # so that project counts as listed in part as well.
  if (length(osf_cut_urls)) {
    osf_cut_urls <- unique(c(osf_cut_urls,
      unname(reg_url_to_parent[intersect(osf_cut_urls, names(reg_url_to_parent))])))
  }
  if (length(osf_short_urls)) {
    osf_short_urls <- unique(c(osf_short_urls,
      unname(reg_url_to_parent[intersect(osf_short_urls, names(reg_url_to_parent))])))
  }
  if (is.data.frame(ownership) && nrow(ownership)) {
    ownership$listing_cut <- ownership$repo_url %in% osf_cut_urls
    ownership$listing_short <- ownership$repo_url %in% osf_short_urls
  }
  # the links that were checked: those in the text, and the authors' own
  # deposits cited in the reference list (see above)
  checked_ref <- ownership$source %in% "reference" &
    paste(ownership$paper_id, ownership$repo_url) %in% paste(ref_checked$paper_id, ref_checked$repo_url)
  own_text <- ownership[ownership$source %in% "text" | checked_ref,
                        c("paper_id", "repo_url", "ownership", "ownership_group", "ownership_reason"), drop = FALSE]
  own_text <- own_text[!duplicated(own_text[c("paper_id", "repo_url")]), , drop = FALSE]

  ## deposits that are not this paper's (#648) ----
  # A deposit linked in the text that holds other people's work, or the
  # authors' earlier work, is listed (Whose Deposits) but not checked: its
  # files leave `table`, which code_check, data_check and, through code_check,
  # reproducibility_check read, and they count toward nothing here (files that
  # do not belong, naming, READMEs, studies, the light). In a hand check of
  # 132 preprints, checking such deposits was wrong every time: a re-analysed
  # study's deposit, a cited toolbox, the authors' deposit for an earlier
  # paper. Its files are kept in `table_not_checked`. A repository two papers
  # link leaves the file list here only when no paper counts it as its own;
  # each paper's own rows are settled once paper_id is attached below.
  not_this <- .own_not_this(ownership, reg_url_to_parent)
  linked_by <- unique(repos[, c("paper_id", "repo_url")])
  not_this_key <- paste(not_this$paper_id, not_this$repo_url, sep = "\x01")
  set_aside_urls <- Filter(function(u) {
    all(paste(linked_by$paper_id[linked_by$repo_url %in% u], u, sep = "\x01") %in% not_this_key)
  }, unique(not_this$repo_url))
  files_not_checked <- all_files[all_files$repo_url %in% set_aside_urls, , drop = FALSE]
  all_files <- all_files[!all_files$repo_url %in% set_aside_urls, , drop = FALSE]
  if (nrow(useless_files))
    useless_files <- useless_files[!useless_files$repo_url %in% set_aside_urls, , drop = FALSE]

  n_useless <- nrow(useless_files)
  is_problem <- useless_files$severity == "problem"
  n_useless_problem <- sum(is_problem)
  n_useless_suggestion <- n_useless - n_useless_problem
  kinds_of <- function(kind) {
    k <- table(kind)
    paste(sprintf("%s (%d)", names(k), as.integer(k)), collapse = ", ")
  }
  summary_useless <- c(
    if (n_useless_problem > 0) sprintf(
      "We left out %d file%s that do%s not belong in a repository: %s. Remove %s from the repository.",
      n_useless_problem, plural(n_useless_problem), if (n_useless_problem == 1) "es" else "",
      kinds_of(useless_files$kind[is_problem]), plural(n_useless_problem, "it", "them")),
    if (n_useless_suggestion > 0) sprintf(
      "We left out %d file%s of %s. Keep %s only if you mean to share %s.",
      n_useless_suggestion, plural(n_useless_suggestion),
      kinds_of(useless_files$kind[!is_problem]),
      plural(n_useless_suggestion, "it", "them"), plural(n_useless_suggestion, "it", "them"))
  )
  report_useless <- if (n_useless > 0) c(
    "#### Files that do not belong in the repository",
    sprintf(paste(
      "We left out %d file%s that software creates by itself, not the",
      "authors: %s. They hold nothing a reader can use, but they clutter the",
      "repository, and software can mistake them for real files (for example",
      "a `._` copy of a `.csv` for data)."),
      n_useless, plural(n_useless), kinds_of(useless_files$kind)),
    paste(
      "*Remove these:*",
      "*Operating system* files: macOS adds a `__MACOSX` folder and a `._` copy of",
      "every file it puts in a zip, and `.DS_Store` files to folders; Windows",
      "adds `Thumbs.db` and `desktop.ini`. On a Mac, create zips from the",
      "command line with `zip -r -X`, or remove them afterwards with",
      "`zip -d archive.zip \"__MACOSX/*\" \"*.DS_Store\"`.",
      "*RStudio session files* (`.Rproj.user/`) hold open tabs and unsaved",
      "edits; *R history* (`.Rhistory`, `.Rapp.history`) lists the commands typed in a session",
      "and can hold file paths and passwords; *Jupyter checkpoints*",
      "(`.ipynb_checkpoints/`) are automatic copies of notebooks; *compiled",
      "Python files* (`__pycache__/`, `.pyc`) are recreated when the code runs;",
      "*open-document lock files* (`~$...docx`, `.~lock...#`) are left while a",
      "document is open; *editor backups* (`file~`, `.bak`, `.swp`) and",
      "*temporary files* (`.tmp`, `.temp`, an interrupted save of R's workspace, `.RDataTmp`) are copies a program made by itself."),
    paste(
      "*Keep only on purpose:* *version control* folders (`.git`, `.svn`, `.hg`)",
      "hold the project's history, which can include earlier versions of files",
      "and data that was later removed; *editor settings* (`.idea`, `.vscode`)",
      "can hold paths on your own computer.",
      "A `.gitignore` based on GitHub's templates",
      "(https://github.com/github/gitignore) keeps them out of a repository."),
    scroll_table(data.frame(Repository = useless_files$repo_url,
                            File = useless_files$file_path,
                            Kind = useless_files$kind), maxrows = 10)) else NULL

  ## preliminary study grouping ----
  # From paths/repo-splits/the manuscript roster (data_group_llm()), on the
  # files classified above (see "preliminary classification"), the same way
  # data_check groups its own, POST-download file set.

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

  # Same paper_id attachment for the per-repo metadata table (see
  # repo_metadata's own construction comment above for why this is one row
  # per repo, not per paper).
  if (nrow(repo_metadata) > 0) {
    repo_metadata <- dplyr::left_join(
      repo_metadata,
      repos[, c("paper_id", "repo_url")],
      by = "repo_url",
      relationship = "many-to-many"
    )
  } else {
    repo_metadata$paper_id <- character(0)
  }

  # whose deposit each link is (see "whose deposit is it" above)
  if (nrow(repo_metadata) > 0)
    repo_metadata <- dplyr::left_join(repo_metadata, own_text, by = c("paper_id", "repo_url"))

  # the deposits that are not this paper's, per paper (see "deposits that are
  # not this paper's" above): a repository another paper of a paper list
  # counts as its own stayed in the file list until here
  aside <- paste(all_files$paper_id, all_files$repo_url, sep = "\x01") %in% not_this_key
  if (any(aside)) {
    files_not_checked <- dplyr::bind_rows(files_not_checked, all_files[aside, , drop = FALSE])
    all_files <- all_files[!aside, , drop = FALSE]
  }
  repos <- repos[!paste(repos$paper_id, repos$repo_url, sep = "\x01") %in% not_this_key, , drop = FALSE]

  ## sequence and expression archives: listed, not checked (#660) ----
  # GEO, the INSDC archives (SRA, ENA, DDBJ) and ArrayExpress keep raw data
  # such as sequencing reads, often hundreds of GB, in their own formats and
  # without READMEs. A paper's analysis starts from tables a processing
  # pipeline made from them (in 13 BES papers with code, the missing files
  # were such tables, never the archive's files), so downloading them would
  # not help to rerun it. The authors' own deposits are listed with what they
  # hold; they are not downloaded, and the README, naming and file checks do
  # not apply to them. Others' deposits are in Whose Deposits.
  arch_types <- c("geo", "ena", "arrayexpress")
  arch_own <- if (is.data.frame(ownership) && nrow(ownership) && "repo_type" %in% names(ownership))
    unique(ownership[ownership$repo_type %in% arch_types & ownership$ownership %in% "own_this",
                     c("paper_id", "repo_url", "repo_type"), drop = FALSE]) else
    data.frame(paper_id = character(0), repo_url = character(0), repo_type = character(0))
  repos <- repos[!repos$repo_type %in% arch_types, , drop = FALSE]
  # R packages on CRAN (#478): the paper's own are listed with their version;
  # CRAN checks its packages, and a package is not the paper's analysis
  cran_own <- if (is.data.frame(ownership) && nrow(ownership) && "repo_type" %in% names(ownership))
    unique(ownership[ownership$repo_type %in% "cran" & ownership$ownership %in% "own_this",
                     c("paper_id", "repo_url"), drop = FALSE]) else
    data.frame(paper_id = character(0), repo_url = character(0))
  repos <- repos[!repos$repo_type %in% "cran", , drop = FALSE]
  # Replication packages at ICPSR and openICPSR (AEA Data and Code
  # Repository): the paper's own are listed; their files cannot be read
  icpsr_own <- if (is.data.frame(ownership) && nrow(ownership) && "repo_type" %in% names(ownership))
    unique(ownership[ownership$repo_type %in% "icpsr" & ownership$ownership %in% "own_this",
                     c("paper_id", "repo_url"), drop = FALSE]) else
    data.frame(paper_id = character(0), repo_url = character(0))
  repos <- repos[!repos$repo_type %in% "icpsr", , drop = FALSE]
  report_icpsr <- NULL
  summary_icpsr <- NULL
  if (nrow(icpsr_own) > 0) {
    k <- if (is.data.frame(icpsr_info_all)) match(icpsr_own$repo_url, icpsr_info_all$icpsr_url) else rep(NA_integer_, nrow(icpsr_own))
    col <- function(nm) if (is.data.frame(icpsr_info_all)) icpsr_info_all[[nm]][k] else rep(NA_character_, nrow(icpsr_own))
    doi <- .icpsr_doi_of(icpsr_own$repo_url)
    icpsr_tbl <- data.frame(
      Deposit = sprintf("<a href='%s' target='_blank'>%s</a>", icpsr_own$repo_url, doi),
      Title = substr(col("title"), 1, 120), Published = col("publication_date"), check.names = FALSE)
    n_i <- nrow(icpsr_own)
    report_icpsr <- c(
      "#### Replication packages at ICPSR",
      paste(sprintf("The paper's replication %s at ICPSR (openICPSR, which holds the AEA Data and Code Repository).",
                    plural(n_i, "package is", "packages are")),
            sprintf("We list %s, but cannot check %s files: ICPSR's site does not allow automatic downloads.",
                    plural(n_i, "it", "them"), plural(n_i, "its", "their"))),
      scroll_table(icpsr_tbl, maxrows = 10))
    summary_icpsr <- sprintf("The paper's replication %s at ICPSR (%s); we list %s but cannot check %s files.",
                             plural(n_i, "package is", "packages are"), paste(doi, collapse = ", "),
                             plural(n_i, "it", "them"), plural(n_i, "its", "their"))
  }
  report_cran <- NULL
  summary_cran <- NULL
  if (nrow(cran_own) > 0) {
    k <- if (is.data.frame(cran_info_all)) match(cran_own$repo_url, cran_info_all$cran_url) else rep(NA_integer_, nrow(cran_own))
    pk <- .cran_pkg_of(cran_own$repo_url)
    col <- function(nm) if (is.data.frame(cran_info_all)) cran_info_all[[nm]][k] else rep(NA_character_, nrow(cran_own))
    cran_tbl <- data.frame(
      Package = sprintf("<a href='%s' target='_blank'>%s</a>", cran_own$repo_url, pk),
      Title = substr(col("title"), 1, 120),
      Version = col("version"), Published = col("publication_date"), check.names = FALSE)
    n_c <- nrow(cran_own)
    report_cran <- c(
      "#### R packages on CRAN",
      paste(sprintf("The paper's own R %s on CRAN.", plural(n_c, "package is", "packages are")),
            sprintf("We list %s, but do not download or check %s: CRAN checks its packages, and a package is not the paper's analysis.",
                    plural(n_c, "it", "them"), plural(n_c, "it", "them"))),
      scroll_table(cran_tbl, maxrows = 10))
    summary_cran <- sprintf("The paper's own R %s on CRAN (%s), which we list but do not check.",
                            plural(n_c, "package is", "packages are"), paste(pk, collapse = ", "))
  }
  report_archives <- NULL
  summary_archives <- NULL
  arch_out <- data.frame(paper_id = character(0), accession = character(0), archive = character(0),
                         repo_url = character(0))
  if (nrow(arch_own) > 0) {
    info_k <- if (is.data.frame(seq_info_all)) seq_info_all[seq_info_all$seq_url %in% arch_own$repo_url, , drop = FALSE] else NULL
    size <- tryCatch(seq_archive_size(info_k), error = \(e) NULL)
    k <- if (is.data.frame(info_k)) match(arch_own$repo_url, info_k$seq_url) else rep(NA_integer_, nrow(arch_own))
    ks <- if (is.data.frame(size)) match(arch_own$repo_url, size$seq_url) else rep(NA_integer_, nrow(arch_own))
    acc <- .seq_acc_of(arch_own$repo_url)
    archive_name <- ifelse(arch_own$repo_type == "geo", "GEO",
                    ifelse(arch_own$repo_type == "arrayexpress", "ArrayExpress",
                    ifelse(grepl("^(PRJEB|ERP|ER|SAMEA)", acc), "ENA",
                    ifelse(grepl("^(PRJDB|DRP|DR)", acc), "DDBJ", "SRA / BioProject"))))
    fmt_size <- function(b) ifelse(is.na(b), "", paste0(", ", vapply(b, \(x)
      if (is.na(x)) "" else utils:::format.object_size(x, units = "auto", standard = "SI", digits = 1), "")))
    held <- if (is.data.frame(size)) ifelse(is.na(size$units[ks]), "",
      paste0(size$units[ks], " ", size$unit[ks], fmt_size(size$bytes[ks]))) else rep("", nrow(arch_own))
    arch_tbl <- data.frame(
      Deposit = sprintf("<a href='%s' target='_blank'>%s</a>", arch_own$repo_url, acc),
      Archive = archive_name,
      Title = if (is.data.frame(info_k)) substr(info_k$title[k], 1, 120) else NA_character_,
      Holds = held, check.names = FALSE)
    n_a <- nrow(arch_own)
    report_archives <- c(
      "#### Sequence and expression archives",
      paste(sprintf("The paper deposited %s in %s for sequence and expression data.",
                    plural(n_a, "a data set", sprintf("%d data sets", n_a)), plural(n_a, "an archive", "archives")),
            "We list them, but do not download them or check them as data and code: these archives keep",
            "raw data, such as sequencing reads, in their own formats."),
      scroll_table(arch_tbl, maxrows = 10))
    arch_out <- data.frame(paper_id = arch_own$paper_id, accession = acc, archive = archive_name,
                           repo_url = arch_own$repo_url)
    summary_archives <- sprintf("The paper deposited %s in archives for sequence and expression data (%s), which we list but do not check.",
                                plural(n_a, "a data set", sprintf("%d data sets", n_a)),
                                paste(sprintf("%s: %s", archive_name, acc), collapse = "; "))
  }

  repos <- dplyr::full_join(repos, all_files, by = c("paper_id", "repo_url"), relationship = "many-to-many") |>
    dplyr::summarise(
      files_n = sum(!is.na(file_name)),
      files_data = sum(file_kind %in% "data"),
      files_code = sum(file_kind %in% "code"),
      files_readme = sum(file_kind %in% "readme"),
      files_zip = sum(file_kind %in% "archive"),
      .by = c(paper_id, repo_url, repo_type, repo_error)
    )


  summary_files <- sprintf(
    "We found %d file%s in %d %s.",
    sum(repos$files_n),
    plural(sum(repos$files_n)),
    nrow(repos),
    plural(nrow(repos), "repository", "repositories")
  )
  # the deposits checked although the paper cites them only in its reference
  # list (see "deposits cited in the reference list")
  ref_checked <- ref_checked[paste(ref_checked$paper_id, ref_checked$repo_url) %in%
                               paste(repos$paper_id, repos$repo_url), , drop = FALSE]
  report_ref_checked <- NULL
  if (nrow(ref_checked)) {
    n_rc <- nrow(ref_checked)
    report_ref_checked <- sprintf(
      "The paper cites %s only in its reference list; we checked %s as the authors' deposit for this paper. Linking %s in the data availability statement as well makes %s easier to find.",
      paste(ref_checked$repo_url, collapse = ", "),
      plural(n_rc, "it", "them"), plural(n_rc, "it", "them"), plural(n_rc, "it", "them"))
    summary_files <- paste(summary_files, report_ref_checked)
  }

  ## deposits we did not check ----
  # (see "deposits that are not this paper's" above); one line per deposit
  # with whose it is, and what an author can do when it is this paper's after
  # all: a deposit whose README, title or description names the paper is
  # counted as the paper's own (.repo_ownership(), "the deposit names this
  # paper"), before any other rule
  not_checked <- own_text[paste(own_text$paper_id, own_text$repo_url, sep = "\x01") %in% not_this_key, , drop = FALSE]
  not_checked <- not_checked[!duplicated(not_checked$repo_url), , drop = FALSE]
  whose_of <- c(own_earlier_data = "the authors' earlier data", own_earlier_code = "the authors' earlier code",
                others_data = "others' data", others_code = "others' code")
  summary_not_checked <- NULL
  report_not_checked <- NULL
  if (nrow(not_checked) > 0) {
    n_nc <- nrow(not_checked)
    summary_not_checked <- sprintf(
      "We did not check %d linked deposit%s that %s not this paper's own: %s.",
      n_nc, plural(n_nc), if (n_nc == 1) "is" else "are",
      paste(sprintf("%s (%s)", not_checked$repo_url, unname(whose_of[not_checked$ownership])), collapse = "; "))
    report_not_checked <- c(
      "#### Deposits we did not check",
      paste(
        "The paper links these deposits, but the paper or the deposit itself shows that they hold other people's",
        "work or the authors' earlier work, so we did not check them as the data and code of this paper (the",
        "table Whose Deposits gives the reason for each). If one of them is where you shared the data or code of",
        "this paper, name this paper in it: its title or DOI in the deposit's description or README. Metacheck",
        "then checks it."),
      scroll_table(data.frame(Repository = link(not_checked$repo_url),
                              Whose = unname(whose_of[not_checked$ownership]),
                              Why = not_checked$ownership_reason,
                              check.names = FALSE), maxrows = 10))
  }

  ## repositories that could not be checked ----
  # A repository with a recorded failure and no files (the OSF API down, a
  # record that was not found, a private link) was never listed: it is not
  # "empty", and nothing is known about its README (#501). It is named, with
  # its reason, instead of being counted with the empty repositories.
  unchecked <- !is.na(repos$repo_error) & repos$files_n == 0
  summary_unchecked <- NULL
  report_unchecked <- NULL
  if (any(unchecked)) {
    summary_unchecked <- sprintf(
      "We could not check %d %s: %s.",
      sum(unchecked), plural(sum(unchecked), "repository", "repositories"),
      paste(sprintf("%s (%s)", repos$repo_url[unchecked], repos$repo_error[unchecked]),
            collapse = "; "))
    report_unchecked <- c(
      "#### Repositories that could not be checked",
      paste("These repositories are linked in the paper, but their contents could",
            "not be retrieved, so nothing below describes them. When the reason is",
            "that the platform did not answer, running the check again later may work."),
      paste0("- ", repos$repo_url[unchecked], ": ", repos$repo_error[unchecked]))
  }

  ## private repositories checked through a view-only or peer-review link ----
  # (see the OSF and ResearchBox blocks above) yellow: the files could be
  # checked, but readers of the paper need the view-only or peer-review link,
  # which was meant for peer review
  summary_view_only <- NULL
  report_view_only <- NULL
  view_only <- rbind(osf_view_only, rb_view_only)
  view_only_projects <- unique(view_only$project)
  if (length(view_only_projects)) {
    summary_view_only <- paste(c(
      sprintf("The OSF project %s is private. Metacheck examined the files through the view_only link. As the preprint is public, and the OSF Files are accessible to anyone, consider making the OSF project public.",
              unique(osf_view_only$project)),
      sprintf("The ResearchBox box %s is private. Metacheck examined the files through the peer-review link. As the preprint is public, consider making the box public.",
              unique(rb_view_only$project))), collapse = " ")
    report_view_only <- c("#### Private repositories checked through a view-only or peer-review link",
                          summary_view_only)
  }

  ## ResearchBox boxes that readers cannot open (#621) ----
  # Still counted as repositories that could not be checked (yellow); the
  # authors are told how to make the link open the box. ResearchBox's API gives
  # the same answer for a box that does not exist and one that is not public.
  summary_not_public <- NULL
  report_not_public <- NULL
  if (length(rb_not_public)) {
    box <- .rbox_id(rb_not_public)
    box <- unique(ifelse(is.na(box), rb_not_public, paste0("https://researchbox.org/", box)))
    summary_not_public <- paste(sprintf(
      "The ResearchBox box %s does not exist or is not public, so readers who follow the link cannot open it. As the preprint is public, consider making the box public. If the box has to stay private for now, link to its full peer-review link (%s&PEER_REVIEW_passcode=...), so that the link itself opens the box.",
      box, box), collapse = " ")
    report_not_public <- c("#### Repositories that readers cannot open", summary_not_public)
  }

  ## empty repos ----
  repo_no_files <- sum(repos$files_n == 0 & !unchecked)
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
  # only repositories that were listed (see `unchecked` above)
  repo_no_readme <- sum(repos$files_readme == 0 & !unchecked)
  summary_readme <- sprintf(
    "We found %d README file%s and %d %s without READMEs.",
    readme_n,
    plural(readme_n),
    repo_no_readme,
    plural(repo_no_readme, "repository", "repositories")
  )
  # no repository left to have one (all set aside, or in sequence archives)
  if (nrow(repos) == 0) summary_readme <- NULL

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
  # A plain .tar listed above (by downloading it) is no longer an archive row,
  # but the advice still applies to it, so it is named here too.
  zip_n <- sum(repos$files_zip)
  zip_files <- all_files$file_name[all_files$file_kind %in% "archive"]
  nonzip_files <- c(zip_files[!grepl("[.]zip$", zip_files, ignore.case = TRUE)],
                    listed_tars)
  report_zip <- if (length(nonzip_files) > 0) {
    sprintf(
      "#### Archive Files\n\nThe following files are not ZIP archives: %s. We recommend the `.zip` format for archives: only ZIP stores its file listing in a way that lets a tool inspect the contents without downloading the whole archive, so a `.zip` is more discoverable and re-usable than a `.7z`, `.rar`, `.tar` or `.tar.gz`.",
      paste(nonzip_files, collapse = ", ")
    )
  } else NULL
  if (zip_n > 0) {
    summary_zip <- sprintf(
      "We found %d archive file%s.",
      zip_n,
      plural(zip_n)
    )
  } else {
    summary_zip <- NULL
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

  ## OSF projects listed only in part ----
  # The listing of these stopped at `osf_max_requests` (reason "request
  # limit"), or the OSF refused part of it, also when asked again (reason
  # "refused by the OSF", #681); see the OSF block above, and `listing_cut`
  # and `listing_short` in `ownership`. A project with both is named for the
  # refusal: the part listed is then not kept for a next run to go on with,
  # so running the check again is what helps.
  part_urls <- intersect(unique(c(osf_short_urls, osf_cut_urls)),
                         c(repos$repo_url, all_files$repo_url))
  partial_repos <- data.frame(
    paper_id = repos$paper_id[match(part_urls, repos$repo_url)],
    repo_url = part_urls,
    files_listed = vapply(part_urls, \(u) sum(all_files$repo_url %in% u), integer(1), USE.NAMES = FALSE),
    reason = ifelse(part_urls %in% osf_short_urls, "refused by the OSF", "request limit"))
  if (nrow(partial_repos) > 0) {
    cut_part <- partial_repos[partial_repos$reason == "request limit", , drop = FALSE]
    short_part <- partial_repos[partial_repos$reason == "refused by the OSF", , drop = FALSE]
    report_partial <- c(
      "#### Partly Listed Repositories",
      if (nrow(cut_part) > 0) sprintf(
        "%s %s (%s) %s more folders than metacheck lists by default, so only part of %s files %s listed (%s), and what this report says about %s files describes only that part. Run the check again with `cache = TRUE` to list the next part, or raise `osf_max_requests`.",
        nrow(cut_part),
        plural(nrow(cut_part), "OSF project", "OSF projects"),
        paste(cut_part$repo_url, collapse = ", "),
        if (nrow(cut_part) == 1) "has" else "have",
        if (nrow(cut_part) == 1) "its" else "their",
        if (sum(cut_part$files_listed) == 1) "was" else "were",
        paste(cut_part$files_listed, plural(cut_part$files_listed, "file", "files"), collapse = ", "),
        if (nrow(cut_part) == 1) "its" else "their"),
      if (nrow(short_part) > 0) sprintf(
        "The OSF refused part of the listing of %s, also when asked again, so what this report says about %s files describes only the part that was listed. Run the check again later.",
        paste(sprintf("%s (%d %s listed)", short_part$repo_url, short_part$files_listed,
                      plural(short_part$files_listed, "file", "files")), collapse = ", "),
        if (nrow(short_part) == 1) "its" else "their")
    ) |> paste(collapse = "\n\n")
    summary_partial <- c(
      if (nrow(cut_part) > 0) sprintf(
        "We listed only part of %d OSF %s (the request limit was reached).",
        nrow(cut_part), plural(nrow(cut_part), "project", "projects")),
      if (nrow(short_part) > 0) sprintf(
        "We listed only part of %d OSF %s, because the OSF refused part of the listing; run the check again.",
        nrow(short_part), plural(nrow(short_part), "project", "projects")))
  } else {
    report_partial <- NULL
    summary_partial <- NULL
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
  # Files data_classify_files() could not place at all (data_type == "unknown").
  # Two kinds are left out, as no name the authors could choose would help
  # (#648): an archive, which is a container whose files are typed once it is
  # opened (and which "Archive Files" already reports), and a hidden settings
  # file (".gitattributes", ".Rbuildignore", ".gitkeep"), whose name the tool
  # that reads it fixes. Hidden settings files were unknown in 83 of 563
  # papers.
  is_unknown <- !is.na(all_files$data_type) & all_files$data_type == "unknown" &
    !.is_readable_archive(all_files$file_name %||% "") &
    !((all_files$file_kind %||% rep(NA_character_, nrow(all_files))) %in% "archive") &
    !grepl("^[.]", all_files$file_name %||% "")
  unknown_files <- all_files$file_name[is_unknown]
  n_unknown <- length(unknown_files)
  # Per-paper count for summary_table, below -- n_unknown above stays a
  # corpus-wide total for the narrative report/summary_text (one paragraph per
  # module call, not per paper), but summary_table is one row per paper, so it
  # needs its own paper_id-grouped count rather than the same total repeated
  # on every row.
  n_unknown_by_paper <- if (nrow(all_files) > 0) {
    all_files |>
      dplyr::mutate(.unknown = is_unknown) |>
      dplyr::summarise(files_unknown = sum(.unknown), .by = paper_id)
  } else {
    data.frame(paper_id = character(0), files_unknown = integer(0))
  }
  # A type metacheck does not know is no fault of the deposit: a trained model
  # (.joblib, .pth), a genome file (.bed), an R package's DESCRIPTION. Advice
  # to rename such files, and a yellow light for them, were wrong in 40 of 132
  # preprints (#648, K5), so they are listed for information only, whatever
  # their type.
  if (n_unknown > 0) {
    report_unknown <- sprintf(
      "#### Unclassified Files\n\n%d file%s of a kind metacheck does not know, so %s not counted as data, code or documentation: %s. This is no problem in itself.",
      n_unknown, plural(n_unknown, " is", "s are"), plural(n_unknown, "it is", "they are"),
      paste(utils::head(unknown_files, 10), collapse = ", "))
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

  # Per-paper naming-issues table (with paper_id attached) and its "bad"
  # count for summary_table, below. check_file_naming() only takes
  # file_name/file_path/data_type (no paper_id in, none out), and its padding
  # check (.file_naming_check_padding()) compares files against each other --
  # correct within one paper's own files, meaningless across unrelated papers.
  # So this re-runs check_file_naming() once per paper on that paper's own
  # file subset, rather than trying to join the corpus-wide naming_issues
  # above back onto paper_id by file_name (file names like "data.csv" repeat
  # across unrelated papers, which would misattribute issues).
  #
  # The tagged result (naming_issues_by_paper) is what gets RETURNED as this
  # module's top-level `naming_issues` field, below -- data_check() reads
  # that field via get_prev_outputs("repo_check", "naming_issues") for its
  # own per-file "Naming issue" column, so it needs paper_id to correctly
  # scope issues to the right paper on a multi-paper (batch/corpus) run, not
  # just a corpus-wide count.
  # Files of a local folder checked without a paper have paper_id NA, which
  # split() drops: the summary named the naming problems, while
  # summary_table$naming_issues was 0 and naming_issues had no rows, so
  # data_check saw none either (#650). NA is kept as a group of its own.
  naming_issues_by_paper_list <- if (nrow(all_files) > 0) {
    split(all_files, addNA(factor(all_files$paper_id), ifany = TRUE)) |>
      lapply(function(df) {
        pf <- check_file_naming(
          df$file_name, file_path = df$file_path %||% df$file_name,
          data_type = df$data_type)
        if (nrow(pf) > 0) pf$paper_id <- df$paper_id[[1]]
        pf
      })
  } else {
    list()
  }
  naming_issues_by_paper <- if (length(naming_issues_by_paper_list) > 0) {
    dplyr::bind_rows(naming_issues_by_paper_list)
  } else {
    naming_issues[0, , drop = FALSE]
  }
  n_naming_bad_by_paper <- if (nrow(naming_issues_by_paper) > 0) {
    naming_issues_by_paper |>
      dplyr::summarise(naming_issues = sum(severity == "bad"), .by = paper_id)
  } else {
    data.frame(paper_id = character(0), naming_issues = integer(0))
  }

  if (nrow(naming_issues) > 0) {
    naming_tbl <- naming_issues
    # Internal severity values ("bad"/"suggestion") drive gating elsewhere
    # (n_naming_bad, n_naming_bad_by_paper) but read oddly as a column a user
    # sees verbatim, so the display table relabels them without touching the
    # underlying naming_issues/severity values used for gating above.
    naming_tbl$severity <- c(bad = "Required", suggestion = "Suggestion")[naming_tbl$severity]
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

  # whose deposit each link is (#634)
  report_ownership <- NULL
  if (is.data.frame(ownership) && nrow(ownership) > 0) {
    whose <- c(own_this = "the authors', for this paper",
               own_earlier_data = "the authors' earlier data",
               own_earlier_code = "the authors' earlier code",
               others_data = "others' data", others_code = "others' code",
               not_data = "a publication, not a deposit")
    own_tbl <- data.frame(
      Repository = link(ownership$repo_url),
      `Cited in` = ifelse(ownership$source %in% "text", "text", "reference list"),
      Whose = unname(whose[ownership$ownership]),
      Why = ownership$ownership_reason,
      check.names = FALSE)
    report_ownership <- c("#### Whose Deposits", scroll_table(own_tbl, maxrows = 10))
  }

  report <- c(
    report_unchecked,
    report_partial,
    report_not_public,
    report_view_only,
    report_repo,
    report_ref_checked,
    report_archives,
    report_cran,
    report_icpsr,
    report_file_share,
    report_not_checked,
    report_ownership,
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
    report_useless,
    report_classification
  )

  # traffic_light ----
  # A repository where metacheck cannot tell how many studies exist, or has
  # real (non-suggestion) naming problems is
  # exactly the kind of ambiguity this traffic light exists to flag, alongside
  # the existing zip/README criteria. Files that do not belong in a repository
  # (useless_files above) are something to fix: red for a "problem" kind,
  # yellow for a "suggestion" kind.
  # Every linked deposit holds other people's or the authors' earlier work:
  # there is nothing of this paper's to judge, as when no repository is linked.
  if (nrow(repos) == 0 && (nrow(not_checked) > 0 || nrow(arch_own) > 0 || nrow(cran_own) > 0 || nrow(icpsr_own) > 0)) {
    tl <- "na"
  } else if (n_useless_problem > 0) {
    tl <- "red"
  } else if (zip_n == 0 && repo_no_readme == 0 &&
      is.null(report_roster) && n_naming_bad == 0 &&
      length(closed_reg_unreachable) == 0 && !any(unchecked) &&
      n_useless_suggestion == 0 && !length(view_only_projects)) {
    tl <- "green"
  } else {
    tl <- "yellow"
  }
  # a link to a file-sharing service is something to fix (#660)
  if (n_file_share > 0 && tl %in% c("green", "na")) tl <- "yellow"

  # summary_table ----
  summary_table <- repos |>
    dplyr::mutate(.unchecked = unchecked) |>
    dplyr::summarise(
      repo_n = dplyr::n(),
      # repositories linked but not retrieved (#501), so "repo_n = 0" and
      # "a repository could not be checked" are told apart
      repo_unchecked = sum(.unchecked),
      dplyr::across(files_n:files_zip, sum),
      .by = c(paper_id)
    )
  # every paper has its row, also one whose deposits were all set aside
  # (others', or in sequence archives): it was left out here and filled with
  # zeros later, so its counts of whose deposits were lost
  miss_ids <- setdiff(unique(paper_id(paper) %||% character(0)), summary_table$paper_id)
  if (length(miss_ids)) {
    summary_table <- dplyr::bind_rows(summary_table, data.frame(paper_id = miss_ids))
    for (cc in setdiff(names(summary_table), "paper_id"))
      summary_table[[cc]][summary_table$paper_id %in% miss_ids] <- 0L
  }
  summary_table <- summary_table |>
    dplyr::left_join(n_unknown_by_paper, by = "paper_id") |>
    dplyr::left_join(n_naming_bad_by_paper, by = "paper_id")
  summary_table$files_unknown[is.na(summary_table$files_unknown)] <- 0L
  # whose deposits (#634): linked in the text and the authors' own, or
  # others'; cited in the reference list and others'
  own_n <- function(src, grp) vapply(summary_table$paper_id, \(id)
    sum(ownership$paper_id %in% id & ownership$source %in% src & ownership$ownership_group %in% grp),
    integer(1), USE.NAMES = FALSE)
  summary_table$repo_own <- own_n("text", "own")
  summary_table$repo_others <- own_n("text", "others")
  summary_table$ref_others <- own_n("reference", "others")
  # cited only in the reference list, and checked as the paper's own
  summary_table$file_share_n <- file_share_n_of(summary_table$paper_id)
  summary_table$icpsr_own_n <- vapply(summary_table$paper_id, \(id) sum(icpsr_own$paper_id %in% id),
                                      integer(1), USE.NAMES = FALSE)
  summary_table$cran_own_n <- vapply(summary_table$paper_id, \(id) sum(cran_own$paper_id %in% id),
                                     integer(1), USE.NAMES = FALSE)
  summary_table$archive_own_n <- vapply(summary_table$paper_id, \(id) sum(arch_own$paper_id %in% id),
                                        integer(1), USE.NAMES = FALSE)
  summary_table$ref_own <- vapply(summary_table$paper_id, \(id) sum(ref_checked$paper_id %in% id),
                                  integer(1), USE.NAMES = FALSE)
  summary_table$naming_issues[is.na(summary_table$naming_issues)] <- 0L
  # private OSF projects and ResearchBox boxes checked through a view-only or
  # peer-review link, per paper
  vo_by_paper <- table(factor(unique(view_only[c("paper_id", "project")])$paper_id,
                              levels = summary_table$paper_id))
  summary_table$repo_view_only <- as.integer(vo_by_paper[summary_table$paper_id])
  summary_table$repo_view_only[is.na(summary_table$repo_view_only)] <- 0L
  summary_table$roster_mismatch <- !is.null(roster_check) &&
    length(roster_check$roster) > 0 &&
    (length(roster_check$missing) > 0 || length(roster_check$extra) > 0)

  # summary_text ----
  summary_text <- c(
    summary_unchecked,
    summary_partial,
    summary_not_public,
    summary_view_only,
    summary_repo,
    if (nrow(repos) > 0 || !(nrow(arch_own) + nrow(cran_own) + nrow(icpsr_own))) summary_files,
    summary_archives,
    summary_cran,
    summary_icpsr,
    summary_file_share,
    summary_not_checked,
    summary_readme,
    summary_zip,
    summary_edat,
    summary_restricted,
    summary_closed_reg,
    summary_unknown,
    summary_roster,
    summary_naming,
    summary_useless
  ) |>
    paste("\n- ", x = _, collapse = "")

  # Repositories that were found but could not be listed (e.g. a GitHub repo
  # over the size gate, or a private OSF component). Carried out of the module
  # so downstream converters can explain *why* a paper produced no files,
  # instead of reporting "no repository". code_check and data_check name the
  # ones with no files in their reports (#681); with `paper_id`, as a
  # paper list's run holds the repositories of every paper.
  gated_repos <- repos[!is.na(repos$repo_error),
                       intersect(c("paper_id", "repo_url", "repo_type", "repo_error"), names(repos)),
                       drop = FALSE]

  # return a list ----
  list(
    table = all_files,
    summary_table = summary_table,
    gated_repos = gated_repos,
    # the authors' own deposits in sequence and expression archives (GEO,
    # SRA/ENA/DDBJ, ArrayExpress): paper_id, accession, archive, repo_url.
    # Listed, not in `table`; code_check names them when files are missing.
    archive_deposits = arch_out,
    # OSF projects listed only in part, because their listing stopped at
    # `osf_max_requests` or the OSF refused part of it: paper_id, repo_url,
    # files_listed and reason ("request limit", "refused by the OSF").
    # data_check and code_check warn that their files may be missing from
    # what they download, and name them in their reports (#681).
    partial_repos = partial_repos,
    # naming_issues_by_paper (paper_id attached), not the un-tagged
    # naming_issues used only for this call's own narrative report/summary
    # text above -- data_check() reads this field via
    # get_prev_outputs("repo_check", "naming_issues") and needs paper_id to
    # scope issues to the right paper on a multi-paper run.
    naming_issues = naming_issues_by_paper,
    # One row per repository: paper_id, repo_url, doi, license. See
    # repo_metadata's own construction comment above for why this is
    # per-repo rather than collapsed to one value per paper.
    repo_metadata = repo_metadata,
    # Whose deposit each link is (#634): one row per repository linked in the
    # text (source "text") and per deposit cited only in the reference list
    # (source "reference"), with `ownership`, `ownership_group` and
    # `ownership_reason`, and what the deposit's record and files showed.
    ownership = ownership,
    # The files of the deposits linked in the text that hold other people's
    # or the authors' earlier work (#648), left out of `table` and so not
    # checked by any module; the same columns as `table`.
    table_not_checked = files_not_checked,
    roster_check = roster_check,
    group_no_evidence = group_no_evidence,
    # Files that do not belong in a repository, left out of `table`: paper_id,
    # repo_url, file_path, kind and severity ("problem" or "suggestion").
    useless_files = useless_files,
    # Private OSF projects and ResearchBox boxes whose files were read through
    # a view-only or peer-review link in the paper: paper_id, repo_url (the
    # link) and project (its plain address).
    view_only = view_only,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}
