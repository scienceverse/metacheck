#' Data Check
#'
#' @description
#' This module classifies the files in a repository into semantic types
#' (data, codebook, code, software, output, supplemental, readme, asset,
#' other) and, for tabular data files, extracts each column's type and summary
#' statistics.
#'
#' @details
#' The Data Check module consumes the file list produced by `repo_check` (or
#' any files added via `local_path`). Each file is classified with rule-based
#' heuristics layered over metacheck's `file_category()` and `file_types`: file
#' names matching README / codebook patterns win first, then an extension
#' crosswalk, then format-locked extension overrides. For every file classified
#' as tabular `data` that is available locally, the module reads the file head,
#' describes each column with `data_col_facets()` and computes summary statistics
#' for numeric columns. Following DDI, a column is described by orthogonal facets
#' rather than a single type: `representation` (numeric/text/datetime/code — how
#' it is stored), `measurement_level` (Stevens: nominal/ordinal/interval/ratio),
#' `concept` (what it measures: reaction_time/age/gender/likert/id/…), `role`
#' (identifier/measure/condition/timestamp), `unit`, and a `quality` state
#' (ok/empty/constant). Concepts are detected by name+value rules (the demographic
#' concepts via `data_check_demographic()`); when `llm_use(TRUE)` the model fills
#' concepts and measurement levels the rules left blank. Each data file also gets
#' an inferred **analysis unit** (DDI `analysisUnit`: person/trial/session/dyad,
#' from `data_analysis_unit()`), and the report flags a repository that mixes
#' units of observation. Qualtrics
#' survey exports are recognised (`data_check_is_qualtrics()`): their extra
#' header rows are stripped so columns type correctly, and the reserved
#' response-metadata columns (StartDate, Duration, Finished, ...) are tagged.
#'
#' The module defaults to **rules-only** when `llm_use(FALSE)`: columns the
#' rules cannot resolve (ambiguous 3–20-unique integers, ambiguous character
#' columns) fall back to `continuous` / `text` respectively. When
#' `llm_use(TRUE)`, ambiguous columns and unresolved file types (`other`) are
#' sent to `llm()` for optional refinement, and every file is assigned a study
#' group (`ex1`, `pilot2`, `shared`, ...) so that multi-study repositories can
#' be recognised (used by `psychds_check`). Without an LLM the study `group` is
#' left `NA` (unknown).
#'
#' Column extraction requires files to be readable from disk. Files fetched by
#' `repo_check` from OSF/GitHub/etc. without a local copy (`file_location` is
#' `NA`) are classified but not column-extracted; pass `local_path` to point at
#' a downloaded copy of the repository.
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object, or NULL to check local
#'   files only (see [test_paper()])
#' @param local_path optional path to a local directory. When provided, all
#'   files in it (recursively) are added to the file list alongside any files
#'   found via `repo_check`, and give the module local copies to column-extract.
#' @param local_only if TRUE, skip online repository lookups (see `repo_check`)
#' @param download what to download from online repositories (OSF/GitHub/Zenodo)
#'   into the shared cache:
#'   * `"data"` (the default) fetches only the machine-readable files the checks
#'     analyse — tabular data plus codebook/README files.
#'   * `"all"` fetches **every** file in the repository (code, materials, PDFs,
#'     assets, ...), the right choice when building a complete data archive with
#'     `convert_psychds()`. Still subject to the size caps below.
#'   * `FALSE` (or `"none"`) downloads nothing — files are only classified by
#'     name. `TRUE` is accepted as a synonym for `"data"`.
#'   Downloads are reused on later runs.
#' @param skip_types an optional character vector of `data_type`s never to
#'   download even under `download = "all"` — e.g. `"materials"` for stimuli/
#'   media a release links to rather than mirrors. When `NULL` (the default),
#'   `"materials"` and `"unknown"` are skipped (stimuli/software, and the
#'   classifier's junk drawer of manuscripts / logs / unrecognised content,
#'   which would only inflate an archive); pass an explicit vector to override
#'   this, or `character(0)` to download every type. Skipped files are still
#'   listed (with a reason) in the manifest. Types: `"data"`, `"code"`,
#'   `"documentation"`, `"materials"`, `"output"`, `"unknown"`.
#' @param peek_zips if TRUE, look inside each `.zip` (via an HTTP range request,
#'   without downloading it — see [zip_peek()]) and only fetch zips that contain
#'   actual data or a codebook. A zip of only stimuli/materials is left for the
#'   release to link to rather than mirror. Zips whose contents cannot be peeked
#'   are downloaded as usual. Off by default.
#' @param max_file_size largest single file to download, in MB (default 100).
#'   The size caps are an upfront, all-or-nothing gate: if any file in a
#'   repository exceeds this, the whole repository is refused (nothing
#'   downloaded) with a message naming the size to lift it. Set `Inf` for no cap.
#' @param max_download_size largest total download per repository, in MB
#'   (default 500). If a repository's total exceeds this, the whole repository is
#'   refused (nothing downloaded) with a message naming the size to lift it. Set
#'   `Inf` for no cap.
#' @param cache if `TRUE`, keep downloaded files in a persistent on-disk cache
#'   (see [repo_cache_dir()]) so they are reused on later runs. If `FALSE` (the
#'   default), download to a temporary directory that is discarded when the R
#'   session ends — nothing accumulates on disk. Use `cache = TRUE` when
#'   repeatedly checking the same repositories or building an archive across
#'   runs; clear the cache with [repo_cache_clear()].
#' @param github_gate if TRUE (default), gate large GitHub repositories during
#'   `repo_check` before recursive listing. If `NULL`, defaults to
#'   `download != "all"`, so `download = "all"` forces full GitHub listing.
#' @param github_max_files GitHub gate threshold for total file count (default 1000)
#' @param manifest optional path to write a per-paper file manifest as JSON: the
#'   full list of repository files with their download URL, size, type, Psych-DS
#'   target path, and whether each was downloaded (and if not, why). A directory
#'   path writes `<paper_id>.manifest.json` inside it; a path ending in `.json`
#'   is used verbatim. `NULL` (the default) writes nothing. `"."` writes to the working folder. Useful for auditing a
#'   corpus (what exists, what was fetched) and for building a data archive.
#' @param model the LLM model name (see `llm_model_list()`) used only when
#'   `llm_use(TRUE)`
#' @param params a named list passed to `llm()` (e.g., `list(seed = 123)`),
#'   used only when `llm_use(TRUE)`
#'
#' @returns a list
data_check <- function(paper, local_path = NULL, local_only = FALSE,
                       download = "data",
                       skip_types = NULL,
                       peek_zips = FALSE,
                       max_file_size = 100,
                       max_download_size = 500,
                       cache = FALSE,
                       github_gate = NULL,
                       github_max_files = 1000,
                       manifest = NULL,
                       model = llm_model(),
                       params = list()) {

  # Normalise `download` to one of "none" / "data" / "all". Accept the legacy
  # logical form (TRUE = "data", FALSE = "none").
  download <- if (isTRUE(download)) "data"
              else if (isFALSE(download)) "none"
              else match.arg(as.character(download), c("data", "all", "none"))
  if (is.null(github_gate)) github_gate <- (download != "all")

  # Icon shown in the "Classified as" column, by data_type (readme is a
  # doc_role sub-kind of "documentation", checked first so a readme gets its
  # own icon rather than the generic documentation one).
  .tree_type_icon <- function(data_type, doc_role) {
    ifelse(!is.na(doc_role) & doc_role == "readme", "\U0001F4C4",   # 📄
    ifelse(data_type == "data", "\U0001F4CA",                       # 📊
    ifelse(data_type == "code", "\U0001F4BB",                       # 💻
    ifelse(data_type == "documentation", "\U0001F4D6",              # 📖
    ifelse(data_type == "materials", "\U0001F4C1",                  # 📁
    ifelse(data_type == "output", "\U0001F4C8",                     # 📈
    "\U00002753"))))))                                              # ❓ unknown
  }
  .tree_type_label <- function(data_type, doc_role) {
    ifelse(!is.na(doc_role) & doc_role == "readme", "readme", data_type)
  }

  # Walk a set of relative paths into tree rows. Unlike a plain text `tree`
  # renderer, each row also carries which INPUT PATH (if any) it is the leaf
  # of, via `leaf_idx` (index into `paths`, NA for a folder-only row) — so the
  # caller can attach per-file columns (type, study, naming issue) to the
  # right row without re-parsing the tree text. Two DISTINCT input rows that
  # happen to share the exact same relative path (should not normally happen —
  # repo_check dedupes by file_url + file_path before data_check sees this)
  # collapse into one tree row, same as the original text-only renderer; only
  # the first row's metadata is then shown for it.
  repo_tree_rows <- function(paths) {
    keep <- !is.na(paths) & nzchar(paths)
    paths <- paths[keep]
    orig_idx <- which(keep)
    if (length(paths) == 0)
      return(data.frame(text = character(0), leaf_idx = integer(0)))
    parts_list <- strsplit(gsub("\\\\", "/", paths), "/", fixed = FALSE)
    parts_list <- lapply(parts_list, function(x) x[nzchar(x)])
    out_text <- character(0)
    out_leaf <- integer(0)

    walk <- function(parts_subset, idx_subset, prefix = "") {
      heads <- vapply(parts_subset, function(x) x[[1]], character(1))
      head_order <- unique(heads)
      has_child <- vapply(head_order, function(head) {
        i <- heads == head
        any(vapply(parts_subset[i], length, integer(1)) > 1)
      }, logical(1))
      head_order <- head_order[order(!has_child, tolower(head_order))]

      for (i in seq_along(head_order)) {
        head <- head_order[[i]]
        sel <- heads == head
        tails <- lapply(parts_subset[sel], function(x) x[-1])
        tail_idx <- idx_subset[sel]
        is_last <- i == length(head_order)
        child_exists <- any(vapply(tails, length, integer(1)) > 0)
        branch <- if (is_last) "└── " else "├── "
        next_prefix <- paste0(prefix, if (is_last) "    " else "│   ")
        # A leaf row (no child_exists) is exactly one input path; a folder row
        # (child_exists) is not itself an input path, so leaf_idx is NA.
        this_leaf <- if (child_exists) NA_integer_ else tail_idx[[1]]
        out_text <<- c(out_text, paste0(prefix, branch, head, if (child_exists) "/" else ""))
        out_leaf <<- c(out_leaf, this_leaf)
        next_subset  <- tails[vapply(tails, length, integer(1)) > 0]
        next_tailidx <- tail_idx[vapply(tails, length, integer(1)) > 0]
        if (length(next_subset) > 0) walk(next_subset, next_tailidx, next_prefix)
      }
    }

    walk(parts_list, orig_idx)
    data.frame(text = out_text, leaf_idx = out_leaf)
  }

  # Build the file tree as a real HTML table: Path (tree-indented, monospace) |
  # Classified as (icon + type) | Study | Naming issue (hover for detail).
  # Replaces the earlier plain-text `tree`-style code block with per-file
  # metacheck findings attached directly to each row, instead of requiring a
  # reader to cross-reference the separate File Classification / File Naming
  # tables in repo_check's report.
  repo_tree_block <- function(files, naming_issues = NULL) {
    if (is.null(files) || nrow(files) == 0) return(NULL)
    repo_urls <- unique(files$repo_url[!is.na(files$repo_url) & nzchar(files$repo_url)])
    if (length(repo_urls) == 0) return(NULL)

    # Naming detail by file_name, first match only (a name flagged by several
    # rules shows its first; the full list remains in repo_check's own table).
    naming_of <- function(file_name) {
      if (is.null(naming_issues) || nrow(naming_issues) == 0)
        return(rep(NA_character_, length(file_name)))
      hit <- match(file_name, naming_issues$file_name)
      ifelse(is.na(hit), NA_character_, naming_issues$detail[hit])
    }

    blocks <- lapply(repo_urls, function(repo) {
      sub <- files[files$repo_url == repo, , drop = FALSE]
      rel_paths <- if ("file_path" %in% names(sub)) sub$file_path else sub$file_name
      rows <- repo_tree_rows(rel_paths)
      if (nrow(rows) == 0) return(NULL)

      data_type <- if ("data_type" %in% names(sub)) sub$data_type else NA_character_
      doc_role  <- if ("doc_role"  %in% names(sub)) sub$doc_role  else NA_character_
      group     <- if ("group"     %in% names(sub)) sub$group     else NA_character_
      naming    <- naming_of(sub$file_name)

      is_leaf <- !is.na(rows$leaf_idx)
      # Path/type/study/naming text can all echo real, arbitrary file names —
      # including names this very table exists to flag as containing unusual
      # characters — so every value rendered into the HTML table is escaped:
      # .spv_html_escape() (R/spv.R) for &/</>, plus an explicit quote escape
      # for the two attributes below (title='...', class='...').
      tbl <- data.frame(
        Path = .spv_html_escape(rows$text),
        `Classified as` = "", Study = "", `Naming issue` = "",
        check.names = FALSE
      )
      type_label <- .spv_html_escape(.tree_type_label(
        data_type[rows$leaf_idx[is_leaf]], doc_role[rows$leaf_idx[is_leaf]]))
      tbl[["Classified as"]][is_leaf] <- sprintf(
        "<span class='dv-tree-type'>%s %s</span>",
        .tree_type_icon(data_type[rows$leaf_idx[is_leaf]], doc_role[rows$leaf_idx[is_leaf]]),
        type_label
      )
      grp <- .spv_html_escape(group[rows$leaf_idx[is_leaf]])
      tbl$Study[is_leaf] <- ifelse(is.na(group[rows$leaf_idx[is_leaf]]), "\U02014", grp)
      nm <- naming[rows$leaf_idx[is_leaf]]
      nm_esc <- gsub("'", "&#39;", .spv_html_escape(nm), fixed = TRUE)
      tbl[["Naming issue"]][is_leaf] <- ifelse(
        is.na(nm), "\U02014",
        sprintf("<span class='dv-tree-naming' title='%s'>%s</span>", nm_esc, nm_esc))

      list(repo = repo, table = tbl)
    })
    blocks <- Filter(Negate(is.null), blocks)
    if (length(blocks) == 0) return(NULL)

    # Minimal inline CSS scoped to this table: monospace path column, tight
    # padding (no gap between the last path character and the next column),
    # alternating white/light-grey rows. Written once per report; harmless if
    # data_check runs more than once (the class names are stable/idempotent).
    css <- paste(
      "<style>",
      ".dv-tree-table{border-collapse:collapse;width:100%;font-size:0.85em;}",
      ".dv-tree-table td,.dv-tree-table th{padding:2px 8px;text-align:left;white-space:nowrap;}",
      ".dv-tree-table td:first-child{font-family:monospace;padding-right:4px;}",
      ".dv-tree-table tr:nth-child(odd){background:#ffffff;}",
      ".dv-tree-table tr:nth-child(even){background:#f2f2f2;}",
      ".dv-tree-naming{color:#b00020;text-decoration:underline dotted;cursor:help;}",
      "</style>",
      sep = "\n")

    sections <- lapply(blocks, function(b) {
      html <- sprintf(
        "<table class='dv-tree-table'><thead><tr><th>Path</th><th>Classified as</th><th>Study</th><th>Naming issue</th></tr></thead><tbody>%s</tbody></table>",
        paste(sprintf(
          "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
          b$table$Path, b$table[["Classified as"]], b$table$Study, b$table[["Naming issue"]]
        ), collapse = "")
      )
      paste0("**Repository: ", b$repo, "**\n\n", html)
    })

    collapse_section(
      c(css,
        "The table below shows where files sit within each repository. Each file is tagged with how it was classified, which study it belongs to (when known), and any file-naming issue found by repo_check — hover an underlined naming issue for the reason it is flagged.",
        unlist(sections)),
      title = "Data Tree",
      callout = "note"
    )
  }

  # Wrap a list of (filename -> table) into a Quarto tabset, one tab per file
  # with the filename as the tab label. Using Quarto's native `.panel-tabset`
  # lets the framework handle tab switching and widget sizing, rather than
  # hand-rolled show/hide JS. Tab-heading level (##) becomes a tab label and is
  # not added to the report TOC.
  file_tabset <- function(files, table_fun) {
    if (length(files) == 0) return(NULL)
    # Each tab heading and its body are separate blocks joined by a blank line:
    # Pandoc only parses a `## heading` when a blank line precedes it, so a
    # single newline can let the heading be swallowed into the previous block
    # (collapsing the tabset to one tab with a literal "## file" showing).
    tabs <- vapply(files, function(f) {
      paste(c(paste0("## ", f), table_fun(f)), collapse = "\n\n")
    }, character(1))
    paste(c("::: {.panel-tabset}", tabs, ":::"), collapse = "\n\n")
  }

  # One descriptives table per source file. `desc` must include a `source_file`
  # column; the remaining columns are shown in each per-file table.
  desc_file_tabset <- function(desc) {
    file_tabset(unique(desc$source_file), function(f) {
      tbl <- desc[desc$source_file == f, setdiff(names(desc), "source_file"),
                  drop = FALSE]
      scroll_table(tbl, maxrows = 25)
    })
  }

  # One raw-data preview per file, showing the actual rows and columns. Previews
  # are capped at `preview_rows` to keep the report light; a note is added when
  # a file is truncated.
  data_file_tabset <- function(previews, preview_rows = 100L) {
    file_tabset(names(previews), function(f) {
      df <- previews[[f]]
      n_full <- nrow(df)
      shown  <- utils::head(df, preview_rows)
      c(
        scroll_table(shown, maxrows = 10),
        if (n_full > preview_rows)
          sprintf("*Showing the first %d of %d rows.*", preview_rows, n_full)
      )
    })
  }

  # Resolve a usable paper_id: the info table (used by paper_id()) may be empty
  # for papers built without full metadata, so fall back to the files table's
  # paper_id and then the paper object's own id.
  .pid <- function(files = NULL) {
    id <- paper_id(paper)
    if (length(id) == 0 && !is.null(files) && "paper_id" %in% names(files))
      id <- unique(files$paper_id)
    if (length(id) == 0) id <- paper$paper_id %||% NA_character_
    id[[1]]
  }

  # ── 1. Get the file list from repo_check ────────────────────────────────────
  all_files <- get_prev_outputs("repo_check", "table")
  # Repositories found but not listable (size-gated GitHub, private OSF, ...);
  # kept so a downstream converter can explain why a paper yielded no files.
  listing_gated <- get_prev_outputs("repo_check", "gated_repos")
  # Per-file naming problems (spaces, special characters, unclassifiable, ...),
  # for the "Naming issue" column in the file tree below. repo_check computes
  # these from file_name/file_path/data_type, so re-fetch rather than recompute.
  tree_naming_issues <- get_prev_outputs("repo_check", "naming_issues")
  if (is.null(all_files)) {
    if (!is.null(local_path)) {
      mo <- module_run(paper, "repo_check", local_path = local_path,
                       local_only = local_only,
                       github_gate = github_gate,
                       github_max_repo_size_mb = max_download_size,
                       github_max_files = github_max_files)
    } else {
      mo <- module_run(paper, "repo_check", local_only = local_only,
                       github_gate = github_gate,
                       github_max_repo_size_mb = max_download_size,
                       github_max_files = github_max_files)
    }
    all_files <- mo$table %||% data.frame(
      file_name = character(0), repo_url = character(0),
      file_location = character(0)
    )
    listing_gated <- mo$gated_repos
    tree_naming_issues <- mo$naming_issues
  }

  # ── 2. Classify every file into a data_check semantic type ───────────────────
  if (nrow(all_files) == 0) {
    # Still write an (empty) manifest when one was requested, so every paper —
    # including those with no accessible repository — has a manifest entry and a
    # corpus audit can tell "no repo" apart from "not yet processed".
    if (!is.null(manifest)) {
      empty_files <- data.frame(
        file_name = character(0), file_path = character(0),
        repo_url = character(0), file_url = character(0),
        file_size = numeric(0), data_type = character(0),
        data_format = character(0), file_location = character(0),
        stringsAsFactors = FALSE)
      manifest_path <- .data_check_write_manifest(
        manifest, empty_files, logical(0), NULL,
        paper_id = .pid(all_files), download = download,
        max_file_size = max_file_size, max_download_size = max_download_size,
        skip_types = skip_types)
    }
    return(list(
      traffic_light = "na",
      summary_text = "We found no files to analyse.",
      gated_repos = listing_gated,
      manifest_path = if (!is.null(manifest)) manifest_path else NULL,
      summary_table = data.frame(
        paper_id = .pid(all_files),
        data_file_n = 0, column_n = 0
      )
    ))
  }

  all_files$data_type <- data_classify_files(all_files$file_name, all_files$file_path)
  # Fine-grained documentation role (readme / codebook / supplemental), NA for
  # non-documentation rows. Drives root-vs-per-study placement (readme is
  # collection-level) and which files codebook_check parses.
  all_files$doc_role <- .data_doc_role(all_files$file_name)
  ext <- tolower(tools::file_ext(all_files$file_name))
  all_files$data_format <- ifelse(all_files$data_type == "data",
                                  data_format(ext), NA_character_)
  # Study-group assignment (ex1 / pilot2) is only attempted with an LLM for the
  # residual cases the deterministic passes leave unresolved; without an LLM it
  # is still fully deterministic (see data_group_llm()). The root readme and
  # root ro-crate-metadata.json are collection-level and are never assigned a
  # group at all (see the exclusion below) — every OTHER file always resolves
  # to a real study code, never NA and never a "shared" placeholder.
  all_files$group <- NA_character_
  all_files$referenced_by <- vector("list", nrow(all_files))

  llm_file_updates <- 0L
  llm_col_updates <- 0L
  llm_group_updates <- 0L
  roster_check      <- NULL
  group_unresolved  <- character(0)
  llm_model_used <- NA_character_

  # Optional LLM pass for files still unresolved after rules.
  if (llm_use()) {
    amb_files <- which(all_files$data_type == "unknown")
    if (length(amb_files) > 0) {
      file_text <- vapply(amb_files, function(i) {
        fname <- all_files$file_name[[i]] %||% ""
        ext_i <- tolower(tools::file_ext(fname))
        sprintf("file_name: %s\nextension: %s", fname, ifelse(nzchar(ext_i), ext_i, "none"))
      }, character(1))

      file_prompt <- paste(
        "Classify each file into one type:",
        "documentation, materials, output, unknown.",
        "Return one result per numbered input line, echoing its index and the",
        "best single type as `value`. Use 'unknown' when uncertain."
      )

      file_levels <- c("documentation", "materials", "output", "unknown")
      pred <- .llm_classify_batched(file_text, file_prompt,
                                    value_desc = "Best single semantic file type",
                                    valid = file_levels,
                                    model = model, params = params,
                                    phase = "Classifying file types")
      if (is.na(llm_model_used))
        llm_model_used <- attr(pred, "llm_model") %||% NA_character_

      ok <- !is.na(pred)
      if (any(ok)) {
        all_files$data_type[amb_files[ok]] <- pred[ok]
        llm_file_updates <- sum(ok)
      }
    }

  }

  # ── 2b. Study groups ─────────────────────────────────────────────────────────
  # Classify each analysable file into a study group. This is DETERMINISTIC where
  # the evidence allows — the source repository, a path that names its study, the
  # data a script reads/writes, and the studies the manuscript names — so it runs
  # whether or not an LLM is enabled. data_group_llm() falls back to the model
  # only for files none of that evidence can place (and only when llm_use(TRUE)).
  #
  # The root readme and root ro-crate-metadata.json are collection-level: they
  # are never assigned a study, so they must never be sent to data_group_llm()
  # at all (it treats every input row as needing exactly one study code). A
  # readme is "root" here when nothing else already ties it to one specific
  # study: its own path names no study, AND it is not inside a directory that
  # (by name) already reads as a per-study folder. A repo with a genuine
  # per-study README (e.g. study1/README.md) keeps that file grouped normally;
  # only the true collection-wide readme is excluded.
  # path_for_group is coalesced PER ELEMENT (not just when the whole column is
  # NULL, which %||% alone would miss): a mixed multi-source file list can have
  # individual rows where file_path is NA even though the column exists, which
  # would otherwise make is_root_readme itself NA for that row.
  path_for_group <- ifelse(
    is.na(all_files$file_path) | !nzchar(all_files$file_path %||% ""),
    all_files$file_name, all_files$file_path)
  # A LICENSE is collection-level exactly like the readme (one licence for the
  # whole deposit, never one per study), so it is excluded from per-study
  # grouping the same way — see the "license" doc_role in .data_doc_role() and
  # its root placement in psychds_check.R's target_of().
  is_root_readme <- !is.na(all_files$doc_role) &
    all_files$doc_role %in% c("readme", "license") &
    is.na(.data_group_from_path(path_for_group)) &
    !grepl("study[-_]?[0-9]|/ex[0-9]|/pilot[0-9]", tolower(path_for_group))
  is_root_readme[is.na(is_root_readme)] <- FALSE

  grp <- data_group_llm(all_files[!is_root_readme, , drop = FALSE],
                        model = model, params = params, paper = paper)
  group_no_evidence <- FALSE
  if (!is.null(grp)) {
    all_files$group[!is_root_readme] <- grp$group
    all_files$referenced_by[!is_root_readme] <- grp$referenced_by
    llm_group_updates <- sum(!is.na(grp$group))
    if (is.na(llm_model_used)) llm_model_used <- grp$model %||% NA_character_
    roster_check <- attr(grp, "roster_check")
    group_unresolved <- attr(grp, "unresolved") %||% character(0)
    group_no_evidence <- isTRUE(attr(grp, "no_evidence"))
  }

  # ── 2c. Download the files this module (and codebook_check) will read ─────────
  # repo_check lists OSF/GitHub/Zenodo files without fetching them. With
  # download = "data" (default) fetch only the readable subset the checks analyse
  # (tabular data + codebook/readme); with download = "all" fetch every file, for
  # building a complete data archive. Repos refused by the size caps (upfront,
  # all-or-nothing per repo).
  gated_repos <- NULL
  oversize_files <- NULL
  failed_files   <- NULL
  # Which files this run WANTS to download (kept in scope for the manifest).
  want <- if (download == "all") {
    rep(TRUE, nrow(all_files))
  } else if (download == "data") {
    # Always fetch .txt and .iqdat, whatever the name-based classifier guessed,
    # and whatever data_format() said. Both carry TRIAL-LEVEL data that is read
    # by the Behaverse path (.bh_read_file), NOT by data_read_head — so neither
    # is "tabular" in the sense the first clause tests, and gating them on it
    # would mean never fetching them.
    #   * .txt is ambiguous: it can hold experiment data (E-Prime exports, task
    #     logs), a codebook, or plain prose, and the classifier only sees the
    #     remote FILE NAME. Fetching them all and reclassifying from content
    #     afterwards (see .txt_reclassify below) is the only way to find
    #     trial-level data published as .txt.
    #   * .iqdat is unambiguous (Inquisit output) but has no data_read_head()
    #     branch, so data_format() correctly calls it "raw". It must still be on
    #     disk for .bh_is_trial_level_file() to screen it and for the psychds
    #     release to carry it into data/, where .bh_data_files() finds it.
    # Both are cheap tab/comma-delimited text: ~169 KB mean for .txt across the
    # corpus cache, and the per-file / per-repo size caps still apply. Note a
    # study can publish one .iqdat PER PARTICIPANT per block (439 in one corpus
    # study for a single task), which the caps are there to bound.
    (all_files$data_type == "data" &
       !is.na(all_files$data_format) & all_files$data_format == "tabular") |
      (all_files$data_type == "documentation" &
         !is.na(all_files$doc_role) & all_files$doc_role %in% c("codebook", "readme")) |
      grepl("[.](txt|iqdat)$", all_files$file_name, ignore.case = TRUE)
  } else {
    rep(FALSE, nrow(all_files))   # download = "none"
  }
  # Never fetch excluded types. When the caller does not pass `skip_types`, two
  # types are excluded BY DEFAULT even under download = "all": `materials`
  # (stimuli/software a release links to rather than mirrors) and `unknown`
  # (the classifier's junk drawer — manuscripts, logs, unrecognised content,
  # with no analytic value that would inflate an archive). Files worth keeping
  # are rescued OUT of `unknown` upstream, before this gate: by the name rules
  # in data_classify_files() (prereg/readme/codebook) and by the LLM file-type
  # pass above. A caller who really wants everything can pass
  # skip_types = character(0).
  never_fetch <- if (is.null(skip_types)) c("materials", "unknown") else skip_types
  if (length(never_fetch) > 0) {
    # A readable archive (.zip / tar family / single-file .gz|.bz2|.xz) is not
    # itself a content type (an archive crosswalks to `unknown` only because it
    # hasn't been opened yet), but its CONTENTS may be data/documentation — the
    # whole point of the peek/expand machinery is to open it and keep the inner
    # data. So the `unknown` exclusion must NOT strip a container we can open,
    # or we would silently drop every zipped/tarred dataset. Archives base R
    # CANNOT open (.7z/.rar/...) are not carved out: they stay excluded and
    # repo_check warns the author to re-upload as .zip.
    keep_archive <- .is_readable_archive(all_files$file_name)
    want <- want & (!(all_files$data_type %in% never_fetch) | keep_archive)
  }

  # Peek inside zips (HTTP range request, no full download) and only keep those
  # that hold actual data/documentation content; a zip of only materials is
  # left to be linked, not mirrored. Zips we cannot peek are kept (downloaded
  # as usual).
  zip_peek_reason <- rep(NA_character_, nrow(all_files))
  if (isTRUE(peek_zips) && download != "none") {
    is_zip <- want & grepl("[.]zip$", all_files$file_name, ignore.case = TRUE) &
      !is.na(all_files$file_url) & nzchar(all_files$file_url %||% "")
    if (any(is_zip)) {
      zpb <- pb(sum(is_zip), "Peeking into zips [:bar] :current/:total")
      on.exit(zpb$terminate(), add = TRUE)
      for (i in which(is_zip)) {
        d <- zip_decision(all_files$file_url[i], skip_types = skip_types %||% "materials")
        if (isFALSE(d$worth)) {
          want[i] <- FALSE
          zip_peek_reason[i] <- paste0("zip skipped: ", d$reason)
        }
        zpb$tick()
      }
    }
  }
  if (download != "none") {
    need_dl <- want &
      (is.na(all_files$file_location) | !nzchar(all_files$file_location %||% "")) &
      !is.na(all_files$file_url) & nzchar(all_files$file_url %||% "")
    if (any(need_dl)) {
      dl <- download_repo_files(all_files[need_dl, , drop = FALSE],
                                max_file_size = max_file_size,
                                max_download_size = max_download_size,
                                cache = cache)
      all_files$file_location[need_dl] <- dl$file_location
      # Files in a gated repo keep file_location = NA, so they fall out of the
      # has_local extraction filter naturally. The refusal was already reported
      # inline (and warned) by cap_report inside download_repo_files.
      gated_repos <- attr(dl, "gated")
      # Kept for the manifest: which files the per-file cap skipped
      # (intentional) and which downloads failed after retries (unintentional —
      # the re-run signal).
      oversize_files <- attr(dl, "oversize_skipped")
      failed_files   <- attr(dl, "failed")
    }

    # Unlike .spv/.smcl (unambiguously SPSS/Stata-specific), ".out" is a
    # generic extension also used for compiled Unix binaries and unrelated
    # tool logs, so classifying it "output" by extension alone (needed to
    # get it downloaded in the first place) can be a false positive.
    # Content can only be checked once the file is actually on disk, so
    # reclassify any downloaded .out that fails the real Mplus-version-banner
    # check (.mplus_is_genuine_output(), R/mplus.R) back to "unknown" here.
    is_out <- all_files$data_type == "output" &
      grepl("\\.out$", all_files$file_name, ignore.case = TRUE) &
      !is.na(all_files$file_location) & nzchar(all_files$file_location %||% "") &
      file.exists(all_files$file_location %||% "")
    if (any(is_out)) {
      not_mplus <- vapply(all_files$file_location[is_out], function(p)
        !.mplus_is_genuine_output(p), logical(1))
      all_files$data_type[which(is_out)[not_mplus]] <- "unknown"
    }

    # Expand downloaded archives we can OPEN with base R (zip, tar family, and
    # single-file .gz/.bz2/.xz): unpack, classify the inner files, add the
    # data/documentation ones to the file list (dropping inner materials), and
    # DROP the archive's own row entirely — a zip/tar/gz is never itself a
    # content type (only its contents are; a .csv.gz is, for every purpose, a
    # .csv), so once its contents have been extracted and added as their own
    # classified rows, the container row has served its purpose and is removed
    # rather than kept around under some placeholder type. The original archive
    # stays the "link" for any inner materials left un-extracted. An archive of
    # only materials contributes no rows, i.e. it is dropped as if it had not
    # been downloaded.
    #
    # Zip expansion is gated on peek_zips (its pre-download peek is the paired
    # optimisation); tar/gz have no peek, so they always expand when downloaded.
    # .7z/.rar are NOT here — base R cannot read them (repo_check warns instead).
    on_disk <- !is.na(all_files$file_location) &
      nzchar(all_files$file_location %||% "") &
      file.exists(all_files$file_location %||% "")
    da_zip <- if (isTRUE(peek_zips))
      which(on_disk & .is_zip(all_files$file_name)) else integer(0)
    da_tar <- which(on_disk & .is_tar_archive(all_files$file_name))
    da_gz  <- which(on_disk & .is_single_compress(all_files$file_name))
    da <- c(da_zip, da_tar, da_gz)
    if (length(da) > 0) {
      extracted <- list()
      for (i in da) {
        row1 <- all_files[i, , drop = FALSE]
        loc  <- all_files$file_location[i]
        st   <- skip_types %||% "materials"
        rows <- if (i %in% da_zip) .expand_zip(loc, row1, skip_types = st)
                else if (i %in% da_tar) .expand_tar(loc, row1, skip_types = st)
                else .expand_compressed(loc, row1, skip_types = st)
        if (nrow(rows) > 0) extracted[[length(extracted) + 1L]] <- rows
      }
      # Drop the consumed container rows (see comment above); `want` is kept
      # aligned by dropping the same positions.
      all_files <- all_files[-da, , drop = FALSE]
      want <- want[-da]
      if (length(extracted) > 0) {
        added <- dplyr::bind_rows(extracted)
        all_files <- dplyr::bind_rows(all_files, added)
        # Keep `want` aligned with `all_files`: inner files from accepted archives
        # are part of the mirrored data/documentation payload.
        want <- c(want, rep(TRUE, nrow(added)))
      }
    }
    # Reclassify downloaded .txt files from their CONTENT. The name-based
    # classifier ran on remote file names, where a .txt is ambiguous — an E-Prime
    # export, a task log, a codebook and a prose note are indistinguishable. Now
    # that the bytes are local, txt_classify_content() can tell data from prose,
    # so trial-level data published as .txt is found instead of being filed as
    # supplemental. Only ever an UPGRADE to "data": an unrecognised .txt keeps
    # whatever the name implied, so prose is never mistaken for data. The file
    # itself is untouched — the cache is persistent by design, and classification
    # decides how a file is USED, not whether it is kept.
    # A name-based readme/codebook verdict is authoritative and is never
    # overridden: "README.txt" is a readme even if it holds a delimiter-rich
    # table, and a codebook stays a codebook (codebook_check parses it). Only the
    # ambiguous leftovers ("unknown", or documentation with doc_role
    # "supplemental") are eligible for the upgrade.
    is_txt <- grepl("[.]txt$", all_files$file_name, ignore.case = TRUE) &
      (all_files$data_type == "unknown" |
         (all_files$data_type == "documentation" &
            !is.na(all_files$doc_role) & all_files$doc_role == "supplemental")) &
      !is.na(all_files$file_location) & nzchar(all_files$file_location %||% "") &
      file.exists(all_files$file_location %||% "")
    if (any(is_txt)) {
      for (i in which(is_txt)) {
        ct <- tryCatch(txt_classify_content(all_files$file_location[i]),
                       error = function(e) NA_character_)
        if (identical(ct, "data")) {
          all_files$data_type[i]   <- "data"
          all_files$data_format[i] <- data_format("txt")
        }
      }
    }
  }

  # ── 3. Select tabular data files that are available locally ──────────────────
  is_tabular_data <- all_files$data_type == "data" &
    !is.na(all_files$data_format) & all_files$data_format == "tabular"
  has_local <- !is.na(all_files$file_location) &
    nzchar(all_files$file_location) &
    file.exists(all_files$file_location %||% "")

  # Hold trial-level files (E-Prime / Inquisit / jsPsych / native Behaverse) OUT
  # of the per-file tabular extractor. These formats publish one file PER
  # PARTICIPANT per block, so treating each as its own dataset would produce
  # hundreds of fragmented "datasets" for what is one instrument. They are instead
  # normalised and MERGED per instrument into Behaverse paradata/<instrument>.json
  # by convert_psychds() (.osd_write_paradata). Detection is header-only (cheap),
  # so screening many files is fast. They are recorded for the manifest and never
  # deleted — only routed away from the tabular path.
  #
  # NOT gated on is_tabular_data: these files are read by .bh_read_file(), a
  # reader separate from data_read_head(), so "can data_read_head() parse it" is
  # the wrong question here. .iqdat has no data_read_head() branch, so
  # data_format() correctly returns "raw" for it — gating this screen on
  # is_tabular_data would mean an .iqdat downloaded via the rescue above (gate 1)
  # is then never screened here (gate 2), landing nowhere: not in the tabular
  # path, not in the Behaverse path.
  is_trial_level <- has_local &
    vapply(all_files$file_location, function(p)
      isTRUE(tryCatch(.bh_is_trial_level_file(p), error = function(e) FALSE)),
      logical(1))
  trial_level_files <- all_files$file_name[is_trial_level]
  # Make the manifest honest about these: they are not unread "raw" data, they
  # are processed by the Behaverse path and merged into paradata/<instrument>.json
  # by convert_psychds(). psychds_check calls data_format() fresh on the
  # EXTENSION (not this column) to plan conversions, so relabelling here cannot
  # misroute anything — it only changes what the manifest reports.
  all_files$data_format[is_trial_level] <- "trial_level"

  data_files <- all_files[is_tabular_data & has_local & !is_trial_level, ,
                          drop = FALSE]

  n_tabular_all <- sum(is_tabular_data & !is_trial_level)
  n_no_local <- sum(is_tabular_data & !has_local)

  # File names detected as manifests (a table-of-contents listing other repo
  # files) rather than real data; demoted to documentation (doc_role
  # "supplemental") after extraction.
  manifest_files <- character(0)
  # .RData/.rda workspaces that held no reusable tabular data (only fitted
  # models / session objects, or could not be restored). Flagged as a
  # data-sharing recommendation: share the underlying data as CSV + codebook.
  workspace_files <- character(0)
  # Files that read as a data frame but are not a usable rectangular dataset
  # (human-formatted coding worksheets: mostly free-text columns and/or almost
  # entirely empty; see .tabular_usable()). Columns are NOT extracted from these
  # and they are NOT sent to the LLM, but they stay in the file classification so
  # data_validate's spreadsheet-formatting checks still inspect them. name -> reason.
  non_tabular_files <- character(0)

  # ── 4. Extract columns + stats from each local tabular data file ─────────────
  columns_df <- NULL
  # Keep each read data frame so the report can preview the raw data without
  # re-reading. Keyed by source file name.
  file_previews <- list()
  if (nrow(data_files) > 0) {
    pb_cols <- pb(nrow(data_files), ":what [:bar] :current/:total")
    pb_cols$tick(0, list(what = ""))
    on.exit(pb_cols$terminate())

    per_file <- lapply(seq_len(nrow(data_files)), function(i) {
      f <- data_files[i, ]
      pb_cols$tick(1, list(what = f$file_name))
      df <- data_read_head(f$file_location, n_rows = Inf)
      if (is.null(df) || ncol(df) == 0) {
        # An .RData/.rda that yielded no data frame is an analysis workspace,
        # not reusable shared data — record it for a sharing recommendation.
        if (tolower(tools::file_ext(f$file_name)) %in% c("rdata", "rda"))
          workspace_files <<- c(workspace_files, f$file_name)
        return(NULL)
      }

      # Skip a file manifest / table-of-contents disguised as tabular data:
      # its cells name other files in the repository (see data_is_manifest()).
      # Compare against every other file in the repo, not itself.
      other_files <- all_files$file_name[all_files$file_name != f$file_name]
      if (data_is_manifest(df, other_files)) {
        manifest_files <<- c(manifest_files, f$file_name)
        return(NULL)
      }

      # Describe each column as orthogonal facets (DDI-style) instead of a single
      # col_type: how it is stored (representation), its measurement level, what
      # it measures (concept), how it functions (role), its unit and data-quality
      # state. See data_col_facets() in data_check_helpers.R.
      cls <- lapply(seq_along(df), function(j) data_col_facets(names(df)[j], df[[j]]))

      # Skip a file that read as a data frame but is not a usable rectangular
      # dataset (a human coding worksheet: mostly free text and/or almost all
      # empty). No columns are extracted and nothing is sent to the LLM, but the
      # file stays classified as data so data_validate's spreadsheet-formatting
      # checks still check its formatting.
      usable <- .tabular_usable(cls, df)
      if (!isTRUE(usable$usable)) {
        non_tabular_files[[f$file_name]] <<- usable$reason
        return(NULL)
      }

      file_previews[[f$file_name]] <<- df

      getf <- function(field) vapply(cls, function(c) {
        v <- c[[field]]; if (is.null(v) || is.na(v)) NA_character_ else v
      }, character(1))
      is_numeric <- vapply(cls, function(c) isTRUE(c$is_numeric), logical(1))

      # Qualtrics response-metadata columns get a concept tag from the export's
      # reserved names (StartDate, Duration, Finished, ...), which is a stronger
      # signal than the value-based rules for those columns.
      concept <- getf("concept")
      file_is_qualtrics <- data_check_is_qualtrics(df)
      if (file_is_qualtrics) {
        qtags <- .qualtrics_tag_cols(names(df))
        concept[!is.na(qtags)] <- qtags[!is.na(qtags)]
      }
      # Qualtrics display-order (`<Q>_DO_<...>`) columns are export-only
      # randomisation metadata with no entry in the .qsf and no analytic value.
      # Marked here (only for genuine Qualtrics exports) so codebook_check can
      # exclude them from name-matching and from the LLM instead of treating
      # each as an unlabelled column. Non-Qualtrics files are never marked.
      qualtrics_display_order <- file_is_qualtrics &
        .qualtrics_is_display_order(names(df))

      # Analysis unit (DDI analysisUnit): what one row of this file represents
      # (person/trial/session/dyad), inferred from the identifier column(s) and
      # their uniqueness. A file-level property, so it is the same for every row.
      id_cols <- names(df)[getf("role") == "identifier"]
      au <- data_analysis_unit(df, id_cols)

      stats_mat <- do.call(rbind, lapply(seq_along(df), function(j) {
        c <- cls[[j]]
        x_for_stats <- c$numeric_values
        if (is.null(x_for_stats) && isTRUE(c$ambiguous) && isTRUE(c$is_numeric))
          x_for_stats <- df[[j]]
        data_col_stats(x_for_stats, df[[j]])
      }))

      sample_vals <- vapply(df, function(col) {
        v <- as.character(col[!is.na(col)])
        if (length(v) == 0) "" else paste(utils::head(v, 5), collapse = " | ")
      }, character(1))

      # Values data_read_head had to re-interpret as Latin-1 because their
      # bytes were not valid UTF-8 (see the "utf8_repaired" attribute): the
      # count per column, 0 when the column needed no repair. data_validate
      # turns nonzero counts into an encoding warning.
      rep_counts <- attr(df, "utf8_repaired") %||% integer(0)
      utf8_fixed <- ifelse(names(df) %in% names(rep_counts),
                           as.integer(rep_counts[names(df)]), 0L)

      data.frame(
        paper_id          = f$paper_id %||% .pid(all_files),
        repo_url          = f$repo_url,
        source_file       = f$file_name,
        group             = f$group %||% NA_character_,
        column_name       = names(df),
        representation    = getf("representation"),
        measurement_level = getf("measurement_level"),
        concept           = concept,
        role              = getf("role"),
        unit              = getf("unit"),
        quality           = getf("quality"),
        parse_note        = getf("parse_note"),
        analysis_unit     = au$unit %||% NA_character_,
        ambiguous         = vapply(cls, function(c) isTRUE(c$ambiguous), logical(1)),
        is_numeric        = is_numeric,
        is_qualtrics      = file_is_qualtrics,
        qualtrics_display_order = qualtrics_display_order,
        sample_values     = sample_vals,
        utf8_repaired     = utf8_fixed,
        stats_mat
      )
    })
    columns_df <- dplyr::bind_rows(Filter(Negate(is.null), per_file))

    # Header signature per source file: the sorted set of its column names. Files
    # that share a signature have the same schema (e.g. pp1.csv … pp30.csv), so a
    # column's LLM-derived concept/level is identical across them and need be
    # classified only once. A file with a different header (one_summary.csv) gets
    # its own signature and is classified on its own. Broadcasting per signature
    # collapses the repeated LLM questions in same-schema repositories.
    if (!is.null(columns_df) && nrow(columns_df) > 0) {
      sig_by_file <- vapply(
        split(columns_df$column_name, columns_df$source_file),
        function(cols) paste(sort(unique(cols)), collapse = "\r"),
        character(1))
      columns_df$header_sig <- unname(sig_by_file[columns_df$source_file])
    }

    # Demote any detected manifests to documentation (doc_role "supplemental")
    # and refresh the tabular flags/counts so they are reported as
    # documentation, not data.
    if (length(manifest_files) > 0) {
      mrows <- all_files$file_name %in% manifest_files
      all_files$data_type[mrows]   <- "documentation"
      all_files$doc_role[mrows]    <- "supplemental"
      all_files$data_format[mrows] <- NA_character_
      is_tabular_data <- all_files$data_type == "data" &
        !is.na(all_files$data_format) & all_files$data_format == "tabular"
      n_tabular_all <- sum(is_tabular_data)
      n_no_local    <- sum(is_tabular_data & !has_local)
    }

    # LLM tier (rules-first, LLM fills gaps): for columns whose *concept* the
    # rules left NA, ask the model what the column measures. Concepts under
    # cryptic names (q3 = a reaction time) are exactly what the rules cannot get.
    # The LLM also gives a measurement level, used only to fill an NA level.
    if (!is.null(columns_df) && nrow(columns_df) > 0 && llm_use()) {
      # Header-signature dedup: classify only the FIRST file of each schema
      # group (the representative) and broadcast its concept/level to the
      # identical-header files afterwards. In a repo of pp1.csv … pp30.csv this
      # turns 30 repeated LLM questions per ambiguous column into one.
      rep_file <- if ("header_sig" %in% names(columns_df)) {
        first_file <- tapply(columns_df$source_file, columns_df$header_sig,
                             function(x) x[[1]])
        columns_df$source_file == first_file[columns_df$header_sig]
      } else rep(TRUE, nrow(columns_df))

      gap_idx <- which((is.na(columns_df$concept) |
                          columns_df$ambiguous %in% TRUE) & rep_file)
      if (length(gap_idx) > 0) {
        col_text <- vapply(gap_idx, function(i) sprintf(
          "column_name: %s\nsample_values: %s\nis_numeric: %s",
          columns_df$column_name[[i]] %||% "",
          columns_df$sample_values[[i]] %||% "",
          ifelse(isTRUE(columns_df$is_numeric[[i]]), "TRUE", "FALSE")),
          character(1))

        # Concept classification.
        concept_levels <- c("reaction_time", "accuracy", "age", "gender",
                            "race", "likert", "condition", "id", "date",
                            "timestamp", "measure", "other")
        concept_prompt <- paste(
          "Each line describes one column of a psychology dataset. Say what the",
          "column MEASURES (its concept), independent of how it is stored. Use",
          "exactly one of:",
          paste(concept_levels, collapse = ", "), ".",
          "'likert' = a rating-scale item; 'measure' = a substantive numeric",
          "measurement with no more specific concept; 'other' when unsure.",
          "Return one {index, value} per numbered line, echoing its index."
        )
        pred_concept <- .llm_classify_batched(
          col_text, concept_prompt, value_desc = "Best single concept",
          valid = concept_levels, model = model, params = params,
          phase = "Classifying column concepts")
        if (is.na(llm_model_used))
          llm_model_used <- attr(pred_concept, "llm_model") %||% NA_character_
        # 'measure'/'other' are non-informative concepts → leave NA.
        fill <- !is.na(pred_concept) & !pred_concept %in% c("measure", "other") &
          is.na(columns_df$concept[gap_idx])
        if (any(fill)) {
          columns_df$concept[gap_idx[fill]] <- pred_concept[fill]
          llm_col_updates <- sum(fill)
        }

        # Measurement level for still-ambiguous numeric columns.
        lvl_idx <- gap_idx[columns_df$ambiguous[gap_idx] %in% TRUE &
                             is.na(columns_df$measurement_level[gap_idx])]
        if (length(lvl_idx) > 0) {
          lvl_text <- vapply(lvl_idx, function(i) sprintf(
            "column_name: %s\nsample_values: %s",
            columns_df$column_name[[i]] %||% "",
            columns_df$sample_values[[i]] %||% ""), character(1))
          lvl_levels <- c("nominal", "ordinal", "interval", "ratio")
          lvl_prompt <- paste(
            "Give the measurement level (Stevens) of each numeric column:",
            "nominal, ordinal, interval, or ratio. A coded category is nominal;",
            "a rating scale is ordinal; a count/magnitude is ratio.",
            "Return one {index, value} per numbered line, echoing its index.")
          pred_lvl <- .llm_classify_batched(
            lvl_text, lvl_prompt, value_desc = "Stevens measurement level",
            valid = lvl_levels, model = model, params = params,
            phase = "Classifying measurement levels")
          ok <- !is.na(pred_lvl)
          if (any(ok)) columns_df$measurement_level[lvl_idx[ok]] <- pred_lvl[ok]
        }

        # Broadcast the representative's LLM-filled concept/level to the other
        # files that share its header signature. Match on (header_sig,
        # column_name); a column keeps its own value when the rules already set
        # it (only NA cells are filled), so a file whose rules resolved a column
        # differently is never overwritten.
        if ("header_sig" %in% names(columns_df) && any(!rep_file)) {
          key <- paste(columns_df$header_sig, columns_df$column_name, sep = "\r")
          rep_key <- key[rep_file]
          for (facet in c("concept", "measurement_level")) {
            rep_val  <- columns_df[[facet]][rep_file]
            donor    <- rep_val[match(key, rep_key)]  # value from the rep file
            fill_row <- !rep_file & is.na(columns_df[[facet]]) & !is.na(donor)
            if (any(fill_row))
              columns_df[[facet]][fill_row] <- donor[fill_row]
          }
        }
      }
    }

    # Rules-only fallbacks for facets the rules could not resolve. Missing
    # representation → decide from is_numeric; missing measurement level on a
    # numeric column → ratio (a bare integer count), on a character column →
    # nominal. Concept legitimately stays NA (not every column has a named one).
    if (!is.null(columns_df) && nrow(columns_df) > 0) {
      rep_na <- is.na(columns_df$representation)
      columns_df$representation[rep_na & columns_df$is_numeric]  <- "numeric"
      columns_df$representation[rep_na & !columns_df$is_numeric] <- "text"
      columns_df$representation[is.na(columns_df$representation)] <- "text"
      lvl_na <- is.na(columns_df$measurement_level)
      columns_df$measurement_level[lvl_na & columns_df$representation == "numeric"] <- "ratio"
      columns_df$ambiguous <- NULL
      columns_df$is_numeric <- NULL
      columns_df$header_sig <- NULL   # internal dedup key, not part of output
    }
  }

  n_columns <- if (!is.null(columns_df)) nrow(columns_df) else 0L

  # Carry the non-tabular verdict into the file classification so downstream
  # modules can react: data_validate's spreadsheet-formatting checks still
  # inspect these for formatting but add a "not a rectangular dataset" note.
  # The column always exists (default TRUE) so consumers can rely on it.
  # non_tabular_files is name -> reason.
  all_files$tabular_usable <- TRUE
  all_files$non_tabular_reason <- NA_character_
  if (length(non_tabular_files) > 0) {
    nt <- all_files$file_name %in% names(non_tabular_files)
    all_files$tabular_usable[nt] <- FALSE
    all_files$non_tabular_reason[nt] <-
      unlist(non_tabular_files[all_files$file_name[nt]], use.names = FALSE)
  }

  # ── 5. Reporting ─────────────────────────────────────────────────────────────
  type_counts <- table(factor(all_files$data_type, levels = .data_check_types))

  summary_files <- sprintf(
    "We classified %d file%s: %s.",
    nrow(all_files), plural(nrow(all_files)),
    paste(sprintf("%d %s", type_counts[type_counts > 0],
                  names(type_counts)[type_counts > 0]), collapse = ", ")
  )

  n_extracted <- nrow(data_files) - length(manifest_files) -
    length(non_tabular_files)
  summary_data <- sprintf(
    "We found %d tabular data file%s and extracted %d column%s from %d of them.",
    n_tabular_all, plural(n_tabular_all),
    n_columns, plural(n_columns), n_extracted
  )

  # Study grouping, when the files split into studies. Purely descriptive: how
  # many studies the files fall into and what they are called. Every file
  # except the collection-level root README/ro-crate-metadata.json resolves to
  # exactly one study — there is no "shared" bucket — so files reused across
  # studies are reported separately, via referenced_by, rather than as a count
  # of ungrouped files.
  study_grp <- if ("group" %in% names(all_files))
    all_files$group else rep(NA_character_, nrow(all_files))
  studies <- unique(study_grp[!is.na(study_grp)])
  n_root <- sum(is.na(study_grp) & !is.na(all_files$doc_role) &
                 all_files$doc_role == "readme")
  n_reused <- if ("referenced_by" %in% names(all_files))
    sum(lengths(all_files$referenced_by) > 0) else 0L
  summary_studies <- if (length(studies) > 0) {
    sprintf(
      "We grouped the files into %d study group%s (%s)%s%s.",
      length(studies), plural(length(studies)),
      paste(sort(studies), collapse = ", "),
      if (n_root > 0) sprintf(", plus %d collection-level file%s (README/ro-crate metadata)",
                              n_root, plural(n_root)) else "",
      if (n_reused > 0) sprintf(", and %d file%s reused across studies (referenced, not duplicated)",
                                n_reused, plural(n_reused)) else ""
    )
  } else character(0)

  # Files the study-group model never answered for, even after retrying in
  # smaller batches — an intermittent provider failure, not a real verdict. Say
  # so, so a silently degraded grouping cannot be mistaken for a confident one.
  summary_ungrouped <- if (length(group_unresolved) > 0) {
    sprintf(paste0("The study-group step did not get an answer for %d file%s ",
                   "(the model request failed); %s grouped by the fallback rules ",
                   "instead."),
            length(group_unresolved), plural(length(group_unresolved)),
            if (length(group_unresolved) == 1) "it was" else "they were")
  } else character(0)

  # Repositories refused by the download size caps. Each message already names
  # the parameter and the value to lift it, so we surface them verbatim.
  gate_msgs <- if (!is.null(gated_repos)) gated_repos$message else character(0)
  summary_omitted <- if (length(gate_msgs) > 0)
    paste(gate_msgs, collapse = "\n\n") else NULL

  # Tabular data files with no readable copy (download off, no URL, or fetch
  # failed). These are counted but not column-extracted.
  summary_nolocal <- if (n_no_local > 0) sprintf(
    paste0("%d tabular data file%s could not be read because %s not downloaded. ",
           "%s"),
    n_no_local, plural(n_no_local),
    if (n_no_local == 1) "it was" else "they were",
    if (download != "none")
      "This can happen for private repositories or failed downloads; you can also pass `local_path` to point at a local copy."
    else
      "Set `download = \"data\"`, or pass `local_path` to point at a local copy, to analyse them."
  ) else NULL

  # .RData/.rda workspaces with no reusable data: a sharing recommendation.
  n_workspace <- length(unique(workspace_files))
  summary_workspace <- if (n_workspace > 0) sprintf(
    paste0("%d R workspace file%s (`.RData`/`.rda`) contain%s no reusable ",
           "tabular data — only fitted models or saved session objects. Such ",
           "files need R (and the exact packages used) to open and are not ",
           "machine-readable. Share the underlying data as CSV (or a documented ",
           "`.sav`/`.dta`) with a codebook so it can be reused without R: %s."),
    n_workspace, plural(n_workspace),
    if (n_workspace == 1) "s" else "",
    paste(unique(workspace_files), collapse = ", ")
  ) else NULL

  # Files that read as a table but are not usable rectangular datasets (coding
  # worksheets: mostly free text and/or almost all empty). Columns were not
  # extracted; recommend sharing the real data as CSV + codebook. The file's
  # own reason is quoted so the author sees which signal fired.
  n_nontab <- length(non_tabular_files)
  summary_nontabular <- if (n_nontab > 0) sprintf(
    paste0("%d file%s read as a table but %s not a usable rectangular dataset ",
           "(%s). No columns were extracted and they were not sent to the LLM. ",
           "Share the underlying data as a plain rectangular table (one header ",
           "row, one column per variable) with a codebook: %s."),
    n_nontab, plural(n_nontab),
    if (n_nontab == 1) "is" else "are",
    paste(sprintf("%s: %s", names(non_tabular_files),
                  unlist(non_tabular_files, use.names = FALSE)),
          collapse = "; "),
    paste(names(non_tabular_files), collapse = ", ")
  ) else NULL

  # Trial-level files (E-Prime / Inquisit / jsPsych / Behaverse) held out of the
  # per-file tabular extraction: they are per-participant fragments of an
  # instrument, merged per instrument into Behaverse paradata/<instrument>.json by
  # convert_psychds() rather than reported as many separate datasets.
  n_trial_level <- length(unique(trial_level_files))
  summary_trial_level <- if (n_trial_level > 0) sprintf(
    paste0("%d trial-level data file%s (E-Prime / Inquisit / jsPsych / Behaverse) ",
           "%s recognised. These are per-participant records of a behavioural task, ",
           "so they are not listed as separate datasets — convert_psychds() merges ",
           "them per instrument into Behaverse `paradata/<instrument>.json` (one ",
           "file per instrument, all participants). Nothing is deleted."),
    n_trial_level, plural(n_trial_level),
    if (n_trial_level == 1) "was" else "were"
  ) else NULL

  # file inventory table
  file_tbl <- all_files |>
    dplyr::count(Type = data_type, name = "Files") |>
    dplyr::arrange(dplyr::desc(.data$Files))

  report <- c(
    "This module classifies repository files and, for tabular data files available locally, extracts each column's type and summary statistics.",
    "#### File Types",
    scroll_table(file_tbl, maxrows = 10)
  )

  tree_block <- repo_tree_block(all_files, tree_naming_issues)
  if (!is.null(tree_block)) {
    report <- c(report, tree_block)
  }

  # Raw data preview, one tab per tabular data file.
  if (length(file_previews) > 0) {
    report <- c(
      report,
      "#### Data Files",
      data_file_tabset(file_previews)
    )
  }

  if (n_columns > 0) {
    # Descriptives overview, one table per source file. columns_df already
    # carries source_file, so split there.
    desc_all <- columns_df |>
      dplyr::transmute(
        .data$source_file,
        Column = .data$column_name,
        Representation = .data$representation,
        Level = dplyr::coalesce(.data$measurement_level, ""),
        Concept = dplyr::coalesce(.data$concept, ""),
        Role = dplyr::coalesce(.data$role, ""),
        Unit = dplyr::coalesce(.data$unit, ""),
        Rows = .data$n + .data$n_missing,
        `% Missing` = dplyr::if_else(
          (.data$n + .data$n_missing) > 0,
          round(100 * .data$n_missing / (.data$n + .data$n_missing), 1),
          NA_real_
        ),
        `N Unique` = .data$n_unique,
        Mean = round(.data$mean, 3),
        SD = round(.data$sd, 3),
        Min = round(.data$min, 3),
        Max = round(.data$max, 3)
      ) |>
      dplyr::arrange(dplyr::desc(.data$`% Missing`), .data$Column)

    n_missing_cols <- sum(!is.na(columns_df$n_missing) & columns_df$n_missing > 0)
    # Fully empty (all-NA) columns: no observed values at all. Flagged like
    # data_validate's spreadsheet "empty columns" check — they carry no
    # information, do not survive a meaningful analysis, and are documented
    # (but marked empty) in a Psych-DS export rather than silently dropped.
    empty_cols <- if ("quality" %in% names(columns_df))
      sum(tolower(columns_df$quality %||% "") == "empty", na.rm = TRUE) else 0L
    empty_note <- if (empty_cols > 0)
      sprintf(" %d column%s %s empty (no values in any row); store data without empty columns.",
              empty_cols, plural(empty_cols),
              if (empty_cols == 1) "is" else "are")
    else ""
    report <- c(
      report,
      sprintf(
        "#### Descriptives Overview\n\nWe found %d column%s with at least one missing value.%s",
        n_missing_cols, plural(n_missing_cols), empty_note
      ),
      desc_file_tabset(desc_all)
    )

    # Analysis unit (DDI analysisUnit): one row per data file. Flag when a repo
    # mixes units (e.g. a person-level and a trial-level file), which changes how
    # the data must be analysed and is a common source of confusion.
    if ("analysis_unit" %in% names(columns_df)) {
      au_tbl <- columns_df |>
        dplyr::filter(!is.na(.data$analysis_unit)) |>
        dplyr::distinct(.data$source_file, .data$analysis_unit)
      if (nrow(au_tbl) > 0) {
        units <- sort(unique(au_tbl$analysis_unit))
        mixed <- length(units) > 1
        au_show <- au_tbl |>
          dplyr::transmute(File = .data$source_file,
                           `Unit of observation` = .data$analysis_unit)
        report <- c(report, "#### Unit of Observation",
          sprintf("We inferred what one row of each data file represents (person, trial, session, or dyad). %s",
                  if (mixed)
                    sprintf("**This repository mixes units of observation (%s)** — check that files at different levels (e.g. one row per participant vs. one row per trial) are not combined without aggregation.",
                            paste(units, collapse = ", "))
                  else sprintf("All files are at the **%s** level.", units[1])),
          scroll_table(au_show, maxrows = 20))
      }
    }
  }

  if (llm_use()) {
    n_groups <- length(unique(stats::na.omit(all_files$group)))
    llm_text <- sprintf(
      "%sreviewed ambiguous cases (%d file%s, %d column%s) and assigned study groups (%d study group%s detected).",
      if (!is.na(llm_model_used)) sprintf("LLM model '%s' ", llm_model_used) else "LLM ",
      llm_file_updates, plural(llm_file_updates),
      llm_col_updates, plural(llm_col_updates),
      n_groups, plural(n_groups)
    )
    report <- c(report, llm_text)
  }

  # ── 6. traffic light + summary table ─────────────────────────────────────────
  tl <- if (n_tabular_all == 0) "na"
        else if (n_no_local > 0) "yellow"
        else "green"

  # per-paper representation counts (wide). Representation is the primary
  # structural facet; concept/level are reported in the descriptives table.
  coltype_wide <- if (n_columns > 0) {
    columns_df |>
      dplyr::count(paper_id, representation) |>
      tidyr::pivot_wider(names_from = representation, values_from = n,
                         names_prefix = "col_", values_fill = 0)
  } else {
    data.frame(paper_id = .pid(all_files))
  }

  empty_col_n <- if (n_columns > 0 && "quality" %in% names(columns_df))
    sum(tolower(columns_df$quality %||% "") == "empty", na.rm = TRUE) else 0L
  summary_table <- data.frame(
    paper_id = .pid(all_files),
    data_file_n = n_tabular_all,
    column_n = n_columns,
    empty_col_n = empty_col_n
  ) |>
    dplyr::left_join(coltype_wide, by = "paper_id")

  summary_text <- c(summary_files, summary_data, summary_studies,
                     summary_ungrouped, summary_nolocal, summary_omitted,
                     summary_workspace, summary_nontabular, summary_trial_level) |>
    paste("\n- ", x = _, collapse = "")

  # ── 6b. Optional file manifest ───────────────────────────────────────────────
  # Persist the full file list (URLs, sizes, types) with the download outcome per
  # file, so a corpus can be audited (what exists vs what was fetched) and a data
  # archive rebuilt without re-querying every repository.
  manifest_path <- NULL
  if (!is.null(manifest)) {
    manifest_path <- .data_check_write_manifest(
      manifest, all_files, want, gated_repos,
      paper_id = .pid(all_files), download = download,
      max_file_size = max_file_size, max_download_size = max_download_size,
      skip_types = skip_types,
      oversize = oversize_files, failed = failed_files,
      zip_peek = zip_peek_reason, model = model)
  }

  # ── 7. Return ────────────────────────────────────────────────────────────────
  list(
    table = columns_df %||% data.frame(),
    structure = all_files,          # per-file classification, for codebook_check
    previews = file_previews,       # full read data frames, for data_validate
    gated_repos = listing_gated,    # repos found but not listable (size gate, ...)
    manifest_path = manifest_path,  # full manifest path, when one was written
    # TRUE when no file's path/repository/code-reference/LLM evidence named a
    # real study anywhere in this repository — every group came from the
    # blanket "ex1" default rather than actual evidence. psychds_check surfaces
    # this as a warning instead of implying real structure was detected.
    group_no_evidence = group_no_evidence,
    summary_table = summary_table,
    na_replace = c(data_file_n = 0, column_n = 0, empty_col_n = 0),
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}
