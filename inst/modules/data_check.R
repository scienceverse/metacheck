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
#' concepts and measurement levels the rules left blank. Qualtrics
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
#' <validation>This module has not been validated. All checks in the data_check module have unknown error rates. Carefully evaluate the output of this module. You can help improve this module by reporting an issue on GitHub.</validation>
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
#' @param manifest optional path to write a per-paper file manifest as JSON: the
#'   full list of repository files with their download URL, size, type, Psych-DS
#'   target path, and whether each was downloaded (and if not, why). A directory
#'   path writes `<paper_id>.manifest.json` inside it; a path ending in `.json`
#'   is used verbatim. `NULL` (the default) writes nothing. `"."` writes to the working folder. Useful for auditing a
#'   corpus (what exists, what was fetched) and for building a data archive.
#' @param plot_distributions if TRUE, draw a distribution plot for each numeric
#'   column and embed it in the report. Off by default because it is the only
#'   part of this module that renders images, and a wide repository produces a
#'   great many of them.
#' @param max_facets the most distribution panels to draw in one plot when
#'   `plot_distributions = TRUE`; columns beyond this are not plotted.
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
                       manifest = NULL,
                       plot_distributions = FALSE,
                       max_facets = .dv_max_facets,
                       model = llm_model(),
                       params = list()) {

  # Normalise `download` to one of "none" / "data" / "all". Accept the legacy
  # logical form (TRUE = "data", FALSE = "none").
  download <- if (isTRUE(download)) "data"
              else if (isFALSE(download)) "none"
              else match.arg(as.character(download), c("data", "all", "none"))

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
      # A tree has to READ as a tree: every line visible at once, in order,
      # with the branch characters lining up. A table breaks that — it splits
      # the drawing into cells and pages through it, so a folder can end up on
      # a different page from its contents. So the rows are laid out as text
      # and emitted as a preformatted block, with the per-file columns padded
      # to align. Widths are computed from the content rather than fixed, so
      # a deep tree or a long type name still lines up.
      type_txt <- rep("", nrow(rows))
      type_txt[is_leaf] <- paste(
        .tree_type_icon(data_type[rows$leaf_idx[is_leaf]], doc_role[rows$leaf_idx[is_leaf]]),
        .tree_type_label(data_type[rows$leaf_idx[is_leaf]], doc_role[rows$leaf_idx[is_leaf]])
      )
      grp <- group[rows$leaf_idx[is_leaf]]
      study_txt <- rep("", nrow(rows))
      study_txt[is_leaf] <- ifelse(is.na(grp), "", grp)
      nm <- naming[rows$leaf_idx[is_leaf]]
      naming_txt <- rep("", nrow(rows))
      naming_txt[is_leaf] <- ifelse(is.na(nm), "", nm)

      # Pad to the widest entry in each column. formatC() counts characters,
      # and an emoji is one character here, so the icon does not throw the
      # alignment off in a monospaced block.
      pad <- function(x, extra = 2) {
        w <- max(nchar(x, type = "chars"), 0) + extra
        formatC(x, width = -w, flag = " ")
      }
      has_study  <- any(nzchar(study_txt))
      has_naming <- any(nzchar(naming_txt))
      # Every column is padded the same way. Trailing space on the last one is
      # stripped below, so padding it too costs nothing and keeps the columns
      # evenly separated whichever ones are present.
      lines <- paste0(
        pad(rows$text),
        pad(type_txt),
        if (has_study) pad(study_txt) else "",
        if (has_naming) naming_txt else ""
      )
      lines <- sub("\\s+$", "", lines)

      list(repo = repo, lines = lines,
           has_study = has_study, has_naming = has_naming)
    })
    blocks <- Filter(Negate(is.null), blocks)
    if (length(blocks) == 0) return(NULL)

    sections <- lapply(blocks, function(b) {
      # A fenced block with no language, so Quarto renders it verbatim in a
      # monospaced font and does not try to highlight it as code.
      #
      # The whole block is ONE element, with its lines joined by single
      # newlines. module_report() joins the report vector with blank lines
      # between elements, which is right between paragraphs but would put an
      # empty row between every line of the tree.
      c(paste0("**Repository: ", b$repo, "**"),
        paste(c("```", b$lines, "```"), collapse = "\n"))
    })

    legend <- paste0(
      "Each file is followed by how it was classified",
      if (any(vapply(blocks, function(b) b$has_study, logical(1))))
        ", which study it belongs to" else "",
      if (any(vapply(blocks, function(b) b$has_naming, logical(1))))
        ", and any file-naming issue found by repo_check" else "",
      ".")

    c("#### Data Tree",
      paste("Where the files sit within each repository.", legend),
      unlist(sections))
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
                       local_only = local_only)
    } else {
      mo <- module_run(paper, "repo_check", local_only = local_only)
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

      # Qualtrics writes TWO extra rows under the header — the full question
      # text, then a {"ImportId":...} JSON row — before any real response. Left
      # in place they make every column a character vector of prose, so a
      # rating item types as text rather than an ordinal scale. Strip them
      # before anything reads the values. Verified on a real 139-column export:
      # without this a 25-item rating battery (british_ratings_1..25) typed as
      # `ratio` with concept NA; with it, `ordinal` / `likert`.
      if (data_check_is_qualtrics(df)) df <- data_strip_qualtrics_header(df)

      # Which columns form a rating-scale block (a run of consecutive
      # same-prefix columns sharing a response range)? Computed once per file
      # here, because it needs the whole data frame — a single column cannot
      # tell a rating item from a trial counter. This is what makes a column
      # `likert` in data_col_facets() below.
      scale_cols <- names(df)[unlist(.detect_scale_blocks(df), use.names = FALSE)]

      # Describe each column as orthogonal facets (DDI-style) instead of a single
      # col_type: how it is stored (representation), its measurement level, what
      # it measures (concept), how it functions (role), its unit and data-quality
      # state. See data_col_facets() in data_check_helpers.R.
      cls <- lapply(seq_along(df), function(j)
        data_col_facets(names(df)[j], df[[j]],
                        in_scale_block = names(df)[j] %in% scale_cols))

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

        # NOTE: measurement level is NOT sent to the LLM. It used to be, for
        # every column flagged `ambiguous` with no level — 935 of 3176 columns
        # (29%) on a 120-file sample, the largest LLM pass in this module. Two
        # reasons it went:
        #
        #   * `ambiguous` does not mean "numeric but unclear level", so the
        #     prompt ("give the measurement level of each NUMERIC column") was
        #     being handed columns like `object_label` (Wine, Hammock,
        #     Binoculars), `event` (onload, subject, mouse) and `ll_amt` (the
        #     literal string "NULL") — none of which parse as numbers at all.
        #   * Its only consumer is convert_psychds(), which writes
        #     `metacheck:measurementLevel` — a metacheck namespace EXTENSION,
        #     not a Psych-DS field — guarded by `if (!is.na(lvl))`. An unknown
        #     level simply omits the property. No check reads it, no report
        #     shows it, no validator requires it.
        #
        # The rules set a level for the columns where one is actually
        # determinable (see .coltype_to_facets() and the concept-implied and
        # non-numeric rules in data_col_facets()); the rest stay NA, which the
        # converter already handles.

        # Broadcast the representative's concept (LLM-filled) and level
        # (rule-derived) to the other
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

  report <- c(
    "This module classifies repository files and, for tabular data files available locally, extracts each column's type and summary statistics."
  )

  # The data tree replaces the former "File Types" count table: it lists every
  # file with the same classification, so a separate per-type tally was a less
  # informative view of information already shown here.
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


  # ══ DATA VALIDATION ══════════════════════════════════════════════════════════
  # Everything below was the `data_validate` module. It is here rather than in a
  # separate module because it never opened a file of its own: it worked entirely
  # on the `previews` this function had already read, so the split was a module
  # boundary with no work behind it. `columns_df`, `previews` and `structure_df`
  # are the same objects the separate module used to re-fetch via
  # get_prev_outputs("data_check", ...).
  previews     <- file_previews
  structure_df <- all_files
  # Codebook output, used ONLY for cosmetic enrichment below: a documented
  # column label in the findings table, and a named scale ("PANAS" rather than
  # the bare variable prefix) on a careless-responding block. Both fall back
  # cleanly when it is absent.
  #
  # It is normally absent. codebook_check reads THIS module's output, so in the
  # pipeline order (repo_check -> data_check -> codebook_check) this fetch
  # returns NULL and the fallbacks are what run. Nothing here depends on it,
  # which is why that is acceptable — but no CHECK may be built on it. The
  # out-of-range check that used to be here was: it silently ran against an
  # inferred range instead of the documented one, so it has moved to
  # codebook_check, where the codebook is actually available.
  labels_df <- get_prev_outputs("codebook_check", "table")

  # ── 1b. Spreadsheet formatting checks (.xlsx/.xls/.ods/.fods) ──────────────
  # Runs on the RAW workbook file via structure_df$file_location, independently
  # of whether data_check could extract a clean `preview` from it: a merged
  # banner cell or an offset header is often exactly why extraction failed, so
  # this must not depend on previews existing. See .dv_spreadsheet_findings()
  # below (ported from the former spreadsheet_check module) for what each check
  # covers (colour coding, merged cells, empty rows/columns, offset header,
  # non-rectangular data, un-inspectable legacy .xls).
  spreadsheet_result <- .dv_spreadsheet_findings(structure_df)
  spreadsheet_findings_df <- spreadsheet_result$findings
  n_spreadsheet_files <- spreadsheet_result$n_files

  empty <- function(text) {
    n_flagged_files_spreadsheet <- length(unique(spreadsheet_findings_df$source_file))
    list(
      table = spreadsheet_findings_df,
      summary_table = data.frame(
        # .pid() takes a single argument (files = NULL) -- this call
        # passed a second, unmatched argument (structure_df), which made
        # every call crash with "unused argument (structure_df)"
        # whenever this branch was reached (any paper where data_check
        # found zero readable tabular files: previews empty, whether from
        # messy/non-rectangular spreadsheets or no tabular data at all).
        # Introduced in 21a6c28 (2026-08-11); the correct one-argument
        # form, .pid(columns_df), was already used seven lines below in
        # that same commit.
        paper_id = .pid(columns_df), column_n = 0, flagged_n = 0,
        spreadsheet_file_n = n_spreadsheet_files,
        spreadsheet_flagged_file_n = n_flagged_files_spreadsheet),
      na_replace = c(column_n = 0, flagged_n = 0,
                     spreadsheet_file_n = 0, spreadsheet_flagged_file_n = 0),
      traffic_light = if (nrow(spreadsheet_findings_df) > 0) "yellow" else "na",
      dv_summary_text = if (nrow(spreadsheet_findings_df) > 0)
        paste(text, .dv_spreadsheet_summary_text(spreadsheet_findings_df, n_spreadsheet_files))
      else text,
      dv_report = if (nrow(spreadsheet_findings_df) > 0)
        .dv_spreadsheet_report(spreadsheet_findings_df, n_spreadsheet_files)
      else NULL
    )
  }

  if (is.null(previews) || length(previews) == 0)
    return(empty("We found no readable tabular data files to validate."))

  # Label lookup (source_file + column_name → documented label).
  label_of <- function(file, col) {
    if (is.null(labels_df) || !all(c("source_file", "column_name", "label")
                                   %in% names(labels_df))) return(NA_character_)
    hit <- labels_df$source_file == file & labels_df$column_name == col &
      labels_df$label_status %in% c("labelled", "llm")
    if (any(hit)) labels_df$label[which(hit)[1]] else NA_character_
  }

  # Values whose bytes were not valid UTF-8 and had to be re-interpreted as
  # Latin-1 at read time (recorded by data_check per column; the repaired
  # preview values no longer show it). 0 when absent or from an older
  # data_check run without the column.
  utf8_repaired_of <- function(file, col) {
    if (is.null(columns_df) || !all(c("source_file", "column_name",
                                      "utf8_repaired") %in% names(columns_df)))
      return(0L)
    hit <- columns_df$source_file == file & columns_df$column_name == col
    if (any(hit)) columns_df$utf8_repaired[which(hit)[1]] %||% 0L else 0L
  }

  # ── 2. Run checks per column ─────────────────────────────────────────────────
  findings <- list()
  plot_specs    <- list()   # per numeric column: values + bounds for the distribution facet
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) == 0) next
    # Names that become identical once special characters are sanitized away
    # (computed per file: collisions are between sibling columns).
    coll <- data_check_colname_collisions(names(df))
    for (col in names(df)) {
      x <- df[[col]]
      lbl <- label_of(file, col)
      col_finds <- list()

      if (is.numeric(x)) {
        # Normalize numeric vectors to base doubles so integer64 columns from
        # fread/readxl do not cause type conflicts in downstream bind_rows().
        x_num <- suppressWarnings(as.numeric(x))
        # MOVED to codebook_check: "values outside the valid range" is a
        # codebook-vs-data comparison ("the codebook says 1-5, but the column
        # contains a 6"), so it belongs where the codebook is read.
        #
        # It used to run here against codebook ground truth fetched with
        # get_prev_outputs("codebook_check", "table") — but codebook_check reads
        # data_check's own output, so in the normal pipeline order that fetch
        # always returned NULL and the ground truth was never available. What
        # actually ran was the INFERRED-scale fallback, which guesses a column's
        # range from its own values. Measured over 1286 real numeric columns
        # that fallback fired once, so little is lost by removing it — and what
        # remains in codebook_check is a check that can state its evidence:
        # a documented range, and the values that violate it.
        # DISABLED: the distribution figure used to carry dashed Tukey fence
        # lines as visual context. They are not drawn, because a fence line
        # implies the values beyond it are suspect, and on most real numeric
        # columns that implication is wrong.
        #
        # Measured over 1276 numeric columns from 120 cached repositories: the
        # fence puts 25.7% of columns outside it somewhere, and 144 columns have
        # more than 5% of their values beyond it (up to 25%). Median absolute
        # skewness is 1.44 among those columns versus 0.08 among the rest — the
        # fence tracks SKEW, not anomalies. The recurring cases are reaction
        # times (`rt`, `startRT`, `resp_*.rt`, `time`), which are lognormal so
        # the long right tail is the expected shape; identifiers (`ID#`: 157
        # distinct values, 37 beyond the fence), which are not measurements at
        # all; and MCMC posterior draws, where the tails are the result.
        #
        # data_check_outliers() is correct Tukey and is KEPT, both as an
        # exported function and in .dv_careless_block(), where it is applied to
        # the IRV index — a bounded within-person statistic, not a raw
        # measurement. What does not work is applying it blind to any numeric
        # column. Restore only with a gate on `role`/`concept` (never for
        # role == "identifier"; log-transform or skip concept ==
        # "reaction_time").
        #
        # Nothing is lost from the report: out-of-range values come from
        # data_check_scale_values() below, which only applies to columns with a
        # DETECTED BOUNDED SCALE — where a stray value really is an error.
        # o <- data_check_outliers(x_num, k = outlier_k)
        v <- x_num[!is.na(x_num) & !is.nan(x_num)]
        if (length(v) >= 4 && length(unique(v)) > 1) {
          plot_specs[[length(plot_specs) + 1L]] <- list(
            file = file, col = col, values = utils::head(v, 5000),
            lower = NA_real_, upper = NA_real_)
        }
      } else {
        # A mostly-numeric column stored as text is really a contaminated
        # numeric column, not a genuine categorical — so if that fires, skip
        # the case check, which would treat every distinct number as a
        # spurious "level".
        nt <- data_check_numeric_in_text(x)
        if (nt$problem) {
          col_finds[["Numeric as text"]] <- nt$message
        } else {
          cc <- data_check_case_issues(x)
          if (cc$problem) col_finds[["Case issues"]] <- cc$message
        }
        ws <- data_check_whitespace(x)
        if (ws$problem) col_finds[["Whitespace"]] <- ws$message
      }
      # An all-missing column (a variable that never recorded anything) is
      # always flagged — that is unambiguous.
      #
      # CONSTANT columns are DISABLED. Measured over 3112 real columns from 120
      # cached repositories, data_check_constant() fired on 20.1% (487 exactly
      # constant, 138 near-constant), and the flags are dominated by columns
      # that are constant BY DESIGN:
      #
      #   * per-participant trial files — one file per person, one row per
      #     trial, so `subject_ID`, `Age`, `Gender`, `Nationality` are constant
      #     because they describe the one participant the file is about. This
      #     is the largest group: 36 of 120 files had more than 30% of their
      #     columns flagged, the worst 81 of 133. Flagging `Age` in a file that
      #     is definitionally one person's data is simply wrong.
      #   * run machinery — `test_loop.thisRepN`, `resp_loop.ran`, `expName`,
      #     `task_version`, `key_press`, `success`.
      #   * placeholders — `undefined`, `NULL`, `Anonymized`.
      #
      # Only the third group is arguably worth reporting (a column that is
      # entirely "undefined" carries no data), and the tiering below could not
      # separate it: whether a constant column is a problem depends on the
      # FILE (is it one participant's trials?), not on the column. data_check
      # already knows that — it marks such files `data_format = "trial_level"`
      # — so a future version could skip those and keep the check elsewhere,
      # or report only placeholder-valued columns.
      #
      # data_check_constant() itself is correct and is KEPT as an exported
      # function.
      emp <- data_check_empty(x)
      if (emp$problem) {
        col_finds[["Empty column"]] <- emp$message
      }
      # cst <- data_check_constant(x)
      # if (cst$problem) {
      #   if (data_check_design_name(col)) {
      #     col_finds[["Constant"]] <- paste(cst$message,
      #       "The name suggests a design/condition variable; if the study had more than one condition, the file may have been filtered before export.")
      #   } else if (!cst$near && is.numeric(x)) {
      #     col_finds[["Constant"]] <- cst$message
      #   } else if (!cst$near) {
      #     meta_const_specs[[length(meta_const_specs) + 1L]] <- data.frame(
      #       source_file = file, column = col, value = cst$values[[1]])
      #   }
      # }
      # An SPSS "Select Cases" filter variable matters whether or not it is
      # constant: constant-1 means the file is a pre-filtered subset; varying
      # means analyses likely used only the selected rows.
      fl <- data_check_spss_filter(col, x)
      if (fl$problem) col_finds[["SPSS filter variable"]] <- fl$message

      # Column-name quality: names with file-illegal characters, control
      # characters, padding, or excessive length break downstream reuse (figure
      # file names, scripts, metadata keys) and often signal a misparsed header.
      cn <- data_check_colname(col)
      if (cn$problem) col_finds[["Problematic column name"]] <- cn$message
      if (!is.null(coll[[col]]))
        col_finds[["Colliding column names"]] <- coll[[col]]

      # Mixed / legacy encoding: values whose bytes were not valid UTF-8 at
      # read time. metacheck re-interpreted them as Latin-1 to continue, but
      # on other systems these characters silently corrupt (é becomes "Ã©" or
      # "�"), so the researcher should re-save the file as UTF-8.
      n_enc <- utf8_repaired_of(file, col)
      if (!is.na(n_enc) && n_enc > 0) {
        col_finds[["Mixed encoding"]] <- sprintf(
          "%d value%s %s not valid UTF-8 (bytes from a legacy encoding such as Latin-1/Windows-1252, e.g. a mis-encoded accent or apostrophe); metacheck re-interpreted %s as Latin-1 to continue. Such characters corrupt silently when the file is opened on another system. Re-save the file with UTF-8 encoding: in Excel use 'Save As > CSV UTF-8 (Comma delimited)'; in R, write.csv(..., fileEncoding = \"UTF-8\"); in SPSS, 'Save as type: CSV' with Encoding 'UTF-8'.",
          n_enc, plural(n_enc), if (n_enc == 1) "is" else "are",
          if (n_enc == 1) "it" else "them")
      }

      # Personal / disclosure information (PII): flag columns that may hold
      # data that should not be shared openly. Reported as "review before
      # sharing" prompts, never echoing the matching value.
      pv <- data_check_pii_values(x)
      if (pv$problem) col_finds[["Personal info (values)"]] <- pv$message
      pn <- data_check_pii_name(col)
      if (pn$problem) col_finds[["Personal info (column name)"]] <- pn$message
      # sibling_names lets the geo check require a partner column: a latitude is
      # only a coordinate when a longitude sits beside it, which is what tells a
      # real `lat` apart from latency or a lateralisation index.
      pg <- data_check_pii_geo(col, x, sibling_names = names(df))
      if (pg$problem) col_finds[["Geographic coordinates"]] <- pg$message
      if (!is.numeric(x)) {
        pf <- data_check_pii_freetext(x)
        if (pf$problem) col_finds[["Free-text (may hold PII)"]] <- pf$message
      }

      for (chk in names(col_finds)) {
        findings[[length(findings) + 1L]] <- data.frame(
          source_file = file, column = col,
          label = lbl, check = chk, detail = col_finds[[chk]]
        )
      }
    }
  }
  # ── 2a. Demographic columns (age / gender / race) ───────────────────────────
  # Report which files contain the demographic variables almost every study
  # collects. Prefer data_check's precomputed `concept` facet (name+value
  # agreement); fall back to computing it here from the previews so this works
  # even against an older data_check run whose table predates the facet.
  demo_specs <- list()
  demo_tagged <- !is.null(columns_df) &&
    all(c("source_file", "column_name", "concept") %in% names(columns_df))
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) == 0) next
    for (col in names(df)) {
      kind <- if (demo_tagged) {
        hit <- columns_df$source_file == file & columns_df$column_name == col
        v <- if (any(hit)) columns_df$concept[which(hit)[1]] else NA_character_
        if (is.na(v) || !nzchar(v)) NA_character_ else v
      } else {
        data_check_demographic(col, df[[col]])
      }
      # The `concept` facet also carries non-demographic concepts (reaction_time,
      # likert, Qualtrics tags, ...); keep only the demographic ones here.
      if (!is.na(kind) && !kind %in% c("age", "gender", "race"))
        kind <- NA_character_
      if (!is.na(kind))
        demo_specs[[length(demo_specs) + 1L]] <- data.frame(
          source_file = file, column = col, demographic = kind)
    }
  }
  demo_df <- if (length(demo_specs) > 0) dplyr::bind_rows(demo_specs) else
    data.frame(source_file = character(0), column = character(0),
               demographic = character(0))

  # ── 2a2. Qualtrics survey metadata ──────────────────────────────────────────
  # For each file that is a Qualtrics export, summarise the things reliably
  # extractable from its response-metadata columns: how many rows are previews /
  # unfinished (and should likely be dropped), the completion-time distribution
  # (with implausibly fast responses), the data-collection window, and which
  # Qualtrics PII fields are present. Nothing here interprets the substantive
  # question columns — that is out of scope.
  qualtrics_specs <- list()
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) == 0) next
    if (!data_check_is_qualtrics(df)) next
    s <- .dv_qualtrics_summary(df)
    if (!is.null(s)) {
      s$source_file <- file
      qualtrics_specs[[length(qualtrics_specs) + 1L]] <- s
    }
  }

  # ── 2b. Careless responding (survey data only) ──────────────────────────────
  # For files that contain a block of Likert-type items AND an identifier column,
  # run careless-response indices (longstring + IRV) per scale block and flag the
  # respondents that look careless. Needs the `careless` package; skipped (with a
  # note) when it is not installed. Findings feed the same per-column tally as a
  # "Careless responding" check, one row per (file, flagged respondent).
  careless_specs <- list()   # per flagged respondent, for the dv_report table
  careless_note  <- NULL
  careless_avail <- requireNamespace("careless", quietly = TRUE)
  n_careless_files <- 0L
  # Files actually screened vs. passed over (too few rows/columns, no scale
  # block, or no identifier). Reported so that "nothing flagged" is not read as
  # "nothing there" — see .dv_careless_coverage_text().
  careless_scored  <- 0L
  careless_skipped <- 0L
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) < .dv_careless_min_items ||
        nrow(df) < .dv_careless_min_rows) {
      careless_skipped <- careless_skipped + 1L; next
    }
    blocks <- .detect_scale_blocks(df)
    if (length(blocks) == 0) { careless_skipped <- careless_skipped + 1L; next }

    # An identifier column: prefer data_check's `identifier` role for this
    # file; fall back to a name-pattern match; else use row numbers.
    id_col <- NULL
    if (!is.null(columns_df) &&
        all(c("source_file", "column_name", "role") %in% names(columns_df))) {
      idc <- columns_df$column_name[columns_df$source_file == file &
                                      columns_df$role %in% "identifier"]
      idc <- idc[idc %in% names(df)]
      if (length(idc) > 0) id_col <- idc[[1]]
    }
    if (is.null(id_col)) {
      # Word-boundary anchored: an unanchored "subject"/"participant" substring
      # match hit real response columns whose names happen to start with that
      # stem — "SubjectiveSES", "subjective_aware", "subject_cond",
      # "subject_parity" are Likert/condition items about a participant's
      # subjective state, not identifier columns — verified against the
      # cached corpus, same bug class as .concept_is_rt/.missing_label_re.
      hit <- grep("(?i)(^id$|\\bparticipant|\\bsubject\\b|\\brespond|_id$|\\bprolific|\\bmturk|\\bworker)",
                  names(df), perl = TRUE)
      if (length(hit) > 0) id_col <- names(df)[hit[[1]]]
    }
    has_id <- !is.null(id_col)
    if (!has_id) {               # careless findings are only actionable with an ID
      careless_skipped <- careless_skipped + 1L; next
    }
    n_careless_files <- n_careless_files + 1L
    if (!careless_avail) {       # count the opportunity, but cannot compute
      careless_skipped <- careless_skipped + 1L; next
    }
    careless_scored <- careless_scored + 1L

    ids <- as.character(df[[id_col]])
    for (cols in blocks) {
      scale  <- .scale_block_range(df[, cols, drop = FALSE])
      # Prefer a named scale identified by codebook_check (e.g. "PANAS") over the
      # bare variable-name prefix, when available for these columns.
      prefix <- .scale_name_prefix(names(df)[cols[[1]]])
      if (!is.null(labels_df) && all(c("source_file", "column_name", "scale")
                                     %in% names(labels_df))) {
        hit <- labels_df$source_file == file &
          labels_df$column_name %in% names(df)[cols] &
          !is.na(labels_df$scale) & nzchar(labels_df$scale %||% "")
        if (any(hit)) prefix <- labels_df$scale[which(hit)[1]]
      }
      res <- tryCatch(.dv_careless_block(df[, cols, drop = FALSE], ids, scale, prefix),
                      error = function(e) NULL)
      if (!is.null(res) && nrow(res) > 0) {
        res$source_file <- file
        careless_specs[[length(careless_specs) + 1L]] <- res
      }
    }
  }
  if (n_careless_files > 0 && !careless_avail)
    careless_note <- sprintf(
      "%d file%s contain survey data with an identifier, but careless-response checks were skipped because the `careless` package is not installed. Install it with `install.packages(\"careless\")` to screen for straightlining and other careless responding.",
      n_careless_files, plural(n_careless_files))

  careless_block_df <- if (length(careless_specs) > 0) dplyr::bind_rows(careless_specs) else
    data.frame(source_file = character(0), scale = character(0),
               respondent = character(0), longstring = integer(0),
               irv = numeric(0), reason = character(0),
               n_items = integer(0), straight_cut = integer(0),
               scale_range = character(0))

  # Aggregate to ONE ROW PER RESPONDENT (the per-block table double-counts a
  # person flagged in several scale blocks). For each respondent we record how
  # many blocks flagged them out of how many they appear in, the flag reasons,
  # and whether every flag is short-scale straightlining — which on a short,
  # unidirectional scale is often normal, coherent answering rather than
  # carelessness, so those respondents should be read with caution.
  careless_df <- .dv_careless_by_respondent(careless_block_df, previews,
                                            columns_df)

  findings_df <- if (length(findings) > 0) dplyr::bind_rows(findings) else
    data.frame(source_file = character(0), column = character(0),
               label = character(0), check = character(0), detail = character(0))
  # Spreadsheet-formatting findings (file/sheet-level: colour, merges, empty
  # rows/columns, offset header, ...) join the same table. They carry
  # column = NA (the issue is not about one column), so they show up in the
  # "Potential Issues" table but are excluded from n_flagged/check_counts
  # below, which are column-level tallies.
  findings_df <- dplyr::bind_rows(findings_df, spreadsheet_findings_df)



  n_columns <- if (!is.null(columns_df)) nrow(columns_df) else
    sum(vapply(previews, ncol, integer(1)))
  # Column-level findings only (spreadsheet findings carry column = NA and are
  # a file/sheet-level concern, not a column one — tallied separately below).
  column_findings_df <- findings_df[!is.na(findings_df$column), , drop = FALSE]
  n_flagged <- length(unique(paste(column_findings_df$source_file,
                                   column_findings_df$column)))
  n_flagged_files_spreadsheet <- length(unique(spreadsheet_findings_df$source_file))

  # Per-check tally: how many distinct columns each check flagged (a column can
  # be flagged by several checks, so these overlap and need not sum to n_flagged).
  check_counts <- if (nrow(column_findings_df) > 0) {
    column_findings_df |>
      dplyr::distinct(.data$source_file, .data$column, .data$check) |>
      dplyr::count(.data$check, name = "columns", sort = TRUE)
  } else {
    data.frame(check = character(0), columns = integer(0))
  }
  # Human-readable phrasing for each check. Noun phrases ("... with potential
  # outliers") so they read correctly after any count, singular or plural.
  check_phrase <- c(
    "Out-of-range values" = "with values outside the column's apparent range (likely data-entry errors)",
    "Miscoded missing"  = "with miscoded missing values",
    Constant            = "with a single constant value",
    "Empty column"      = "with no observed values (entirely empty)",
    "SPSS filter variable" = "holding an SPSS \"Select Cases\" filter (analyses may have used only a subset of rows)",
    "Case issues"       = "with inconsistent category casing",
    Whitespace          = "with leading/trailing whitespace",
    "Numeric as text"   = "with numeric values stored as text",
    "Problematic column name" = "whose name contains file-illegal characters or is excessively long",
    "Colliding column names" = "whose names become identical when special characters are removed",
    "Mixed encoding"    = "with values in a legacy (non-UTF-8) encoding",
    "Personal info (values)"      = "whose values look like personal information",
    "Personal info (column name)" = "whose name suggests personal information",
    "Geographic coordinates"      = "that look like geographic coordinates",
    "Free-text (may hold PII)"    = "of free text that may contain personal detail"
  )

  # ── 3. Traffic light ─────────────────────────────────────────────────────────
  frac_flagged <- if (n_columns > 0) n_flagged / n_columns else 0
  dv_tl <- if (n_flagged == 0) "green"
        else if (frac_flagged < 0.25) "yellow"
        else "red"
  # Careless-responding findings are respondent-level (not counted in the column
  # tally); if any were found, the result is at least yellow.
  if (nrow(careless_df) > 0 && dv_tl == "green") dv_tl <- "yellow"
  # Spreadsheet-formatting findings are file-level (not counted in the column
  # tally either); if any were found, the result is at least yellow (matches
  # the former spreadsheet_check module, which never went red on its own).
  if (nrow(spreadsheet_findings_df) > 0 && dv_tl == "green") dv_tl <- "yellow"

  # ── 4. Report ────────────────────────────────────────────────────────────────
  dv_summary_text <- if (n_flagged == 0) {
    sprintf("We ran automated data-quality checks on %d column%s and found no issues.",
            n_columns, plural(n_columns))
  } else {
    # Append a per-check breakdown so reviewers see what dominates without
    # scrolling every column, e.g. "42 contain potential outliers; 18 have ...".
    parts <- vapply(seq_len(nrow(check_counts)), function(i) {
      chk <- check_counts$check[i]
      phrase <- if (chk %in% names(check_phrase)) check_phrase[[chk]] else
        sprintf("flagged by %s", tolower(chk))
      n <- check_counts$columns[i]
      sprintf("%d column%s %s", n, plural(n), phrase)
    }, character(1))
    sprintf("We ran automated data-quality checks on %d column%s; %d column%s %s at least one potential issue (%s).",
            n_columns, plural(n_columns), n_flagged, plural(n_flagged),
            if (n_flagged == 1) "has" else "have",
            paste(parts, collapse = "; "))
  }
  if (nrow(careless_df) > 0) {
    n_car   <- nrow(careless_df)                 # distinct respondents
    n_short <- sum(careless_df$short_scale_only %in% TRUE)
    dv_summary_text <- paste0(dv_summary_text,
      sprintf(" %d survey respondent%s %s flagged for possible careless responding%s.",
              n_car, plural(n_car), if (n_car == 1) "was" else "were",
              if (n_short > 0) sprintf(
                " (%d of them only via short-scale straightlining, which can be normal answering)",
                n_short) else ""))
  }
  if (nrow(demo_df) > 0) {
    kinds <- sort(unique(demo_df$demographic))
    dv_summary_text <- paste0(dv_summary_text,
      sprintf(" We detected demographic column%s for %s.",
              plural(length(kinds)),
              paste(tools::toTitleCase(kinds), collapse = ", ")))
  }
  if (length(qualtrics_specs) > 0) {
    n_drop <- sum(vapply(qualtrics_specs, function(s) s$n_drop %||% 0L, integer(1)))
    dv_summary_text <- paste0(dv_summary_text,
      sprintf(" %d file%s %s a Qualtrics survey export%s.",
              length(qualtrics_specs), plural(length(qualtrics_specs)),
              if (length(qualtrics_specs) == 1) "is" else "are",
              if (n_drop > 0)
                sprintf(" (%d row%s look like previews/unfinished responses to review)",
                        n_drop, plural(n_drop)) else ""))
  }
  if (nrow(spreadsheet_findings_df) > 0) {
    dv_summary_text <- paste(dv_summary_text,
      .dv_spreadsheet_summary_text(spreadsheet_findings_df, n_spreadsheet_files))
  }

  dv_report <- c(
    "This module runs automated data-quality checks (outliers, miscoded missing values, empty and constant columns, SPSS filter variables, category casing, problematic column names, mixed text encodings, spreadsheet formatting) on the extracted data files.",
    sprintf("We examined %d column%s across %d data file%s.",
            n_columns, plural(n_columns),
            length(previews), plural(length(previews)))
  )

  # Single "Issues identified" table: one row per (file, column) that has at
  # least one issue, one cell listing every issue found for it (icon + short
  # label, hover for the full detail sentence). PII findings — "Personal info
  # (values)" and "Personal info (column name)" are two independent detectors
  # (one scans cell contents, one scans the column name) but are merged to a
  # single displayed "Personally Identifying Information" label here, since the
  # distinction is not one readers need — see .dv_issue_cell().
  # Spreadsheet-formatting findings (column = NA) join the same table: their
  # "column" cell shows the sheet name instead.
  all_issue_findings <- dplyr::bind_rows(column_findings_df, spreadsheet_findings_df)
  if (nrow(all_issue_findings) > 0) {
    issues_tbl <- all_issue_findings |>
      dplyr::mutate(.col_display = ifelse(is.na(.data$column), .data$label, .data$column)) |>
      dplyr::arrange(.data$source_file, .data$.col_display) |>
      dplyr::summarise(
        Issues = .dv_issue_cell(.data$check, .data$detail),
        .by = c(source_file, .col_display)
      ) |>
      dplyr::transmute(File = .data$source_file, Column = .data$.col_display,
                       Issues = .data$Issues)
    # When any PII finding is present, say plainly how these detectors work and
    # why a flag is not a verdict. They are deliberately cautious — a missed
    # identifier is a disclosure, a false alarm costs a glance — so the report
    # should not read as an accusation that data was leaked.
    pii_checks <- c("Personal info (values)", "Personal info (column name)",
                    "Free-text (may hold PII)")
    pii_note <- if (any(all_issue_findings$check %in% pii_checks)) (
      paste0(
        "\n\n**About the personal-information flags.** These are pattern ",
        "matches, not confirmed disclosures, and they are tuned to over-report ",
        "rather than miss something. Expect false positives:\n\n",
        "- **Column names** are matched against identifying words in English ",
        "and other European languages (`email`, `telefoonnummer`, ",
        "`geboortedatum`, ...). A column called `phone_number` flags whether ",
        "or not it holds one, and an empty or already-anonymised column flags ",
        "on its name alone.\n",
        "- **Values** are matched against the shape of an identifier. An IP ",
        "address pattern also matches a version string or a dotted numeric ",
        "code; a card-number pattern requires both a real issuer prefix and a ",
        "checksum, but a long numeric ID can still coincide.\n",
        "- **Free text** is flagged when a column holds long, mostly-distinct ",
        "prose — the shape open-ended answers take. Most such columns contain ",
        "no names at all.\n\n",
        "A flag means *look at this column*. Where the data is already ",
        "de-identified (an `ip` column reading `Anonymized`, a `name` column ",
        "the authors emptied), no action is needed.")
    ) else NULL

    dv_report <- c(dv_report, "#### Issues Identified",
                paste0(
                  sprintf("%d file/column combination%s %s at least one issue.",
                          nrow(issues_tbl), plural(nrow(issues_tbl)),
                          if (nrow(issues_tbl) == 1) "has" else "have"),
                  pii_note %||% ""),
                scroll_table(issues_tbl, maxrows = 20, escape = FALSE))
  }



  # Distributions: a single faceted figure with one small histogram per numeric
  # column (outlier fences drawn as dashed lines), instead of a separate plot
  # per column. One render call keeps wide files fast; the facet count is capped
  # so the figure stays legible.
  if (length(plot_specs) > 0 && requireNamespace("ggplot2", quietly = TRUE) && plot_distributions == TRUE) {
    dv_report <- c(dv_report, "#### Distributions",
                data_validate_dist_facets(plot_specs, max_facets = max_facets))
  } else if (length(plot_specs) > 0 && plot_distributions == TRUE) {
    dv_report <- c(dv_report,
      "*Install the `ggplot2` package to see the distribution histograms.*")
  }

  # Qualtrics survey metadata: for each detected Qualtrics export, a summary of
  # the reliably-extractable response metadata — preview/unfinished rows to drop,
  # completion-time distribution, data-collection window, and PII fields present.
  if (length(qualtrics_specs) > 0) {
    dv_report <- c(dv_report, .dv_qualtrics_report(qualtrics_specs, length(previews)))
  }

  # Spreadsheet formatting (colour coding, merged cells, empty rows/columns,
  # offset headers). These findings are file/sheet-level, so they are reported
  # in their own section rather than in the per-column "Issues Identified"
  # table. Only the `empty()` early-return rendered this section before, so a
  # repository whose data files DID read left its spreadsheet findings in the
  # findings table and the summary counts but showed no section in the report.
  if (nrow(spreadsheet_findings_df) > 0) {
    dv_report <- c(dv_report,
      .dv_spreadsheet_report(spreadsheet_findings_df, n_spreadsheet_files))
  }

  # Demographic columns (age/gender/race) are still computed into `demo_df`
  # and returned via the `demographics` field for programmatic consumers, but
  # are no longer rendered as their own dv_report section — they are informational
  # rather than an issue, and do not belong in the "Issues Identified" table.

  # Careless responding: respondents flagged by longstring / IRV on a survey
  # scale block. Reported per respondent (with the scale and reason), so a
  # reviewer can inspect those rows in the raw data.
  if (nrow(careless_df) > 0) {
    car_tbl <- careless_df |>
      dplyr::transmute(
        Respondent = .data$respondent,
        `Blocks flagged` = .data$n_blocks_flagged,
        Scales = .data$scales,
        `Longest run` = .data$threshold,
        `Short-scale only` = ifelse(.data$short_scale_only %in% TRUE,
                                    "yes", "no"))
    n_car   <- nrow(careless_df)
    n_short <- sum(careless_df$short_scale_only %in% TRUE)
    dv_report <- c(dv_report,
      "#### Careless Responding",
      sprintf("%d distinct respondent%s were flagged for **straightlining**: giving the same answer for at least 80%% of the items in a multi-item scale of %d items or more. One respondent can be flagged in several scales; the table below is **one row per person**, and the *Longest run* column gives the run that triggered the strongest flag, so you can check it against the raw data.",
              n_car, plural(n_car), .dv_careless_min_items),
      if (n_short > 0) sprintf(
        "Of these, **%d were flagged *only* by short-scale straightlining** (a run of identical answers on a scale of %d items or fewer). On a short, one-directional scale, answering consistently is often normal, coherent responding rather than carelessness — treat these as weak signals and inspect the actual responses before excluding anyone.",
        n_short, .dv_short_scale_max) else NULL,
      "These are prompts to inspect those rows, not definitive judgements.",
      scroll_table(car_tbl, maxrows = 20),
      .dv_careless_coverage_text(careless_scored, careless_skipped))
  } else if (!is.null(careless_note)) {
    dv_report <- c(dv_report, "#### Careless Responding", careless_note)
  } else if (careless_scored > 0 || careless_skipped > 0) {
    dv_report <- c(dv_report, "#### Careless Responding",
      if (careless_scored > 0) sprintf(
        "No respondent was flagged for straightlining in the %d file%s that could be screened.",
        careless_scored, plural(careless_scored)) else
        "No file could be screened for careless responding.",
      .dv_careless_coverage_text(careless_scored, careless_skipped))
  }

  # ── 5. Summary table + return ────────────────────────────────────────────────
  dv_summary_table <- data.frame(
    paper_id  = .pid(columns_df),
    column_n  = n_columns,
    flagged_n = n_flagged,
    spreadsheet_file_n = n_spreadsheet_files,
    spreadsheet_flagged_file_n = n_flagged_files_spreadsheet
  )

  qualtrics_df <- if (length(qualtrics_specs) > 0)
    dplyr::bind_rows(lapply(qualtrics_specs, function(s) data.frame(
      source_file = s$source_file, n_rows = s$n_rows,
      n_drop = s$n_drop, median_seconds = s$median_seconds,
      n_fast = s$n_fast, date_min = s$date_min, date_max = s$date_max,
      pii_fields = paste(s$pii_fields, collapse = ", ")))) else
    data.frame()


  # ── 8. Return ────────────────────────────────────────────────────────────────
  # Traffic light is the WORST of the two halves: a data-quality problem must
  # never be hidden behind a successful extraction, and an extraction failure
  # must not be masked by a clean bill of health on the columns that did load.
  tl <- {
    rank <- c(na = 0L, green = 1L, yellow = 2L, red = 3L)
    both <- c(tl, dv_tl)
    both <- both[both %in% names(rank)]
    # Index by VALUE, not by the max value used as a position: `rank` starts at
    # na = 0, so its names are offset by one from the ranks they carry and
    # `names(rank)[max(rank[both])]` silently returned the light one step too
    # low (green+yellow -> "green", yellow+red -> "yellow").
    if (!length(both)) "na" else names(rank)[which(rank == max(rank[both]))[1]]
  }

  list(
    # One row per COLUMN: what each column is (representation, level, concept,
    # role, unit, quality). Read by codebook_check and psychds_check.
    table = columns_df %||% data.frame(),
    # One row per FINDING: what is wrong with a column, a sheet or a respondent.
    # A different grain from `table` on purpose -- one column can raise several
    # findings, and a spreadsheet-formatting or careless-responding finding has
    # no column to attach to at all.
    findings = findings_df,
    careless = careless_df,
    demographics = demo_df,
    qualtrics = qualtrics_df,
    structure = all_files,          # per-file classification, for codebook_check
    previews = file_previews,       # full read data frames
    gated_repos = listing_gated,    # repos found but not listable (size gate, ...)
    manifest_path = manifest_path,  # full manifest path, when one was written
    # TRUE when no file's path/repository/code-reference/LLM evidence named a
    # real study anywhere in this repository — every group came from the
    # blanket "ex1" default rather than actual evidence. psychds_check surfaces
    # this as a warning instead of implying real structure was detected.
    group_no_evidence = group_no_evidence,
    summary_table = merge(summary_table, dv_summary_table, by = "paper_id",
                          all = TRUE, sort = FALSE),
    na_replace = c(data_file_n = 0, column_n = 0, empty_col_n = 0,
                   flagged_n = 0, spreadsheet_file_n = 0,
                   spreadsheet_flagged_file_n = 0),
    traffic_light = tl,
    report = c(report, dv_report),
    summary_text = paste(summary_text, dv_summary_text)
  )
}
# Careless screening needs MORE items than scale-block detection does. A block
# of 3-4 items is a valid scale (.scale_min_items = 3) but useless for
# straightlining: on 3 items with 8 levels, answering identically has
# probability ~0.05 under independent responding, so it happens to roughly 1 in
# 20 ordinary respondents. Measured on the cached corpus, one 3-item block
# produced 85 of 92 flags on its own; requiring 5 items removes all 85 and
# leaves the flags on the 25- and 50-item batteries untouched. At 5 items the
# same chance probability is ~0.008, and it falls away steeply after that.
.dv_careless_min_items <- 5L
.dv_careless_min_rows  <- 30L

# Scale-block detection is shared with codebook_check: see .detect_scale_blocks,
# .scale_name_prefix and .scale_block_range in R/data_check_helpers.R.

# Run careless indices on one scale block and return the respondents that look
# careless. `block` is a numeric data frame of items; `ids` is the identifier
# column aligned to its rows (or row numbers).
#
# ONLY straightlining flags a respondent: a run of identical consecutive
# answers covering at least 80% of the block's items. The IRV (SD of a
# respondent's answers) is computed and returned for context, but does not
# flag. Two earlier designs were measured against the cached corpus and
# rejected:
#
#   * A Tukey fence on the block's own IRV distribution. Being a within-sample
#     percentile, it removes ~2-3% of ANY block whether or not anyone responded
#     carelessly, and the careless mass shifts the fence that is meant to catch
#     it.
#   * An absolute cut at a fixed fraction of the response range. Measured
#     median IRV barely tracks scale width (0.73 on a 1-4 scale, 0.88/0.92/0.74
#     on 1-7), so a range-scaled cut over-flags wide scales badly: 0.5% of a
#     1-4 block but 12-30% of the 1-7 blocks, catching respondents at SD 0.56
#     who plainly varied their answers.
#
# What remains — IRV exactly 0 — flags the same respondents as a full identical
# run, so it is not independent evidence and adds only a second name for one
# finding. The careless literature gives no universal cutoff (Curran 2016;
# Ward & Meade 2023 both decline to give one) and recommends judging thresholds
# per dataset, which an automated report cannot do; an absolute run-length rule
# is the one threshold here that does not depend on the sample it is applied to.
#
# The 80% (rather than 100%) run requirement is deliberate: on the corpus it
# added two respondents with runs of 21/25 and 22/25 — flat apart from one or
# two stray answers — whom a perfect-run rule would clear. At 25 items and 7
# levels a run that long has probability ~2e-15 under independent responding.
.dv_careless_block <- function(block, ids, scale, prefix) {
  block <- as.data.frame(lapply(block, function(x)
    suppressWarnings(as.numeric(as.character(x)))))
  n_items <- ncol(block)
  ls <- careless::longstring(block)
  iv <- careless::irv(block, na.rm = TRUE)

  # Straightlining: same answer for >= 80% of items (and at least 5 in a row).
  # Callers gate on .dv_careless_min_items, so n_items >= 5 here and the floor
  # is never clamped below the block's own width.
  straight_cut <- max(5L, ceiling(0.8 * n_items))
  flagged <- which(!is.na(ls) & ls >= straight_cut)
  if (length(flagged) == 0) return(NULL)
  data.frame(
    scale        = paste0(prefix, " (", scale, ", ", n_items, " items)"),
    respondent   = as.character(ids[flagged]),
    longstring   = ls[flagged],
    irv          = round(iv[flagged], 2),
    reason       = "straightlining",
    n_items      = n_items,
    straight_cut = straight_cut,
    scale_range  = scale
  )
}

# Scope caveat for the careless section. Two limits have to be stated together,
# because either alone reads as a stronger all-clear than the check supports:
#   * only files with a detectable multi-item scale block are screened at all;
#   * within those files, only the scale-block columns are screened, so single
#     items, open text, demographics and one-off questions are never examined.
# An absence of flags is therefore never evidence that a dataset is clean.
.dv_careless_coverage_text <- function(n_scored, n_skipped) {
  scope <- if (n_skipped > 0)
    sprintf("Only %d of %d data file%s could be screened: the rest had no detectable multi-item scale block, or too few respondents. ",
            n_scored, n_scored + n_skipped, plural(n_scored + n_skipped)) else ""
  paste0(
    "**What this does not cover.** ", scope,
    "Even in a screened file, these checks only look at multi-item rating scales — the only place where a repeated or flat answer pattern is interpretable. Single questions, open text, demographics and any item outside a detected scale block are never examined. A respondent can therefore answer most of a survey carelessly and still not be flagged here, and finding nothing is not evidence that a dataset is free of careless responding.")
}

# One-line statement of the rule a flagged respondent crossed, in the units a
# reader can check against the raw data. `row` is a single row of the
# per-(respondent x block) table. Straightlining is reported when present
# because it is the more concrete finding; otherwise the IRV rule is stated.
.dv_careless_threshold_text <- function(row) {
  if (nrow(row) == 0) return(NA_character_)
  sprintf("same answer %d times in a row, out of %d items (flagged at %d)",
          row$longstring, row$n_items, row$straight_cut)
}

# Item count of a "prefix (min-max, N items)" scale label, or NA.
.dv_scale_n_items <- function(scale_label) {
  m <- regmatches(scale_label, regexpr("([0-9]+) items", scale_label))
  suppressWarnings(as.integer(sub(" items$", "", m)))
}

# A block is "short" (straightlining is weak evidence there) when it has few
# items — a run of identical answers is common and often coherent on a short
# unidirectional scale.
.dv_short_scale_max <- 7L

# Collapse the per-(respondent x block) careless table to one row per
# respondent. Columns: paper-level respondent id, source_file(s), how many
# blocks flagged them and on which scales, the distinct reasons, worst
# longstring / most extreme IRV, and `short_scale_only` — TRUE when every flag
# is short-scale straightlining (likely-benign consistent answering, not clear
# carelessness). Returns a zero-row frame with the same columns when empty.
.dv_careless_by_respondent <- function(block_df, previews = NULL,
                                       columns_df = NULL) {
  cols0 <- c("respondent", "source_file", "n_blocks_flagged", "scales",
             "reasons", "max_longstring", "irv", "short_scale_only",
             "threshold")
  if (is.null(block_df) || nrow(block_df) == 0)
    return(stats::setNames(
      data.frame(matrix(nrow = 0, ncol = length(cols0))), cols0))

  block_df$.n_items   <- .dv_scale_n_items(block_df$scale)
  block_df$.is_short_straight <-
    grepl("straightlin", block_df$reason) &
    !grepl("IRV", block_df$reason) &
    !is.na(block_df$.n_items) & block_df$.n_items <= .dv_short_scale_max

  parts <- lapply(split(block_df, block_df$respondent), function(g) {
    # Strongest single piece of evidence: the block with the longest run.
    i <- which.max(g$longstring)[1]
    data.frame(
      respondent       = g$respondent[1],
      source_file      = paste(sort(unique(g$source_file)), collapse = "; "),
      n_blocks_flagged = nrow(g),
      scales           = paste(sort(unique(g$scale)), collapse = "; "),
      reasons          = paste(sort(unique(g$reason)), collapse = "; "),
      max_longstring   = max(g$longstring, na.rm = TRUE),
      irv              = min(g$irv, na.rm = TRUE),
      # TRUE only if EVERY block that flagged this person is short-scale
      # straightlining — i.e. no long-scale or IRV-based flag corroborates it.
      short_scale_only = all(g$.is_short_straight),
      # What that strongest flag actually crossed, in the reader's units.
      threshold        = .dv_careless_threshold_text(g[i, , drop = FALSE]),
      stringsAsFactors = FALSE)
  })
  out <- dplyr::bind_rows(parts)
  # Most-flagged first.
  out[order(-out$n_blocks_flagged, -out$max_longstring), , drop = FALSE]
}

# ── Qualtrics metadata helpers ────────────────────────────────────────────────

# Locate a Qualtrics metadata column by its semantic tag (see
# .qualtrics_meta_cols in data_check_helpers.R); returns the column vector or
# NULL. Matching is by tag, so a renamed-but-recognised column is still found.
.dv_q_col <- function(df, tag) {
  tags <- .qualtrics_tag_cols(names(df))
  hit <- which(tags == tag)
  if (length(hit) == 0) NULL else df[[hit[1]]]
}

# Parse a Qualtrics datetime column (ISO "YYYY-MM-DD HH:MM:SS") to POSIXct.
# Returns all-NA (never errors) when the values are not datetimes: as.POSIXct
# with tryFormats *errors* rather than warns when no format matches, so we parse
# per-format and coalesce, guarded by tryCatch.
.dv_q_datetime <- function(x) {
  if (is.null(x)) return(NULL)
  xc <- as.character(x)
  out <- as.POSIXct(rep(NA_real_, length(xc)), tz = "UTC", origin = "1970-01-01")
  for (fmt in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")) {
    miss <- is.na(out)
    if (!any(miss)) break
    parsed <- tryCatch(
      as.POSIXct(xc[miss], tz = "UTC", format = fmt),
      error = function(e) as.POSIXct(rep(NA_real_, sum(miss)), tz = "UTC",
                                     origin = "1970-01-01"))
    out[miss] <- parsed
  }
  out
}

# Summarise one Qualtrics export's response metadata. Returns a list of the
# reliably-extractable facts (or NULL if the file carries none of them):
#   n_rows, n_drop (preview/spam/unfinished rows), median_seconds, n_fast
#   (implausibly fast completions), date_min/date_max (collection window),
#   pii_fields (which Qualtrics PII columns are present).
.dv_qualtrics_summary <- function(df) {
  n_rows <- nrow(df)

  # Rows to review/drop: Status marks previews (1) and spam (2/8); Finished == 0
  # / FALSE and Progress < 100 mark incomplete responses. Any one qualifies.
  status <- .dv_q_col(df, "qualtrics_status")
  finished <- .dv_q_col(df, "qualtrics_finished")
  progress <- .dv_q_col(df, "qualtrics_progress")
  drop <- rep(FALSE, n_rows)
  if (!is.null(status)) {
    s <- suppressWarnings(as.numeric(as.character(status)))
    st <- tolower(trimws(as.character(status)))
    # Numeric coding (0 = real IP response) or text labels ("Survey Preview").
    drop <- drop | (!is.na(s) & s %in% c(1, 2, 8)) |
      grepl("preview|spam", st)
  }
  if (!is.null(finished)) {
    f <- tolower(trimws(as.character(finished)))
    drop <- drop | f %in% c("0", "false", "no")
  }
  if (!is.null(progress)) {
    p <- suppressWarnings(as.numeric(as.character(progress)))
    drop <- drop | (!is.na(p) & p < 100)
  }
  n_drop <- sum(drop)

  # Completion time: Duration (in seconds); fall back to EndDate - StartDate.
  dur <- suppressWarnings(as.numeric(as.character(.dv_q_col(df, "qualtrics_duration"))))
  if (all(is.na(dur))) {
    sd <- .dv_q_datetime(.dv_q_col(df, "qualtrics_start"))
    ed <- .dv_q_datetime(.dv_q_col(df, "qualtrics_end"))
    if (!is.null(sd) && !is.null(ed))
      dur <- as.numeric(difftime(ed, sd, units = "secs"))
  }
  dur <- dur[!is.na(dur) & dur >= 0]
  median_seconds <- if (length(dur) > 0) stats::median(dur) else NA_real_
  # Implausibly fast: under half the median AND under 2 minutes (a heuristic
  # speeding flag; only meaningful with enough completed responses).
  n_fast <- if (length(dur) >= 10 && !is.na(median_seconds))
    sum(dur < pmin(0.5 * median_seconds, 120)) else NA_integer_

  # Collection window: prefer RecordedDate, else StartDate.
  dt <- .dv_q_datetime(.dv_q_col(df, "qualtrics_recorded"))
  if (is.null(dt) || all(is.na(dt))) dt <- .dv_q_datetime(.dv_q_col(df, "qualtrics_start"))
  date_min <- date_max <- NA_character_
  if (!is.null(dt) && any(!is.na(dt))) {
    date_min <- format(min(dt, na.rm = TRUE), "%Y-%m-%d")
    date_max <- format(max(dt, na.rm = TRUE), "%Y-%m-%d")
  }

  # Which Qualtrics PII fields are present (drives a before-sharing prompt).
  pii_tags <- c(qualtrics_ip = "IP address", qualtrics_email = "email",
                qualtrics_lat = "location", qualtrics_lon = "location",
                qualtrics_externalref = "external reference (e.g. panel ID)",
                qualtrics_recipient = "recipient name")
  present <- unique(unname(pii_tags[intersect(
    .qualtrics_tag_cols(names(df)), names(pii_tags))]))

  list(n_rows = n_rows, n_drop = n_drop, median_seconds = median_seconds,
       n_fast = n_fast, date_min = date_min, date_max = date_max,
       pii_fields = present)
}

# Build the "Qualtrics Survey Metadata" report section from the per-file
# summaries produced by .dv_qualtrics_summary().
.dv_qualtrics_report <- function(specs, n_previews) {
  fmt_dur <- function(sec) {
    if (is.na(sec)) return("—")
    if (sec < 90) sprintf("%.0f s", sec)
    else if (sec < 5400) sprintf("%.1f min", sec / 60)
    else sprintf("%.1f h", sec / 3600)
  }
  rows <- lapply(specs, function(s) data.frame(
    File = s$source_file,
    Responses = s$n_rows,
    `Preview/unfinished` = s$n_drop,
    `Median time` = fmt_dur(s$median_seconds),
    `Very fast` = if (is.na(s$n_fast)) "—" else as.character(s$n_fast),
    `Collected` = if (is.na(s$date_min)) "—" else
      if (identical(s$date_min, s$date_max)) s$date_min else
        paste(s$date_min, "to", s$date_max),
    `PII fields` = if (length(s$pii_fields) == 0) "none" else
      paste(s$pii_fields, collapse = ", "),
    check.names = FALSE))
  tbl <- dplyr::bind_rows(rows)

  n_files <- length(specs)
  total_drop <- sum(vapply(specs, function(s) s$n_drop, integer(1)))
  intro <- sprintf(
    "%d of the %d data file%s %s a Qualtrics survey export. The table below summarises the response metadata Qualtrics records for every survey (not the substantive question columns): how many rows look like previews or unfinished responses that usually need dropping before analysis, the typical completion time (with a count of implausibly fast responses), the data-collection window, and which Qualtrics fields carry personal information to review before sharing.",
    n_files, n_previews, plural(n_previews),
    if (n_files == 1) "is" else "are")
  note <- if (total_drop > 0)
    sprintf(" Across these files, %d row%s %s flagged as a preview, spam, or unfinished response — check whether they should be excluded.",
            total_drop, plural(total_drop),
            if (total_drop == 1) "is" else "are") else ""

  c("#### Qualtrics Survey Metadata",
    paste0(intro, note),
    scroll_table(tbl, maxrows = 20))
}

# ── Issues Identified table (single-column display) ───────────────────────────

# Icon shown before each issue's short label in the "Issues Identified" table.
# "Personal info (values)" and "Personal info (column name)" are two
# independent detectors (data_check_pii_values() scans cell contents,
# data_check_pii_name() scans only the column name — a column can trip either
# without the other) but share one icon/label here: the distinction between
# "the data itself looks sensitive" and "the name suggests it might be" is not
# one a reader of this table needs, both mean the same thing — review before
# sharing. The two checks themselves are untouched; only this DISPLAY mapping
# merges them.
.dv_issue_icon <- c(
  "Values outside the scale"       = "\U0001F4C8",  # 📈 out-of-range
  "Miscoded missing"                = "\U00002753",  # ❓
  "Constant"                        = "\U0001F7F0",  # 🟰
  "Empty column"                    = "\U00002B1C",  # ⬜
  "SPSS filter variable"            = "\U0001F9EE",  # 🧮
  "Case issues"                     = "\U0001F524",  # 🔤
  "Whitespace"                      = "\U00002194\U0000FE0F",  # ↔️
  "Numeric as text"                 = "#\U0000FE0F\U000020E3",  # #️⃣
  "Problematic column name"         = "\U0001F3F7\U0000FE0F",  # 🏷️
  "Colliding column names"          = "\U0001F465",  # 👥
  "Mixed encoding"                  = "\U0001F523",  # 🔣
  "Personal info (values)"          = "\U0001F512",  # 🔒
  "Personal info (column name)"     = "\U0001F512",  # 🔒
  "Geographic coordinates"          = "\U0001F30D",  # 🌍
  "Free-text (may hold PII)"        = "\U0001F512",  # 🔒
  "Not a rectangular dataset"       = "\U0001F4D0",  # 📐
  "Header not on first row"         = "\U0001F4CB",  # 📋
  "Un-inspectable (.xls)"           = "\U000026A0\U0000FE0F",  # ⚠️
  "Unreadable"                      = "\U000026A0\U0000FE0F",  # ⚠️
  "Colour coding"                   = "\U0001F3A8",  # 🎨
  "Merged cells"                    = "\U0001F517",  # 🔗
  "Empty rows"                      = "\U00002B1C",  # ⬜
  "Empty or unnamed columns"        = "\U00002B1C"   # ⬜
)

# Checks whose displayed label/icon should collapse together (PII, values vs.
# name) — every key in this vector maps to the same shown label.
.dv_issue_merge_label <- c(
  "Personal info (values)"      = "Personally Identifying Information",
  "Personal info (column name)" = "Personally Identifying Information",
  "Free-text (may hold PII)"    = "Personally Identifying Information"
)

# Build the "Issues" cell HTML for one (file, column) group: one line per
# issue (icon + short label), each wrapped in a span whose `title` attribute
# carries the full original `detail` sentence(s) for hover. Several issues on
# the same column stack as several lines in the same cell (via <br>, since
# scroll_table() replaces "\n" with "<br>" — see report-helpers.R). Checks that
# share a merged display label (the PII trio) collapse to ONE line.
.dv_issue_cell <- function(check, detail) {
  # dplyr::summarise() calls this once per (source_file, column) GROUP, with
  # `check`/`detail` as the group's vectors — build one cell string per group.
  #
  # Checks that share a merged display label (currently just the PII trio —
  # see .dv_issue_merge_label) are grouped FIRST, by that label, before
  # building any HTML: two PII detectors firing on the same column must become
  # ONE line with both details in its tooltip, not two lines that only look
  # identical by accident (their `detail` text differs even though the label
  # is shared, so de-duplicating the finished HTML strings would not catch
  # this — grouping by label upfront is what makes the merge actually happen).
  display_label <- ifelse(!is.na(.dv_issue_merge_label[check]),
                          unname(.dv_issue_merge_label[check]), check)
  groups <- split(seq_along(check), display_label)

  lines <- vapply(groups, function(idx) {
    lbl <- display_label[idx[[1]]]
    # One icon per group: the first check's icon (all PII checks share one
    # anyway; a future merge group with genuinely different icons would still
    # need a single glyph here, so "first wins" is the deliberate rule).
    icon <- .dv_issue_icon[check[idx[[1]]]]
    icon <- if (is.na(icon)) "\U00002139\U0000FE0F" else unname(icon)
    # First clause of each detail (up to the first period/semicolon), capped
    # in length, as the scannable summary; the full sentence(s) go in the
    # hover title below instead.
    short <- vapply(idx, function(i)
      sub("[.;].*$", "", detail[[i]]), character(1))
    short <- short[!is.na(short) & nzchar(trimws(short))]
    short <- unique(short)
    short <- ifelse(nchar(short) > 70, paste0(substr(short, 1, 67), "..."), short)
    short_txt <- if (length(short) > 0)
      paste0(" — ", paste(short, collapse = "; ")) else ""
    # Full sentences from every finding in the group, for the hover tooltip.
    full_detail <- paste(unique(detail[idx]), collapse = " ")
    # detail/label can echo values read from the paper's own data (a category
    # string, a file name, ...), so both are HTML-escaped before going into
    # the report: .stat_html_escape() (R/stat_helpers.R) handles &/</>; the title
    # attribute is single-quoted, so a single quote needs its own escape too.
    detail_esc <- gsub("'", "&#39;", .stat_html_escape(full_detail), fixed = TRUE)
    text_esc   <- .stat_html_escape(paste0(lbl, short_txt))
    sprintf("<span title='%s'>%s %s</span>", detail_esc, icon, text_esc)
  }, character(1))

  paste(lines, collapse = "\n")
}

# ── Module-local helper ───────────────────────────────────────────────────────

# Maximum number of numeric columns drawn in the combined distribution figure.
# Beyond this the facet grid becomes unreadable and slow, so we show the first
# `.dv_max_facets` and note how many were omitted.
.dv_max_facets <- 40L

# Build ONE faceted figure (a small histogram per numeric column, with the
# Tukey outlier fences as dashed lines) and return it as a self-contained
# base64 <img>. A single ggsave replaces the previous one-render-per-column
# tabset, which rendered hundreds of PNGs on wide files. The image is embedded
# as an inline data URI so a moved report keeps its figure (Quarto's own figure
# output links external `_files/` PNGs that the embed step does not inline).
data_validate_dist_facets <- function(plot_specs, max_facets = .dv_max_facets) {
  n_total <- length(plot_specs)
  if (n_total == 0) return(NULL)
  specs <- utils::head(plot_specs, max_facets)

  # Long data frame: one row per value, tagged by a unique per-column facet key
  # ("file: column"), plus the fences carried alongside for the rule layers.
  facet_label <- function(s) {
    lab <- if (nzchar(s$file)) paste0(s$col, "  (", s$file, ")") else s$col
    substr(lab, 1, 60)
  }
  parts <- lapply(seq_along(specs), function(i) {
    s <- specs[[i]]
    data.frame(facet = facet_label(s), v = as.numeric(s$values))
  })
  fences <- do.call(rbind, lapply(seq_along(specs), function(i) {
    s <- specs[[i]]
    if (is.na(s$lower)) NULL else
      data.frame(facet = facet_label(specs[[i]]),
                 xint = c(s$lower, s$upper))
  }))
  d <- do.call(rbind, parts)
  # Stable facet order matching plot_specs order.
  d$facet <- factor(d$facet, levels = unique(vapply(specs, facet_label, character(1))))

  uri <- tryCatch({
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$v)) +
      ggplot2::geom_histogram(bins = 30, fill = "grey70") +
      ggplot2::facet_wrap(~ facet, scales = "free", ncol = 4) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 8) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
    if (!is.null(fences)) {
      fences$facet <- factor(fences$facet, levels = levels(d$facet))
      p <- p + ggplot2::geom_vline(
        data = fences, ggplot2::aes(xintercept = .data$xint),
        linetype = "dashed", colour = "red", linewidth = 0.3)
    }
    n_facets <- nlevels(d$facet)
    nrow_facets <- ceiling(n_facets / 4)
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    ggplot2::ggsave(tmp, p, width = 9,
                    height = max(2, nrow_facets * 1.3), dpi = 96,
                    device = "png", bg = "white", limitsize = FALSE)
    paste0("data:image/png;base64,", base64enc::base64encode(tmp))
  }, error = function(e) NA_character_)

  if (is.na(uri)) return("*Distribution figure could not be rendered.*")
  img <- sprintf("<img src=\"%s\" alt=\"Column distributions\" style=\"max-width:100%%\"/>", uri)
  note <- if (n_total > length(specs))
    sprintf(paste0("\n\n*Showing the first %d of %d numeric columns (all were ",
                   "checked; only the figure is limited). Set `max_facets = %d` ",
                   "to plot them all.*"),
            length(specs), n_total, n_total) else ""
  paste0(img, note)
}

# ── Spreadsheet formatting checks ─────────────────────────────────────────────
#
# Formerly the standalone `spreadsheet_check` module; merged into data_validate
# because both check "additional categories of things that can be wrong" with a
# data file — this half just inspects the raw workbook CONTAINER (colour, merges,
# empty rows/columns, offset headers) rather than the CONTENT data_validate's
# other checks read from data_check's already-parsed `previews`. Those file-level
# facts (e.g. a cell's fill colour) are gone by the time a file becomes a data
# frame, so this reads the raw .xlsx/.ods zip/XML directly via structure_df's
# file_location, same as before.
#
# Findings are returned in data_validate's shared findings shape (source_file,
# column, label, check, detail) — column = NA for these, since the issue is
# file/sheet-level, not about one column — so they merge into the same
# `findings_df` / "Potential Issues" reporting path as every other check here.

.dv_spreadsheet_exts <- c("xlsx", "xls", "ods", "fods")

# Run the spreadsheet-formatting checks over every spreadsheet file in
# structure_df (data_check's file-classification table) that has a readable
# local copy. Returns list(findings, n_files): `findings` in the shared
# (source_file, column, label, check, detail) shape (0 rows when none found or
# no spreadsheet files exist), `n_files` the count of spreadsheet files examined
# (0 when none).
.dv_spreadsheet_findings <- function(structure_df) {
  empty_findings <- data.frame(source_file = character(0), column = character(0),
                               label = character(0), check = character(0),
                               detail = character(0))

  xl_rows <- if (!is.null(structure_df) && nrow(structure_df) > 0) {
    structure_df[
      tolower(tools::file_ext(structure_df$file_name)) %in% .dv_spreadsheet_exts &
        !is.na(structure_df$file_location) &
        nzchar(structure_df$file_location) &
        file.exists(structure_df$file_location %||% ""),
      , drop = FALSE
    ]
  } else structure_df[0, , drop = FALSE]

  if (is.null(xl_rows) || nrow(xl_rows) == 0)
    return(list(findings = empty_findings, n_files = 0L))

  n_files <- nrow(xl_rows)
  findings <- list()

  for (i in seq_len(n_files)) {
    fname <- xl_rows$file_name[i]
    path  <- xl_rows$file_location[i]
    ext   <- tolower(tools::file_ext(fname))

    # data_check flagged this file as not a usable rectangular dataset (a coding
    # worksheet: mostly free text and/or almost all empty). It is still inspected
    # for formatting below; here we add the structural note so the author knows
    # the file needs restructuring, not just reformatting.
    if (isFALSE(xl_rows$tabular_usable[i])) {
      reason <- xl_rows$non_tabular_reason[i] %||% NA_character_
      findings[[length(findings) + 1L]] <- data.frame(
        source_file = fname, column = NA_character_, label = NA_character_,
        check = "Not a rectangular dataset",
        detail = sprintf(
          "This file reads as a table but is not a usable dataset%s. Store the data as a plain rectangular table (one header row, one column per variable) with a codebook.",
          if (!is.na(reason)) sprintf(" (%s)", reason) else ""))
    }

    # Offset header: a banner / blank / units row above the real column header, so
    # the file does not read as a clean table. Reported so the AUTHOR removes the
    # junk row(s) at source; metacheck also repairs it in-memory for its own checks.
    #
    # This runs for EVERY format, including .xls: it reads the first rows through
    # readxl/readODS rather than the XML, so it does not need the zipped-XML
    # structure the style-level checks below require.
    oh <- tryCatch(.dv_spreadsheet_offset_header(path, ext), error = function(e) NULL)
    if (!is.null(oh)) {
      findings[[length(findings) + 1L]] <- data.frame(
        source_file = fname, column = NA_character_, label = NA_character_,
        check = "Header not on first row",
        detail = sprintf(
          "The column header is on row %d; above it is %s. Remove the row%s above the header so the first row of the sheet is the column header — otherwise the file reads with invented column names (…1, …4) and the data mis-types.",
          oh$header_row, oh$detail, plural(oh$n_above)))
    }

    # Style-level checks (colour, merges, empty rows/columns) need the document
    # XML. .xlsx and .ods/.fods both provide it; binary .xls does not.
    if (ext == "xls") {
      findings[[length(findings) + 1L]] <- data.frame(
        source_file = fname, column = NA_character_, label = NA_character_,
        check = "Un-inspectable (.xls)",
        detail = "Legacy .xls format: colour, merged cells and empty rows/columns cannot be inspected. Convert to .xlsx or .ods for a full check.")
      next
    }

    insp <- tryCatch(
      if (ext %in% c("ods", "fods")) .dv_ods_inspect(path) else .dv_excel_inspect(path),
      error = function(e) NULL)
    if (is.null(insp)) {
      findings[[length(findings) + 1L]] <- data.frame(
        source_file = fname, column = NA_character_, label = NA_character_,
        check = "Unreadable",
        detail = sprintf("The file could not be parsed as a %s workbook.",
                         if (ext %in% c("ods", "fods")) "OpenDocument" else ".xlsx"))
      next
    }

    for (s in insp$sheets) {
      if (s$color_cells > 0) {
        findings[[length(findings) + 1L]] <- data.frame(
          source_file = fname, column = NA_character_, label = s$name,
          check = "Colour coding",
          detail = sprintf("%d cell%s use fill colour to encode information; colour is lost on CSV export.",
                           s$color_cells, plural(s$color_cells)))
      }
      if (length(s$merges) > 0) {
        findings[[length(findings) + 1L]] <- data.frame(
          source_file = fname, column = NA_character_, label = s$name,
          check = "Merged cells",
          detail = sprintf("%d merged range%s (%s) break the rectangular grid.",
                           length(s$merges), plural(length(s$merges)),
                           paste(utils::head(s$merges, 5), collapse = ", ")))
      }
      if (s$empty_rows > 0) {
        findings[[length(findings) + 1L]] <- data.frame(
          source_file = fname, column = NA_character_, label = s$name,
          check = "Empty rows",
          detail = sprintf("%d fully empty row%s inside the data range.",
                           s$empty_rows, plural(s$empty_rows)))
      }
      if (s$empty_cols > 0) {
        findings[[length(findings) + 1L]] <- data.frame(
          source_file = fname, column = NA_character_, label = s$name,
          check = "Empty or unnamed columns",
          detail = sprintf("%d column%s %s empty or have a blank header.",
                           s$empty_cols, plural(s$empty_cols),
                           if (s$empty_cols == 1) "is" else "are"))
      }
    }
  }

  findings_df <- if (length(findings) > 0) dplyr::bind_rows(findings) else empty_findings
  list(findings = findings_df, n_files = n_files)
}

# Human-readable rollup of the spreadsheet findings, appended to data_validate's
# summary_text. Mirrors the former spreadsheet_check module's summary wording.
.dv_spreadsheet_summary_text <- function(findings_df, n_files) {
  n_flagged_files <- length(unique(findings_df$source_file))
  sprintf("%d of %d spreadsheet file%s %s at least one formatting issue (colour coding, merged cells, empty rows/columns, or an offset header).",
          n_flagged_files, n_files, plural(n_files),
          if (n_flagged_files == 1) "has" else "have")
}

# Report section for spreadsheet-formatting findings, appended after the
# per-column "Potential Issues" table.
.dv_spreadsheet_report <- function(findings_df, n_files) {
  n_flagged_files <- length(unique(findings_df$source_file))
  tbl <- findings_df |>
    dplyr::transmute(File = .data$source_file, Sheet = .data$label,
                     Issue = .data$check, Detail = .data$detail)
  c("#### Spreadsheet Formatting",
    sprintf("We examined %d spreadsheet file%s in the repository; %d %s at least one formatting issue.",
            n_files, plural(n_files), n_flagged_files,
            if (n_flagged_files == 1) "has" else "have"),
    scroll_table(tbl, maxrows = 20),
    "Spreadsheet formatting such as colour, merged cells, and blank rows/columns is not preserved when data are read programmatically or exported to CSV. Store data as a plain rectangular table (one header row, one column per variable, no colour-encoded meaning) so it is machine-readable.")
}

# Detect an OFFSET HEADER: a banner / blank / units / repeated-label row sitting
# ABOVE the real column header, so the file does not read as a clean rectangular
# table (the reader takes the junk row as the header and invents …N names, or
# spreads one label — CDA merged across 110 columns — into CDA…1 … CDA…110).
#
# Reuses the same detector as the read-time repair (data_promote_header_row /
# .detect_header_row) so the flag and the repair cannot disagree about where the
# header is. Returns NULL when the header is already row 1, else a one-line human
# description of what sits above it, for the researcher to remove at source.
.dv_spreadsheet_offset_header <- function(path, ext = tolower(tools::file_ext(path))) {
  raw <- if (ext %in% c("ods", "fods")) {
    # readODS is in Suggests: without it an .ods simply skips this check (the
    # xml2-based style checks still run), rather than erroring.
    if (!requireNamespace("readODS", quietly = TRUE)) return(NULL)
    # No `range=`: a fixed range like "A1:Z6" silently TRUNCATES the sheet to 26
    # columns, which would hide the header on a wide export (a 30-column sheet
    # reads back as 26). Read the sheet and cap the rows in R instead.
    tryCatch({
      d <- as.data.frame(suppressWarnings(readODS::read_ods(
        path, col_names = FALSE, .name_repair = "minimal")))
      utils::head(d, 6L)
    }, error = function(e) NULL)
  } else {
    tryCatch(as.data.frame(suppressWarnings(readxl::read_excel(
      path, col_names = FALSE, n_max = 6L, col_types = "text",
      .name_repair = "minimal"))), error = function(e) NULL)
  }
  if (is.null(raw) || nrow(raw) < 2 || ncol(raw) < 2) return(NULL)
  rows <- lapply(seq_len(nrow(raw)), function(i) as.character(raw[i, , drop = TRUE]))
  det  <- .detect_header_row(rows)
  if (det$header_row <= 1L || length(det$stripped) == 0) return(NULL)

  # Describe each stripped row: a repeated banner ("CDA" × 110), a near-empty
  # spacer, or a stale placeholder header. Keep it short and concrete.
  describe <- function(v) {
    vals <- trimws(as.character(v)); nz <- vals[nzchar(vals) & !is.na(vals)]
    if (length(nz) == 0) return("an empty row")
    dup <- .row_duplication(v)
    if (dup >= 0.6 && length(unique(nz)) <= 3)
      return(sprintf("\"%s\" repeated across %d column%s",
                     paste(unique(nz), collapse = "\"/\""),
                     length(nz), plural(length(nz))))
    if (mean(.is_placeholder_name(v)) >= 0.5)
      return("a row of placeholder names from an earlier mis-read")
    sprintf("a partial label row (%s%s)",
            paste(utils::head(nz, 3), collapse = ", "),
            if (length(nz) > 3) ", …" else "")
  }
  descr <- vapply(det$stripped, describe, character(1))
  list(header_row = det$header_row, n_above = length(det$stripped),
       detail = paste(descr, collapse = "; then "))
}

# Inspect one .xlsx file by reading it as a zip of XML parts. Returns a list with
# `sheets`, each a list(name, color_cells, merges, empty_rows, empty_cols), or
# NULL on failure. Uses only xml2 (no readxl/openxlsx dependency for the
# style-level checks); the empty-row/column checks read cell values from the
# sheet XML directly.
.dv_excel_inspect <- function(path) {
  if (!file.exists(path)) return(NULL)
  tmp <- tempfile("xlsx_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ok <- tryCatch({ utils::unzip(path, exdir = tmp); TRUE },
                 error = function(e) FALSE)
  if (!ok) return(NULL)

  xl <- file.path(tmp, "xl")
  if (!dir.exists(xl)) return(NULL)

  # Which cell style indices (s=) reference a non-default fill colour?
  colored_styles <- .dv_excel_colored_styles(file.path(xl, "styles.xml"))

  # Sheet name + file mapping (workbook order matches sheetN.xml order well
  # enough for reporting; fall back to the file stem).
  wb_path <- file.path(xl, "workbook.xml")
  sheet_names <- if (file.exists(wb_path)) {
    wb <- tryCatch(xml2::read_xml(wb_path), error = function(e) NULL)
    if (is.null(wb)) character(0) else {
      xml2::xml_attr(xml2::xml_find_all(wb, ".//*[local-name()='sheet']"), "name")
    }
  } else character(0)

  sheet_files <- sort(list.files(file.path(xl, "worksheets"),
                                 pattern = "^sheet[0-9]+\\.xml$",
                                 full.names = TRUE))
  sheets <- list()
  for (j in seq_along(sheet_files)) {
    nm <- if (j <= length(sheet_names)) sheet_names[[j]] else
      tools::file_path_sans_ext(basename(sheet_files[[j]]))
    sheets[[length(sheets) + 1L]] <-
      .dv_excel_inspect_sheet(sheet_files[[j]], nm, colored_styles)
  }

  list(sheets = sheets)
}

# Return the 0-based cell-style indices (positions in cellXfs) whose fill is a
# non-default colour. Default fills are patternType none/gray125 or black/white
# fgColor; anything else counts as colour coding.
.dv_excel_colored_styles <- function(styles_path) {
  if (!file.exists(styles_path)) return(integer(0))
  st <- tryCatch(xml2::read_xml(styles_path), error = function(e) NULL)
  if (is.null(st)) return(integer(0))

  fills <- xml2::xml_find_all(st, ".//*[local-name()='fills']/*[local-name()='fill']")
  fill_is_color <- vapply(fills, function(fl) {
    fg <- xml2::xml_find_first(fl, ".//*[local-name()='fgColor']")
    if (inherits(fg, "xml_missing")) return(FALSE)
    rgb <- xml2::xml_attr(fg, "rgb")
    # A themed colour (no rgb) or a plain black/white fill is not "colour coding".
    !is.na(rgb) && nzchar(rgb) &&
      !toupper(rgb) %in% c("FF000000", "FFFFFFFF", "00000000")
  }, logical(1))
  colored_fill_ids <- which(fill_is_color) - 1L    # 0-based fillId

  if (length(colored_fill_ids) == 0) return(integer(0))

  xfs <- xml2::xml_find_all(st, ".//*[local-name()='cellXfs']/*[local-name()='xf']")
  xf_fill <- suppressWarnings(as.integer(xml2::xml_attr(xfs, "fillId")))
  which(xf_fill %in% colored_fill_ids) - 1L         # 0-based style index (s=)
}

# Inspect one worksheet XML: count colour-coded cells, merged ranges, fully
# empty rows, and empty/unnamed columns. `colored_styles` is the set of 0-based
# style indices that use a colour fill.
#
# All cell-level facts are extracted with a handful of whole-document xml2 calls
# and then reduced with base-R vector ops. A previous version ran an XPath query
# per cell to test whether it held a value, which is O(rows x cols) XPath
# evaluations — minutes on a wide (e.g. 300 x 2000) Qualtrics export. Here the
# populated cells come from a single `.//c[v|is]` query, and row/column identity
# is parsed from the vector of cell references.
.dv_excel_inspect_sheet <- function(sheet_path, name, colored_styles) {
  blank <- list(name = name, color_cells = 0L, merges = character(0),
                empty_rows = 0L, empty_cols = 0L)
  sh <- tryCatch(xml2::read_xml(sheet_path), error = function(e) NULL)
  if (is.null(sh)) return(blank)

  # Every cell, and its reference (e.g. "B12"). One query for all cells.
  cells    <- xml2::xml_find_all(sh, ".//*[local-name()='c']")
  cell_ref <- xml2::xml_attr(cells, "r")

  # Populated cells: those with a <v> (value) or <is> (inline string) child.
  # One query returns exactly the non-empty cells, avoiding per-cell XPath.
  val_cells <- xml2::xml_find_all(
    sh, ".//*[local-name()='c'][*[local-name()='v'] or *[local-name()='is']]")
  val_ref <- xml2::xml_attr(val_cells, "r")

  # Colour-coded cells: cells whose style index is in colored_styles.
  color_cells <- 0L
  if (length(colored_styles) > 0 && length(cells) > 0) {
    cell_s <- suppressWarnings(as.integer(xml2::xml_attr(cells, "s")))
    color_cells <- sum(cell_s %in% colored_styles, na.rm = TRUE)
  }

  # Merged ranges.
  merges <- xml2::xml_attr(
    xml2::xml_find_all(sh, ".//*[local-name()='mergeCell']"), "ref")
  merges <- merges[!is.na(merges)]

  # Split cell references into column letters and row numbers (vectorised).
  ref_col <- function(ref) sub("[0-9]+$", "", ref)
  ref_row <- function(ref) suppressWarnings(as.integer(sub("^[A-Za-z]+", "", ref)))
  val_col <- ref_col(val_ref)
  val_rownum <- ref_row(val_ref)

  # Empty rows: rows inside the populated range that carry no value. Only blanks
  # that fall between the first and last populated row are counted (trailing
  # blank <row> elements in the XML are rare and not meaningful).
  empty_rows <- 0L
  if (length(val_rownum) > 0) {
    populated <- sort(unique(val_rownum))
    if (length(populated) > 1)
      empty_rows <- (max(populated) - min(populated) + 1L) - length(populated)
  }

  # Empty / unnamed columns. The header is the first populated row; a column is
  # problematic if its header cell is blank, or it has a header but no value in
  # any row below. Column identity is the letter part of the cell reference.
  empty_cols <- 0L
  if (length(val_ref) > 0) {
    hdr_row  <- min(val_rownum)
    hdr_cols <- val_col[val_rownum == hdr_row]                 # cols with a header value
    all_cols <- unique(ref_col(cell_ref))                      # every column that appears
    body_cols <- unique(val_col[val_rownum > hdr_row])         # cols with a body value
    blank_header  <- setdiff(all_cols, hdr_cols)               # column present but no header
    header_no_body <- setdiff(hdr_cols, body_cols)             # header but empty below
    empty_cols <- length(unique(c(blank_header, header_no_body)))
  }

  list(name = name, color_cells = color_cells, merges = merges,
       empty_rows = empty_rows, empty_cols = empty_cols)
}

# ── OpenDocument (.ods / .fods) ───────────────────────────────────────────────
#
# ODF stores the same facts as OOXML but in a structurally different way, so the
# .dv_excel_* parsers above cannot simply be pointed at it:
#
#   * ALL sheets live in one content.xml (not one sheetN.xml per sheet);
#   * cells carry NO reference attribute (no r="B12") — position is IMPLICIT in
#     document order, so row/column indices must be reconstructed by counting;
#   * blank runs are COMPRESSED: `table:number-columns-repeated="3"` stands for
#     three cells and `table:number-rows-repeated="5"` for five rows. Counting
#     elements naively would report one empty row where there are five;
#   * colour is `fo:background-color` on a named cell style, not a fillId;
#   * merges are `table:number-columns-spanned`/`-rows-spanned` on the anchor
#     cell (followed by <table:covered-table-cell> placeholders), not a
#     ready-made "A1:B1" range string — so the range label is synthesised here
#     to match the xlsx report wording.
#
# Returns the SAME shape as .dv_excel_inspect(): list(sheets = list(list(name,
# color_cells, merges, empty_rows, empty_cols))), so the module body treats the
# two formats identically.
.dv_ods_inspect <- function(path) {
  if (!file.exists(path)) return(NULL)

  # .fods is a single flat XML file; .ods is a zip whose content.xml holds it.
  ext <- tolower(tools::file_ext(path))
  doc <- if (identical(ext, "fods")) {
    tryCatch(xml2::read_xml(path), error = function(e) NULL)
  } else {
    tmp <- tempfile("ods_")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    ok <- tryCatch({ utils::unzip(path, exdir = tmp); TRUE },
                   error = function(e) FALSE)
    if (!ok) return(NULL)
    cx <- file.path(tmp, "content.xml")
    if (!file.exists(cx)) return(NULL)
    tryCatch(xml2::read_xml(cx), error = function(e) NULL)
  }
  if (is.null(doc)) return(NULL)

  colored <- .dv_ods_colored_styles(doc)

  # One <table:table> per sheet, in workbook order.
  tables <- xml2::xml_find_all(doc, ".//*[local-name()='table']")
  # Guard: nested tables (a table inside a cell) would double-count. Keep only
  # tables whose parent is the spreadsheet body.
  keep <- vapply(tables, function(tb) {
    p <- xml2::xml_parent(tb)
    identical(xml2::xml_name(p), "spreadsheet")
  }, logical(1))
  tables <- tables[keep]
  if (length(tables) == 0) return(list(sheets = list()))

  sheets <- lapply(seq_along(tables), function(j) {
    nm <- xml2::xml_attr(tables[[j]], "name")
    if (is.na(nm) || !nzchar(nm)) nm <- paste0("Sheet", j)
    .dv_ods_inspect_sheet(tables[[j]], nm, colored)
  })
  list(sheets = sheets)
}

# Names of cell styles whose background is a real colour. Mirrors
# .dv_excel_colored_styles(): a style counts only if it sets an explicit
# background that is not the default/neutral (transparent, white, black). Both
# the automatic styles (used by cells) and any common styles are scanned.
.dv_ods_colored_styles <- function(doc) {
  sty <- xml2::xml_find_all(
    doc, ".//*[local-name()='style'][@*[local-name()='family']='table-cell']")
  if (length(sty) == 0) return(character(0))
  nm <- xml2::xml_attr(sty, "name")
  bg <- vapply(sty, function(s) {
    p <- xml2::xml_find_first(s, ".//*[local-name()='table-cell-properties']")
    if (inherits(p, "xml_missing")) return(NA_character_)
    xml2::xml_attr(p, "background-color")
  }, character(1))
  is_col <- !is.na(bg) & nzchar(bg) &
    !tolower(bg) %in% c("transparent", "none", "#ffffff", "#fff",
                        "#000000", "#000")
  nm[is_col & !is.na(nm)]
}

# Convert a 1-based column index to spreadsheet letters (1 -> A, 27 -> AA), so
# merged ranges can be reported as "A1:B1" exactly like the xlsx path.
.dv_ods_col_letter <- function(i) {
  out <- character(length(i))
  for (k in seq_along(i)) {
    n <- i[[k]]; s <- ""
    while (n > 0) {
      r <- (n - 1L) %% 26L
      s <- paste0(LETTERS[r + 1L], s)
      n <- (n - 1L) %/% 26L
    }
    out[[k]] <- s
  }
  out
}

# Inspect one <table:table>. Walks rows/cells in document order, expanding the
# repeat counters, and records for each populated cell its (row, col) position —
# reconstructing the coordinates OOXML gives for free. Downstream logic then
# matches .dv_excel_inspect_sheet() exactly.
.dv_ods_inspect_sheet <- function(tbl, name, colored) {
  blank <- list(name = name, color_cells = 0L, merges = character(0),
                empty_rows = 0L, empty_cols = 0L)

  rows <- xml2::xml_find_all(tbl, "./*[local-name()='table-row']")
  if (length(rows) == 0) return(blank)

  int_attr <- function(node, a, default = 1L) {
    v <- suppressWarnings(as.integer(xml2::xml_attr(node, a)))
    if (is.na(v) || v < 1L) default else v
  }

  val_row <- integer(0); val_col <- integer(0)   # populated cell coordinates
  seen_col <- integer(0)                          # every column that appears
  color_cells <- 0L
  merges <- character(0)
  r <- 0L   # 1-based row cursor

  # A trailing run of empty rows is padding (LibreOffice writes rows out to the
  # sheet limit, e.g. number-rows-repeated="1048570"); it is not data structure,
  # and the empty-row count below only looks BETWEEN populated rows anyway.
  for (ri in seq_along(rows)) {
    row <- rows[[ri]]
    rep_r <- int_attr(row, "number-rows-repeated")
    cells <- xml2::xml_find_all(
      row, "./*[local-name()='table-cell' or local-name()='covered-table-cell']")

    cc <- 0L   # 1-based column cursor within this row
    for (ci in seq_along(cells)) {
      cell  <- cells[[ci]]
      rep_c <- int_attr(cell, "number-columns-repeated")
      # Populated = has a value type or any text content (matches the xlsx rule
      # of "<v> or <is> present").
      vt <- xml2::xml_attr(cell, "value-type")
      txt <- trimws(xml2::xml_text(cell))
      populated <- (!is.na(vt) && nzchar(vt)) || nzchar(txt)

      # A huge repeat count on an EMPTY cell is right-padding to the sheet limit;
      # it does not mean thousands of real columns. Only count padding runs when
      # the cell actually holds something.
      span_c <- if (populated) rep_c else min(rep_c, 1024L)

      idx <- cc + seq_len(span_c)
      seen_col <- c(seen_col, idx)

      if (populated) {
        # The same value repeated across `rep_c` columns occupies each of them.
        for (rr in seq_len(rep_r)) {
          val_row <- c(val_row, rep(r + rr, span_c))
          val_col <- c(val_col, idx)
        }
        sn <- xml2::xml_attr(cell, "style-name")
        if (!is.na(sn) && sn %in% colored)
          color_cells <- color_cells + (span_c * rep_r)
      } else {
        sn <- xml2::xml_attr(cell, "style-name")
        # A colour-filled but EMPTY cell still encodes information visually
        # (a shaded block marking a group), so it counts — but only when the
        # run is a plausible real range, not sheet-limit padding.
        if (!is.na(sn) && sn %in% colored)
          color_cells <- color_cells + (span_c * rep_r)
      }

      # Merge anchor: spans recorded on the cell that starts the range.
      sc <- suppressWarnings(as.integer(
        xml2::xml_attr(cell, "number-columns-spanned")))
      sr <- suppressWarnings(as.integer(
        xml2::xml_attr(cell, "number-rows-spanned")))
      sc <- if (is.na(sc)) 1L else sc
      sr <- if (is.na(sr)) 1L else sr
      if (sc > 1L || sr > 1L) {
        merges <- c(merges, sprintf(
          "%s%d:%s%d",
          .dv_ods_col_letter(cc + 1L), r + 1L,
          .dv_ods_col_letter(cc + sc), r + sr))
      }

      cc <- cc + span_c
    }
    r <- r + rep_r
  }

  # Empty rows: blank rows BETWEEN the first and last populated row (identical
  # rule to the xlsx path — trailing padding is not counted).
  empty_rows <- 0L
  if (length(val_row) > 0) {
    populated <- sort(unique(val_row))
    if (length(populated) > 1)
      empty_rows <- (max(populated) - min(populated) + 1L) - length(populated)
  }

  # Empty / unnamed columns: header is the first populated row; a column is
  # problematic if it has no header value, or a header but nothing below it.
  empty_cols <- 0L
  if (length(val_col) > 0) {
    hdr_row  <- min(val_row)
    hdr_cols <- unique(val_col[val_row == hdr_row])
    body_cols <- unique(val_col[val_row > hdr_row])
    # Only consider columns within the used range; padding beyond the last
    # populated column is not a missing column.
    all_cols <- unique(seen_col[seen_col <= max(val_col)])
    blank_header   <- setdiff(all_cols, hdr_cols)
    header_no_body <- setdiff(hdr_cols, body_cols)
    empty_cols <- length(unique(c(blank_header, header_no_body)))
  }

  list(name = name, color_cells = color_cells, merges = merges,
       empty_rows = empty_rows, empty_cols = empty_cols)
}
