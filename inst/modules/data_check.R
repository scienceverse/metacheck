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
#' @param file_limit the maximum number of tabular data files to column-extract
#'   per repository (guards against hundreds of per-participant files)
#' @param download if TRUE (default), download the readable files (tabular data
#'   and codebook/README) from online repositories (OSF/GitHub/Zenodo) into a
#'   shared cache so their contents can be analysed. Downloads are reused on
#'   later runs. Set FALSE to only classify files by name without downloading.
#' @param max_file_size largest single file to download, in MB (default 10);
#'   larger files are skipped and reported
#' @param max_download_size largest total download per repository, in MB
#'   (default 100); once exceeded, the largest remaining files are skipped
#' @param model the LLM model name (see `llm_model_list()`) used only when
#'   `llm_use(TRUE)`
#' @param params a named list passed to `llm()` (e.g., `list(seed = 123)`),
#'   used only when `llm_use(TRUE)`
#'
#' @returns a list
data_check <- function(paper, local_path = NULL, local_only = FALSE,
                       file_limit = 30,
                       download = TRUE,
                       max_file_size = 10,
                       max_download_size = 100,
                       model = llm_model(),
                       params = list()) {

  repo_tree_lines <- function(paths) {
    paths <- unique(paths[!is.na(paths) & nzchar(paths)])
    if (length(paths) == 0) return(character(0))
    parts_list <- strsplit(gsub("\\\\", "/", paths), "/", fixed = FALSE)
    parts_list <- lapply(parts_list, function(x) x[nzchar(x)])
    lines <- character(0)

    walk <- function(parts_subset, prefix = "") {
      heads <- vapply(parts_subset, function(x) x[[1]], character(1))
      head_order <- unique(heads)
      has_child <- vapply(head_order, function(head) {
        idx <- heads == head
        any(vapply(parts_subset[idx], length, integer(1)) > 1)
      }, logical(1))
      head_order <- head_order[order(!has_child, tolower(head_order))]

      for (i in seq_along(head_order)) {
        head <- head_order[[i]]
        idx <- heads == head
        tails <- lapply(parts_subset[idx], function(x) x[-1])
        is_last <- i == length(head_order)
        child_exists <- any(vapply(tails, length, integer(1)) > 0)
        branch <- if (is_last) "└── " else "├── "
        next_prefix <- paste0(prefix, if (is_last) "    " else "│   ")
        lines <<- c(lines, paste0(prefix, branch, head, if (child_exists) "/" else ""))
        next_subset <- tails[vapply(tails, length, integer(1)) > 0]
        if (length(next_subset) > 0) walk(next_subset, next_prefix)
      }
    }

    walk(parts_list)
    lines
  }

  repo_tree_block <- function(files) {
    if (is.null(files) || nrow(files) == 0) return(NULL)
    repo_urls <- unique(files$repo_url[!is.na(files$repo_url) & nzchar(files$repo_url)])
    if (length(repo_urls) == 0) return(NULL)

    blocks <- vapply(repo_urls, function(repo) {
      sub <- files[files$repo_url == repo, , drop = FALSE]
      rel_paths <- if ("file_path" %in% names(sub)) sub$file_path else sub$file_name
      rel_paths <- rel_paths[!is.na(rel_paths) & nzchar(rel_paths)]
      tree <- repo_tree_lines(rel_paths)
      # Collapse each repo's block into one string so the tree lines stay
      # together; module_report() joins report elements with "\n\n", which
      # would otherwise blank-line-separate every tree line.
      paste(
        c(
          paste0("Repository: ", repo),
          "```",
          if (length(tree) > 0) tree else "(no file tree available)",
          "```"
        ),
        collapse = "\n"
      )
    }, character(1))

    collapse_section(
      c("The tree below shows where files sit within each repository, using the relative paths available from repo_check.",
        blocks),
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
  }

  # ── 2. Classify every file into a data_check semantic type ───────────────────
  if (nrow(all_files) == 0) {
    return(list(
      traffic_light = "na",
      summary_text = "We found no files to analyse.",
      summary_table = data.frame(
        paper_id = .pid(all_files),
        data_file_n = 0, column_n = 0
      )
    ))
  }

  all_files$data_type <- data_classify_files(all_files$file_name)
  ext <- tolower(tools::file_ext(all_files$file_name))
  all_files$data_format <- ifelse(all_files$data_type == "data",
                                  data_format(ext), NA_character_)
  # Study-group assignment (ex1 / pilot2 / shared) is only attempted with an
  # LLM; without one it stays NA ("unknown"), and psychds_check reports that
  # subgrouping could not be detected.
  all_files$group <- NA_character_

  llm_file_updates <- 0L
  llm_col_updates <- 0L
  llm_group_updates <- 0L
  llm_model_used <- NA_character_

  # Optional LLM pass for files still unresolved after rules.
  if (llm_use()) {
    amb_files <- which(all_files$data_type == "other")
    if (length(amb_files) > 0) {
      file_text <- vapply(amb_files, function(i) {
        fname <- all_files$file_name[[i]] %||% ""
        ext_i <- tolower(tools::file_ext(fname))
        sprintf("file_name: %s\nextension: %s", fname, ifelse(nzchar(ext_i), ext_i, "none"))
      }, character(1))

      file_prompt <- paste(
        "Classify each file into one type:",
        "codebook, software, output, supplemental, asset, other.",
        "Return one result per numbered input line, echoing its index and the",
        "best single type as `value`. Use 'other' when uncertain."
      )

      file_levels <- c("codebook", "software", "output", "supplemental",
                       "asset", "other")
      pred <- .llm_classify_batched(file_text, file_prompt,
                                    value_desc = "Best single semantic file type",
                                    valid = file_levels,
                                    model = model, params = params)
      if (is.na(llm_model_used))
        llm_model_used <- attr(pred, "llm_model") %||% NA_character_

      ok <- !is.na(pred)
      if (any(ok)) {
        all_files$data_type[amb_files[ok]] <- pred[ok]
        llm_file_updates <- sum(ok)
      }
    }

    # Study-group pass: classify each analysable file into a study group from
    # its path context (folder + name). data_group_llm skips assets and batches
    # the rest, so this scales to large repositories. Only runs with an LLM;
    # group stays NA otherwise.
    grp <- data_group_llm(all_files, model = model, params = params)
    if (!is.null(grp)) {
      all_files$group <- grp$group
      llm_group_updates <- sum(!is.na(grp$group) & grp$group != "shared")
      if (is.na(llm_model_used)) llm_model_used <- grp$model %||% NA_character_
    }
  }

  # ── 2c. Download the files this module (and codebook_check) will read ─────────
  # repo_check lists OSF/GitHub/Zenodo files without fetching them. Download the
  # readable subset (tabular data + codebook/readme) into the shared cache so
  # their contents can be analysed; assets and non-tabular data are skipped.
  omitted_files <- NULL
  if (isTRUE(download)) {
    want <- (all_files$data_type == "data" &
               !is.na(all_files$data_format) & all_files$data_format == "tabular") |
            all_files$data_type %in% c("codebook", "readme")
    need_dl <- want &
      (is.na(all_files$file_location) | !nzchar(all_files$file_location %||% "")) &
      !is.na(all_files$file_url) & nzchar(all_files$file_url %||% "")
    if (any(need_dl)) {
      dl <- download_repo_files(all_files[need_dl, , drop = FALSE],
                                max_file_size = max_file_size,
                                max_download_size = max_download_size)
      all_files$file_location[need_dl] <- dl$file_location
      omitted_files <- attr(dl, "omitted")
    }
  }

  # ── 3. Select tabular data files that are available locally ──────────────────
  is_tabular_data <- all_files$data_type == "data" &
    !is.na(all_files$data_format) & all_files$data_format == "tabular"
  has_local <- !is.na(all_files$file_location) &
    nzchar(all_files$file_location) &
    file.exists(all_files$file_location %||% "")

  data_files <- all_files[is_tabular_data & has_local, , drop = FALSE]

  # cap per repo to avoid runaway extraction
  if (nrow(data_files) > 0 && is.finite(file_limit)) {
    data_files <- dplyr::slice_head(data_files, n = file_limit, by = repo_url)
  }

  n_tabular_all <- sum(is_tabular_data)
  n_no_local <- sum(is_tabular_data & !has_local)

  # File names detected as manifests (a table-of-contents listing other repo
  # files) rather than real data; demoted to "supplemental" after extraction.
  manifest_files <- character(0)
  # .RData/.rda workspaces that held no reusable tabular data (only fitted
  # models / session objects, or could not be restored). Flagged as a
  # data-sharing recommendation: share the underlying data as CSV + codebook.
  workspace_files <- character(0)

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

      file_previews[[f$file_name]] <<- df

      # Describe each column as orthogonal facets (DDI-style) instead of a single
      # col_type: how it is stored (representation), its measurement level, what
      # it measures (concept), how it functions (role), its unit and data-quality
      # state. See data_col_facets() in data_check_helpers.R.
      cls <- lapply(seq_along(df), function(j) data_col_facets(names(df)[j], df[[j]]))
      getf <- function(field) vapply(cls, function(c) {
        v <- c[[field]]; if (is.null(v) || is.na(v)) NA_character_ else v
      }, character(1))
      is_numeric <- vapply(cls, function(c) isTRUE(c$is_numeric), logical(1))

      # Qualtrics response-metadata columns get a concept tag from the export's
      # reserved names (StartDate, Duration, Finished, ...), which is a stronger
      # signal than the value-based rules for those columns.
      concept <- getf("concept")
      if (data_check_is_qualtrics(df)) {
        qtags <- .qualtrics_tag_cols(names(df))
        concept[!is.na(qtags)] <- qtags[!is.na(qtags)]
      }

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
        sample_values     = sample_vals,
        stats_mat
      )
    })
    columns_df <- dplyr::bind_rows(Filter(Negate(is.null), per_file))

    # Demote any detected manifests to supplemental and refresh the tabular
    # flags/counts so they are reported as supplemental, not data.
    if (length(manifest_files) > 0) {
      mrows <- all_files$file_name %in% manifest_files
      all_files$data_type[mrows]   <- "supplemental"
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
      gap_idx <- which(is.na(columns_df$concept) | columns_df$ambiguous %in% TRUE)
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
          valid = concept_levels, model = model, params = params)
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
            valid = lvl_levels, model = model, params = params)
          ok <- !is.na(pred_lvl)
          if (any(ok)) columns_df$measurement_level[lvl_idx[ok]] <- pred_lvl[ok]
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
    }
  }

  n_columns <- if (!is.null(columns_df)) nrow(columns_df) else 0L

  # ── 5. Reporting ─────────────────────────────────────────────────────────────
  type_counts <- table(factor(all_files$data_type, levels = .data_check_types))

  summary_files <- sprintf(
    "We classified %d file%s: %s.",
    nrow(all_files), plural(nrow(all_files)),
    paste(sprintf("%d %s", type_counts[type_counts > 0],
                  names(type_counts)[type_counts > 0]), collapse = ", ")
  )

  n_extracted <- nrow(data_files) - length(manifest_files)
  summary_data <- sprintf(
    "We found %d tabular data file%s and extracted %d column%s from %d of them.",
    n_tabular_all, plural(n_tabular_all),
    n_columns, plural(n_columns), n_extracted
  )

  # Files skipped by the download size caps: tell the user how to raise them.
  n_omitted <- if (!is.null(omitted_files)) nrow(omitted_files) else 0L
  summary_omitted <- if (n_omitted > 0) sprintf(
    paste0("%d file%s exceeded the download size limits and %s not analysed. ",
           "Raise `max_file_size` (currently %g MB per file) or ",
           "`max_download_size` (currently %g MB total) to include them."),
    n_omitted, plural(n_omitted), if (n_omitted == 1) "was" else "were",
    max_file_size, max_download_size
  ) else NULL

  # Tabular data files with no readable copy (download off, no URL, or fetch
  # failed). These are counted but not column-extracted.
  summary_nolocal <- if (n_no_local > 0) sprintf(
    paste0("%d tabular data file%s could not be read because %s not downloaded. ",
           "%s"),
    n_no_local, plural(n_no_local),
    if (n_no_local == 1) "it was" else "they were",
    if (isTRUE(download))
      "This can happen for private repositories or failed downloads; you can also pass `local_path` to point at a local copy."
    else
      "Set `download = TRUE`, or pass `local_path` to point at a local copy, to analyse them."
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

  # file inventory table
  file_tbl <- all_files |>
    dplyr::count(Type = data_type, name = "Files") |>
    dplyr::arrange(dplyr::desc(.data$Files))

  report <- c(
    "This module classifies repository files and, for tabular data files available locally, extracts each column's type and summary statistics.",
    "#### File Types",
    scroll_table(file_tbl, maxrows = 10)
  )

  tree_block <- repo_tree_block(all_files)
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
    report <- c(
      report,
      sprintf(
        "#### Descriptives Overview\n\nWe found %d column%s with at least one missing value.",
        n_missing_cols, plural(n_missing_cols)
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
    n_groups <- length(unique(stats::na.omit(all_files$group[
      !is.na(all_files$group) & all_files$group != "shared"])))
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

  summary_table <- data.frame(
    paper_id = .pid(all_files),
    data_file_n = n_tabular_all,
    column_n = n_columns
  ) |>
    dplyr::left_join(coltype_wide, by = "paper_id")

  summary_text <- c(summary_files, summary_data, summary_nolocal,
                     summary_omitted, summary_workspace) |>
    paste("\n- ", x = _, collapse = "")

  # ── 7. Return ────────────────────────────────────────────────────────────────
  list(
    table = columns_df %||% data.frame(),
    structure = all_files,          # per-file classification, for codebook_check
    previews = file_previews,       # full read data frames, for data_validate
    summary_table = summary_table,
    na_replace = c(data_file_n = 0, column_n = 0),
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}
