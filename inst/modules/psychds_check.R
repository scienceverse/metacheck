#' Psych-DS Check
#'
#' @description
#' This module checks how close a repository is to the
#' [Psych-DS](https://psych-ds.github.io/) machine-readable dataset standard. It
#' reports which required files are missing, which existing files are in the
#' wrong place, shows the file tree the repository *should* have (with missing
#' items highlighted), and gives concrete suggestions for reaching compliance.
#'
#' @details
#' Psych-DS 1.5 expects, at minimum, a root `dataset_description.json` metadata
#' file, a `data/` directory, and at least one `*_data.csv` file; it recommends
#' a `README`, a `CHANGES` file, and conventional subdirectories (`analysis/`,
#' `materials/`, `documentation/`, `documentation/codebooks/`). This module
#' compares those
#' expectations against the repository's actual files, using the classification
#' and columns from `data_check` and the variable documentation from
#' `codebook_check`.
#'
#' Each repository file is mapped to its Psych-DS destination by type: data →
#' `data/`, code → `analysis/`, materials → `materials/`, documentation →
#' `documentation/` (or `documentation/codebooks/` for `documentation` rows
#' whose fine-grained `doc_role` is `"codebook"`), unknown → `documentation/`,
#' the root readme (`doc_role == "readme"`) → root `README`. The module then
#' renders the target tree, marking files that are **present**, **missing**
#' (required but absent — shown in red), or **misplaced** (present but at the
#' wrong path — shown at the target location annotated with their current
#' location).
#'
#' `data_check` assigns every file except the collection-level root README/
#' `ro-crate-metadata.json` to exactly one study group — deterministically
#' where path/repository/code-reference evidence allows, and via an LLM only
#' for the residual cases (see `data_group_llm()`). A multi-study repository is
#' modelled with a `study-<group>/` directory per study (each a complete
#' Psych-DS dataset); only the root readme and ro-crate metadata sit at the
#' collection root beside them. When no evidence at all names a study (no
#' path/repository split and no LLM), a single-rooted tree is shown together
#' with a note that subgrouping could not be detected. A materials or
#' documentation file genuinely reused across studies is still owned by
#' exactly one study; the others get a reference to it (not a copy) written
#' into their own metadata by `convert_psychds()`.
#'
#' This module only *checks* compliance; it does not modify the repository. The
#' report points to a dedicated builder for generating a compliant copy.
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object, or NULL to check local
#'   files only (see [test_paper()])
#' @param local_path optional path to a local directory, passed through to
#'   `data_check` / `repo_check` when their output is not already available
#' @param local_only if TRUE, skip online repository lookups (see `repo_check`)
#' @param model the LLM model name (see `llm_model_list()`) used only when
#'   `llm_use(TRUE)`
#' @param params a named list passed to `llm()`, used only when `llm_use(TRUE)`
#'
#' @returns a list
psychds_check <- function(paper, local_path = NULL, local_only = FALSE,
                          model = llm_model(),
                          params = list()) {

  # File-type → Psych-DS subdirectory (data and readme handled separately, by
  # doc_role, below).
  type_to_subdir <- c(
    code          = "analysis",
    materials     = "materials",
    output        = "outputs",
    documentation = "documentation",
    unknown       = "documentation"
  )

  .pid <- function(...) {
    id <- paper_id(paper)
    for (df in list(...)) {
      if (length(id) > 0) break
      if (!is.null(df) && "paper_id" %in% names(df)) id <- unique(df$paper_id)
    }
    if (length(id) == 0) NA_character_ else id[[1]]
  }

  # Psych-DS keyword values are alphanumeric (key-value_..._data.csv). Slugify a
  # filename stem: lowercase, non-alphanumeric runs → nothing, spaces dropped.
  keyword_slug <- function(x) {
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "", x)
    x
  }

  # ── 1. Inputs from data_check (+ codebook_check for documentation) ───────────
  structure_df <- get_prev_outputs("data_check", "structure")
  columns_df   <- get_prev_outputs("data_check", "table")
  group_no_evidence <- get_prev_outputs("data_check", "group_no_evidence")
  if (is.null(structure_df)) {
    mo <- if (!is.null(local_path)) {
      module_run(paper, "data_check", local_path = local_path,
                 local_only = local_only, model = model, params = params)
    } else {
      module_run(paper, "data_check", local_only = local_only,
                 model = model, params = params)
    }
    structure_df <- mo$structure
    columns_df   <- mo$table
    group_no_evidence <- mo$group_no_evidence
  }
  group_no_evidence <- isTRUE(group_no_evidence)
  labels_df <- get_prev_outputs("codebook_check", "table")

  empty <- function(text) {
    list(
      table = data.frame(),
      summary_table = data.frame(
        paper_id = .pid(structure_df, columns_df),
        required_met = 0L, required_missing = 0L,
        recommended_met = 0L, recommended_missing = 0L,
        misplaced_n = 0L
      ),
      na_replace = c(required_met = 0, required_missing = 0,
                     recommended_met = 0, recommended_missing = 0,
                     misplaced_n = 0),
      traffic_light = "na",
      summary_text = text
    )
  }

  if (is.null(structure_df) || nrow(structure_df) == 0)
    return(empty("We found no repository files to check for Psych-DS compliance."))

  n_files <- nrow(structure_df)

  # ── 2. Do study groups exist? ────────────────────────────────────────────────
  # Every file except the collection-level root README/ro-crate-metadata.json
  # resolves to exactly one study — there is no "shared" bucket to filter out;
  # those root files simply carry group = NA (see data_check.R).
  groups <- if ("group" %in% names(structure_df))
    structure_df$group else rep(NA_character_, n_files)
  doc_role <- if ("doc_role" %in% names(structure_df))
    structure_df$doc_role else rep(NA_character_, n_files)
  study_groups <- unique(groups[!is.na(groups)])
  have_groups  <- length(study_groups) > 0
  multi_study  <- length(study_groups) > 1

  # ── 3. Map each file to its Psych-DS target path ─────────────────────────────
  # data files → data/<...>_data.csv; readme → README; everything else → its
  # type subdirectory. Study prefix is added when groups exist.
  is_data <- !is.na(structure_df$data_type) & structure_df$data_type == "data"

  # Only a repository with >=2 detected study groups uses the study-<group>/
  # layout; a single study (or unknown grouping) is a flat single dataset.
  #
  # Files that belong to a specific study go under study-<group>/ (a complete,
  # valid Psych-DS dataset). Only the root README/ro-crate-metadata.json (group
  # is NA by construction — see data_check.R) get NO study prefix: they live at
  # the dataset root, beside the study-*/ folders. This follows BIDS
  # (collection-level content sits at the root, never in a pseudo-subject like
  # sub-shared/) and keeps every study-*/ a real dataset.
  target_of <- function(i) {
    dt   <- structure_df$data_type[i] %||% "unknown"
    role <- doc_role[i]
    name <- basename(gsub("\\\\", "/", structure_df$file_name[i]))
    grp  <- groups[i]
    prefix <- if (multi_study && !is.na(grp))
      paste0("study-", grp, "/") else ""

    if (dt == "data") {
      stem <- keyword_slug(tools::file_path_sans_ext(name))
      if (!nzchar(stem)) stem <- paste0("file", i)
      # Every data file gets a Psych-DS *_data.csv target. When the source is
      # NOT already a CSV (xlsx/sav/dta/...), the converter writes a real CSV
      # here (not a renamed copy of the original) AND keeps the original file
      # beside it (see original_target_of); see convert_psychds().
      paste0(prefix, "data/source-", stem, "_data.csv")
    } else if (dt == "documentation" && !is.na(role) && role == "readme") {
      # The root readme/ro-crate-metadata.json never carries a study prefix
      # (grp is NA for these rows by construction); a PER-STUDY readme (rare,
      # but possible if a study's own folder has its own README) still gets one.
      ext <- tools::file_ext(name)
      paste0(prefix, if (nzchar(ext)) paste0("README.", ext) else "README")
    } else if (dt == "documentation" && !is.na(role) && role == "license") {
      # A LICENSE is collection-level, same as the readme: one licence covers
      # the whole deposit, so it goes at the archive root with no study prefix.
      ext <- tools::file_ext(name)
      paste0(prefix, if (nzchar(ext)) paste0("LICENSE.", ext) else "LICENSE")
    } else {
      # Single-bracket lookup: an unmapped data_type returns NA rather than
      # throwing "subscript out of bounds" as `[[` would.
      sub <- unname(type_to_subdir[dt])
      if (is.na(sub)) sub <- "documentation"
      paste0(prefix, sub, "/", name)
    }
  }
  target_path  <- vapply(seq_len(n_files), target_of, character(1))
  current_path <- gsub("\\\\", "/", structure_df$file_path %||% structure_df$file_name)
  current_path <- ifelse(is.na(current_path), structure_df$file_name, current_path)
  # target_of() always returns a real path now (consumed archive containers
  # never reach this table at all — data_check.R drops those rows once their
  # contents are extracted, rather than keeping a placeholder row for them).
  # This guard is kept defensively in case a future data_type slips through
  # unmapped; it should never trigger in practice.
  is_excluded  <- is.na(target_path)
  misplaced    <- !is_excluded & current_path != target_path

  # A TABULAR data file whose source is not already a CSV (xlsx/xls/ods/tsv/dat/
  # sav/dta/sas7bdat/jasp/omv/rds/rdata) is CONVERTED to CSV for its _data.csv
  # target (rather than having its bytes renamed, which would be an invalid
  # CSV), and its ORIGINAL kept alongside so the release retains the authored
  # artifact (an .xlsx carries formatting/sheets, a .sav/.dta carries value
  # labels). `convert` marks those rows; `original_target` is where the untouched
  # original goes (same data/ dir, original extension).
  #
  # Conversion is best-effort: a source that turns out to hold no table (an
  # .rdata of fitted models only) makes .psychds_write_data_csv() return FALSE,
  # and convert_psychds still copies the original to `original_target` — so the
  # file is never dropped from the release, it just arrives without a CSV.
  #
  # A RAW (non-tabular) data file — .npy/.h5/.pickle/.fif/... — cannot be read
  # as a table, so it is neither converted nor renamed to .csv: it is copied
  # with its true extension to a raw_target and does NOT claim a _data.csv path.
  src_ext        <- tolower(tools::file_ext(structure_df$file_name))
  # "Convertible" is asked of data_format(), the package's single source of
  # truth for what data_read_head() can parse — the same reader
  # .psychds_write_data_csv() uses to do the conversion. A hardcoded list here
  # would drift from the reader (it did: .ods/.fods were readable but copied
  # raw). A .csv needs no conversion, so it is excluded even though it is
  # tabular.
  needs_convert  <- is_data & src_ext != "csv" &
                    data_format(src_ext) == "tabular"
  is_raw_data    <- is_data & nzchar(src_ext) & src_ext != "csv" & !needs_convert

  # Convertible: keep the _data.csv target, add original alongside.
  original_target <- ifelse(
    needs_convert,
    paste0(sub("_data\\.csv$", "", target_path), ".", src_ext),
    NA_character_)
  # Raw: replace the (wrong) _data.csv target with the original extension, and
  # do not treat it as a CSV to write.
  raw_target <- ifelse(
    is_raw_data,
    paste0(sub("_data\\.csv$", "", target_path), ".", src_ext),
    NA_character_)
  target_path <- ifelse(is_raw_data, raw_target, target_path)

  # ── 4. Required / recommended compliance items ───────────────────────────────
  file_names_lc <- tolower(basename(current_path))
  has_dataset_desc <- any(file_names_lc == "dataset_description.json")
  has_data_files   <- any(is_data)
  has_readme       <- any(!is.na(doc_role) & doc_role == "readme")
  has_changes      <- any(grepl("^changes(\\.|$)", file_names_lc))

  # Are the data columns describable (so a valid variableMeasured could exist)?
  n_columns   <- if (!is.null(columns_df)) nrow(columns_df) else 0L
  describable <- n_columns > 0

  # Documentation coverage from codebook_check, if available.
  n_documented <- if (!is.null(labels_df) && "label_status" %in% names(labels_df))
    sum(labels_df$label_status %in% c("labelled", "llm")) else 0L

  required <- list(
    c(item = "dataset_description.json",
      met = has_dataset_desc,
      detail = "Root metadata file describing the dataset (required)."),
    c(item = "data/ directory with data files",
      met = has_data_files,
      detail = "At least one machine-readable data file under data/ (required)."),
    c(item = "describable variables (variableMeasured)",
      met = describable,
      detail = "Data files must have extractable, typed columns to populate variableMeasured (required).")
  )
  recommended <- list(
    c(item = "README", met = has_readme,
      detail = "A README at the repository root (recommended)."),
    c(item = "CHANGES", met = has_changes,
      detail = "A CHANGES file logging dataset versions (recommended)."),
    c(item = "variable descriptions", met = n_documented > 0,
      detail = "Codebook descriptions for variables improve reuse (recommended; see codebook_check).")
  )

  req_met  <- vapply(required,    function(x) isTRUE(as.logical(x[["met"]])), logical(1))
  rec_met  <- vapply(recommended, function(x) isTRUE(as.logical(x[["met"]])), logical(1))
  n_req_missing <- sum(!req_met)
  n_rec_missing <- sum(!rec_met)
  n_misplaced   <- sum(misplaced)

  # ── 5. Traffic light ─────────────────────────────────────────────────────────
  tl <- if (has_dataset_desc && n_req_missing == 0) "green"     # already compliant
        else if (has_data_files && describable) "yellow"        # convertible
        else "red"                                              # not convertible yet

  # ── 6. Build the required-vs-present tree ────────────────────────────────────
  # Each node carries a status used for colour: present / missing / move.
  present_nodes <- data.frame(
    path    = target_path,
    status  = ifelse(misplaced, "move", "present"),
    note    = ifelse(misplaced, paste0("move from ", current_path), ""),
    stringsAsFactors = FALSE
  )

  # Required scaffolding that must appear even if absent. `is_dataset` marks a
  # root that is a real Psych-DS dataset (needs dataset_description.json); the
  # collection root of a multi-study repository is NOT a dataset, so it gets only
  # the BIDS-style root README/CHANGES, no dataset_description.json.
  scaffold <- function(prefix = "", is_dataset = TRUE) {
    rows <- list(
      c(path = paste0(prefix, "README"),  req = !has_readme),
      c(path = paste0(prefix, "CHANGES"), req = !has_changes)
    )
    if (is_dataset)
      rows <- c(list(c(path = paste0(prefix, "dataset_description.json"),
                       req = !has_dataset_desc)), rows)
    do.call(rbind, lapply(rows, function(r) data.frame(
      path = r[["path"]],
      status = if (isTRUE(as.logical(r[["req"]]))) "missing" else "present-scaffold",
      note = "", stringsAsFactors = FALSE)))
  }
  # Scaffolding: in a multi-study repo, each study-<group>/ is a full dataset,
  # and the collection root gets README/CHANGES only (no dataset_description.json,
  # since the root is a collection of datasets, not itself one — see Option A in
  # convert_psychds). A single study is one flat dataset at the root.
  scaffold_nodes <- if (multi_study)
    rbind(do.call(rbind, lapply(paste0("study-", study_groups, "/"), scaffold)),
          scaffold("", is_dataset = FALSE))
  else scaffold("")
  # Only show missing scaffolding (present README/desc already appear as files).
  scaffold_nodes <- scaffold_nodes[scaffold_nodes$status == "missing", , drop = FALSE]

  tree_nodes <- rbind(present_nodes, scaffold_nodes)
  tree_html  <- psychds_tree_html(tree_nodes)

  # ── 7. Suggestions ───────────────────────────────────────────────────────────
  suggestions <- character(0)
  for (r in required) if (!isTRUE(as.logical(r[["met"]])))
    suggestions <- c(suggestions, paste0("**Add** ", r[["item"]], " — ", r[["detail"]]))
  if (n_misplaced > 0) {
    mv <- which(misplaced)
    ex <- utils::head(mv, 5)
    for (i in ex)
      suggestions <- c(suggestions,
        sprintf("**Move** `%s` → `%s`.", current_path[i], target_path[i]))
    if (length(mv) > 5)
      suggestions <- c(suggestions,
        sprintf("...and %d more file%s to relocate.", length(mv) - 5,
                plural(length(mv) - 5)))
  }
  if (describable && n_documented < n_columns)
    suggestions <- c(suggestions, sprintf(
      "**Document** the %d of %d data column%s without a codebook description (see codebook_check).",
      n_columns - n_documented, n_columns, plural(n_columns)))
  for (r in recommended) if (!isTRUE(as.logical(r[["met"]])))
    suggestions <- c(suggestions, paste0("**Add** ", r[["item"]], " — ", r[["detail"]]))

  # ── 8. Report ────────────────────────────────────────────────────────────────
  summary_text <- if (has_dataset_desc && n_req_missing == 0) {
    "This repository already contains the files required for a Psych-DS dataset."
  } else {
    c(
      sprintf("%d of %d required Psych-DS item%s present; %d missing.",
              sum(req_met), length(req_met), plural(length(req_met)), n_req_missing),
      if (n_misplaced > 0) sprintf(
        "%d file%s would need to move to a Psych-DS location.",
        n_misplaced, plural(n_misplaced)),
      if (n_rec_missing > 0) sprintf(
        "%d recommended item%s missing.", n_rec_missing, plural(n_rec_missing))
    ) |> paste("\n- ", x = _, collapse = "")
  }

  report <- c(sprintf(
    "This module checks the repository against the Psych-DS dataset standard. We examined %d file%s%s.",
    n_files, plural(n_files),
    if (have_groups) sprintf(" across %d study group%s", length(study_groups),
                             plural(length(study_groups))) else ""
  ))

  # Compliance checklist table.
  checklist <- data.frame(
    Requirement = c(vapply(required, `[[`, character(1), "item"),
                    vapply(recommended, `[[`, character(1), "item")),
    Level  = c(rep("required", length(required)),
               rep("recommended", length(recommended))),
    Status = c(ifelse(req_met, "present", "missing"),
               ifelse(rec_met, "present", "missing")),
    Detail = c(vapply(required, `[[`, character(1), "detail"),
               vapply(recommended, `[[`, character(1), "detail"))
  )
  report <- c(report, "#### Compliance Checklist",
              scroll_table(checklist, maxrows = 10))

  # Target tree.
  report <- c(report, "#### Target Psych-DS Structure",
              "The tree below shows the Psych-DS layout this repository should have. Files in <span style=\"color:#c0392b\">red</span> are required but missing; files annotated *(move from ...)* exist but need relocating.",
              tree_html)
  if (group_no_evidence) {
    # Every file always resolves to a real study group now (there is no
    # "shared"/ungrouped state), so this can no longer mean "grouping failed" —
    # it means grouping succeeded only via the blanket "ex1" default: no file
    # path, repository split, code reference, or LLM answer named a study
    # anywhere in this repository. Worth flagging distinctly from a genuinely
    # detected single study, since the layout below is a guess, not evidence.
    report <- c(report,
      paste0("*Study subgrouping could not be detected: no file path or ",
             "repository split names a study",
             if (!llm_use()) ", and no LLM was used" else "",
             ". Every file was placed in a single default study (`ex1`) rather ",
             "than from real evidence. If this repository contains multiple ",
             "studies, name the study in the file or folder names ",
             "(`study1/`, `experiment2_data.csv`)",
             if (!llm_use()) " or run with `llm_use(TRUE)`" else "",
             " so a multi-study Psych-DS layout can be modelled.*"))
  }

  # Suggestions.
  if (length(suggestions) > 0) {
    report <- c(report, "#### Suggestions",
                paste0("- ", suggestions, collapse = "\n"))
  }

  # Call to action.
  report <- c(report,
    "You can generate a Psych-DS-compliant copy of this repository — with the moves above applied and a `dataset_description.json` built from the extracted variables — using `metacheck::convert_psychds(paper, output_dir)`.",
    "To also produce a human-readable codebook of the extracted variables (a labelled data frame plus a ready-to-run R Markdown document for the `codebook` package), use `metacheck::convert_codebook(paper, output_dir)`.")

  # ── 9. Summary table + return ────────────────────────────────────────────────
  summary_table <- data.frame(
    paper_id            = .pid(structure_df, columns_df),
    required_met        = sum(req_met),
    required_missing    = n_req_missing,
    recommended_met     = sum(rec_met),
    recommended_missing = n_rec_missing,
    misplaced_n         = n_misplaced
  )

  # referenced_by: which OTHER studies reuse this file (cross-study reuse via
  # a script's own read/write references — see data_group_llm()). A plain list
  # column so convert_psychds() can write a reference into each of those
  # studies' own metadata instead of copying the file (see
  # .psychds_dataset_description() / .psychds_rocrate_json()).
  plan_table <- data.frame(
    file_name       = structure_df$file_name,
    data_type       = structure_df$data_type,
    group           = groups,
    current_path    = current_path,
    target_path     = target_path,
    # `excluded` = a data_type target_of() could not map (should not occur in
    # practice; see the comment above is_excluded).
    status          = ifelse(is_excluded, "excluded",
                             ifelse(misplaced, "move", "present")),
    # convert = TRUE → converter writes a real CSV at target_path from the
    # read data; original_target = where to also copy the untouched original.
    convert         = needs_convert,
    original_target = original_target
  )
  plan_table$referenced_by <- if ("referenced_by" %in% names(structure_df))
    structure_df$referenced_by else vector("list", n_files)

  list(
    table = plan_table,
    summary_table = summary_table,
    na_replace = c(required_met = 0, required_missing = 0,
                   recommended_met = 0, recommended_missing = 0,
                   misplaced_n = 0),
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}

# ── Module-local helper ───────────────────────────────────────────────────────

# Render a set of target paths as an HTML <pre> tree. `nodes` is a data.frame
# with `path`, `status` ("present" | "move" | "missing" | "present-scaffold"),
# and `note`. Missing leaves are shown in red; move leaves carry their note.
psychds_tree_html <- function(nodes) {
  if (is.null(nodes) || nrow(nodes) == 0) return("")
  nodes <- nodes[!duplicated(nodes$path), , drop = FALSE]
  norm  <- gsub("\\\\", "/", nodes$path)
  status_of <- setNames(nodes$status, norm)
  note_of   <- setNames(nodes$note %||% rep("", nrow(nodes)), norm)

  parts_list <- lapply(strsplit(norm, "/", fixed = TRUE),
                       function(x) x[nzchar(x)])

  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }

  lines <- character(0)
  # `parents` is the accumulated path to the current subset's parent.
  walk <- function(subset, parents, prefix = "") {
    heads <- vapply(subset, function(x) x[[1]], character(1))
    order_heads <- unique(heads)
    has_child <- vapply(order_heads, function(h)
      any(vapply(subset[heads == h], length, integer(1)) > 1), logical(1))
    order_heads <- order_heads[order(!has_child, tolower(order_heads))]

    for (i in seq_along(order_heads)) {
      h    <- order_heads[[i]]
      idx  <- heads == h
      tails <- lapply(subset[idx], function(x) x[-1])
      is_last <- i == length(order_heads)
      child_exists <- any(vapply(tails, length, integer(1)) > 0)
      branch <- if (is_last) "└── " else "├── "
      next_prefix <- paste0(prefix, if (is_last) "    " else "│   ")

      full  <- if (nzchar(parents)) paste0(parents, "/", h) else h
      label <- paste0(h, if (child_exists) "/" else "")
      # Safe lookup: intermediate directories are not keys in status_of/note_of.
      st    <- if (full %in% names(status_of)) status_of[[full]] else ""
      note  <- if (full %in% names(note_of))   note_of[[full]]   else ""

      styled <- if (identical(st, "missing")) {
        sprintf("<span style=\"color:#c0392b\">%s  ← missing</span>", esc(label))
      } else if (identical(st, "move") && nzchar(note)) {
        sprintf("%s <span style=\"color:#b9770e\">(%s)</span>", esc(label), esc(note))
      } else esc(label)

      lines <<- c(lines, paste0(esc(prefix), branch, styled))
      keep <- vapply(tails, length, integer(1)) > 0
      if (any(keep)) walk(tails[keep], full, next_prefix)
    }
  }
  walk(parts_list, "")

  paste0("<pre style=\"line-height:1.4\">\n",
         paste(lines, collapse = "\n"), "\n</pre>")
}
