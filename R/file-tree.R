# The repository file tree: everything in each repository, drawn as a tree,
# with how each file was classified, its study and any file-naming issue, and
# marks for junk files, hidden files and empty folders. data_check shows it in
# its report ("#### Data Tree"); the repository apps' brief report and external
# modules (such as the TU/e data manager check) show the same tree.
#
# What a tree shows:
# - a local folder (a repo_url that is a folder on this computer) is listed
#   itself, with every file and folder, including hidden files, junk files and
#   empty folders, plus any file of the file table that is not on disk (files
#   inside zip archives, as data_check lists them);
# - a repository link shows the file table's files plus repo_check's
#   `useless_files` (junk), since repo_check leaves junk out of its table;
#   empty folders cannot be shown, as repository listings hold only files;
# - a junk folder (.git/, __pycache__/, __MACOSX/, ...) is one line with the
#   number of files inside, not its contents;
# - folders first (empty ones included), then files, each alphabetically
#   without regard to capitals;
# - the tree sits in a box with its own scroll bar, so a large repository does
#   not make the report endless.

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

# The marks for files and folders that are not classified.
.tree_mark_junk <- c(problem = "junk: remove",
                     suggestion = "junk: keep only on purpose")
.tree_mark_empty <- "empty folder"
.tree_mark_hidden <- "hidden file"

# Walk a set of relative paths into tree rows. Each row also carries which
# INPUT PATH (if any) it is the leaf of, via `leaf_idx` (index into `paths`,
# NA for a folder row that is not itself an input) — so the caller can attach
# per-file columns (type, study, naming issue, mark) to the right row without
# re-parsing the tree text. `is_dir` (same length as `paths`, default all
# FALSE) marks paths that are folders: a folder with nothing under it (an empty
# folder, or a junk folder shown as one line) is still drawn and sorted as a
# folder, with a trailing "/", and its row's leaf_idx points at it. Two input
# rows with the exact same path collapse into one tree row; only the first
# row's metadata is then shown for it.
.file_tree_rows <- function(paths, is_dir = NULL) {
  if (is.null(is_dir)) is_dir <- rep(FALSE, length(paths))
  keep <- !is.na(paths) & nzchar(paths)
  paths <- paths[keep]
  is_dir <- is_dir[keep]
  orig_idx <- which(keep)
  if (length(paths) == 0)
    return(data.frame(text = character(0), leaf_idx = integer(0)))
  parts_list <- strsplit(gsub("\\\\", "/", paths), "/", fixed = FALSE)
  parts_list <- lapply(parts_list, function(x) x[nzchar(x)])
  out_text <- character(0)
  out_leaf <- integer(0)

  walk <- function(parts_subset, idx_subset, dir_subset, prefix = "") {
    heads <- vapply(parts_subset, function(x) x[[1]], character(1))
    depth1 <- vapply(parts_subset, length, integer(1)) == 1
    head_order <- unique(heads)
    # a head is a folder when something lies below it, or when it is itself
    # an input folder (an empty folder, or a junk folder shown as one line)
    is_folder <- vapply(head_order, function(head) {
      i <- heads == head
      any(!depth1[i]) || any(depth1[i] & dir_subset[i])
    }, logical(1))
    head_order <- head_order[order(!is_folder, tolower(head_order))]
    is_folder <- is_folder[match(head_order, unique(heads))]
    names(is_folder) <- head_order

    for (i in seq_along(head_order)) {
      head <- head_order[[i]]
      sel <- heads == head
      tails <- lapply(parts_subset[sel], function(x) x[-1])
      tail_idx <- idx_subset[sel]
      tail_dir <- dir_subset[sel]
      is_last <- i == length(head_order)
      branch <- if (is_last) "\u2514\u2500\u2500 " else "\u251C\u2500\u2500 "
      next_prefix <- paste0(prefix, if (is_last) "    " else "\u2502   ")
      deeper <- vapply(tails, length, integer(1)) > 0
      # the row of this head points at the input path that IS this head (a
      # file, or a folder given as an input), else NA (a plain folder row)
      own <- which(!deeper)
      this_leaf <- if (length(own)) tail_idx[[own[[1]]]] else NA_integer_
      out_text <<- c(out_text, paste0(prefix, branch, head, if (is_folder[[head]]) "/" else ""))
      out_leaf <<- c(out_leaf, this_leaf)
      if (any(deeper)) walk(tails[deeper], tail_idx[deeper], tail_dir[deeper], next_prefix)
    }
  }

  walk(parts_list, orig_idx, is_dir)
  data.frame(text = out_text, leaf_idx = out_leaf)
}

# The entries of one repository's tree: one row per file or folder to draw,
# with path (relative, "/"), is_dir, and the columns shown after it.
# `sub` is the file table's rows of this repository; `useless` is
# repo_check's useless_files rows of this repository (or NULL).
.file_tree_entries <- function(repo, sub, useless) {
  col <- function(df, name) {
    if (!is.null(df) && name %in% names(df)) df[[name]] else rep(NA_character_, NROW(df))
  }
  table_paths <- col(sub, "file_path")
  no_path <- is.na(table_paths) | !nzchar(table_paths)
  table_paths[no_path] <- col(sub, "file_name")[no_path]
  table_paths <- gsub("\\\\", "/", table_paths)

  if (!is.na(repo) && nzchar(repo) && dir.exists(repo)) {
    # a local folder: list everything that is there
    all <- list.files(repo, all.files = TRUE, recursive = TRUE,
                      include.dirs = TRUE, no.. = TRUE)
    is_dir <- dir.exists(file.path(repo, all))
    on_disk <- data.frame(path = all, is_dir = is_dir)
    # plus files of the table that are not on disk (inside zip archives)
    extra <- setdiff(table_paths, all)
    paths <- rbind(on_disk, data.frame(path = extra, is_dir = rep(FALSE, length(extra))))
    junk_kind <- .useless_file_kind(paths$path)
  } else {
    # a repository link: the table's files plus repo_check's junk files
    junk_paths <- gsub("\\\\", "/", col(useless, "file_path"))
    junk_paths <- junk_paths[!is.na(junk_paths)]
    paths <- data.frame(path = c(table_paths, junk_paths),
                        is_dir = rep(FALSE, length(table_paths) + length(junk_paths)))
    paths <- paths[!duplicated(paths$path), , drop = FALSE]
    junk_kind <- .useless_file_kind(paths$path)
  }

  # A junk folder is shown as one line: the shallowest folder of a junk path
  # that is itself junk (".git" for ".git/objects/ab/cd"), with the number of
  # files under it; nothing below it is drawn.
  junk_root <- vapply(seq_len(nrow(paths)), function(i) {
    if (is.na(junk_kind[[i]])) return(NA_character_)
    seg <- strsplit(paths$path[[i]], "/", fixed = TRUE)[[1]]
    if (length(seg) < 2) return(NA_character_)
    for (k in seq_len(length(seg) - 1)) {
      prefix <- paste(seg[seq_len(k)], collapse = "/")
      if (!is.na(.useless_file_kind(prefix))) return(prefix)
    }
    NA_character_
  }, character(1))
  in_junk_dir <- !is.na(junk_root)
  # a folder that is itself junk counts as its own root
  own_root <- paths$is_dir & !is.na(junk_kind) & !in_junk_dir
  roots <- unique(c(junk_root[in_junk_dir], paths$path[own_root]))
  root_rows <- data.frame(
    path = as.character(roots), is_dir = rep(TRUE, length(roots)),
    n_files = unname(vapply(roots, function(r) {
      sum(!paths$is_dir & startsWith(paths$path, paste0(r, "/")))
    }, integer(1)))
  )
  keep <- !in_junk_dir & !own_root
  entries <- paths[keep, , drop = FALSE]
  entries$n_files <- NA_integer_
  entries <- rbind(entries, root_rows)
  kind <- .useless_file_kind(entries$path)

  # empty folders: folders with no file anywhere below them
  file_paths <- paths$path[!paths$is_dir]
  entries$empty <- entries$is_dir & is.na(entries$n_files) &
    !vapply(entries$path, function(p) any(startsWith(file_paths, paste0(p, "/"))), logical(1))

  # what is shown after each name
  hit <- match(entries$path, table_paths)
  data_type <- col(sub, "data_type")[hit]
  doc_role <- col(sub, "doc_role")[hit]
  entries$type_txt <- ifelse(is.na(hit) | entries$is_dir, "",
                             paste(.tree_type_icon(data_type, doc_role),
                                   .tree_type_label(data_type, doc_role)))
  entries$study <- ifelse(is.na(hit), "", dplyr::coalesce(col(sub, "group")[hit], ""))
  entries$file_name <- col(sub, "file_name")[hit]
  hidden <- grepl("(^|/)\\.", entries$path)
  sev <- .useless_file_severity(kind)
  entries$mark <- dplyr::case_when(
    !is.na(kind) & !is.na(entries$n_files) ~
      sprintf("%s (%s file%s)", .tree_mark_junk[sev],
              formatC(entries$n_files, format = "d", big.mark = ","),
              ifelse(entries$n_files == 1, "", "s")),
    !is.na(kind) ~ unname(.tree_mark_junk[sev]),
    entries$empty ~ .tree_mark_empty,
    hidden & !entries$is_dir & entries$type_txt == "" ~ .tree_mark_hidden,
    .default = ""
  )
  entries
}

# The file tree as report text: "#### Data Tree", a legend, and per repository
# a preformatted block in a scroll box, with each file's classification, study
# and naming issue, or a mark (junk, hidden file, empty folder). `files` is a
# file table with repo_url and file_path (or file_name), and optionally
# data_type, doc_role and group, as data_check and repo_check give it;
# `naming_issues` is repo_check's naming_issues table; `useless_files` is
# repo_check's useless_files table. NULL when there is nothing to draw.
.file_tree_block <- function(files, naming_issues = NULL, useless_files = NULL) {
  repos <- c(if (!is.null(files) && "repo_url" %in% names(files)) files$repo_url,
             if (!is.null(useless_files) && "repo_url" %in% names(useless_files)) useless_files$repo_url)
  repo_urls <- unique(repos[!is.na(repos) & nzchar(repos)])
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
    sub <- if (!is.null(files) && "repo_url" %in% names(files)) {
      files[!is.na(files$repo_url) & files$repo_url == repo, , drop = FALSE]
    } else NULL
    useless <- if (!is.null(useless_files) && "repo_url" %in% names(useless_files)) {
      useless_files[!is.na(useless_files$repo_url) & useless_files$repo_url == repo, , drop = FALSE]
    } else NULL
    entries <- .file_tree_entries(repo, sub, useless)
    if (nrow(entries) == 0) return(NULL)
    rows <- .file_tree_rows(entries$path, entries$is_dir)
    if (nrow(rows) == 0) return(NULL)

    # A tree has to READ as a tree: every line in order, with the branch
    # characters lining up. So the rows are laid out as text and emitted as a
    # preformatted block, with the per-file columns padded to align. Widths
    # are computed from the content, so a deep tree or a long type name still
    # lines up.
    at <- rows$leaf_idx
    has <- !is.na(at)
    first_col <- rep("", nrow(rows))
    first_col[has] <- ifelse(nzchar(entries$mark[at[has]]), entries$mark[at[has]],
                             entries$type_txt[at[has]])
    study_txt <- rep("", nrow(rows))
    study_txt[has] <- entries$study[at[has]]
    naming_txt <- rep("", nrow(rows))
    nm <- naming_of(entries$file_name[at[has]])
    naming_txt[has] <- ifelse(is.na(nm), "", nm)

    # Pad to the widest entry in each column. formatC() counts characters,
    # and an emoji is one character here, so the icon does not throw the
    # alignment off in a monospaced block.
    pad <- function(x, extra = 2) {
      w <- max(nchar(x, type = "chars"), 0) + extra
      formatC(x, width = -w, flag = " ")
    }
    has_study  <- any(nzchar(study_txt))
    has_naming <- any(nzchar(naming_txt))
    lines <- paste0(
      pad(rows$text),
      pad(first_col),
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
    # A fenced block with no language, so Quarto (and Pandoc, for the simple
    # report) renders it verbatim in a monospaced font, inside a box of about
    # 30 lines with its own scroll bar. The whole block is ONE element, with
    # its lines joined by single newlines: module_report() joins the report
    # vector with blank lines between elements, which would otherwise put an
    # empty row between every line of the tree.
    # A local folder by its own name: its full path is the place on the
    # computer that ran the check, on a server its job folder
    # (Lakens/tue_datamanager_check#5, item 2).
    label <- if (grepl("^[A-Za-z][A-Za-z0-9+.-]*://", b$repo)) b$repo else basename(b$repo)
    c(paste0("**Repository: ", label, "**"),
      paste(c("::: {style=\"max-height: 40em; overflow: auto;\"}",
              "```", b$lines, "```", ":::"), collapse = "\n"))
  })

  legend <- paste0(
    "Each file is followed by how it was classified",
    if (any(vapply(blocks, function(b) b$has_study, logical(1))))
      ", which study it belongs to" else "",
    if (any(vapply(blocks, function(b) b$has_naming, logical(1))))
      ", and any file-naming issue found by repo_check" else "",
    "; junk files, hidden files and empty folders are marked.")

  c("#### Data Tree",
    paste("Where the files sit within each repository.", legend),
    unlist(sections))
}

# Where the "#### Data Tree" part of a report (data_check's) starts and ends:
# the index of its heading and of its last element. NULL when the report has
# no tree.
.file_tree_report_range <- function(report) {
  if (is.null(report)) return(NULL)
  start <- which(report == "#### Data Tree")
  if (length(start) == 0) return(NULL)
  start <- start[[1]]
  is_heading <- grepl("^#{1,4} ", report)
  after <- which(is_heading & seq_along(report) > start)
  end <- if (length(after) > 0) after[[1]] - 1 else length(report)
  c(start = start, end = end)
}

# The "#### Data Tree" part of a report (data_check's), without its heading.
# NULL when the report has no tree.
.file_tree_from_report <- function(report) {
  range <- .file_tree_report_range(report)
  if (is.null(range) || range[["end"]] <= range[["start"]]) return(NULL)
  report[(range[["start"]] + 1):range[["end"]]]
}

# A report (data_check's) without its "#### Data Tree" part, heading
# included: the checklist report shows the tree in a section of its own.
.file_tree_drop_from_report <- function(report) {
  range <- .file_tree_report_range(report)
  if (is.null(range)) return(report)
  report[-(range[["start"]]:range[["end"]])]
}

# The file tree of a run, without its heading: data_check's (which also lists
# files inside zip archives and each file's study), else one drawn from
# repo_check's file table and junk files, since data_check reports nothing at
# all when a repository has no data file it can read. `module_output` is a
# list of module outputs named by module, as report_module_run() returns.
# NULL when neither module gives a tree.
.file_tree <- function(module_output) {
  tree <- .file_tree_from_report(module_output$data_check$report)
  if (!is.null(tree)) return(tree)
  block <- .file_tree_block(module_output$repo_check$table,
                            module_output$repo_check$naming_issues,
                            module_output$repo_check$useless_files)
  if (is.null(block)) return(NULL)
  block[-1]
}
