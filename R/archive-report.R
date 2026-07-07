# Corpus-level archive reporting. Builds flat, browsable CSVs from the artifacts
# a batch run leaves on disk:
#   * write_file_manifest()  — one row per repository file across all papers,
#     from the per-paper *.manifest.json files data_check writes.
#   * collect_check_results() — the check outcomes of a run: one row per
#     paper x module (traffic lights + counts) and one row per individual
#     finding, from per-paper check-summary files a run has saved.
#
# Both operate purely on files already on disk, so they can be run any time
# (e.g. after an interrupted batch) without re-checking anything.

# Read one manifest field as a flat atomic vector. jsonlite keeps a field that
# is present on some files and null on others (e.g. file_url) as a LIST-column
# with NULL elements; a NULL element has length 0 and makes data.frame() throw
# "differing number of rows". Flatten list-columns to character, NULL -> NA.
.manifest_col <- function(d, nm) {
  if (!nm %in% names(d)) return(NA)
  v <- d[[nm]]
  if (is.list(v))
    v <- vapply(v, function(x)
      if (length(x) == 0) NA_character_ else as.character(x)[1], character(1))
  v
}

#' Build a corpus-wide file inventory from per-paper manifests
#'
#' Reads every `*.manifest.json` in `manifest_dir` (written by [data_check()]
#' when its `manifest` argument is set) and flattens them into a single table:
#' one row per repository file across all papers, with its download status and
#' skip reason. This is the whole-archive view of what exists versus what was
#' fetched.
#'
#' It is the reusable form of the inventory step a batch script runs at the end;
#' because it reads only the on-disk manifests, it can be run at any time — after
#' an interrupted run, or to refresh the inventory once more files download on a
#' re-run.
#'
#' @param manifest_dir directory holding the per-paper `*.manifest.json` files.
#' @param out optional path to write the inventory CSV to. `NULL` (default)
#'   writes `_all_files.csv` inside `manifest_dir`; `NA` writes nothing (the data
#'   frame is only returned).
#'
#' @returns the inventory as a data.frame, invisibly. Columns: `paper_id`,
#'   `repo_url`, `file_name`, `file_path`, `file_url`, `file_size`,
#'   `file_size_mb`, `data_type`, `downloaded`, `status`, `skip_intentional`,
#'   `skip_reason`.
#' @seealso [collect_check_results()], [data_check()]
#' @export
#' @examples
#' \dontrun{
#' write_file_manifest("output/openmind/_manifests")
#' }
write_file_manifest <- function(manifest_dir, out = NULL) {
  mfiles <- list.files(manifest_dir, pattern = "[.]manifest[.]json$",
                       full.names = TRUE)
  if (length(mfiles) == 0) {
    warning("No *.manifest.json files found in ", manifest_dir, call. = FALSE)
    return(invisible(data.frame()))
  }

  col <- .manifest_col
  rows <- lapply(mfiles, function(f) {
    j <- jsonlite::fromJSON(f, simplifyVector = TRUE)
    # paper_id can be JSON null (a paper with no resolvable id), read back as
    # NULL (length 0); fall back to the file name so every paper gets a row.
    pid <- j$paper_id %||% sub("[.]manifest[.]json$", "", basename(f))
    if (is.null(j$files) || length(j$files) == 0)
      return(data.frame(
        paper_id = pid, repo_url = NA, file_name = NA, file_path = NA,
        file_url = NA, file_size = NA, file_size_mb = NA, data_type = NA,
        downloaded = NA, status = "no_repo", skip_intentional = NA,
        skip_reason = "no repository", stringsAsFactors = FALSE))
    d <- j$files
    fs <- as.numeric(col(d, "file_size"))
    data.frame(
      paper_id = pid, repo_url = col(d, "repo_url"),
      file_name = col(d, "file_name"), file_path = col(d, "file_path"),
      file_url = col(d, "file_url"), file_size = fs,
      file_size_mb = round(fs / 1024^2, 3),
      data_type = col(d, "data_type"), downloaded = col(d, "downloaded"),
      status = col(d, "status"), skip_intentional = col(d, "skip_intentional"),
      skip_reason = col(d, "skip_reason"), stringsAsFactors = FALSE)
  })
  inv <- do.call(rbind, rows)

  dest <- if (is.null(out)) file.path(manifest_dir, "_all_files.csv") else out
  if (!is.na(dest)) {
    utils::write.csv(inv, dest, row.names = FALSE, na = "")
    message("Wrote file inventory: ", normalizePath(dest, mustWork = FALSE),
            " (", nrow(inv), " files across ", length(mfiles), " papers).")
  }
  invisible(inv)
}

# Modules whose `table` is a per-column/per-file INVENTORY, not a list of
# flagged issues — one row per column (data_check) or per documented variable
# (codebook_check). Left whole they dominate _all_findings.csv (hundreds of
# thousands of rows, with bloated free-text like data_check$sample_values), so
# they are reduced to their genuine problem rows below rather than dumped.
.inventory_modules <- c("data_check", "codebook_check", "repo_check")

# Columns never useful in a findings file (large free-text / per-value dumps
# that inflate the CSV and can carry embedded commas/quotes).
.findings_drop_cols <- c("sample_values", "value_labels", "missing_values",
                         "universe", "question")

# Some modules return RESULTS in named list elements OTHER than `table` — a
# data.frame per element — that the report should also collect. Each entry maps
# `module -> c(element_name = "check label")`; every row of that element becomes
# a finding tagged with the given `check`. (data_check's structure/previews/
# gated_repos are intentionally NOT here: those are inter-module plumbing, not
# results.)
.extra_finding_elements <- list(
  data_validate = c(careless     = "Careless responding",
                    demographics = "Demographic column",
                    qualtrics    = "Qualtrics survey metadata"),
  codebook_check = c(codebook_vars = "Unused codebook variable")
)

# Max bytes for any single character cell in the findings CSV. A finding's
# free text (e.g. data_validate's `detail`) is a human-readable pointer, so a
# few hundred characters is plenty; anything longer is truncated. Without this,
# a single pathological cell (e.g. every distinct value of a column of large
# array-valued cells) can be megabytes and blow up the CSV.
.finding_cell_max <- 500L

# Clean one character vector for a CSV cell: collapse embedded newlines/tabs/
# carriage returns to spaces (they otherwise break row alignment even when
# quoted, and corrupt downstream readers), squeeze runs of whitespace, then cap
# the length. Applied to every character column before binding/writing.
.clean_cell <- function(col) {
  if (!is.character(col)) return(col)
  # Drop invalid-UTF-8 bytes so nchar()/write.csv() do not choke (some data
  # cells carry latin1/garbage bytes).
  col <- iconv(col, to = "UTF-8", sub = "")
  # Collapse control chars (newlines/tabs/CR and other C0) to spaces, and turn
  # double quotes into single quotes: both otherwise break CSV row alignment.
  col <- gsub("[[:cntrl:]]+", " ", col)
  col <- gsub('"', "'", col, fixed = TRUE)
  col <- gsub(" {2,}", " ", col)
  col <- trimws(col)
  long <- !is.na(col) & nchar(col, type = "bytes") > .finding_cell_max
  if (any(long))
    col[long] <- paste0(substr(col[long], 1, .finding_cell_max), " ... [truncated]")
  col
}

# Reduce an inventory module's table to only the rows that represent an actual
# problem, so the findings file stays about issues. Returns NULL when the module
# contributes no issue rows.
.inventory_findings <- function(nm, tbl) {
  if (identical(nm, "data_check")) {
    # The one flagged data_check issue: values that had to be repaired from a
    # legacy (non-UTF-8) encoding.
    if ("utf8_repaired" %in% names(tbl)) {
      n <- suppressWarnings(as.numeric(tbl$utf8_repaired))
      keep <- !is.na(n) & n > 0
      if (any(keep)) {
        out <- tbl[keep, intersect(c("source_file", "column_name",
                                     "utf8_repaired"), names(tbl)), drop = FALSE]
        out$check <- "Mixed encoding"
        return(out)
      }
    }
    return(NULL)
  }
  if (identical(nm, "codebook_check")) {
    parts <- list()
    # Issue: undocumented variables (no usable label).
    if ("label_status" %in% names(tbl)) {
      keep <- tbl$label_status %in% c("unlabelled", "", NA)
      if (any(keep)) {
        u <- tbl[keep, intersect(c("source_file", "column_name",
                                   "label_status"), names(tbl)), drop = FALSE]
        u$check <- "Undocumented variable"
        parts[[length(parts) + 1L]] <- u
      }
    }
    # Informational: identified psychometric scales (one row per scale member).
    # Not a problem, but a result worth surfacing corpus-wide.
    if ("scale" %in% names(tbl)) {
      has <- !is.na(tbl$scale) & nzchar(tbl$scale)
      if (any(has)) {
        s <- tbl[has, intersect(c("source_file", "column_name", "scale",
                                  "scale_confidence"), names(tbl)), drop = FALSE]
        s$check <- "Identified scale"
        parts[[length(parts) + 1L]] <- s
      }
    }
    if (length(parts) == 0) return(NULL)
    return(dplyr::bind_rows(parts))
  }
  # repo_check: a file listing, not issues.
  NULL
}

# Flatten one paper's module-output chain (from report_module_run()) into two
# tidy pieces for on-disk capture during a run:
#   * checks:   one row per module — paper_id, module, traffic_light,
#               summary_text, and the module's summary_table count columns
#               (JSON-encoded so a single column holds a variable set of counts).
#   * findings: one row per FLAGGED ISSUE. Issue modules (data_validate,
#               excel_check, code_check, psychds_check) contribute their table
#               rows directly; inventory modules (data_check, codebook_check)
#               contribute only their genuine problem rows (see
#               .inventory_findings). Tagged with paper_id + module.
# Returned as a list(checks = df, findings = df). Used by capture_check_results()
# and consumed later by collect_check_results().
.flatten_check_chain <- function(chain, paper_id = NULL) {
  mods <- Filter(function(x) inherits(x, "metacheck_module_output"), chain)
  if (length(mods) == 0) mods <- chain     # tolerate a plain named list

  # One paper id for the whole chain. Prefer an explicit `paper_id`; otherwise
  # take the first non-missing id any module reported. Using a single id keeps
  # every module's rows joinable (some modules key their summary_table by a
  # hash rather than the corpus id, which would otherwise fragment the paper).
  chain_pid <- paper_id
  if (is.null(chain_pid) || is.na(chain_pid) || !nzchar(chain_pid %||% "")) {
    for (mo in mods) {
      st <- mo$summary_table
      if (!is.null(st) && "paper_id" %in% names(st) && nrow(st) > 0) {
        cand <- as.character(st$paper_id[[1]])
        if (!is.na(cand) && nzchar(cand)) { chain_pid <- cand; break }
      }
    }
  }
  pid_of <- function(mo) chain_pid %||% NA_character_

  # report_module_run() strips summary_table from every module EXCEPT the last,
  # whose summary_table is the COMBINED wide row (all modules' count columns
  # merged, e.g. data_file_n + matched_n + scale_blocks_n + required_met + ...).
  # So a module's own summary_table is usually NULL here; fall back to this
  # combined row so per-module counts are not lost. (dplyr's join suffixes a
  # duplicated name as "<col>.<module>"; those are kept as-is.)
  combined_summary <- NULL
  for (mo in mods) {
    st <- mo$summary_table
    if (!is.null(st) && is.data.frame(st) && nrow(st) > 0 &&
        ncol(st) > ncol(combined_summary %||% st[, 0, drop = FALSE]))
      combined_summary <- st
  }

  checks <- list()
  findings <- list()
  for (nm in names(mods)) {
    mo <- mods[[nm]]
    if (is.null(mo)) next
    pid <- pid_of(mo)

    # Prefer the module's own summary_table; fall back to the combined one.
    st <- mo$summary_table %||% combined_summary
    counts <- if (!is.null(st) && nrow(st) > 0) {
      keep <- setdiff(names(st), "paper_id")
      if (length(keep)) jsonlite::toJSON(as.list(st[1, keep, drop = FALSE]),
                                         auto_unbox = TRUE) else NA_character_
    } else NA_character_

    checks[[length(checks) + 1L]] <- data.frame(
      paper_id      = pid,
      module        = nm,
      traffic_light = mo$traffic_light %||% NA_character_,
      summary_text  = mo$summary_text %||% NA_character_,
      counts        = as.character(counts),
      stringsAsFactors = FALSE)

    tbl <- mo$table
    if (!is.null(tbl) && is.data.frame(tbl) && nrow(tbl) > 0) {
      # Inventory modules: keep only genuine problem rows, not every column.
      if (nm %in% .inventory_modules) tbl <- .inventory_findings(nm, tbl)
      if (!is.null(tbl) && nrow(tbl) > 0) {
        # Drop large free-text/per-value columns that bloat the findings CSV.
        tbl <- tbl[, setdiff(names(tbl), .findings_drop_cols), drop = FALSE]
        tbl <- as.data.frame(lapply(tbl, function(col)
          if (is.list(col)) vapply(col, function(x)
            paste(as.character(x), collapse = "; "), character(1))
          else col), stringsAsFactors = FALSE)
        # Clean + cap every character cell (embedded newlines break CSV row
        # alignment; a finding's free text must never reach megabytes). See
        # .clean_cell.
        tbl <- as.data.frame(lapply(tbl, .clean_cell), stringsAsFactors = FALSE)
        # Force the chain's single paper id so findings join to the check rows
        # (a module's own table may carry a hash id or none).
        tbl$paper_id <- pid
        tbl$module <- nm
        # paper_id + module first, for readability.
        front <- c("paper_id", "module")
        tbl <- tbl[, c(front, setdiff(names(tbl), front)), drop = FALSE]
        findings[[length(findings) + 1L]] <- tbl
      }
    }

    # Results some modules return in named elements OTHER than `table`
    # (careless, demographics, qualtrics, codebook_vars): capture each as
    # findings tagged with its check label. See .extra_finding_elements.
    extras <- .extra_finding_elements[[nm]]
    for (el in names(extras)) {
      ed <- mo[[el]]
      if (is.null(ed) || !is.data.frame(ed) || nrow(ed) == 0) next
      ed <- ed[, setdiff(names(ed), .findings_drop_cols), drop = FALSE]
      ed <- as.data.frame(lapply(ed, .clean_cell), stringsAsFactors = FALSE)
      ed$paper_id <- pid
      ed$module   <- nm
      ed$check    <- unname(extras[[el]])
      front <- c("paper_id", "module", "check")
      ed <- ed[, c(front, setdiff(names(ed), front)), drop = FALSE]
      findings[[length(findings) + 1L]] <- ed
    }
  }

  list(
    checks = if (length(checks)) dplyr::bind_rows(checks) else data.frame(),
    findings = if (length(findings)) dplyr::bind_rows(findings) else data.frame()
  )
}

#' Save one paper's check results to disk
#'
#' Flattens a module-output chain (the result of [report_module_run()] or
#' [report()]) into per-paper check summaries and findings, and writes them as
#' `<paper_id>.checks.json` in `results_dir`. Call it inside a batch loop right
#' after running the checks for a paper, so the check outcomes survive the run
#' (the in-memory chain is otherwise discarded once the archive is built) and an
#' interrupted run keeps everything processed so far.
#'
#' @param chain a module-output chain from [report_module_run()].
#' @param results_dir directory to write the per-paper `*.checks.json` into
#'   (created if needed).
#' @param paper_id optional paper id, used only if it cannot be recovered from
#'   the chain's summary tables.
#'
#' @returns the written path, invisibly.
#' @seealso [collect_check_results()]
#' @export
capture_check_results <- function(chain, results_dir, paper_id = NULL) {
  flat <- .flatten_check_chain(chain, paper_id = paper_id)
  pid <- if (nrow(flat$checks)) flat$checks$paper_id[[1]] else
    paper_id %||% "paper"
  if (is.na(pid) || !nzchar(pid)) pid <- paper_id %||% "paper"

  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(results_dir, paste0(pid, ".checks.json"))
  # Data frames -> row-wise JSON so collect_check_results() can rebind them.
  doc <- list(
    paper_id = pid,
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    checks = flat$checks,
    findings = flat$findings
  )
  json <- jsonlite::toJSON(doc, auto_unbox = TRUE, dataframe = "rows",
                           na = "null", null = "null")
  writeLines(json, path, useBytes = TRUE)
  invisible(path)
}

#' Collect a run's check results into corpus-wide CSVs
#'
#' Reads every `<paper_id>.checks.json` in `results_dir` (written by
#' [capture_check_results()]) and produces two flat tables:
#'
#' * `_all_checks.csv` — one row per paper per module: the module's
#'   `traffic_light`, its `summary_text`, and its per-paper count columns
#'   (unpacked from the module's `summary_table`, so e.g. `code_check`'s
#'   `code_file_n` or `data_validate`'s `flagged_n` become their own columns).
#' * `_all_findings.csv` — one row per individual flagged item across all papers
#'   and modules (the row-level `table` each module returns), tagged with
#'   `paper_id` and `module`.
#'
#' Like [write_file_manifest()], it reads only on-disk artifacts, so it can be
#' run at any time after (or during) a batch.
#'
#' @param results_dir directory holding the per-paper `*.checks.json` files.
#' @param out_dir directory to write the two CSVs into. `NULL` (default) uses
#'   `results_dir`.
#'
#' @returns invisibly, a list with `checks` and `findings` data frames.
#' @seealso [capture_check_results()], [write_file_manifest()]
#' @export
#' @examples
#' \dontrun{
#' collect_check_results("output/openmind/_checks")
#' }
collect_check_results <- function(results_dir, out_dir = NULL) {
  cfiles <- list.files(results_dir, pattern = "[.]checks[.]json$",
                       full.names = TRUE)
  if (length(cfiles) == 0) {
    warning("No *.checks.json files found in ", results_dir,
            ". Did the run call capture_check_results()?", call. = FALSE)
    return(invisible(list(checks = data.frame(), findings = data.frame())))
  }

  checks_l <- list()
  finds_l  <- list()
  for (f in cfiles) {
    j <- jsonlite::fromJSON(f, simplifyVector = TRUE)
    if (!is.null(j$checks) && length(j$checks) &&
        (is.data.frame(j$checks) && nrow(j$checks) > 0)) {
      ck <- j$checks
      # Unpack the JSON `counts` column into real per-count columns.
      count_cols <- lapply(ck$counts, function(x) {
        if (is.na(x) || !nzchar(x)) return(list())
        tryCatch(as.list(jsonlite::fromJSON(x)), error = function(e) list())
      })
      all_names <- unique(unlist(lapply(count_cols, names)))
      for (nm in all_names)
        ck[[nm]] <- vapply(count_cols, function(cc) {
          v <- cc[[nm]]; if (is.null(v)) NA_real_ else as.numeric(v)[1]
        }, numeric(1))
      ck$counts <- NULL
      checks_l[[length(checks_l) + 1L]] <- ck
    }
    if (!is.null(j$findings) && is.data.frame(j$findings) &&
        nrow(j$findings) > 0) {
      # Clean + cap character cells here too, so a .checks.json written before
      # the capture-time cleaning existed cannot still corrupt/bloat the CSV.
      fnd <- as.data.frame(lapply(j$findings, .clean_cell),
                           stringsAsFactors = FALSE)
      finds_l[[length(finds_l) + 1L]] <- fnd
    }
  }

  checks <- if (length(checks_l)) dplyr::bind_rows(checks_l) else data.frame()
  finds  <- if (length(finds_l))  dplyr::bind_rows(finds_l)  else data.frame()

  dest <- out_dir %||% results_dir
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(checks, file.path(dest, "_all_checks.csv"),
                   row.names = FALSE, na = "")
  utils::write.csv(finds, file.path(dest, "_all_findings.csv"),
                   row.names = FALSE, na = "")
  message("Wrote check results: ",
          nrow(checks), " paper x module rows (_all_checks.csv), ",
          nrow(finds), " findings (_all_findings.csv), from ",
          length(cfiles), " papers.")
  invisible(list(checks = checks, findings = finds))
}
