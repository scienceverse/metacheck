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

# Flatten one paper's module-output chain (from report_module_run()) into two
# tidy pieces for on-disk capture during a run:
#   * checks:   one row per module — paper_id, module, traffic_light,
#               summary_text, and the module's summary_table count columns
#               (JSON-encoded so a single column holds a variable set of counts).
#   * findings: one row per row of each module's `table` (the row-level flagged
#               items), tagged with paper_id + module.
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

  checks <- list()
  findings <- list()
  for (nm in names(mods)) {
    mo <- mods[[nm]]
    if (is.null(mo)) next
    pid <- pid_of(mo)

    st <- mo$summary_table
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
      tbl <- as.data.frame(lapply(tbl, function(col)
        if (is.list(col)) vapply(col, function(x)
          paste(as.character(x), collapse = "; "), character(1))
        else col), stringsAsFactors = FALSE)
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
        nrow(j$findings) > 0)
      finds_l[[length(finds_l) + 1L]] <- j$findings
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
