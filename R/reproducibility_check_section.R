# The checklist report's own section for reproducibility_check (its
# `checklist_section`, shown under its own heading by .report_checklist_qmd()):
# for each script that was run, what happened, in words a reader of the report
# knows, then the problems in the files and the comparison with the paper.

# The error a script stopped with, from its error output: for R the lines from
# the first "Error" line up to R's own closing lines ("Calls:", "Backtrace:",
# "In addition:", "Execution halted"), for Python the last error line of the
# traceback. "" when there is none.
.repro_error_line <- function(stderr, python = FALSE, max_chars = 300) {
  if (is.null(stderr) || !length(stderr) || is.na(stderr[[1]])) return("")
  lines <- trimws(strsplit(stderr[[1]], "\r?\n")[[1]])
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return("")
  if (python) {
    hit <- grep("^[A-Za-z_.]*(Error|Exception)\\b", lines)
    if (!length(hit)) return("")
    out <- lines[max(hit)]
  } else {
    start <- grep("^Error\\b", lines)
    if (!length(start)) return("")
    start <- start[[1]]
    ends <- grep(paste0("^(Calls:|Backtrace:|In addition:|Execution halted|",
                        "Run `rlang::last_trace|Warning message)"), lines)
    ends <- ends[ends > start]
    end <- if (length(ends)) ends[[1]] - 1 else min(length(lines), start + 5)
    out <- paste(lines[start:end], collapse = " ")
  }
  out <- gsub("\\s+", " ", out)
  if (nchar(out) > max_chars) out <- paste0(substr(out, 1, max_chars - 1), "…")
  out
}

# Text for a table cell, shown as it is: HTML-escaped, and the characters
# Markdown would read as formatting (or "|", which ends a table cell) written
# as HTML character references.
.repro_cell <- function(x) {
  x <- .spv_html_escape(x)
  for (ch in c("\\", "|", "*", "_", "`", "$", "[", "]", "~", "^")) {
    x <- gsub(ch, sprintf("&#%d;", utf8ToInt(ch)), x, fixed = TRUE)
  }
  x
}

# The section, as R Markdown. `table` is the module's per-script table,
# `run_results` its per-script run results, `missing_inputs` the rows of
# repro_missing_inputs() for both languages, `n_dup` the number of identical
# copies that were not run again, `n_tests`/`n_found` the comparison with the
# paper (NA when there was none), `recorded_note` a sentence on the recorded
# package versions used (see .repro_recorded_summary()). NULL when no script
# was run.
.repro_checklist_section <- function(table, run_results, missing_inputs = NULL,
                                     n_dup = 0, n_tests = NA, n_found = NA,
                                     sandbox = "docker", timeout = 600,
                                     install_missing = FALSE, recorded_note = NULL) {
  if (is.null(table) || !nrow(table) || is.null(run_results) || !nrow(run_results)) {
    return(NULL)
  }
  tbl <- table[order(is.na(table$run_order), table$run_order), , drop = FALSE]
  is_py <- grepl("\\.(py|ipynb)$", tbl$file_name, ignore.case = TRUE)
  rr <- run_results[match(tbl$file_name, run_results$file_name), , drop = FALSE]
  outcome <- ifelse(is.na(rr$outcome), tbl$outcome, rr$outcome)
  # a file that was not handed to R or Python: why, from the static check
  outcome[is.na(outcome)] <- paste0("static_", tbl$not_runnable_reason[is.na(outcome)])

  # why an input file is not available, from repro_missing_inputs()
  mi <- missing_inputs %||% data.frame(basename = character(0))
  input_reason <- function(f) {
    i <- match(tolower(f), tolower(mi$basename))
    if (is.na(i)) return(sprintf("%s (not in the repository)", f))
    why <- switch(mi$status[[i]],
      withheld_size = "too large for us to download",
      in_archive = "only inside a .zip or .tar file",
      in_repo_not_downloaded = "in the repository, but not downloaded",
      "not in the repository")
    similar <- mi$similar_to[[i]] %||% NA_character_
    if (!is.na(similar) && nzchar(similar)) {
      why <- sprintf("%s, but %s is", why, similar)
    }
    sprintf("%s (%s)", f, why)
  }

  used_by <- if ("used_by" %in% names(tbl)) tbl$used_by else rep(NA_character_, nrow(tbl))

  # one row per script: its result and what happened, in run order
  rows <- vapply(seq_len(nrow(tbl)), \(i) {
    lang <- if (is_py[[i]]) "Python" else "R"
    err <- .repro_error_line(rr$stderr[[i]], python = is_py[[i]])
    if (!nzchar(err)) err <- .repro_error_line(rr$error[[i]], python = is_py[[i]])
    se <- rr$stderr[[i]] %||% ""
    if (is.na(se)) se <- ""
    pkg <- regmatches(se, regexpr(paste0("(no package called|No module named) ",
                                         "['\"‘]([^'\"’]+)['\"’]"), se))
    pkg <- if (length(pkg)) sub(".*['\"‘]([^'\"’]+)['\"’].*", "\\1", pkg) else ""
    inputs <- strsplit(tbl$unresolved_inputs[[i]] %||% NA_character_, ", ", fixed = TRUE)[[1]]
    inputs <- inputs[!is.na(inputs) & nzchar(inputs)]
    res <- switch(outcome[[i]],
      ran_ok = c("Ran to the end", ""),
      run_by_source = c("Ran inside another script",
                        "Another script runs it with source(), so it ran as part of that script."),
      run_by_import = c("Ran inside another script",
                        "Another script imports it, so it ran as part of that script."),
      errored = c("Stopped with an error",
                  if (nzchar(err)) sprintf("<code>%s</code>", .repro_cell(err))
                  else "The script stopped with an error (see its output below)."),
      timed_out = c("Ran out of time",
                    sprintf("It was still running after %s seconds, our time limit per script.",
                            timeout)),
      dependency_unavailable = c("Could not run on our server", sprintf(
        "It needs the %s package %s, which %s. This is not an error in the script.",
        lang, if (nzchar(pkg)) .repro_cell(pkg) else "(see its output below)",
        if (isTRUE(install_missing)) "we could not install"
        else "is not installed on our server (we do not install packages)")),
      skipped_missing_inputs = c("Not run", sprintf(
        "It reads %s that %s not available: %s.",
        if (length(inputs) == 1) "a file" else "files",
        if (length(inputs) == 1) "is" else "are",
        .repro_cell(paste(vapply(inputs, input_reason, character(1)), collapse = "; ")))),
      skipped_time_limit = c("Not run",
                             "The time for all scripts together had run out before it could start."),
      not_parsed = , static_parse_error = c("Not run", sprintf(
        "%s cannot read the script: it contains a syntax error.", lang)),
      python2 = , static_python2 = c("Not run", paste0(
        "It is written for Python 2, which has been unsupported since 2020. We run Python 3, ",
        "and do not convert code, since that can change results.")),
      static_unplaceable = c("Not run",
                             "We could not work out when to run it, relative to the other scripts."),
      jags_model = , static_jags_model = c("Not run", "It is a JAGS model, which another script reads."),
      not_r_content = , static_not_r_content = c("Not run", "It is not R code that can run on its own."),
      defines_functions = , static_defines_functions = c(
        "Not run",
        if (!is.na(used_by[[i]])) sprintf("It only defines functions, which %s uses.", .repro_cell(used_by[[i]]))
        else "It only defines functions, and no script in the deposit uses them."),
      docker_failed = c("Not run", "Docker failed on our side, so this says nothing about the script."),
      not_analysis = , static_not_analysis = c(
        "Not run", "It is a test of a package or the file that installs it, not part of the analysis."),
      c("Not run", ""))
    sprintf("| %s | %s | %s |", .repro_cell(tbl$file_name[[i]]), res[[1]], res[[2]])
  }, character(1))

  # the opening sentences: what was run, and how
  counted <- !outcome %in% c("run_by_source", "run_by_import", "jags_model", "not_r_content",
                             "static_jags_model", "static_not_r_content",
                             "defines_functions", "not_analysis",
                             "static_defines_functions", "static_not_analysis")
  n_r <- sum(!is_py & counted)
  n_py <- sum(is_py & counted)
  scripts <- paste(c(
    if (n_r) sprintf("%d R script%s", n_r, plural(n_r)),
    if (n_py) sprintf("%d Python script%s", n_py, plural(n_py))), collapse = " and ")
  how <- if (sandbox == "docker") "each in an isolated container" else
    "each in a separate R or Python session"
  intro <- sprintf(
    "We ran the paper's %s in the order they depend on each other, %s, for at most %s minute%s per script.%s",
    scripts, how, format(round(timeout / 60, 1)), plural(round(timeout / 60, 1)),
    if (!isTRUE(install_missing)) " We did not install packages the scripts need." else "")
  if (length(recorded_note) > 0) intro <- paste(intro, recorded_note)
  if (n_dup > 0) {
    intro <- paste(intro, sprintf(
      "%d identical cop%s of these scripts, which other links in the paper lead to, %s not run again.",
      n_dup, plural(n_dup, "y", "ies"), plural(n_dup, "was", "were")))
  }

  # the headline: how many ran to the end, and what happened to the others
  groups <- c(errored = "stopped with an error", timed_out = "ran out of time",
              dependency_unavailable = "could not run on our server",
              docker_failed = "could not run on our server")
  n_ok <- sum(outcome[counted] == "ran_ok")
  parts <- vapply(names(groups), \(g) {
    n <- sum(outcome[counted] == g)
    if (n) sprintf("%d %s", n, groups[[g]]) else ""
  }, character(1))
  n_not_run <- sum(counted) - n_ok - sum(outcome[counted] %in% names(groups))
  parts <- c(parts[nzchar(parts)],
             if (n_not_run) sprintf("%d %s not run", n_not_run, plural(n_not_run, "was", "were")))
  headline <- sprintf("**%d of %d script%s ran to the end**%s.",
                      n_ok, sum(counted), plural(sum(counted)),
                      if (length(parts)) paste0(": ", paste(parts, collapse = ", ")) else "")

  # problems in the files the table does not show: setwd() / os.chdir()
  setwd_n <- suppressWarnings(as.integer(tbl$setwd_removed))
  setwd_n[is.na(setwd_n)] <- 0L
  problems <- vapply(which(setwd_n > 0), \(i) {
    fun <- if (is_py[[i]]) "os.chdir()" else "setwd()"
    sprintf(paste0("- %s calls %s, which ties the script to a folder on the ",
                   "author's computer. We replaced the call with one that does ",
                   "nothing so the script could run; it should be removed from the script."),
            .repro_cell(tbl$file_name[[i]]), fun)
  }, character(1))
  # file names spelled otherwise in the code than in the deposit
  names_fixed <- if ("names_corrected" %in% names(tbl)) tbl$names_corrected else rep(NA_character_, nrow(tbl))
  problems <- c(problems, vapply(which(!is.na(names_fixed)), \(i) {
    sprintf(paste0("- %s reads files under another name than the deposit holds (%s). We ",
                   "corrected the names so the script could run; they should be changed in the script."),
            .repro_cell(tbl$file_name[[i]]), .repro_cell(names_fixed[[i]]))
  }, character(1)))
  # notebook commands left out to run the code
  commands <- if ("commands_removed" %in% names(tbl)) tbl$commands_removed else rep(NA_character_, nrow(tbl))
  problems <- c(problems, vapply(which(!is.na(commands)), \(i) {
    sprintf(paste0("- %s holds notebook commands that are not Python (%s). We left them out ",
                   "so the code could run; what they install or prepare was not done."),
            .repro_cell(tbl$file_name[[i]]), .repro_cell(commands[[i]]))
  }, character(1)))

  # the comparison with the paper
  if (is.null(n_found) || is.na(n_found)) n_found <- 0L
  comparison <- if (is.null(n_tests) || is.na(n_tests) || n_tests == 0) {
    "We found no statistical tests in the paper to compare with the output of the code."
  } else {
    paste0(
      sprintf("Of the %d statistical test%s reported in the paper, %d %s found in the output of the code.",
              n_tests, plural(n_tests), n_found, plural(n_found, "was", "were")),
      if (n_ok == 0) " No script ran to the end, so most results were never produced." else "")
  }

  c(
    intro,
    headline,
    paste(c("| Script | Result | What happened |", "|---|---|---|", rows), collapse = "\n"),
    if (length(problems)) c("### Other problems in the files", paste(problems, collapse = "\n")),
    "### Comparison with the paper",
    comparison
  ) |> paste(collapse = "\n\n")
}
