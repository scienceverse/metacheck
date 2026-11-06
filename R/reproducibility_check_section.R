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

# Packages removed from CRAN, with the packages that replaced them: rgdal, rgeos
# and maptools were retired in October 2023, and their successors are sf and terra.
.repro_retired_packages <- c(rgdal = "sf or terra", rgeos = "sf or terra", maptools = "sf")

# Errors of code written for older versions of a package: a function or argument
# that was removed or renamed since, as group_indices()'s arguments, ggplot2's
# "Cannot add <ggproto> objects together", or "is defunct" (#575).
.repro_changed_package_pat <- paste0(
  "defunct|deprecated|is not an exported object|unused argument|Cannot add <ggproto>|",
  "was removed in|no longer supported|has been removed|",
  # Python: keras.utils.generic_utils, requests_cache.core
  "module '[^']+' has no attribute|cannot import name|No module named '[^']+\\.[^']+'")

# How to make the code run on another computer, from what stopped each script
# (#575): one block per problem, in the order in which they should be fixed
# (a problem near the top can cause the ones below it), each with the scripts
# it affects, why it fails, and the steps to fix it. It is written for the
# authors, as guidance a student can follow. A failure whose cause the output
# does not show gets a block that says so, rather than a guess. `rr` holds the
# run results in the order of `tbl`; `outcome` the outcome per row; `mi` the
# rows of repro_missing_inputs(); `install_results` the install of the
# packages; `versions_recorded` whether the deposit records package versions
# (see .repro_recorded_advice()); `timeout` the time limit per script, in
# seconds. Returns the blocks (markdown), with the rows they cover as the
# attribute `scripts`; character(0) when nothing failed.
.repro_advice <- function(tbl, rr, outcome, mi, install_results = NULL, versions_recorded = FALSE,
                          timeout = 600) {
  name <- tbl$file_name
  is_py <- grepl("\\.(py|ipynb)$", name, ignore.case = TRUE)
  msg <- paste(ifelse(is.na(rr$error), "", rr$error), ifelse(is.na(rr$stderr), "", rr$stderr))
  reason <- if ("reason" %in% names(rr) && any(!is.na(rr$reason))) rr$reason else
    .repro_failure_reason(outcome, msg)
  key <- vapply(seq_along(msg), \(i) .repro_error_line(msg[[i]], python = is_py[[i]], max_chars = 5000), character(1))
  err <- outcome %in% "errored"
  writes <- if ("writes" %in% names(tbl)) strsplit(tolower(ifelse(is.na(tbl$writes), "", tbl$writes)), ",\\s*") else
    vector("list", nrow(tbl))
  mi_base <- tolower(mi$basename %||% character(0))
  status <- mi$status %||% character(0)
  detail <- mi$detail %||% character(0)
  similar <- mi$similar_to %||% rep(NA_character_, length(mi_base))

  code <- function(x) sprintf("<code>%s</code>", .repro_cell(x))
  # the lines naming the scripts of a problem: at most ten, then how many more
  lines_of <- function(x) {
    x <- unique(x)
    out <- paste0("- ", utils::head(x, 10))
    if (length(x) > 10) out <- c(out, sprintf("- and %d more", length(x) - 10))
    paste(out, collapse = "\n")
  }
  steps_of <- function(...) paste(sprintf("%d. %s", seq_along(c(...)), c(...)), collapse = "\n")
  blocks <- list()
  covered <- integer(0)
  block <- function(id, rows, title, lines = character(0), text = character(0), steps = character(0)) {
    if (!length(rows)) return(invisible())
    blocks[[id]] <<- paste(c(sprintf("**%s**", title), if (length(lines)) lines_of(lines),
                             text, if (length(steps)) do.call(steps_of, as.list(steps))), collapse = "\n\n")
    covered <<- union(covered, rows)
  }

  # A file a script could not open: what kind of file it is decides what helps.
  f <- list(by_script = list(), absolute = list(), renamed = list(), archived = list(), absent = list())
  put <- function(kind, i, line) f[[kind]][[length(f[[kind]]) + 1]] <<- list(i = i, line = line)
  for (i in which(err & reason %in% "file")) {
    for (p in .repro_unopened_files(msg[[i]], full = TRUE)) {
      b <- tolower(basename(p)); k <- match(b, mi_base)
      maker <- setdiff(which(vapply(writes, \(w) b %in% w, logical(1))), i)
      if (length(maker)) {
        put("by_script", i, sprintf("%s needs %s, which %s makes.", code(name[[i]]), code(basename(p)),
                                    code(name[[maker[[1]]]])))
      } else if (!is.na(k) && !is.na(similar[[k]])) {
        put("renamed", i, sprintf("%s reads %s; your deposit holds %s.", code(name[[i]]), code(basename(p)),
                                  code(similar[[k]])))
      } else if (!is.na(k) && status[[k]] %in% "in_archive") {
        arch <- sub("^.*only inside (.*?); unpack.*$", "\\1", detail[[k]])
        put("archived", i, sprintf("%s reads %s, which is inside %s.", code(name[[i]]), code(p),
                                   if (identical(arch, detail[[k]])) "an archive" else code(arch)))
      } else if (grepl("^(/(?!sandbox/)|~|[A-Za-z]:)", p, perl = TRUE)) {
        put("absolute", i, sprintf("%s reads %s.", code(name[[i]]), code(p)))
      } else {
        put("absent", i, sprintf("%s reads %s.", code(name[[i]]), code(basename(p))))
      }
    }
  }
  rows_of <- function(l) vapply(l, `[[`, integer(1), "i")
  text_of <- function(l) vapply(l, `[[`, character(1), "line")
  block("by_script", rows_of(f$by_script), "A script needs a file that another script should have made.",
        text_of(f$by_script), paste(
          "The other script stopped before it made the file, so fix that script first; this one may",
          "then run as well. To make the order clear to readers, number your scripts in the order",
          "they run (<code>01&#95;clean.R</code>, <code>02&#95;model.R</code>), or add one main script that runs the others in order."))
  block("absent", rows_of(f$absent), "Files are missing from your deposit.", text_of(f$absent),
        "The script stops at the line that reads the file.",
        c("Find the file on your computer.",
          "Add it to the deposit, in the folder where the script looks for it.",
          "If you cannot share it, for example for privacy reasons, say so in the README, and explain where readers can get it and where to put it."))
  block("absolute", rows_of(f$absolute), "A script uses a location that exists only on your computer.",
        text_of(f$absolute), "No other computer has that folder, and the file is also not in the deposit.",
        c("Add the file to the deposit.",
          paste("In the script, write the location from the top folder of your project, such as",
                "<code>data/survey.csv</code>. In R, <code>here::here(\"data\", \"survey.csv\")</code> does this for you."),
          "Remove any <code>setwd()</code> lines."))
  block("renamed", rows_of(f$renamed), "A file may have been renamed.", text_of(f$renamed),
        "Check whether they are the same file. If so, use one name in both the script and the deposit.")
  block("archived", rows_of(f$archived), "A file is only inside a zip file.", text_of(f$archived), paste(
    "Code cannot read a file inside an archive until someone unpacks it. Either upload the files",
    "unpacked, in the folders the script expects, or start the script with a line that unpacks the",
    "archive, such as <code>unzip(\"Data.zip\")</code> in R."))
  unnamed <- setdiff(which(err & reason %in% "file"), covered)
  block("unnamed", unnamed, "A script could not open a file, but the error does not say which.", code(name[unnamed]),
        paste("Check every file the script reads. Make sure each one is in the deposit, and that its",
              "location is written from the top folder of the project."))

  # Code written for older versions of its packages.
  changed <- which(err & reason %in% c("other", "package") &
                     grepl(.repro_changed_package_pat, key, ignore.case = TRUE))
  short <- function(i) sub("^Error( in [^:]*)? *: *", "", substr(key[[i]], 1, 160))
  block("changed", changed, "Your code was written for older versions of its packages.",
        sprintf("%s: \"%s\"", code(name[changed]), .repro_cell(vapply(changed, short, character(1)))),
        paste("A function or option has changed since you wrote the code, so it stops with today's",
              "version of the package."),
        if (isTRUE(versions_recorded))
          character(0) else
          c(paste("In R, run <code>renv::init()</code> in your project and add the <code>renv.lock</code>",
                  "file this creates to the deposit; in Python, run <code>pip freeze &gt; requirements.txt</code>",
                  "and add that file."),
            "State your version of R or Python in the README.",
            "Better still, update the code so it also works with current versions."))
  if (isTRUE(versions_recorded) && !is.null(blocks$changed))
    blocks$changed <- paste(blocks$changed, paste(
      "Your deposit records which package versions you used, and a run with those versions may",
      "succeed. Check that the record lists every package the scripts load."), sep = "\n\n")

  # A package the code loads that was not there, which the install did not know of
  pkg_of <- function(m) {
    hit <- regmatches(m, regexec("(?:no package called|No module named) ['\"‘]([^'\"’]+)['\"’]", m, perl = TRUE))[[1]]
    if (length(hit) > 1) hit[2] else NA_character_
  }
  missing_pkg <- setdiff(which(err & reason %in% "package"), covered)
  mp <- vapply(missing_pkg, \(i) pkg_of(msg[[i]]), character(1))
  block("missing_pkg", missing_pkg, "A package was not installed.",
        ifelse(is.na(mp), code(name[missing_pkg]), sprintf("%s: %s", code(name[missing_pkg]), code(mp))),
        paste("The code loads it in a way we could not detect before running it. List every package",
              "with its version, in <code>renv.lock</code> (R) or <code>requirements.txt</code> (Python), and load",
              "each one at the top of the script with <code>library()</code> or <code>import</code>."))

  # A package that could not be installed.
  ir <- install_results
  failed <- if (!is.null(ir) && nrow(ir) && all(c("package", "installed") %in% names(ir)))
    ir[!ir$installed %in% TRUE, , drop = FALSE] else data.frame(package = character(0), category = character(0))
  dep <- which(outcome %in% "dependency_unavailable")
  if (length(dep)) {
    dp <- vapply(dep, \(i) pkg_of(msg[[i]]), character(1))
    pkgs <- unique(dp[!is.na(dp)])
    explain <- vapply(pkgs, \(p) {
      if (p %in% names(.repro_retired_packages))
        sprintf(paste("%s was removed from CRAN in 2023 and can no longer be installed. Replace it with %s,",
                      "which do the same work, and check that your results stay the same."),
                code(p), paste(code(strsplit(.repro_retired_packages[[p]], " or ", fixed = TRUE)[[1]]), collapse = " or "))
      else if (p %in% failed$package[failed$category %in% "source_unknown"])
        sprintf(paste("%s is not on CRAN, and your code does not say where it comes from. Add the command",
                      "that installs it to the README, for example %s."),
                code(p), code(sprintf("remotes::install_github(\"user/%s\")", p)))
      else
        sprintf(paste("%s could not be installed with the current version of R, probably because it is old.",
                      "Record the versions you used (in R, <code>renv::init()</code> writes them to a <code>renv.lock</code> file)",
                      "and your R version. To be",
                      "sure the code keeps running, provide a Docker image: a frozen copy of the whole computer",
                      "setup the code needs, which you can build from <code>rocker/r-ver</code> at your R version."), code(p))
    }, character(1))
    block("dep", dep, "A package could not be installed.",
          ifelse(is.na(dp), code(name[dep]), sprintf("%s needs %s.", code(name[dep]), code(dp))),
          unname(explain))
  }

  # What only the authors' computer, or another computer than ours, has.
  program <- .repro_missing_program(msg)
  prog <- which(err & !is.na(program))
  block("program", prog, "A script starts a program outside R or Python.",
        sprintf("%s starts %s.", code(name[prog]), code(program[prog])),
        paste("Our computer does not have this program, and neither will most readers'. Name the program,",
              "its version and where to get it in the README. A Docker image can include it."))
  memory <- which(outcome %in% "out_of_memory" | (err & .repro_out_of_memory(msg)))
  keys <- setdiff(which(err & grepl("API key|api[_ ]key|set env(ironment)? var", key, ignore.case = TRUE)), prog)
  windows <- setdiff(which(err & grepl("RODBC|odbcConnect|\\.accdb|\\.mdb\\b", msg)), prog)
  env <- setdiff(which(err & reason %in% "environment"), c(prog, memory, keys, windows))
  block("env", env, "A script needs software our computer does not have.",
        sprintf("%s: \"%s\"", code(name[env]), .repro_cell(vapply(env, short, character(1)))),
        paste("Name everything the code needs beyond R and Python packages in the README; a Docker image",
              "can include all of it."))
  block("keys", keys, "A script uses an online service that needs a personal key.", code(name[keys]),
        paste("Say in the README which service it is and how to get a key. Services change or close (Dark",
              "Sky has closed), so also save the data the script downloads, add it to the deposit, and let",
              "the script read the saved copy."))
  block("windows", windows, "A script reads a Microsoft Access database, which works only on Windows.",
        code(name[windows]), "Export the tables to CSV files, add them to the deposit, and read those instead.")

  # What the code itself does.
  undefined <- setdiff(which(err & reason %in% "undefined"), covered)
  obj <- vapply(undefined, \(i) {
    hit <- regmatches(key[[i]], regexec("object '([^']+)' not found|could not find function \"([^\"]+)\"|name '([^']+)' is not defined", key[[i]]))[[1]]
    hit <- hit[-1][nzchar(hit[-1])]
    if (length(hit)) hit[[1]] else NA_character_
  }, character(1))
  block("undefined", undefined, "A script uses something it does not create or load itself.",
        ifelse(is.na(obj), code(name[undefined]), sprintf("%s: %s", code(name[undefined]), code(obj))),
        "Usually the object was still in your R session from earlier work, or another script made it.",
        c("Restart R (in RStudio: Session > Restart R).",
          "Run each script from top to bottom, and fix what is missing.",
          "If a script needs results of another, save them to a file in the first script and read them in the second.",
          "Load every package with <code>library()</code> at the top."))
  syntax <- which(outcome %in% c("not_parsed", "static_parse_error") | (err & reason %in% "syntax"))
  block("syntax", syntax, "R or Python could not read a script, because of a mistake in the code itself.",
        code(name[syntax]),
        "Run each script once in a fresh session before you deposit it; the error message shows the line to fix.")
  slow <- which(outcome %in% c("timed_out", "skipped_time_limit"))
  minutes <- format(round(timeout / 60, 1))
  block("slow", slow, sprintf("A script runs longer than our limit of %s minute%s.", minutes, plural(as.numeric(minutes))),
        ifelse(outcome[slow] %in% "skipped_time_limit",
               sprintf("%s was not started, because the time for all scripts together had run out.", code(name[slow])),
               code(name[slow])),
        paste("State in the README how long each script runs. If some steps take hours, save their results",
              "(such as fitted models) to files, add those to the deposit, and let the script read them, so",
              "readers can check the rest without waiting."))
  gb <- regmatches(msg[memory], regexpr("memory limit of [0-9.]+ GB", msg[memory]))
  if (!length(gb) && !is.na(.repro_memory_limit_text())) gb <- paste("memory limit of", .repro_memory_limit_text())
  block("memory", memory, if (length(gb)) sprintf("A script needs more memory than our %s.", sub("memory limit of", "limit of", gb[[1]]))
        else "A script needs more memory than our limit.",
        code(name[memory]), "State in the README how much memory the code needs.")
  online <- which(outcome %in% c("needs_internet", "download_too_large"))
  block("online", online, "A script downloads data while it runs.", code(name[online]),
        paste("Online files can move, change or disappear. Save the downloaded files, add them to the",
              "deposit, and let the script read the saved copies."))
  person <- which(outcome %in% c("needs_a_person", "static_needs_a_person"))
  block("person", person, "A script waits for a person to choose a file, answer a question, or give arguments on the command line.",
        code(name[person]),
        paste("Nobody can answer when the code is checked automatically. Write the file name into the",
              "script, and put the exact command with its arguments in the README."))

  # A failure no block explains is said to be one, not left out
  unknown <- setdiff(which(err | outcome %in% c("dependency_unavailable", "timed_out", "skipped_time_limit",
                                                 "needs_internet", "download_too_large", "needs_a_person",
                                                 "not_parsed", "out_of_memory")), covered)
  block("unknown", unknown, "Metacheck can't automatically establish what caused the error, or how it should be improved.",
        code(name[unknown]), "The error message of each script is in the table above.")

  # in the order in which the problems should be fixed
  order <- c("by_script", "absent", "absolute", "renamed", "archived", "unnamed", "changed", "missing_pkg",
             "dep", "program", "env", "keys", "windows", "undefined", "syntax", "slow", "memory", "online",
             "person", "unknown")
  structure(as.character(unlist(blocks[intersect(order, names(blocks))], use.names = FALSE)),
            scripts = sort(covered))
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
# package versions used (see .repro_recorded_summary()), `source_unknown` the
# packages found nowhere (see .repro_source_unknown()), `install_results` the
# install of the packages and `versions_recorded` whether the deposit records
# package versions, both for what would make the code run (see .repro_advice()).
# NULL when no script was run.
.repro_checklist_section <- function(table, run_results, missing_inputs = NULL,
                                     n_dup = 0, n_tests = NA, n_found = NA,
                                     sandbox = "docker", timeout = 600,
                                     install_missing = FALSE, recorded_note = NULL,
                                     net = NULL, source_unknown = character(0),
                                     install_results = NULL, versions_recorded = FALSE) {
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
  # why a file is not part of the analysis (see .repro_software_kind())
  not_analysis_why <- .repro_not_analysis_why(
    if ("not_analysis_why" %in% names(tbl)) tbl$not_analysis_why else rep(NA_character_, nrow(tbl)))

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
      timed_out = c("Ran out of time", paste0(
        sprintf("It was still running after %s seconds, our time limit per script", timeout),
        if (grepl("while a download was still arriving", rr$error[[i]] %||% ""))
          ", while it was still downloading." else ".")),
      download_too_large = c("Stopped", paste0(
        "It downloaded more than our limits allow",
        if (grepl("^stopped: ", rr$error[[i]] %||% ""))
          sprintf(" (%s)", .repro_cell(sub("^stopped: ", "", rr$error[[i]]))),
        ".")),
      dependency_unavailable = c("Could not run on our server", sprintf(
        "It needs the %s package %s, which %s. This is not an error in the script.",
        lang, if (nzchar(pkg)) .repro_cell(pkg) else "(see its output below)",
        if (pkg %in% source_unknown)
          "is not on CRAN or Bioconductor, and the code does not say where it comes from"
        else if (isTRUE(install_missing)) "we could not install"
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
      not_downloaded = c("Not tried", paste(
        "It needs a file that the deposit holds but that we did not download, such as one",
        "over our size limit, so this says nothing about the script.")),
      out_of_memory = c("Ran out of memory", if (is.na(.repro_memory_limit_text()))
        "It needed more memory than it could use here." else
        sprintf("It needed more than the %s of memory it could use here.", .repro_memory_limit_text())),
      needs_internet = c("Stopped",
        if (is.null(net)) "It needs the internet, which is off while we run code."
        else "It could not reach what it asked for on the internet."),
      needs_a_person = , static_needs_a_person = c(
        "Stopped", "It asks a person to choose a file or answer a question, starts an app, or is a program that needs its arguments."),
      not_analysis = , static_not_analysis = c("Not run", .repro_cell(not_analysis_why[[i]])),
      c("Not run", ""))
    sprintf("| %s | %s | %s |", .repro_cell(tbl$file_name[[i]]), res[[1]], res[[2]])
  }, character(1))

  # the opening sentences: what was run, and how
  counted <- !outcome %in% c("run_by_source", "run_by_import", "jags_model", "not_r_content",
                             "static_jags_model", "static_not_r_content",
                             "defines_functions", "not_analysis",
                             "static_defines_functions", "static_not_analysis",
                             "not_downloaded")
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
  if (!is.null(net)) intro <- paste(intro, .repro_internet_text(net))
  if (length(recorded_note) > 0) intro <- paste(intro, recorded_note)
  if (n_dup > 0) {
    intro <- paste(intro, sprintf(
      "%d identical cop%s of these scripts, which other links in the paper lead to, %s not run again.",
      n_dup, plural(n_dup, "y", "ies"), plural(n_dup, "was", "were")))
  }

  # the headline: how many ran to the end, and what happened to the others
  groups <- c(errored = "stopped with an error", timed_out = "ran out of time",
              dependency_unavailable = "could not run on our server",
              docker_failed = "could not run on our server",
              needs_a_person = "needed a person", needs_internet = "needed the internet",
              out_of_memory = "ran out of memory",
              download_too_large = "downloaded too much")
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
  n_nd <- sum(outcome %in% "not_downloaded")
  if (n_nd) headline <- paste(headline, sprintf(
    "%d more %s not tried, because %s a file that we did not download.",
    n_nd, plural(n_nd, "script was", "scripts were"), plural(n_nd, "it needs", "they need")))

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

  # what the authors can change so the code runs, one line per cause (#575)
  advice <- .repro_advice(tbl, rr, outcome, mi, install_results, versions_recorded, timeout = timeout)

  c(
    intro,
    headline,
    paste(c("| Script | Result | What happened |", "|---|---|---|", rows), collapse = "\n"),
    if (length(advice)) c("### How to make your code run on another computer", paste(
      "We ran your code on a clean computer, the way a reader or reviewer would. Below is each",
      "problem we found, the scripts it affects, and how to fix it. Fix them in the order given:",
      "a problem near the top can cause the ones below it."), advice),
    if (length(problems)) c("### Other problems in the files", paste(problems, collapse = "\n")),
    "### Comparison with the paper",
    comparison
  ) |> paste(collapse = "\n\n")
}
