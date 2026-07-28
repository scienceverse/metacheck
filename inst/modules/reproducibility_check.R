#' Reproducibility Check
#'
#' @description
#' This module assesses whether a paper's code can be run on its data. It works
#' in two phases. The **static** phase (always run) collects the package
#' dependencies the code declares, rewrites each script's data-file paths to the
#' Psych-DS layout the release uses, works out the order the scripts must run in,
#' and diagnoses why any referenced input file is unavailable — reporting what a
#' reproduction attempt would involve and where it is likely to break. The
#' **execution** phase (opt-in, `execute = TRUE`) then actually runs the code:
#' it materialises the Psych-DS layout into a throwaway directory, optionally
#' installs the declared dependencies into a throwaway library, runs each script
#' in order in an isolated subprocess with a per-script timeout, and captures the
#' outcome and full output of each run.
#'
#' @details
#' By default (`execute = FALSE`) the module does **not run any code** — it
#' performs only static analysis: reading dependencies with the same extractors
#' as `code_check`, mapping referenced files to their new locations with the
#' plan from `psychds_check`, and building a run-order dependency graph. It
#' reports what *would* be installed and run rather than doing it.
#'
#' When `execute = TRUE`, the module additionally **runs the downloaded code on
#' your machine** — a deliberate, opt-in action gated behind that argument
#' (`callr` is required). Each script runs in an isolated subprocess so a crash
#' cannot take down the R session; note this isolates crashes, **not** the
#' filesystem or network. Installs (when `install_missing = TRUE`) and the
#' throwaway run library never touch your main R library. Each script's outcome
#' is one of `ran_ok`, `errored`, `timed_out`, `skipped_missing_inputs`, or
#' `not_parsed`; a timeout means "still running at the cutoff", not a failure, so
#' raise `timeout` for legitimately long-running (e.g. Bayesian) scripts. The
#' full stdout/stderr of every run is kept in the result's `run_results` (the
#' report shows it in per-script dropdowns, capped for readability). The traffic
#' light stays **static** — it reflects readiness to run, and execution outcomes
#' are reported separately without recolouring it.
#'
#' Dependencies are reported name-only, with the source (CRAN, GitHub, or a base
#' package already shipped with R) — static analysis cannot know which *version*
#' was used, and running against current package versions is itself informative:
#' a break argues the authors should have pinned versions.
#'
#' Run order is derived from read-after-write data dependencies (a script writing
#' a file another reads must run first), `source()` edges, and numeric filename
#' prefixes as a weak tie-breaker. Ambiguous orderings, dependency cycles, and
#' inputs a script reads but that no earlier script produces (and that are not in
#' the repository) are surfaced as warnings, since these are the situations most
#' likely to make an otherwise-correct reproduction fail.
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object, or NULL to check local files
#'   only (see [test_paper()])
#' @param local_path optional path to a local directory, passed through to
#'   `data_check` / `repo_check` when their output is not already available
#' @param local_only if TRUE, skip online repository lookups (see `repo_check`)
#' @param model the LLM model name used only when `llm_use(TRUE)` (passed to the
#'   upstream modules whose study grouping the path rewrite relies on)
#' @param params a named list passed to `llm()`, used only when `llm_use(TRUE)`
#' @param execute if TRUE, actually RUN the paper's code (in isolated
#'   subprocesses, against a throwaway copy of the Psych-DS layout). This runs
#'   downloaded code on your machine and is off by default; it is a deliberate,
#'   opt-in action. `callr` is required.
#' @param install_missing if TRUE (and `execute = TRUE`), install the code's
#'   declared dependencies into a throwaway temp library before running (CRAN via
#'   `install.packages`, GitHub/URL via `remotes`). Default FALSE: a script
#'   needing an absent package simply errors, recorded as its outcome.
#' @param cran_install_main if TRUE (and `install_missing` and `execute` are
#'   both TRUE), CRAN-source dependencies are installed into your DEFAULT R
#'   library instead of the throwaway one, and so persist after the run — see
#'   [repro_install_deps()]. Useful when calling `reproducibility_check()` over
#'   many papers in one script: a CRAN package installed for an early paper is
#'   already present (and skipped) for every later paper needing it, instead of
#'   being reinstalled into a fresh throwaway library each time. GitHub/URL
#'   sources are unaffected — always installed into the throwaway library.
#'   Default FALSE (everything throwaway, nothing persists).
#' @param timeout per-script timeout in seconds for the execute phase
#'   (default 600).
#' @param keep_sandbox if TRUE, do not delete the temp materialised layout
#'   (data/, statistical_output/, and — when `execute = TRUE` — the rewritten
#'   scripts + temp library) after the run, and return its path as attribute
#'   `"sandbox"` on the result — so you can inspect exactly what ran. Default
#'   FALSE (cleaned up). A materialised layout (and so `statistical_output/`) is
#'   built whenever there is any extracted statistical output (a `.jasp`/`.omv`
#'   file, or — with `execute = TRUE` — executed R code that printed results),
#'   independent of `execute`. **Set `keep_sandbox = TRUE` if you want
#'   [convert_psychds()] to include `statistical_output/` in the archive it
#'   builds** — it copies the folder from `attr(result, "sandbox")` when
#'   present, and silently omits it otherwise (it does not run
#'   `reproducibility_check` itself, so it cannot force this for you).
#'
#' @returns a list
reproducibility_check <- function(paper, local_path = NULL, local_only = FALSE,
                                  model = llm_model(), params = list(),
                                  execute = FALSE, install_missing = FALSE,
                                  cran_install_main = FALSE,
                                  timeout = 600, keep_sandbox = FALSE) {
  # paper <- psychsci[[233]] # to test (many code files, several issues)

  # Executing downloaded code runs it on this machine: gate it behind an explicit
  # opt-in and make callr's presence a hard requirement (it isolates crashes).
  if (isTRUE(execute) && !requireNamespace("callr", quietly = TRUE))
    stop("execute = TRUE needs the 'callr' package (it runs each script in an ",
         "isolated subprocess). Install callr, or leave execute = FALSE.",
         call. = FALSE)

  .pid <- function(...) {
    id <- paper_id(paper)
    for (df in list(...)) {
      if (length(id) > 0) break
      if (!is.null(df) && "paper_id" %in% names(df)) id <- unique(df$paper_id)
    }
    if (length(id) == 0) NA_character_ else id[[1]]
  }

  # ── 1. Inputs from the upstream modules ─────────────────────────────────────
  # code_check gives the code files + their per-file analysis (parse status,
  # packages); psychds_check gives the file→target plan the path rewrite needs;
  # data_check gives the download status behind a missing input. Reuse chained
  # outputs; run what is missing.
  code_tbl     <- get_prev_outputs("code_check", "table")
  plan         <- get_prev_outputs("psychds_check", "table")
  structure_df <- get_prev_outputs("data_check", "structure")

  run_missing <- function(mod) {
    # model / params only go to the modules that accept them (data_check,
    # psychds_check use the LLM for study grouping); code_check has no such
    # arguments, so passing them would error with "unused arguments".
    args <- list(paper, mod, local_only = local_only)
    if (!is.null(local_path)) args$local_path <- local_path
    if (mod %in% c("data_check", "psychds_check")) {
      args$model <- model; args$params <- params
    }
    do.call(module_run, args)
  }
  if (is.null(structure_df)) {
    dc <- run_missing("data_check"); structure_df <- dc$structure
  }
  if (is.null(plan)) {
    pc <- run_missing("psychds_check"); plan <- pc$table
  }
  if (is.null(code_tbl)) {
    cc <- run_missing("code_check"); code_tbl <- cc$table
  }

  # ── SPSS data without syntax ────────────────────────────────────────────────
  # An SPSS data file (.sav/.zsav/.por) with NO SPSS syntax (.sps) means the data
  # was deposited but not the code that produced the results — so that analysis
  # cannot be reproduced from the deposit. This forces the light red (part of the
  # analysis is unreproducible), and we recommend jamovi (.omv) / JASP (.jasp),
  # which bundle data + analyses together and so are self-reproducible. Detected
  # from the file inventory (data_check's structure) so it works even when the
  # paper has no R code at all.
  fnames_all <- if (!is.null(structure_df) && "file_name" %in% names(structure_df))
    structure_df$file_name else character(0)
  has_sav <- any(grepl("\\.(sav|zsav|por)$", fnames_all, ignore.case = TRUE))
  has_sps <- any(grepl("\\.sps$", fnames_all, ignore.case = TRUE))
  has_selfcontained <- any(grepl("\\.(omv|jasp)$", fnames_all, ignore.case = TRUE))
  spss_red <- has_sav && !has_sps
  spss_report <- if (has_sav) {
    rec <- if (!has_selfcontained)
      paste0(" Consider depositing the analysis in **jamovi** (`.omv`) or ",
             "**JASP** (`.jasp`) instead: these formats store the data and the ",
             "analyses together in one file, so the results are reproducible from ",
             "the file itself.") else
      paste0(" (A jamovi/JASP file is also present, which is self-reproducible.)")
    if (!has_sps) c("#### SPSS data without syntax", paste0(
      "An SPSS data file (`.sav`/`.por`) is present but **no SPSS syntax file ",
      "(`.sps`) was found**, so the analysis that produced the results cannot be ",
      "reproduced from the deposit. This is treated as a reproducibility failure ",
      "(red).", rec))
    else c("#### SPSS data", paste0(
      "SPSS data and syntax (`.sav` + `.sps`) are present. Note this module runs ",
      "only R, so it cannot execute the SPSS syntax — the syntax is at least ",
      "documented, but not run here.", rec))
  } else NULL

  # ── JASP / jamovi self-reproducible output ──────────────────────────────────
  # A .jasp/.omv file bundles the data AND its analyses+results together, so it
  # is reproducible from the file itself (no code to run). We EXTRACT the
  # rendered result tables (read_stat_tables), type each statistic with the STATO
  # ontology, and serialise them: a flat queryable form (stat_results_long, for
  # the scienceverse DB) and a full ISA-JSON document (stat_output_isa, for the
  # logs). This runs regardless of whether the paper also has R code.
  stat_output <- NULL          # per-file: list(file, n_tables, isa, long)
  self_repro_report <- NULL
  jasp_omv <- if (!is.null(structure_df) &&
                  all(c("file_name", "file_location") %in% names(structure_df))) {
    hit <- grepl("\\.(jasp|omv)$", structure_df$file_name, ignore.case = TRUE)
    data.frame(file_name = structure_df$file_name[hit],
               file_location = structure_df$file_location[hit],
               stringsAsFactors = FALSE)
  } else data.frame(file_name = character(0), file_location = character(0))

  if (nrow(jasp_omv) > 0 &&
      requireNamespace("xml2", quietly = TRUE) &&
      requireNamespace("rvest", quietly = TRUE)) {
    .pid_here <- .pid(structure_df, code_tbl)
    stat_output <- lapply(seq_len(nrow(jasp_omv)), function(i) {
      loc <- jasp_omv$file_location[i]; fn <- jasp_omv$file_name[i]
      if (is.na(loc) || !nzchar(loc) || !file.exists(loc)) return(NULL)
      tabs <- tryCatch(read_stat_tables(loc), error = function(e) list())
      if (!length(tabs)) return(NULL)
      list(file = fn, n_tables = length(tabs),
           isa  = stat_output_isa(tabs, paper_id = .pid_here, source_file = basename(fn)),
           long = stat_results_long(tabs, paper_id = .pid_here,
                                    source_file = basename(fn)))
    })
    stat_output <- Filter(Negate(is.null), stat_output)
    if (length(stat_output) > 0) {
      n_files  <- length(stat_output)
      n_tables <- sum(vapply(stat_output, `[[`, integer(1), "n_tables"))
      n_stats  <- sum(vapply(stat_output, function(s) nrow(s$long), integer(1)))
      self_repro_report <- c("#### Self-reproducible output (JASP / jamovi)",
        sprintf(paste0(
          "%d JASP/jamovi file%s bundle%s the data and its analyses together, so ",
          "%s reproducible from the file itself. We extracted %d result table%s ",
          "(%d individual statistic%s), typed with the STATO ontology, and export ",
          "them as ISA-JSON (in the logs) and as queryable rows."),
          n_files, plural(n_files), plural(n_files, "s", ""),
          plural(n_files, "it is", "they are"), n_tables, plural(n_tables),
          n_stats, plural(n_stats)))
    }
  }

  # ── Materialised root for the statistical_output/ folder ────────────────────
  # Any extracted statistical output (JASP/jamovi here; executed R console
  # output is added to `stat_output` further down when execute = TRUE) is
  # written to disk as a dedicated statistical_output/ folder, sibling to a
  # data/ copy — same materialised-layout idea repro_materialize_layout()
  # already uses for the execute phase, reused here (and, when execute = TRUE,
  # SHARED with it — see below — rather than building two roots). Built
  # unconditionally so a JASP/jamovi-only paper (execute = FALSE, no R code at
  # all) still gets the folder; cleaned up on exit unless keep_sandbox = TRUE,
  # exactly like the execute-phase sandbox.
  sandbox_root <- NULL
  if (length(stat_output) > 0) {
    sandbox_root <- tempfile("repro_sandbox_")
    if (!isTRUE(keep_sandbox))
      on.exit(unlink(sandbox_root, recursive = TRUE), add = TRUE)
    repro_materialize_layout(plan, structure_df, sandbox_root)
    stat_output_write(stat_output, sandbox_root)
  }

  empty <- function(text, tl = "na", extra_report = NULL) {
    resp <- list(
      table = data.frame(),
      summary_table = data.frame(
        paper_id = .pid(structure_df, code_tbl),
        repro_code_n = 0L, repro_runnable = 0L,
        repro_missing_inputs = 0L, repro_deps = 0L
      ),
      na_replace = c(repro_code_n = 0, repro_runnable = 0,
                     repro_missing_inputs = 0, repro_deps = 0),
      traffic_light = tl,
      summary_text = text,
      # Even a no-R-code paper can have self-reproducible JASP/jamovi output —
      # carry the extracted results (and its report section) out.
      report = c(extra_report, self_repro_report),
      stat_output = stat_output
    )
    if (!is.null(sandbox_root)) attr(resp, "sandbox") <- sandbox_root
    resp
  }

  # This phase assesses R only. code_check discards non-R, non-"listed" files
  # (e.g. Python), so its table cannot tell "no code at all" from "code, but not
  # R". To phrase the message honestly, count the non-R code files directly from
  # data_check's structure table (every downloaded file, by name). We don't name
  # the language, just flag that non-R code exists but is out of scope here.
  non_r_code_pat <- "\\.(py|ipynb|jl|m|mat|sas|sps|spss|do|ado|java|cpp|c|sql|sav|jasp|omv|mplus|inp)$"
  n_non_r <- if (!is.null(structure_df) && "file_name" %in% names(structure_df))
    sum(grepl(non_r_code_pat, structure_df$file_name, ignore.case = TRUE)) else 0L
  non_r_note <- if (n_non_r > 0) sprintf(
    " The repository does contain %d non-R code file%s, which this phase does not assess (only R is supported).",
    n_non_r, plural(n_non_r)) else ""

  # SPSS-data-without-syntax makes even a no-R-code paper red (and carries the
  # warning); otherwise a no-code paper is na.
  empty_tl <- if (spss_red) "red" else "na"

  # Only R is handled in this phase.
  if (is.null(code_tbl) || nrow(code_tbl) == 0 ||
      !"language" %in% names(code_tbl))
    return(empty(paste0(
      "We found no R code files to assess for reproducibility.", non_r_note),
      tl = empty_tl, extra_report = spss_report))

  r_files <- code_tbl[!is.na(code_tbl$language) & code_tbl$language == "R", ,
                      drop = FALSE]
  # The renv bootstrap (renv/activate.R) is machinery, not the paper's analysis
  # code; it is not something a reproduction "runs", so drop it.
  r_files <- r_files[!grepl("(^|/)renv/activate\\.R$", r_files$file_name,
                            ignore.case = TRUE), , drop = FALSE]
  if (nrow(r_files) == 0)
    return(empty(paste0(
      "We found no R code files to assess for reproducibility (only R is supported in this phase).",
      non_r_note), tl = empty_tl, extra_report = spss_report))

  n_code <- nrow(r_files)

  # ── 2. Resolve each R file to its on-disk location and read its text ─────────
  # code_check drops file_location from its table, so resolve the path from
  # data_check's structure table (which keeps it), by file_name; fall back to
  # code_check's file_url when structure has no local copy. Basename keying is
  # imperfect for duplicated names across repos (a documented limitation), but is
  # the only shared key the two tables have.
  loc_lookup <- if (!is.null(structure_df) &&
                    all(c("file_name", "file_location") %in% names(structure_df)))
    stats::setNames(structure_df$file_location, structure_df$file_name) else
    character(0)
  resolve_path <- function(fn) {
    loc <- if (fn %in% names(loc_lookup)) loc_lookup[[fn]] else NA_character_
    if (!is.na(loc) && nzchar(loc) && file.exists(loc)) return(loc)
    # basename fallback (file_name in the two tables may differ by directory)
    if (length(loc_lookup)) {
      m <- which(basename(names(loc_lookup)) == basename(fn))
      for (j in m) {
        l <- loc_lookup[[j]]
        if (!is.na(l) && nzchar(l) && file.exists(l)) return(l)
      }
    }
    NA_character_
  }

  pb_read <- pb(n_code, ":what [:bar] :current/:total")
  pb_read$tick(0, list(what = ""))
  on.exit(pb_read$terminate())

  code_text_list <- lapply(seq_len(n_code), function(i) {
    the_file <- r_files[i, ]
    pb_read$tick(1, list(what = the_file$file_name))
    path <- resolve_path(the_file$file_name)
    if (is.na(path) && "file_url" %in% names(the_file) &&
        !is.na(the_file$file_url) && nzchar(the_file$file_url))
      path <- the_file$file_url
    if (is.na(path)) return(character(0))
    is_rmd <- grepl("\\.(rmd|qmd)$", the_file$file_name, ignore.case = TRUE)
    tryCatch(
      if (is_rmd) code_extract_r(path) else code_read(path),
      error = function(e) character(0))
  })
  names(code_text_list) <- r_files$file_name

  # ── 3. Dependencies (pooled across files) ───────────────────────────────────
  # repro_dependencies() adds the install source (cran/github/url/base) + ref
  # that code_check's `packages` column does not carry. But if a file's text
  # could not be read here while code_check still captured its packages, fall
  # back to that column (via the shared code_packages() helper) so no dependency
  # is silently lost — tagging the recovered names as cran (source unknown).
  deps <- repro_dependencies(code_text_list, "R")
  cc_pkgs <- tryCatch(code_packages(r_files$packages), error = function(e) character(0))
  missing_pkgs <- setdiff(cc_pkgs, deps$package)
  if (length(missing_pkgs) > 0) {
    base_pkgs <- .repro_base_packages()
    deps <- dplyr::bind_rows(deps, data.frame(
      package = missing_pkgs, source = ifelse(missing_pkgs %in% base_pkgs, "base", "cran"),
      ref = NA_character_, base = missing_pkgs %in% base_pkgs))
  }
  install_deps <- deps[!deps$base, , drop = FALSE]   # base pkgs need no install
  n_deps    <- nrow(install_deps)
  n_github  <- sum(install_deps$source %in% c("github", "url"))

  # ── 4. Per-file path rewrite + I/O for ordering ─────────────────────────────
  io <- repro_file_io(code_text_list)

  rewrite_list <- lapply(seq_len(n_code), function(i)
    repro_rewrite_paths(code_text_list[[i]], r_files$file_name[i], plan, "R"))
  names(rewrite_list) <- r_files$file_name

  # Per-file rewrite counts + unresolved reads (referenced, not matched in plan).
  rewrites_n  <- vapply(rewrite_list, function(d)
    if (nrow(d)) sum(d$matched & !d$ambiguous) else 0L, integer(1))
  ambiguous_n <- vapply(rewrite_list, function(d)
    if (nrow(d)) sum(d$ambiguous) else 0L, integer(1))
  unresolved_refs <- unlist(lapply(rewrite_list, function(d)
    if (nrow(d)) d$basename[!d$matched] else character(0)), use.names = FALSE)

  # ── 5. Run order ────────────────────────────────────────────────────────────
  order_tbl <- repro_run_order(io)
  cycle     <- attr(order_tbl, "cycle") %||% character(0)
  ambiguous_order <- isTRUE(attr(order_tbl, "ambiguous"))

  # ── 6. Missing-input diagnosis ──────────────────────────────────────────────
  # A read is a "missing input" when no earlier script writes it AND it is not a
  # file available in the repository. Cross-reference the download records.
  produced <- unique(tolower(unlist(io$writes %||% list(), use.names = FALSE)))
  candidate_missing <- setdiff(unique(tolower(unresolved_refs)), produced)
  missing_inputs <- repro_missing_inputs(candidate_missing, plan, structure_df)
  n_missing_inputs <- nrow(missing_inputs)
  n_withheld <- sum(missing_inputs$status == "withheld_size")

  # Parse status carried over from code_check (a file that will not parse cannot
  # run). code_check stored parse_error per file.
  parse_errs <- if ("parse_error" %in% names(r_files))
    sum(r_files$parse_error, na.rm = TRUE) else 0L

  # A file is "runnable-so-far" when it parses, all its referenced inputs
  # resolve (matched or produced upstream), and it is placeable in the order.
  parses <- if ("parse_error" %in% names(r_files))
    !isTRUE_vec(r_files$parse_error) else rep(TRUE, n_code)
  file_order <- order_tbl$order[match(r_files$file_name, order_tbl$file_name)]
  placeable <- !is.na(file_order)

  # "Produced upstream" must respect the run order: a file counts as available
  # to script S only when some script that runs *before* S writes it. The flat
  # pool of all writes would also excuse a file that is only produced later,
  # which cannot help S actually run.
  writes_by_file <- io$writes %||% list()
  names(writes_by_file) <- io$file_name %||% names(writes_by_file)
  produced_before <- lapply(seq_len(n_code), function(i) {
    ord_i <- file_order[i]
    if (is.na(ord_i)) return(character(0))
    earlier <- r_files$file_name[!is.na(file_order) & file_order < ord_i]
    unique(tolower(unlist(writes_by_file[earlier], use.names = FALSE)))
  })

  # Per-file unresolved inputs, kept as names so the table can say *why* a file
  # is not runnable rather than only that it is not.
  unresolved_list <- lapply(seq_len(n_code), function(i) {
    d <- rewrite_list[[i]]
    if (!nrow(d)) return(character(0))
    unique(tolower(d$basename)[!d$matched &
                                 !(tolower(d$basename) %in% produced_before[[i]])])
  })
  file_unresolved <- lengths(unresolved_list) > 0L
  runnable <- parses & placeable & !file_unresolved
  n_runnable <- sum(runnable)

  # Why each file is not runnable, in the order the conditions are applied.
  not_runnable_reason <- ifelse(
    runnable, NA_character_,
    ifelse(!parses, "parse_error",
           ifelse(!placeable, "unplaceable", "missing_input")))
  unresolved_inputs <- vapply(unresolved_list, function(x)
    if (length(x)) paste(x, collapse = ", ") else NA_character_, character(1))

  # ── 7. Traffic light (static part) ──────────────────────────────────────────
  # Static red is the "code cannot run only because size-gated inputs were
  # withheld" story: some inputs missing, and all of them size-gated. When the
  # code is actually run (execute = TRUE), an execution ERROR overrides this to
  # red below — a script that crashes is a reproduction failure.
  tl <- if (n_missing_inputs > 0 && n_withheld == n_missing_inputs) "red"
        else if (parse_errs == 0 && n_missing_inputs == 0 &&
                 length(cycle) == 0 && !ambiguous_order &&
                 sum(ambiguous_n) == 0) "green"
        else "yellow"

  # SPSS data with no syntax means part of the analysis is unreproducible: red,
  # even alongside runnable R code (an incidental .sav still fails to reproduce).
  if (isTRUE(spss_red)) tl <- "red"

  # ── 7b. Execution (opt-in; runs downloaded code) ────────────────────────────
  # Everything above is static. When execute = TRUE we materialise the Psych-DS
  # layout into a throwaway temp dir, write the path-rewritten scripts into it,
  # optionally install the declared deps into a throwaway library, and run each
  # script in run order in an isolated subprocess, capturing its outcome. Unlike
  # the static signals, an execution error DOES recolour the light: any errored /
  # timed-out script forces red (see after the block).
  run_results <- NULL
  install_results <- NULL
  if (isTRUE(execute)) {
    # ── DEBUG tracing (TEMPORARY — remove before release). Prints every step of
    # the execute phase so a hang is attributable to the exact operation. ───────
    .dbg <- function(...) message("[repro] ", ...)
    .dbg("execute = TRUE. install_missing = ", install_missing,
         ", timeout = ", timeout, "s, keep_sandbox = ", keep_sandbox)

    # Reuse the root the JASP/jamovi step already materialised (if any stat
    # output existed above), so data/ and statistical_output/ end up in the
    # SAME tree the executed scripts also run against, rather than two roots.
    if (is.null(sandbox_root)) {
      sandbox_root <- tempfile("repro_sandbox_")
      if (!isTRUE(keep_sandbox))
        on.exit(unlink(sandbox_root, recursive = TRUE), add = TRUE)
    }
    lib_dir <- file.path(sandbox_root, "_lib")
    .dbg("sandbox root: ", sandbox_root)

    # 1. Build the data tree the plan describes, and write the scripts into it.
    .dbg("materialising Psych-DS layout from plan (", nrow(plan %||% data.frame()),
         " plan rows) ...")
    ml <- repro_materialize_layout(plan, structure_df, sandbox_root)
    .dbg("  materialised ", sum(attr(ml, "materialised")$ok %||% FALSE), "/",
         nrow(attr(ml, "materialised") %||% data.frame()), " files.")
    .dbg("writing ", n_code, " rewritten script(s) into the layout ...")
    run_tbl <- repro_write_scripts(code_text_list, rewrite_list, plan, sandbox_root)
    .dbg("  wrote ", nrow(run_tbl), " script(s).")

    # 2. Optionally install dependencies (CRAN sources into your main library
    #    when cran_install_main = TRUE, else — like GitHub/URL always — into
    #    the throwaway library).
    if (isTRUE(install_missing) && n_deps > 0) {
      .dbg("installing ", n_deps, " dependenc(y/ies) (cran_install_main = ",
           cran_install_main, "): ", paste(install_deps$package, collapse = ", "))
      .dbg("  repos = ", paste(getOption("repos"), collapse = "; "))
      install_results <- repro_install_deps(install_deps, lib_dir,
                                            cran_to_main_lib = cran_install_main)
      .dbg("  install done: ", sum(install_results$installed), " ok, ",
           sum(!install_results$installed), " failed.")
    } else {
      .dbg("install_missing FALSE or no deps; skipping installs.")
    }

    # 3. Run in order. A file whose inputs are known-missing is not run (it
    #    cannot succeed); a file that will not parse is not run either.
    parses_named <- stats::setNames(parses, r_files$file_name)
    skip_files <- r_files$file_name[file_unresolved]
    if (length(skip_files))
      .dbg("skipping (missing inputs): ", paste(skip_files, collapse = ", "))
    run_order_names <- order_tbl$file_name[order(order_tbl$order)]
    .dbg("running ", length(run_order_names), " script(s) in order: ",
         paste(run_order_names, collapse = " -> "))
    run_results <- repro_run_scripts(
      run_tbl, run_order_names,
      lib_dir = if (dir.exists(lib_dir)) lib_dir else NULL,
      timeout = timeout, skip = skip_files, parses = parses_named)
    .dbg("execution finished (pass 1). outcomes: ",
         paste(sprintf("%s=%s", run_results$file_name, run_results$outcome),
               collapse = "; "))

    # 4. Corrective re-run (ONE extra pass, no more). An "object 'X' not found"
    #    error means the script expected a variable another script defines but
    #    that ran later (or not at all). Find the file that defines each missing
    #    variable, add "definer precedes user" edges, recompute the order, and
    #    re-run once. A second undefined-var error after this pass is accepted as
    #    the final outcome (we do not iterate to convergence).
    undef_err <- run_results[!is.na(run_results$error_type) &
                             run_results$error_type == "undefined_variable", ,
                             drop = FALSE]
    reran <- FALSE
    if (nrow(undef_err) > 0) {
      defs <- repro_defined_vars(code_text_list)
      def_of <- function(v) {
        # file_name(s) that define variable v at top level
        hit <- vapply(defs$defines, function(dd) v %in% dd, logical(1))
        defs$file_name[hit]
      }
      extra_edges <- list()
      for (i in seq_len(nrow(undef_err))) {
        user <- undef_err$file_name[i]; v <- undef_err$undefined_var[i]
        definers <- setdiff(def_of(v), user)
        # Only act when exactly one file defines it (unambiguous); 0 = genuinely
        # undefined (a real bug, leave it), >1 = ambiguous (do not guess).
        if (length(definers) == 1) {
          extra_edges <- c(extra_edges, list(c(definers, user)))
          .dbg("undefined-var edge: '", definers, "' defines '", v,
               "' needed by '", user, "'")
        }
      }
      if (length(extra_edges) > 0) {
        order_tbl2 <- repro_run_order(io, extra_edges = extra_edges)
        run_order2 <- order_tbl2$file_name[order(order_tbl2$order)]
        .dbg("re-running ", length(run_order2), " script(s) in corrected order: ",
             paste(run_order2, collapse = " -> "))
        run_results <- repro_run_scripts(
          run_tbl, run_order2,
          lib_dir = if (dir.exists(lib_dir)) lib_dir else NULL,
          timeout = timeout, skip = skip_files, parses = parses_named)
        order_tbl <- order_tbl2   # report the corrected order
        reran <- TRUE
        .dbg("execution finished (pass 2). outcomes: ",
             paste(sprintf("%s=%s", run_results$file_name, run_results$outcome),
                   collapse = "; "))
      }
    }
    attr(run_results, "reran_for_order") <- reran

    # An execution error (errored / timed_out) is a reproduction failure: force
    # the traffic light red, whatever the static signals said.
    if (any(run_results$outcome %in% c("errored", "timed_out"))) tl <- "red"

    # Parse each script's captured CONSOLE OUTPUT into statistical results, the
    # same way JASP/jamovi files are handled — so RUN R code contributes to the
    # statistical_output too (t.test/lm/aov/... printed to stdout). Reuses the
    # STATO typing + ISA-JSON emitter. script_lines (the executed script's own
    # text, from repro_run_scripts()'s echo = TRUE run) lets read_r_output()
    # attach the source LINE each result came from. Appended to stat_output
    # alongside any JASP/jamovi results.
    r_stat_output <- lapply(seq_len(nrow(run_results)), function(i) {
      so <- run_results$stdout[i]
      if (is.null(so) || !nzchar(so)) return(NULL)
      fn <- run_results$file_name[i]
      exec_lines <- run_results$script_lines[[i]]
      tabs <- tryCatch(
        read_r_output(so, source_label = fn,
                      code_lines = if (length(exec_lines)) exec_lines else NULL),
        error = function(e) list())
      if (!length(tabs)) return(NULL)
      list(file = fn, n_tables = length(tabs), source = "r_output",
           isa  = stat_output_isa(tabs, paper_id = .pid(structure_df, code_tbl),
                                  source_file = fn),
           long = stat_results_long(tabs, paper_id = .pid(structure_df, code_tbl),
                                    source_file = fn))
    })
    r_stat_output <- Filter(Negate(is.null), r_stat_output)
    if (length(r_stat_output) > 0) {
      stat_output <- c(stat_output %||% list(), r_stat_output)
      .dbg("extracted statistical output from ", length(r_stat_output),
           " script(s)' console output.")
    }

    # Re-write statistical_output/ now that executed-script results (if any)
    # have joined stat_output — the earlier write (above, before this block)
    # only had JASP/jamovi results available. repro_materialize_layout()'s
    # data/ copy is idempotent (file.copy(overwrite = TRUE)), so re-running it
    # via step 1 above already refreshed data/; this just refreshes the
    # statistical_output/ files to match the now-complete stat_output.
    if (length(stat_output) > 0) {
      if (is.null(sandbox_root)) sandbox_root <- tempfile("repro_sandbox_")
      stat_output_write(stat_output, sandbox_root)
    }
  }

  # ── 8. Per-file table ───────────────────────────────────────────────────────
  ord_of <- order_tbl$order[match(r_files$file_name, order_tbl$file_name)]
  basis_of <- order_tbl$order_basis[match(r_files$file_name, order_tbl$file_name)]
  dep_of   <- order_tbl$depends_on[match(r_files$file_name, order_tbl$file_name)]

  # Execution outcome + error detail per file (NA in the static-only path),
  # aligned by file_name. setwd_removed comes from the write step (run_tbl).
  match_run <- if (!is.null(run_results))
    match(r_files$file_name, run_results$file_name) else rep(NA_integer_, n_code)
  outcome_of   <- if (!is.null(run_results)) run_results$outcome[match_run]      else rep(NA_character_, n_code)
  errtype_of   <- if (!is.null(run_results)) run_results$error_type[match_run]   else rep(NA_character_, n_code)
  undefvar_of  <- if (!is.null(run_results)) run_results$undefined_var[match_run] else rep(NA_character_, n_code)
  setwd_of <- if (exists("run_tbl", inherits = FALSE) && "setwd_removed" %in% names(run_tbl))
    run_tbl$setwd_removed[match(r_files$file_name, run_tbl$file_name)] else
    rep(NA_integer_, n_code)

  # Per-file data reads/writes (the code<->data provenance link), aligned by
  # file_name from repro_file_io()'s `io`. Computed above (line 329) for run-
  # ordering only and previously dropped before reaching `table` — surfaced
  # here as list-columns of basenames so a caller (e.g. the RO-Crate
  # provenance builder in R/psychds-convert.R) can build a real object/
  # instrument/result CreateAction per code file instead of recomputing this.
  match_io <- match(r_files$file_name, io$file_name)
  reads_of  <- if ("reads"  %in% names(io)) io$reads[match_io]  else rep(list(character(0)), n_code)
  writes_of <- if ("writes" %in% names(io)) io$writes[match_io] else rep(list(character(0)), n_code)
  reads_of[is.na(match_io)]  <- list(character(0))
  writes_of[is.na(match_io)] <- list(character(0))

  table <- data.frame(
    paper_id       = if ("paper_id" %in% names(r_files)) r_files$paper_id else .pid(),
    file_name      = r_files$file_name,
    parses         = parses,
    run_order      = ord_of,
    order_basis    = basis_of,
    depends_on     = dep_of,
    paths_rewritten = rewrites_n,
    paths_ambiguous = ambiguous_n,
    setwd_removed  = setwd_of,
    runnable       = runnable,
    not_runnable_reason = not_runnable_reason,
    unresolved_inputs   = unresolved_inputs,
    outcome        = outcome_of,
    error_type     = errtype_of,
    undefined_var  = undefvar_of
  )
  table$reads  <- reads_of
  table$writes <- writes_of

  # ── 9. Report ───────────────────────────────────────────────────────────────
  intro <- if (isTRUE(execute))
    paste0("This module assesses whether the paper's code could be run on its ",
           "data, and — because `execute = TRUE` — **actually ran it**. We ",
           "examined %d R code file%s.") else
    paste0("This module assesses whether the paper's code could be run on its ",
           "data. It performs static analysis only here: **no code is run**. We ",
           "examined %d R code file%s.")
  report <- c(sprintf(intro, n_code, plural(n_code)))

  ## Dependencies ----
  if (n_deps == 0) {
    report_deps <- "No installable package dependencies were detected in the code (only base R packages, or none)."
  } else {
    gh_note <- if (n_github > 0) sprintf(
      " %d of these %s a non-CRAN (GitHub/URL) source and would be fetched from the source the code names.",
      n_github, plural(n_github, "has", "have")) else ""
    report_deps <- sprintf(
      paste0("The code declares %d installable package dependenc%s (names only; ",
             "static analysis cannot recover the versions used).%s A future run ",
             "would install these into a throwaway library, against current ",
             "versions — a break under current versions is itself a finding, ",
             "arguing for pinned versions."),
      n_deps, if (n_deps == 1) "y" else "ies", gh_note)
  }
  dep_table <- if (n_deps > 0) {
    d <- install_deps[, c("package", "source", "ref")]
    d$ref <- ifelse(is.na(d$ref), "", d$ref)
    colnames(d) <- c("Package", "Source", "GitHub/URL ref")
    d
  } else NULL

  ## Run order ----
  ordered <- order_tbl[!is.na(order_tbl$order), , drop = FALSE]
  ordered <- ordered[order(ordered$order), , drop = FALSE]
  order_table <- data.frame(
    Order = ordered$order,
    `File` = ordered$file_name,
    `Runs after` = ifelse(nzchar(ordered$depends_on), ordered$depends_on, "—"),
    Basis = ordered$order_basis,
    check.names = FALSE
  )
  report_order <- if (ambiguous_order) {
    "No ordering signal (no data dependency, `source()` call, or numeric filename prefix) distinguishes the run order of these files. They may need to run in a specific order that could not be determined automatically — check this before running."
  } else if (length(cycle) > 0) {
    sprintf("A dependency **cycle** was detected among %d file%s (%s): each reads a file another writes, so no run order satisfies all of them. This must be resolved before the code can run.",
            length(cycle), plural(length(cycle)), paste(cycle, collapse = ", "))
  } else {
    "The scripts were ordered by their data dependencies (a script writing a file another reads runs first), `source()` calls, and numeric filename prefixes."
  }

  ## Path rewriting ----
  total_rewrites <- sum(rewrites_n)
  total_ambiguous <- sum(ambiguous_n)
  report_paths <- sprintf(
    "To run against the Psych-DS layout, %d referenced data path%s would be rewritten to %s new location%s.",
    total_rewrites, plural(total_rewrites),
    plural(total_rewrites, "its", "their"), plural(total_rewrites))
  if (total_ambiguous > 0)
    report_paths <- paste(report_paths, sprintf(
      "%d reference%s matched several files sharing a name and could not be resolved by study group; %s left unrewritten and flagged.",
      total_ambiguous, plural(total_ambiguous),
      plural(total_ambiguous, "it was", "they were")))

  ## Missing inputs ----
  if (n_missing_inputs == 0) {
    report_missing <- "Every data file the code reads is either produced by an earlier script or present in the repository."
    missing_table <- NULL
  } else {
    report_missing <- sprintf(
      "%d input file%s the code reads %s not available. Files withheld only because of their size are distinguished from files absent from the repository — the former are a size-cap issue, not a reproducibility failure.",
      n_missing_inputs, plural(n_missing_inputs),
      plural(n_missing_inputs, "is", "are"))
    missing_table <- data.frame(
      File   = missing_inputs$basename,
      Status = missing_inputs$status,
      Reason = missing_inputs$detail
    )
  }

  report <- c(
    report,
    "#### Package dependencies", report_deps,
    if (!is.null(dep_table)) scroll_table(dep_table, maxrows = 10),
    "#### Run order", report_order, scroll_table(order_table, maxrows = 10),
    "#### File paths", report_paths,
    "#### Missing inputs", report_missing,
    if (!is.null(missing_table)) scroll_table(missing_table, maxrows = 10)
  )
  if (parse_errs > 0)
    report <- c(report, "#### Parsing", sprintf(
      "%d R file%s did not parse and cannot be run (see code_check for the errors).",
      parse_errs, plural(parse_errs)))

  ## Execution ----
  if (!is.null(run_results) && nrow(run_results) > 0) {
    oc <- run_results$outcome
    n_ran_ok  <- sum(oc == "ran_ok")
    n_errored <- sum(oc == "errored")
    n_timeout <- sum(oc == "timed_out")
    n_skipped <- sum(oc == "skipped_missing_inputs")
    n_noparse <- sum(oc == "not_parsed")
    report_exec <- sprintf(
      paste0("**The code was run.** Each script ran in an isolated subprocess ",
             "against a throwaway copy of the Psych-DS layout, in the run order ",
             "above, with a %d-second per-script timeout. Of %d script%s: %d ran ",
             "without error, %d errored, %d timed out, %d %s skipped (inputs ",
             "unavailable), and %d did not parse. Running against current package ",
             "versions: a break can reflect version drift, which argues for ",
             "pinning versions."),
      timeout, nrow(run_results), plural(nrow(run_results)),
      n_ran_ok, n_errored, n_timeout, n_skipped,
      plural(n_skipped, "was", "were"), n_noparse)

    # Note the corrective re-run when it happened, so the outcomes are read as
    # "after re-ordering", not the first attempt.
    if (isTRUE(attr(run_results, "reran_for_order")))
      report_exec <- paste(report_exec,
        "\n\n*One script hit an `object '...' not found` error, indicating it",
        "expected a variable another script defines. We inferred which script",
        "supplies it, re-ordered so that script runs first, and re-ran once. The",
        "outcomes above are from that corrected order.*")

    # setwd() warning. A script that only runs after we strip a setwd() is
    # "reproducible after ignoring a setwd() that should not be in the code".
    setwd_rows <- if (exists("run_tbl", inherits = FALSE) &&
                      "setwd_removed" %in% names(run_tbl))
      run_tbl[run_tbl$setwd_removed > 0, , drop = FALSE] else run_tbl[0, ]
    report_setwd <- if (nrow(setwd_rows) > 0) {
      # Which of these still ran ok — i.e. reproduced *because* we stripped setwd.
      ok_after <- intersect(
        setwd_rows$file_name, run_results$file_name[run_results$outcome == "ran_ok"])
      c(sprintf(
        paste0("**%d script%s call%s `setwd()`.** This is bad practice: it ",
               "hardcodes where the code must run (often an absolute path on the ",
               "author's own machine) and breaks the moment the code is run ",
               "anywhere else. To run the code we commented these calls out. Any ",
               "such script that then ran is **reproducible only after ignoring a ",
               "`setwd()` that should not have been in the code and needs to be ",
               "fixed**."),
        nrow(setwd_rows), plural(nrow(setwd_rows)),
        plural(nrow(setwd_rows), "s", "")),
        if (length(ok_after) > 0) sprintf(
          "Reproducible after removing `setwd()`: %s.",
          paste(ok_after, collapse = ", ")),
        scroll_table(data.frame(
          File = setwd_rows$file_name,
          `setwd() removed` = setwd_rows$setwd_removed,
          `Path(s) it set` = ifelse(nzchar(setwd_rows$setwd_paths),
                                    setwd_rows$setwd_paths, "(non-literal)"),
          check.names = FALSE), maxrows = 10))
    } else NULL

    # Undefined-variable errors, called out explicitly: an "object 'X' not found"
    # usually means the script is a snippet expecting a variable another script
    # defines, rather than a standalone program.
    undef <- run_results[!is.na(run_results$error_type) &
                         run_results$error_type == "undefined_variable", ,
                         drop = FALSE]
    report_undef <- if (nrow(undef) > 0) c(sprintf(
      paste0("**%d script%s failed on an undefined variable** (`object '...' not ",
             "found`). This typically means the script expects a variable that ",
             "another script defines — i.e. it is meant to be sourced into a ",
             "larger session, not run on its own — or the variable is simply ",
             "never created. Each is counted as an error (red)."),
      nrow(undef), plural(nrow(undef))),
      scroll_table(data.frame(
        File = undef$file_name,
        `Missing variable` = undef$undefined_var,
        check.names = FALSE), maxrows = 10)) else NULL

    exec_table <- data.frame(
      File    = run_results$file_name,
      Outcome = run_results$outcome,
      Detail  = ifelse(!is.na(run_results$error_type),
                       run_results$error_type, ""),
      `Time (s)` = round(run_results$elapsed, 1),
      check.names = FALSE)

    # For EVERY script (not just failures) offer a collapsed dropdown holding the
    # output the run generated — stdout (the analysis results the script printed)
    # followed by stderr (warnings / the error that stopped it). This is the
    # reproduction transcript: what the code actually produced when run. Each
    # stream is tail-capped so a very chatty script cannot bloat the HTML; the
    # FULL, untruncated output is always kept in the returned result object (see
    # the note below). The `.truncated` flag signals when a cap actually fired.
    max_lines <- 5000L
    .truncated <- FALSE
    tail_cap <- function(txt) {
      if (is.null(txt) || !nzchar(txt)) return(character(0))
      lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
      if (length(lines) > max_lines) {
        .truncated <<- TRUE
        lines <- c(sprintf("... [%d earlier lines omitted — see full output in the result object] ...",
                           length(lines) - max_lines),
                   utils::tail(lines, max_lines))
      }
      lines
    }
    output_blocks <- unlist(lapply(seq_len(nrow(run_results)), function(i) {
      so <- tail_cap(run_results$stdout[i])
      se <- tail_cap(run_results$stderr[i])
      # A file that never ran (skipped / not parsed) has no transcript to show.
      if (length(so) == 0 && length(se) == 0 &&
          !nzchar(run_results$error[i] %||% "")) return(NULL)
      # Four-backtick fences (Pandoc idiom) so a stray ``` line in the captured
      # output cannot prematurely close the block and corrupt the report.
      body <- character(0)
      if (length(so) > 0)
        body <- c(body, "**Output (stdout):**",
                  "````", so, "````")
      if (length(se) > 0)
        body <- c(body, "**Messages / errors (stderr):**",
                  "````", se, "````")
      if (length(body) == 0 && nzchar(run_results$error[i] %||% ""))
        body <- c("````", run_results$error[i], "````")
      collapse_section(
        paste(body, collapse = "\n"),
        title = sprintf("Output — %s (%s)",
                        run_results$file_name[i], run_results$outcome[i]))
    }), use.names = FALSE)

    # Note pointing to the full, untruncated output — shown only when a cap fired.
    truncation_note <- if (isTRUE(.truncated)) sprintf(
      paste0("*Output shown here is capped at the last %d lines per stream to ",
             "keep the report readable. The complete, untruncated output of every ",
             "script is stored in the module result: ",
             "`reproducibility_check_output$run_results$stdout` (and `$stderr`), ",
             "one row per script.*"),
      max_lines) else NULL

    report <- c(report, "#### Execution", report_exec,
                scroll_table(exec_table, maxrows = 15),
                if (!is.null(report_undef)) c("**Undefined-variable errors**", report_undef),
                if (!is.null(report_setwd)) c("**`setwd()` in the code**", report_setwd),
                if (length(output_blocks) > 0)
                  "*Expand a row below to see the output each script produced.*",
                output_blocks, truncation_note)

    if (!is.null(install_results) && nrow(install_results) > 0) {
      n_ok   <- sum(install_results$installed)
      n_fail <- sum(!install_results$installed)
      report_inst <- sprintf(
        "Dependencies were installed into a throwaway library before running: %d succeeded, %d failed. A failed install is a distinct outcome from a code error.",
        n_ok, n_fail)
      inst_table <- data.frame(
        Package   = install_results$package,
        Source    = install_results$source,
        Installed = install_results$installed,
        Note      = ifelse(nzchar(install_results$message), install_results$message, ""),
        check.names = FALSE)
      report <- c(report, report_inst, scroll_table(inst_table, maxrows = 15))
    }
  } else {
    report <- c(report,
      "*Executing the code is a separate, opt-in phase (run with `execute = TRUE`). This report describes what a run would involve; it did not run anything.*")
  }

  # ── 10. Summary ─────────────────────────────────────────────────────────────
  n_ran_ok <- if (!is.null(run_results)) sum(run_results$outcome == "ran_ok") else NA_integer_
  summary_text <- c(
    if (isTRUE(execute)) sprintf(
      "We assessed AND ran %d R code file%s for reproducibility (each in an isolated subprocess).",
      n_code, plural(n_code)) else sprintf(
      "We assessed %d R code file%s for reproducibility (static analysis; no code was run).",
      n_code, plural(n_code)),
    sprintf("%d file%s appear%s runnable so far (parses, inputs resolve, placeable in the run order).",
            n_runnable, plural(n_runnable), if (n_runnable == 1) "s" else ""),
    if (isTRUE(execute) && !is.null(run_results)) sprintf(
      "%d of %d script%s ran without error when executed.",
      n_ran_ok, nrow(run_results), plural(nrow(run_results))),
    if (n_missing_inputs > 0) sprintf(
      "%d referenced input%s unavailable (%d withheld due to size).",
      n_missing_inputs, plural(n_missing_inputs), n_withheld),
    sprintf("%d installable dependenc%s detected.", n_deps,
            if (n_deps == 1) "y" else "ies")
  ) |> paste("\n- ", x = _, collapse = "")

  summary_table <- data.frame(
    paper_id             = .pid(structure_df, code_tbl),
    repro_code_n         = n_code,
    repro_runnable       = n_runnable,
    repro_missing_inputs = n_missing_inputs,
    repro_deps           = n_deps,
    repro_ran_ok         = n_ran_ok
  )

  # SPSS data ± syntax note (also flagged the light red above when no .sps).
  if (!is.null(spss_report)) report <- c(report, spss_report)
  # Self-reproducible JASP/jamovi output (extracted result tables, STATO-typed).
  if (!is.null(self_repro_report)) report <- c(report, self_repro_report)

  out <- list(
    table = table,
    summary_table = summary_table,
    na_replace = c(repro_code_n = 0, repro_runnable = 0,
                   repro_missing_inputs = 0, repro_deps = 0,
                   repro_ran_ok = 0),
    traffic_light = tl,
    report = report,
    summary_text = summary_text,
    run_results = run_results,
    install_results = install_results,
    # Extracted JASP/jamovi statistical output: per-file ISA-JSON + flat rows.
    stat_output = stat_output
  )
  # When asked to keep the sandbox, surface its path so the caller can inspect
  # exactly what ran (data/, statistical_output/, and — when execute = TRUE —
  # the rewritten scripts + temp library). Not gated on execute: a JASP/jamovi
  # -only paper (execute = FALSE) also materialises a root, for
  # statistical_output/ alone.
  if (isTRUE(keep_sandbox) && !is.null(sandbox_root))
    attr(out, "sandbox") <- sandbox_root
  out
}
