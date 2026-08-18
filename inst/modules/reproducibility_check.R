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
#' is one of `ran_ok`, `errored`, `timed_out`, `skipped_missing_inputs`,
#' `dependency_unavailable`, or `not_parsed`; a timeout means "still running at
#' the cutoff", not a failure, so raise `timeout` for legitimately long-running
#' (e.g. Bayesian) scripts. `dependency_unavailable` means the script's own
#' package could not be installed from live CRAN, the CRAN Archive, or a named
#' GitHub/URL source — an infrastructure limitation, not a defect in the
#' paper's code, so unlike `errored`/`timed_out` it does **not** force the
#' traffic light red. The full stdout/stderr of every run is kept in the
#' result's `run_results` (the report shows it in per-script dropdowns, capped
#' for readability). The traffic light reflects the STATIC readiness signals
#' by default, but an execution error (`errored`/`timed_out`) does recolour it
#' to red — a script that crashes on its own analysis is a genuine
#' reproduction failure.
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
#' @param execute if TRUE, actually RUN the paper's code (against a throwaway
#'   copy of the Psych-DS layout). This runs downloaded code on your machine
#'   and is off by default; it is a deliberate, opt-in action. See `sandbox`
#'   for how it is isolated.
#' @param sandbox how `execute = TRUE` runs each script. `"process"`
#'   (default) runs it in an isolated `callr` subprocess on this machine —
#'   this isolates a CRASH, but NOT the filesystem or network: the code can
#'   still read/write/delete anywhere this R session can, and reach the
#'   network freely. `"docker"` runs it inside a locked-down Docker container
#'   instead (network disabled, filesystem read-only outside the sandbox, a
#'   non-root user) — a real containment boundary, appropriate for running
#'   code you do not trust. Requires Docker to be installed and running (see
#'   [repro_docker_available()]). By default (see `docker_use_declared_version`)
#'   the base image is `ghcr.io/scienceverse/metacheck_r:latest`, a
#'   pre-built image with the ~750 most common corpus packages already
#'   installed, so most papers skip most of the install phase entirely
#'   rather than reinstalling their dependencies from scratch on every run.
#'   `"process"` needs `callr`; `"docker"` needs `processx`.
#' @param docker_use_declared_version if TRUE (and `sandbox = "docker"`), use
#'   the R version `code_check`'s version-pinning detection found declared in
#'   the repository (an `renv.lock`/`sessionInfo()` record) instead of the
#'   pre-built `metacheck_r` image — `rocker/r-ver:<declared version>`, a
#'   bare image with no packages pre-installed. Slower (every dependency is
#'   installed from scratch, from source, in that run — see
#'   [repro_install_deps_docker()]) but matches the R version the paper's
#'   authors actually used. Default FALSE: always use the fast pre-built
#'   image regardless of what a paper declared. When FALSE and a paper DID
#'   declare a version, a warning is issued once per call naming the
#'   mismatch and how to opt into the slower, version-matched path — so this
#'   is never a silent substitution.
#' @param install_missing if TRUE (and `execute = TRUE`), install the code's
#'   declared dependencies into a throwaway temp library before running (CRAN via
#'   `install.packages`, GitHub/URL via `remotes`). Default FALSE: a script
#'   needing an absent package simply errors, recorded as its outcome.
#' @param cran_install_main if TRUE (and `install_missing` and `execute` are
#'   both TRUE, and `sandbox = "process"`), CRAN-source dependencies are
#'   installed into your DEFAULT R library instead of the throwaway one, and so
#'   persist after the run — see [repro_install_deps()]. Useful when calling
#'   `reproducibility_check()` over many papers in one script: a CRAN package
#'   installed for an early paper is already present (and skipped) for every
#'   later paper needing it, instead of being reinstalled into a fresh
#'   throwaway library each time. GitHub/URL sources are unaffected — always
#'   installed into the throwaway library. Default FALSE (everything throwaway,
#'   nothing persists). Ignored when `sandbox = "docker"`: a container has no
#'   access to the host's main library, so every install always goes into the
#'   throwaway library regardless of this argument.
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
#' @param cache if `TRUE`, forwarded to the internal `data_check`/`code_check`
#'   runs: files they download are kept in the persistent on-disk cache (see
#'   [repo_cache_dir()]) and reused on later runs instead of being
#'   re-downloaded. If `FALSE` (the default), downloads go to a temporary
#'   directory discarded when the R session ends — nothing accumulates on
#'   disk. Only takes effect when `data_check`/`code_check` have not already
#'   been run for this paper (see the chained-output note above); if you ran
#'   them yourself first, set `cache` there instead.
#'
#' @returns a list
reproducibility_check <- function(paper, local_path = NULL, local_only = FALSE,
                                  model = llm_model(), params = list(),
                                  execute = FALSE, sandbox = c("process", "docker"),
                                  docker_use_declared_version = FALSE,
                                  install_missing = FALSE,
                                  cran_install_main = FALSE,
                                  timeout = 600, keep_sandbox = FALSE,
                                  cache = FALSE) {
  # paper <- psychsci[[233]] # to test (many code files, several issues)
  sandbox <- match.arg(sandbox)

  # Executing downloaded code runs it on this machine: gate it behind an explicit
  # opt-in. sandbox = "process" (default) isolates a CRASH via callr, not the
  # filesystem/network -- code can still write/delete/reach the network freely.
  # sandbox = "docker" is the actual containment boundary (network off, read-only
  # filesystem, non-root user during the run phase -- see
  # R/reproducibility_check_docker.R's header comment); use it for untrusted
  # downloaded code.
  if (isTRUE(execute) && sandbox == "process" &&
      !requireNamespace("callr", quietly = TRUE))
    stop("execute = TRUE with sandbox = \"process\" needs the 'callr' package ",
         "(it runs each script in an isolated subprocess). Install callr, ",
         "or use sandbox = \"docker\".", call. = FALSE)
  if (isTRUE(execute) && sandbox == "docker") {
    docker_ok <- repro_docker_available()
    if (!docker_ok$ok)
      stop("execute = TRUE with sandbox = \"docker\": ", docker_ok$msg, call. = FALSE)
  }

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
  # NOTE: inst/modules/psychds_check.R is DUPLICATED into this branch and into
  # the convert_psychds branch — this module needs psychds_check's plan
  # (below) but nothing from convert_psychds/psychds-convert.R itself. Once
  # both modules are mature, consider extracting psychds_check's plan-building
  # logic into a shared internal helper instead of maintaining two copies of
  # the file.
  code_tbl     <- get_prev_outputs("code_check", "table")
  plan         <- get_prev_outputs("psychds_check", "table")
  structure_df <- get_prev_outputs("data_check", "structure")
  # code_check's declared-version detection (renv.lock/sessionInfo/groundhog/
  # checkpoint — see .code_version_pin_check()), reused by the Docker backend
  # below to pick the base image's R version. NULL until code_tbl's source is
  # resolved (either already cached, or freshly run just below).
  version_pin  <- get_prev_outputs("code_check", "version_pin")

  run_missing <- function(mod) {
    # model / params only go to the modules that accept them (data_check,
    # psychds_check use the LLM for study grouping); code_check has no such
    # arguments, so passing them would error with "unused arguments". Same
    # reasoning for cache: only data_check/code_check download files.
    args <- list(paper, mod, local_only = local_only)
    if (!is.null(local_path)) args$local_path <- local_path
    if (mod %in% c("data_check", "psychds_check")) {
      args$model <- model; args$params <- params
    }
    if (mod %in% c("data_check", "code_check")) {
      args$cache <- cache
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
    version_pin <- cc$version_pin
  }

  # ── SPSS / Stata data without syntax ─────────────────────────────────────────
  # An SPSS data file (.sav/.zsav/.por) with NO SPSS syntax (.sps) means the data
  # was deposited but not the code that produced the results — so that analysis
  # cannot be reproduced from the deposit. This forces the light red (part of the
  # analysis is unreproducible), and we recommend jamovi (.omv) / JASP (.jasp),
  # which bundle data + analyses together and so are self-reproducible. Detected
  # from the file inventory (data_check's structure) so it works even when the
  # paper has no R code at all.
  #
  # has_sps/has_do check BOTH structure_df (an author-saved .sps/.do actually
  # in the deposit) AND code_tbl (a SYNTHETIC .sps/.do that code_check's
  # .code_expand_spv()/.code_expand_smcl() recovered from a .spv/.smcl output
  # file and materialised into a `code` subfolder — see R/code_check.R). A
  # .spv-only deposit (data + syntax + results, no separate .sps at all) would
  # otherwise be wrongly flagged red here despite being fully self-reproducible
  # in its own right, since its syntax lives only in code_tbl, never in
  # structure_df.
  fnames_all <- if (!is.null(structure_df) && "file_name" %in% names(structure_df))
    structure_df$file_name else character(0)
  code_fnames <- if (!is.null(code_tbl) && "file_name" %in% names(code_tbl))
    code_tbl$file_name else character(0)
  has_sav <- any(grepl("\\.(sav|zsav|por)$", fnames_all, ignore.case = TRUE))
  has_sps <- any(grepl("\\.sps$", c(fnames_all, code_fnames), ignore.case = TRUE))
  has_selfcontained <- any(grepl("\\.(omv|jasp|spv)$", fnames_all, ignore.case = TRUE))
  spss_red <- has_sav && !has_sps
  spss_report <- if (has_sav) {
    rec <- if (!has_selfcontained)
      paste0(" Consider depositing the analysis in **jamovi** (`.omv`) or ",
             "**JASP** (`.jasp`) instead: these formats store the data and the ",
             "analyses together in one file, so the results are reproducible from ",
             "the file itself.") else
      paste0(" (A jamovi/JASP/SPSS-Viewer file is also present, which is ",
             "self-reproducible.)")
    if (!has_sps) c("#### SPSS data without syntax", paste0(
      "An SPSS data file (`.sav`/`.por`) is present but **no SPSS syntax file ",
      "(`.sps`) was found**, so the analysis that produced the results cannot be ",
      "reproduced from the deposit. This is treated as a reproducibility failure ",
      "(red).", rec))
    else c("#### SPSS data", paste0(
      "SPSS data and syntax (`.sav` + `.sps`) are present. This module does not ",
      "execute SPSS syntax itself — the syntax is at least documented, but not ",
      "run here. If an SPSS-Viewer file (`.spv`) is also deposited, its results ",
      "are extracted directly (no execution needed); consider adding one if not.",
      rec))
  } else NULL

  # Same logic, for Stata: a .dta with no .do means the analysis code was not
  # deposited, UNLESS a .smcl output log recovered it as a synthetic .do (see
  # .code_expand_smcl()). Unlike SPSS, Stata has no self-contained-bundle
  # counterpart to .jasp/.omv/.spv (a .smcl carries no dataset at all), so
  # there is no "consider depositing as X instead" recommendation to make —
  # only "deposit the .do file (or the .smcl log it can be recovered from)".
  has_dta <- any(grepl("\\.dta$", fnames_all, ignore.case = TRUE))
  has_do  <- any(grepl("\\.(do|ado)$", c(fnames_all, code_fnames), ignore.case = TRUE))
  stata_red <- has_dta && !has_do
  stata_report <- if (has_dta) {
    if (!has_do) c("#### Stata data without syntax", paste0(
      "A Stata data file (`.dta`) is present but **no Stata syntax file (`.do`) ",
      "or output log (`.smcl`) was found**, so the analysis that produced the ",
      "results cannot be reproduced from the deposit. This is treated as a ",
      "reproducibility failure (red). Consider depositing the `.do` file (or, at ",
      "minimum, a `.smcl`/`.log` output file, which at least records the exact ",
      "commands that were run)."))
    else c("#### Stata data", paste0(
      "Stata data and syntax (`.dta` + `.do`, or a `.smcl` output log with the ",
      "syntax recovered from it) are present. This module does not execute Stata ",
      "syntax itself — the syntax is at least documented, but not run here. If a ",
      "`.smcl`/`.log` output file is also deposited, its results are extracted ",
      "directly (no Stata needed); consider adding one if not."))
  } else NULL

  # ── JASP / jamovi / SPSS Viewer / notebook self-reproducible output ─────────
  # A .jasp/.omv/.spv file bundles the data AND its analyses+results together
  # (an .spv's own structure XML stores each table's dimension/cell structure
  # directly in the archive, exactly like .jasp/.omv -- see R/spv.R), so it is
  # reproducible from the file itself (no code to run). We EXTRACT the
  # rendered result tables (read_stat_tables, which dispatches on each
  # archive's OWN content -- analyses.json for .jasp, protobuf blobs for
  # .omv, structure XML for .spv -- not on file extension), type each
  # statistic with the STATO ontology, and serialise them: a flat queryable
  # form (stat_results_long, for the scienceverse DB) and a full structured
  # document (stat_output_json, for the logs). This runs regardless of
  # whether the paper also has R code.
  #
  # A .ipynb belongs here for the same reason: a Jupyter notebook saves the
  # outputs each code cell produced INTO the file, so the statistics a Python
  # analysis reported are recoverable from the notebook alone, with no Python
  # installed and nothing run. It is the only member of this set that is not a
  # zip archive (it is JSON), which read_stat_tables() handles by dispatching
  # .ipynb on extension -- see .ipynb_read_tables() (R/stat-tables.R). Note the
  # asymmetry with a plain .py, which has NO output counterpart at all (Python
  # has no equivalent of .spv/.smcl/.out -- whatever a script printed went to a
  # terminal nobody saved), so a .py is checked only as code, never here.
  stat_output <- NULL          # per-file: list(file, n_tables, json, long)
  self_repro_report <- NULL
  jasp_omv <- if (!is.null(structure_df) &&
                  all(c("file_name", "file_location") %in% names(structure_df))) {
    hit <- grepl("\\.(jasp|omv|spv|ipynb)$", structure_df$file_name, ignore.case = TRUE)
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
           json = stat_output_json(tabs, paper_id = .pid_here, source_file = basename(fn)),
           long = stat_results_long(tabs, paper_id = .pid_here,
                                    source_file = basename(fn)))
    })
    stat_output <- Filter(Negate(is.null), stat_output)
    if (length(stat_output) > 0) {
      n_files  <- length(stat_output)
      n_tables <- sum(vapply(stat_output, `[[`, integer(1), "n_tables"))
      n_stats  <- sum(vapply(stat_output, function(s) nrow(s$long), integer(1)))
      self_repro_report <- c("#### Self-reproducible output (JASP / jamovi / SPSS Viewer / Jupyter notebook)",
        sprintf(paste0(
          "%d JASP/jamovi/SPSS-Viewer/notebook file%s bundle%s the analyses and ",
          "their saved results together, so the reported statistics are ",
          "recoverable from the file%s itself, with nothing re-run. We extracted ",
          "%d result table%s (%d individual statistic%s), typed with the STATO ",
          "ontology, and export them as a statistical-output JSON document (in ",
          "the logs) and as queryable rows."),
          n_files, plural(n_files), plural(n_files, "s", ""),
          plural(n_files), n_tables, plural(n_tables),
          n_stats, plural(n_stats)))
    }
  }

  # ── Stata output logs (.smcl) — recovered syntax, extracted statistics ──────
  # A .smcl file is Stata's rendered OUTPUT log, not a self-contained bundle
  # like .jasp/.omv/.spv -- it has no dataset inside it, only the commands
  # that were run (echoed verbatim, so ALSO recovered as a synthetic .do file
  # by code_check()'s .code_expand_smcl(), which is what makes has_do below
  # TRUE) and whatever they printed. We still extract its result tables the
  # same way (STATO-typed, serialised to the same two forms), since a .smcl
  # log genuinely does carry the paper's reported statistics even though
  # reproducing them would require running the recovered .do file in Stata
  # itself (out of scope -- this module runs only R; see the SPSS-without-
  # syntax note above for the same limitation with .sps).
  smcl_rows <- if (!is.null(structure_df) &&
                   all(c("file_name", "file_location") %in% names(structure_df))) {
    hit <- grepl("\\.smcl$", structure_df$file_name, ignore.case = TRUE)
    data.frame(file_name = structure_df$file_name[hit],
               file_location = structure_df$file_location[hit],
               stringsAsFactors = FALSE)
  } else data.frame(file_name = character(0), file_location = character(0))

  if (nrow(smcl_rows) > 0) {
    .pid_here2 <- .pid(structure_df, code_tbl)
    smcl_output <- lapply(seq_len(nrow(smcl_rows)), function(i) {
      loc <- smcl_rows$file_location[i]; fn <- smcl_rows$file_name[i]
      if (is.na(loc) || !nzchar(loc) || !file.exists(loc)) return(NULL)
      tabs <- tryCatch(import_stata_smcl(loc), error = function(e) list())
      if (!length(tabs)) return(NULL)
      list(file = fn, n_tables = length(tabs),
           json = stat_output_json(tabs, paper_id = .pid_here2, source_file = basename(fn)),
           long = stat_results_long(tabs, paper_id = .pid_here2,
                                    source_file = basename(fn)))
    })
    smcl_output <- Filter(Negate(is.null), smcl_output)
    if (length(smcl_output) > 0) {
      stat_output <- c(stat_output %||% list(), smcl_output)
      n_files  <- length(smcl_output)
      n_tables <- sum(vapply(smcl_output, `[[`, integer(1), "n_tables"))
      n_stats  <- sum(vapply(smcl_output, function(s) nrow(s$long), integer(1)))
      self_repro_report <- c(self_repro_report,
        "#### Stata output logs (.smcl)",
        sprintf(paste0(
          "%d Stata output log%s (`.smcl`) %s present, recording every command that ",
          "was run and what it printed. We extracted %d result table%s (%d ",
          "individual statistic%s), typed with the STATO ontology. The commands' ",
          "own syntax was also recovered as a `.do` file for `code_check`, but ",
          "actually re-running it would require Stata itself, which this module ",
          "does not do (it runs only R)."),
          n_files, plural(n_files), plural(n_files, "is", "are"),
          n_tables, plural(n_tables), n_stats, plural(n_stats)))
    }
  }

  # ── Mplus output (.out) — self-contained syntax + extracted statistics ─────
  # A .out file is Mplus's own rendered output, and (unlike .jasp/.omv/.spv,
  # which bundle data+analyses, and unlike .smcl, which needs a synthetic
  # .do recovered alongside it) it is ALWAYS self-documenting: its own
  # "INPUT INSTRUCTIONS" section holds the exact verbatim syntax that
  # produced it, with no separate data-file-without-syntax failure mode the
  # way .sav/.dta have — Mplus reads whatever plain-text/external data file
  # the syntax points to, but the syntax itself is never missing from a
  # genuine .out. So there is no "Mplus data without syntax" red-flag
  # section to add here, unlike the SPSS/Stata blocks above. We still
  # extract its result tables (STATO-typed, serialised to the same two
  # forms) the same way as .jasp/.omv/.spv/.smcl.
  mplus_rows <- if (!is.null(structure_df) &&
                    all(c("file_name", "file_location") %in% names(structure_df))) {
    hit <- grepl("\\.out$", structure_df$file_name, ignore.case = TRUE)
    data.frame(file_name = structure_df$file_name[hit],
               file_location = structure_df$file_location[hit],
               stringsAsFactors = FALSE)
  } else data.frame(file_name = character(0), file_location = character(0))

  if (nrow(mplus_rows) > 0) {
    .pid_here3 <- .pid(structure_df, code_tbl)
    mplus_output <- lapply(seq_len(nrow(mplus_rows)), function(i) {
      loc <- mplus_rows$file_location[i]; fn <- mplus_rows$file_name[i]
      if (is.na(loc) || !nzchar(loc) || !file.exists(loc)) return(NULL)
      if (!.mplus_is_genuine_output(loc)) return(NULL)
      tabs <- tryCatch(import_mplus_output(loc), error = function(e) list())
      if (!length(tabs)) return(NULL)
      list(file = fn, n_tables = length(tabs),
           json = stat_output_json(tabs, paper_id = .pid_here3, source_file = basename(fn)),
           long = stat_results_long(tabs, paper_id = .pid_here3,
                                    source_file = basename(fn)))
    })
    mplus_output <- Filter(Negate(is.null), mplus_output)
    if (length(mplus_output) > 0) {
      stat_output <- c(stat_output %||% list(), mplus_output)
      n_files  <- length(mplus_output)
      n_tables <- sum(vapply(mplus_output, `[[`, integer(1), "n_tables"))
      n_stats  <- sum(vapply(mplus_output, function(s) nrow(s$long), integer(1)))
      self_repro_report <- c(self_repro_report,
        "#### Mplus output (.out)",
        sprintf(paste0(
          "%d Mplus output file%s (`.out`) %s present. A `.out` file always ",
          "carries its own verbatim analysis syntax (under \"INPUT ",
          "INSTRUCTIONS\"), so it is self-documenting. We extracted %d result ",
          "table%s (%d individual statistic%s), typed with the STATO ontology, ",
          "and export them as a statistical-output JSON document (in the logs) ",
          "and as queryable rows."),
          n_files, plural(n_files), plural(n_files, "is", "are"),
          n_tables, plural(n_tables), n_stats, plural(n_stats)))
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

  # ── Non-R code with no way to check its results ─────────────────────────────
  # spss_report/stata_report above cover "data present, syntax withheld"
  # (has_sav/has_dta with no .sps/.do) and "data + syntax present, neither run"
  # (has_sav+has_sps / has_dta+has_do). Neither covers the MIRROR gap: syntax
  # DEPOSITED with no data and no output of any kind, so not even the numbers
  # the syntax originally produced are recoverable from this deposit — as real
  # as "data without syntax", just the opposite half missing. Confirmed as a
  # live gap: a corpus paper depositing 7 Stata .do files with no .dta and no
  # .smcl fell through EVERY existing check (spss_red/stata_red both require
  # data present; has_self_repro requires JASP/jamovi/.spv/.smcl/.out) straight
  # to the generic "no R code" na-branch below, so it never told the reader who
  # the missing piece even was — this section exists to name it explicitly,
  # per language, with what a self-reproducible-output deposit would look like
  # for THAT language specifically.
  #
  # code_tbl$language is code_check's own code_lang() classification (already
  # computed there — reused rather than re-detecting file extensions here) so
  # "R"/"JASP"/"jamovi" rows are excluded: R is the module's own code-execution
  # path (handled entirely separately below), and JASP/jamovi files are the
  # SELF-REPRODUCIBLE OUTPUT itself (already covered by self_repro_report),
  # never "code with no output" — a .omv/.jasp IS both at once.
  nonr_lang <- if (!is.null(code_tbl) && "language" %in% names(code_tbl))
    code_tbl$language[!is.na(code_tbl$language) &
                      !code_tbl$language %in% c("R", "JASP", "jamovi")] else character(0)
  nonr_report <- NULL
  if (length(nonr_lang) > 0) {
    lang_counts <- table(nonr_lang)
    has_self_repro_now <- length(stat_output) > 0
    for (lang in names(lang_counts)) {
      n <- as.integer(lang_counts[[lang]])
      # Per-language: does EITHER that language's own recognised output
      # format exist, OR (for SAS/MATLAB, which have no such format in
      # metacheck at all) any self-reproducible output exists from ANY
      # source? The Stata/SPSS cases are also already fully handled by
      # spss_report/stata_report whenever data (.sav/.dta) is present too —
      # this block only needs to fire for the DATA-ABSENT half of those two,
      # since the data-present half already has its own message above.
      skip <- FALSE
      block <- NULL
      if (identical(lang, "Stata")) {
        if (has_dta) skip <- TRUE   # already covered by stata_report above
        else {
          has_smcl_out <- any(grepl("\\.smcl$", fnames_all, ignore.case = TRUE))
          # NOT ".smcl or .log": a plain .log is Stata's OTHER, unmarked-up
          # log format, and import_stata_smcl() (R/stata.R) specifically
          # parses .smcl's own markup codes ({com}/{txt}/...) -- a .log has
          # none of those, so it is not something this reads. Only .smcl is
          # ever recommended.
          if (!has_smcl_out) block <- c("#### Stata code without data or output", sprintf(
            paste0(
              "%d Stata syntax file%s (`.do`) %s present, but no output log ",
              "(`.smcl`) was found. Without output, we cannot compare the ",
              "results from the software against those reported in the ",
              "manuscript to check whether all results can be reproduced ",
              "based on the shared data, code, and results. Consider ",
              "depositing a `.smcl` output file."),
            n, plural(n), plural(n, "is", "are")))
        }
      } else if (identical(lang, "SPSS")) {
        if (has_sav) skip <- TRUE   # already covered by spss_report above
        else {
          has_spv_out <- any(grepl("\\.spv$", fnames_all, ignore.case = TRUE))
          if (!has_spv_out) block <- c("#### SPSS syntax without data or output", sprintf(
            paste0(
              "%d SPSS syntax file%s (`.sps`) %s present, but no output ",
              "(`.spv`) was found. Without output, we cannot compare the ",
              "results from the software against those reported in the ",
              "manuscript to check whether all results can be reproduced ",
              "based on the shared data, code, and results. Consider ",
              "depositing an `.spv` file instead of (or alongside) the ",
              "`.sav`/`.sps` pair — it bundles the data and analyses together ",
              "and is fully self-reproducible from the file itself."),
            n, plural(n), plural(n, "is", "are")))
        }
      } else if (identical(lang, "SAS")) {
        if (!has_self_repro_now) block <- c("#### SAS code, no reproducible output", sprintf(
          paste0(
            "%d SAS code file%s %s present, but this module has no way to ",
            "extract statistics from SAS output, and no self-reproducible ",
            "output (JASP/jamovi, SPSS-Viewer, a Stata output log, or Mplus ",
            "output) was found to fall back on. Consider re-running the ",
            "analysis in **jamovi** or **JASP** (free; the `.omv`/`.jasp` ",
            "file bundles data and results together) or depositing SAS's ",
            "own output log if one exists."),
          n, plural(n), plural(n, "is", "are")))
      } else if (identical(lang, "MATLAB")) {
        if (!has_self_repro_now) block <- c("#### MATLAB code, no reproducible output", sprintf(
          paste0(
            "%d MATLAB code file%s %s present, but this module has no way to ",
            "extract statistics from MATLAB output, and no self-reproducible ",
            "output (JASP/jamovi, SPSS-Viewer, a Stata output log, or Mplus ",
            "output) was found to fall back on. Readers without a MATLAB ",
            "license cannot currently see what the code produced. Consider ",
            "depositing the results as well, so they are visible without ",
            "MATLAB installed: run `diary('output.txt')` (or `diary on`) ",
            "before the analysis and `diary off` after to save a plain-text ",
            "transcript of everything printed to the Command Window, or use ",
            "`publish('script.m', 'pdf')` (or the Live Editor's Export/Save ",
            "As) to generate a single readable document with the code, its ",
            "output, and any figures together."),
          n, plural(n), plural(n, "is", "are")))
      } else if (!has_self_repro_now) {
        # Generic fallback: any OTHER code_lang() result (Python, Julia, Java,
        # C/C++, SQL — see code_check.R's non_r_code_pat) with no output at
        # all. Deliberately generic rather than naming a specific missing
        # output format, since none of these languages has one metacheck
        # recognises.
        block <- c(sprintf("#### %s code, no reproducible output", lang), sprintf(
          paste0(
            "%d %s code file%s %s present, but this module has no way to ",
            "extract statistics from %s output, and no self-reproducible ",
            "output (JASP/jamovi, SPSS-Viewer, a Stata output log, or Mplus ",
            "output) was found to fall back on. Consider depositing the ",
            "analysis output (a rendered results/log file), or re-running ",
            "the analysis in **jamovi** or **JASP** (free; the `.omv`/",
            "`.jasp` file bundles data and results together)."),
          n, lang, plural(n), plural(n, "is", "are"), lang))
      }
      if (!skip && !is.null(block)) nonr_report <- c(nonr_report, block)
    }
  }

  # Run the SAME reported-vs-reproduced matching the R-code path runs further
  # down (see "## Reported vs. reproduced" below), here too — a paper with NO
  # R code but real self-reproducible output (JASP/jamovi/.spv/.smcl/Mplus
  # .out) previously returned from empty() BEFORE that matching code ever ran,
  # so the paper's REPORTED numbers were never actually checked against the
  # data extracted from those files: the module extracted 1500+ statistics but
  # never answered the question it exists to answer for them. Confirmed as a
  # real, live gap — a corpus paper's 6 .omv files went through full
  # extraction (self_repro_report/stat_output above) but the matching step,
  # the report's "Reported vs. reproduced" section, and every summary sentence
  # about it were all skipped, because the R-only early return happened first.
  .empty_match <- function() {
    out <- list(report = NULL, summary = NULL)
    if (length(stat_output) == 0) return(out)
    # include_tables = TRUE: also check statistics reported only in a results
    # table's cells (extract_eq_table(), via paper$table$contents) against the
    # extracted output — see match_reported_output()'s own comment on why this
    # is opt-in rather than the default for every caller.
    matched <- tryCatch(match_reported_output(paper, stat_output,
                                              include_tables = TRUE),
                        error = function(e) NULL)
    if (is.null(matched) || nrow(matched) == 0) return(out)
    ms <- attr(matched, "summary")
    match_report <- sprintf(
      paste0("%d statistical test%s reported in the manuscript text %s ",
             "checked against the extracted analysis output: %d matched ",
             "(%d fully, %d partially) — %s%% found."),
      ms$n_tests, plural(ms$n_tests), plural(ms$n_tests, "was", "were"),
      ms$n_found, ms$n_full, ms$n_partial, ms$pct_found)
    # A single-value match ("Plausible" = FALSE) is genuinely weaker evidence
    # than a multi-component one: match_reported_output() explains why in its
    # own docs (a lone value rounding to the reported number is far likelier
    # by chance than a whole signature doing so), and .regroup_by_evidence()
    # adds a second, sharper case of the same risk — a component split away
    # from the OTHER values it was originally reported alongside, with no
    # shared row_label linking its new site back to theirs. Explained ONCE
    # here, generally, rather than per-row, so a reader knows what the
    # Plausible column means before they hit a FALSE.
    n_flagged <- sum(!is.na(matched$plausible_split))
    if (n_flagged > 0) match_report <- c(match_report, paste(
      "Some reported values were only found alongside OTHER components",
      "after being separated from their own originally-reported neighbours",
      "(e.g. a mean split from its own confidence interval). The",
      "\"Plausible\" column marks whether that split is well-supported: TRUE",
      "when the pieces still trace back to the same underlying variable (via",
      "the output's own row labels), FALSE when no such link was found — a",
      "FALSE match is not necessarily wrong, but is worth checking against",
      "its own Source file/Analysis columns before treating it as confirmed."))
    match_table <- data.frame(
      Reported   = matched$reported,
      Found      = matched$found,
      Confidence = matched$confidence,
      Plausible  = ifelse(is.na(matched$plausible_split), "",
                          ifelse(matched$plausible_split, "yes", "no")),
      `Matched values` = ifelse(is.na(matched$match_values), "", matched$match_values),
      `Not matched` = ifelse(is.na(matched$not_matched), "", matched$not_matched),
      `Source file` = ifelse(is.na(matched$source_file), "", matched$source_file),
      Analysis   = ifelse(is.na(matched$analysis), "", matched$analysis),
      check.names = FALSE)
    list(report = c("#### Reported vs. reproduced", match_report,
                    scroll_table(match_table, maxrows = 15)),
         summary = ms, table_raw = matched)
  }

  empty <- function(text, tl = "na", extra_report = NULL) {
    em <- .empty_match()
    n_output_stats <- sum(vapply(stat_output, function(s) nrow(s$long), integer(1)))
    has_self_repro_here <- length(stat_output) > 0

    # The HEADLINE summary must not read as a null result when reproduction
    # actually succeeded via self-reproducible output — "no R code" is true
    # but, on its own, implies nothing was assessed, which is false whenever
    # has_self_repro_here. Lead with what was actually found; the "no R code"
    # fact becomes a secondary note (still present, since R-code absence is
    # real information — e.g. it means the analysis is not independently
    # re-runnable even though its recorded results are).
    #
    # The SAME reasoning applies to nonr_report: when it has content, the
    # module found something SPECIFIC and actionable (e.g. "7 Stata syntax
    # files, no output log") — leading with the generic "we found no R code
    # files... this phase only runs R code" is actively misleading there, not
    # just uninformative: it reads as "nothing to report" immediately above a
    # section that names exactly what's missing and what would fix it.
    # Confirmed as a real, live problem — a corpus paper with 7 orphaned
    # Stata .do files (no .dta, no .smcl) produced a summary bullet reading
    # "We found no R code files... (only R is supported)... see the sections
    # above" while the ACTUAL finding — Stata code without output — sat
    # below it in a section the summary bullet never named.
    headline <- if (has_self_repro_here) sprintf(
      "We assessed %d self-reproducible output file%s (JASP/jamovi/SPSS-Viewer/Mplus) instead of R code.",
      length(stat_output), plural(length(stat_output))) else if (!is.null(nonr_report))
      "We found no R code to run, but did find other code with no reproducible output to check it against — see below." else text
    # `text` (the generic "we found no R code files..." line) is dropped
    # entirely, not just demoted, when nonr_report exists: unlike the
    # has_self_repro_here case (where `text`'s "no R code" fact is still new
    # information alongside a DIFFERENT headline about extracted output),
    # here `text` says nothing the headline above and the detailed section
    # below don't already say better — keeping it produced two redundant,
    # near-identical bullets in a row (confirmed against a real corpus
    # paper), which is worse than the original single-bullet problem this
    # was meant to fix.
    secondary <- if (has_self_repro_here) text else NULL

    summary_text <- c(
      headline,
      if (has_self_repro_here) sprintf(
        "%d statistic%s stored from the extracted output.",
        n_output_stats, plural(n_output_stats)),
      if (!is.null(em$summary)) sprintf(
        "%d of %d reported test%s matched (%s%%).",
        em$summary$n_found, em$summary$n_tests, plural(em$summary$n_tests),
        em$summary$pct_found),
      secondary
    ) |> paste("\n- ", x = _, collapse = "")

    resp <- list(
      table = data.frame(),
      summary_table = data.frame(
        paper_id = .pid(structure_df, code_tbl),
        repro_code_n = 0L, repro_runnable = 0L,
        repro_missing_inputs = 0L, repro_deps = 0L,
        repro_tests_reported = if (!is.null(em$summary)) em$summary$n_tests else NA_integer_,
        repro_tests_matched  = if (!is.null(em$summary)) em$summary$n_found else NA_integer_
      ),
      na_replace = c(repro_code_n = 0, repro_runnable = 0,
                     repro_missing_inputs = 0, repro_deps = 0),
      traffic_light = tl,
      summary_text = summary_text,
      # Even a no-R-code paper can have self-reproducible JASP/jamovi output —
      # carry the extracted results, the reported-vs-reproduced match, and
      # both reports out.
      report = c(extra_report, self_repro_report, em$report),
      stat_output = stat_output,
      match_table = em$table_raw %||% NULL
    )
    # A NAMED LIST ELEMENT, not an attribute -- see the main return path's own
    # comment near the end of this function for why: module_run() copies only
    # named elements when repackaging a module's return value, silently
    # dropping any attribute set on the object itself. This empty() path
    # (JASP/jamovi-only, SPSS/Stata-without-syntax, or no-R-code papers) used
    # attr(resp, "sandbox") <- ... here until this was caught as the same bug
    # already fixed on the main path -- keep_sandbox = TRUE was silently
    # inert for every paper that returns via empty() specifically.
    if (isTRUE(keep_sandbox) && !is.null(sandbox_root)) resp$sandbox <- sandbox_root
    resp
  }

  # This phase assesses R only. code_check discards non-R, non-"listed" files
  # (e.g. Python), so its table cannot tell "no code at all" from "code, but not
  # R". To phrase the message honestly, count the non-R code files directly from
  # data_check's structure table (every downloaded file, by name). We don't name
  # the language, just flag that non-R code exists but is out of scope here.
  #
  # This list is CODE extensions only. It used to also include several DATA and
  # self-contained-OUTPUT extensions, which meant a paper whose only "non-R
  # files" were e.g. precomputed .mat data or a .jasp bundle was told it had
  # "non-R code files this phase does not assess" — misleading on two counts:
  # those files were never code, and (for .jasp/.omv/.spv/.smcl/.out) their
  # content IS already assessed, just via the separate self-reproducible-output
  # path above (self_repro_report/stat_output), not as "code". Removed here:
  # `sav` (SPSS data, not code — handled by the spss_red logic above, which
  # would otherwise also double-count these same files under the wrong label),
  # `jasp`/`omv`/`spv`/`smcl`/`out` (self-contained output, already extracted
  # above). `mat` (MATLAB's pure-data container) never belonged in a CODE list
  # at all — split out below as data, distinct from `m` (real MATLAB code).
  # `ipynb` is NOT in this list, for exactly the reason `jasp`/`omv`/`spv`
  # are not: a notebook's saved cell outputs ARE already assessed, via the
  # self-reproducible-output path above (self_repro_report/stat_output).
  # Counting it here as well would tell the reader it holds "non-R code this
  # phase does not assess" in the same report that just extracted its
  # statistics. A plain `py` stays, since a Python SCRIPT genuinely is
  # unassessed code with no output counterpart to recover.
  non_r_code_pat <- "\\.(py|jl|m|sas|sps|spss|do|ado|java|cpp|c|sql|inp)$"
  non_r_data_pat <- "\\.(mat)$"
  n_non_r <- if (!is.null(structure_df) && "file_name" %in% names(structure_df))
    sum(grepl(non_r_code_pat, structure_df$file_name, ignore.case = TRUE)) else 0L
  n_non_r_data <- if (!is.null(structure_df) && "file_name" %in% names(structure_df))
    sum(grepl(non_r_data_pat, structure_df$file_name, ignore.case = TRUE)) else 0L
  non_r_note <- if (n_non_r > 0) sprintf(
    paste0(
      " The repository does contain %d non-R code file%s; this phase only ",
      "RUNS R code (see the section%s above for what we found and could — or ",
      "could not — check for those files)."),
    n_non_r, plural(n_non_r), plural(length(nonr_lang %||% character(0)))) else ""
  non_r_note <- paste0(non_r_note, if (n_non_r_data > 0) sprintf(
    " It also contains %d MATLAB data file%s (`.mat`), which %s not code and %s not assessed here either.",
    n_non_r_data, plural(n_non_r_data), plural(n_non_r_data, "is", "are"),
    plural(n_non_r_data, "is", "are")) else "")

  # SPSS/Stata-data-without-syntax makes even a no-R-code paper red (and
  # carries the warning). Data WITH syntax is not a failure, but it is not
  # "nothing to report" either (spss_report/stata_report already document a
  # real, informative finding — data and syntax are both there, just not
  # executable in this R-only phase), so it gets yellow rather than na.
  # report_qmd()'s section filter drops the ENTIRE detailed report body for
  # na/fail modules ("remove fail and na from main report section"), which
  # would otherwise silently suppress these sections even though empty()
  # already passes them through as extra_report — na must be reserved for
  # genuinely empty modules, or a real finding never reaches the rendered
  # report despite being computed. A true no-code, no-SPSS, no-Stata paper
  # still has nothing to say and stays na.
  #
  # The SAME risk applies to self-reproducible JASP/jamovi/SPSS-Viewer output
  # (self_repro_report/stat_output, computed above): a paper whose only
  # analysis artifacts are e.g. .omv files has no R code to assess (correctly
  # "na" for THIS reason) but DOES have a real, substantive finding — 100+
  # extracted result tables, hundreds of typed statistics — that must not be
  # silently dropped by report.R's na/fail filter either. Confirmed as a real,
  # live bug: a corpus paper with 6 real .omv files and 1562 extracted
  # statistics rendered a "na"-flagged, entirely EMPTY reproducibility section
  # in its actual report.qmd output — the module's own `report` field had the
  # "Self-reproducible output" section computed correctly, but report.R's
  # section builder threw it away before render because traffic_light was "na".
  # nonr_report (built above) is the SAME "real finding must not be dropped
  # by na" risk as spss_report/stata_report/self_repro_report already guard
  # against, for the language-without-output cases they don't cover — so it
  # gets the SAME treatment: na is reserved for a module with truly nothing
  # to say, and any of these four report sources having content forces at
  # least "info" (nonr_report specifically is guidance, not a pass/fail
  # verdict — see this module's own call site above for why "info" rather
  # than "yellow" was chosen for it).
  has_self_repro <- length(stat_output) > 0
  empty_tl <- if (spss_red || stata_red) "red"
              else if (has_sav || has_dta || has_self_repro) "yellow"
              else if (!is.null(nonr_report)) "info"
              else "na"

  # Only R is handled in this phase.
  if (is.null(code_tbl) || nrow(code_tbl) == 0 ||
      !"language" %in% names(code_tbl))
    return(empty(paste0(
      "We found no R code files to assess for reproducibility.", non_r_note),
      tl = empty_tl, extra_report = c(spss_report, stata_report, nonr_report)))

  r_files <- code_tbl[!is.na(code_tbl$language) & code_tbl$language == "R", ,
                      drop = FALSE]
  # The renv bootstrap (renv/activate.R) is machinery, not the paper's analysis
  # code; it is not something a reproduction "runs", so drop it.
  r_files <- r_files[!grepl("(^|/)renv/activate\\.R$", r_files$file_name,
                            ignore.case = TRUE), , drop = FALSE]
  if (nrow(r_files) == 0)
    return(empty(paste0(
      "We found no R code files to assess for reproducibility (this phase runs R code only).",
      non_r_note), tl = empty_tl, extra_report = c(spss_report, stata_report, nonr_report)))

  n_code <- nrow(r_files)

  # ── 2. Resolve each R file to its on-disk location and read its text ─────────
  # code_check() keeps file_location in its own table (r_files IS code_tbl,
  # filtered to language == "R"), so a code file's local path is usually
  # already sitting on the row itself -- checked FIRST, by row index, since
  # r_files$file_name commonly repeats across mirrors and any name-keyed
  # lookup can only ever return one row's path for all of them (see the
  # dedup comment below). data_check's structure table is consulted next (by
  # file_name, with a basename fallback) only because a file can, in some
  # configurations, be present there and not in code_tbl (e.g. a code file
  # data_check happened to also fetch under download = "all"). file_url
  # (streaming, no local copy) is the last resort either way.
  loc_lookup <- if (!is.null(structure_df) &&
                    all(c("file_name", "file_location") %in% names(structure_df)))
    stats::setNames(structure_df$file_location, structure_df$file_name) else
    character(0)
  resolve_row_path <- function(i) {
    own <- r_files$file_location[i] %||% NA_character_
    if (!is.na(own) && nzchar(own) && file.exists(own)) return(own)

    fn <- r_files$file_name[i]
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

    url <- r_files$file_url[i] %||% NA_character_
    if (!is.na(url) && nzchar(url)) return(url)
    NA_character_
  }

  # ── 1b. Drop byte-identical duplicate files across repo mirrors ─────────────
  # A paper can link several OSF components that mirror each other (a "live"
  # component, an "Archive of OSF Storage" snapshot, a view-only anonymised
  # link) -- code_check() lists each mirror's copy of a script as its OWN row,
  # so the SAME analysis file is run once per mirror. Confirmed against a real
  # corpus paper linking 4 OSF components: CFA.Rmd and utils.R each had a
  # byte-identical copy in two of them, so every script in the run silently
  # ran 2x (some 4-6x, when a name also recurred within one mirror's own
  # subfolders) -- wasted execution time, and a report cluttered with
  # duplicate "ran/errored" rows for what is really one script.
  #
  # Hashed by CONTENT, not by basename or file_name: two files sharing a name
  # are only true duplicates when their bytes match -- a same-named script
  # that genuinely differs between mirrors (a later revision pushed to one but
  # not the other) must still run once per version, since those are different
  # analyses in every sense that matters here.
  #
  # resolve_row_path() resolves by ROW, not by file_name: r_files$file_name
  # commonly has DUPLICATE values (several mirrors sharing the same
  # basename), and each such row can carry its OWN location/URL even when the
  # name repeats -- looking the path up by name (e.g. via match()) would
  # always return the FIRST row's path for every duplicate-named row,
  # silently hashing the same content for all of them and making two
  # genuinely DIFFERENT same-named files (a real, if rare, possibility)
  # impossible to tell apart. code_read()/code_extract_r() already accept a
  # URL transparently (used the same way just below for the real read), so
  # hashing the CONTENT they return (rather than requiring a literal local
  # file for tools::md5sum()) works identically whether the source resolved
  # to a local path or a URL.
  #
  # This dedup step is why resolve_row_path() must check r_files$file_location
  # itself, not only structure_df: before code_check() kept its own
  # file_location (see code_check.R's table-return step), structure_df was
  # the ONLY source of a local path, but data_check() populates it only for
  # DATA files by default (download = "data") -- an R script's location lived
  # in code_check()'s own file_url instead. Confirmed as a real bug when that
  # was the only path available: resolve_path() returned NA for every code
  # file here (0/24 resolved against a real corpus paper), so md5sum() on
  # those NAs was never even reached, dedup silently did nothing, and every
  # mirror still ran -- 4 near-simultaneous copies of the SAME script sharing
  # one materialised sandbox then raced on that shared file, producing a
  # spurious "cannot open the connection" that had nothing to do with the
  # paper's own code.
  hash_source <- function(i) {
    path <- resolve_row_path(i)
    if (is.na(path)) return(NA_character_)
    txt <- tryCatch(code_read(path), error = function(e) NULL)
    if (is.null(txt) || !length(txt)) return(NA_character_)
    digest_txt <- paste(txt, collapse = "\n")
    unname(tools::md5sum(local({
      tf <- tempfile(); writeLines(digest_txt, tf, useBytes = TRUE); tf
    })))
  }
  hashes <- vapply(seq_len(nrow(r_files)), hash_source, character(1))
  dup <- !is.na(hashes) & duplicated(hashes)
  n_dup <- sum(dup)
  dup_report <- NULL
  if (n_dup > 0) {
    # For the report: which kept file each dropped duplicate mirrors, and
    # WHERE each copy came from -- file_name alone is useless here, since
    # mirrors share the same basename by construction (that is the whole
    # premise of this dedup step), so "X (same as X)" would say nothing.
    # resolve_row_path() (already used for hashing, above) gives the local
    # cache path or source URL, which actually differs between mirrors.
    dup_kept_idx <- vapply(which(dup), function(i) which(hashes == hashes[i])[1], integer(1))
    dup_loc <- vapply(which(dup), function(i) resolve_row_path(i) %||% NA_character_, character(1))
    kept_loc <- vapply(dup_kept_idx, function(i) resolve_row_path(i) %||% NA_character_, character(1))
    dup_of <- r_files$file_name[dup_kept_idx]
    dup_desc <- sprintf(
      "`%s`%s (same as `%s`%s)",
      r_files$file_name[dup],
      ifelse(!is.na(dup_loc) & nzchar(dup_loc), paste0(" at `", dup_loc, "`"), ""),
      dup_of,
      ifelse(!is.na(kept_loc) & nzchar(kept_loc), paste0(" at `", kept_loc, "`"), ""))
    dup_report <- c("#### Duplicate files across repo mirrors", sprintf(
      paste0(
        "%d file%s %s byte-identical to another file already in the run ",
        "(the paper links more than one repository/component that mirrors ",
        "the same materials). %s only run once, from its first occurrence, ",
        "to avoid wasted duplicate execution: %s."),
      n_dup, plural(n_dup), plural(n_dup, "is", "are"),
      plural(n_dup, "It was", "They were"),
      paste(dup_desc, collapse = "; ")))
    r_files <- r_files[!dup, , drop = FALSE]
    n_code <- nrow(r_files)
  }

  pb_read <- pb(n_code, ":what [:bar] :current/:total")
  pb_read$tick(0, list(what = ""))
  on.exit(pb_read$terminate())

  code_text_list <- lapply(seq_len(n_code), function(i) {
    the_file <- r_files[i, ]
    pb_read$tick(1, list(what = the_file$file_name))
    path <- resolve_row_path(i)
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
    repro_rewrite_paths(code_text_list[[i]], r_files$file_name[i], plan, "R",
                       structure_df = structure_df))
  names(rewrite_list) <- r_files$file_name

  # Per-file rewrite counts + unresolved reads (referenced, not matched in plan).
  rewrites_n  <- vapply(rewrite_list, function(d)
    if (nrow(d)) sum(d$matched & !d$ambiguous) else 0L, integer(1))
  ambiguous_n <- vapply(rewrite_list, function(d)
    if (nrow(d)) sum(d$ambiguous) else 0L, integer(1))
  unresolved_refs <- unlist(lapply(rewrite_list, function(d)
    if (nrow(d)) d$basename[!d$matched] else character(0)), use.names = FALSE)

  # Paths assembled at runtime (sprintf()/paste()-family — see
  # repro_rewrite_paths()'s `is_call` rows) are fragile authoring regardless of
  # whether we managed to resolve them, so both counts are reported: how many
  # such calls exist at all, and how many of those we could NOT resolve to a
  # plan file (left unrewritten, the script runs against the un-rewritten call
  # and will very likely fail on its own working-directory assumption).
  call_rows_all <- dplyr::bind_rows(lapply(rewrite_list, function(d)
    if (nrow(d) && "is_call" %in% names(d)) d[d$is_call, , drop = FALSE] else d[0, ]))
  n_call_paths     <- nrow(call_rows_all)
  n_call_unresolved <- if (n_call_paths) sum(!call_rows_all$matched) else 0L

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
    !(!is.na(r_files$parse_error) & r_files$parse_error) else rep(TRUE, n_code)
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

  # SPSS/Stata data with no syntax means part of the analysis is
  # unreproducible: red, even alongside runnable R code (an incidental
  # .sav/.dta still fails to reproduce).
  if (isTRUE(spss_red) || isTRUE(stata_red)) tl <- "red"

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
    .dbg("execute = TRUE (sandbox = ", sandbox, "). install_missing = ", install_missing,
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

    # Docker base image: by default the pre-built metacheck_r image (fast --
    # most dependencies already installed), regardless of what R version the
    # paper declared. docker_use_declared_version = TRUE opts into matching
    # the paper's own declared R version instead (rocker/r-ver:<version>, a
    # bare image with nothing pre-installed -- slower, since every
    # dependency installs from scratch). When docker_use_declared_version is
    # FALSE (the default) and the paper DID declare a version, warn once so
    # this is never a silent substitution of one R version for another.
    docker_image <- if (sandbox == "docker") {
      declared_v <- version_pin$r_versions %||% character(0)
      if (!isTRUE(docker_use_declared_version) && length(declared_v) > 0) {
        warning(
          "reproducibility_check(sandbox = \"docker\"): this paper declared R ",
          "version ", declared_v[[1]], ", but docker_use_declared_version = FALSE ",
          "(the default), so the run uses the pre-built metacheck_r image's own ",
          "R version instead, for speed. Set docker_use_declared_version = TRUE ",
          "to match the paper's declared version exactly -- slower, since every ",
          "dependency then installs from scratch rather than using the ",
          "pre-built image.", call. = FALSE)
      }
      .repro_docker_image_for(declared_v, use_declared_version = docker_use_declared_version)
    } else NULL
    if (sandbox == "docker") .dbg("docker image: ", docker_image)

    # 1. Build the data tree the plan describes, and write the scripts into it.
    .dbg("materialising Psych-DS layout from plan (", nrow(plan %||% data.frame()),
         " plan rows) ...")
    ml <- repro_materialize_layout(plan, structure_df, sandbox_root)
    .dbg("  materialised ", sum(attr(ml, "materialised")$ok %||% FALSE), "/",
         nrow(attr(ml, "materialised") %||% data.frame()), " files.")
    .dbg("writing ", n_code, " rewritten script(s) into the layout ...")
    run_tbl <- repro_write_scripts(code_text_list, rewrite_list, plan, sandbox_root)
    .dbg("  wrote ", nrow(run_tbl), " script(s).")

    # 2. Optionally install dependencies. sandbox = "process": CRAN sources
    #    into your main library when cran_install_main = TRUE, else (like
    #    GitHub/URL always) into the throwaway library. sandbox = "docker":
    #    always the throwaway library — a container has no access to (and
    #    must not be given access to) the host's real R library, so
    #    cran_install_main has no meaning there and is silently ignored.
    failed_deps <- character(0)
    if (isTRUE(install_missing) && n_deps > 0) {
      .dbg("installing ", n_deps, " dependenc(y/ies)",
           if (sandbox == "process") paste0(" (cran_install_main = ", cran_install_main, ")") else "",
           ": ", paste(install_deps$package, collapse = ", "))
      install_results <- if (sandbox == "docker") {
        repro_install_deps_docker(install_deps, lib_dir, image = docker_image,
                                  timeout = timeout)
      } else {
        .dbg("  repos = ", paste(getOption("repos"), collapse = "; "))
        repro_install_deps(install_deps, lib_dir, cran_to_main_lib = cran_install_main)
      }
      .dbg("  install done: ", sum(install_results$installed), " ok, ",
           sum(!install_results$installed), " failed.")
      # A dependency this call could not install (live CRAN AND the Archive
      # retry both failed, or a GitHub/URL source errored) — a script that
      # then fails on it is classified dependency_unavailable, not errored.
      failed_deps <- install_results$package[!install_results$installed]
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
    run_results <- if (sandbox == "docker") {
      repro_run_scripts_docker(
        run_tbl, run_order_names, sandbox_root = sandbox_root,
        lib_dir = if (dir.exists(lib_dir)) lib_dir else NULL,
        image = docker_image, timeout = timeout, skip = skip_files,
        parses = parses_named, failed_deps = failed_deps)
    } else {
      repro_run_scripts(
        run_tbl, run_order_names,
        lib_dir = if (dir.exists(lib_dir)) lib_dir else NULL,
        timeout = timeout, skip = skip_files, parses = parses_named,
        failed_deps = failed_deps)
    }
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
        run_results <- if (sandbox == "docker") {
          repro_run_scripts_docker(
            run_tbl, run_order2, sandbox_root = sandbox_root,
            lib_dir = if (dir.exists(lib_dir)) lib_dir else NULL,
            image = docker_image, timeout = timeout, skip = skip_files,
            parses = parses_named, failed_deps = failed_deps)
        } else {
          repro_run_scripts(
            run_tbl, run_order2,
            lib_dir = if (dir.exists(lib_dir)) lib_dir else NULL,
            timeout = timeout, skip = skip_files, parses = parses_named,
            failed_deps = failed_deps)
        }
        order_tbl <- order_tbl2   # report the corrected order
        reran <- TRUE
        .dbg("execution finished (pass 2). outcomes: ",
             paste(sprintf("%s=%s", run_results$file_name, run_results$outcome),
                   collapse = "; "))
      }
    }
    attr(run_results, "reran_for_order") <- reran

    # An execution error (errored / timed_out) is a reproduction failure: force
    # the traffic light red, whatever the static signals said. A
    # dependency_unavailable script is deliberately excluded: it failed because
    # ITS OWN package could not be installed from either live CRAN or the
    # Archive (or a GitHub/URL source), which is an infrastructure limitation,
    # not evidence the paper's code is broken.
    if (any(run_results$outcome %in% c("errored", "timed_out"))) tl <- "red"

    # Turn each script's run into statistical results, the same way JASP/jamovi
    # files are handled — so RUN R code contributes to statistical_output too.
    # TWO sources, merged by .r_merge_captures():
    #   * CAPTURED OBJECTS (R/r-capture.R) — the htest/summary.lm/anova objects
    #     themselves, recorded by a task callback in the run subprocess. Exact
    #     values, and the object knows which test produced a statistic (the same
    #     "W" is Shapiro-Wilk's after shapiro.test but the rank sum after
    #     wilcox.test), which the printed form cannot express;
    #   * CONSOLE OUTPUT — read_r_output() parsing stdout, which still catches
    #     results a script only ever printed (inside a loop, via print()) and so
    #     never returned at top level.
    # Captures win where both describe the same source line; the text path fills
    # the rest, so this is strictly additive.
    r_stat_output <- lapply(seq_len(nrow(run_results)), function(i) {
      so <- run_results$stdout[i]
      fn <- run_results$file_name[i]
      exec_lines <- run_results$script_lines[[i]]
      caps <- if ("captures" %in% names(run_results))
        run_results$captures[[i]] else NULL

      cap_tabs <- tryCatch(
        .r_captures_to_tables(caps, source_label = fn,
                              code_lines = if (length(exec_lines)) exec_lines else NULL),
        error = function(e) list())
      txt_tabs <- if (!is.null(so) && nzchar(so)) tryCatch(
        read_r_output(so, source_label = fn,
                      code_lines = if (length(exec_lines)) exec_lines else NULL),
        error = function(e) list()) else list()

      tabs <- .r_merge_captures(cap_tabs, txt_tabs)
      if (!length(tabs)) return(NULL)
      list(file = fn, n_tables = length(tabs), source = "r_output",
           n_captured = length(cap_tabs),
           json = stat_output_json(tabs, paper_id = .pid(structure_df, code_tbl),
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
  if (n_call_paths > 0)
    report_paths <- paste(report_paths, sprintf(
      paste0("**%d data path%s %s built at runtime** with `sprintf()`/`paste()`",
             "/`file.path()` instead of written as a literal path — this is ",
             "fragile authoring practice (it silently breaks if the code is ",
             "run from a different working directory than the author's own). ",
             "We resolved %s on a best-effort basis%s."),
      n_call_paths, plural(n_call_paths), plural(n_call_paths, "was", "were"),
      if (n_call_unresolved == 0) "all of these" else sprintf(
        "%d of %d", n_call_paths - n_call_unresolved, n_call_paths),
      if (n_call_unresolved > 0) sprintf(
        "; %d could not be resolved and %s left unrewritten (the script runs ",
        n_call_unresolved, plural(n_call_unresolved, "was", "were")) |>
        paste0("against the original, un-rewritten call and will likely fail") else ""))

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
    n_nodep   <- sum(oc == "dependency_unavailable")
    report_exec <- sprintf(
      paste0("**The code was run.** Each script ran in an isolated subprocess ",
             "against a throwaway copy of the Psych-DS layout, in the run order ",
             "above, with a %d-second per-script timeout. Of %d script%s: %d ran ",
             "without error, %d errored, %d timed out, %d %s skipped (inputs ",
             "unavailable), %d could not run because one of its own ",
             "dependencies is unavailable, and %d did not parse. Running ",
             "against current package versions: a break can reflect version ",
             "drift, which argues for pinning versions."),
      timeout, nrow(run_results), plural(nrow(run_results)),
      n_ran_ok, n_errored, n_timeout, n_skipped,
      plural(n_skipped, "was", "were"), n_nodep, n_noparse)

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

    # Dependency-unavailable errors, called out explicitly: a script that
    # library()/require()s a package neither live CRAN nor the CRAN Archive
    # could provide never got to run its own analysis. Reported separately from
    # "errored" and NOT counted toward the traffic light, since this is an
    # availability problem, not a bug in the paper's code.
    nodep <- run_results[!is.na(run_results$error_type) &
                         run_results$error_type == "dependency_unavailable", ,
                         drop = FALSE]
    report_nodep <- if (nrow(nodep) > 0) c(sprintf(
      paste0("**%d script%s could not run because %s own dependency is ",
             "unavailable** — the package failed to install from both live ",
             "CRAN and the CRAN Archive (or a GitHub/URL source could not be ",
             "resolved). This is an infrastructure limitation, not a defect in ",
             "the paper's code, so it does **not** force the traffic light red."),
      nrow(nodep), plural(nrow(nodep)), plural(nrow(nodep), "its", "their")),
      scroll_table(data.frame(
        File = nodep$file_name,
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
                if (!is.null(report_nodep)) c("**Dependency-unavailable errors**", report_nodep),
                if (!is.null(report_setwd)) c("**`setwd()` in the code**", report_setwd),
                if (length(output_blocks) > 0)
                  "*Expand a row below to see the output each script produced.*",
                output_blocks, truncation_note)

    if (!is.null(install_results) && nrow(install_results) > 0) {
      n_ok   <- sum(install_results$installed)
      n_fail <- sum(!install_results$installed)
      # A package installed via the CRAN Archive retry (live CRAN no longer
      # serves it) reproduced against a STALE version, not necessarily the one
      # the authors used — worth distinguishing from an ordinary install.
      via_arch <- if ("via_archive" %in% names(install_results))
        install_results$package[!is.na(install_results$via_archive) & install_results$via_archive] else character(0)
      report_inst <- sprintf(
        "Dependencies were installed into a throwaway library before running: %d succeeded, %d failed. %s were installed.",
        n_ok, n_fail, paste(install_results$package, collapse = ", "))
      report_arch <- if (length(via_arch) > 0) sprintf(
        paste0("**%d of these were no longer on live CRAN** and were instead ",
               "installed from the **CRAN Archive**'s most recent published ",
               "version — a real version the package once had, but not ",
               "necessarily the one the paper's authors used: %s."),
        length(via_arch), paste(via_arch, collapse = ", ")) else NULL
      failed <- install_results[!install_results$installed, , drop = FALSE]
      report_fail <- if (nrow(failed) > 0) sprintf(
        "**Failed:** %s", paste(sprintf(
          "%s%s", failed$package,
          ifelse(nzchar(failed$message), sprintf(" (%s)", failed$message), "")),
          collapse = "; ")) else NULL
      report <- c(report, report_inst, report_arch, report_fail)
    }
  } else {
    report <- c(report,
      "*Executing the code is a separate, opt-in phase (run with `execute = TRUE`). This report describes what a run would involve; it did not run anything.*")
  }

  ## Reported vs. reproduced ----
  # Match statistical tests the manuscript text reports against the tests
  # actually present in the extracted analysis output (JASP/jamovi tables and,
  # when execute = TRUE, the run's own console output), via
  # match_reported_output(). Only attempted when there is output to match
  # against — with none, every test would trivially show as unmatched, which
  # is a "nothing was run/extracted" fact already covered above, not a new
  # finding about THIS section.
  match_report <- NULL
  match_summary <- NULL
  match_table_raw <- NULL   # exported in the return list, below — the FULL
  # per-test match_reported_output() result (not the report's own prettified
  # `match_table`, which blanks NA to "" and renames columns for display),
  # so a caller can inspect exactly which reported test matched what without
  # re-running the match from scratch. Previously computed here and used only
  # to build report TEXT, then discarded — absent from the module's own
  # returned list and so absent from any saved .rds of the result, which meant
  # re-auditing match quality needed a live re-run against the paper object.
  n_output_stats <- sum(vapply(stat_output, function(s) nrow(s$long), integer(1)))
  if (length(stat_output) > 0) {
    # include_tables = TRUE: see .empty_match()'s identical call above for why.
    matched <- tryCatch(match_reported_output(paper, stat_output,
                                              include_tables = TRUE),
                        error = function(e) NULL)
    if (!is.null(matched) && nrow(matched) > 0) {
      match_table_raw <- matched
      ms <- attr(matched, "summary")
      match_summary <- ms
      match_report <- sprintf(
        paste0("%d statistical test%s reported in the manuscript text %s ",
               "checked against the extracted analysis output: %d matched ",
               "(%d fully, %d partially) — %s%% found."),
        ms$n_tests, plural(ms$n_tests), plural(ms$n_tests, "was", "were"),
        ms$n_found, ms$n_full, ms$n_partial, ms$pct_found)
      # See .empty_match()'s identical block above for the full rationale —
      # this is the same "Plausible" explanation, duplicated here because
      # this code path and that one are themselves an intentional duplicate
      # (see that function's own header comment for why).
      n_flagged <- sum(!is.na(matched$plausible_split))
      if (n_flagged > 0) match_report <- c(match_report, paste(
        "Some reported values were only found alongside OTHER components",
        "after being separated from their own originally-reported neighbours",
        "(e.g. a mean split from its own confidence interval). The",
        "\"Plausible\" column marks whether that split is well-supported: TRUE",
        "when the pieces still trace back to the same underlying variable (via",
        "the output's own row labels), FALSE when no such link was found — a",
        "FALSE match is not necessarily wrong, but is worth checking against",
        "its own Source file/Analysis columns before treating it as confirmed."))
      match_table <- data.frame(
        Reported   = matched$reported,
        Found      = matched$found,
        Confidence = matched$confidence,
        Plausible  = ifelse(is.na(matched$plausible_split), "",
                            ifelse(matched$plausible_split, "yes", "no")),
        `Matched values` = ifelse(is.na(matched$match_values), "",
                                  matched$match_values),
        `Not matched` = ifelse(is.na(matched$not_matched), "",
                               matched$not_matched),
        `Source file` = ifelse(is.na(matched$source_file), "",
                               matched$source_file),
        Analysis   = ifelse(is.na(matched$analysis), "", matched$analysis),
        check.names = FALSE)
      report <- c(report, "#### Reported vs. reproduced", match_report,
                  scroll_table(match_table, maxrows = 15))
    }
  }

  # ── 10. Summary ─────────────────────────────────────────────────────────────
  n_ran_ok <- if (!is.null(run_results)) sum(run_results$outcome == "ran_ok") else NA_integer_
  n_nodep_sum <- if (!is.null(run_results))
    sum(run_results$outcome == "dependency_unavailable") else 0L
  summary_text <- c(
    if (isTRUE(execute)) sprintf(
      "We assessed AND ran %d R code file%s for reproducibility (each in an isolated subprocess).",
      n_code, plural(n_code)) else sprintf(
      "We assessed %d R code file%s for reproducibility (static analysis; no code was run).",
      n_code, plural(n_code)),
    sprintf("%d file%s appear%s runnable so far (parses, inputs resolve, placeable in the run order).",
            n_runnable, plural(n_runnable), if (n_runnable == 1) "s" else ""),
    if (n_dup > 0) sprintf(
      "%d file%s skipped as byte-identical duplicate%s of another file already in the run (the paper links more than one repository/component).",
      n_dup, plural(n_dup), plural(n_dup)),
    if (isTRUE(execute) && !is.null(run_results)) sprintf(
      "%d of %d script%s ran without error when executed.",
      n_ran_ok, nrow(run_results), plural(nrow(run_results))),
    if (n_nodep_sum > 0) sprintf(
      "%d script%s could not run because %s own dependency is unavailable (not counted against the traffic light).",
      n_nodep_sum, plural(n_nodep_sum), plural(n_nodep_sum, "its", "their")),
    if (n_missing_inputs > 0) sprintf(
      "%d referenced input%s unavailable (%d withheld due to size).",
      n_missing_inputs, plural(n_missing_inputs), n_withheld),
    sprintf("%d installable dependenc%s detected.", n_deps,
            if (n_deps == 1) "y" else "ies"),
    if (!is.null(match_report)) sprintf(
      "%d statistic%s stored from the extracted output; %d of %d reported test%s matched (%s%%).",
      n_output_stats, plural(n_output_stats), match_summary$n_found,
      match_summary$n_tests, plural(match_summary$n_tests), match_summary$pct_found)
  ) |> paste("\n- ", x = _, collapse = "")

  summary_table <- data.frame(
    paper_id             = .pid(structure_df, code_tbl),
    repro_code_n         = n_code,
    repro_runnable       = n_runnable,
    repro_missing_inputs = n_missing_inputs,
    repro_deps           = n_deps,
    repro_ran_ok         = n_ran_ok,
    repro_tests_reported = if (!is.null(match_summary)) match_summary$n_tests else NA_integer_,
    repro_tests_matched  = if (!is.null(match_summary)) match_summary$n_found else NA_integer_
  )

  # SPSS/Stata data ± syntax note (also flagged the light red above when no
  # .sps/.do was found).
  if (!is.null(spss_report)) report <- c(report, spss_report)
  if (!is.null(stata_report)) report <- c(report, stata_report)
  # Self-reproducible JASP/jamovi/SPSS-Viewer output, and Stata .smcl output
  # logs (extracted result tables, STATO-typed).
  if (!is.null(self_repro_report)) report <- c(report, self_repro_report)
  # Non-R code (Stata/SPSS without data+output, SAS, MATLAB, or any other
  # code_lang()) with no reproducible output to fall back on — appended here
  # too, not just in empty()'s early-return paths, so a paper with BOTH R
  # code (this path) AND e.g. orphaned SAS files still surfaces the SAS gap,
  # not just papers with no R code at all.
  if (!is.null(nonr_report)) report <- c(report, nonr_report)
  # Byte-identical duplicate files across repo mirrors, skipped before the run
  # (see the dedup step in section 2 above) — surfaced so a reader knows why
  # fewer scripts ran than code_check listed, rather than it looking like some
  # were silently missed.
  if (!is.null(dup_report)) report <- c(report, dup_report)

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
    # Extracted JASP/jamovi statistical output: per-file statistical-output
    # JSON (R/stat-output.R's stat_output_json(); a metacheck-native schema,
    # not ISA-JSON — an earlier version borrowed ISA's vocabulary and that was
    # dropped, see R/stat-output.R's file header) + flat rows.
    stat_output = stat_output,
    # The full match_reported_output() result (one row per reported test —
    # see match_table_raw's own comment above), NULL when there was no output
    # to match against at all.
    match_table = match_table_raw
  )
  # When asked to keep the sandbox, surface its path so the caller can inspect
  # exactly what ran (data/, statistical_output/, and — when execute = TRUE —
  # the rewritten scripts + temp library). Not gated on execute: a JASP/jamovi
  # -only paper (execute = FALSE) also materialises a root, for
  # statistical_output/ alone.
  #
  # A NAMED LIST ELEMENT, not an R attribute: module_run() (R/module.R) wraps
  # every module's return value into a fresh `report_items` list, copying only
  # named elements (plus module()/report()'s own fixed set) — any attribute set
  # on the returned object itself (as `attr(out, "sandbox") <- ...` used to do
  # here) is silently dropped in that repackaging. convert_psychds() then always
  # saw sandbox = NULL and never copied statistical_output/ into the archive.
  if (isTRUE(keep_sandbox) && !is.null(sandbox_root))
    out$sandbox <- sandbox_root
  out
}
