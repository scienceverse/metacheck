#' Reproducibility Check
#'
#' @description
#' This module assesses whether a paper's R and Python code can be run on its
#' data. It works in two phases. The **static** phase (always run) collects the package
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
#' plan from `psychds_file_plan()`, and building a run-order dependency graph.
#' It reports what *would* be installed and run rather than doing it.
#'
#' When `execute = TRUE`, the module additionally **runs the downloaded code on
#' your machine** — a deliberate, opt-in action gated behind that argument
#' (`callr` is required for R code). Each script runs in an isolated subprocess so a crash
#' cannot take down the R session; note this isolates crashes, **not** the
#' filesystem or network. Installs (when `install_missing = TRUE`) and the
#' throwaway run library never touch your main R library. Each script's outcome
#' is one of `ran_ok`, `errored`, `timed_out`, `skipped_missing_inputs`,
#' `dependency_unavailable`, `not_parsed`, `run_by_source` (an R script
#' another script runs with `source()`, which is run only inside that script,
#' since on its own it lacks the packages and objects the sourcing script sets
#' up first), or `run_by_import` (a Python file other files import, a module
#' rather than a script, unless it has an `if __name__ == "__main__":`
#' block); a timeout means "still running at
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
#' reproduction failure. A script that fails with `could not find function`
#' is run once more with the `library()` calls the project's other scripts
#' make added at its top (or the one package that exports the function, or
#' magrittr for `%>%`), and a script that then runs is reported as
#' reproducible only after that repair.
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
#' Python code (`.py` files, and notebooks and Quarto documents whose code is
#' Python, from which the code is extracted first) goes through the same two
#' phases, reported in its own "Python code" section. Each file is checked for
#' syntax errors with Python's own parser when a Python interpreter (`python`,
#' or else `python3`) is available; dependencies are read from the `import`
#' statements, with versions from a `requirements.txt` or `pyproject.toml`
#' when there is one, and installed with `pip`; each script runs as its own
#' `python` process (`processx` is required), so unlike R scripts, a name one
#' script defines is not available to another. When no Python interpreter is
#' available, the Python files are reported as not run and the R results are
#' still returned. Statistics the Python scripts print are compared with the
#' paper together with the R output and any self-reproducible output
#' (JASP/jamovi, SPSS-Viewer, notebook outputs).
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
#'   copy of the package or the Psych-DS layout, see `layout`). This runs
#'   downloaded code on your machine
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
#'   the base image is `ghcr.io/scienceverse/metacheck_r_large:latest`, a
#'   pre-built image with the ~750 most common corpus packages already
#'   installed, so most papers skip most of the install phase entirely
#'   rather than reinstalling their dependencies from scratch on every run.
#'   Python code runs in `ghcr.io/scienceverse/metacheck_py_small:latest`.
#'   `"process"` needs `callr` for R code and `processx` and a Python
#'   interpreter for Python code; `"docker"` needs `processx`.
#' @param layout the files the code runs against when `execute = TRUE`.
#'   `"package"` copies every file the run has (every file of a local folder;
#'   of a repository, every file that was downloaded, see `download`) to its
#'   own path, so the code finds the folders and files it reads where they
#'   are in the package, and only a path that does not work there (an
#'   absolute path on the author's computer, a file in another folder) is
#'   rewritten. `"psychds"` copies only the data files, laid out as a
#'   Psych-DS release, and rewrites the code's paths to match. `"auto"` (the
#'   default) uses `"package"` for a local folder (`local_path`) and
#'   `"psychds"` otherwise.
#' @param docker_use_declared_version if TRUE (and `sandbox = "docker"`), use
#'   the R version `code_check`'s version-pinning detection found declared in
#'   the repository (an `renv.lock`/`sessionInfo()` record) instead of the
#'   pre-built `metacheck_r_large` image — `rocker/r-ver:<declared version>`, a
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
#'   `install.packages`, GitHub/URL via `remotes`; Python packages via
#'   `pip install --target`). Default FALSE: a script
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
#' @param total_timeout the time in seconds for running all scripts together,
#'   R and Python, counted from when the first script starts (default 3600,
#'   one hour; `NULL` or `Inf` for no limit), or a clock time (`POSIXct`) by
#'   which all scripts must be done, such as a server's job limit minus the
#'   time its report needs. Once it has run out, no further
#'   script is started (outcome `skipped_time_limit`), and a running script is
#'   stopped, so a package with many slow scripts still gets a report.
#' @param keep_sandbox if TRUE, do not delete the temp materialised layout
#'   (data/, statistical_output/, and — when `execute = TRUE` — the rewritten
#'   scripts + temp library) after the run, and return its path as attribute
#'   `"sandbox"` on the result — so you can inspect exactly what ran. Default
#'   FALSE (cleaned up). A materialised layout (and so `statistical_output/`) is
#'   built whenever there is any extracted statistical output (a `.jasp`/`.omv`
#'   file, or — with `execute = TRUE` — executed R code that printed results),
#'   independent of `execute`.
#' @param cache if `TRUE`, forwarded to the internal `data_check`/`code_check`
#'   runs: files they download are kept in the persistent on-disk cache (see
#'   [repo_cache_dir()]) and reused on later runs instead of being
#'   re-downloaded. If `FALSE` (the default), downloads go to a temporary
#'   directory discarded when the R session ends — nothing accumulates on
#'   disk. Only takes effect when `data_check`/`code_check` have not already
#'   been run for this paper (see the chained-output note above); if you ran
#'   them yourself first, set `cache` there instead.
#' @param download,skip_types,peek_zips,max_file_size,max_download_size
#'   forwarded to the internal `data_check` run, with the same defaults
#'   `data_check` itself uses — see `data_check`'s own docs for what each
#'   controls. Only takes effect when `data_check` has not already been run
#'   for this paper (see the chained-output note above); if you ran it
#'   yourself first with different arguments, that run's file set is reused
#'   as-is. Matching these to whatever built a paper's on-disk archive matters
#'   for more than just speed: `data_check`'s LLM-refined column/file
#'   classification is cached by exact call inputs (see [llm_cache()]), and
#'   the file set these arguments select is part of that batched input — a
#'   different `download`/`peek_zips`/etc. here silently produces a different
#'   file set, and so a cache miss, even for an identical paper and an
#'   identical underlying question.
#' @param tables_dir optional directory of per-paper `<paper_id>.rds` files
#'   written by `capture_module_tables()` (e.g. a prior full corpus build).
#'   When set, `data_check`/`code_check`/`psychds_check` outputs are read
#'   from this paper's saved file FIRST, before the chained-output check
#'   above and before falling back to actually running the module (or, for
#'   the file/folder plan `psychds_check` would have supplied, building it
#'   directly from `structure_df` via `psychds_file_plan()`) -- so a
#'   `reproducibility_check`-only retest of an already-built paper (e.g.
#'   `report(paper, "reproducibility_check")`, with nothing else chained) can
#'   reuse a previous full build's already-computed results and skip
#'   `data_check`/`code_check` (and their downloads/LLM calls) entirely.
#'   `NULL` (the default) disables this and behaves exactly as before.
#' @param workers when `paper` is a paperlist AND `execute = TRUE` AND
#'   `sandbox = "docker"`, the number of papers to run CONCURRENTLY, each in
#'   its own background R process (`callr`). Default 1: papers are checked
#'   one at a time, in order, exactly as before. Raising this starts the
#'   next paper's Docker run the moment a running one finishes, instead of
#'   waiting for the whole paperlist to go one paper at a time — the actual
#'   work of each paper (installing dependencies, running scripts in
#'   containers) is unaffected; only how many papers' worth of that work
#'   happen at once changes. Ignored (treated as 1) for a single paper, or
#'   whenever `execute = FALSE` or `sandbox = "process"`: those paths are
#'   fast enough, or already isolated only at the crash level, that running
#'   several at once is not worth the added complexity — the docker sandbox
#'   is the one with slow, external, genuinely parallelisable work (`docker
#'   run`, package installs). Each worker's container is capped at
#'   `1/workers` of the host's CPUs and memory (see
#'   `.repro_docker_resource_limits()`) so `workers` concurrent containers
#'   cannot collectively exceed the host — set `workers` no higher than the
#'   number of papers you actually want fighting over that share at once.
#' @param results_dir when `paper` is a paperlist and `workers > 1`, an
#'   optional directory to write each paper's full result to
#'   (`<paper_id>.rds`, via `capture_module_tables()`'s own format) AS SOON
#'   as that paper finishes, not just once the whole paperlist is done — so
#'   a long batch is inspectable and resumable mid-run (see
#'   `collect_module_tables()`) even if it is interrupted partway through,
#'   and a crashed/killed run does not lose the papers that already
#'   completed. `NULL` (the default) does not write anything; every paper's
#'   result is still returned in memory either way.
#'
#' @returns a list
reproducibility_check <- function(paper, local_path = NULL, local_only = FALSE,
                                  model = llm_model(), params = list(),
                                  execute = FALSE, sandbox = c("process", "docker"),
                                  layout = c("auto", "package", "psychds"),
                                  docker_use_declared_version = FALSE,
                                  install_missing = FALSE,
                                  cran_install_main = FALSE,
                                  timeout = 600, total_timeout = 3600,
                                  keep_sandbox = FALSE,
                                  cache = FALSE,
                                  download = "data",
                                  skip_types = NULL,
                                  peek_zips = TRUE,
                                  max_file_size = 100,
                                  max_download_size = 500,
                                  skip_on_api_limit = FALSE,
                                  tables_dir = NULL,
                                  workers = 1,
                                  results_dir = NULL) {
  # paper <- psychsci[[233]] # to test (many code files, several issues)
  sandbox <- match.arg(sandbox)
  # set when the first script starts (see total_timeout)
  script_deadline <- NULL
  layout <- match.arg(layout)
  # A local folder has every file on disk: run the code in a copy of the
  # package as it is (see .repro_package_plan()).
  if (layout == "auto") layout <- if (!is.null(local_path)) "package" else "psychds"
  layout_name <- if (layout == "package") "the package" else "the Psych-DS layout"

  # ── Paperlist + workers > 1: run papers CONCURRENTLY ────────────────────────
  # Every argument above is still just for ONE paper's own reproducibility
  # check; this branch does not change any of that single-paper logic (below,
  # unchanged) — it only decides how many papers' worth of it run at the same
  # time when there is more than one paper to check. Restricted to the case
  # that actually benefits: sandbox = "docker" execution is slow, external
  # (docker run, package installs) work that genuinely overlaps across
  # papers; a single paper, execute = FALSE (static analysis only), or
  # sandbox = "process" all fall through to the existing sequential path
  # unchanged (workers is simply ignored there, not an error, so a caller
  # looping over mixed execute/sandbox settings does not need to branch on it
  # themselves).
  if (.is_paper_list(paper) && length(paper) > 1 && workers > 1 &&
      isTRUE(execute) && sandbox == "docker") {
    return(.reproducibility_check_batch(
      paper, workers = workers, results_dir = results_dir,
      local_path = local_path, local_only = local_only,
      model = model, params = params,
      execute = execute, sandbox = sandbox,
      docker_use_declared_version = docker_use_declared_version,
      install_missing = install_missing,
      cran_install_main = cran_install_main,
      timeout = timeout, total_timeout = total_timeout,
      keep_sandbox = keep_sandbox,
      cache = cache, download = download, skip_types = skip_types,
      peek_zips = peek_zips, max_file_size = max_file_size,
      max_download_size = max_download_size,
      skip_on_api_limit = skip_on_api_limit, tables_dir = tables_dir))
  }

  # Executing downloaded code runs it on this machine: gate it behind an explicit
  # opt-in. sandbox = "process" (default) isolates a CRASH via callr, not the
  # filesystem/network -- code can still write/delete/reach the network freely.
  # sandbox = "docker" is the actual containment boundary (network off, read-only
  # filesystem, non-root user during the run phase -- see
  # R/reproducibility_check_docker.R's header comment); use it for untrusted
  # downloaded code. The 'callr' check for sandbox = "process" comes later,
  # once it is known whether the paper has R code: Python scripts run through
  # processx and do not need callr.
  if (isTRUE(execute) && sandbox == "docker") {
    docker_ok <- repro_docker_available()
    if (!docker_ok$ok)
      stop("execute = TRUE with sandbox = \"docker\": ", docker_ok$msg, call. = FALSE)
  }

  # Session-scoped, same temporary-override pattern as llm_use() below: every
  # *_info()/*_links() listing call this run makes (repo_check, and anything
  # data_check/code_check call in turn) shares ONE retry helper,
  # .batch_query() (R/utils.R), which has no skip_on_api_limit parameter of
  # its own and is not reachable by threading one through this function's own
  # call chain alone. Setting the option here, once, for the whole paper,
  # covers that path too instead of just download_repo_files()'s. Restored
  # unconditionally on exit so it never leaks into the caller's session.
  prev_skip_opt <- getOption("metacheck.skip_on_api_limit")
  options(metacheck.skip_on_api_limit = skip_on_api_limit)
  on.exit(options(metacheck.skip_on_api_limit = prev_skip_opt), add = TRUE)

  .pid <- function(...) {
    id <- paper_id(paper)
    for (df in list(...)) {
      if (length(id) > 0) break
      if (!is.null(df) && "paper_id" %in% names(df)) id <- unique(df$paper_id)
    }
    if (length(id) == 0) NA_character_ else id[[1]]
  }

  # ── 0. Saved module outputs from a prior full build (tables_dir) ────────────
  # Read once, lazily: only opened if tables_dir is actually set, and only
  # once even though several items below may be pulled from it. A paper never
  # captured (or with no file for this module) simply yields NULL per item, so
  # this degrades to the pre-tables_dir behaviour with no special-casing
  # needed at the call sites below.
  .saved_tables <- NULL
  .saved_tables_tried <- FALSE
  get_saved_output <- function(mod, item) {
    if (is.null(tables_dir)) return(NULL)
    if (!.saved_tables_tried) {
      .saved_tables_tried <<- TRUE
      pid <- .pid()
      path <- if (!is.na(pid)) file.path(tables_dir, paste0(pid, ".rds")) else NA_character_
      if (!is.na(path) && file.exists(path))
        .saved_tables <<- tryCatch(readRDS(path), error = function(e) NULL)
    }
    .saved_tables$modules[[mod]][[item]]
  }

  # ── 1. Inputs from the upstream modules ─────────────────────────────────────
  # code_check gives the code files + their per-file analysis (parse status,
  # packages); psychds_file_plan() gives the file→target plan the path rewrite
  # needs (plain file/folder organisation, no Psych-DS compliance checking);
  # data_check gives the download status behind a missing input. Reuse chained
  # outputs first (a live report() chain that already ran these modules);
  # then a prior full build's saved tables (tables_dir, above); run what is
  # still missing after both.
  code_tbl     <- get_prev_outputs("code_check", "table") %||% get_saved_output("code_check", "table")
  # psychds_check's own `table` IS the plan (unchanged shape after the
  # psychds_file_plan() split below), so a live chain that already ran
  # psychds_check, or a prior saved build, both still supply it under that
  # name; only a genuinely missing plan is built fresh, directly from
  # structure_df, without going through the psychds_check module at all.
  plan         <- get_prev_outputs("psychds_check", "table") %||%
    get_saved_output("psychds_check", "table")
  structure_df <- get_prev_outputs("data_check", "structure") %||% get_saved_output("data_check", "structure")
  group_no_evidence <- get_prev_outputs("data_check", "group_no_evidence") %||%
    get_saved_output("data_check", "group_no_evidence")
  # code_check's declared-version detection (renv.lock/sessionInfo/groundhog/
  # checkpoint — see .code_version_pin_check()), reused by the Docker backend
  # below to pick the base image's R version. NULL until code_tbl's source is
  # resolved (either already cached, or freshly run just below).
  version_pin  <- get_prev_outputs("code_check", "version_pin") %||% get_saved_output("code_check", "version_pin")

  run_missing <- function(mod) {
    # model / params only go to the modules that accept them (data_check uses
    # the LLM for study grouping); code_check has no such arguments, so
    # passing them would error with "unused arguments".
    args <- list(paper, mod, local_only = local_only)
    if (!is.null(local_path)) args$local_path <- local_path
    if (mod == "data_check") {
      args$model <- model; args$params <- params
    }
    if (mod %in% c("data_check", "code_check")) {
      args$cache <- cache
      args$skip_on_api_limit <- skip_on_api_limit
    }
    if (mod == "data_check") {
      args$download <- download
      args$skip_types <- skip_types
      args$peek_zips <- peek_zips
      args$max_file_size <- max_file_size
      args$max_download_size <- max_download_size
    }

    # This module's own execution/matching logic never depends on LLM-refined
    # column classification or scale naming (data_check's own optional
    # refinement over what deterministic rules already resolve) -- only on
    # file names/locations, code language, and the path-rewrite plan. So
    # force llm_use(FALSE) for JUST this fallback call, regardless of
    # whatever the caller's session has set globally: a solo
    # reproducibility_check retest run with llm_use(TRUE) left on from
    # unrelated work would otherwise pay data_check's full LLM-classification
    # cost (which can be slow) for a result this module never uses. Restored
    # unconditionally on exit, including on error, so it never leaks into the
    # caller's session.
    prev_llm_use <- llm_use()
    llm_use(FALSE)
    on.exit(llm_use(prev_llm_use), add = TRUE)

    do.call(module_run, args)
  }
  if (is.null(structure_df)) {
    dc <- run_missing("data_check")
    structure_df <- dc$structure
    group_no_evidence <- dc$group_no_evidence
  }
  if (is.null(plan)) {
    # psychds_file_plan() is a pure function of structure_df -- no module_run()
    # needed, so building a missing plan here never re-downloads or re-runs
    # data_check the way going through the psychds_check module used to.
    plan <- psychds_file_plan(structure_df, group_no_evidence)$table
  }
  if (layout == "package") plan <- .repro_package_plan(structure_df)
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
  # never "code with no output" — a .omv/.jasp IS both at once. "Python" is
  # ALSO excluded now, for the same reason "R" is: it has its own
  # code-execution path below (parallel to the R path), not "code with no
  # reproducible output to fall back on".
  nonr_lang <- if (!is.null(code_tbl) && "language" %in% names(code_tbl))
    code_tbl$language[!is.na(code_tbl$language) &
                      !code_tbl$language %in% c("R", "Python", "JASP", "jamovi")] else character(0)
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

  # The reported-vs-reproduced comparison, used by every path: by empty() for
  # a paper with no R or Python code, and after both language blocks (see
  # "## Shared across languages" below) otherwise. It reads stat_output when it
  # is CALLED, so after the language blocks it sees the self-reproducible
  # output plus whatever the R and Python runs added. A paper with no R code
  # but real self-reproducible output (JASP/jamovi/.spv/.smcl/Mplus .out) once
  # returned from empty() before any comparison ran, so the paper's REPORTED
  # numbers were never checked against the 1500+ statistics extracted from
  # those files.
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
  # statistics. `py` is ALSO no longer in this list, for the same reason —
  # Python code is now actually run by this module (see the "Python code"
  # path below, parallel to the R path), so a `.py` file is no longer
  # "unassessed code with no output counterpart to recover"; it has its own
  # static+execute phase and its own report section, same as R.
  non_r_code_pat <- "\\.(jl|m|sas|sps|spss|do|ado|java|cpp|c|sql|inp)$"
  non_r_data_pat <- "\\.(mat)$"
  n_non_r <- if (!is.null(structure_df) && "file_name" %in% names(structure_df))
    sum(grepl(non_r_code_pat, structure_df$file_name, ignore.case = TRUE)) else 0L
  n_non_r_data <- if (!is.null(structure_df) && "file_name" %in% names(structure_df))
    sum(grepl(non_r_data_pat, structure_df$file_name, ignore.case = TRUE)) else 0L
  non_r_note <- if (n_non_r > 0) sprintf(
    paste0(
      " The repository does contain %d code file%s in other languages; this ",
      "phase only RUNS R and Python code (see the section%s above for what we ",
      "found and could — or could not — check for those files)."),
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

  # R and Python are both handled in this phase (Python added alongside the
  # pre-existing R path -- see the "Python code" section far below, which
  # runs in parallel to the R section immediately following this gate and is
  # merged with it right before this function's final return). Neither
  # language present at all is still a genuine "nothing to assess" na/early
  # return, same as before Python support existed.
  if (is.null(code_tbl) || nrow(code_tbl) == 0 ||
      !"language" %in% names(code_tbl))
    return(empty(paste0(
      "We found no R or Python code files to assess for reproducibility.", non_r_note),
      tl = empty_tl, extra_report = c(spss_report, stata_report, nonr_report)))

  r_files <- code_tbl[!is.na(code_tbl$language) & code_tbl$language == "R", ,
                      drop = FALSE]
  # renv's own files (renv/activate.R, renv/library/) are not the paper's
  # analysis code, so they are not run. code_check already leaves them out;
  # this also covers a code table passed in directly. The pattern used to be
  # matched against file_name, which holds no folder, so it never matched
  # (#548); see .in_renv_folder().
  r_files <- r_files[!.in_renv_folder(r_files), , drop = FALSE]
  py_files <- code_tbl[!is.na(code_tbl$language) & code_tbl$language == "Python", ,
                       drop = FALSE]
  py_files <- py_files[!.in_renv_folder(py_files), , drop = FALSE]
  has_r <- nrow(r_files) > 0
  has_py <- nrow(py_files) > 0

  # ── DEBUG tracing (TEMPORARY — remove before release). Prints every step of
  # the execute phase so a hang is attributable to the exact operation.
  # Hoisted here (shared by both the R and Python execute blocks below,
  # rather than defined twice) so a Python-only paper's execute block can
  # call it too, even when has_r is FALSE and the R block's own copy never
  # runs. ───────────────────────────────────────────────────────────────────
  .dbg <- function(...) message("[repro] ", ...)
  if (!has_r && !has_py)
    return(empty(paste0(
      "We found no R or Python code files to assess for reproducibility.",
      non_r_note), tl = empty_tl, extra_report = c(spss_report, stata_report, nonr_report)))

  if (has_r && isTRUE(execute) && sandbox == "process" &&
      !requireNamespace("callr", quietly = TRUE))
    stop("execute = TRUE with sandbox = \"process\" needs the 'callr' package ",
         "to run R code (it runs each script in an isolated subprocess). ",
         "Install callr, or use sandbox = \"docker\".", call. = FALSE)

  # The comparison with the paper (the "Reported vs. reproduced" section), the
  # SPSS/Stata, self-reproducible-output and other-language sections, and the
  # SPSS/Stata red light do not belong to either language: they are added once,
  # after both the R and the Python blocks (see "## Shared across languages"
  # below), so they also apply to a Python-only paper and the comparison sees
  # the output of both languages.

  # ── R code path (unchanged from before Python support existed) ─────────────
  # Guarded by has_r rather than an early return, so a Python-only paper still
  # reaches the Python section below. Every R-side local variable this guard
  # produces (table, summary_table, report, summary_text, run_results,
  # install_results, modifications, tl, stat_output additions, ...) is
  # combined with its py_-prefixed Python twin at the very end of this
  # function (see "## Combine R + Python results" below) -- nothing inside
  # this block itself was changed to make that possible.
  if (has_r) {

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
    dup_kept_idx <- vapply(which(dup), function(i) which(hashes == hashes[i])[1], integer(1))
    # Each copy by its link, else its path in the repository; never its place
    # on the computer that runs the check, which on a server is the server's
    # job folder (Lakens/tue_datamanager_check#5, item 2).
    dup_loc <- .repro_shown_location(r_files, which(dup))
    kept_loc <- .repro_shown_location(r_files, dup_kept_idx)
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

  # raw_text_list keeps each file's UNEXTRACTED text alongside code_text_list
  # (which, for an .Rmd/.qmd, is already knitr::purl()'d down to just its {r}
  # chunks) -- the content sniffs below (#394/#396) need the file's true raw
  # content to recognise a JSON/HTML dump or a JAGS model saved with a `.R`
  # extension; code_extract_r() on such a file returns character(0) (no {r}
  # chunks to purl), which would make every sniff see nothing to sniff at all.
  raw_text_list <- lapply(seq_len(n_code), function(i) {
    path <- resolve_row_path(i)
    if (is.na(path)) return(character(0))
    tryCatch(code_read(path), error = function(e) character(0))
  })
  names(raw_text_list) <- r_files$file_name

  code_text_list <- lapply(seq_len(n_code), function(i) {
    the_file <- r_files[i, ]
    pb_read$tick(1, list(what = the_file$file_name))
    path <- resolve_row_path(i)
    if (is.na(path)) return(character(0))
    is_rmd <- grepl("\\.(rmd|qmd)$", the_file$file_name, ignore.case = TRUE)
    # An R-kernel notebook is JSON: its code cells are extracted, as code_check
    # does, or the JSON itself would be run as R.
    is_ipynb <- grepl("\\.ipynb$", the_file$file_name, ignore.case = TRUE)
    tryCatch(
      if (is_rmd) code_extract_r(path)
      else if (is_ipynb) code_extract_py(path)
      else raw_text_list[[i]],
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
  # the package layout leaves a reference that works as written as it is
  if (layout == "package") rewrite_list <- stats::setNames(lapply(seq_along(rewrite_list), function(i)
    .repro_keep_working_refs(rewrite_list[[i]], plan, r_files$file_path[i])), names(rewrite_list))

  # Per-file rewrite counts + unresolved reads (referenced, not matched in plan).
  rewrites_n  <- vapply(rewrite_list, function(d)
    if (nrow(d)) sum(d$matched & !d$ambiguous & !(d$as_written %||% FALSE)) else 0L, integer(1))
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
  # source() edges resolved only after normalising both basenames (a
  # renumbered/reworded script reference — see repro_run_order()'s own
  # roxygen and issue #392) — surfaced in the report below as lower-
  # confidence matches, distinct from an exact source() resolution.
  fuzzy_sources <- attr(order_tbl, "fuzzy_sources") %||%
    data.frame(from = character(0), to = character(0))

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
  parse_error_raw <- if ("parse_error" %in% names(r_files))
    !is.na(r_files$parse_error) & r_files$parse_error else rep(FALSE, n_code)

  # Before treating a parse failure as a genuine code defect, sniff whether
  # the file's content even looks like R at all (#394: a JSON/HTML dump saved
  # with a `.R`/`.Rmd` extension) or is a well-known non-R DSL this module
  # recognises (#396: a JAGS/BUGS model definition, `model { ... }`) — see
  # .repro_content_sniff()/.repro_is_jags_model() (R/reproducibility_check.R).
  # Both are informational/positive findings, not reproducibility defects:
  # the file is exactly what it claims to be, just not standalone R, so
  # neither counts toward parse_errs/the traffic light the way a real syntax
  # error does — same treatment, since they are siblings (see #396's own
  # issue text for why they must stay consistent with each other).
  sniff_type <- rep(NA_character_, n_code)   # "not_r_content" | "jags_model" | NA
  sniff_detail <- rep(NA_character_, n_code)
  if (any(parse_error_raw)) {
    for (i in which(parse_error_raw)) {
      raw <- raw_text_list[[i]]
      fn  <- r_files$file_name[i]
      jags_hit <- .repro_is_jags_model(
        raw, fn, other_code_text = raw_text_list[setdiff(seq_len(n_code), i)])
      if (jags_hit) {
        sniff_type[i] <- "jags_model"
        sniff_detail[i] <- "JAGS/BUGS model definition (not standalone R)"
        next
      }
      detected <- .repro_content_sniff(raw)
      if (!is.na(detected)) {
        sniff_type[i] <- "not_r_content"
        sniff_detail[i] <- sprintf(
          "file content does not appear to be R (detected: %s)", detected)
      }
    }
  }
  n_not_r_content <- sum(sniff_type == "not_r_content", na.rm = TRUE)
  n_jags_model    <- sum(sniff_type == "jags_model", na.rm = TRUE)

  # A "genuine" parse error excludes both sniffed categories -- this is what
  # feeds parse_errs/the traffic light/the "Parsing" report section below, so
  # neither a non-R dump nor a JAGS model recolours the light or counts
  # against the paper's own code.
  parse_error_genuine <- parse_error_raw & is.na(sniff_type)
  parse_errs <- sum(parse_error_genuine)

  # A file is "runnable-so-far" when it parses, all its referenced inputs
  # resolve (matched or produced upstream), and it is placeable in the order.
  # A sniffed file (not_r_content/jags_model) is still not RUN as R (it is
  # not R), but is no longer marked not-runnable *because it "failed to
  # parse"* -- see not_runnable_reason below, which gives it its own reason
  # instead of lumping it in with "parse_error".
  parses <- !parse_error_genuine
  runs_as_r <- !parse_error_raw   # still gates execution (parses_named), below
  file_order <- order_tbl$order[match(r_files$file_name, order_tbl$file_name)]
  placeable <- !is.na(file_order)

  # "Produced upstream" must respect the run order: a file counts as available
  # to script S only when some script that runs *before* S writes it. The flat
  # pool of all writes would also excuse a file that is only produced later,
  # which cannot help S actually run. A file S writes itself is not one it is
  # missing either: a script that reads back what it saved was "missing_input"
  # and not run (U235).
  writes_by_file <- io$writes %||% list()
  names(writes_by_file) <- io$file_name %||% names(writes_by_file)
  produced_before <- lapply(seq_len(n_code), function(i) {
    own <- unlist(writes_by_file[r_files$file_name[i]], use.names = FALSE)
    ord_i <- file_order[i]
    earlier <- if (is.na(ord_i)) character(0) else
      r_files$file_name[!is.na(file_order) & file_order < ord_i]
    unique(tolower(c(own, unlist(writes_by_file[earlier], use.names = FALSE))))
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
  # A sniffed file (not_r_content/jags_model) is real content that happens
  # not to be standalone R -- it "parses" for THIS purpose (parse_error_
  # genuine is FALSE for it), but it is still not something the run order/
  # missing-input logic should treat as ordinary analysis code either; it is
  # excluded from `runnable` via its own dedicated reason below, not via
  # `parses`, so it never contributes to parse_errs or the traffic light.
  runnable <- parses & placeable & !file_unresolved & is.na(sniff_type)
  n_runnable <- sum(runnable)

  # Why each file is not runnable, in the order the conditions are applied.
  # not_r_content/jags_model checked FIRST: a file that is not R at all
  # cannot meaningfully be "unplaceable" or "missing an input" either, and
  # this is what gives the report a distinct, positive reason instead of
  # folding it into "parse_error" (see the sniff step above and #394/#396).
  not_runnable_reason <- ifelse(
    !is.na(sniff_type), sniff_type,
    ifelse(runnable, NA_character_,
    ifelse(!parses, "parse_error",
           ifelse(!placeable, "unplaceable", "missing_input"))))
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

  # (SPSS/Stata data without syntax turns the light red for the paper as a
  # whole, after both languages -- see "## Shared across languages" below.)

  # ── 7b. Execution (opt-in; runs downloaded code) ────────────────────────────
  # Everything above is static. When execute = TRUE we materialise the Psych-DS
  # layout into a throwaway temp dir, write the path-rewritten scripts into it,
  # optionally install the declared deps into a throwaway library, and run each
  # script in run order in an isolated subprocess, capturing its outcome. Unlike
  # the static signals, an execution error DOES recolour the light: any errored /
  # timed-out script forces red (see after the block).
  run_results <- NULL
  install_results <- NULL
  # scripts whose output was searched only in part (see .repro_output_head())
  stat_cut <- integer(0)
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

    # Docker base image: by default the pre-built metacheck_r_large image (fast --
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
          "(the default), so the run uses the pre-built metacheck_r_large image's own ",
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
    run_tbl <- repro_write_scripts(code_text_list, rewrite_list, plan, sandbox_root,
                                   script_paths = if (layout == "package") r_files$file_path,
                                   writes_in_place = layout == "package")
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
      # Nothing was installed, so a script that then fails because one of its
      # packages is not there could not run here: dependency_unavailable, not
      # an error in the script.
      if (!isTRUE(install_missing)) failed_deps <- install_deps$package
    }

    # 3. Run in order. A file whose inputs are known-missing is not run (it
    #    cannot succeed); a file that will not parse is not run either. Gated
    #    on runs_as_r (not `parses`): a sniffed file (#394/#396) "parses" for
    #    the runnable/traffic-light bookkeeping above, but it is still not R
    #    and must not actually be handed to callr/Docker to execute.
    parses_named <- stats::setNames(runs_as_r, r_files$file_name)
    skip_files <- r_files$file_name[file_unresolved]
    if (length(skip_files))
      .dbg("skipping (missing inputs): ", paste(skip_files, collapse = ", "))
    run_order_names <- order_tbl$file_name[order(order_tbl$order)]

    # A script another script runs with source() (main.R loads tidyverse,
    # then source("a.R")) only works inside that script: each script runs in
    # its own fresh R process, so on its own it lacks the packages and objects
    # the sourcing script set up first, and fails (most often on `%>%`). It is
    # therefore not run on its own; it runs inside the script that sources it,
    # and is recorded as `run_by_source`. See .repro_run_by_source() for when
    # a sourced script still runs on its own.
    can_run <- stats::setNames(runs_as_r & !(r_files$file_name %in% skip_files),
                               r_files$file_name)
    run_by_source <- .repro_run_by_source(order_tbl, can_run)
    if (length(run_by_source))
      .dbg("run only through source(): ", paste(sprintf("%s (by %s)", names(run_by_source),
           vapply(run_by_source, paste, character(1), collapse = ", ")), collapse = "; "))
    run_r_scripts <- function(tbl, ord) {
      own <- tbl[!tbl$file_name %in% names(run_by_source), , drop = FALSE]
      res <- if (sandbox == "docker") {
        repro_run_scripts_docker(
          own, ord, sandbox_root = sandbox_root,
          lib_dir = if (dir.exists(lib_dir)) lib_dir else NULL,
          image = docker_image, timeout = timeout, skip = skip_files,
          parses = parses_named, failed_deps = failed_deps,
          deadline = script_deadline)
      } else {
        repro_run_scripts(
          own, ord,
          lib_dir = if (dir.exists(lib_dir)) lib_dir else NULL,
          timeout = timeout, skip = skip_files, parses = parses_named,
          failed_deps = failed_deps, deadline = script_deadline)
      }
      if (!length(run_by_source)) return(res)
      by_src <- data.frame(
        file_name = names(run_by_source), outcome = "run_by_source",
        error = "", error_type = NA_character_, undefined_var = NA_character_,
        stdout = "", stderr = "", elapsed = 0)
      by_src$script_lines <- rep(list(character(0)), nrow(by_src))
      by_src$captures <- rep(list(NULL), nrow(by_src))
      res <- dplyr::bind_rows(res, by_src)
      res[order(match(res$file_name, ord)), , drop = FALSE]
    }

    .dbg("running ", length(run_order_names), " script(s) in order: ",
         paste(run_order_names, collapse = " -> "))
    if (is.null(script_deadline)) script_deadline <- .repro_deadline(total_timeout)
    run_results <- run_r_scripts(run_tbl, run_order_names)
    .dbg("execution finished (pass 1). outcomes: ",
         paste(sprintf("%s=%s", run_results$file_name, run_results$outcome),
               collapse = "; "))

    # 4. Corrective re-run (ONE extra pass, no more). Two sibling correctable
    #    causes, both surfaced as error_type == "undefined_variable" by
    #    repro_run_scripts()'s classification (it deliberately does not split
    #    them — see its own roxygen): an "object 'X' not found" error means the
    #    script expected a VARIABLE another script defines but that ran later
    #    (or not at all); a "could not find function \"X\"" error more often
    #    means the script simply never library()'d the package that exports
    #    it. The raw message text (still available in `error`) is what tells
    #    the two apart here, since undefined_var/error_type alone cannot.
    #    Find the file that defines each missing variable (unchanged from
    #    before), AND find the one package that unambiguously exports each
    #    missing function name (new — see #390), add "definer precedes user"
    #    edges and/or inject the resolved library() call, recompute the order,
    #    and re-run once. A second undefined-var error after this pass is
    #    accepted as the final outcome (we do not iterate to convergence).
    undef_err <- run_results[!is.na(run_results$error_type) &
                             run_results$error_type == "undefined_variable", ,
                             drop = FALSE]
    reran <- FALSE
    reran_for_reorder <- FALSE
    # Where each pass-1 undefined symbol is actually defined, keyed on
    # "<user_file>||<symbol>" — kept around (not just used to decide whether to
    # reorder) so the FINAL report's undefined-variable table can say "defined
    # in <file>" even for a symbol that did not get an unambiguous reorder edge
    # (0 or >1 definers), and even after `run_results` is replaced by the
    # corrective re-run below.
    # A list, not character(0): `[[` on a named character vector throws
    # "subscript out of bounds" for a missing key instead of returning NULL,
    # which breaks the `definer_lookup[[key]] %||% ""` lookup below whenever
    # a symbol has 0 or >1 definers (see metacheck#402).
    definer_lookup <- list()
    # library() injections this pass decided on, file_name -> package(s) — kept
    # around (not just applied) so the final report / `modifications` output
    # (see #391) can say what was injected and why, the same way
    # definer_lookup survives for the reorder case. A list, since a script
    # can get several packages (see .repro_missing_fn_pkgs()).
    library_injections <- list()
    if (nrow(undef_err) > 0) {
      # "could not find function \"X\"" vs "object 'X' not found" — the SAME
      # regexes repro_run_scripts() itself matched against (R/reproducibility_
      # check.R), re-applied here to the raw error text because error_type
      # does not keep the distinction. A row whose message matches neither
      # (should not happen, since error_type == "undefined_variable" was only
      # ever set when one of them matched) is treated as the object-not-found
      # branch, the pre-existing behaviour.
      fn_pat <- "could not find function ['\"]([^'\"]+)['\"]"
      is_fn_missing <- grepl(fn_pat, undef_err$error, perl = TRUE)

      defs <- repro_defined_vars(code_text_list)
      def_of <- function(v) {
        # file_name(s) that define variable v at top level
        hit <- vapply(defs$defines, function(dd) v %in% dd, logical(1))
        defs$file_name[hit]
      }
      # Scoped candidate package set for the missing-library() lookup: the
      # paper's OWN declared dependencies (already resolved above as `deps`)
      # plus a small curated safe-list of extremely common packages — NOT a
      # blind scan of every installed package (see .repro_find_export_pkg()'s
      # own roxygen for why that is unsafe).
      candidate_pkgs <- unique(c(deps$package %||% character(0), .repro_common_pkgs()))

      extra_edges <- list()
      for (i in seq_len(nrow(undef_err))) {
        user <- undef_err$file_name[i]; v <- undef_err$undefined_var[i]
        definers <- setdiff(def_of(v), user)
        if (length(definers) > 0)
          definer_lookup[[paste0(user, "||", v)]] <- paste(definers, collapse = ", ")
        # Only act when exactly one file defines it (unambiguous); 0 = genuinely
        # undefined (a real bug, leave it), >1 = ambiguous (do not guess).
        if (length(definers) == 1) {
          extra_edges <- c(extra_edges, list(c(definers, user)))
          .dbg("undefined-var edge: '", definers, "' defines '", v,
               "' needed by '", user, "'")
        } else if (length(definers) == 0 && isTRUE(is_fn_missing[i]) &&
                   !(user %in% names(library_injections))) {
          # Zero same-repo definers for a missing FUNCTION (not object): add
          # the library() calls the project's other scripts make when one of
          # them provides it, else the one package that exports it, else
          # magrittr for `%>%` (see .repro_missing_fn_pkgs()). One injection
          # per file per pass (not %in% names() already) — a script failing
          # on a second, different missing function in the same pass is left
          # for a later, separate report rather than stacking guesses.
          pkgs <- .repro_missing_fn_pkgs(v, user, code_text_list, candidate_pkgs)
          if (length(pkgs)) {
            library_injections[[user]] <- pkgs
            .dbg("missing-library injection: '", v, "' -> library(",
                 paste(pkgs, collapse = ", "), ") for '", user, "'")
          }
        }
      }
      if (length(extra_edges) > 0 || length(library_injections) > 0) {
        order_tbl2 <- if (length(extra_edges) > 0)
          repro_run_order(io, extra_edges = extra_edges) else order_tbl
        run_order2 <- order_tbl2$file_name[order(order_tbl2$order)]
        # Re-write the scripts with the resolved library() injections applied
        # (repro_write_scripts()'s own `inject_libs` param — see #390) before
        # re-running; a file with no injection this pass is written exactly
        # as before (inject_libs only touches files named in it).
        run_tbl2 <- if (length(library_injections) > 0)
          repro_write_scripts(code_text_list, rewrite_list, plan, sandbox_root,
                              inject_libs = library_injections,
                              script_paths = if (layout == "package") r_files$file_path,
                              writes_in_place = layout == "package") else run_tbl
        .dbg("re-running ", length(run_order2), " script(s) in corrected order",
             if (length(library_injections) > 0) sprintf(
               " (with %d library() injection(s): %s)",
               length(library_injections),
               paste(sprintf("%s: library(%s)", names(library_injections),
                             vapply(library_injections, paste, character(1),
                                    collapse = ", ")), collapse = "; ")) else "",
             ": ", paste(run_order2, collapse = " -> "))
        run_results2 <- run_r_scripts(run_tbl2, run_order2)
        # a script the second run had no time left for keeps its first result
        late <- run_results2$outcome == "skipped_time_limit" &
          run_results2$file_name %in% run_results$file_name
        if (any(late)) {
          run_results2 <- dplyr::bind_rows(
            run_results2[!late, , drop = FALSE],
            run_results[match(run_results2$file_name[late], run_results$file_name), , drop = FALSE])
          run_results2 <- run_results2[order(match(run_results2$file_name, run_order2)), , drop = FALSE]
        }
        run_results <- run_results2
        order_tbl <- order_tbl2   # report the corrected order
        run_tbl <- run_tbl2       # so downstream setwd/family/injection detail (#391) reflects pass 2
        reran <- TRUE
        reran_for_reorder <- length(extra_edges) > 0
        .dbg("execution finished (pass 2). outcomes: ",
             paste(sprintf("%s=%s", run_results$file_name, run_results$outcome),
                   collapse = "; "))
      }
    }
    attr(run_results, "reran_for_order") <- reran
    attr(run_results, "reran_for_reorder") <- reran_for_reorder

    # An execution error (errored / timed_out) is a reproduction failure: force
    # the traffic light red, whatever the static signals said. A
    # dependency_unavailable script is deliberately excluded: it failed because
    # ITS OWN package could not be installed from either live CRAN or the
    # Archive (or a GitHub/URL source), which is an infrastructure limitation,
    # not evidence the paper's code is broken.
    if (any(run_results$outcome %in% c("errored", "timed_out"))) tl <- "red"
    else if (any(run_results$outcome == "skipped_time_limit") && tl == "green") tl <- "yellow"

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
      # One line per script, with the amount of output: parsing long output
      # used to take hours with nothing printed (#483).
      .dbg("parsing output of '", fn, "' (",
           if (is.null(so) || is.na(so) || !nzchar(so)) 0L else length(strsplit(so, "\n", fixed = TRUE)[[1]]),
           " lines) ...")
      so <- .repro_output_head(so)
      if (!is.null(attr(so, "n_lines"))) stat_cut[[fn]] <<- attr(so, "n_lines")
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
  if (nrow(fuzzy_sources) > 0)
    report_order <- paste(report_order, sprintf(
      paste0("**%d `source()` reference%s** only matched a file after ",
             "normalising both names (lowercase, extension stripped, a ",
             "leading numbering prefix stripped, separators collapsed) — a ",
             "likely renumbered/reworded filename rather than an exact ",
             "match: %s. Treat these as lower-confidence than an exact ",
             "`source()` resolution."),
      nrow(fuzzy_sources), plural(nrow(fuzzy_sources)),
      paste(sprintf("`%s` -> `%s`", fuzzy_sources$to, fuzzy_sources$from),
            collapse = "; ")))

  ## Path rewriting ----
  total_rewrites <- sum(rewrites_n)
  total_ambiguous <- sum(ambiguous_n)
  report_paths <- if (layout == "package") sprintf(
    "To run against the package, %d referenced data path%s that do%s not work as written would be rewritten to where the file is in the package.",
    total_rewrites, plural(total_rewrites), plural(total_rewrites, "es", "")) else sprintf(
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

  # Sniffed non-R content (#394/#396): informational, not a reproducibility
  # defect -- reported distinctly from a genuine parse failure so a reader
  # does not read "the author has a bug" into a file that was never meant to
  # be standalone R in the first place.
  if (n_not_r_content > 0 || n_jags_model > 0) {
    sniff_rows <- which(!is.na(sniff_type))
    report <- c(report, "#### Non-R content detected",
      if (n_not_r_content > 0) sprintf(
        "%d file%s that failed to parse %s not actually R content (a JSON/HTML/XML dump saved with an R-type extension) -- this is informational, not a reproducibility defect.",
        n_not_r_content, plural(n_not_r_content), plural(n_not_r_content, "is", "are")),
      if (n_jags_model > 0) sprintf(
        "%d file%s %s a JAGS/BUGS model definition (`model { ... }`), not standalone R -- these are meant to be passed to `rjags::jags.model()`/`R2jags::jags()`/`runjags::run.jags()`, not run directly, and are not a reproducibility defect.",
        n_jags_model, plural(n_jags_model), plural(n_jags_model, "is", "are")),
      scroll_table(data.frame(
        File = r_files$file_name[sniff_rows],
        Detected = sniff_type[sniff_rows],
        Detail = sniff_detail[sniff_rows],
        check.names = FALSE), maxrows = 10))
  }

  ## Execution ----
  if (!is.null(run_results) && nrow(run_results) > 0) {
    oc <- run_results$outcome
    n_ran_ok  <- sum(oc == "ran_ok")
    n_errored <- sum(oc == "errored")
    n_timeout <- sum(oc == "timed_out")
    n_skipped <- sum(oc == "skipped_missing_inputs")
    n_noparse <- sum(oc == "not_parsed")
    n_nodep   <- sum(oc == "dependency_unavailable")
    n_by_src  <- sum(oc == "run_by_source")
    n_late    <- sum(oc == "skipped_time_limit")
    report_exec <- sprintf(
      paste0("**The code was run.** Each script ran in an isolated %s ",
             "against a throwaway copy of ", layout_name, ", in the run order ",
             "above, with a %d-second per-script timeout. Of %d script%s: %d ran ",
             "without error, %d errored, %d timed out, %d %s skipped (inputs ",
             "unavailable), %d could not run because one of its own ",
             "dependencies is unavailable, %d did not parse%s. Running ",
             "against current package versions: a break can reflect version ",
             "drift, which argues for pinning versions."),
      if (sandbox == "docker") "Docker container" else "subprocess",
      timeout, nrow(run_results), plural(nrow(run_results)),
      n_ran_ok, n_errored, n_timeout, n_skipped,
      plural(n_skipped, "was", "were"), n_nodep, n_noparse,
      if (n_by_src > 0) sprintf(", and %d %s run by another script with `source()`",
                                n_by_src, plural(n_by_src, "was", "were")) else "")

    # scripts not started because the time for all scripts together ran out
    # (Lakens/tue_datamanager_check#4, item 6)
    if (n_late > 0) {
      report_exec <- paste0(report_exec, sprintf(
        " %d %s not started, because %s had run out.",
        n_late, plural(n_late, "script was", "scripts were"), .repro_deadline_text(total_timeout)))
    }

    # Scripts run only inside the script that sources them (see
    # run_by_source above): say which, and why they were not run on their own.
    report_by_source <- if (n_by_src > 0) c(sprintf(
      paste0("%d script%s %s run by another script with `source()`, so %s ",
             "not run on %s own: each script runs in a fresh R process, and on ",
             "%s own a sourced script lacks the packages and objects the ",
             "sourcing script sets up first. %s outcome is part of the ",
             "sourcing script's outcome."),
      n_by_src, plural(n_by_src), plural(n_by_src, "is", "are"),
      plural(n_by_src, "it was", "they were"), plural(n_by_src, "its", "their"),
      plural(n_by_src, "its", "their"), plural(n_by_src, "Its", "Their")),
      scroll_table(data.frame(
        File = names(run_by_source),
        `Run by` = vapply(run_by_source, paste, character(1), collapse = ", "),
        check.names = FALSE), maxrows = 10)) else NULL

    # library() calls added for a `could not find function` error, reported
    # like the setwd() repair below: what was added, and which scripts ran
    # once it was. A script that only runs with a library() call it does not
    # make itself is reproducible only after that repair.
    report_libs <- if (length(library_injections) > 0) {
      ok_after_libs <- intersect(
        names(library_injections), run_results$file_name[run_results$outcome == "ran_ok"])
      c(sprintf(
        paste0("**%d script%s use%s functions from packages %s not load ",
               "%s.** The packages are usually loaded in another script of the ",
               "project, which only helps when the scripts run in one R session ",
               "in the right order. To run %s we added the `library()` calls ",
               "below at the top. Any such script that then ran is **reproducible ",
               "only after adding `library()` calls it should make itself**."),
        length(library_injections), plural(length(library_injections)),
        plural(length(library_injections), "s", ""),
        plural(length(library_injections), "it does", "they do"),
        plural(length(library_injections), "itself", "themselves"),
        plural(length(library_injections), "it", "them")),
        if (length(ok_after_libs) > 0) sprintf(
          "Reproducible after adding `library()` calls: %s.",
          paste(ok_after_libs, collapse = ", ")),
        scroll_table(data.frame(
          File = names(library_injections),
          `library() calls added` = vapply(library_injections, function(p)
            paste(sprintf("library(%s)", p), collapse = ", "), character(1)),
          check.names = FALSE), maxrows = 10))
    } else NULL

    # Note the corrective re-run when it happened, so the outcomes are read as
    # "after re-ordering"/"after injecting library()", not the first attempt.
    if (isTRUE(attr(run_results, "reran_for_order"))) {
      reran_bits <- character(0)
      if (isTRUE(attr(run_results, "reran_for_reorder")))
        reran_bits <- c(reran_bits, paste(
          "a script hit an `object '...' not found` error, indicating it",
          "expected a variable another script defines. We inferred which",
          "script supplies it and re-ordered so that script runs first"))
      if (length(library_injections) > 0)
        reran_bits <- c(reran_bits, paste(
          "a script hit a `could not find function \"...\"` error; we added",
          "the `library()` calls it needs (see below)"))
      report_exec <- paste(report_exec, sprintf(
        "\n\n*%s, and re-ran once. The outcomes above are from that corrected run.*",
        paste(reran_bits, collapse = "; ")))
    }

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
               "anywhere else. To run the code we replaced these calls with a statement that does nothing. Any ",
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
    # "Defined in <file>" per row, from the definer_lookup built during the
    # corrective re-run step above — covers a symbol left unresolved there too
    # (0 or >1 definers, so no reorder edge was added), not just the ones that
    # got reordered. Blank when the symbol is not defined anywhere in the
    # paper's code at all (a genuine bug, not a sourcing/order gap).
    undef_defined_in <- if (nrow(undef) > 0)
      vapply(seq_len(nrow(undef)), function(i)
        definer_lookup[[paste0(undef$file_name[i], "||", undef$undefined_var[i])]] %||% "",
        character(1)) else character(0)
    report_undef <- if (nrow(undef) > 0) c(sprintf(
      paste0("**%d script%s failed on an undefined variable or function** (`object ",
             "'...' not found` / `could not find function \"...\"`). This typically ",
             "means the script expects a variable or function that another script ",
             "defines — i.e. it is meant to be sourced into a larger session, not ",
             "run on its own — or the symbol is simply never created. When another ",
             "of the paper's own script files defines the same name, this is called ",
             "out below as a likely sourcing/copy-paste gap. Each is counted as an ",
             "error (red)."),
      nrow(undef), plural(nrow(undef))),
      scroll_table(data.frame(
        File = undef$file_name,
        `Missing variable` = undef$undefined_var,
        `Defined in` = ifelse(nzchar(undef_defined_in), undef_defined_in,
                              "(not defined anywhere in the paper's code)"),
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
                if (!is.null(report_libs)) c("**`library()` calls added**", report_libs),
                if (!is.null(report_by_source)) c("**Run by another script**", report_by_source),
                if (length(output_blocks) > 0)
                  "*Expand a row below to see the output each script produced.*",
                output_blocks, truncation_note, .repro_output_cut_note(stat_cut))

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
      # A failure category (see .repro_classify_install_message()) separates
      # "genuinely gone from CRAN" (permanent) from "transient network hiccup"
      # (worth a retry) from "needs a compiler toolchain" (environment setup,
      # not a paper problem) — surfaced as a label alongside the raw message,
      # which stays the authoritative text. Absent (older saved results, or a
      # category of NA) falls back to no label, same as before this was added.
      cat_label <- function(cat) {
        nm <- c(cran_unavailable = "not available on CRAN for this R version",
               compile_failure = "compilation/configure failure",
               network = "network issue",
               transitive_dependency_missing = "a dependency of this package is unavailable",
               other = "uncategorised")
        unname(nm[cat])
      }
      failed_cat <- if ("category" %in% names(failed))
        vapply(failed$category, function(c)
          if (is.na(c)) "" else cat_label(c) %||% "", character(1)) else
        rep("", nrow(failed))
      report_fail <- if (nrow(failed) > 0) sprintf(
        "**Failed:** %s", paste(sprintf(
          "%s%s%s", failed$package,
          ifelse(nzchar(failed_cat), sprintf(" [%s]", failed_cat), ""),
          ifelse(nzchar(failed$message), sprintf(" (%s)", failed$message), "")),
          collapse = "; ")) else NULL
      report <- c(report, report_inst, report_arch, report_fail)
    }
  } else {
    report <- c(report,
      "*Executing the code is a separate, opt-in phase (run with `execute = TRUE`). This report describes what a run would involve; it did not run anything.*")
  }

  # ── 10. Summary ─────────────────────────────────────────────────────────────
  n_ran_ok <- if (!is.null(run_results)) sum(run_results$outcome == "ran_ok") else NA_integer_
  n_nodep_sum <- if (!is.null(run_results))
    sum(run_results$outcome == "dependency_unavailable") else 0L
  n_by_src_sum <- if (!is.null(run_results))
    sum(run_results$outcome == "run_by_source") else 0L
  n_run_own <- if (!is.null(run_results)) nrow(run_results) - n_by_src_sum else 0L
  summary_text <- c(
    if (isTRUE(execute)) sprintf(
      "We assessed AND ran %d R code file%s for reproducibility (each in an isolated %s).",
      n_code, plural(n_code), if (sandbox == "docker") "Docker container" else "subprocess") else sprintf(
      "We assessed %d R code file%s for reproducibility (static analysis; no code was run).",
      n_code, plural(n_code)),
    sprintf("%d file%s appear%s runnable so far (parses, inputs resolve, placeable in the run order).",
            n_runnable, plural(n_runnable), if (n_runnable == 1) "s" else ""),
    if (n_dup > 0) sprintf(
      "%d file%s skipped as byte-identical duplicate%s of another file already in the run (the paper links more than one repository/component).",
      n_dup, plural(n_dup), plural(n_dup)),
    # Scripts run only inside the script that sources them are not counted
    # here: their outcome is part of that script's.
    if (isTRUE(execute) && !is.null(run_results)) sprintf(
      "%d of %d script%s ran without error when executed%s.",
      n_ran_ok, n_run_own, plural(n_run_own),
      if (n_by_src_sum > 0) sprintf(
        " (%d more %s run by another script with `source()`)", n_by_src_sum,
        plural(n_by_src_sum, "is", "are")) else ""),
    if (n_nodep_sum > 0) sprintf(
      "%d script%s could not run because %s own dependency is unavailable (not counted against the traffic light).",
      n_nodep_sum, plural(n_nodep_sum), plural(n_nodep_sum, "its", "their")),
    if (n_missing_inputs > 0) sprintf(
      "%d referenced input%s unavailable (%d withheld due to size).",
      n_missing_inputs, plural(n_missing_inputs), n_withheld),
    sprintf("%d installable dependenc%s detected.", n_deps,
            if (n_deps == 1) "y" else "ies")
  ) |> paste("\n- ", x = _, collapse = "")

  # repro_tests_reported/repro_tests_matched are filled in after both
  # languages, from the one shared comparison with the paper.
  summary_table <- data.frame(
    paper_id             = .pid(structure_df, code_tbl),
    repro_code_n         = n_code,
    repro_runnable       = n_runnable,
    repro_missing_inputs = n_missing_inputs,
    repro_deps           = n_deps,
    repro_ran_ok         = n_ran_ok,
    repro_tests_reported = NA_integer_,
    repro_tests_matched  = NA_integer_
  )

  # Byte-identical duplicate files across repo mirrors, skipped before the run
  # (see the dedup step in section 2 above) — surfaced so a reader knows why
  # fewer scripts ran than code_check listed, rather than it looking like some
  # were silently missed.
  if (!is.null(dup_report)) report <- c(report, dup_report)

  # ── `modifications`: per-change detail, not just aggregate counts ──────────
  # Everything below was already computed above (rewrite_list at "4. Per-file
  # path rewrite + I/O", run_tbl's setwd_paths/family_detail/library_injected
  # at "1. Build the data tree..." and the corrective re-run) purely to derive
  # a COUNT (paths_rewritten, setwd_removed, ...) for `table` — the per-change
  # detail itself was discarded once the count was taken. This keeps it alive
  # through to the final return instead, one row per individual change, so a
  # reader can see WHICH path was rewritten to WHAT, WHICH setwd() path was
  # removed, WHICH font family was replaced, and WHICH library() was injected,
  # rather than only "12 paths were rewritten".
  # Path rewrites are computed unconditionally (section 4, above) since they
  # are part of the STATIC analysis — surfaced here regardless of `execute`.
  # setwd()/font/library() detail only exists once repro_write_scripts()
  # has actually written the scripts, which only happens when execute = TRUE.
  rw_rows <- dplyr::bind_rows(lapply(names(rewrite_list), function(fn) {
    d <- rewrite_list[[fn]]
    if (is.null(d) || !nrow(d)) return(NULL)
    good <- d$matched & !d$ambiguous & !is.na(d$target) & nzchar(d$target %||% "")
    if (!any(good)) return(NULL)
    data.frame(file_name = fn, change_type = "path_rewrite",
              detail = sprintf('"%s" -> "%s"', d$ref[good], d$target[good]),
              stringsAsFactors = FALSE)
  }))

  modifications <- if (isTRUE(execute)) {
    setwd_rows_mod <- if (exists("run_tbl", inherits = FALSE) &&
                          "setwd_paths" %in% names(run_tbl)) {
      hit <- run_tbl$setwd_removed > 0 & nzchar(run_tbl$setwd_paths %||% "")
      if (any(hit)) dplyr::bind_rows(lapply(which(hit), function(i)
        data.frame(file_name = run_tbl$file_name[i], change_type = "setwd_removed",
                  detail = strsplit(run_tbl$setwd_paths[i], ", ", fixed = TRUE)[[1]],
                  stringsAsFactors = FALSE)))
      else NULL
    } else NULL

    family_rows_mod <- if (exists("run_tbl", inherits = FALSE) &&
                           "family_detail" %in% names(run_tbl)) {
      hit <- run_tbl$family_replaced > 0 & nzchar(run_tbl$family_detail %||% "")
      if (any(hit)) dplyr::bind_rows(lapply(which(hit), function(i)
        data.frame(file_name = run_tbl$file_name[i], change_type = "font_replaced",
                  detail = strsplit(run_tbl$family_detail[i], "; ", fixed = TRUE)[[1]],
                  stringsAsFactors = FALSE)))
      else NULL
    } else NULL

    # One row per library() call added (a script can get several).
    library_rows_mod <- if (length(library_injections) > 0)
      data.frame(file_name = rep(names(library_injections), lengths(library_injections)),
                 change_type = "library_injected",
                 detail = sprintf("library(%s)", unlist(library_injections, use.names = FALSE))) else NULL

    dplyr::bind_rows(rw_rows, setwd_rows_mod, family_rows_mod, library_rows_mod)
  } else rw_rows
  if (is.null(modifications) || !nrow(modifications))
    modifications <- data.frame(file_name = character(0), change_type = character(0),
                                detail = character(0))

  # Minimal-diff renaming right before the merge point (explicitly permitted —
  # see this module's own development notes): every local the R block above
  # computed is handed to the shared `r_*` names the merge step below reads,
  # with NOTHING else about the block above touched.
  r_table <- table
  r_summary_table <- summary_table
  r_tl <- tl
  r_report <- report
  r_summary_text <- summary_text
  r_run_results <- run_results
  r_install_results <- install_results
  r_modifications <- modifications
  r_missing_inputs <- missing_inputs
  r_n_dup <- n_dup

  } # end if (has_r)

  if (!has_r) {
    # No R files at all (Python-only paper): empty/placeholder R-side values
    # so the merge step below has something consistent to read, mirroring
    # this module's OWN na-summary shape (repro_code_n = 0 etc., same as
    # empty()'s summary_table) rather than inventing a new convention.
    r_table <- data.frame()
    r_summary_table <- data.frame(
      paper_id = .pid(structure_df, code_tbl),
      repro_code_n = 0L, repro_runnable = 0L, repro_missing_inputs = 0L,
      repro_deps = 0L, repro_ran_ok = NA_integer_,
      repro_tests_reported = NA_integer_, repro_tests_matched = NA_integer_)
    r_tl <- "na"
    r_report <- NULL
    r_summary_text <- NULL
    r_run_results <- NULL
    r_install_results <- NULL
    r_modifications <- data.frame(file_name = character(0), change_type = character(0),
                                  detail = character(0))
    r_missing_inputs <- NULL
    r_n_dup <- 0L
  }

  ## ── Python code path ────────────────────────────────────────────────────
  # Parallel to the R path above, reusing the SAME shared, language-
  # parametrised helpers that path already calls (repro_file_io(),
  # repro_rewrite_paths(), repro_run_order(), repro_missing_inputs(),
  # repro_materialize_layout(), repro_write_scripts()), plus the Python-only
  # helpers in R/reproducibility_check_python.R (repro_dependencies_py(),
  # repro_install_deps_py(), repro_run_scripts_py(), .repro_py_defined_vars())
  # and their Docker twins in R/reproducibility_check_python_docker.R. Every
  # local here is `py_`-prefixed so nothing here can collide with (or
  # accidentally read) an R-side local from the block above, even though both
  # blocks share this one function's scope — merged with the R results at the
  # very end of this function (see "## Combine R + Python results" below).
  py_table <- data.frame()
  py_summary_table <- data.frame(
    paper_id = .pid(structure_df, code_tbl),
    repro_code_n = 0L, repro_runnable = 0L, repro_missing_inputs = 0L,
    repro_deps = 0L, repro_ran_ok = NA_integer_,
    repro_tests_reported = NA_integer_, repro_tests_matched = NA_integer_)
  py_tl <- "na"
  py_report <- NULL
  py_summary_text <- NULL
  py_run_results <- NULL
  py_install_results <- NULL
  py_stat_cut <- integer(0)
  py_modifications <- data.frame(file_name = character(0), change_type = character(0),
                                 detail = character(0))
  py_missing_inputs <- NULL
  n_py_dup <- 0L

  if (has_py) {
  n_py_code <- nrow(py_files)

  # ── Resolve each Python file to its on-disk location and read its text ───
  # Identical approach to the R path's own resolve_row_path()/hash_source()
  # (just applied to py_files as a separate pass, not merged with the R
  # pass — a cross-language byte-identical duplicate is not a real scenario
  # worth sharing dedup bookkeeping over, and keeping the two passes and
  # their report counts separate per language is what the brief calls for).
  py_loc_lookup <- if (!is.null(structure_df) &&
                       all(c("file_name", "file_location") %in% names(structure_df)))
    stats::setNames(structure_df$file_location, structure_df$file_name) else
    character(0)
  py_resolve_row_path <- function(i) {
    own <- py_files$file_location[i] %||% NA_character_
    if (!is.na(own) && nzchar(own) && file.exists(own)) return(own)
    fn <- py_files$file_name[i]
    loc <- if (fn %in% names(py_loc_lookup)) py_loc_lookup[[fn]] else NA_character_
    if (!is.na(loc) && nzchar(loc) && file.exists(loc)) return(loc)
    if (length(py_loc_lookup)) {
      m <- which(basename(names(py_loc_lookup)) == basename(fn))
      for (j in m) {
        l <- py_loc_lookup[[j]]
        if (!is.na(l) && nzchar(l) && file.exists(l)) return(l)
      }
    }
    url <- py_files$file_url[i] %||% NA_character_
    if (!is.na(url) && nzchar(url)) return(url)
    NA_character_
  }

  py_hash_source <- function(i) {
    path <- py_resolve_row_path(i)
    if (is.na(path)) return(NA_character_)
    txt <- tryCatch(code_read(path), error = function(e) NULL)
    if (is.null(txt) || !length(txt)) return(NA_character_)
    digest_txt <- paste(txt, collapse = "\n")
    unname(tools::md5sum(local({
      tf <- tempfile(); writeLines(digest_txt, tf, useBytes = TRUE); tf
    })))
  }
  py_hashes <- vapply(seq_len(nrow(py_files)), py_hash_source, character(1))
  py_dup <- !is.na(py_hashes) & duplicated(py_hashes)
  n_py_dup <- sum(py_dup)
  py_dup_report <- NULL
  if (n_py_dup > 0) {
    py_dup_kept_idx <- vapply(which(py_dup), function(i)
      which(py_hashes == py_hashes[i])[1], integer(1))
    # by its link or path in the repository, as for R above
    py_dup_loc <- .repro_shown_location(py_files, which(py_dup))
    py_kept_loc <- .repro_shown_location(py_files, py_dup_kept_idx)
    py_dup_of <- py_files$file_name[py_dup_kept_idx]
    py_dup_desc <- sprintf(
      "`%s`%s (same as `%s`%s)",
      py_files$file_name[py_dup],
      ifelse(!is.na(py_dup_loc) & nzchar(py_dup_loc), paste0(" at `", py_dup_loc, "`"), ""),
      py_dup_of,
      ifelse(!is.na(py_kept_loc) & nzchar(py_kept_loc), paste0(" at `", py_kept_loc, "`"), ""))
    py_dup_report <- c("#### Duplicate files across repo mirrors", sprintf(
      paste0(
        "%d file%s %s byte-identical to another file already in the run ",
        "(the paper links more than one repository/component that mirrors ",
        "the same materials). %s only run once, from its first occurrence, ",
        "to avoid wasted duplicate execution: %s."),
      n_py_dup, plural(n_py_dup), plural(n_py_dup, "is", "are"),
      plural(n_py_dup, "It was", "They were"),
      paste(py_dup_desc, collapse = "; ")))
    py_files <- py_files[!py_dup, , drop = FALSE]
    n_py_code <- nrow(py_files)
  }

  # A notebook is JSON and a Quarto document is Markdown: their Python code is
  # extracted first, as code_check does (and as the R path purls an
  # .Rmd/.qmd), or `python` would be run on the JSON/Markdown itself.
  py_code_text_list <- lapply(seq_len(n_py_code), function(i) {
    path <- py_resolve_row_path(i)
    if (is.na(path)) return(character(0))
    fn <- py_files$file_name[i]
    tryCatch(
      if (grepl("\\.ipynb$", fn, ignore.case = TRUE)) code_extract_py(path)
      else if (grepl("\\.qmd$", fn, ignore.case = TRUE)) code_extract_qmd_py(path)
      else code_read(path),
      error = function(e) character(0))
  })
  names(py_code_text_list) <- py_files$file_name

  # ── Dependencies (pooled across files) ────────────────────────────────────
  # requirements.txt / pyproject.toml, if the repository has one: preferred
  # in that order when BOTH exist (judgment call — a requirements.txt is the
  # more directly actionable/installable of the two for pip, and is the more
  # common convention in a corpus of analysis scripts rather than installable
  # packages). Matched by basename (case-insensitive) against structure_df's
  # own file list, the same file-location resolution approach as the R path's
  # own loc_lookup (structure_df first; code_tbl file_location as a fallback
  # for a manifest that only code_check happened to pick up).
  py_manifest_text <- NULL
  if (!is.null(structure_df) && "file_name" %in% names(structure_df)) {
    manifest_hit <- which(grepl("(^|/)requirements\\.txt$", structure_df$file_name,
                                ignore.case = TRUE))
    if (!length(manifest_hit))
      manifest_hit <- which(grepl("(^|/)pyproject\\.toml$", structure_df$file_name,
                                  ignore.case = TRUE))
    if (length(manifest_hit)) {
      manifest_loc <- structure_df$file_location[manifest_hit[1]]
      if (!is.na(manifest_loc) && nzchar(manifest_loc) && file.exists(manifest_loc))
        py_manifest_text <- tryCatch(code_read(manifest_loc), error = function(e) NULL)
    }
  }
  py_deps <- repro_dependencies_py(py_code_text_list, manifest_text = py_manifest_text)
  py_install_deps <- py_deps[!py_deps$base, , drop = FALSE]
  # The repository's own modules (`from utils import helpers`, with
  # utils/helpers.py in the repository) are not packages: nothing to install,
  # and a failed import of one is an error in the run, not a missing package
  py_local_paths <- py_files$file_path %||% py_files$file_name
  py_local_paths <- gsub("\\\\", "/", ifelse(is.na(py_local_paths) | !nzchar(py_local_paths),
                                             py_files$file_name, py_local_paths))
  py_local_modules <- unique(c(tools::file_path_sans_ext(basename(py_local_paths)),
                               unlist(strsplit(dirname(py_local_paths), "/", fixed = TRUE))))
  py_install_deps <- py_install_deps[!py_install_deps$package %in% py_local_modules, , drop = FALSE]
  n_py_deps <- nrow(py_install_deps)

  # ── Per-file path rewrite + I/O for ordering ──────────────────────────────
  py_io <- repro_file_io(py_code_text_list, "Python")

  py_rewrite_list <- lapply(seq_len(n_py_code), function(i)
    repro_rewrite_paths(py_code_text_list[[i]], py_files$file_name[i], plan, "Python",
                       structure_df = structure_df))
  names(py_rewrite_list) <- py_files$file_name
  if (layout == "package") py_rewrite_list <- stats::setNames(lapply(seq_along(py_rewrite_list), function(i)
    .repro_keep_working_refs(py_rewrite_list[[i]], plan, py_files$file_path[i])), names(py_rewrite_list))

  py_rewrites_n <- vapply(py_rewrite_list, function(d)
    if (nrow(d)) sum(d$matched & !d$ambiguous & !(d$as_written %||% FALSE)) else 0L, integer(1))
  py_ambiguous_n <- vapply(py_rewrite_list, function(d)
    if (nrow(d)) sum(d$ambiguous) else 0L, integer(1))
  py_unresolved_refs <- unlist(lapply(py_rewrite_list, function(d)
    if (nrow(d)) d$basename[!d$matched] else character(0)), use.names = FALSE)

  # ── Run order ──────────────────────────────────────────────────────────
  py_order_tbl <- repro_run_order(py_io)
  py_cycle <- attr(py_order_tbl, "cycle") %||% character(0)
  py_ambiguous_order <- isTRUE(attr(py_order_tbl, "ambiguous"))
  py_fuzzy_sources <- attr(py_order_tbl, "fuzzy_sources") %||%
    data.frame(from = character(0), to = character(0))

  # ── Missing-input diagnosis ───────────────────────────────────────────────
  py_produced <- unique(tolower(unlist(py_io$writes %||% list(), use.names = FALSE)))
  py_candidate_missing <- setdiff(unique(tolower(py_unresolved_refs)), py_produced)
  py_missing_inputs <- repro_missing_inputs(py_candidate_missing, plan, structure_df)
  n_py_missing_inputs <- nrow(py_missing_inputs)
  n_py_withheld <- sum(py_missing_inputs$status == "withheld_size")

  # code_check only checks R files for syntax errors, so Python files are
  # checked here, with Python's own parser (.repro_py_parses(): reads the code
  # without running it). It needs a Python interpreter on this machine; with
  # none (e.g. sandbox = "docker" on a machine without Python), every file
  # counts as parsing and a syntax error only shows when the code is run.
  py_parse_bin <- .repro_find_python()
  py_parses <- unname(.repro_py_parses(py_code_text_list))
  n_py_noparse_static <- sum(!py_parses)
  # Set when the execute phase below could not run the Python code at all.
  py_exec_error <- NULL
  py_file_order <- py_order_tbl$order[match(py_files$file_name, py_order_tbl$file_name)]
  py_placeable <- !is.na(py_file_order)

  py_writes_by_file <- py_io$writes %||% list()
  names(py_writes_by_file) <- py_io$file_name %||% names(py_writes_by_file)
  # a file the script writes itself counts too: open(path, "w") names its path
  # like a read, so the script that creates the file was "missing_input" (U235)
  py_produced_before <- lapply(seq_len(n_py_code), function(i) {
    own <- unlist(py_writes_by_file[py_files$file_name[i]], use.names = FALSE)
    ord_i <- py_file_order[i]
    earlier <- if (is.na(ord_i)) character(0) else
      py_files$file_name[!is.na(py_file_order) & py_file_order < ord_i]
    unique(tolower(c(own, unlist(py_writes_by_file[earlier], use.names = FALSE))))
  })

  py_unresolved_list <- lapply(seq_len(n_py_code), function(i) {
    d <- py_rewrite_list[[i]]
    if (!nrow(d)) return(character(0))
    unique(tolower(d$basename)[!d$matched &
                                 !(tolower(d$basename) %in% py_produced_before[[i]])])
  })
  py_file_unresolved <- lengths(py_unresolved_list) > 0L
  py_runnable <- py_parses & py_placeable & !py_file_unresolved
  n_py_runnable <- sum(py_runnable)

  py_not_runnable_reason <- ifelse(
    py_runnable, NA_character_,
    ifelse(!py_parses, "parse_error",
           ifelse(!py_placeable, "unplaceable", "missing_input")))
  py_unresolved_inputs <- vapply(py_unresolved_list, function(x)
    if (length(x)) paste(x, collapse = ", ") else NA_character_, character(1))

  # ── Traffic light (static part) ───────────────────────────────────────────
  # Same priority structure as the R path's own static tl (section 7 above).
  py_tl <- if (n_py_missing_inputs > 0 && n_py_withheld == n_py_missing_inputs) "red"
           else if (n_py_noparse_static == 0 && n_py_missing_inputs == 0 &&
                    length(py_cycle) == 0 && !py_ambiguous_order &&
                    sum(py_ambiguous_n) == 0) "green"
           else "yellow"

  ## ── Execution (opt-in; runs downloaded code) ────────────────────────────
  if (isTRUE(execute)) {
    .dbg("[repro/py] execute = TRUE (sandbox = ", sandbox, "). install_missing = ",
         install_missing, ", timeout = ", timeout, "s, keep_sandbox = ", keep_sandbox)

    # Reuse the SAME sandbox_root the R path (and/or the JASP/jamovi step)
    # already materialised, if any — one shared tree, not two. This is the
    # reason repro_materialize_layout() is NOT called a second time here:
    # it operates over the WHOLE plan/structure_df, language-agnostically,
    # so a second call would just redundantly re-copy the same data files.
    if (is.null(sandbox_root)) {
      sandbox_root <- tempfile("repro_sandbox_")
      if (!isTRUE(keep_sandbox))
        on.exit(unlink(sandbox_root, recursive = TRUE), add = TRUE)
      repro_materialize_layout(plan, structure_df, sandbox_root)
    } else if (!has_r) {
      # has_r's own materialise call never ran (R section skipped entirely),
      # so a Python-only paper still needs the layout actually built into
      # whatever sandbox_root the JASP/jamovi stat_output step above created
      # (that step only writes statistical_output/, never the data/ tree).
      repro_materialize_layout(plan, structure_df, sandbox_root)
    }
    py_lib_dir <- file.path(sandbox_root, "_pylib")

    .dbg("[repro/py] writing ", n_py_code, " rewritten script(s) into the layout ...")
    py_run_tbl <- repro_write_scripts(py_code_text_list, py_rewrite_list, plan,
                                      sandbox_root, lang = "Python",
                                      script_paths = if (layout == "package") py_files$file_path,
                                      writes_in_place = layout == "package")
    .dbg("[repro/py]   wrote ", nrow(py_run_tbl), " script(s).")

    # The process sandbox needs a Python interpreter on this computer (looked
    # up above as py_parse_bin: `python`, else `python3`); sandbox = "docker"
    # uses the image's own. Without one, or when installing or running stops
    # with an error, the Python files are recorded as not run and the R
    # results are still returned, instead of the whole module stopping.
    if (sandbox != "docker" && !nzchar(py_parse_bin))
      py_exec_error <- paste0(.repro_python_missing_reason(), ".")

    py_failed_deps <- character(0)
    if (is.null(py_exec_error) && isTRUE(install_missing) && n_py_deps > 0) {
      .dbg("[repro/py] installing ", n_py_deps, " dependenc(y/ies): ",
           paste(py_install_deps$package, collapse = ", "))
      py_install_results <- tryCatch(
        if (sandbox == "docker") {
          repro_install_deps_py_docker(py_install_deps, py_lib_dir, timeout = timeout)
        } else {
          repro_install_deps_py(py_install_deps, py_lib_dir, python_bin = py_parse_bin,
                                timeout = timeout)
        },
        error = function(e) {
          py_exec_error <<- conditionMessage(e)
          NULL
        })
      if (!is.null(py_install_results)) {
        .dbg("[repro/py]   install done: ", sum(py_install_results$installed), " ok, ",
             sum(!py_install_results$installed), " failed.")
        py_failed_deps <- py_install_results$package[!py_install_results$installed]
      }
    } else {
      .dbg("[repro/py] install_missing FALSE or no deps; skipping installs.")
      # as for R: nothing installed, so a missing package means the script
      # could not run here
      if (!isTRUE(install_missing)) py_failed_deps <- py_install_deps$package
    }

    py_parses_named <- stats::setNames(py_parses, py_files$file_name)
    py_skip_files <- py_files$file_name[py_file_unresolved]
    py_run_order_names <- py_order_tbl$file_name[order(py_order_tbl$order)]
    # A module other files import is not run on its own (see
    # .repro_py_imported()), like an R script another script source()s.
    py_imported <- .repro_py_imported(py_code_text_list, py_files$file_path)
    if (length(py_imported))
      .dbg("[repro/py] imported by other files, not run on their own: ",
           paste(names(py_imported), collapse = ", "))
    py_run_order_names <- setdiff(py_run_order_names, names(py_imported))
    if (is.null(py_exec_error)) {
      if (is.null(script_deadline)) script_deadline <- .repro_deadline(total_timeout)
      .dbg("[repro/py] running ", length(py_run_order_names), " script(s) in order: ",
           paste(py_run_order_names, collapse = " -> "))
      py_run_results <- tryCatch(
        if (sandbox == "docker") {
          repro_run_scripts_py_docker(
            py_run_tbl, py_run_order_names, sandbox_root = sandbox_root,
            lib_dir = if (dir.exists(py_lib_dir)) py_lib_dir else NULL,
            timeout = timeout, skip = py_skip_files, parses = py_parses_named,
            failed_deps = py_failed_deps, deadline = script_deadline)
        } else {
          repro_run_scripts_py(
            py_run_tbl, py_run_order_names,
            lib_dir = if (dir.exists(py_lib_dir)) py_lib_dir else NULL,
            python_bin = py_parse_bin,
            timeout = timeout, skip = py_skip_files, parses = py_parses_named,
            failed_deps = py_failed_deps, deadline = script_deadline)
        },
        error = function(e) {
          py_exec_error <<- conditionMessage(e)
          NULL
        })
    }
    if (!is.null(py_run_results) && length(py_imported)) {
      py_run_results <- py_run_results[!py_run_results$file_name %in% names(py_imported), , drop = FALSE]
      py_run_results <- dplyr::bind_rows(py_run_results, data.frame(
        file_name = names(py_imported), outcome = "run_by_import",
        error = "", stdout = "", stderr = ""))
    }
    if (!is.null(py_run_results))
      .dbg("[repro/py] execution finished. outcomes: ",
           paste(sprintf("%s=%s", py_run_results$file_name, py_run_results$outcome),
                 collapse = "; "))
    else
      .dbg("[repro/py] Python code not run: ", py_exec_error)

    # A NameError usually means the script expects a name another of the
    # paper's scripts defines. Unlike the R path, there is no re-run in a
    # corrected order: every Python script runs in its own `python` process,
    # so a name another script defines is never available to it, whatever the
    # order. The report names the script that defines the name instead.
    py_definer_lookup <- list()
    py_undef_err <- if (!is.null(py_run_results))
      py_run_results[py_run_results$error_type %in% "undefined_variable", , drop = FALSE]
    if (NROW(py_undef_err) > 0) {
      py_defs <- .repro_py_defined_vars(py_code_text_list)
      for (i in seq_len(nrow(py_undef_err))) {
        user <- py_undef_err$file_name[i]; v <- py_undef_err$undefined_var[i]
        hit <- vapply(py_defs$defines, function(dd) v %in% dd, logical(1))
        definers <- setdiff(py_defs$file_name[hit], user)
        if (length(definers) > 0)
          py_definer_lookup[[paste0(user, "||", v)]] <- paste(definers, collapse = ", ")
      }
    }

    if (any(py_run_results$outcome %in% c("errored", "timed_out"))) py_tl <- "red"
    else if (any(py_run_results$outcome == "skipped_time_limit") && py_tl == "green") py_tl <- "yellow"

    # ── Statistical output extraction from Python stdout ─────────────────────
    # Mirrors the R path's own r_stat_output block (see "Statistical output
    # matching mechanism" in this module's own development brief), but
    # TEXT-PARSED ONLY: Python has no object-capture analogue (`captures` is
    # always NULL per row from both Python run-script backends), so there is
    # no .r_merge_captures() call here — just .ipynb_stat_line()/
    # .ipynb_stat_table() directly against each script's captured stdout.
    # Deliberately NO raw-text fallback when neither parses (consistent with
    # read_r_output()'s own behaviour on the R side — see R/r-output.R: it
    # returns list() for unparseable console output, never a raw-text
    # column, so the Python path does the same rather than silently
    # diverging from what the R path already does).
    py_stat_output <- lapply(seq_len(NROW(py_run_results)), function(i) {
      so <- py_run_results$stdout[i]
      fn <- py_run_results$file_name[i]
      if (is.null(so) || !nzchar(so)) return(NULL)
      lines <- strsplit(so, "\n", fixed = TRUE)[[1]]
      .dbg("[repro/py] parsing output of '", fn, "' (", length(lines), " lines) ...")
      lines <- .repro_output_head(lines)
      if (!is.null(attr(lines, "n_lines"))) py_stat_cut[[fn]] <<- attr(lines, "n_lines")
      lines <- lines[nzchar(trimws(lines))]
      if (!length(lines) || .ipynb_is_noise(lines)) return(NULL)

      tabs <- .ipynb_stat_line(lines)
      if (is.null(tabs)) tabs <- .ipynb_stat_table(lines)
      if (is.null(tabs)) return(NULL)

      list(file = fn, n_tables = length(tabs), source = "python_output",
           json = stat_output_json(tabs, paper_id = .pid(structure_df, code_tbl),
                                  source_file = fn),
           long = stat_results_long(tabs, paper_id = .pid(structure_df, code_tbl),
                                    source_file = fn))
    })
    py_stat_output <- Filter(Negate(is.null), py_stat_output)
    if (length(py_stat_output) > 0) {
      stat_output <- c(stat_output %||% list(), py_stat_output)
      .dbg("[repro/py] extracted statistical output from ", length(py_stat_output),
           " script(s)' console output.")
      if (is.null(sandbox_root)) sandbox_root <- tempfile("repro_sandbox_")
      stat_output_write(stat_output, sandbox_root)
    }
  }

  ## ── Per-file table ────────────────────────────────────────────────────
  py_ord_of <- py_order_tbl$order[match(py_files$file_name, py_order_tbl$file_name)]
  py_basis_of <- py_order_tbl$order_basis[match(py_files$file_name, py_order_tbl$file_name)]
  py_dep_of <- py_order_tbl$depends_on[match(py_files$file_name, py_order_tbl$file_name)]

  py_match_run <- if (!is.null(py_run_results))
    match(py_files$file_name, py_run_results$file_name) else rep(NA_integer_, n_py_code)
  py_outcome_of <- if (!is.null(py_run_results)) py_run_results$outcome[py_match_run] else
    rep(NA_character_, n_py_code)
  py_errtype_of <- if (!is.null(py_run_results)) py_run_results$error_type[py_match_run] else
    rep(NA_character_, n_py_code)
  py_undefvar_of <- if (!is.null(py_run_results)) py_run_results$undefined_var[py_match_run] else
    rep(NA_character_, n_py_code)
  py_setwd_of <- if (exists("py_run_tbl", inherits = FALSE) &&
                     "setwd_removed" %in% names(py_run_tbl))
    py_run_tbl$setwd_removed[match(py_files$file_name, py_run_tbl$file_name)] else
    rep(NA_integer_, n_py_code)

  py_match_io <- match(py_files$file_name, py_io$file_name)
  py_reads_of <- if ("reads" %in% names(py_io)) py_io$reads[py_match_io] else
    rep(list(character(0)), n_py_code)
  py_writes_of <- if ("writes" %in% names(py_io)) py_io$writes[py_match_io] else
    rep(list(character(0)), n_py_code)
  py_reads_of[is.na(py_match_io)] <- list(character(0))
  py_writes_of[is.na(py_match_io)] <- list(character(0))

  py_table <- data.frame(
    paper_id        = if ("paper_id" %in% names(py_files)) py_files$paper_id else .pid(),
    file_name       = py_files$file_name,
    parses          = py_parses,
    run_order       = py_ord_of,
    order_basis     = py_basis_of,
    depends_on      = py_dep_of,
    paths_rewritten = py_rewrites_n,
    paths_ambiguous = py_ambiguous_n,
    setwd_removed   = py_setwd_of,
    runnable        = py_runnable,
    not_runnable_reason = py_not_runnable_reason,
    unresolved_inputs   = py_unresolved_inputs,
    outcome         = py_outcome_of,
    error_type      = py_errtype_of,
    undefined_var   = py_undefvar_of
  )
  py_table$reads  <- py_reads_of
  py_table$writes <- py_writes_of

  ## ── Report ────────────────────────────────────────────────────────────
  py_intro <- if (isTRUE(execute))
    paste0("This module also assesses whether the paper's Python code could be ",
           "run on its data, and — because `execute = TRUE` — **actually ran ",
           "it**. We examined %d Python code file%s.") else
    paste0("This module also assesses whether the paper's Python code could be ",
           "run on its data. It performs static analysis only here: **no code ",
           "is run**. We examined %d Python code file%s.")
  py_report <- c(sprintf(py_intro, n_py_code, plural(n_py_code)))

  py_ordered <- py_order_tbl[!is.na(py_order_tbl$order), , drop = FALSE]
  py_ordered <- py_ordered[order(py_ordered$order), , drop = FALSE]
  py_order_table <- data.frame(
    Order = py_ordered$order,
    `File` = py_ordered$file_name,
    `Runs after` = ifelse(nzchar(py_ordered$depends_on), py_ordered$depends_on, "—"),
    Basis = py_ordered$order_basis,
    check.names = FALSE
  )
  py_report_order <- if (py_ambiguous_order) {
    "No ordering signal (no data dependency, `source()`-equivalent, or numeric filename prefix) distinguishes the run order of these files. They may need to run in a specific order that could not be determined automatically — check this before running."
  } else if (length(py_cycle) > 0) {
    sprintf("A dependency **cycle** was detected among %d file%s (%s): each reads a file another writes, so no run order satisfies all of them. This must be resolved before the code can run.",
            length(py_cycle), plural(length(py_cycle)), paste(py_cycle, collapse = ", "))
  } else {
    "The scripts were ordered by their data dependencies (a script writing a file another reads runs first) and numeric filename prefixes. Python has no `source()`-equivalent import-based ordering signal (an `import` loads a module, not the paper's own analysis flow), so that basis never applies here."
  }

  py_total_rewrites <- sum(py_rewrites_n)
  py_total_ambiguous <- sum(py_ambiguous_n)
  py_report_paths <- if (layout == "package") sprintf(
    "To run against the package, %d referenced data path%s that do%s not work as written would be rewritten to where the file is in the package.",
    py_total_rewrites, plural(py_total_rewrites), plural(py_total_rewrites, "es", "")) else sprintf(
    "To run against the Psych-DS layout, %d referenced data path%s would be rewritten to %s new location%s.",
    py_total_rewrites, plural(py_total_rewrites),
    plural(py_total_rewrites, "its", "their"), plural(py_total_rewrites))
  if (py_total_ambiguous > 0)
    py_report_paths <- paste(py_report_paths, sprintf(
      "%d reference%s matched several files sharing a name and could not be resolved by study group; %s left unrewritten and flagged.",
      py_total_ambiguous, plural(py_total_ambiguous),
      plural(py_total_ambiguous, "it was", "they were")))

  if (n_py_missing_inputs == 0) {
    py_report_missing <- "Every data file the code reads is either produced by an earlier script or present in the repository."
    py_missing_table <- NULL
  } else {
    py_report_missing <- sprintf(
      "%d input file%s the code reads %s not available. Files withheld only because of their size are distinguished from files absent from the repository — the former are a size-cap issue, not a reproducibility failure.",
      n_py_missing_inputs, plural(n_py_missing_inputs),
      plural(n_py_missing_inputs, "is", "are"))
    py_missing_table <- data.frame(
      File   = py_missing_inputs$basename,
      Status = py_missing_inputs$status,
      Reason = py_missing_inputs$detail
    )
  }

  py_report <- c(
    py_report,
    "#### Run order", py_report_order, scroll_table(py_order_table, maxrows = 10),
    "#### File paths", py_report_paths,
    "#### Missing inputs", py_report_missing,
    if (!is.null(py_missing_table)) scroll_table(py_missing_table, maxrows = 10),
    # Same position as the R path's own "#### Parsing" subsection. Also shown
    # when the check could not be done, so a reader does not take a missing
    # section to mean every file parsed.
    if (!nzchar(py_parse_bin)) c("#### Parsing",
      sprintf("The Python files were not checked for syntax errors, because %s; a syntax error will only show if the code is run.",
              .repro_python_missing_reason()))
    else if (n_py_noparse_static > 0) c("#### Parsing", sprintf(
      "%d Python file%s did not parse and cannot be run (Python's own parser found a syntax error; code written for Python 2 also fails this check): %s.",
      n_py_noparse_static, plural(n_py_noparse_static),
      paste(py_files$file_name[!py_parses], collapse = ", ")))
  )

  if (!is.null(py_run_results) && nrow(py_run_results) > 0) {
    py_oc <- py_run_results$outcome
    n_py_ran_ok  <- sum(py_oc == "ran_ok")
    n_py_errored <- sum(py_oc == "errored")
    n_py_timeout <- sum(py_oc == "timed_out")
    n_py_skipped <- sum(py_oc == "skipped_missing_inputs")
    n_py_noparse <- sum(py_oc == "not_parsed")
    n_py_nodep   <- sum(py_oc == "dependency_unavailable")
    n_py_import  <- sum(py_oc == "run_by_import")
    n_late       <- sum(py_oc == "skipped_time_limit")
    py_report_exec <- sprintf(
      paste0("**The Python code was run.** Each script ran in an isolated ",
             "%s against a throwaway copy of %s, in the run ",
             "order above, with a %d-second per-script timeout. Of %d script%s: ",
             "%d ran without error, %d errored, %d timed out, %d %s skipped ",
             "(inputs unavailable), %d could not run because one of its own ",
             "dependencies is unavailable, and %d did not parse.%s Running ",
             "against current package versions: a break can reflect version ",
             "drift, which argues for pinning versions (e.g. in a ",
             "`requirements.txt`)."),
      if (sandbox == "docker") "Docker container" else "subprocess",
      layout_name, timeout, nrow(py_run_results), plural(nrow(py_run_results)),
      n_py_ran_ok, n_py_errored, n_py_timeout, n_py_skipped,
      plural(n_py_skipped, "was", "were"), n_py_nodep, n_py_noparse,
      if (n_py_import) sprintf(
        " %d %s not run on %s own, because other files import %s: %s.",
        n_py_import, plural(n_py_import, "file was", "files were"),
        plural(n_py_import, "its", "their"), plural(n_py_import, "it", "them"),
        paste(sprintf("%s (by %s)", names(py_imported),
                      vapply(py_imported, paste, character(1), collapse = ", ")),
              collapse = "; ")) else "")

    # scripts not started because the time for all scripts together ran out
    # (Lakens/tue_datamanager_check#4, item 6)
    if (n_late > 0) {
      py_report_exec <- paste0(py_report_exec, sprintf(
        " %d %s not started, because %s had run out.",
        n_late, plural(n_late, "script was", "scripts were"), .repro_deadline_text(total_timeout)))
    }

    py_setwd_rows <- if (exists("py_run_tbl", inherits = FALSE) &&
                         "setwd_removed" %in% names(py_run_tbl))
      py_run_tbl[py_run_tbl$setwd_removed > 0, , drop = FALSE] else py_run_tbl[0, ]
    py_report_setwd <- if (!is.null(py_setwd_rows) && nrow(py_setwd_rows) > 0) {
      py_ok_after <- intersect(
        py_setwd_rows$file_name, py_run_results$file_name[py_run_results$outcome == "ran_ok"])
      c(sprintf(
        paste0("**%d script%s call%s `os.chdir()`.** This is bad practice: it ",
               "hardcodes where the code must run (often an absolute path on ",
               "the author's own machine) and breaks the moment the code is ",
               "run anywhere else. To run the code we replaced these calls ",
               "with `pass`. Any such script that then ran is **reproducible only ",
               "after ignoring an `os.chdir()` that should not have been in ",
               "the code and needs to be fixed**."),
        nrow(py_setwd_rows), plural(nrow(py_setwd_rows)),
        plural(nrow(py_setwd_rows), "s", "")),
        if (length(py_ok_after) > 0) sprintf(
          "Reproducible after removing `os.chdir()`: %s.",
          paste(py_ok_after, collapse = ", ")),
        scroll_table(data.frame(
          File = py_setwd_rows$file_name,
          `os.chdir() removed` = py_setwd_rows$setwd_removed,
          `Path(s) it set` = ifelse(nzchar(py_setwd_rows$setwd_paths),
                                    py_setwd_rows$setwd_paths, "(non-literal)"),
          check.names = FALSE), maxrows = 10))
    } else NULL

    py_undef <- py_run_results[!is.na(py_run_results$error_type) &
                               py_run_results$error_type == "undefined_variable", ,
                               drop = FALSE]
    py_undef_defined_in <- if (nrow(py_undef) > 0)
      vapply(seq_len(nrow(py_undef)), function(i)
        py_definer_lookup[[paste0(py_undef$file_name[i], "||", py_undef$undefined_var[i])]] %||% "",
        character(1)) else character(0)
    py_report_undef <- if (nrow(py_undef) > 0) c(sprintf(
      paste0("**%d script%s failed on an undefined name** (`NameError`). This ",
             "typically means the script expects a variable, function, or ",
             "class that another script defines — i.e. it is meant to be run ",
             "alongside a larger set of scripts, not on its own — or the name ",
             "is simply never created. When another of the paper's own ",
             "script files defines the same name, this is called out below as ",
             "a likely ordering/copy-paste gap. Each is counted as an error ",
             "(red)."),
      nrow(py_undef), plural(nrow(py_undef))),
      scroll_table(data.frame(
        File = py_undef$file_name,
        `Missing name` = py_undef$undefined_var,
        `Defined in` = ifelse(nzchar(py_undef_defined_in), py_undef_defined_in,
                              "(not defined anywhere in the paper's code)"),
        check.names = FALSE), maxrows = 10)) else NULL

    py_nodep <- py_run_results[!is.na(py_run_results$error_type) &
                               py_run_results$error_type == "dependency_unavailable", ,
                               drop = FALSE]
    py_report_nodep <- if (nrow(py_nodep) > 0) c(sprintf(
      paste0("**%d script%s could not run because %s own dependency is ",
             "unavailable** — the package failed to `pip install`. This is an ",
             "infrastructure limitation, not a defect in the paper's code, so ",
             "it does **not** force the traffic light red."),
      nrow(py_nodep), plural(nrow(py_nodep)), plural(nrow(py_nodep), "its", "their")),
      scroll_table(data.frame(File = py_nodep$file_name, check.names = FALSE),
                  maxrows = 10)) else NULL

    py_exec_table <- data.frame(
      File    = py_run_results$file_name,
      Outcome = py_run_results$outcome,
      Detail  = ifelse(!is.na(py_run_results$error_type), py_run_results$error_type, ""),
      `Time (s)` = round(py_run_results$elapsed, 1),
      check.names = FALSE)

    py_max_lines <- 5000L
    py_truncated <- FALSE
    py_tail_cap <- function(txt) {
      if (is.null(txt) || !nzchar(txt)) return(character(0))
      lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
      if (length(lines) > py_max_lines) {
        py_truncated <<- TRUE
        lines <- c(sprintf("... [%d earlier lines omitted — see full output in the result object] ...",
                           length(lines) - py_max_lines),
                   utils::tail(lines, py_max_lines))
      }
      lines
    }
    py_output_blocks <- unlist(lapply(seq_len(nrow(py_run_results)), function(i) {
      so <- py_tail_cap(py_run_results$stdout[i])
      se <- py_tail_cap(py_run_results$stderr[i])
      if (length(so) == 0 && length(se) == 0 &&
          !nzchar(py_run_results$error[i] %||% "")) return(NULL)
      body <- character(0)
      if (length(so) > 0) body <- c(body, "**Output (stdout):**", "````", so, "````")
      if (length(se) > 0) body <- c(body, "**Messages / errors (stderr):**", "````", se, "````")
      if (length(body) == 0 && nzchar(py_run_results$error[i] %||% ""))
        body <- c("````", py_run_results$error[i], "````")
      collapse_section(
        paste(body, collapse = "\n"),
        title = sprintf("Output — %s (%s)",
                        py_run_results$file_name[i], py_run_results$outcome[i]))
    }), use.names = FALSE)

    py_truncation_note <- if (isTRUE(py_truncated)) sprintf(
      paste0("*Output shown here is capped at the last %d lines per stream to ",
             "keep the report readable. The complete, untruncated output of every ",
             "script is stored in the module result: ",
             "`reproducibility_check_output$run_results$stdout` (and `$stderr`), ",
             "one row per script.*"),
      py_max_lines) else NULL

    py_report <- c(py_report, "#### Execution", py_report_exec,
                   scroll_table(py_exec_table, maxrows = 15),
                   if (!is.null(py_report_undef)) c("**Undefined-name errors**", py_report_undef),
                   if (!is.null(py_report_nodep)) c("**Dependency-unavailable errors**", py_report_nodep),
                   if (!is.null(py_report_setwd)) c("**`os.chdir()` in the code**", py_report_setwd),
                   if (length(py_output_blocks) > 0)
                     "*Expand a row below to see the output each script produced.*",
                   py_output_blocks, py_truncation_note, .repro_output_cut_note(py_stat_cut))

    if (!is.null(py_install_results) && nrow(py_install_results) > 0) {
      n_py_ok <- sum(py_install_results$installed)
      n_py_fail <- sum(!py_install_results$installed)
      py_report_inst <- sprintf(
        "Dependencies were installed into a throwaway library before running (`pip install`): %d succeeded, %d failed. %s were installed.",
        n_py_ok, n_py_fail, paste(py_install_results$package, collapse = ", "))
      py_failed_rows <- py_install_results[!py_install_results$installed, , drop = FALSE]
      py_report_fail <- if (nrow(py_failed_rows) > 0) sprintf(
        "**Failed:** %s", paste(sprintf(
          "%s%s", py_failed_rows$package,
          ifelse(nzchar(py_failed_rows$message), sprintf(" (%s)", py_failed_rows$message), "")),
          collapse = "; ")) else NULL
      py_report <- c(py_report, py_report_inst, py_report_fail)
    }
  } else if (!is.null(py_exec_error)) {
    py_report <- c(py_report, "#### Execution", paste(
      "**The Python code was not run.** This is a limitation of the computer",
      "running this check, not of the paper's code:", py_exec_error))
  } else {
    py_report <- c(py_report,
      "*Executing the code is a separate, opt-in phase (run with `execute = TRUE`). This report describes what a run would involve; it did not run anything.*")
  }

  ## ── Summary ────────────────────────────────────────────────────────────
  n_py_ran_ok_sum <- if (!is.null(py_run_results)) sum(py_run_results$outcome == "ran_ok") else NA_integer_
  py_summary_text <- c(
    if (isTRUE(execute) && !is.null(py_exec_error)) sprintf(
      "We assessed %d Python code file%s for reproducibility, but could not run %s: %s",
      n_py_code, plural(n_py_code), plural(n_py_code, "it", "them"), py_exec_error)
    else if (isTRUE(execute)) sprintf(
      "We assessed AND ran %d Python code file%s for reproducibility.",
      n_py_code, plural(n_py_code)) else sprintf(
      "We assessed %d Python code file%s for reproducibility (static analysis; no code was run).",
      n_py_code, plural(n_py_code)),
    sprintf("%d Python file%s appear%s runnable so far (parses, inputs resolve, placeable in the run order).",
            n_py_runnable, plural(n_py_runnable), if (n_py_runnable == 1) "s" else ""),
    if (n_py_dup > 0) sprintf(
      "%d Python file%s skipped as byte-identical duplicate%s of another file already in the run.",
      n_py_dup, plural(n_py_dup), plural(n_py_dup)),
    # Files other files import are not counted, as R's sourced scripts are not.
    if (isTRUE(execute) && !is.null(py_run_results)) {
      n_py_imp <- sum(py_run_results$outcome == "run_by_import")
      n_py_own <- nrow(py_run_results) - n_py_imp
      sprintf("%d of %d Python script%s ran without error when executed%s.",
              n_py_ran_ok_sum, n_py_own, plural(n_py_own),
              if (n_py_imp > 0) sprintf(" (%d more %s imported by other files, not run on %s own)",
                                        n_py_imp, plural(n_py_imp, "file is", "files are"),
                                        plural(n_py_imp, "its", "their")) else "")
    },
    if (n_py_missing_inputs > 0) sprintf(
      "%d referenced Python input%s unavailable (%d withheld due to size).",
      n_py_missing_inputs, plural(n_py_missing_inputs), n_py_withheld),
    sprintf("%d installable Python dependenc%s detected.", n_py_deps,
            if (n_py_deps == 1) "y" else "ies")
  ) |> paste("\n- ", x = _, collapse = "")

  py_summary_table <- data.frame(
    paper_id             = .pid(structure_df, code_tbl),
    repro_code_n         = n_py_code,
    repro_runnable       = n_py_runnable,
    repro_missing_inputs = n_py_missing_inputs,
    repro_deps           = n_py_deps,
    repro_ran_ok         = n_py_ran_ok_sum,
    repro_tests_reported = NA_integer_,
    repro_tests_matched  = NA_integer_
  )

  if (!is.null(py_dup_report)) py_report <- c(py_report, py_dup_report)

  py_rw_rows <- dplyr::bind_rows(lapply(names(py_rewrite_list), function(fn) {
    d <- py_rewrite_list[[fn]]
    if (is.null(d) || !nrow(d)) return(NULL)
    good <- d$matched & !d$ambiguous & !is.na(d$target) & nzchar(d$target %||% "")
    if (!any(good)) return(NULL)
    data.frame(file_name = fn, change_type = "path_rewrite",
              detail = sprintf('"%s" -> "%s"', d$ref[good], d$target[good]),
              stringsAsFactors = FALSE)
  }))

  py_modifications <- if (isTRUE(execute)) {
    # Reusing "setwd_removed" as the change_type label for os.chdir() removal
    # too — same underlying concept (working-directory override stripped
    # before running), and downstream consumers of this column key on
    # change_type strings, so a new label would need a second case
    # everywhere this column is read; see R/reproducibility_check_python.R
    # for the Python os.chdir() detection itself.
    py_setwd_rows_mod <- if (exists("py_run_tbl", inherits = FALSE) &&
                            "setwd_paths" %in% names(py_run_tbl)) {
      hit <- py_run_tbl$setwd_removed > 0 & nzchar(py_run_tbl$setwd_paths %||% "")
      if (any(hit)) dplyr::bind_rows(lapply(which(hit), function(i)
        data.frame(file_name = py_run_tbl$file_name[i], change_type = "setwd_removed",
                  detail = strsplit(py_run_tbl$setwd_paths[i], ", ", fixed = TRUE)[[1]],
                  stringsAsFactors = FALSE)))
      else NULL
    } else NULL
    dplyr::bind_rows(py_rw_rows, py_setwd_rows_mod)
  } else py_rw_rows
  if (is.null(py_modifications) || !nrow(py_modifications))
    py_modifications <- data.frame(file_name = character(0), change_type = character(0),
                                   detail = character(0))

  } # end if (has_py)

  ## ── Combine R + Python results ────────────────────────────────────────────
  # One shared row per paper (not one row per language) — see this module's
  # own development brief for the exact rule: sum the per-language counts,
  # treating NA as 0 EXCEPT when BOTH sides are NA (matching na_replace's own
  # semantics for repro_ran_ok, the one column that can genuinely be NA on
  # both sides at once -- execute = FALSE).
  na_sum <- function(a, b) {
    if (is.na(a) && is.na(b)) return(NA_integer_)
    (if (is.na(a)) 0L else a) + (if (is.na(b)) 0L else b)
  }
  table <- dplyr::bind_rows(r_table, py_table)

  ## ── Shared across languages ─────────────────────────────────────────────
  # The comparison with the paper runs ONCE, here, after both language blocks:
  # stat_output now holds the self-reproducible output plus whatever the R
  # and Python runs added, so Python console output is compared too.
  shared_match <- .empty_match()
  match_table_raw <- shared_match$table_raw
  n_output_stats <- sum(vapply(stat_output, function(s) nrow(s$long), integer(1)))
  # Sections that are about the paper's files as a whole, not about the R or
  # Python code: SPSS/Stata data and syntax, self-reproducible output
  # (JASP/jamovi/SPSS-Viewer, Stata .smcl logs, notebook outputs), and other
  # code (SAS, MATLAB, ...) with no reproducible output to fall back on.
  shared_report <- c(shared_match$report, spss_report, stata_report,
                     self_repro_report, nonr_report)
  shared_summary_text <- if (!is.null(shared_match$summary)) paste0("\n- ", sprintf(
    "%d statistic%s stored from the extracted output; %d of %d reported test%s matched (%s%%).",
    n_output_stats, plural(n_output_stats), shared_match$summary$n_found,
    shared_match$summary$n_tests, plural(shared_match$summary$n_tests),
    shared_match$summary$pct_found))

  # repro_tests_reported/repro_tests_matched come from the one shared
  # comparison above, so they are not summed across languages.
  summary_table <- data.frame(
    paper_id             = r_summary_table$paper_id[1] %||% .pid(structure_df, code_tbl),
    repro_code_n         = na_sum(r_summary_table$repro_code_n, py_summary_table$repro_code_n),
    repro_runnable       = na_sum(r_summary_table$repro_runnable, py_summary_table$repro_runnable),
    repro_missing_inputs = na_sum(r_summary_table$repro_missing_inputs, py_summary_table$repro_missing_inputs),
    repro_deps           = na_sum(r_summary_table$repro_deps, py_summary_table$repro_deps),
    repro_ran_ok         = na_sum(r_summary_table$repro_ran_ok, py_summary_table$repro_ran_ok),
    repro_tests_reported = shared_match$summary$n_tests %||% NA_integer_,
    repro_tests_matched  = shared_match$summary$n_found %||% NA_integer_
  )

  # Combined traffic light: red > error > (yellow or info) > green, with na
  # only when BOTH sides are na -- the SAME priority convention the batch
  # dispatcher's own overall_tl uses (see .reproducibility_check_batch(),
  # later in this file) for combining several papers' traffic lights into
  # one, applied here instead to combining two LANGUAGES' traffic lights for
  # one paper.
  tl_rank <- c(red = 4L, error = 4L, yellow = 2L, info = 2L, green = 1L, na = 0L)
  combine_tl <- function(a, b) {
    if (identical(a, "na") && identical(b, "na")) return("na")
    ranks <- tl_rank[c(a, b)]
    c(a, b)[which.max(ranks)]
  }
  # combine_tl(r_tl, py_tl) alone is already correct for every case, R-only
  # and Python-only included: the absent side's placeholder tl is "na" (see
  # the has_r/has_py defaults above), and combine_tl("na", x) == x whenever
  # x != "na" by construction (the tl_rank of "na" is the lowest, so
  # which.max() never picks it over a real verdict) — so there is no need
  # for a separate has_r/has_py special case here.
  tl <- combine_tl(r_tl, py_tl)
  # SPSS/Stata data with no syntax means part of the analysis is
  # unreproducible: red, whatever languages the paper's code is in (an
  # incidental .sav/.dta still fails to reproduce).
  if (isTRUE(spss_red) || isTRUE(stata_red)) tl <- "red"

  # Report: R section first, then a clearly-headed Python section so a reader
  # can tell which language a given subsection is about ("### Python code" is
  # one level ABOVE this file's usual `####` subsection convention, so it reads
  # as a sibling SECTION to the R material above it), then the shared
  # sections. Those get their own heading when a Python section precedes
  # them, so they are not read as part of it; an R-only report keeps its
  # layout, with the shared sections directly after the R ones.
  report <- c(
    if (!is.null(r_report)) r_report,
    if (has_py) c("### Python code", py_report),
    if (has_py && length(shared_report)) "### All code and output",
    shared_report
  )
  # ONE string: each part is already a "\n- ..." markdown list, and report.R
  # reads summary_text as a single string (nzchar(summary_text) && ...), which
  # errors on a longer vector.
  summary_text <- paste(c(r_summary_text, py_summary_text, shared_summary_text),
                        collapse = "")
  run_results <- dplyr::bind_rows(r_run_results, py_run_results)
  if (!nrow(run_results)) run_results <- r_run_results %||% py_run_results
  install_results <- dplyr::bind_rows(r_install_results, py_install_results)
  if (!nrow(install_results)) install_results <- r_install_results %||% py_install_results
  modifications <- dplyr::bind_rows(r_modifications, py_modifications)

  out <- list(
    table = table,
    summary_table = summary_table,
    na_replace = c(repro_code_n = 0, repro_runnable = 0,
                   repro_missing_inputs = 0, repro_deps = 0,
                   repro_ran_ok = 0),
    traffic_light = tl,
    report = report,
    summary_text = summary_text,
    # Kept in the brief report whatever the traffic light when code was run
    # (#504): there a green light means "we ran your code and it worked",
    # the main result, not "nothing to fix", and leaving it out read as if
    # the code had not been checked at all. See report_qmd().
    brief = isTRUE(execute) && "outcome" %in% names(run_results) &&
      any(run_results$outcome %in% c("ran_ok", "errored", "timed_out")),
    run_results = run_results,
    install_results = install_results,
    # Extracted JASP/jamovi/R-console/Python-console statistical output:
    # per-file statistical-output JSON (R/stat-output.R's stat_output_json();
    # a metacheck-native schema, not ISA-JSON — an earlier version borrowed
    # ISA's vocabulary and that was dropped, see R/stat-output.R's file
    # header) + flat rows.
    stat_output = stat_output,
    # The full match_reported_output() result (one row per reported test, not
    # the report's prettified table, which blanks NA to "" and renames columns),
    # so a caller can inspect exactly which reported test matched what without
    # re-running the match; NULL when there was no output to match against.
    match_table = match_table_raw,
    # Per-file modification detail (path rewrites, setwd()/os.chdir() removal,
    # font substitution, and library() injection) — see the "modifications"
    # build above. Empty (0-row, same columns) when execute = FALSE (nothing
    # was actually written/run to modify) or nothing needed modifying.
    modifications = modifications,
    # The checklist report's own section on this check: what happened to each
    # script, the problems in the files and the comparison with the paper
    # (see .repro_checklist_section()). NULL when no code was run.
    checklist_section = if (isTRUE(execute)) .repro_checklist_section(
      table, run_results,
      missing_inputs = dplyr::bind_rows(r_missing_inputs, py_missing_inputs),
      n_dup = r_n_dup + n_py_dup,
      n_tests = shared_match$summary$n_tests %||% NA_integer_,
      n_found = shared_match$summary$n_found %||% NA_integer_,
      sandbox = sandbox, timeout = timeout, install_missing = install_missing)
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

# ── Concurrent multi-paper dispatch (reproducibility_check(paper = <paperlist>,
# workers > 1, execute = TRUE, sandbox = "docker")) ─────────────────────────
#
# One background R process per paper (callr::r_bg()), up to `workers` alive
# at any moment; the instant one finishes, the next queued paper starts —
# never fewer than `workers` running while papers remain, never more.  Each
# process runs the SAME single-paper reproducibility_check() call this file
# already defines above, completely unchanged — this is dispatch only, not a
# second implementation of the check itself.
#
# A background R PROCESS per paper, not e.g. a future/promise sharing this R
# session: reproducibility_check() reads and restores several process-global
# settings (options(metacheck.skip_on_api_limit = ...), llm_use()), which are
# fine to set/restore around ONE call in the caller's own session but are not
# safe for several papers to set/restore concurrently in a SHARED session —
# see this function's own header comment on options(). A separate OS process
# per paper sidesteps that entirely: each has its own session, so one
# paper's options() never observes another's.
.reproducibility_check_batch <- function(paper, workers, results_dir, ...) {
  if (!requireNamespace("callr", quietly = TRUE))
    stop("workers > 1 with sandbox = \"docker\" needs the 'callr' package ",
         "(each paper runs in its own background R process).", call. = FALSE)

  pids <- names(paper)
  n <- length(pids)
  workers <- max(1L, min(as.integer(workers), n))
  args_common <- list(...)

  # Per-worker container resource limits, sized so `workers` concurrent
  # containers cannot collectively exceed the host — see
  # .repro_docker_resource_limits()'s own header for why this is set here
  # (once, from the number of workers actually running) rather than as a
  # fixed constant in reproducibility_check_docker.R.
  limits <- .repro_docker_resource_limits(workers)
  prev_limits <- getOption("metacheck.docker_resource_limits")
  options(metacheck.docker_resource_limits = limits)
  on.exit(options(metacheck.docker_resource_limits = prev_limits), add = TRUE)

  # Every metacheck option the caller set (including the resource limits just
  # set above), copied into each background process: a callr process starts
  # with R's defaults, so it would otherwise lose e.g. metacheck.cache.dir and
  # write every cache into the working directory (#474).
  mc_opts <- options()[grep("^metacheck\\.", names(options()))]

  message("[repro] running ", n, " paper", if (n != 1) "s", " with up to ",
          workers, " at a time (", limits$cpus, " CPU / ", limits$memory_gb,
          "GB per container) ...")

  queue <- seq_len(n)          # indices into `paper`/`pids` still to start
  running <- list()            # pid -> callr process handle
  err_files <- list()          # pid -> stderr temp file path (see launch())
  out_files <- list()          # pid -> stdout temp file path (see launch())
  results <- vector("list", n); names(results) <- pids

  launch <- function(i) {
    pid <- pids[[i]]
    message("[repro]   -> starting '", pid, "' (", sum(lengths(running) > 0 |
            vapply(running, function(x) TRUE, logical(1))), " already running)")
    # stdout/stderr go to temp files, NOT the r_bg() default of "|" (a pipe).
    # A pipe has a small, fixed OS buffer (~64KB); nothing in the poll loop
    # below drains it while a worker is still running (only read_all_error()
    # is called, and only once is_alive() has already gone FALSE). A paper
    # whose Docker run/module_run() output exceeds that buffer before the
    # parent reads it blocks the child on its next write() forever: the
    # process stays alive (is_alive() == TRUE, CPU frozen, not accumulating)
    # and its slot in `running` never frees, so the queue stalls behind it
    # and this function never returns. See callr::r_bg()'s own "Draining
    # standard output and error" documentation. Files have no such buffer
    # limit, so this class of deadlock cannot occur.
    out_file <- tempfile("repro_worker_out_")
    err_file <- tempfile("repro_worker_err_")
    p <- callr::r_bg(
      # module_run(), not reproducibility_check() directly: the module is
      # loaded from inst/modules/reproducibility_check.R at run time (see
      # module_run()'s own source(module_path, local = TRUE)), it is not an
      # exported package function -- library(metacheck) alone does not make
      # it callable, only module_run() knows how to find and source it.
      func = function(paper1, args, opts) {
        options(opts)
        do.call(metacheck::module_run,
                c(list(paper = paper1, module = "reproducibility_check"), args))
      },
      args = list(paper1 = paper[[i]], args = args_common, opts = mc_opts),
      package = "metacheck",
      supervise = TRUE,
      stdout = out_file,
      stderr = err_file
    )
    running[[pid]] <<- p
    err_files[[pid]] <<- err_file
    out_files[[pid]] <<- out_file
  }

  # Prime the pool: start up to `workers` papers immediately.
  n_start <- min(workers, length(queue))
  if (n_start > 0) {
    for (i in queue[seq_len(n_start)]) launch(i)
    queue <- queue[-seq_len(n_start)]
  }

  # Poll for completions; the moment a slot frees up, start the next queued
  # paper. A short sleep between polls (not a busy-loop) — Docker runs take
  # minutes, so polling every second costs nothing but keeps the pool full
  # promptly rather than only checking once a minute.
  while (length(running) > 0) {
    done_now <- character(0)
    for (pid in names(running)) {
      p <- running[[pid]]
      if (!p$is_alive()) done_now <- c(done_now, pid)
    }
    for (pid in done_now) {
      p <- running[[pid]]
      res <- tryCatch(p$get_result(), error = function(e) {
        message("[repro]   x  '", pid, "' failed: ", conditionMessage(e))
        NULL
      })
      if (is.null(res)) {
        # stderr was redirected to a file (see launch()), not read via
        # p$read_all_error() -- that reads from the pipe callr uses by
        # default, which this worker was not given.
        err <- tryCatch(paste(readLines(err_files[[pid]], warn = FALSE), collapse = "\n"),
                        error = function(e) "")
        res <- list(traffic_light = "error",
                    summary_text = paste0("Docker reproducibility check failed: ",
                                          if (nzchar(err)) err else "unknown error"),
                    table = data.frame(),
                    summary_table = data.frame(paper_id = pid))
      }
      unlink(c(out_files[[pid]], err_files[[pid]]))
      out_files[[pid]] <- NULL
      err_files[[pid]] <- NULL
      results[[pid]] <- res
      if (!is.null(results_dir)) {
        # res is already a metacheck_module_output (from module_run(), inside
        # the background process above) whenever the paper's own check
        # succeeded; the synthetic error stand-in built above (get_result()
        # itself failing) is not, so it is wrapped the same way here as a
        # single-module "chain" for capture_module_tables()'s own contract.
        chain <- if (inherits(res, "metacheck_module_output")) list(res) else {
          err_out <- res
          class(err_out) <- "metacheck_module_output"
          list(err_out)
        }
        tryCatch(capture_module_tables(chain, results_dir, paper_id = pid),
                error = function(e) message(
                  "[repro]   (could not write ", pid, ".rds to results_dir: ",
                  conditionMessage(e), ")"))
      }
      message("[repro]   <- '", pid, "' done (", sum(vapply(results, Negate(is.null), logical(1))),
              "/", n, ")")
      running[[pid]] <- NULL
      if (length(queue) > 0) {
        launch(queue[[1]])
        queue <- queue[-1]
      }
    }
    if (length(running) > 0 && length(done_now) == 0) Sys.sleep(1)
  }

  # ── Combine into the shape module_run() expects for a paperlist call ───────
  # One row per paper_id in `summary_table` (module_run() left-joins this onto
  # its own data.frame(paper_id = names(paper)) — see R/module.R), and every
  # paper's own `table` row-bound together, tagged by paper_id, exactly like
  # every other paperlist-aware module (e.g. stat_p_exact() via
  # extract_p_values()/text_search()) already returns.
  summary_table <- dplyr::bind_rows(lapply(pids, function(pid) {
    st <- results[[pid]]$summary_table
    if (is.null(st) || !nrow(st)) data.frame(paper_id = pid) else st
  }))
  table <- dplyr::bind_rows(lapply(pids, function(pid) {
    tb <- results[[pid]]$table
    if (is.null(tb) || !nrow(tb)) return(NULL)
    if (!"paper_id" %in% names(tb)) tb$paper_id <- pid
    tb
  }))
  if (is.null(table)) table <- data.frame()

  tls <- vapply(results, function(r) r$traffic_light %||% "na", character(1))
  overall_tl <- if (all(tls == "na")) "na" else
    if (any(tls == "red")) "red" else
    if (any(tls == "error")) "error" else
    if (any(tls == "yellow") || any(tls == "info")) "yellow" else "green"

  n_ok <- sum(tls %in% c("green", "yellow", "info"))
  n_red <- sum(tls == "red")
  n_err <- sum(tls == "error")
  summary_text <- sprintf(
    "Ran the reproducibility check on %d paper%s (up to %d at a time): %d ok, %d red, %d error%s.",
    n, plural(n), workers, n_ok, n_red, n_err, plural(n_err))

  list(
    table = table,
    summary_table = summary_table,
    traffic_light = overall_tl,
    summary_text = summary_text,
    report = summary_text,
    per_paper = results   # each paper's full, untruncated result list
  )
}

# Per-container `--cpus`/`--memory` limits for N concurrent workers, so N
# containers running at once cannot collectively claim more than the host
# has. Split evenly across `workers`: with 1 worker (the pre-existing,
# sequential path) this returns the host's full capacity, same as an
# unlimited container behaved before this existed. Read back by
# repro_install_deps_docker()/repro_run_scripts_docker() (see
# reproducibility_check_docker.R) via the `metacheck.docker_resource_limits`
# option this batch dispatcher sets around each worker's own call — set as an
# option, not a function argument threaded through reproducibility_check()'s
# own long signature, since it is dispatch-level configuration (how many
# OTHER papers are running right now), not something any single paper's own
# check has an opinion on.
.repro_docker_resource_limits <- function(workers) {
  workers <- max(1L, as.integer(workers))
  ncores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
  if (is.na(ncores) || ncores < 1) ncores <- 4L   # conservative fallback

  mem_bytes <- tryCatch(.repro_docker_host_memory_bytes(), error = function(e) NA_real_)
  mem_gb <- if (is.na(mem_bytes)) 8 else mem_bytes / (1024^3)   # conservative fallback

  cpus <- max(1, floor((ncores / workers) * 10) / 10)  # one decimal place
  memory_gb <- max(1, floor(mem_gb / workers))
  list(cpus = cpus, memory_gb = memory_gb)
}

# Best-effort total host RAM in bytes, cross-platform, for
# .repro_docker_resource_limits(). Returns NA (handled by that function's own
# fallback) rather than erroring when the platform-specific lookup fails --
# e.g. a locked-down CI container without /proc, or wmic removed from newer
# Windows builds.
.repro_docker_host_memory_bytes <- function() {
  sysname <- Sys.info()[["sysname"]]
  if (identical(sysname, "Linux")) {
    meminfo <- tryCatch(readLines("/proc/meminfo", n = 1), error = function(e) NA_character_)
    kb <- suppressWarnings(as.numeric(sub("[^0-9]*([0-9]+).*", "\\1", meminfo)))
    if (!is.na(kb)) return(kb * 1024)
  } else if (identical(sysname, "Darwin")) {
    out <- tryCatch(system("sysctl -n hw.memsize", intern = TRUE), error = function(e) NA_character_)
    b <- suppressWarnings(as.numeric(out))
    if (!is.na(b)) return(b)
  } else if (identical(sysname, "Windows")) {
    out <- tryCatch(
      system("powershell -NoProfile -Command \"(Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory\"",
             intern = TRUE),
      error = function(e) NA_character_)
    b <- suppressWarnings(as.numeric(trimws(out[length(out)])))
    if (!is.na(b)) return(b)
  }
  NA_real_
}
