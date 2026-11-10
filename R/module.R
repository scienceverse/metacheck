#' Run a module
#'
#' @param paper a paper object or a list of paper objects, or NULL to check
#'   only local files (a module's `local_path` argument)
#' @param module the name of a module or path to a module to run on this object
#' @param ... further arguments to the module (e.g., arguments for the `llm()` function like `params`); these will override any arguments in the module
#'
#' @return a list of the returned table and report text
#' @export
#'
#' @examples
#' paper <- demopaper()
#' module_run(paper, "all_p_values")
module_run <- function(paper, module, ...) {
  module_path <- module_find(module)

  # paper = NULL is documented for checking local files only (local_path):
  # an empty paper of unknown id, which every module can run on.
  # paper_id(NULL) stopped repo_check, and so every module that runs it, with
  # "paper must be a paper or paperlist object" (U78).
  if (is.null(paper)) {
    paper <- paper()
    paper$paper_id <- NA_character_
  }
  info <- module_info(module_path)
  .module_check_version(info, module)

  # handle metacheck_module_output in pipeline
  .__mc__prev_outputs <- list()
  if (inherits(paper, "metacheck_module_output")) {
    prev <- paper

    # pull out objects to use later
    paper <- prev$paper
    prev$paper <- NULL
    summary_table <- prev$summary_table %||% data.frame(paper_id = character(0))
    prev$summary_table <- NULL

    # get or set up .__mc__prev_outputs
    .__mc__prev_outputs <- prev$prev_outputs %||% list()
    prev$prev_outputs <- NULL
    .__mc__prev_outputs[[prev$module]] <- prev
  } else if (.is_paper_list(paper)) {
    # papers sharing a paper_id are renamed "~2", ... (U208)
    paper <- .unique_paper_ids(paper)
    # names(list()) is NULL, which gave a table without a paper_id column, and
    # the join with the module's summary stopped (U79)
    summary_table <- data.frame(paper_id = names(paper) %||% character(0))
  } else {
    summary_table <- data.frame(paper_id = paper$paper_id)
  }

  # load required libraries: one @import line can name several packages, and
  # a package only on GitHub is named "owner/pkg" (#223)
  for (imp in unlist(info[["import"]])) {
    pkg <- .pkg_name(imp)
    if (!require(pkg,
      quietly = TRUE,
      warn.conflicts = FALSE,
      character.only = TRUE
    )) {
      # installed but not loadable is told apart from not installed (#620)
      problem <- .pkg_problem(pkg)
      if (is.na(problem)) problem <- sprintf("the R package '%s' loads but could not be attached", pkg)
      stop("The module '", module, "' cannot run: ", problem,
           .pkg_github_hint(imp), ".", call. = FALSE)
    }
  }

  # loading required functions
  for (imp in info[["importFrom"]]) {
    pkg <- .pkg_name(imp[[1]])
    for (arg in imp[-1]) {
      if (!require(pkg,
        quietly = TRUE,
        warn.conflicts = FALSE,
        character.only = TRUE,
        include.only = arg
      ) ||
        !exists(arg)) {
        problem <- .pkg_problem(pkg)
        stop(
          "The function '", pkg, "::", arg, "' is required, but ",
          if (is.na(problem)) "the package does not have it" else problem,
          .pkg_github_hint(imp[[1]]), ".",
          call. = FALSE
        )
      }
    }
  }

  # can't pass relative paths to modules if you change the wd here
  # orig_wd <- getwd()
  # on.exit(setwd(orig_wd))
  # dirname(module_path) |> setwd()

  #tryCatch(basename(module_path) |> source(local = TRUE),
  tryCatch(module_path |> source(local = TRUE),
    error = function(e) {
      m <- basename(module) |> gsub("\\.R$", "", x = _)
      logger(m, list(paper = paper$paper_id,
                     error = e$message))
      stop("The module '", m, "' has errors: ", e$message)
    }
  )

  if (list(...) |> length()) {
    code <- sprintf("%s(paper, ...)", info$func_name)
  } else {
    code <- sprintf("%s(paper)", info$func_name)
  }

  results <- tryCatch(eval(parse(text = code)),
    error = function(e) {
      m <- basename(module) |> gsub("\\.R$", "", x = _)
      # paper_id() can fail too (a paper with no id, #499); that must not
      # replace the module's own error, which is the one to report
      logger(m, list(paper = tryCatch(paper_id(paper), error = \(e2) NA_character_),
                     error = e$message,
                     details = as.character(e)))
      stop("Running the module '", m, "' produced errors: ", e$message, call. = FALSE)
    }
  )

  if (is.data.frame(results)) {
    results <- list(table = results)
  }

  # A key (an API key, an access token) that a module read from a file is
  # replaced by "[key removed]" in its result, so metacheck never copies one
  # into its report, tables or saved results (#607). data_check warns about the
  # key itself, by its first characters only (.key_scan_files()).
  results <- .key_remove(results)
  attr(results, "keys_removed") <- NULL

  # add defaults
  if (is.null(results$traffic_light)) {
    results$traffic_light <- "info"
  }

  results$report <- results$report %||% results$summary_text %||% ""

  # process summary table
  if (!is.null(results$summary_table) &&
    "paper_id" %in% names(results$summary_table)) {
    suffix <- module_path |>
      basename() |>
      sub("\\.(r|R)$", "", x = _, ) |>
      paste0(".", x = _)

    cols_before <- colnames(summary_table)
    summary_table <- summary_table |>
      dplyr::left_join(results$summary_table,
        by = "paper_id",
        suffix = c("", suffix)
      )

    if (!is.null(results$na_replace)) {
      # replace NAs
      narep <- results$na_replace
      # only this module's own columns: in a chain the table also holds the
      # earlier modules' columns, whose NA cells an unnamed na_replace filled
      # with 0/FALSE (U77); never the paper id: an unknown id (paper = NULL,
      # local files only) became the text "0" (U78)
      cols <- setdiff(colnames(summary_table), c(cols_before, "paper_id"))
      if (is.null(names(narep))) {
        narep <- rep_len(narep, length(cols))
        names(narep) <- cols
      } else {
        # a named value finds its column under the module's suffix too
        nm <- names(narep)
        nm <- ifelse(nm %in% cols, nm,
                     ifelse(paste0(nm, suffix) %in% cols, paste0(nm, suffix), nm))
        names(narep) <- nm
      }
      narep <- narep[intersect(names(narep), cols)]
      for (col in names(narep)) {
        summary_table[is.na(summary_table[[col]]), col] <- narep[[col]]
      }
    }
  }

  # fix section if missing or not in list or multiple
  section_levels <- c("general", "intro", "method", "results", "discussion", "reference")
  section <- info$keywords[info$keywords %in% section_levels]
  if (!length(section)) section <- "general"

  report_items <- list(
    module = module,
    title = info$title,
    section = section[[1]],
    table = results$table,
    report = results$report,
    traffic_light = results$traffic_light,
    summary_text = results$summary_text,
    summary_table = summary_table,
    paper = paper,
    prev_outputs = .__mc__prev_outputs
  )

  # add any extra results
  remaining_results <- setdiff(names(results), names(report_items))
  report_items[remaining_results] <- results[remaining_results]

  class(report_items) <- "metacheck_module_output"

  return(report_items)
}

#' Find a module by name or path
#'
#' @param module the name of a module or path to a module
#'
#' @returns the path to the module
#' @keywords internal
module_find <- function(module) {
  # search for modules in built-in directory, then in the modules installed
  # with module_install(), and only then in the working directory and a
  # directory called modules in it (#512). The working directory came before
  # the installed modules, so a file modules/<name>.R replaced an installed
  # module: a server that runs a check from inside an uploaded folder ran the
  # upload's own code as that module.
  find_r <- function(dirs) {
    sapply(dirs, list.files, pattern = "\\.R$", full.names = TRUE) |>
      unlist(use.names = FALSE)
  }
  module_paths <- c(
    find_r(list.dirs(system.file("modules", package = "metacheck"))),
    .module_installed_files(),
    find_r(c(".", "modules"))
  )

  module_names <- basename(module_paths) |> sub("\\.R$", "", x = _)

  which_mod <- which(module_names == module) # catches modules by name only
  if (length(which_mod) > 0) {
    module_path <- module_paths[which_mod[[1]]]
  } else if (file.exists(module)) {
    module_path <- module
  } else {
    logger("module_find", list(module = module, error = "Can't find module"))
    stop("There were no modules that matched ", module,
      "\nuse module_list() to see a list of built-in modules.",
      call. = FALSE
    )
  }

  return(module_path)
}

#' List modules
#'
#' @param module_dir the directory to search for modules (defaults to the built-in modules)
#' @param external whether to also list the modules installed from GitHub with [module_install()]
#
#' @return a data frame of modules; `source` is the GitHub repository of an installed module, or NA
#' @export
#'
#' @examples
#' mods <- module_list()
module_list <- function(module_dir = system.file("modules", package = "metacheck"),
                        external = TRUE) {
  files <- list.files(module_dir, "\\.R$",
    full.names = TRUE,
    recursive = TRUE
  )
  if (external) files <- c(files, .module_installed_files())
  txt <- lapply(files, \(mod) tryCatch(
    module_info(mod),
    error = \(e) {}
  ))

  # remove errored files
  valid <- !sapply(txt, is.null)
  txt <- txt[valid]

  display <- data.frame(
    name = basename(files[valid]) |> sub("\\.R$", "", x = _),
    title = sapply(txt, \(x) x[["title"]][[1]]),
    description = sapply(txt, `[[`, "description") |>
      sapply(\(x) x[[1]] %||% ""),
    section = sapply(txt, `[[`, "keywords") |>
      sapply(\(x) x[[1]] %||% "general"),
    path = files[valid],
    source = sapply(txt, \(x) x$source$repo %||% NA_character_)
  )

  section_levels <- c("general", "intro", "method", "results", "discussion", "reference")
  display$section <- factor(display$section, section_levels)
  display <- sort_by(display, list(display$section, display$name))

  class(display) <- c("metacheck_module_list", "data.frame")
  rownames(display) <- NULL

  return(display)
}

#' Get module information
#'
#' @param module the name of a module or path to a module
#'
#' @returns a list of module info
#' @export
#'
#' @examples
#' module_info("all_p_values")
module_info <- function(module) {
  module_path <- module_find(module)
  tryCatch(
    {
      roxy <- roxygen2::parse_file(module_path, env = NULL)
    },
    error = function(e) {
      m <- basename(module) |> gsub("\\.R$", "", x = _)
      logger(m, list(error = e$message))
      stop("The module '", m, "' code has errors: ", e$message)
    }
  )

  tags <- roxy[[1]]$tags
  vals <- lapply(tags, \(x) x$val)
  names <- sapply(tags, \(x) x$tag)
  info <- list()
  for (n in unique(names)) {
    val <- vals[names == n]
    if (length(val) == 1) val <- val[[1]]
    info[[n]] <- val
  }

  if (is.vector(info$importFrom)) {
    info$importFrom <- list(info$importFrom)
  }

  # get argument defaults
  info$arg_defaults <- roxy[[1]]$call[[3]][[2]]

  # get function name
  lines <- readLines(module_path)
  pattern <- "^\\s*([a-zA-Z0-9\\._]+)\\s*(<-|=)\\s*function\\b"
  funcs <- grepl(pattern, lines) |> which()
  match <- regexec(pattern, lines[funcs[1]])
  info$func_name <- regmatches(lines[funcs[1]], match)[[1]][2]

  # the GitHub repository and commit of a module from module_install()
  info$source <- .module_source(module_path)

  info
}


#' Get Module Help
#'
#' See the help files for a module by name (get a list of names from `module_list()`)
#'
#' @param module the name of a module or path to a module
#'
#' @returns the help text
#' @export
#'
#' @examples
#' module_help("marginal")
module_help <- function(module = NULL) {
  if (is.null(module)) {
    module_list() |> print()
    return(invisible(NULL))
  }

  help <- module_info(module)
  help$func_name <- NULL
  help$module <- module

  help[sapply(help, is.null)] <- NULL
  class(help) <- "metacheck_module_help"

  return(help)
}

#' Print Module List Object
#'
#' @param x The metacheck_module_list object
#' @param ... Additional parameters for print
#' @returns `x`, invisibly
#'
#' @export
#' @keywords internal
#'
print.metacheck_module_list <- function(x, ...) {
  txt <- sapply(levels(x$section), \(s) {
    sub <- x[x$section == s, ]
    if (nrow(sub) == 0) {
      return(NULL)
    }
    from <- ifelse(is.na(sub$source), "", paste0(" [from ", sub$source, "]"))
    items <- paste0("* ", sub$name, ": ", sub$description, from, "\n")
    title <- sprintf("\n*** %s ***\n\n", toupper(s))

    c(title, items)
  })
  txt <- unlist(txt)

  # txt <- paste0("* ", x$name, ": ", x$description, "\n")
  cat("", txt, "\nUse `module_help(\"module_name\")` for help with a specific module\n", sep = "")
  invisible(x)
}

#' Print Module Output
#'
#' @param x The metacheck_module_output object
#' @param ... Additional parameters for `module_report()`
#' @returns `x`, invisibly
#'
#' @export
#' @keywords internal
#'
print.metacheck_module_output <- function(x, ...) {
  # args <- list(...)
  # args$module_output <- x
  #
  # # set defaults
  # if (!"header" %in% names(args)) args$header = ""
  # if (!"maxrows" %in% names(args)) args$maxrows = 20
  # if (!"trunc_cell" %in% names(args)) args$trunc_cell = 100
  #
  # txt <- do.call(module_report, args)

  txt <- sprintf("%s: %s", x$title, x$summary_text)

  cat(txt)
  invisible(x)
}

#' Print Module Help Object
#'
#' @param x The metacheck_module_help object
#' @param ... Additional parameters for print
#' @returns `x`, invisibly
#'
#' @export
#' @keywords internal
#'
print.metacheck_module_help <- function(x, ...) {
  p <- ""
  if (length(x$arg_defaults) > 1) {
    params <- x$arg_defaults
    params$paper <- NULL
    pd <- paste(names(params), "=", params)
    p <- paste0(", ", pd, collapse = "")
  }
  usage <- sprintf(
    "module_run(paper, \"%s\"%s)",
    x$module, p
  )

  # make a list if only 1 param
  if (!is.null(x$param$name)) x$param <- list(x$param)
  args <- x$param |>
    sapply(\(p) paste0("- ", p$name, ": ", p$description, "  ")) |>
    paste(collapse = "\n")


  c(
    x$title %||% "{no title}",
    x$description %||% "{no description}",
    usage,
    args,
    x$details %||% ""
  ) |>
    paste(collapse = "\n\n") |>
    gsub("\n{2,}", "\n\n", x = _) |>
    trimws() |>
    cat()
  invisible(x)
}


#' Create a Module from a Template
#'
#' @param module_name The short name of the module (should contain only letters, numbers, and _)
#' @param path The path of the directory to save the module in, such as
#'   `"modules"`; there is no default, because CRAN does not allow writing into
#'   the working directory unless you ask for it
#'
#' @returns the file path (invisibly)
#' @export
#' @examples
#' path <- module_template("my_check", file.path(tempdir(), "modules"))
#' head(readLines(path))
module_template <- function(module_name, path) {
  if (!grepl("^[a-zA-Z0-9_]+$", module_name)) {
    stop("The module_name must contain only letters, numbers, and _")
  }

  template <- system.file("templates/_module.R", package = "metacheck") |>
    readLines() |>
    gsub("module_name", module_name, x = _)

  dir.create(path, FALSE)
  filepath <- file.path(path, paste0(module_name, ".R"))
  write(template, filepath)

  if (!file.exists(filepath)) {
    stop("The file ", filepath, " did not save")
  }

  if (interactive() &
    requireNamespace("rstudioapi", quietly = TRUE) &
    rstudioapi::isAvailable()) {
    rstudioapi::documentOpen(filepath)
  }

  invisible(filepath)
}

#' Get Previous Outputs
#'
#' A helper for creating modules. Checks for previous module outputs in a chain and returns the named list item if it exists in any parent environment.
#'
#' @param module the name of a previously run module
#' @param item the name of the list item to extract
#' @param parent_n the number of parents to traverse up the chain. Noramlly 2 if you are calling this from a module function, but maybe more if you are calling it from a helper function.
#'
#' @returns the extracted list item, or NULL if not found
#' @export
#'
#' @examples
#' # .__mc__prev_outputs is usually created by `module_run()`
#' .__mc__prev_outputs <- list(mod_1 = list(a = 1, b = 2))
#' f <- function(item) {
#'   get_prev_outputs("mod_1", item)
#' }
#' f("a")
#' f("d")
get_prev_outputs <- function(module, item, parent_n = 2) {
  obj <- ".__mc__prev_outputs"
  prev <- get0(obj, parent.frame(parent_n))
  prev[[module]][[item]]
}

# Non-result list elements of a metacheck_module_output: inter-module plumbing
# and the paper itself, none of which belong in a saved results object. Every
# OTHER element (table, summary_table, and any module-specific result frame)
# is kept as-is.
.module_output_plumbing <- c("paper", "prev_outputs", "module", "title",
                             "section")

# data_check's `previews` holds the FULL read data frame of every tabular data
# file (not a truncated preview), kept only so data_validate can read it back
# via get_prev_outputs() from the LIVE chain. Dropped here: it is not a
# reduced/derived result but a near-complete copy of the paper's raw data
# files, and can be arbitrarily large. It remains available to the live
# chain, just not persisted.
.module_output_archive_exclude <- c("previews")

# Save one paper's full module outputs to disk (lossless), so a later
# reproducibility_check-only (or any single-module) retest can load a
# paper's already-computed code_tbl/plan/structure_df instead of re-running
# data_check/code_check/psychds_check (and their LLM calls) from scratch.
# Unlike a reduced/truncated JSON summary, result data here is not coerced:
# the object round-trips exactly (list-columns, numeric types, factors).
#
# Only the module *results* are kept; inter-module plumbing (prev_outputs,
# paper, and the routing fields module/title/section) is stripped so the
# file stays small and self-contained.
#
# `chain` is a module-output chain from `report_module_run()` (or
# `report()`'s return value) -- a flat list of metacheck_module_output
# elements, one per module that ran.
# `results_dir` is the directory to write the per-paper `<paper_id>.rds`
# into (created if needed).
# `paper_id` is an optional paper id, used to name the file when it cannot be
# recovered from the chain's summary tables.
# Returns the written path, invisibly.
capture_module_tables <- function(chain, results_dir, paper_id = NULL) {
  mods <- Filter(function(x) inherits(x, "metacheck_module_output"), chain)
  if (length(mods) == 0) mods <- chain

  # Recover one paper id for the file name.
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
  pid <- chain_pid %||% "paper"
  if (is.na(pid) || !nzchar(pid)) pid <- paper_id %||% "paper"

  # Keep every result element of each module, drop the plumbing and the
  # explicitly excluded large/non-archival elements (e.g. data_check's previews).
  outputs <- lapply(mods, function(mo) {
    keep <- setdiff(names(mo),
                    c(.module_output_plumbing, .module_output_archive_exclude))
    mo[keep]
  })

  # Name outputs by each module's own $module field, so collect_module_tables()
  # can look them up by name. Fall back to a positional name for anything
  # missing/blank/duplicated, so a lookup miss never silently collides.
  mod_names <- vapply(mods, function(mo) mo$module %||% NA_character_, character(1))
  bad <- is.na(mod_names) | !nzchar(mod_names) | duplicated(mod_names)
  mod_names[bad] <- paste0("module_", which(bad))
  names(outputs) <- mod_names

  # report_module_run() strips each module's own summary_table and left-joins
  # them all into ONE wide row carried by the last module (duplicate names
  # suffixed ".<module>"). So a mid-chain module's `summary_table` slot is
  # usually empty here; store that combined wide row once at the top level.
  # The per-module `table`s are untouched and complete.
  combined_summary <- NULL
  for (mo in mods) {
    st <- mo$summary_table
    if (!is.null(st) && is.data.frame(st) && nrow(st) > 0 &&
        ncol(st) > ncol(combined_summary %||% st[, 0, drop = FALSE]))
      combined_summary <- st
  }

  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(results_dir, paste0(pid, ".rds"))
  saveRDS(list(paper_id = pid,
               generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
               summary_table = combined_summary,
               modules = outputs),
          path)
  invisible(path)
}

# Stack one module's full tables across every saved paper. Reads every
# `<paper_id>.rds` written by `capture_module_tables()` and binds a chosen
# element (default "table"; also e.g. "summary_table", or a module's extra
# frame) of ONE module across all papers into a single data frame, tagged
# with paper_id. Returns a data.frame (empty if none had it).
collect_module_tables <- function(results_dir, module, element = "table") {
  rfiles <- list.files(results_dir, pattern = "[.]rds$", full.names = TRUE)
  if (length(rfiles) == 0) {
    warning("No *.rds files found in ", results_dir,
            ". Did the run call capture_module_tables()?", call. = FALSE)
    return(data.frame())
  }
  parts <- lapply(rfiles, function(f) {
    j <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(j) || is.null(j$modules[[module]])) return(NULL)
    el <- j$modules[[module]][[element]]
    if (is.null(el) || !is.data.frame(el) || nrow(el) == 0) return(NULL)
    if (!"paper_id" %in% names(el)) el$paper_id <- j$paper_id
    el
  })
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) {
    warning(length(rfiles), " *.rds file(s) found in ", results_dir,
            ", but none had a \"", module, "\" module with a non-empty \"",
            element, "\".", call. = FALSE)
    return(data.frame())
  }
  out <- dplyr::bind_rows(parts)
  front <- intersect("paper_id", names(out))
  out[, c(front, setdiff(names(out), front)), drop = FALSE]
}

# Load a paper's saved module outputs (written by `capture_module_tables()`)
# back into a metacheck_module_output-shaped chain, so `module_run()` sees
# them exactly as if they had just been computed live in this session --
# i.e. get_prev_outputs("data_check", "structure") etc. finds them without
# re-running data_check/code_check/psychds_check (or their LLM calls).
#
# `results_dir` is the directory holding the per-paper `<paper_id>.rds` files
# (as written by `capture_module_tables()`).
# `paper_id` is the paper id to load (matches the saved file's basename).
# `paper` is the paper object itself (not persisted by `capture_module_tables()`,
# since every module already carries it live) -- required so the returned
# chain has somewhere for `module_run()` to find `paper$paper_id` etc.
#
# Returns a metacheck_module_output object usable as the `paper` argument to
# `module_run()`/`report()`/`report_module_run()`, or NULL if no saved file
# exists for this paper_id.
#
# @examples
# \dontrun{
# chain <- .load_module_tables("D:/psychscience_data/psychsci/_tables",
#                              "collabra.123", paper)
# if (!is.null(chain)) {
#   out <- module_run(chain, "reproducibility_check", execute = TRUE)
# }
# }
.load_module_tables <- function(results_dir, paper_id, paper) {
  path <- file.path(results_dir, paste0(paper_id, ".rds"))
  if (!file.exists(path)) return(NULL)
  saved <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(saved) || length(saved$modules) == 0) return(NULL)

  mod_names <- names(saved$modules)
  prev_outputs <- stats::setNames(lapply(mod_names, function(m) {
    mo <- saved$modules[[m]]
    mo$module <- m
    mo$title <- mo$title %||% m
    mo$section <- mo$section %||% "general"
    mo$paper <- paper
    class(mo) <- "metacheck_module_output"
    mo
  }), mod_names)

  # The LAST module becomes the top-level chain object (same shape
  # report_module_run() builds), with every module (itself included) nested
  # under prev_outputs so get_prev_outputs() finds all of them.
  last <- prev_outputs[[length(prev_outputs)]]
  last$prev_outputs <- prev_outputs
  last$summary_table <- saved$summary_table
  last
}
