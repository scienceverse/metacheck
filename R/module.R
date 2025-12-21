#' Run a module
#'
#' @param paper a paper object or a list of paper objects
#' @param module the name of a module or path to a module to run on this object
#' @param ... further arguments to the module (e.g., arguments for the `llm()` function like `seed`); these will override any arguments in the module
#'
#' @return a list of the returned table and report text
#' @export
#'
#' @examples
#' module_run(psychsci[[1]], "all_p_values")
module_run <- function(paper, module, ...) {
  module_path <- module_find(module)
  info <- module_info(module_path)

  # handle metacheck_module_output in pipeline
  .prev_outputs__ <- list()
  if (inherits(paper, "metacheck_module_output")) {
    prev <- paper

    # pull out objects to use later
    paper <- prev$paper
    prev$paper <- NULL
    summary_table <- prev$summary_table
    prev$summary_table <- NULL

    # get or set up .prev_outputs__
    .prev_outputs__ <- prev$prev_outputs %||% list()
    prev$prev_outputs <- NULL
    .prev_outputs__[[prev$module]] <- prev
  } else if (is_paper_list(paper)) {
    summary_table <- data.frame(id = names(paper))
  } else {
    summary_table <- data.frame(id = paper$id)
  }

  # make previous outputs available to module code
  # TODO: check if there is a better way
  assign(".prev_outputs__", .prev_outputs__, .GlobalEnv)
  on.exit(rm(".prev_outputs__", envir = .GlobalEnv))

  # load required libraries
  for (pkg in info[["import"]]) {
    if (!require(pkg, quietly = TRUE,
                 warn.conflicts = FALSE,
                 character.only = TRUE)) {
      stop("The '", pkg, "' package is required but not installed.")
    }
  }

  # loading required functions
  for (pkg in info[["importFrom"]]) {
    for (arg in pkg[-1]) {
      if (!require(pkg[[1]], quietly = TRUE,
                   warn.conflicts = FALSE,
                   character.only = TRUE,
                   include.only = arg) ||
          !exists(arg)) {
        stop("The function '", pkg[[1]], "::", arg,
             "' is required but not installed.")
      }
    }
  }

  orig_wd <- getwd(); on.exit(setwd(orig_wd))
  dirname(module_path) |> setwd()

  tryCatch(basename(module_path) |> source(local = TRUE),
           error = function(e) {
             stop("The module code has errors: ", e$message)
           })

  if (list(...) |> length()) {
    code <- sprintf("%s(paper, ...)", info$func_name)
  } else {
    code <- sprintf("%s(paper)", info$func_name)
  }
  results <- tryCatch(eval(parse(text = code)),
                      error = function(e) {
                        stop("Running the module produced errors: ", e$message)
                      })

  if (is.data.frame(results)) {
    results <- list(table = results)
  }

  # add defaults
  if (is.null(results$traffic_light) & is.data.frame(results$table)) {
    results$traffic_light = ifelse(nrow(results$table), "info", "na")
  }

  results$report <- results$report %||% results$summary_text %||% ""

  # process summary table
  if (!is.null(results$summary_table)) {
    suffix <- module_path |> basename() |>
      sub("\\.(r|R)$", "", x = _,) |>
      paste0(".", x = _)

    summary_table <- summary_table |>
      dplyr::left_join(results$summary_table, by = "id",
                       suffix = c("", suffix))

    if (!is.null(results$na_replace)) {
      # replace NAs
      narep <- results$na_replace
      cols <- colnames(summary_table)
      if (is.null(names(narep))) {
        narep <- rep_len(narep, length(cols))
        names(narep) <- cols
      }
      narep <- narep[intersect(names(narep), cols)]
      for (col in names(narep)) {
        summary_table[is.na(summary_table[[col]]), col] <- narep[[col]]
      }
    }
  }

  report_items <- list(
    module = module,
    title = info$title,
    section = info$keywords,
    table = results$table,
    report = results$report,
    traffic_light = results$traffic_light,
    summary_text = results$summary_text,
    summary_table = summary_table,
    paper = paper,
    prev_outputs = .prev_outputs__
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
  # search for modules in built-in directory
  module_libs <- system.file("modules", package = "metacheck") |>
    list.dirs() |>
    c(".", "modules") # also search working directory and any directory called modules
  module_paths <- sapply(module_libs, list.files,
                         pattern = "\\.R$",
                         full.names = TRUE) |>
    unlist(use.names = FALSE)

  module_names <- basename(module_paths) |> sub("\\.R$", "", x = _)

  which_mod <- which(module_names == module)
  if (length(which_mod) > 0) {
    module_path <- module_paths[which_mod[[1]]]
  } else if (file.exists(module)) {
    module_path <- module
  } else {
    stop("There were no modules that matched ", module, "; use module_list() to see a list of built-in modules.")
  }

  return(module_path)
}

#' List modules
#'
#' @param module_dir the directory to search for modules (defaults to the built-in modules)
#
#' @return a data frame of modules
#' @export
#'
#' @examples
#' mods <- module_list()
module_list <- function(module_dir = system.file("modules", package = "metacheck")) {
  files <- list.files(module_dir, "\\.R$",
                      full.names = TRUE,
                      recursive = TRUE)
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
    section = sapply(txt, `[[`, "keywords"),
    path = files[valid]
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
  tryCatch({
    roxy <- roxygen2::parse_file(module_path, env = NULL)
  }, error = function(e) {
    stop("The module code has errors: ", e$message)
  })

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

  # get function name
  lines <- readLines(module_path)
  pattern <- "^\\s*([a-zA-Z0-9\\._]+)\\s*(<-|=)\\s*function\\b"
  funcs <- grepl(pattern, lines) |> which()
  match <- regexec(pattern, lines[funcs[1]])
  info$func_name <- regmatches(lines[funcs[1]], match)[[1]][2]

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

  info <- module_info(module)

  help <- info[c("title", "description", "details", "examples")]
  help[sapply(help, is.null)] <- NULL
  class(help) <- "metacheck_module_help"

  return(help)
}

#' Print Module List Object
#'
#' @param x The metacheck_module_list object
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.metacheck_module_list <- function(x, ...) {
  txt <- sapply(levels(x$section), \(s) {
    sub <- x[x$section == s, ]
    if (nrow(sub) == 0) return(NULL)
    items <- paste0("* ", sub$name, ": ", sub$description, "\n")
    title <- sprintf("\n*** %s ***\n", toupper(s))

    c(title, items)
  })
  txt <- unlist(txt)

  #txt <- paste0("* ", x$name, ": ", x$description, "\n")
  cat("", txt, "\nUse `module_help(\"module_name\")` for help with a specific module\n")
}

#' Print Module Output
#'
#' @param x The metacheck_module_output object
#' @param ... Additional parameters for `module_report()`
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
}

#' Print Module Help Object
#'
#' @param x The metacheck_module_help object
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.metacheck_module_help <- function(x, ...) {
  examples <- ""
  if (!is.null(x$examples)) {
    examples <- sprintf("``` r\n%s\n```", x$examples)
  }

  sprintf("%s\n\n%s\n\n%s\n\n%s",
          x$title %||% "{no title}",
          x$description %||% "{no description}",
          x$details %||% "",
          examples) |>
    gsub("\n{2,}", "\n\n", x = _) |>
    trimws() |>
    cat()
}



#' Create a Module from a Template
#'
#' @param module_name The short name of the module (should contain only letters, numbers, and _)
#' @param path The path of the directory to save the module in (defaults to a directory called "modules" in the working directory)
#'
#' @returns the file path (invisibly)
#' @export
module_template <- function(module_name, path = "./modules") {
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
#'
#' @returns the extracted list item, or NULL if not found
#' @export
#'
#' @examples
#' # .prev_outputs__ is usually created by `module_run()`
#' .prev_outputs__ <- list(mod_1 = list(a = 1, b = 2))
#' f <- function(item) { get_prev_outputs("mod_1", item) }
#' f("a")
#' f("d")
get_prev_outputs <- function(module, item) {
  obj <- ".prev_outputs__"
  # exists <- vapply(
  #   sys.parents(),
  #   function(i) exists(obj, envir = sys.frame(i)),
  #   logical(1)
  # )
  #
  # if (!any(exists)) return(NULL)
  #
  # i <- which(exists)[[1]]
  # prev <- get(obj, envir = sys.frame(i))

  if (!exists(obj, envir = .GlobalEnv, inherits = FALSE)) {
    return(NULL)
  }
  prev <- get(obj, envir = .GlobalEnv)
  prev[[module]][[item]]
}
