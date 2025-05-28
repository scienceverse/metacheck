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

  # handle ppchk_module_output in pipeline
  if (inherits(paper, "ppchk_module_output")) {
    summary_table <- paper$summary
    paper <- paper$paper
  } else if (is_paper_list(paper)) {
    summary_table <- data.frame(id = names(paper))
  } else {
    summary_table <- data.frame(id = paper$id)
  }

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
                   include.only = arg)) {
        stop("The '", pkg[[1]], "' package is required but not installed.")
      }
    }
  }

  orig_wd <- getwd(); on.exit(setwd(orig_wd))
  dirname(module_path) |> setwd()

  tryCatch(basename(module_path) |>source(local = TRUE),
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

  results$report <- results$report %||% ""

  # process summary table
  if (!is.null(results$summary)) {
    suffix <- module_path |> basename() |>
      sub("\\.(r|R)$", "", x = _,) |>
      paste0(".", x = _)

    summary_table <- summary_table |>
      dplyr::left_join(results$summary, by = "id",
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
    table = results$table,
    report = results$report,
    traffic_light = results$traffic_light,
    summary = summary_table,
    paper = paper
  )

  class(report_items) <- "ppchk_module_output"

  return(report_items)
}

#' Find a module by name or path
#'
#' @param module the name of a module or path to a module
#'
#' @returns the path to the module
#' @keywords internal
module_find <- function(module) {
  # search for module in built-in directory
  module_libs <- system.file("modules", package = "papercheck")
  module_paths <- sapply(module_libs, list.files, full.names = TRUE, recursive = TRUE)
  module_names <- basename(module_paths) |> sub("\\.R$", "", x = _)

  which_mod <- which(module_names == module)
  if (length(which_mod) == 1) {
    module_path <- module_paths[which_mod]
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
#' module_list()
module_list <- function(module_dir = system.file("modules", package = "papercheck")) {
  files <- list.files(module_dir, "\\.R$",
                      full.names = TRUE,
                      recursive = TRUE)
  txt <- lapply(files, module_info)

  display <- data.frame(
    name = basename(files) |> sub("\\.R$", "", x = _),
    title = sapply(txt, \(x) x[["title"]][[1]]),
    description = sapply(txt, `[[`, "description") |>
      sapply(\(x) x[[1]] %||% ""),
    path = files
  )
  class(display) <- c("ppchk_module_list", "data.frame")
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
module_help <- function(module) {
  info <- module_info(module)

  help <- info[c("title", "description", "details", "examples")]
  help[sapply(help, is.null)] <- NULL
  class(help) <- "ppchk_module_help"

  return(help)
}

#' Print Module Help Object
#'
#' @param x The ppchk_module_help object
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.ppchk_module_help <- function(x, ...) {
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
