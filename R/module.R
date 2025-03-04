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
#' filename <- demoxml()
#' paper <- read_grobid(filename)
#' module_run(paper, "imprecise-p")
module_run <- function(paper, module, ...) {
  module_path <- module_find(module)

  # read in module
  json <- tryCatch({
    jsonlite::read_json(module_path, simplifyVector = TRUE)
  }, error = function(e) {
    stop("The module has a problem with JSON format:\n", e$message, call. = FALSE)
  })
  module_dir <- dirname(module_path)

  mod_chunks <- names(json) %in% c("text", "code", "llm") |>
    which()

  results <- paper
  for (chunk in mod_chunks) {
    type <- names(json)[[chunk]]
    if ("table" %in% names(results) && is.data.frame(results$table)) {
      results <- results$table
    }

    args <- json[[chunk]]
    override_args <- list(...)
    args[names(override_args)] <- override_args

    if (type == "text") {
      results <- module_run_text(results, args)
    } else if (type == "code") {
      results <- module_run_code(results, args, module_dir)
    # } else if (type == "ml") {
    #   results <- module_run_ml(results, args, module_dir)
    } else if (type == "llm") {
      results <- module_run_llm(results, args)
    } else {
      stop("The module has an invalid type of ", type)
    }
  }

  # traffic light ----
  if (is.null(results$traffic_light) &&
      !is.null(json$traffic_light)) {
    results$traffic_light <- ifelse(
      nrow(results$table) == 0,
      json$traffic_light$not_found,
      json$traffic_light$found
    )
  }

  results$traffic_light <- results$traffic_light %||% "na"

  # report text ----
  if (is.null(results$report) && !is.null(json$report)) {
    results$report <- paste(
      json$report[["all"]],
      json$report[[results$traffic_light]]
    ) |> trimws()
  }
  results$report <- results$report %||% ""

  report_items <- list(
    module = module,
    title = json$title,
    table = results$table,
    report = results$report,
    traffic_light = results$traffic_light
  )
  class(report_items) <- "ppchk_module_output"

  return(report_items)
}


#' Run text module
#'
#' @param paper the paper object (or list of objects)
#' @param args a list of arguments to `search_text()`
#'
#' @return data frame
#' @keywords internal
module_run_text <- function(paper, args) {
  args$paper <- paper
  list(
    table = do.call(search_text, args)
  )
}


#' Run code module
#'
#' @param paper the paper object (or list of objects)
#' @param args a list of arguments
#' @param module_dir the base directory for the module, in case code is in files with relative paths
#'
#' @return data frame
#' @keywords internal
module_run_code <- function(paper, args, module_dir = ".") {
  if (!is.null(args$path)) {
    filepath <- file.path(module_dir, args$path)
    if (!file.exists(filepath)) {
      stop("The code file ", args$path, " could not be found")
    }
    args$code <- readLines(filepath)
  }
  code <- paste(args$code, collapse = "\n")

  results <- tryCatch(eval(parse(text = code)),
           error = function(e) {
             stop("The function has errors:", e$message)
           })

  if (is.data.frame(results)) {
    results <- list(table = results)
  }

  return(results)
}

# ' Run machine learning module
# '
# ' @param paper the paper object (or list of objects)
# ' @param args a list of arguments
# ' @param module_dir the base directory for the module, in case resources are in files with relative paths
# '
# ' @return data frame
# ' @keywords internal
# module_run_ml <- function(paper, args, module_dir = ".") {
#   model_dir <- file.path(module_dir, args$model_dir)
#   if (!file.exists(model_dir)) {
#     stop("The model directory ", args$model_dir, " could not be found; make sure the module specification file is using a relative path to the directory")
#   }
#
#   if (is.vector(paper)) {
#     text <- data.frame(text = paper)
#   } else if (is.data.frame(paper)) {
#     text <- paper
#   } else {
#     text <- search_text(paper, return = "sentence")
#   }
#
#   class_col <- args$class_col %||% "class"
#   return_prob <- args$return_prob %||% FALSE
#
#   results <- ml(text, model_dir,
#      class_col = class_col,
#      map = args$map,
#      return_prob = return_prob)
#
#   if (!is.null(args$filter)) {
#     keep <- results[[class_col]] %in% args$filter
#     results <- results[keep, ]
#   }
#
#   return( list(table = results) )
# }

#' Run LLM module
#'
#' @param paper the paper object (or list of object)
#' @param args a list of arguments to `llm()`
#'
#' @return data frame
#' @keywords internal
module_run_llm <- function(paper, args) {
  args$text <- search_text(paper)
  results <- do.call(llm, args)
  # attr(results, "llm")

  return( list(table = results) )
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
  files <- list.files(module_dir, "\\.mod$",
                      full.names = TRUE,
                      recursive = TRUE)
  json <- lapply(files, jsonlite::read_json, simplifyVector = TRUE)

  type <- sapply(json, function(j) {
    if ("llm" %in% names(j)) return("llm")
    if ("ml" %in% names(j)) return("ml")
    if ("code" %in% names(j)) return("code")
    if ("text" %in% names(j)) return("text")
  })

  display <- data.frame(
    name = basename(files) |> sub("\\.mod$", "", x = _),
    title = sapply(json, `[[`, "title"),
    description = sapply(json, `[[`, "description") |>
      sapply(\(x) x %||% ""),
    type = type,
    path = files
  )
  class(display) <- c("ppchk_module_list", "data.frame")
  rownames(display) <- NULL

  return(display)
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
  module_names <- basename(module_paths) |> sub("\\.mod$", "", x = _)

  which_mod <- which(module_names == module)
  if (length(which_mod) == 1) {
    module_path <- module_paths[which_mod]
  } else if (file.exists(module)) {
    module_path <- module
  } else {
    stop("There were no modules that matched ", module)
  }

  return(module_path)
}
