#' Default value for `NULL`
#'
#' This infix function makes it easy to replace `NULL`s with a default
#' value. It's inspired by the way that Ruby's or operation (`||`)
#' works.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns `x`.
#' @export
#' @name op-null-default
#' @examples
#' 1 %||% 2
#' NULL %||% 2
`%||%` <- function(x, y) {
  if (is_null(x)) y else x
}

# Reexport from base on newer versions of R to avoid conflict messages
if (exists("%||%", envir = baseenv())) {
  `%||%` <- get("%||%", envir = baseenv())
}

#' Less scary green messages
#'
#' @param ... message components (see \code{\link[base]{message}})
#' @param domain (see \code{\link[base]{message}})
#' @param appendLF append new line? (see \code{\link[base]{message}})
#'
#' @return TRUE
#' @keywords internal
#'
message <- function (..., domain = NULL, appendLF = TRUE) {
  if (getOption("papercheck.verbose")) {
    if (interactive()) {
      # not in knitr environment
      base::message("\033[32m", ..., "\033[39m",
                    domain = domain, appendLF = appendLF)
    } else {
      base::message(..., domain = domain, appendLF = appendLF)
    }
  }
}


#' Check if site is available
#'
#' @param url A URL to check
#' @param msg A message that contains %s to replace in the site name
#' @param error Throw an error if the site is down; otherwise return a logical
#'
#' @return logical
#' @keywords internal
site_down <- function(url, msg = "The website %s is not available", error = TRUE) {
  site <- url |>
    gsub("https?\\://", "", x = _) |>
    gsub("/.*", "", x = _)

  down <- tryCatch(httr::http_error(site),
                          error = function(e) { return(TRUE) })

  if (down & error) {
    sprintf(msg, site) |> stop(call. = FALSE)
  }

  return(down)
}



#' Concatenate tables
#'
#' Concatenate tables across a list of paper objects
#'
#' @param papers a list of paper objects
#' @param name_path a vector of names that get you to the table
#'
#' @return a merged table
#' @export
#'
#' @examples
#' grobid_dir <- system.file("grobid", package = "papercheck")
#' papers <- read_grobid(grobid_dir)
#' references <- concat_tables(papers, c("refs", "references"))
concat_tables <- function(papers, name_path) {
  if ("scivrs_paper" %in% class(papers)) {
    # single paper object
    papers <- list(papers)
  }

  table_list <- papers
  for (name in name_path) {
    table_list <- lapply(table_list, `[[`, name)
  }
  for (i in seq_along(papers)) {
    x <- table_list[[i]]
    if (is.data.frame(x) && nrow(x) > 0) {
      table_list[[i]]$id <- papers[[i]]$info$filename
    }
  }

  merged_table <- do.call(rbind, table_list)
  rownames(merged_table) <- NULL

  merged_table
}


#' Print Paper Object
#'
#' @param x The scivrs_paper list
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.scivrs_paper <- function(x, ...) {
  underline <- rep("-", nchar(x$name)) |> paste(collapse="")
  txt <- sprintf("%s\n%s\n%s\n\n* Sections: %d\n* Sentences: %d\n* References: %d\n* Citations: %d\n\n",
                 underline, x$name, underline,
                 max(c(0, x$full_text$div)),
                 nrow(x$full_text),
                 nrow(x$references),
                 nrow(x$citations))

  cat(txt)
}

#' Print Validation List Object
#'
#' @param x The ppchk_validate object
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.ppchk_validate <- function(x, ...) {
  txt <- sprintf("Validated matches from a sample of %i:
* tables: %i%s
* reports: %i%s
* traffic lights: %i%s\n",
    nrow(x$sample),
    round(x$tables_matched * 100),
    ifelse(is.na(x$tables_matched), "", "%"),
    round(x$reports_matched * 100),
    ifelse(is.na(x$reports_matched), "", "%"),
    round(x$tl_matched * 100),
    ifelse(is.na(x$tl_matched), "", "%")
  )
  cat("", txt)
}

#' Print Module List Object
#'
#' @param x The ppchk_module_list object
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.ppchk_module_list <- function(x, ...) {
  txt <- paste0("* ", x$name, ": ", x$description, "\n")
  cat("", txt)
}

#' Print Module Output
#'
#' @param x The ppchk_module_output object
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.ppchk_module_output <- function(x, ...) {
  txt <- module_report(x, 3)
  cat(txt)
}


#' Get demo PDF file
#'
#' @return vector of paths
#' @export
#'
#' @examples
#' demopdf()
demopdf <- function() {
  grobid_dir <- system.file("extdata", package="papercheck")
  pattern <- "\\.pdf$"
  file <- list.files(grobid_dir, pattern, full.names = TRUE)
  return(file)
}

#' Get demo XML file
#'
#' @return vector of paths
#' @export
#'
#' @examples
#' demoxml()
demoxml <- function() {
  grobid_dir <- system.file("extdata", package="papercheck")
  pattern <- "\\.xml$"
  file <- list.files(grobid_dir, pattern, full.names = TRUE)
  return(file)
}

#' Get demo directory of grobid XML files
#'
#' @return paths
#' @export
#'
#' @examples
#' demodir()
demodir <- function() {
  grobid_dir <- system.file("grobid", package="papercheck")
  return(grobid_dir)
}
