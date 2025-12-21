#' Default value for `NULL`
#'
#' This infix function makes it easy to replace `NULL`s with a default value. It's inspired by the way that Ruby's or operation (`||`) works.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns `x`.
#' @export
#' @keywords internal
#' @name op-null-default
#' @examples
#' 1 %||% 2
#' NULL %||% 2
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
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
  if (verbose()) {
    if (interactive()) {
      # not in knitr environment
      base::message("\033[32m", ..., "\033[39m",
                    domain = domain, appendLF = appendLF)
    } else {
      base::message(..., domain = domain, appendLF = appendLF)
    }
  }
}

#' Set or get metacheck verbosity
#'
#' @param verbose if logical, sets whether to show verbose output messages and progress bars
#'
#' @returns the current option value (logical)
#' @export
#'
#' @examples
#' verbose()
verbose <- function(verbose = NULL) {
  if (is.null(verbose)) {
    return(getOption("metacheck.verbose"))
  } else if (as.logical(verbose) %in% c(TRUE, FALSE)) {
    options(metacheck.verbose = as.logical(verbose))
    invisible(getOption("metacheck.verbose"))
  } else {
    stop("set verbose with TRUE or FALSE")
  }
}

#' Set or get email
#'
#' @param email if a string, sets the email
#'
#' @returns the current option value (character)
#' @export
#'
#' @examples
#' email()
email <- function(email = NULL) {
  if (is.null(email)) {
    email <- getOption("metacheck.email") %||% "metacheck@scienceverse.org"
    return(email)
  } else if (is.character(email) && grepl(".+@.+\\..+$", email)) {
    options(metacheck.email = email)
    invisible(getOption("metacheck.email"))
  } else {
    stop("Set email with a valid email address")
  }
}

#' Check if the host of a URL is online
#'
#' @param url a URL to check
#'
#' @returns boolean
#' @export
#'
#' @examples
#' online()
online <- function(url = "google.com") {
  host <- urltools::domain(url)
  !is.null(curl::nslookup(host, error = FALSE))
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
#' biblio <- concat_tables(psychsci[1:10], "bib")
#' xrefs <- concat_tables(psychsci[1:10], "xrefs")
concat_tables <- function(papers, name_path) {
  if (!is_paper_list(papers)) papers <- list(papers)

  table_list <- papers #
  for (name in name_path) {
    table_list <- lapply(table_list, `[[`, name)
  }
  for (i in seq_along(papers)) {
    x <- table_list[[i]]
    if (is.data.frame(x) && nrow(x) > 0) {
      table_list[[i]]$id <- papers[[i]]$id
    }
  }

  merged_table <- do.call(rbind, table_list)
  rownames(merged_table) <- NULL

  merged_table
}


#' Detect a paper object
#'
#' @param paper the object to test
#'
#' @returns logical
#' @export
#' @keywords internal
is_paper <- function(paper) {
  if (!is.list(paper)) return(FALSE)
  is_paper <- inherits(paper, "scivrs_paper")

  return(is_paper)
}

#' Detect a list of paper objects
#'
#' @param paper the object to test
#'
#' @returns logical
#' @export
#' @keywords internal
is_paper_list <- function(paper) {
  if (!is.list(paper)) return(FALSE)

  is_paper <- sapply(paper, inherits, what = "scivrs_paper")
  if (all(is_paper)) return(TRUE)

  return(FALSE)
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
  underline <- rep("-", nchar(x$id)) |> paste(collapse="")
  txt <- sprintf("%s\n%s\n%s\n\n%s\n\n* Sections: %d\n* Sentences: %d\n* Bibliography: %d\n* X-Refs: %d\n\n",
                 underline, x$id, underline,
                 x$info$title %||% "{No title}",
                 max(c(0, x$full_text$div)),
                 nrow(x$full_text),
                 nrow(x$bib),
                 nrow(x$xrefs))

  cat(txt)
}

#' Print PaperList Object
#'
#' @param x The scivrs_paperlist object
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
print.scivrs_paperlist <- function(x, ...) {
  txt <- info_table(x, c("title", "doi"))

  print(txt)
}

#' Subset PaperList Object
#'
#' @param x The scivrs_paperlist object
#' @param ... Additional parameters for print
#' @param drop relevant for matrices and arrays. If TRUE the result is coerced to the lowest possible dimension (see the examples).
#'
#' @export
#' @keywords internal
`[.scivrs_paperlist` <- function(x, ..., drop=TRUE) {
  paperlist(NextMethod())
}




#' Get demo PDF file
#'
#' @return vector of paths
#' @export
#'
#' @examples
#' demopdf()
demopdf <- function() {
  grobid_dir <- system.file("extdata", package="metacheck")
  pattern <- "to_err_is_human\\.pdf$"
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
  grobid_dir <- system.file("extdata", package="metacheck")
  pattern <- "to_err_is_human\\.xml$"
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
  grobid_dir <- system.file("grobid", package="metacheck")
  return(grobid_dir)
}


#' Psychologial Science Open Access Paper Set
#'
#' 250 open access papers from Psychological Science.
#'
#' @format A list of 250 paper objects
#' @source \url{https://journals.sagepub.com/home/pss}
"psychsci"


#' Progress Bar
#'
#' @param total total number of ticks
#' @param format The format of the progress bar
#'
#' @returns a function
#' @export
pb <- function(total, format = "[:bar] :percent") {
  if (verbose()) {
    pb <- progress::progress_bar$new(
      total = total, clear = FALSE,
      format = format,
      show_after = 0
    )
    pb$tick(0)
    # Sys.sleep(0.2)
    # pb$tick(0)
  } else {
    # dummy functions so we don't have to call if (verbose())
    pb <- list(
      tick = function(...) { invisible() },
      message = function(...) { invisible() }
    )
  }

  return(pb)
}
