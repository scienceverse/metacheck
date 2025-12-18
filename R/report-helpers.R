#' Make Scroll Table
#'
#' A helper function for making module reports.
#'
#' See [quarto article layout](https://quarto.org/docs/authoring/article-layout.html) for column options. The most common are "body" (centre column), "page" (span all columns"), and "margin" (only in right margin).
#'
#' To set colwidths, use a numeric or character vector. For a numeric vector, numbers greater than 1 wil be interpreted as pixels, less than 1 as percents. Character vectors will be passed as is (e.g., "3em"). If you only want to specify some columns, set the others to NA, like c(200, NA, NA, NA). Vectors shorter than the number of columns will be recycled.
#'
#' @param table the data frame to show in a table, or a vector for a list
#' @param colwidths set column widths as a vector of px (number > 1) or percent (numbers <= 1)
#' @param scroll_above if the table has more rows than this, scroll
#' @param height the height of the scroll window
#' @param escape whether or not to escape the DT (necessary if using raw html)
#' @param column which quarto column to show tables in
#'
#' @returns the markdown R chunk to create this table
#' @export
#'
#' @examples
#' scroll_table(LETTERS)
scroll_table <- function(table,
                         colwidths = "auto",
                         scroll_above = 2,
                         height = 200,
                         escape = FALSE,
                         column = "body") {
  # convert vectors to a table
  if (is.atomic(table)) {
    table <- data.frame(table)
    colnames(table) <- ""
  }

  # return nothing if no table contents
  if (is.null(table) || nrow(table) == 0 || ncol(table) == 0) {
    return("")
  }

  tbl_code <- paste(deparse(table), collapse = "\n")

  scrollY <- ifelse(nrow(table) <= scroll_above, "",
                    sprintf(", scrollY = %d", height))

  column_loc <- ""
  if (column != "body") {
    column_loc <- paste0("#| column: ", column)
  }

  # columnDef
  if (length(colwidths) == 1 && colwidths == "auto") {
    cd_code <- "list()"
  } else {
    colwidths <- rep_len(colwidths, ncol(table))
    cd <- lapply(seq_along(colwidths), \(i) {
      x <- colwidths[[i]]
      if (is.na(x)) return(NULL)

      if (is.numeric(x)) {
        if (x > 1) {
          x <- paste0(x, "px")
        } else {
          x <- paste0(x*100, "%")
        }
      }
      # targets are 0-based
      list(targets = i-1, width = x)
    })
    cd_code <- paste(deparse(cd[!sapply(cd, is.null)]), collapse = "\n")
  }

  # generate markdown to create the table
  md <- sprintf('
```{r}
#| echo: false
%s
# table data
table <- %s

# table options
cd <- %s
options <- list(dom = "t", ordering = FALSE, columnDefs = cd %s)

# display table
DT::datatable(table, options, selection = "none", rownames = FALSE, escape = %s)
```
', column_loc, tbl_code, cd_code, scrollY,
                ifelse(isTRUE(escape), "TRUE", "FALSE"))

  return(md)
}

#' Make Collapsible Section
#'
#' A helper function for making module reports.
#'
#' @param text The text to put in the collapsible section; vectors will be collapse with line breaks between (e.g., into paragraphs)
#' @param title The title of the collapse header
#' @param callout the type of quarto callout block
#' @param collapse whether to collapse the block at the start
#'
#' @returns text
#' @export
#'
#' @examples
#' text <- c("Paragraph 1...", "Paragraph 2...")
#' collapse_section(text) |> cat()
collapse_section <- function(text, title = "Learn More",
                             callout = c("tip", "note", "warning", "important", "caution"),
                             collapse = TRUE) {
  fmt <- '::: {.callout-%s title="%s" collapse="%s"}\n\n%s\n\n:::\n'

  sprintf(
    fmt,
    match.arg(callout),
    title,
    ifelse(collapse, "true", "false"),
    paste0(text, collapse = "\n\n")
  )
}

#' Pluralise
#'
#' Helper function for conditional plurals. For example, if you want to return "1 error" or "2 errors", you can use this in a sprintf().
#'
#' @param n the number
#' @param singular the word or ending when n = 1
#' @param plural the word or ending n != 1
#'
#' @returns a string
#' @export
#'
#' @examples
#' n <- 0:3
#' sprintf("I have %d friend%s", n, plural(n))
#' sprintf("I have %d %s", n, plural(n, "octopus", "octopi"))
plural <- function(n, singular = "", plural = "s") {
  ifelse(n == 1, singular, plural)
}


#' Make an html link
#'
#' @param url the URL to link to
#' @param text the text to link
#' @param new_window whether to open in a new window
#'
#' @returns string
#' @export
#'
#' @examples
#' link("https://scienceverse.org")
link <- function(url, text = url, new_window = TRUE) {
  nw <- ""
  text <- gsub("^https?://", "", text)
  if (new_window) nw <- " target='_blank'"
  links <- sprintf("<a href='%s'%s>%s</a>",
                   url, nw, text)
  links[is.na(url)] <- NA

  return(links)
}


#' Format Reference
#'
#' Format a reference for display in a report.
#'
#' The argument `bib` should be a bibentry object (e.g., like those made by `citation()`, but it can also handle a bibtex object or a bibtex formatted character vector. If these do not read in as valid bibtex, the original text of bib will be returned unformatted.
#'
#' @param bib a bibentry object or list of bibentry objects
#'
#' @returns formatted text
#' @export
#'
#' @examples
#' mc <- citation("metacheck")
#' format_ref(mc)
#'
#' # handles bibtext
#' bib_mc <- utils::toBibtex(mc)
#' format_ref(bib_mc)
#'
#' paper <- read(demoxml())
#' format_ref(paper$bib$ref[1:2])
format_ref <- function(bib) {
  if (!all(sapply(bib, inherits, "bibentry"))) {
    # try parsing as bibtex
    tmpfile <- tempfile(fileext = ".bib")
    writeLines(bib, tmpfile)
    bib <- tryCatch(bibtex::read.bib(tmpfile),
                    error = \(e) { return(bib) })
  }

  # handle list of bibentries
  if (!inherits(bib, "bibentry") && is.list(bib)) {
    bib <- Reduce(c, bib)
  }

  formatted <- format(bib, style = "md")

  # tidy up
  gsub("\\n", " ", trimws(formatted))
}

