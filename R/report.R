#' Create a report
#'
#' @param paper a paper object
#' @param modules a vector of modules to run (names for built-in modules or paths for custom modules)
#' @param output_file the name of the output file
#' @param output_format the format to create the report in
#'
#' @return the file path the report is saved to
#' @export
#'
#' @examples
#' \donttest{
#' filename <- demoxml()
#' paper <- read(filename)
#' report(paper)
#' }
report <- function(paper,
                   modules = c("exact_p", "marginal", "effect_size", "statcheck", "retractionwatch", "ref_consistency"),
                   output_file = paste0(paper$name, "_report.", output_format),
                   output_format = c("qmd", "html", "pdf")) {
  output_format <- match.arg(output_format)

  # check paper has required things
  if (!"scivrs_paper" %in% class(paper)) {
    stop("The paper argument must be a paper object (e.g., created with `read()`)")
  }

  # check if modules are available ----
  mod_exists <- sapply(modules, module_find)

  # set up progress bar ----
  if (verbose()) {
    pb <- progress::progress_bar$new(
      total = length(modules) + 3,
      clear = FALSE,
      show_after = 0,
      format = ":what [:bar] :current/:total :elapsedfull"
    )
    pb$tick(0, tokens = list(what = "Running modules"))
  }

  # run each module ----
  module_output <- lapply(modules, \(module) {
    if (verbose())
      pb$tick(tokens = list(what = module))
      op <- tryCatch(module_run(paper, module),
             error = function(e) {
               report_items <- list(
                 module = module,
                 title = module,
                 table = NULL,
                 report = e$message,
                 summary_text = "This module failed to run",
                 traffic_light = "fail"
               )

               return(report_items)
             })
  })

  # set up report ----
  if (verbose())
    pb$tick(tokens = list(what = "Creating report"))

  ## read in report template ----
  report_template <- system.file("templates/_report.qmd",
                                 package = "metacheck")
  rt <- readLines(report_template)
  cut_after <- which(rt == "## Demo") - 1
  rt_head <- paste(rt[1:cut_after], collapse = "\n")
  # turn real % to %%, leave %s, %d, %f, %i
  rt_head <- gsub("\\%(?![sdfi])", "%%", rt_head, perl = TRUE)
  qmd_header <- sprintf(rt_head,
                        paper$info$title,
                        as.character(packageVersion("metacheck")),
                        Sys.Date())

  ## generate summary section ----
  summary_list <- sapply(module_output, \(x) {
    sprintf("- [%s](#%s){.%s}: %s  ",
            x$title,
            gsub("\\s", "-", tolower(x$title)),
            x$traffic_light %||% "info",
            x$summary_text %||% "")
  })
  summary_text <- sprintf("## Summary\n\n%s\n\n",
                          paste(summary_list, collapse = "\n"))

  ## format module reports ----
  module_reports <- sapply(module_output, module_report) |>
    paste(collapse = "\n\n") |>
    gsub("\\n{3,}", "\n\n", x = _)

  report_text <- paste(qmd_header,
                       summary_text,
                       module_reports,
                       sep = "\n\n")

  if (verbose())
    pb$tick(tokens = list(what = "Rendering Report"))
  if (output_format == "qmd") {
    write(report_text, output_file)
  } else {
    # render report ----
    temp_input <- tempfile(fileext = ".qmd")
    temp_output <- sub("qmd$", output_format, temp_input)

    ## clean up
    on.exit(unlink(temp_input))
    on.exit(unlink(temp_output)) # won't exist if rename works

    write(report_text, temp_input)

    tryCatch({
      quarto::quarto_render(input = temp_input,
                            quiet = TRUE,
                            output_format = output_format)
    }, error = function(e) {
      stop("There was an error rendering your report:\n", e$message)
    })

    file.rename(temp_output, output_file)
  }

  if (verbose())
    pb$tick(tokens = list(what = "Report Saved"))

  invisible(output_file)
}

#' Report from module output
#'
#' @param module_output the output of a `module_run()`
#' @param header header level (default 2)
#' @param maxrows the maximum number of table rows to print
#' @param trunc_cell truncate any cell to this number of characters
#'
#' @return text
#' @export
#'
#' @examples
#' filename <- demoxml()
#' paper <- read(filename)
#' op <- module_run(paper, "exact_p")
#' module_report(op) |> cat()
module_report <- function(module_output,
                          header = 2,
                          maxrows = Inf,
                          trunc_cell = Inf) {

  # set up header
  if (is.null(header)) {
    head <- ""
  } else if (header == 0) {
    head <- module_output$title
  } else if (header %in% 1:6) {
    head <- rep("#", header) |> paste(collapse = "") |>
      paste0(" ", module_output$title,
             " {.", module_output$traffic_light, "}")
  } else {
    head <- header
  }

  # set up report
  report <- module_output$report
  if (all(report == "")) report <- NULL

  paste0(c(head, report), collapse = "\n\n")
}



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
                         column = "body") {
  # convert vectors to a table
  if (is.atomic(table)) {
    table <- data.frame(table)
    colnames(table) <- ""
  }

  # return nothing if no table contents
  if (nrow(table) == 0 | ncol(table) == 0) {
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
DT::datatable(table, options, selection = "none", rownames = FALSE)
```
', column_loc, tbl_code, cd_code, scrollY)

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
