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

  if (output_format == "pdf") {
    format <- paste0("  pdf:\n",
                     "    toc: true\n")
  } else {
    format <- paste0("  html:\n",
                     "    theme: flatly\n",
                     "    toc: true\n",
                     "    toc-location: right\n",
                     "    page-layout: full\n",
                     "    title-block-banner: true\n",
                     "    link-external-newwindow: true\n",
                     "    embed-resources: true\n")
  }

  emojis <- metacheck::emojis
  head <- paste0("---\n",
                "title: MetaCheck Report\n",
                "subtitle: \"", paper$info$title, "\"\n",
                "date: ", Sys.Date(), "\n",
                "format:\n", format,
                "---\n\n",
                "<style>\n",
                "  .na::before     { content: '", emojis$tl_na,     " '; }\n",
                "  .fail::before   { content: '", emojis$tl_fail,   " '; }\n",
                "  .info::before   { content: '", emojis$tl_info,   " '; }\n",
                "  .red::before    { content: '", emojis$tl_red,    " '; }\n",
                "  .yellow::before { content: '", emojis$tl_yellow, " '; }\n",
                "  .green::before  { content: '", emojis$tl_green,  " '; }\n",
                "  section::before { content: '' !important; }\n",
                "  details summary { font-size: 150%; padding: 0.5em 0; }\n",
                "</style>\n\n",
                "::: {.column-margin}\n",
                "[MetaCheck](http://www.scienceverse.org/metacheck) version ",
                packageVersion("metacheck"), "<br><br>\n\n",
                emojis$tl_green,  " no problems detected;<br>\n",
                emojis$tl_yellow, " something to check;<br>\n",
                emojis$tl_red,    " possible problems detected;<br>\n",
                emojis$tl_info,   " informational only;<br>\n",
                emojis$tl_na,     " not applicable;<br>\n",
                emojis$tl_fail,   " check failed\n",
                ":::\n\n")

  summary_list <- sapply(module_output, \(x) {
    sprintf("- [%s](#%s){.%s}: %s  ",
            x$title,
            gsub("\\s", "-", tolower(x$title)),
            x$traffic_light %||% "info",
            x$summary_text %||% "")
  })
  summary_text <- sprintf("## Summary\n\n%s\n\n",
                          paste(summary_list, collapse = "\n"))

  body <- sapply(module_output, module_report) |>
    paste(collapse = "\n\n") |>
    gsub("\\n{3,}", "\n\n", x = _)

  if (verbose())
    pb$tick(tokens = list(what = "Rendering Report"))
  if (output_format == "qmd") {
    write(paste0(head, summary_text, body), output_file)
  } else {
    # render report ----
    temp_input <- tempfile(fileext = ".qmd")
    temp_output <- sub("qmd$", output_format, temp_input)
    write(paste0(head, summary_text, body), temp_input)

    tryCatch({
      quarto::quarto_render(input = temp_input,
                            quiet = TRUE,
                            output_format = output_format)
    }, error = function(e) {
      stop("There was an error rendering your report:\n", e$message)
    })

    file.rename(temp_output, output_file)
    unlink(temp_input) # clean up
  }
  if (verbose())
    pb$tick(tokens = list(what = "Report Saved"))

  return(output_file)
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
  if (report == "") report <- NULL

  paste0(c(head, report), collapse = "\n\n")
}



#' Make Scroll Table
#'
#' A helper function for making module reports.
#'
#' @param table the data frame to show in a table, or a vector for a list
#' @param scroll_above if the table has more rows than this, scroll
#' @param height the height of the scroll window
#'
#' @returns the markdown R chunk to create this table
#' @export
#'
#' @examples
#' scroll_table(LETTERS)
scroll_table <- function(table,
                         scroll_above = 2,
                         height = 200) {
  if (is.atomic(table)) {
    table <- data.frame(table)
    colnames(table) <- ""
  }
  tbl_code <- paste(deparse(table), collapse = "\n")

  scrollY <- ifelse(nrow(table) <= scroll_above, "",
                    sprintf(", scrollY = %d", height))

  ordering <- FALSE

  # generate markdown to create the table
  md <- sprintf('
```{r}
#| echo: false
# table data
table <- %s

# table options
options <- list(dom = "t", ordering = %s %s)

# display table
DT::datatable(table, options, selection = "none", rownames = FALSE)
```
', tbl_code, ordering, scrollY)

  return(md)
}

#' Make Collapsible Section
#'
#' A helper function for making module reports.
#'
#' @param text The text to put in the collapsible section; vectors will be collapse with line breaks between (e.g., into paragraphs)
#' @param title The title of the collapse header
#'
#' @returns text
#' @export
#'
#' @examples
#' text <- c("Paragraph 1...", "Paragraph 2...")
#' collapse_section(text) |> cat()
collapse_section <- function(text, title = "Learn More") {
  p <- paste0(text, collapse = "\n\n")
  fmt <- '
<details>
  <summary>%s</summary>
  <div>
%s
  </div>
</details>'

  sprintf(fmt, title, p)
}
