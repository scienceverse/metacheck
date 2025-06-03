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
#' \dontrun{
#' filename <- demoxml()
#' paper <- read_grobid(filename)
#' report(paper)
#' }
report <- function(paper,
                   modules = c("imprecise_p", "marginal", "statcheck", "osf_check", "retractionwatch", "ref_consistency"),
                   output_file = paste0(paper$name, "_report.", output_format),
                   output_format = c("qmd", "html", "pdf")) {
  output_format <- match.arg(output_format)

  # check paper has required things
  if (!"scivrs_paper" %in% class(paper)) {
    stop("The paper argument must be a paper object (e.g., created with `read_grobid()`)")
  }

  # check if modules are available ----
  builtin <- module_list()$name
  custom <- setdiff(modules, builtin)
  cm_exists <- sapply(custom, file.exists)
  if (any(!cm_exists)) {
    stop("Some modules are not available: ",
         paste(custom[!cm_exists], collapse = ", "))
  }

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
                     "    toc-float: true\n",
                     "    toc-location: left\n",
                     "    df-print: paged\n",
                     "    minimal: true\n",
                     "    embed-resources: true\n")
  }

  head <- paste0("---\n",
                "title: PaperCheck Report\n",
                "subtitle: \"", paper$info$title, "\"\n",
                "date: ", Sys.Date(), "\n",
                "format:\n", format,
                "---\n\n",
                "<style>\n",
                "  h2.na::before { content: '\u26aa\ufe0f '; }\n",
                "  h2.fail::before { content: '\u26ab\ufe0f '; }\n",
                "  h2.info::before { content: '\ud83d\udd35 '; }\n",
                "  h2.red::before { content: '\ud83d\udd34 '; }\n",
                "  h2.yellow::before { content: '\ud83d\udfe1 '; }\n",
                "  h2.green::before { content: '\ud83d\udfe2 '; }\n",
                "</style>\n\n",
                "::: {.column-margin}\n",
                "\ud83d\udfe2 no problems detected;<br>\n",
                "\ud83d\udfe1 something to check;<br>\n",
                "\ud83d\udd34 possible problems detected;<br>\n",
                "\ud83d\udd35 informational only;<br>\n",
                "\u26aa\ufe0f not applicable;<br>\n",
                "\u26ab\ufe0f check failed\n",
                ":::\n\n")

  body <- sapply(module_output, module_report) |>
    paste(collapse = "\n\n") |>
    gsub("\\n{3,}", "\n\n", x = _)

  if (verbose())
    pb$tick(tokens = list(what = "Rendering Report"))
  if (output_format == "qmd") {
    write(paste0(head, body), output_file)
  } else {
    # render report ----
    temp_input <- tempfile(fileext = ".qmd")
    temp_output <- sub("qmd$", output_format, temp_input)
    write(paste0(head, body), temp_input)

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
#' paper <- read_grobid(filename)
#' op <- module_run(paper, "imprecise_p")
#' module_report(op) |> cat()
module_report <- function(module_output,
                          header = 2,
                          maxrows = Inf,
                          trunc_cell = Inf) {
  # set up table
  if ("table" %in% names(module_output)) {
    tab <- module_output$tab
  } else {
    tab <- ""
  }
  # use summary table if available and reporting more than one paper
  if ("summary" %in% names(module_output) &&
      nrow(module_output$summary) > 1) {
    tab <- module_output$summary
  }


  rowwarning <- ""
  if (is.data.frame(tab)) {
    # get rid of the id column if only one ID
    if (unique(tab$id) |> length() < 2) tab$id <- NULL
    # get rid of all columns with only NA or header column
    keep <- sapply(tab, \(col) {!all(is.na(col))})
    keep <- setdiff(keep, "header")
    tab <- tab[, keep, drop = FALSE]

    if (nrow(tab) == 0 || ncol(tab) == 0) {
      tab <- ""
    } else {
      # truncate
      rows <- min(maxrows, nrow(tab))
      rowwarning <- paste("\n\nShowing", rows, "of", nrow(tab), "rows")
      tab <- tab[1:rows, ]
      if (trunc_cell < Inf) {
          char_cols <- names(tab)[sapply(tab, is.character)]
          for (col in char_cols) {
            needs_trunc <- which(nchar(tab[[col]]) > trunc_cell)
            tab[[col]][needs_trunc] <- tab[[col]][needs_trunc] |>
              substr(1, trunc_cell-3) |>
              paste0("...")
          }
      }

      tab <- tab |>
        knitr::kable(format = "markdown") |>
        as.character() |>
        paste(collapse = "\n")
    }
  }

  # set up header
  if (is.null(header) || header == "") {
    head <- ""
  } else if (header == 0) {
    head <- paste(module_output$title, "\n\n")
  } else if (header %in% 1:6) {
    head <- rep("#", header) |> paste(collapse = "") |>
      paste0(" ", module_output$title,
             " {.", module_output$traffic_light, "}\n\n")
  } else {
    head <- paste0(header, "\n\n")
  }

  # set up report
  report <- ""
  if (module_output$report != "")
    report <- paste0(module_output$report, "\n\n")

  paste0(head, report, tab, rowwarning)
}
