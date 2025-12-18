#' Create a Report
#'
#' Run specified modules on a paper and generate a report in quarto (qmd), html, or pdf format.
#'
#' Pass arguments to modules in a named list of lists, using the same names as the `modules` argument. You only need to specify modules with arguments.
#' ```
#' args <- list(power = list(seed = 8675309))
#' ```
#'
#' @param paper a paper object
#' @param modules a vector of modules to run (names for built-in modules or paths for custom modules)
#' @param output_file the name of the output file
#' @param output_format the format to create the report in
#' @param args a list of arguments to pass to modules (see Details)
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
                   modules = c("prereg_check",
                               "power",
                               "exact_p",
                               "nonsig_p",
                               "marginal",
                               "effect_size",
                               "code_check",
                               "statcheck",
                               "reference_check"),
                   output_file = paste0(paper$name, "_report.", output_format),
                   output_format = c("qmd", "html", "pdf"),
                   args = list()) {
  output_format <- match.arg(output_format)

  # check paper has required things
  if (!"scivrs_paper" %in% class(paper)) {
    stop("The paper argument must be a paper object (e.g., created with `read()`)")
  }

  # check if modules are available ----
  mod_exists <- sapply(modules, module_find)

  # set up progress bar ----
  pb <- pb(length(modules) + 3,
           ":what [:bar] :current/:total :elapsedfull")
  pb$tick(0, tokens = list(what = "Running modules"))

  # run each module ----
  #module_output <- lapply(modules, \(module) {
  op <- paper
  for (module in modules) {
    pb$tick(tokens = list(what = module))
    mod_args <- args[[module]] %||% list()
    mod_args$paper <- op
    mod_args$module <- module

    op <- tryCatch(do.call(module_run, mod_args),
           error = function(e) {
             report_items <- list(
               module = module,
               title = module,
               table = NULL,
               report = e$message,
               summary_text = "This module failed to run",
               traffic_light = "fail",
               paper = op$paper %||% op,
               prev_outputs = paper$prev_outputs
             )

             return(report_items)
           })
  }

  # pull module output out
  module_output <- op$prev_outputs
  op$prev_outputs <- NULL
  op$paper <- NULL
  module_output[[op$module]] <- op

  # organise modules ----
  section_levels <- c("general", "intro", "method", "results", "discussion", "reference")
  sections <- sapply(module_output, \(mo) mo$section)
  sections <- factor(sections, section_levels)
  module_output <- sort_by(module_output, sections)

  # set up report ----
  pb$tick(tokens = list(what = "Creating report"))

  ## read in report template ----
  report_template <- system.file("templates/_report.qmd",
                                 package = "metacheck")
  rt <- readLines(report_template)
  cut_after <- which(rt == "## Demo") - 1
  rt_head <- paste(rt[1:cut_after], collapse = "\n")
  # turn real % to %%, leave %s, %d, %f, %i
  rt_head <- gsub("\\%(?![sdfi])", "%%", rt_head, perl = TRUE)
  doi_text <- ifelse(paper$info$doi == "", "",
                     sprintf("DOI: [%s](https://doi.org/%s)", paper$info$doi, paper$info$doi))
  qmd_header <- sprintf(rt_head,
                        paper$info$title,
                        as.character(utils::packageVersion("metacheck")),
                        Sys.Date(),
                        doi_text)

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
  module_reports <- sapply(section_levels, \(sec) {
    this_section <- sapply(module_output, `[[`, "section") == sec
    if (!any(this_section)) return(NULL)

    section_op <- module_output[this_section]
    mr <- sapply(section_op, module_report)

    title <- sprintf("## %s%s Modules",
                     toupper(substr(sec, 1, 1)),
                     substr(sec, 2, nchar(sec)))
    c(title, mr)
  }) |>
    unlist() |>
    paste(collapse = "\n\n") |>
    gsub("\\n{3,}", "\n\n", x = _)

  report_text <- paste(qmd_header,
                       summary_text,
                       module_reports,
                       sep = "\n\n")

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
                          header = 3,
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

