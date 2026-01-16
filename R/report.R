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
                               "funding_check",
                               "coi_check",
                               "power",
                               "code_check",
                               "stat_check",
                               "stat_p_exact",
                               "stat_p_nonsig",
                               "stat_effect_size",
                               "marginal",
                               "ref_doi_check",
                               "ref_replication",
                               "ref_retraction",
                               "ref_pubpeer"),
                   output_file = paste0(paper$name, "_report.", output_format),
                   output_format = c("html", "qmd"),
                   args = list()) {
  # error catching ----
  ## check output format
  output_format <- tolower(output_format[[1]])
  if (!output_format %in% c("html", "qmd")) {
    stop("The output_format must be either 'html' or 'qmd'.",
         call. = FALSE)
  }

  ## check if the output_file is valid
  # so the modules don't run then failure
  tryCatch(suppressWarnings( write("test", output_file) ),
           error = \(e) {
             stop("The output_file is not a valid path.", call. = FALSE)
           })

  ## check paper has required things
  if (!"scivrs_paper" %in% class(paper)) {
    stop("The paper argument must be a paper object (e.g., created with `read()`)", .call = FALSE)
  }

  ## check if modules are available
  mod_exists <- sapply(modules, module_find)

  # set up progress bar
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
             warning("Error in ", module)
             prev <- mod_args$paper$prev_outputs
             report_items <- list(
               module = module,
               title = module,
               table = NULL,
               report = e$message,
               summary_text = "This module failed to run",
               summary_table = mod_args$paper$summary_table,
               traffic_light = "fail",
               paper = paper,
               prev_outputs = prev
             )
             class(report_items) <- "metacheck_module_output"

             return(report_items)
           })
  }

  # pull last module output out
  module_output <- op$prev_outputs
  op$prev_outputs <- NULL
  op$paper <- NULL
  module_output[[op$module]] <- op

  # organise modules ----
  section_levels <- c("general", "intro", "method", "results", "discussion", "reference")
  sections <- sapply(module_output, \(mo) mo$section)
  sections <- factor(sections, section_levels)
  tl_levels <- c("red", "yellow", "green", "info", "na", "fail")
  tls <- sapply(module_output, \(mo) mo$traffic_light %||% "info")
  tls <- factor(tls, tl_levels)
  # this seems hacky, but I can't figure out how to sort by 2 vectors
  mod_order <- xtfrm(sections)*10 + xtfrm(tls)
  module_output <- sort_by(module_output, mod_order)

  # set up report ----
  pb$tick(tokens = list(what = "Creating report"))

  ## read in report template ----
  report_template <- system.file("templates/_report.qmd",
                                 package = "metacheck")
  rt <- readLines(report_template)
  cut_after <- which(rt == "<!-- Demo -->") - 1
  rt_head <- paste(rt[1:cut_after], collapse = "\n")
  # turn real % to %%, leave %s, %d, %f, %i
  rt_head <- gsub("\\%(?![sdfi])", "%%", rt_head, perl = TRUE)
  doi_text <- ifelse(paper$info$doi == "", "",
                     sprintf("DOI: [%s](https://doi.org/%s)", paper$info$doi, paper$info$doi))
  author_text <- utils::capture.output(print.scivrs_authors(paper$authors))
  if (length(author_text) == 0) author_text <- ""
  qmd_header <- sprintf(rt_head,
                        gsub('"', '\\\\"', paper$info$title),
                        author_text,
                        as.character(utils::packageVersion("metacheck")),
                        Sys.Date(),
                        doi_text)

  ## generate summary section ----
  summary_list <- sapply(module_output, \(x) {
    tl <- paste0("tl_", x$traffic_light %||% "info")
    summary_text <- x$summary_text %||% ""
    # indent 4 if first line is \n (probably a list)
    if (nzchar(summary_text) && substr(summary_text, 1, 1) == "\n") {
      summary_text <- gsub("\n", "\n    ", summary_text)
    }
    sprintf("- %s [%s](#%s){.%s}: %s  ",
            emojis[[tl]],
            x$title,
            gsub("\\s", "-", tolower(x$title)),
            x$traffic_light %||% "info",
            summary_text)
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
                       "\n", # prevent incomplete final line warnings
                       sep = "\n\n")

  pb$tick(tokens = list(what = "Rendering Report"))
  if (output_format == "qmd") {
    write(report_text, output_file)
    save_path <- output_file
  } else {
    # render report ----
    temp_input <- tempfile(fileext = ".qmd")
    temp_output <- sub("qmd$", output_format, temp_input)

    ## clean up
    on.exit(unlink(temp_input))
    on.exit(unlink(temp_output)) # won't exist if rename works

    write(report_text, temp_input)

    save_path <- tryCatch({
      quarto::quarto_render(input = temp_input,
                            quiet = TRUE,
                            output_format = output_format)
      file.rename(temp_output, output_file)
      output_file
    }, error = function(e) {
      # save the qmd on render error and return its path
      output_qmd <- output_file |>
        gsub("\\.html$", "", x = _) |>
        paste0(".qmd")
      write(report_text, output_qmd)
      warning("There was an error rendering your report:\n", e$message,
              "\n\nSee the following for the quarto file:\n", output_qmd,
              call. = FALSE)
      return(output_qmd)
    })
  }

  pb$tick(tokens = list(what = "Report Saved"))

  invisible(save_path)
}

#' Report from module output
#'
#' @param module_output the output of a `module_run()`
#' @param header header level (default 2)
#'
#' @return text
#' @export
#'
#' @examples
#' filename <- demoxml()
#' paper <- read(filename)
#' op <- module_run(paper, "stat_p_exact")
#' module_report(op) |> cat()
module_report <- function(module_output,
                          header = 3) {
  n <- NULL

  # set up header
  tl <- module_output$traffic_light %||% "info"
  tl_symbol <- emojis[[paste0("tl_", tl)]]
  if (is.null(header)) {
    head <- ""
  } else if (header == 0) {
    head <- sprintf("%s %s", tl_symbol, module_output$title)
  } else if (header %in% 1:6) {
    head <- sprintf("%s %s %s {#%s .%s}",
                    rep("#", header) |> paste(collapse = ""),
                    tl_symbol,
                    module_output$title,
                    gsub(" ", "-", tolower(module_output$title)),
                    tl)
  } else {
    head <- header
  }

  # set up report
  report <- module_output$report %||% module_output$summary_text
  if (all(report == "")) report <- NULL

  # how it works
  hiw <- tryCatch({
    info <- module_info(module_output$module)

    author_ack <- tryCatch({
      if (!is.null(info$author)) {
        a <- info$author |>
          gsub("\\s*\\(.*email\\{.+\\})", "", x = _)
        authors <- if (length(a) < 3) {
          paste(a, collapse = " and ")
        } else {
          n <- length(a)
          paste0(paste(a[-n], collapse = ", "), " and ", a[n])
        }
        sprintf("This module was developed by %s", authors)
      }
    })

    c(info$description, info$details, author_ack) |>
      collapse_section("How It Works", callout = "note")
    }, error = \(e) { return(NULL) })

  # create collapsible boxes around substantial reports (> 300 char)
  pre <- paste(module_output$summary_text,
               "<details><summary>View detailed feedback</summary><div>",
               sep = "\n\n")
  post <- "</div></details>"
  if (is.null(report) ||
      all(module_output$summary_text == report) ||
      paste(report, collapse = "\n\n") |> nchar() < 300) {
    pre <- post <- NULL
  }

  paste0(c(head, pre, report, post, hiw), collapse = "\n\n")
}
