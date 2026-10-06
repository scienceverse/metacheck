# Directory holding a Pandoc binary rmarkdown::render() can use, for the
# report_type("simple") render path (see report_qmd()'s own comment for why
# that path avoids quarto::quarto_render()). rmarkdown::pandoc_available()
# only finds a standalone Pandoc install or one RStudio has already pointed
# it at via the RSTUDIO_PANDOC env var (RStudio sets that for its own
# bundled copy when running inside the IDE) -- in a plain Rscript/terminal
# session with no standalone Pandoc, confirmed to return FALSE with nothing
# set. Rather than require a second, separate Pandoc install just for this
# one render path, reuse the copy Quarto already bundles (report() already
# depends on Quarto for the full-report path) via quarto::quarto_path(),
# whose directory layout is <quarto_root>/bin/quarto(.exe) with Pandoc at
# <quarto_root>/bin/tools/. Returns NULL (not an error) if that binary or
# layout cannot be found, so the caller can still try rmarkdown's own
# default detection rather than fail outright.
.report_pandoc_dir <- function() {
  quarto_bin <- tryCatch(quarto::quarto_path(), error = function(e) NULL)
  if (is.null(quarto_bin) || !nzchar(quarto_bin)) return(NULL)

  pandoc_dir <- file.path(dirname(quarto_bin), "tools")
  has_pandoc <- file.exists(file.path(pandoc_dir, "pandoc")) ||
    file.exists(file.path(pandoc_dir, "pandoc.exe"))
  if (!dir.exists(pandoc_dir) || !has_pandoc) return(NULL)

  pandoc_dir
}

#' Create a Report
#'
#' Run specified modules on a paper and generate a report in quarto (qmd), html, or pdf format.
#'
#' Pass arguments to modules in a named list of lists, using the same names as the `modules` argument. You only need to specify modules with arguments.
#' ```
#' args <- list(power = list(seed = 8675309))
#' ```
#'
#' @param paper a paper object or a paperlist object
#' @param modules a vector of modules to run (names for built-in modules or paths for custom modules)
#' @param output_file the name of the output file
#' @param output_format the format to create the report in
#' @param report_type `"full"` (the default) for the normal interactive
#'   report (theme toggle, sortable/paginated JavaScript tables, tabbed
#'   sections), or `"simple"` for a static report with none of that -- plain
#'   tables, stacked headings instead of tabs, a single theme, no embedded
#'   JavaScript at all. Use `"simple"` for a report you plan to email: mail
#'   clients and scanners routinely flag the JavaScript a full report embeds
#'   (even though it is inert without a browser). See [report_type()].
#' @param args a list of arguments to pass to modules (see Details)
#'
#' @return the module output, invisibly, with the report's file path in its
#'   `save_path` attribute (a vector of paths, one per paper, when `paper` is
#'   a paperlist)
#' @export
#'
#' @examples
#' \dontrun{
#' paper <- demopaper()
#' report(paper)
#' report(paper, report_type = "simple") # email-safe, no embedded JS
#' }
report <- function(paper,
                   modules = c(
                     "prereg_check",
                     "funding_check",
                     "coi_check",
                     "power",
                     "repo_check",
                     "code_check",
                     "stat_check",
                     "stat_p_exact",
                     "stat_p_nonsig",
                     "stat_effect_size",
                     "marginal",
                     "ref_accuracy",
                     "ref_replication",
                     "ref_retraction",
                     "ref_pubpeer",
                     "ref_summary"
                   ),
                   output_file = paste0(paper$paper_id,
                                        "_report.",
                                        output_format),
                   output_format = c("html", "qmd"),
                   report_type = c("full", "simple"),
                   args = list()) {
  # error catching ----
  ## check output format
  output_format <- tolower(output_format[[1]])
  if (!output_format %in% c("html", "qmd")) {
    stop("The output_format must be either 'html' or 'qmd'.",
      call. = FALSE
    )
  }

  ## check report type, and set it for the duration of this call (module code
  ## that builds tables/tabsets, e.g. report_table(), checks report_type()
  ## itself -- see its own docs for why that cannot be a plain argument).
  ## Restored on exit, including on error, so it never leaks into the
  ## caller's session.
  report_type <- tolower(report_type[[1]])
  if (!report_type %in% c("full", "brief", "simple", "simple_brief")) {
    stop("The report_type must be one of 'full', 'brief', 'simple', or 'simple_brief'.",
      call. = FALSE
    )
  }
  if (.report_is_static(report_type) && output_format != "qmd" &&
      !requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("report_type = '", report_type, "' needs the 'rmarkdown' package. ",
      "Install it with install.packages('rmarkdown').",
      call. = FALSE
    )
  }
  prev_report_type <- metacheck::report_type()
  metacheck::report_type(report_type)
  on.exit(metacheck::report_type(prev_report_type), add = TRUE)

  ## check if modules are available
  mod_exists <- sapply(modules, module_find)

  # vectorise over papers ----
  if (.is_paper_list(paper) && length(paper) == 1) {
    # treat length-1 paperlist like 1 paper
    paper <- paper[[1]]
  } else if (.is_paper_list(paper)) {
    ## set up progress bar ----
    pb <- pb(
      length(paper),
      ":what [:bar] :current/:total :elapsedfull"
    )
    pb$tick(0, tokens = list(what = "Creating reports"))

    if (length(output_file) != length(paper)) {
      bn <- paste0(names(paper), x = basename(output_file[[1]]))
      output_file <- dirname(output_file[[1]]) |> file.path(bn)
    }

    reports <- mapply(\(x, of) {
      r <- tryCatch(report(x, modules, of, output_format, report_type, args),
        error = \(e) {
          logger("report", list(paper = x$id, error = e$message))
          warning("Error in ", x$id, ":\n", e$message,
            call. = FALSE
          )
          return(NULL)
        }
      )
      pb$tick(tokens = list(what = x$id))
      r
    }, x = paper, of = output_file, SIMPLIFY = FALSE)

    attr(reports, "save_path") <- sapply(reports, attr, "save_path")

    return(invisible(reports))
  }

  ## check if the output_file is valid
  # so the modules don't run then failure
  tryCatch(suppressWarnings(write("test", output_file)),
    error = \(e) {
      stop("The output_file is not a valid path.", call. = FALSE)
    }
  )

  ## check paper has required things
  if (!"scivrs_paper" %in% class(paper)) {
    stop("The paper argument must be a paper object (e.g., created with `read()`)", call. = FALSE)
  }

  # run modules ----
  module_output <- report_module_run(paper, modules, args)

  # set up report ----
  report_text <- report_qmd(module_output, paper)

  if (output_format == "qmd") {
    write(report_text, output_file)
    save_path <- output_file
  } else if (.report_is_static(report_type)) {
    # Simple-mode rendering goes around Quarto's HTML format entirely (see
    # report_qmd()'s own comment for why) -- rmarkdown::render() on the plain
    # R Markdown text built above, using the SAME Pandoc binary Quarto
    # bundles (.report_pandoc_dir()), since requiring a second, separate
    # Pandoc install just for this would be a real new dependency.
    temp_input <- tempfile(fileext = ".Rmd")
    temp_output <- sub("Rmd$", output_format, temp_input)

    on.exit(unlink(temp_input), add = TRUE)
    on.exit(unlink(temp_output), add = TRUE)

    write(report_text, temp_input)

    save_path <- tryCatch(
      {
        pandoc_dir <- .report_pandoc_dir()
        prev_pandoc_env <- Sys.getenv("RSTUDIO_PANDOC", unset = NA)
        if (!is.null(pandoc_dir)) Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
        on.exit({
          if (is.na(prev_pandoc_env)) Sys.unsetenv("RSTUDIO_PANDOC")
          else Sys.setenv(RSTUDIO_PANDOC = prev_pandoc_env)
        }, add = TRUE)

        rmarkdown::render(
          input = temp_input,
          output_file = basename(temp_output),
          output_dir = dirname(temp_output),
          quiet = TRUE,
          envir = new.env(parent = globalenv())
        )
        file.rename(temp_output, output_file)
        output_file
      },
      error = function(e) {
        # save the Rmd on render error and return its path
        output_rmd <- output_file |>
          gsub("\\.html$", "", x = _) |>
          paste0(".Rmd")
        write(report_text, output_rmd)

        logger("rmarkdown render", list(paper = paper$paper_id,
                                        rmd = output_rmd,
                                        error = e$message))

        warning("There was an error rendering your report:\n", e$message,
          "\n\nSee the following for the R Markdown file:\n", output_rmd,
          call. = FALSE
        )
        return(output_rmd)
      }
    )
  } else {
    # render report ----
    temp_input <- tempfile(fileext = ".qmd")
    temp_output <- sub("qmd$", output_format, temp_input)

    ## clean up
    # add = TRUE on both: a bare on.exit() call REPLACES every handler
    # already registered in this call (including temp_input's own cleanup
    # just below, and report_type's restoration above) rather than adding to
    # them -- confirmed to have silently skipped temp_input's unlink() even
    # before report_type existed, since the very next on.exit() call wiped it.
    on.exit(unlink(temp_input), add = TRUE)
    on.exit(unlink(temp_output), add = TRUE) # won't exist if rename works

    write(report_text, temp_input)

    save_path <- tryCatch(
      {
        quarto::quarto_render(
          input = temp_input,
          quiet = TRUE,
          output_format = output_format
        )
        file.rename(temp_output, output_file)
        output_file
      },
      error = function(e) {
        # save the qmd on render error and return its path
        output_qmd <- output_file |>
          gsub("\\.html$", "", x = _) |>
          paste0(".qmd")
        write(report_text, output_qmd)

        logger("quarto render", list(paper = paper$paper_id,
                                     quarto = output_qmd,
                                     error = e$message))

        warning("There was an error rendering your report:\n", e$message,
          "\n\nSee the following for the quarto file:\n", output_qmd,
          call. = FALSE
        )
        return(output_qmd)
      }
    )
  }

  attr(module_output, "save_path") <- save_path

  invisible(module_output)
}

#' Create a Report for a Local Repository
#'
#' Runs the repository modules on a folder of files on your own computer and
#' writes a single report. Use it on a repository you have downloaded (for
#' example with [osf_file_download()]) to see what was shared and what could be
#' improved, before archiving the files somewhere permanent.
#'
#' Four modules run in order, each building on the one before it:
#' `repo_check` takes an inventory of the files,
#' `code_check` reads the analysis scripts,
#' `data_check` reads the data files and runs data-quality checks,
#' and `codebook_check` checks whether the data columns are
#' documented. Only the first is told where the files are; the rest reuse its
#' results.
#'
#' Nothing is downloaded and no links are followed: only the folder you name is
#' read. Whether a language model is used is decided by [llm_use()], exactly as
#' when running the modules individually.
#'
#' @param path path to the repository folder to check
#' @param output_file the name of the output file. Defaults to the folder's own
#'   name with `_report.html` appended, written to the working directory. Give a
#'   path here to write it somewhere else; any folders in that path must already
#'   exist.
#' @param output_format the format to create the report in, `"html"` (the
#'   default) or `"qmd"`
#' @param report_type `"full"` (the default) or `"simple"` (a static,
#'   email-safe report with no embedded JavaScript) -- see [report()] and
#'   [report_type()].
#' @param modules the modules to run. Defaults to the four repository modules,
#'   in the order they depend on each other. Change it to run fewer.
#' @param args a list of extra arguments to pass to modules, named by module
#'   (see [report()]). `local_path` and `local_only` are set for you.
#'
#' @return the module output, invisibly, with the report's file path in its
#'   `save_path` attribute
#' @export
#'
#' @examples
#' \dontrun{
#' # check a folder you have downloaded
#' report_repository("how_many_registered_studies_are_published")
#'
#' # write the report somewhere else
#' report_repository("my_study", output_file = "reports/my_study.html")
#'
#' # a static, email-safe report
#' report_repository("my_study", report_type = "simple")
#' }
report_repository <- function(path,
                              output_file = NULL,
                              output_format = c("html", "qmd"),
                              report_type = c("full", "simple"),
                              modules = c("repo_check", "code_check",
                                          "data_check", "codebook_check"),
                              args = list()) {
  output_format <- tolower(output_format[[1]])
  report_type <- tolower(report_type[[1]])

  ## error checking ----
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("`path` must be a single path to a repository folder", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop("No folder found at ", path,
         ".\nCheck the path, or download the repository first with ",
         "osf_file_download().", call. = FALSE)
  }

  ## default the report name to the folder's own name ----
  # normalizePath so a trailing slash or "." resolves to a real folder name
  # rather than an empty string or a dot.
  full_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (is.null(output_file)) {
    output_file <- paste0(basename(full_path), "_report.", output_format)
  }

  ## the first module reads the folder; the rest reuse its results ----
  # Passing local_path to a later module would force it to re-run repo_check
  # from scratch, so it is set on the first module only. local_only stops any
  # link in the paper object being followed -- there is no paper here, but it
  # also skips the link searches entirely.
  first <- modules[[1]]
  args[[first]] <- utils::modifyList(
    args[[first]] %||% list(),
    list(local_path = full_path, local_only = TRUE)
  )

  # There is no manuscript here, so the paper object is an empty stand-in for
  # the modules to hang their results on. Its title becomes the report's
  # subtitle, so name it after the folder rather than leaving it "Test Paper".
  paper <- test_paper()
  paper$info$title <- basename(full_path)

  report(
    paper = paper,
    modules = modules,
    output_file = output_file,
    output_format = output_format,
    report_type = report_type,
    args = args
  )
}

#' Run modules for a report
#'
#' Runs modules in order on the paper and orders by section and traffic light.
#'
#' Pass arguments to modules in a named list of lists, using the same names as the `modules` argument. You only need to specify modules with arguments.
#' ```
#' args <- list(power = list(seed = 8675309))
#' ```
#'
#' @param paper a paper object
#' @param modules a vector of modules to run
#' @param args optional list of arguments to pass to modules
#'
#' @returns a list of module outputs
#' @export
#'
#' @examples
#' paper <- demopaper()
#' modules <- c("stat_p_exact", "stat_p_nonsig")
#' module_output <- report_module_run(paper, modules)
report_module_run <- function(paper, modules, args = list()) {
  # set up progress bar
  pb <- pb(
    length(modules),
    ":what [:bar] :current/:total :elapsedfull"
  )
  on.exit(pb$terminate())
  pb$tick(0, tokens = list(what = "Running modules"))

  # run each module ----
  # module_output <- lapply(modules, \(module) {
  op <- paper
  for (module in modules) {
    # Label the bar with the module about to run (zero-advance) so a slow module
    # — e.g. one making LLM calls — is attributed to its own name, not the next
    # module's. The bar advances only after the module returns (below).
    pb$tick(0, tokens = list(what = module))
    mod_args <- args[[module]] %||% list()
    mod_args$paper <- op
    mod_args$module <- module

    op <- tryCatch(do.call(module_run, mod_args),
      error = function(e) {
        warning("Error in ", module, call. = FALSE)
        prev <- list()
        if (inherits(mod_args$paper, "metacheck_module_output")) {
          prev <- mod_args$paper$prev_outputs %||% list()
          this_out <- mod_args$paper
          this_out$prev_outputs <- NULL
          this_out$paper <- NULL
          prev[[this_out$module]] <- this_out
        }
        report_items <- list(
          module = module,
          title = module,
          table = NULL,
          report = e$message,
          summary_text = "This module failed to run",
          summary_table = mod_args$paper$summary_table %||% data.frame(
            paper_id = paper$paper_id),
          traffic_light = "fail",
          paper = paper,
          prev_outputs = prev
        )
        class(report_items) <- "metacheck_module_output"

        return(report_items)
      }
    )
    # Advance now that the module has finished, so its elapsed time is charged to
    # its own label rather than bleeding into the next module's.
    pb$tick(tokens = list(what = module))
  }

  # pull last module output out
  module_output <- op$prev_outputs
  op$prev_outputs <- NULL
  # Keep one copy of the paper as an attribute of the returned list (the per-
  # module $paper slots are stripped to keep the object flat), so downstream
  # consumers — e.g. convert_psychds() / convert_codebook() reusing a captured
  # result — can recover the paper without re-reading it.
  paper_obj <- op$paper
  op$paper <- NULL
  module_output[[op$module]] <- op

  # organise modules ----
  section_levels <- c("general", "intro", "method", "results", "discussion", "reference")
  sections <- sapply(module_output, \(mo) mo$section)
  sections <- factor(sections, section_levels)
  # tl_levels <- c("red", "yellow", "green", "info", "na", "fail")
  # tls <- sapply(module_output, \(mo) mo$traffic_light %||% "info")
  # tls <- factor(tls, tl_levels)
  # # this seems hacky, but I can't figure out how to sort by 2 vectors
  # mod_order <- xtfrm(sections) * 10 + xtfrm(tls)
  mod_order <- xtfrm(sections)
  module_output <- sort_by(module_output, mod_order)

  attr(module_output, "paper") <- paper_obj
  return(module_output)
}

#' Create Report from Module Output
#'
#' @param module_output a list of module output (usually from `report_module_run()`)
#' @param paper a paper object
#'
#' @returns report text
#' @export
report_qmd <- function(module_output, paper = list()) {
  ## read in report template ----
  # report_type() (set for the call's duration by report(), or directly by
  # the caller -- see its own docs) picks which template builds the header:
  # _report_simple.Rmd is a plain R MARKDOWN template (not Quarto), rendered
  # by report() via rmarkdown::render() rather than quarto::quarto_render().
  # Quarto's HTML format always embeds its own baseline JavaScript bundle
  # (tabsets.js, Bootstrap/Popper) REGARDLESS of theme/feature settings --
  # there is no Quarto option to suppress it -- so getting an email-safe
  # report with (almost) no embedded <script> content requires going around
  # Quarto's HTML format entirely and using the same Pandoc binary Quarto
  # bundles, driven through rmarkdown's much plainer default template
  # instead. See .report_pandoc_dir()'s own docs for how that Pandoc binary
  # is located.
  template_file <- if (.report_is_static(report_type()))
    "templates/_report_simple.Rmd" else "templates/_report.qmd"
  report_template <- system.file(template_file,
    package = "metacheck"
  )
  rt <- readLines(report_template)
  cut_after <- which(rt == "<!-- Demo -->") - 1
  rt_head <- paste(rt[1:cut_after], collapse = "\n")

  # The simple template's title block is hand-rolled (logo left of
  # title/subtitle) instead of Pandoc's auto-generated one, so the report
  # stays a single self-contained file an email client can open with no
  # external image request -- embedded as a base64 `data:` URI, same
  # convention as the jasp.R/omv.R/spv.R plot embeds elsewhere in this
  # package. Substituted via a literal token + fixed-string gsub(), NOT
  # through the sprintf() call below: a base64-encoded PNG is tens of
  # thousands of characters, and sprintf()'s `fmt` argument has an 8192-
  # character limit -- folding the logo in as a %s would make rt_head
  # itself (the fmt string) exceed that the moment the logo is this large,
  # throwing "'fmt' length exceeds maximal format length 8192" (confirmed
  # live once this template's CSS grew past the remaining headroom).
  # Applied to every template type (a no-op replace when the token is
  # absent, e.g. the Quarto template) so this stays template-agnostic.
  logo_data_uri <- if (.report_is_static(report_type())) {
    logo_path <- system.file("app/www/images/logo.png", package = "metacheck")
    if (nzchar(logo_path))
      paste0("data:image/png;base64,", base64enc::base64encode(logo_path))
    else ""
  } else ""
  rt_head <- gsub("{{LOGO_DATA_URI}}", logo_data_uri, rt_head, fixed = TRUE)

  subtitle <- gsub('"', '\\\\"', paper$info$title %||% "")
  doi_text <- ifelse(is.na(paper$info$doi) |
                       (paper$info$doi %||% "") == "", "",
    sprintf("DOI: [%s](https://doi.org/%s)", paper$info$doi, paper$info$doi)
  )

  # get authors
  author_text <- ""
  if (nrow(paper$author) > 0) {
    names <- paste(paper$author$given,
                   paper$author$family)
    last <- utils::tail(names, 1)
    first <- setdiff(names, last)
    if (length(first)) first <- paste(first, collapse = ", ")
    author_text <- c(first, last) |> paste(collapse = " & ")
  }

  # The static template has one extra placeholder after the YAML subtitle
  # (subtitle again, for the hand-rolled title block below the YAML -- see
  # its own comment; the logo itself is substituted separately above, not
  # through sprintf()); the Quarto template has no such placeholder, so
  # that extra arg is only appended for the static case.
  header_args <- c(
    list(subtitle),
    if (.report_is_static(report_type())) list(subtitle),
    list(
      # author_text, # was confusing about author of paper vs report
      as.character(utils::packageVersion("metacheck")),
      Sys.Date(),
      doi_text
    )
  )
  # Fills rt_head's %s placeholders in order, one at a time, instead of
  # sprintf(rt_head, ...): sprintf()'s `fmt` argument has an 8192-character
  # limit, and rt_head (the whole template header, CSS included) now
  # exceeds that on its own -- confirmed live ("'fmt' length exceeds
  # maximal format length 8192") once this template's CSS grew past the
  # old template's length. A literal, real "%" in the template (CSS
  # percentages, e.g. `width: 100%`) needs no escaping here, unlike
  # sprintf()'s `%%` requirement, since each replacement only ever touches
  # the first remaining literal "%s" via fixed = TRUE matching.
  qmd_header <- rt_head
  for (arg in header_args) {
    qmd_header <- sub("%s", as.character(arg), qmd_header, fixed = TRUE)
  }

  # Brief mode (see .report_is_brief()'s own comment) drops the template's
  # explanatory boilerplate -- what Metacheck is, the values statement, the
  # validation blurb, the continuous-development note -- keeping only the
  # YAML/<style> block (both templates need it to render at all), folding
  # the version/date/DOI info and the emoji legend into one combined box.
  # A brief report's only top-level heading is "## Summary", so the
  # auto-generated table of contents both templates' YAML normally turns
  # on (rmarkdown's `toc: true`, Quarto's `toc: true` + its sidebar
  # `toc-title` legend) would otherwise render as a single, pointless
  # "Summary" link -- turned off here instead of carrying that forward.
  if (.report_is_brief(report_type())) {
    info_lines <- regmatches(qmd_header,
      regexpr("(?s)::: ?\\{#info\\}\\n(.*?)\\n:::", qmd_header, perl = TRUE)) |>
      sub("(?s)::: ?\\{#info\\}\\n(.*?)\\n:::", "\\1", x = _, perl = TRUE) |>
      # The original #info div's own trailing double-spaces (a markdown
      # hard line break) don't survive capture/reinsertion reliably, so its
      # three logical lines (link+version, date, DOI) are forced back onto
      # separate lines explicitly rather than trusting whatever whitespace
      # the regex happened to preserve.
      trimws() |>
      strsplit("\n") |>
      _[[1]] |>
      trimws() |>
      paste(collapse = "  \n")

    legend_items <- paste(
      "Legend: ⚠️ possible problems detected;",
      "\U0001F50D something to check;",
      "✅️ no problems detected;",
      "ℹ️ informational only;",
      "⬜ not applicable;",
      "☠️ check failed."
    )
    info_box <- sprintf(
      "::: {.legend}\n%s\n\n---\n\n%s\n:::",
      info_lines, legend_items
    )

    # Cut everything after the banner (static template) or the <style>
    # block (Quarto template, which has no banner markup of its own) --
    # NOT always right after </style>: that used to also delete the static
    # template's own banner div (logo + title/subtitle), since the banner
    # comes after </style> in that file, leaving brief-mode simple reports
    # with no visible title at all (confirmed live: report_simple_brief.html
    # rendered with only the CSS-hidden plain Pandoc title block and no
    # banner). _report_simple.Rmd marks the banner's own end with a literal
    # "<!-- End Banner -->" comment for exactly this cut, rather than
    # counting the banner's nested closing </div> tags here.
    cut_pattern <- if (.report_is_static(report_type()))
      "(?s)(</style>\\n.*?<!-- End Banner -->\\n).*$"
    else
      "(?s)(</style>\\n).*$"
    qmd_header <- sub(cut_pattern, "\\1", qmd_header, perl = TRUE)
    # The full-length static template reopens `.mc-page` (the max-width/
    # padding container) right after the banner, closed once at the very
    # end of report_qmd() -- the cut above removes that reopen along with
    # everything else after the banner, so it needs to come back here, or
    # report_qmd()'s closing_div has no matching open tag in brief mode.
    if (.report_is_static(report_type()))
      qmd_header <- paste0(qmd_header, "\n<div class=\"mc-page\">\n")
    qmd_header <- paste(qmd_header, info_box, sep = "\n\n")

    # Both templates' TOC is a YAML setting read before report_qmd() ever
    # runs, so it has to be rewritten in the text itself rather than passed
    # as a chunk option. "toc: true" (shared verbatim by both YAML keys,
    # rmarkdown's and Quarto's) is the only occurrence in either template.
    qmd_header <- sub("toc: true", "toc: false", qmd_header, fixed = TRUE)
  }

  ## generate summary section ----
  # Brief mode (see report_type()'s own docs) narrows both the summary and
  # the per-module sections below to only what needs attention -- red,
  # yellow, and fail (a module that errored is itself something to fix) --
  # dropping green/info/na entirely, since the point of brief is to get to
  # "what to improve" as fast as possible.
  brief <- .report_is_brief(report_type())
  summary_output <- if (brief) {
    flagged <- sapply(module_output, `[[`, "traffic_light") %in%
      c("red", "yellow", "fail")
    module_output[flagged]
  } else {
    module_output
  }

  emojis <- metacheck::emojis
  # Each row's icon/title/one-line summary is raw HTML, not a markdown "- "
  # bullet: a plain bullet list gives every row IDENTICAL markup regardless
  # of traffic light (Pandoc's bracketed-span `{.class}` syntax used
  # previously for the link only lands the class on the <a>, never the <li>
  # itself -- confirmed in the rendered output), so there was no way to
  # style a row by its own status (a left accent stripe, tinted background
  # for red/yellow) -- the ledger-row look the user asked for needs the
  # class on the row. Raw HTML passes through untouched in both Quarto and
  # plain Pandoc. The icon/title/summary_text going into it are confirmed
  # markdown-free (checked against a live run), so this is safe -- but the
  # per-module TO-DO bullets below are NOT: they mix real markdown
  # (**bold**) with already-raw HTML (<a>, <details>, <pre>) depending on
  # the source module, and Pandoc does not recursively markdown-process
  # text inside a raw HTML block, so folding them into this same HTML <li>
  # would print **bold** as literal asterisks instead of rendering it
  # (confirmed against a live corpus: power.R's and ref_consistency.R's own
  # todo bullets use exactly this **label:** convention). The todo list
  # therefore stays a SEPARATE, ordinary markdown bullet list immediately
  # following the HTML row (so Pandoc parses it normally, as before),
  # visually indented under the row via CSS margin rather than real DOM
  # nesting inside the <li>.
  summary_list <- sapply(summary_output, \(x) {
    tl <- x$traffic_light %||% "info"
    tl_symbol <- emojis[[paste0("tl_", tl)]]
    summary_text <- x$summary_text %||% ""
    # Some modules' summary_text is ITSELF a multi-item markdown list (one
    # "- " bullet per finding, e.g. code_check's "- We found 33 R...\n- 1
    # code file had no comments.\n..."), not a single sentence -- confirmed
    # against a live run: code_check, codebook_check, data_check, repo_check,
    # reproducibility_check, causal_claims all do this. Dumped into the raw
    # HTML row's one-line <span> as before, every "- " marker and newline
    # just ran together into one unreadable paragraph (confirmed live: "- We
    # found 33 R... - 1 code file had no comments. - 11 files..." as a
    # single run-on line). Detected by its own leading "\n" (the existing
    # convention already used below) and, when present, left OUT of the
    # row's one-line span -- the row then shows just the module title, and
    # the real list renders as its own proper markdown list right after the
    # row, same placement/reasoning as the to-do list below.
    is_list_summary <- nzchar(summary_text) && substr(summary_text, 1, 1) == "\n"
    row_summary_text <- if (is_list_summary) "" else summary_text
    summary_list_md <- if (is_list_summary) {
      sprintf("\n\n::: {.mc-summary-todo}\n%s\n:::\n\n", trimws(summary_text))
    } else {
      ""
    }
    if (brief) {
      # No per-module section exists in brief mode (see below), so the title
      # is plain text, not a link to an anchor that was never emitted.
      # Followed by one to-do bullet per specific thing to fix (the exact
      # sentence/value/file flagged), pulled from the module's own detailed
      # report via .report_flagged_bullets() -- see its own comment for why
      # this is the only available source -- as a separate markdown list
      # (see this block's own comment on why it cannot live inside the row's
      # raw HTML), wrapped in a div so it can be indented to sit visually
      # under the row above it.
      row_html <- sprintf(
        "<li class=\"mc-summary-row %s\"><span class=\"mc-summary-icon\">%s</span><span class=\"mc-summary-body\"><span class=\"mc-summary-title\">%s</span><span class=\"mc-summary-text\">%s</span></span></li>",
        tl, tl_symbol, x$title, row_summary_text
      )
      todo <- .report_flagged_bullets(x)
      todo_md <- if (length(todo)) {
        sprintf("\n\n::: {.mc-summary-todo}\n%s\n:::\n\n",
               paste(todo, collapse = "\n"))
      } else {
        ""
      }
      return(paste0(row_html, summary_list_md, todo_md))
    }
    row_html <- sprintf(
      "<li class=\"mc-summary-row %s\"><span class=\"mc-summary-icon\">%s</span><span class=\"mc-summary-body\"><span class=\"mc-summary-title\"><a href=\"#%s\">%s</a></span><span class=\"mc-summary-text\">%s</span></span></li>",
      tl, tl_symbol, .report_title_slug(x$title), x$title, row_summary_text
    )
    paste0(row_html, summary_list_md)
  })
  if (brief && length(summary_list) == 0) {
    summary_list <- "<li class=\"mc-summary-row\">Nothing flagged -- no modules reported a red, yellow, or fail status.</li>"
  }
  summary_text <- sprintf(
    "## Summary\n\n<ul class=\"mc-summary\">\n\n%s\n\n</ul>\n\n",
    paste(summary_list, collapse = "\n\n")
  )

  ## format module reports ----
  # In brief mode, the per-module sections below would be redundant with the
  # (already filtered) summary above -- summary_list already gives every
  # flagged module's one-line text plus its to-do bullets, which is all
  # brief mode shows for a module. Skip straight to the setup chunk instead
  # of re-emitting the same modules a second time -- except for a single
  # combined validation table (.report_validation_table()'s own comment),
  # replacing the per-module validation callout brief mode otherwise has no
  # equivalent of.
  module_reports <- if (brief) {
    validation <- if (length(summary_output) > 0) .report_validation_table(summary_output) else ""
    feedback <- paste(
      "If there are things we can improve, please email us at",
      "[metacheck@scienceverse.org](mailto:metacheck@scienceverse.org)",
      "or leave an issue at",
      "[github.com/scienceverse/metacheck/issues](https://github.com/scienceverse/metacheck/issues)."
    )
    paste(validation, feedback, sep = "\n\n")
  } else {
    section_levels <- c("general", "intro", "method", "results", "discussion", "reference")

    sapply(section_levels, \(sec) {
      this_section <- sapply(module_output, `[[`, "section") == sec
      # remove fail and na from main report section
      valid_tl <- !sapply(module_output, `[[`, "traffic_light") %in% c("na", "fail")
      if (!any(this_section & valid_tl)) {
        return(NULL)
      }

      section_op <- module_output[this_section & valid_tl]
      mr <- sapply(section_op, module_report)

      title <- sprintf(
        "## %s%s Modules",
        toupper(substr(sec, 1, 1)),
        substr(sec, 2, nchar(sec))
      )
      c(title, mr)
    }) |>
      unlist() |>
      paste(collapse = "\n\n") |>
      gsub("\\n{3,}", "\n\n", x = _)
  }

  # Both quarto::quarto_render() and rmarkdown::render() always execute this
  # document's R chunks in a separate subprocess -- options() set in the
  # calling R session, including report_type()'s, never reach it. The text
  # above (scroll_table()'s .panel-tabset vs. plain headings,
  # codebook_file_tabset()) was already built correctly in THIS process,
  # which does see report_type() -- but the embedded
  # `metacheck::report_table(...)` calls inside those R chunks only run
  # later, in the subprocess, so it needs its own explicit setup chunk to
  # pick the same mode. Also makes a saved/re-rendered document
  # self-consistent: whatever generated its text is what re-rendering it
  # reproduces. Classic `{r, include=FALSE}` chunk-option syntax, not
  # Quarto's `#| include: false` YAML form, since this chunk is shared by
  # both the Quarto .qmd and the plain R Markdown .Rmd template -- Quarto
  # accepts the classic form too, but plain knitr/rmarkdown does not
  # understand Quarto's YAML chunk-option syntax.
  setup_chunk <- sprintf(
    "```{r, include=FALSE}\nmetacheck::report_type(\"%s\")\n```",
    report_type()
  )

  # The static template opens a `<div class="mc-page">` right after its
  # banner (see _report_simple.Rmd's own comment) to keep the
  # max-width/padding container active for the rest of the report body,
  # which is appended here, outside that template file -- so the matching
  # close belongs here too, and only for that template (the Quarto template
  # never opens it).
  closing_div <- if (.report_is_static(report_type())) "\n\n</div>\n" else ""

  report_text <- paste(qmd_header,
    setup_chunk,
    summary_text,
    module_reports,
    closing_div,
    "\n", # prevent incomplete final line warnings
    sep = "\n\n"
  )

  return(report_text)
}

# Modules with no chapter yet in the online manual
# (https://www.scienceverse.org/metacheck_book/) -- utility/listing modules
# (all_urls, all_p_values), overinclusive variants (coi_check_oi,
# funding_check_oi), and modules still too early-stage to document
# (causal_claims, ref_miscitation -- see its own roxygen @description).
# Every other built-in module's chapter lives at a predictable URL (see
# .module_chapter_link()'s own comment), so this is a manually-maintained
# exception list, not the rule -- update it when the book's table of
# contents gains or loses a module chapter.
.module_no_chapter <- c(
  "all_urls", "all_p_values", "coi_check_oi", "funding_check_oi",
  "causal_claims", "ref_miscitation"
)

# One sentence linking to a module's chapter in the online manual, or NULL
# for a module not yet documented there (see .module_no_chapter). Checked
# against the book's actual table of contents (as of 2026-10), every
# chapter for a built-in checking module sits at
# chapters/mod-<module name, underscores as hyphens>.html -- e.g. "marginal"
# -> mod-marginal.html, "stat_p_exact" -> mod-stat-p-exact.html -- so the
# link is built from that pattern rather than hand-listing 24+ URLs that
# would drift out of sync with the module list.
.module_chapter_link <- function(module) {
  if (module %in% .module_no_chapter) return(NULL)

  slug <- gsub("_", "-", module)
  url <- sprintf(
    "https://www.scienceverse.org/metacheck_book/chapters/mod-%s.html",
    slug
  )
  sprintf("Read more in the [metacheck manual](%s).", url)
}

# Brief mode's single combined validation table -- full/simple mode is
# untouched and keeps its existing per-module validation callout (see
# module_report()'s own "validation" variable); brief mode skips the whole
# per-module detail section (including that callout) entirely, so it had
# no validation content of its own before this. One table for every
# flagged module (same set as the Summary section itself), rather than
# repeating each module's own callout, since the point here is a single,
# scannable transparency statement about the whole report, not a repeat of
# detail brief mode otherwise deliberately omits. A module with no
# `<validation>` tag at all (most of the 19 utility/newer/overinclusive
# modules) is listed too, marked as such, rather than silently left out --
# the absence of validation evidence is as relevant to trust as its
# presence.
.report_validation_table <- function(module_output) {
  rows <- lapply(module_output, \(x) {
    validation_text <- tryCatch({
      info <- module_info(x$module)
      m <- gregexpr("<validation>.*?</validation>", info$details)
      if (m[[1]][1] > -1) {
        regmatches(info$details, m)[[1]] |>
          sub("<validation>\\s*", "", x = _) |>
          sub("\\s*</validation>", "", x = _)
      } else {
        "No validation information yet."
      }
    }, error = \(e) "No validation information yet.")
    data.frame(Module = x$title, Validation = validation_text)
  })
  tbl <- dplyr::bind_rows(rows)

  sprintf(
    "## Validation\n\nIn line with Metacheck's values, we transparently communicate how each module was validated against manually coded ground truth data.\n\n<details><summary>Validation details</summary>\n\n%s\n\n</details>",
    knitr::kable(tbl, format = "html", escape = TRUE, row.names = FALSE,
                table.attr = 'class="dt-static"') |> as.character()
  )
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
#' paper <- demopaper()
#' op <- module_run(paper, "stat_p_exact")
#' module_report(op) |> cat()
# A module title becomes both a Pandoc header-attribute id (module_report()'s
# own `{#id .class}`, below) and a matching link target (the Summary
# section's `[title](#id)`, in report_qmd() above) -- both MUST derive the
# id the same way, or the Summary link points at an id the heading never
# actually gets. Pandoc's `{#id}` attribute syntax itself cannot contain
# "(", ")", or other punctuation -- a title like "Funding Check
# (Overinclusive)" previously produced `{#funding-check-(overinclusive)
# .red}`, which Pandoc could not parse as an attribute at all, so the whole
# `{...}` block fell through and rendered as literal visible text instead of
# becoming an id (confirmed live: the TOC entry and heading both read
# "Funding Check (Overinclusive) {#funding-check-(overinclusive) .red}").
# Mirrors Pandoc's own auto-generated heading-id rule: lowercase, whitespace
# to hyphens, strip everything outside [a-z0-9-_].
.report_title_slug <- function(title) {
  title |>
    tolower() |>
    gsub("\\s+", "-", x = _) |>
    gsub("[^a-z0-9_-]", "", x = _)
}

module_report <- function(module_output,
                          header = 3) {
  n <- NULL
  emojis <- metacheck::emojis

  # set up header
  tl <- module_output$traffic_light %||% "info"
  tl_symbol <- emojis[[paste0("tl_", tl)]]
  if (is.null(header)) {
    head <- ""
  } else if (header == 0) {
    head <- sprintf("%s %s", tl_symbol, module_output$title)
  } else if (header %in% 1:6) {
    head <- sprintf(
      "%s %s %s {#%s .%s}",
      rep("#", header) |> paste(collapse = ""),
      tl_symbol,
      module_output$title,
      .report_title_slug(module_output$title),
      tl
    )
  } else {
    head <- header
  }

  # set up report
  summary <- module_output$summary_text %||% "..."
  report <- module_output$report %||% module_output$summary_text
  if (all(report == "")) report <- NULL


  # how it works
  hiw <- tryCatch(
    {
      validation <- NULL
      info <- module_info(module_output$module)

      # set up validation section if tagged. Emit a native Quarto fenced div, not
      # a raw <p>: raw HTML gets passed through by Pandoc wrapped in
      # \if{html}{\out{...}}, and that wrapper leaked into the rendered report as
      # literal "}}" / "\if{html}{\out{" around the validation text. A fenced div
      # renders to <div class="validation"> cleanly and keeps the CSS hook.
      m <- gregexpr("<validation>.*?</validation>", info$details)
      if (m[[1]][1] > -1) {
        validation <- regmatches(info$details, m) |>
          _[[1]] |>
          sub("<validation>\\s*", "::: {.validation}\nValidation: ", x = _) |>
          sub("\\s*</validation>", "\n:::", x = _)
      }

      # get authors
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

      # remove validation section
      details <- gsub("\\s*<validation>.*</validation>\\s*", "", info$details)

      chapter_link <- .module_chapter_link(module_output$module)

      c(info$description, details, author_ack, chapter_link) |>
        collapse_section("How It Works", callout = "note")
    },
    error = \(e) {
      return(NULL)
    }
  )

  # create collapsible boxes around substantial reports (> 300 char)
  # No inner <div> wrapper: it serves no CSS/JS purpose (confirmed against
  # both templates -- the demo section's own <details> examples never used
  # one either) and plain Pandoc's HTML-block parser cannot reliably track a
  # raw <div> left open across many lines of interleaved content (a fenced
  # div closing in between makes it "close implicitly" with a stderr
  # warning, confirmed live rendering a report_type("simple") report whose
  # body contains a `collapse_section()` callout) -- <details> alone is
  # already a real block container, so it needs no second one nested inside.
  pre <- "<details><summary>View detailed feedback</summary>"
  post <- "</details>"
  if (is.null(report) ||
    all(module_output$summary_text == report)) {
    pre <- post <- report <- NULL
  } else if (paste(report, collapse = "\n\n") |> nchar() < 300) {
    pre <- post <- NULL
  }

  paste0(c(head, summary, pre, report, post, hiw, validation), collapse = "\n\n")
}
