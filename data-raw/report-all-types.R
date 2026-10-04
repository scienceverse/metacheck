# Runs every built-in module once against the demo paper, then renders all
# four report_type() combinations (full, brief, simple, simple_brief) from
# that single run -- so comparing the four doesn't mean re-running every
# module four times over. Output goes to the gitignored tmp_output/ folder.
#
# The module run itself (network calls, ~30 modules) is the slow part, so its
# result is cached to tmp_output/module_output.rds. Re-running this script
# reuses that cache and only re-renders -- set rerun_modules <- TRUE (or
# delete the .rds) to force a fresh module run, e.g. after changing a
# module's code.
#
# Usage: source("data-raw/report-all-types.R")

devtools::load_all(".")

out_dir <- "tmp_output"
dir.create(out_dir, showWarnings = FALSE)
cache_file <- file.path(out_dir, "module_output.rds")

if (!exists("rerun_modules")) rerun_modules <- FALSE

paper <- demopaper()

if (!rerun_modules && file.exists(cache_file)) {
  module_output <- readRDS(cache_file)
} else {
  # psychds_check excluded: still under development
  modules <- setdiff(module_list()$name, "psychds_check")
  # cache = TRUE on every module that accepts it: an on-disk cache
  # (.metacheck_repo_cache/, see metacheck_cache_info()) of repository
  # listings/downloaded files, separate from this script's own
  # module_output.rds result cache above -- without it, repo_check/
  # code_check/data_check/reproducibility_check re-fetch every file from
  # OSF/Zenodo/etc. over the network on every rerun_modules <- TRUE run,
  # even when nothing about the repository changed.
  cache_args <- list(cache = TRUE)
  args <- list(
    repo_check = cache_args,
    code_check = cache_args,
    data_check = cache_args,
    # reproducibility_check: actually execute the paper's code (not just
    # the static analysis execute = FALSE gives by default), in a plain
    # subprocess sandbox rather than Docker, since Docker is not assumed
    # to be available in whatever environment this script runs in.
    reproducibility_check = c(cache_args, list(execute = TRUE, sandbox = "process"))
  )
  module_output <- report_module_run(paper, modules, args = args)
  saveRDS(module_output, cache_file)
}

render_report <- function(report_type, out_dir, paper, module_output) {
  prev <- report_type()
  report_type(report_type)
  on.exit(report_type(prev), add = TRUE)

  report_text <- report_qmd(module_output, paper)

  if (.report_is_static(report_type())) {
    # Same plain-Pandoc path report() itself uses for "simple"/"simple_brief"
    # -- see report()'s own comment for why Quarto's HTML format is skipped
    # entirely for these two.
    input_rmd <- file.path(out_dir, paste0("report_", report_type(), ".Rmd"))
    output_html <- sub("\\.Rmd$", ".html", input_rmd)
    writeLines(report_text, input_rmd)

    pandoc_dir <- .report_pandoc_dir()
    prev_pandoc_env <- Sys.getenv("RSTUDIO_PANDOC", unset = NA)
    if (!is.null(pandoc_dir)) Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
    on.exit({
      if (is.na(prev_pandoc_env)) Sys.unsetenv("RSTUDIO_PANDOC")
      else Sys.setenv(RSTUDIO_PANDOC = prev_pandoc_env)
    }, add = TRUE)

    rmarkdown::render(
      input = input_rmd,
      output_file = basename(output_html),
      output_dir = dirname(output_html),
      quiet = TRUE,
      envir = new.env(parent = globalenv())
    )
    return(output_html)
  }

  input_qmd <- file.path(out_dir, paste0("report_", report_type(), ".qmd"))
  output_html <- sub("\\.qmd$", ".html", input_qmd)
  writeLines(report_text, input_qmd)
  quarto::quarto_render(input_qmd, output_format = "html")
  output_html
}

report_types <- c("full", "brief", "simple", "simple_brief")
output_files <- sapply(report_types, render_report,
                       out_dir = out_dir, paper = paper,
                       module_output = module_output)

print(output_files)

# browseURL(output_files[["full"]])
# browseURL(output_files[["brief"]])
# browseURL(output_files[["simple"]])
# browseURL(output_files[["simple_brief"]])
