## render_report.R ---------------------------------------------------------
## Render a metacheck report on a server using metacheck's NORMAL Quarto path.
##
## metacheck::report(..., output_format = "html") calls quarto::quarto_render(),
## which needs the Quarto CLI binary. We install Quarto into the deploying
## user's home (~/opt/quarto) -- see setup.R -- and point the quarto R package
## at it via the QUARTO_PATH env var. This yields output identical to a local
## metacheck run: the flatly/darkly light-dark theme switch, native collapsible
## callouts, the ::: {.validation} block, the title banner and TOC, etc.
##
## (An earlier edition of this file reimplemented the Quarto shell by hand with
## rmarkdown because Quarto wasn't installed. That approximation diverged from
## the real report -- no dark mode, mis-converted callouts, an unrendered
## ::: {.validation} block -- so we now render with Quarto directly.)

# Absolute fallback locations for the Quarto CLI. IMPORTANT: Shiny Server runs
# this app as the `shiny` user (HOME=/home/shiny), NOT as the user who deployed
# it and ran setup.R. So `~/opt/quarto` would resolve to the wrong home. We
# therefore check explicit absolute paths -- the deploy user's install
# (/home/daniel/opt/quarto, readable by `shiny`) is the one setup.R creates.
# Override with the QUARTO_PATH env var if Quarto lives elsewhere.
.QUARTO_ABS_PATHS <- c(
  "/home/daniel/opt/quarto/bin/quarto",
  "/home/shiny/opt/quarto/bin/quarto",
  "/opt/quarto/bin/quarto",
  "/usr/local/bin/quarto"
)

# Candidate locations for the Quarto CLI, in priority order: an explicit
# QUARTO_PATH, then the deploying user's home install, then absolute fallbacks,
# then anything on PATH.
.quarto_candidates <- function() {
  c(
    Sys.getenv("QUARTO_PATH", ""),
    path.expand("~/opt/quarto/bin/quarto"),
    .QUARTO_ABS_PATHS,
    unname(Sys.which("quarto"))
  )
}

#' Ensure the Quarto CLI is discoverable by the quarto R package
#'
#' Sets QUARTO_PATH to the first existing candidate so quarto::quarto_render()
#' can find the binary even when it isn't on the app process's PATH (Shiny
#' Server does not inherit a login shell's PATH). Safe to call on every render.
#'
#' @return invisibly, the resolved quarto path (or "" if none was found).
ensure_quarto <- function() {
  cands <- .quarto_candidates()
  cands <- cands[nzchar(cands)]
  found <- cands[file.exists(cands)]
  if (length(found)) {
    Sys.setenv(QUARTO_PATH = found[[1]])
    return(invisible(found[[1]]))
  }
  # Nothing found: leave QUARTO_PATH as-is and let quarto report a clear error.
  invisible("")
}

#' Render a paper to a self-contained HTML report with Quarto
#'
#' Uses metacheck's own rendering pipeline (modules + Quarto), so the result is
#' identical to a local metacheck report.
#'
#' @param paper a metacheck paper object (from metacheck::read()).
#' @param modules character vector of module names to run.
#' @param output_file path to write the final .html to.
#' @param args list of per-module arguments (passed to metacheck::report()).
#' @return the path to the rendered HTML file.
render_report_no_quarto <- function(paper, modules, output_file, args = list()) {
  # (Name kept for backwards compatibility with app.R; it now DOES use Quarto.)
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("The 'quarto' package is required to render reports on this server.")
  }
  qpath <- ensure_quarto()
  if (!nzchar(qpath)) {
    stop("No Quarto CLI found. Run setup.R to install Quarto into ~/opt/quarto, ",
         "or set QUARTO_PATH to a quarto binary.")
  }

  # metacheck::report() writes the rendered .html next to a temp .qmd (the .qmd
  # contains the full report text, i.e. the user's data) and returns the path it
  # wrote. We snapshot the temp dir first so we can delete every intermediate
  # the call leaves behind -- not just at session end, but right now.
  td <- normalizePath(tempdir(), winslash = "/")
  before <- list.files(td, recursive = TRUE, full.names = TRUE)

  result <- metacheck::report(
    paper,
    modules       = modules,
    output_file   = output_file,
    output_format = "html",
    args          = args
  )

  # metacheck::report() may return the produced path; normalise to output_file.
  produced <- if (is.character(result) && length(result) == 1 &&
                  file.exists(result)) result else output_file

  if (!identical(normalizePath(produced, mustWork = FALSE),
                 normalizePath(output_file, mustWork = FALSE)) &&
      file.exists(produced)) {
    file.copy(produced, output_file, overwrite = TRUE)
    if (!startsWith(normalizePath(produced), normalizePath(tempdir())))
      unlink(produced)
  }

  # Delete every NEW intermediate file/dir the render created in tempdir
  # (e.g. the .qmd, any *_files figure dirs), except our final output_file.
  after <- list.files(td, recursive = TRUE, full.names = TRUE)
  new_paths <- setdiff(after, before)
  keep <- normalizePath(output_file, winslash = "/", mustWork = FALSE)
  to_delete <- new_paths[normalizePath(new_paths, winslash = "/",
                                       mustWork = FALSE) != keep]
  if (length(to_delete)) unlink(to_delete, recursive = TRUE, force = TRUE)

  if (!file.exists(output_file)) {
    stop("Quarto render did not produce an HTML file at: ", output_file)
  }
  output_file
}
