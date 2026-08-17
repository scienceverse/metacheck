## setup.R ------------------------------------------------------------------
## Run ONCE on the server after deploying this app, e.g.:
##   Rscript setup.R
##
## It installs the R packages the app needs and installs a standalone Quarto
## CLI into the deploying user's home (~/opt/quarto), so reports render through
## metacheck's normal Quarto pipeline (identical output to a local run). No
## admin rights, system Quarto, GROBID, Docker, or Ollama is required on the
## server -- only R, the ability to install packages from CRAN and metacheck
## from GitHub, and outbound HTTPS to download Quarto.

repos <- "https://cloud.r-project.org"

cran_pkgs <- c(
  "shiny", "shinyjs", "shinydashboard",
  "quarto", "remotes"
)

missing <- cran_pkgs[!vapply(cran_pkgs, requireNamespace,
                             logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing CRAN packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = repos)
}

# metacheck is on GitHub (scienceverse/metacheck)
if (!requireNamespace("metacheck", quietly = TRUE)) {
  message("Installing metacheck from GitHub...")
  remotes::install_github("scienceverse/metacheck")
}

## --- Install the Quarto CLI into ~/opt/quarto ------------------------------
## metacheck::report(output_format = "html") calls quarto::quarto_render(),
## which needs the Quarto binary. We install a self-contained copy under the
## user's home so no admin rights are needed.
QUARTO_VERSION <- "1.6.40"
quarto_home <- path.expand("~/opt/quarto")
quarto_bin  <- file.path(quarto_home, "bin", "quarto")

install_quarto <- function(version, home) {
  arch <- switch(Sys.info()[["machine"]],
                 "x86_64"  = "amd64",
                 "aarch64" = "arm64",
                 "arm64"   = "arm64",
                 stop("Unsupported architecture: ", Sys.info()[["machine"]]))
  tarball <- sprintf("quarto-%s-linux-%s.tar.gz", version, arch)
  url <- sprintf(
    "https://github.com/quarto-dev/quarto-cli/releases/download/v%s/%s",
    version, tarball)
  dest_parent <- dirname(home)
  dir.create(dest_parent, showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile(fileext = ".tar.gz")
  message("Downloading Quarto ", version, " from ", url)
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  versioned <- file.path(dest_parent, paste0("quarto-", version))
  unlink(versioned, recursive = TRUE)
  dir.create(versioned, recursive = TRUE)
  # extract, stripping the leading quarto-<version>/ directory
  utils::untar(tmp, exdir = versioned, extras = "--strip-components=1")
  unlink(tmp)
  # symlink ~/opt/quarto -> ~/opt/quarto-<version>
  if (file.exists(home) || nzchar(Sys.readlink(home))) unlink(home)
  file.symlink(versioned, home)
  invisible(home)
}

source("render_report.R")  # for ensure_quarto()

if (!file.exists(quarto_bin)) {
  message("Installing Quarto CLI...")
  install_quarto(QUARTO_VERSION, quarto_home)
} else {
  message("Quarto already installed at ", quarto_home)
}

qpath <- ensure_quarto()
if (nzchar(qpath) && requireNamespace("quarto", quietly = TRUE)) {
  ver <- tryCatch(as.character(quarto::quarto_version()),
                  error = function(e) "unknown")
  message("Setup complete. Quarto ", ver, " is available at ", qpath, ".")
} else {
  stop("Quarto CLI not found after install -- check the download/extract step.")
}
