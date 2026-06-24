# Local RegCheck server helpers
#
# The RegCheck Python app is bundled in inst/regcheck/. These functions start
# it either via Docker (recommended, no Python install needed) or via a manual
# Python virtual environment.

.regcheck_app_dir <- function() {
  system.file("regcheck", package = "metacheck")
}

.regcheck_venv_dir <- function() {
  file.path(.regcheck_app_dir(), ".venv")
}

.regcheck_default_token <- "metacheck-local"

#' Set up the local RegCheck server (manual Python path)
#'
#' Creates a Python virtual environment inside the bundled RegCheck app
#' directory, installs all dependencies, and downloads the NLTK data. Only
#' needed if you are using the manual Python path — if you have Docker,
#' use [regcheck_start_local()] directly with `method = "docker"`.
#'
#' **System requirements:**
#' \itemize{
#'   \item Python 3.12+ — \url{https://www.python.org/downloads/}
#'   \item Ollama — \url{https://ollama.com/download}, then run
#'         \code{ollama pull llama3.2} and
#'         \code{ollama pull nomic-embed-text-v2-moe} in a terminal.
#' }
#'
#' @param python path to the Python 3.12+ executable; if `NULL` (default),
#'   tries `python3` then `python` on the system PATH.
#'
#' @return invisibly, the path to the virtual environment
#' @export
regcheck_setup_local <- function(python = NULL) {
  app_dir <- .regcheck_app_dir()
  if (!nzchar(app_dir)) {
    stop("Could not find the bundled RegCheck app. ",
         "Is the metacheck package installed?", call. = FALSE)
  }

  venv_dir <- .regcheck_venv_dir()

  python <- python %||% Sys.which("python3")
  if (!nzchar(python)) python <- Sys.which("python")
  if (!nzchar(python)) {
    stop("Python 3.12+ not found. Install it from https://www.python.org/downloads/ ",
         "and ensure it is on your PATH.", call. = FALSE)
  }
  ver <- tryCatch(
    system2(python, c("-c", "import sys; print(sys.version_info[:2])"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) ""
  )
  message("Using Python: ", python, " (", ver, ")")

  if (!dir.exists(venv_dir)) {
    message("Creating Python virtual environment...")
    ret <- system2(python, c("-m", "venv", venv_dir))
    if (ret != 0) stop("Failed to create virtual environment.", call. = FALSE)
  } else {
    message("Virtual environment already exists, skipping creation.")
  }

  pip <- if (.Platform$OS.type == "windows") {
    file.path(venv_dir, "Scripts", "pip.exe")
  } else {
    file.path(venv_dir, "bin", "pip")
  }

  req_file <- file.path(app_dir, "requirements.txt")
  message("Installing Python dependencies (this may take a few minutes)...")
  ret <- system2(pip, c("install", "-r", req_file, "--quiet"))
  if (ret != 0) stop("pip install failed.", call. = FALSE)

  py_bin <- if (.Platform$OS.type == "windows") {
    file.path(venv_dir, "Scripts", "python.exe")
  } else {
    file.path(venv_dir, "bin", "python")
  }
  message("Downloading NLTK sentence tokeniser data...")
  system2(py_bin,
          c("-c", "import nltk; nltk.download('punkt'); nltk.download('punkt_tab')"),
          stdout = FALSE, stderr = FALSE)

  message("\nSetup complete. Start the server with regcheck_start_local().")
  invisible(venv_dir)
}

#' Start the local RegCheck server
#'
#' Starts the bundled RegCheck server as a background process, either via
#' Docker (recommended) or a manual Python virtual environment. The server
#' runs at `http://localhost:8000` and is used automatically by
#' `module_run(paper, "reg_check")` (the default `client = "ollama"`).
#'
#' **Docker path** (`method = "docker"`, recommended):
#' Requires [Docker](https://www.docker.com/get-started/) to be installed.
#' The first run builds the image (~5 minutes); subsequent starts are instant.
#' No Python installation needed.
#'
#' **Python path** (`method = "python"`):
#' Requires Python 3.12+ and Ollama. Run [regcheck_setup_local()] once first.
#'
#' In both cases, Ollama must be running with the required models pulled.
#' The embedding model is fixed; the language model can be any Ollama model
#' you have pulled — use the largest one that fits your hardware:
#' ```
#' ollama pull nomic-embed-text-v2-moe
#' ollama pull llama3.2          # or llama3.1, mistral, etc.
#' ```
#'
#' The API token is set automatically to `"metacheck-local"` — no manual
#' token configuration needed.
#'
#' @param method `"docker"` (default) or `"python"`
#' @param model Ollama language model to use. Defaults to `NULL`, which
#'   auto-selects the largest language model currently pulled in Ollama. Any
#'   model you have pulled with `ollama pull` can be used here — larger
#'   models generally produce better judgements.
#' @param port port to run the server on (default 8000)
#'
#' @return invisibly, the process object
#' @export
regcheck_start_local <- function(method = c("docker", "python"),
                                 model  = NULL,
                                 port   = 8000) {
  method <- match.arg(method)

  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("The 'processx' package is required. ",
         "Install it with: install.packages('processx')", call. = FALSE)
  }

  app_dir <- .regcheck_app_dir()

  # the local server always uses the built-in default token; set it in this R
  # session so regcheck_compare() picks it up automatically
  Sys.setenv(REGCHECK_API_TOKEN = .regcheck_default_token)

  # auto-select model if not specified ----
  if (is.null(model)) {
    ollama_base_url <- Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")
    models <- tryCatch(ellmer::models_ollama(ollama_base_url),
                       error = function(e) NULL)
    if (is.null(models) || nrow(models) == 0) {
      stop("Ollama is not running or has no models pulled.\n",
           "Start Ollama and pull a language model, e.g.:\n",
           "  ollama pull llama3.2", call. = FALSE)
    }
    lm_models <- models[grepl("completion", models$capabilities), ]
    if (nrow(lm_models) == 0) {
      stop("No language models found in Ollama (only embedding models are installed).\n",
           "Pull a language model, e.g.:\n",
           "  ollama pull llama3.2", call. = FALSE)
    }
    model <- lm_models$id[which.max(lm_models$size)]
    message("Auto-selected Ollama model: ", model)
  }

  if (method == "docker") {
    docker <- Sys.which("docker")
    if (!nzchar(docker)) {
      stop("Docker not found. Install it from https://www.docker.com/get-started/",
           call. = FALSE)
    }
    message("Starting local RegCheck server via Docker at http://localhost:", port,
            " (model: ", model, ") ...")
    message("(First run will build the image — this takes a few minutes.)")
    proc <- processx::process$new(
      command = docker,
      args    = c("compose", "up", "--build", "--force-recreate"),
      wd      = app_dir,
      env     = c(
        Sys.getenv(),
        OLLAMA_MODEL        = model,
        REGCHECK_API_TOKEN  = Sys.getenv("REGCHECK_API_TOKEN")
      ),
      stdout  = "|",
      stderr  = "|",
      cleanup = TRUE
    )
  } else {
    venv_dir <- .regcheck_venv_dir()
    if (!dir.exists(venv_dir)) {
      stop("Virtual environment not found. Run regcheck_setup_local() first.",
           call. = FALSE)
    }
    uvicorn <- if (.Platform$OS.type == "windows") {
      file.path(venv_dir, "Scripts", "uvicorn.exe")
    } else {
      file.path(venv_dir, "bin", "uvicorn")
    }
    message("Starting local RegCheck server (Python) at http://localhost:", port,
            " (model: ", model, ") ...")
    proc <- processx::process$new(
      command = uvicorn,
      args    = c("backend.main:create_app", "--factory",
                  "--host", "127.0.0.1",
                  "--port", as.character(port)),
      wd      = app_dir,
      env     = c(
        Sys.getenv(),
        OLLAMA_MODEL       = model,
        REGCHECK_API_TOKEN = Sys.getenv("REGCHECK_API_TOKEN")
      ),
      stdout  = "|",
      stderr  = "|",
      cleanup = TRUE
    )
  }

  message("Waiting for RegCheck server to become ready", appendLF = FALSE)
  server_url <- paste0("http://localhost:", port)
  deadline <- Sys.time() + 600
  ready <- FALSE
  while (Sys.time() < deadline) {
    if (!proc$is_alive()) {
      err <- proc$read_all_error()
      out <- proc$read_all_output()
      stop("RegCheck server failed to start.\n", err, out, call. = FALSE)
    }
    up <- tryCatch({
      httr2::request(server_url) |>
        httr2::req_timeout(2) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform()
      TRUE
    }, error = function(e) FALSE)
    if (up) { ready <- TRUE; break }
    message(".", appendLF = FALSE)
    Sys.sleep(3)
  }
  message("")
  if (!ready) stop("RegCheck server did not become ready within 10 minutes.",
                   call. = FALSE)

  message("RegCheck server running (PID ", proc$get_pid(), "). ",
          "Stop it with regcheck_stop_local().")

  assign(".regcheck_proc", proc, envir = .regcheckEnv)
  invisible(proc)
}

#' Stop the local RegCheck server
#'
#' Kills the background process started by [regcheck_start_local()].
#'
#' @return invisibly NULL
#' @export
regcheck_stop_local <- function() {
  proc <- tryCatch(get(".regcheck_proc", envir = .regcheckEnv),
                   error = function(e) NULL)
  if (is.null(proc) || !proc$is_alive()) {
    message("No local RegCheck server appears to be running.")
    return(invisible(NULL))
  }
  proc$kill()
  message("Local RegCheck server stopped.")
  invisible(NULL)
}

.regcheckEnv <- new.env(parent = emptyenv())
