## background_job.R ---------------------------------------------------------
## Run a long job (a whole report) in a separate R process.
##
## The open-source Shiny Server runs ONE R process per app, and R does one
## thing at a time. A report run inside that process freezes every other
## visitor's session in the app until it finishes, and a new visitor cannot
## even load the page (the TU/e proxy in front of the server then answers
## "Bad Gateway"). Running the report in its own R process (via callr) keeps
## the app's process free: it only checks on the job twice a second, moves the
## progress bar, and picks up the result when the job is done.
##
## The job writes its progress to a small file (see shiny_progress.R, which
## does this when the "shiny_progress_file" option is set); bg_watch() reads
## that file and shows it in a Shiny progress bar.
##
## Kept identical in every app folder that uses it (like render_report.R).

#' Start `fun` in a separate R process
#'
#' @param fun a self-contained function: it runs in a fresh R session (with
#'   this app's folder as working directory), so it must load packages and
#'   source helper files itself, and get everything else through `args`.
#' @param args named list of arguments for `fun`.
#' @param keys a visitor's own API keys, named by service (see api_keys.R),
#'   used in this job only.
#' @return a job: list(proc = the callr process, progress_file = path).
bg_start <- function(fun, args = list(), keys = list()) {
  progress_file <- tempfile("job_progress_", fileext = ".txt")
  file.create(progress_file)
  # A function keeps the environment it was created in; sent to the other
  # process it would drag that whole environment along. It needs nothing
  # from it, so detach it.
  environment(fun) <- globalenv()
  runner <- function(fun, args, app_dir, progress_file, keys) {
    setwd(app_dir)
    options(shiny_progress_file = progress_file)
    # The server's keys are environment variables, which this process
    # inherited; a visitor's own keys take their place for this job only.
    # Skipped with a metacheck older than api_keys_use().
    # On someone's own computer (METACHECK_LOCAL) the keys are their own.
    if (exists("api_keys_use", envir = asNamespace("metacheck"))) {
      metacheck::api_keys_use(
        keys, shared = !identical(tolower(Sys.getenv("METACHECK_LOCAL")), "true"))
    }
    do.call(fun, args)
  }
  proc <- callr::r_bg(
    runner,
    args = list(fun = fun, args = args, app_dir = getwd(),
                progress_file = progress_file, keys = keys),
    # "" = the job's messages and warnings go to this app's own log file.
    stdout = "", stderr = "",
    # The job is stopped if this app's R process ends.
    supervise = TRUE
  )
  list(proc = proc, progress_file = progress_file)
}

# The job's latest progress, as list(value, detail), or NULL if none yet. The
# job keeps one line in the file ("value<TAB>detail"), replaced on each update.
.bg_progress_read <- function(progress_file) {
  line <- tryCatch(readLines(progress_file, n = 1, warn = FALSE, encoding = "UTF-8"),
                   error = function(e) character(0))
  if (length(line) == 0 || !nzchar(line)) return(NULL)
  parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
  list(value  = suppressWarnings(as.numeric(parts[1])),
       detail = if (length(parts) > 1) paste(parts[-1], collapse = " ") else "")
}

# The message of an error raised inside the job (callr wraps it).
.bg_error_message <- function(err) {
  inner <- err$parent %||% err
  msg <- conditionMessage(inner)
  if (!nzchar(msg)) msg <- conditionMessage(err)
  msg
}

#' Watch a job: show its progress, then hand over its result
#'
#' @param job as returned by bg_start().
#' @param session the Shiny session.
#' @param message the progress bar's title.
#' @param on_done function(result) called with the job's return value.
#' @param on_error function(message) called if the job failed.
#' @param heartbeat optional path (e.g. a capacity slot folder) whose
#'   modification time is refreshed while the job runs, so a long job's slot
#'   is not mistaken for one left behind by a crashed run.
#' @param max_minutes the job is stopped after this long. Without a limit, a
#'   job stuck on an unresponsive outside service would hold its slot forever,
#'   since the heartbeat keeps the slot fresh.
#' @return a function that stops the job (e.g. when the visitor leaves).
bg_watch <- function(job, session, message, on_done, on_error,
                     heartbeat = NULL, max_minutes = 30) {
  progress <- shiny::Progress$new(session, min = 0, max = 1)
  progress$set(message = message, value = 0)
  started <- Sys.time()
  finished <- FALSE

  finish <- function() {
    finished <<- TRUE
    # Closing fails harmlessly if the visitor's session has already ended.
    try(progress$close(), silent = TRUE)
    unlink(job$progress_file)
  }

  watcher <- shiny::observe({
    shiny::invalidateLater(500, session)
    upd <- .bg_progress_read(job$progress_file)
    if (!is.null(upd)) {
      progress$set(value = if (is.na(upd$value)) NULL else upd$value,
                   detail = upd$detail)
    }
    if (!is.null(heartbeat) && nzchar(heartbeat) && dir.exists(heartbeat)) {
      Sys.setFileTime(heartbeat, Sys.time())
    }
    if (job$proc$is_alive()) {
      if (difftime(Sys.time(), started, units = "mins") < max_minutes) return()
      watcher$destroy()
      job$proc$kill()
      finish()
      on_error(sprintf("Stopped after reaching the time limit of %d minutes.",
                       as.integer(max_minutes)))
      return()
    }

    watcher$destroy()
    result <- tryCatch(job$proc$get_result(), error = function(e) e)
    finish()
    if (inherits(result, "error")) {
      on_error(.bg_error_message(result))
    } else {
      on_done(result)
    }
  })

  # Stop the job and the watcher; safe to call more than once.
  function() {
    if (finished) return(invisible())
    watcher$destroy()
    if (job$proc$is_alive()) job$proc$kill()
    finish()
    invisible()
  }
}
