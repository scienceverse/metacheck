# API keys for the data repositories and databases metacheck reads.
#
# A key is read from an R option first and from an environment variable
# second (osf_pat(), github_pat(), dryad_pat(), rbox_token(), gitlab_pat(),
# openalex_key()). On a shared server (the metacheck apps and API) the
# server's own keys are environment variables, read by every R process the
# server starts. A visitor's own key is set as an option inside that visitor's
# job only (api_keys_use()), so for that job it takes the place of the
# server's key, and it never reaches the process the other visitors share.
#
# What went wrong with a key -- the service refused it, or its limit was
# reached -- is written to a small file in metacheck's state folder
# (.metacheck_state_dir()), which every R process of the same user on the
# computer reads: the app, each job, the API's jobs. So a job started after
# another job used up the server's key knows it before sending anything, and
# the app can tell visitors that the key metacheck provides has run out and
# that they can enter their own (api_key_status()). Problems with a visitor's
# own key are not written there: they concern that visitor's job only.

# The services for which a key is worth setting on a shared server: a key is
# required (Dryad downloads, the ResearchBox API) or raises a limit a shared
# server reaches (GitHub 60 -> 5,000 requests per hour; OSF 100 per hour ->
# 10,000 per day; GitLab and OpenAlex). Zenodo, Figshare, Dataverse and the
# other repositories gave no gain for public records when checked on
# 2026-10-28, so their keys are only for a user's own private records.
.key_services <- list(
  github = list(
    label = "GitHub", option = "metacheck.github.pat", env = "GITHUB_PAT",
    url = "https://github.com/settings/personal-access-tokens/new"),
  osf = list(
    label = "OSF", option = "metacheck.osf.pat", env = "OSF_PAT",
    url = "https://osf.io/settings/tokens"),
  dryad = list(
    label = "Dryad", option = "metacheck.dryad.pat", env = "DRYAD_PAT",
    url = "https://datadryad.org/account"),
  researchbox = list(
    label = "ResearchBox", option = "metacheck.researchbox.token",
    env = "RESEARCHBOX_API_KEY", url = "https://researchbox.org/api"),
  gitlab = list(
    label = "GitLab", option = "metacheck.gitlab.pat", env = "GITLAB_PAT",
    url = "https://gitlab.com/-/user_settings/personal_access_tokens"),
  openalex = list(
    label = "OpenAlex", option = "metacheck.openalex.key",
    env = "OPENALEX_API_KEY", url = "https://openalex.org/settings/api-key")
)

# The service whose key a request to `host` uses, or NA. Files on
# raw.githubusercontent.com are fetched without the GitHub key, so its limit
# is not the key's.
.key_service_for_host <- function(host) {
  host <- tolower(host %||% "")
  domains <- list(github = "github.com", osf = "osf.io",
                  dryad = "datadryad.org", researchbox = "researchbox.org",
                  gitlab = "gitlab.com", openalex = "openalex.org")
  for (service in names(domains)) {
    d <- domains[[service]]
    if (host == d || endsWith(host, paste0(".", d))) return(service)
  }
  NA_character_
}

# The key a request to `service` would use now ("" when none is set)
.key_get <- function(service) {
  switch(service,
         github = github_pat(),
         osf = osf_pat(),
         dryad = dryad_pat(),
         researchbox = rbox_token(),
         gitlab = gitlab_pat(),
         openalex = openalex_key(),
         "") %||% ""
}

# Whose key a request to `service` uses: "user" for a key set as an option
# (by api_keys_use() or a setter such as osf_pat("...")) that differs from the
# environment variable, "server" for the environment variable's key (Dryad:
# also its client id and secret), "none" without a key.
.key_source <- function(service) {
  s <- .key_services[[service]]
  env <- Sys.getenv(s$env)
  opt <- getOption(s$option) %||% ""
  if (nzchar(opt) && !identical(opt, env)) return("user")
  if (nzchar(env)) return("server")
  if (service == "dryad" && nzchar(Sys.getenv("DRYAD_CLIENT_ID")) &&
      nzchar(Sys.getenv("DRYAD_CLIENT_SECRET"))) return("server")
  if (service == "github" && nzchar(.key_get("github"))) return("server")
  "none"
}

# The folder for what metacheck keeps between R sessions and shares between
# the R processes of one user (key problems, ResearchBox's count of requests
# for boxes that could not be found). Unlike the caches it is not in the
# working directory: each job of the apps and the API runs in a folder of its
# own, which is removed when the job ends.
.metacheck_state_dir <- function() {
  dir <- getOption("metacheck.state.dir") %||% Sys.getenv("METACHECK_STATE_DIR")
  if (!nzchar(dir)) {
    dir <- file.path(rappdirs::user_data_dir("metacheck", "scienceverse"), "state")
  }
  dir
}

.key_status_path <- function() {
  file.path(.metacheck_state_dir(), "key_status.rds")
}

.key_status_read <- function() {
  path <- .key_status_path()
  st <- if (file.exists(path)) tryCatch(readRDS(path), error = \(e) NULL)
  if (is.list(st)) st else list()
}

# Written to a temporary file first and then renamed, so a process reading
# the file never sees half of it.
.key_status_write <- function(st) {
  path <- .key_status_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("key_status", tmpdir = dirname(path), fileext = ".rds")
  ok <- tryCatch({ saveRDS(st, tmp); file.rename(tmp, path) },
                 error = \(e) FALSE, warning = \(w) FALSE)
  if (!isTRUE(ok)) unlink(tmp)
  invisible(isTRUE(ok))
}

# Record that `service` refused its key ("rejected") or that its limit was
# reached ("limit") until `until`. Not for a visitor's own key (see the top of
# this file). A rejected key is tried again after an hour, so a key replaced
# on the server is used again without anyone clearing the record.
.key_problem_record <- function(service, problem, until = NA,
                                source = .key_source(service)) {
  if (identical(source, "user")) return(invisible(NULL))
  if (problem == "rejected") until <- Sys.time() + 3600
  if (is.na(until)) until <- Sys.time() + 3600
  st <- .key_status_read()
  st[[service]] <- list(problem = problem, source = source,
                        until = as.POSIXct(until), time = Sys.time())
  .key_status_write(st)
  invisible(NULL)
}

.key_problem_clear <- function(service) {
  st <- .key_status_read()
  if (is.null(st[[service]])) return(invisible(NULL))
  st[[service]] <- NULL
  .key_status_write(st)
  invisible(NULL)
}

# The current problem of the key `service` uses now, or NULL. A record about
# another source (a key was added or removed since) or whose time has passed
# does not count.
.key_problem <- function(service, source = .key_source(service)) {
  p <- .key_status_read()[[service]]
  if (is.null(p) || identical(source, "user")) return(NULL)
  if (!identical(p$source, source)) return(NULL)
  if (!is.na(p$until) && p$until <= Sys.time()) return(NULL)
  p
}

# The sentence shown to the user for a key problem. `shared` is TRUE in the
# apps and the API (see api_keys_use()), where the key is metacheck's, not the
# user's own.
.key_problem_message <- function(service, problem, until = NA,
                                 source = .key_source(service),
                                 shared = isTRUE(getOption("metacheck.keys.shared"))) {
  s <- .key_services[[service]]
  when <- if (length(until) == 1 && !is.na(until)) {
    paste0(" until ", format(as.POSIXct(until), "%Y-%m-%d %H:%M UTC", tz = "UTC"))
  } else ""
  whose <- switch(source,
    user = sprintf("Your %s key", s$label),
    server = if (shared) sprintf("The %s key provided by metacheck", s$label)
             else sprintf("The %s key in %s", s$label, s$env),
    sprintf("%s's limit for requests without a key", s$label))
  what <- if (problem == "rejected") {
    sprintf("%s was rejected by %s; it may have expired or been withdrawn.",
            whose, s$label)
  } else if (source == "none") {
    sprintf("%s was reached%s.", whose, when)
  } else {
    sprintf("%s has reached %s's limit%s.", whose, s$label, when)
  }
  ask <- if (source == "user") {
    sprintf("Check the key, or make a new one at %s", s$url)
  } else {
    sprintf("Please provide your own %s key to continue (you can make one at %s)",
            s$label, s$url)
  }
  paste(what, ask)
}

# Record that the limit of `service` was reached and return the message for
# it, prefixed with "API rate limit exhausted: " like download_repo_files()'s
# own rate-limit errors.
.key_limit_reached <- function(service, until = NA) {
  source <- .key_source(service)
  .key_problem_record(service, "limit", until, source)
  paste0("API rate limit exhausted: ",
         .key_problem_message(service, "limit", until, source))
}

# Keys already checked in this R process (in memory only, never on disk):
# each key is checked once per process, see .key_use().
.key_checked <- new.env(parent = emptyenv())

# The key to send to `service`: its current key, or "" when there is none or
# the service refused it. A key the service refuses makes it answer 401 even
# for public records (OSF, GitHub, GitLab, OpenAlex, Dataverse; checked
# 2026-10-28), so one wrong or expired key would stop every request. Each key
# is therefore checked once per R process with `check` (a function of the key
# returning TRUE, FALSE for refused, or NA when the check itself failed),
# and a refused key is left out, with a warning, so requests go without a key.
.key_use <- function(service, key = .key_get(service), check = NULL,
                     id = service) {
  if (!nzchar(key %||% "") || is.null(check)) return(key %||% "")
  memo <- paste(id, key)
  ok <- .key_checked[[memo]]
  if (is.null(ok)) {
    ok <- tryCatch(check(key), error = \(e) NA)
    assign(memo, ok, envir = .key_checked)
    known <- service %in% names(.key_services)
    if (isFALSE(ok) && known) {
      source <- .key_source(service)
      .key_problem_record(service, "rejected", source = source)
      warning(.key_problem_message(service, "rejected", source = source),
              " Continuing without the key.", call. = FALSE)
    } else if (isFALSE(ok)) {
      warning("The ", id, " key was rejected; continuing without the key.",
              call. = FALSE)
    } else if (isTRUE(ok) && known &&
               identical(.key_status_read()[[service]]$problem, "rejected")) {
      .key_problem_clear(service)
    }
  }
  if (isFALSE(ok)) "" else key
}

# One request to `url` with `headers`: TRUE for 200, FALSE for 401/403, NA
# for anything else (no connection, the service down).
.key_check_request <- function(url, headers) {
  resp <- httr2::request(url) |>
    httr2::req_headers(!!!headers, `User-Agent` = "metacheck") |>
    httr2::req_timeout(15) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()
  status <- httr2::resp_status(resp)
  if (status == 200L) TRUE else if (status %in% c(401L, 403L)) FALSE else NA
}

#' Check the API keys metacheck uses
#'
#' Lists the services for which metacheck uses an API key, whether a key is
#' set, and whether one was recently refused or reached its limit. On a shared
#' server (the metacheck apps and API) the keys are the server's own; the apps
#' show the `message` to visitors so they can enter their own key.
#'
#' Problems are recorded in a file in metacheck's state folder, shared by
#' every R session of the same user on the computer. Its location is the
#' `metacheck.state.dir` option, else the `METACHECK_STATE_DIR` environment
#' variable, else a folder `state` in `rappdirs::user_data_dir("metacheck",
#' "scienceverse")`.
#'
#' @returns a data frame with one row per service: `service`, `label`, `key`
#'   (`"user"`, `"server"` or `"none"`, see [api_keys_use()]), `problem`
#'   (`NA`, `"rejected"` or `"limit"`), `until` (when a limit ends), `url`
#'   (where a key is made) and `message` (`NA` without a problem)
#' @seealso [api_keys_use()]
#' @export
#' @examples
#' \dontrun{
#' api_key_status()
#' }
api_key_status <- function() {
  rows <- lapply(names(.key_services), \(service) {
    s <- .key_services[[service]]
    source <- .key_source(service)
    p <- .key_problem(service, source)
    data.frame(
      service = service, label = s$label, key = source,
      problem = p$problem %||% NA_character_,
      until = if (is.null(p)) as.POSIXct(NA) else p$until,
      url = s$url,
      message = if (is.null(p)) NA_character_
                else .key_problem_message(service, p$problem, p$until, source),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Use your own API keys
#'
#' Sets keys for the rest of the R session, in place of the keys set as
#' environment variables. The metacheck apps and API call this inside each
#' visitor's own job with the keys the visitor entered, so the keys are used
#' for that job only.
#'
#' @param keys a named list or character vector of keys, named by service:
#'   `github`, `osf`, `dryad`, `researchbox`, `gitlab`, `openalex` (see
#'   [api_key_status()]). Empty and missing keys are skipped, so the key set
#'   as an environment variable is used for those services.
#' @param shared `TRUE` when the environment-variable keys belong to a shared
#'   server, so that messages call them "the key provided by metacheck"
#'
#' @returns invisibly, the previous values of the options that were set, to
#'   restore them with [options()]
#' @seealso [api_key_status()]
#' @export
#' @examples
#' \dontrun{
#' old <- api_keys_use(list(github = "github_pat_...", osf = ""))
#' options(old)
#' }
api_keys_use <- function(keys = list(), shared = FALSE) {
  keys <- as.list(keys)
  unknown <- setdiff(names(keys), names(.key_services))
  if (length(keys) && (is.null(names(keys)) || length(unknown))) {
    stop("Name each key by its service: ",
         paste(names(.key_services), collapse = ", "), call. = FALSE)
  }
  keys <- Filter(\(k) is.character(k) && length(k) == 1 && !is.na(k) &&
                   nzchar(trimws(k)), keys)
  opts <- lapply(keys, trimws)
  names(opts) <- vapply(names(keys), \(s) .key_services[[s]]$option, character(1))
  opts$metacheck.keys.shared <- isTRUE(shared)
  invisible(options(opts))
}
