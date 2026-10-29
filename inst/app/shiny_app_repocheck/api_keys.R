## api_keys.R ---------------------------------------------------------------
## Visitors' own API keys, and a notice when metacheck's key ran out.
##
## The server's own keys (GITHUB_PAT, OSF_PAT, DRYAD_*, RESEARCHBOX_API_KEY,
## ...) are environment variables, read by this app and every job it starts.
## When one is refused or reached its limit, metacheck records that (see
## metacheck::api_key_status()) and api_key_notice() tells the visitor, so
## they can enter their own key. A visitor's key is passed to their own job
## only (bg_start()'s `keys`, applied there with metacheck::api_keys_use()):
## it is never set in this app's process, which every visitor shares, and is
## not written anywhere but the job's own temporary files.
##
## Kept identical in every app folder that uses it (like background_job.R).

# How each service's key is labelled in the form
API_KEY_LABELS <- c(
  github = "GitHub token",
  osf = "OSF token",
  dryad = "Dryad token",
  researchbox = "ResearchBox API key",
  gitlab = "GitLab token",
  openalex = "OpenAlex API key"
)

# Messages in this app call the server's keys "the key provided by
# metacheck", except on someone's own computer (METACHECK_LOCAL), where the
# keys are their own
options(metacheck.keys.shared =
          !identical(tolower(Sys.getenv("METACHECK_LOCAL")), "true"))

# The collapsible box with one password field per service in `services`
api_keys_box <- function(services) {
  box(title = "Your own API keys (optional)", collapsible = TRUE,
      collapsed = TRUE, width = 12,
    tags$p(
      "Only needed when the notice above says that a key provided by ",
      "metacheck has reached its limit. A key you enter is used for your own ",
      "check only, and is removed when the check ends."),
    lapply(services, function(s) {
      passwordInput(paste0("api_key_", s), API_KEY_LABELS[[s]], width = "100%")
    })
  )
}

# The keys the visitor entered, as a named list (empty fields left out)
api_keys_entered <- function(input, services) {
  keys <- lapply(services, function(s) trimws(input[[paste0("api_key_", s)]] %||% ""))
  names(keys) <- services
  keys[nzchar(unlist(keys))]
}

# A notice for each service in `services` whose key (the server's, or none)
# was refused or reached its limit; NULL when there is none. `where` says
# where the key fields are. Checked again every `every_ms`.
api_key_notice <- function(services, session, where = "below",
                           every_ms = 30000) {
  invalidateLater(every_ms, session)
  st <- tryCatch(metacheck::api_key_status(), error = function(e) NULL)
  if (is.null(st)) return(NULL)
  st <- st[st$service %in% services & !is.na(st$message), , drop = FALSE]
  if (nrow(st) == 0) return(NULL)
  tags$div(class = "busy-banner busy-full",
    lapply(st$message, function(m) tags$p(icon("key"), " ", m)),
    tags$p(sprintf("You can enter your own key under \"Your own API keys\" %s.", where))
  )
}
