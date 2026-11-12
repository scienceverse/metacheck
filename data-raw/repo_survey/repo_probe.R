# Which software runs each DataCite repository? One DOI per repository (DataCite
# sample, one request), its registered address, then a few light requests to
# that host: the landing page (generator tag, page text) and the typical API
# address of each repository software. Saved per batch of 100 hosts; a restart
# skips saved batches.
Sys.unsetenv(c("GITHUB_PAT", "GITHUB_TOKEN", "OSF_PAT"))
suppressMessages(library(httr2))
sp <- "data-raw/repo_survey/"
# Usage: Rscript repo_probe.R [crossref]  (default: the DataCite sample)
cr <- identical(commandArgs(TRUE)[1], "crossref")
bd <- paste0(sp, if (cr) "probe_cr/" else "probe/"); dir.create(bd, showWarnings = FALSE)

if (cr) {
  s <- readRDS(paste0(sp, "cr_sample.rds"))[, c("doi", "url")]
} else {
  j <- jsonlite::fromJSON(paste0(sp, "dc_sample.json"))
  s <- data.frame(doi = j$data$id, url = j$data$attributes$url)
}
s$prefix <- tolower(sub("/.*", "", s$doi))
s <- s[!is.na(s$url) & grepl("^https?://", s$url), ]
s$host0 <- tolower(sub("^https?://([^/:?#]+).*", "\\1", s$url))
hosts <- unique(s$host0)
cat("DOIs:", nrow(s), " hosts:", length(hosts), "\n")

ua <- "metacheck repository survey (https://github.com/scienceverse/metacheck)"
rq <- function(u) request(u) |> req_user_agent(ua) |> req_timeout(20) |>
  req_error(is_error = \(r) FALSE)
body <- function(r, n = 2e5) {
  if (!inherits(r, "httr2_response")) return(NA_character_)
  b <- tryCatch(resp_body_raw(r), error = \(e) raw(0))
  if (length(b) > n) b <- b[seq_len(n)]
  tryCatch(iconv(rawToChar(b[b != as.raw(0)]), "", "UTF-8", sub = ""), error = \(e) NA_character_)
}
status <- function(r) if (inherits(r, "httr2_response")) resp_status(r) else NA_integer_
par <- function(urls) req_perform_parallel(lapply(urls, rq), on_error = "continue",
                                           max_active = 20, progress = FALSE)
gen <- function(x) {
  m <- regmatches(x, regexpr('(?i)<meta[^>]+name=["\']generator["\'][^>]*>', x, perl = TRUE))
  if (!length(m)) return(NA_character_)
  sub('(?i).*content=["\']([^"\']+)["\'].*', "\\1", m, perl = TRUE)
}

batches <- split(hosts, ceiling(seq_along(hosts) / 100))
for (b in seq_along(batches)) {
  f <- sprintf("%sb%03d.rds", bd, b)
  if (file.exists(f)) next
  h <- batches[[b]]
  # the landing page of one DOI per host, following redirects
  u <- s$url[match(h, s$host0)]
  lp <- par(u)
  final <- vapply(lp, \(r) if (inherits(r, "httr2_response")) r$url else NA_character_, "")
  fh <- ifelse(is.na(final), h, tolower(sub("^https?://([^/:?#]+).*", "\\1", final)))
  html <- vapply(lp, body, "")
  base <- paste0("https://", fh)
  # typical API addresses of each software
  ds7 <- par(paste0(base, "/server/api"))
  dsl <- par(paste0(base, "/rest/status"))
  dv  <- par(paste0(base, "/api/info/version"))
  inv <- par(paste0(base, "/api/records?size=1"))
  ck  <- par(paste0(base, "/api/3/action/status_show"))
  d <- data.frame(host0 = h, landing = u, final = final, host = fh,
    landing_status = vapply(lp, status, 1L),
    generator = vapply(html, gen, ""),
    page_dspace = grepl("(?i)dspace", html, perl = TRUE),
    page_eprints = grepl("(?i)eprints", html, perl = TRUE),
    page_dataverse = grepl("(?i)dataverse", html, perl = TRUE),
    page_invenio = grepl("(?i)invenio", html, perl = TRUE),
    page_ckan = grepl("(?i)ckan", html, perl = TRUE),
    page_ojs = grepl("(?i)pkp|open journal systems", html, perl = TRUE),
    dspace7 = vapply(ds7, \(r) status(r) %in% 200 && grepl("dspaceVersion|dspaceUI", body(r, 5e4)), TRUE),
    dspace7_version = vapply(ds7, \(r) { x <- body(r, 5e4); m <- regmatches(x, regexpr('"dspaceVersion"\\s*:\\s*"[^"]+"', x)); if (length(m)) sub('.*:\\s*"', "", sub('"$', "", m)) else NA_character_ }, ""),
    dspace_legacy = vapply(dsl, \(r) status(r) %in% 200 && grepl('"okay"', body(r, 5e4)), TRUE),
    dataverse = vapply(dv, \(r) status(r) %in% 200 && grepl('"version"', body(r, 5e4)), TRUE),
    invenio = vapply(inv, \(r) status(r) %in% 200 && grepl('"hits"', body(r, 5e4)), TRUE),
    ckan = vapply(ck, \(r) status(r) %in% 200 && grepl('ckan_version', body(r, 5e4)), TRUE),
    row.names = NULL)
  saveRDS(d, f)
  cat("batch", b, "of", length(batches), " dspace7", sum(d$dspace7), " legacy", sum(d$dspace_legacy),
      " dataverse", sum(d$dataverse), " invenio", sum(d$invenio), " ckan", sum(d$ckan), "\n")
}
cat("done\n")
