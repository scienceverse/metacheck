# Hosts of one repository program and the DOI prefixes registered on them
# (#636, #637). Usage, from the package folder:
#   Rscript data-raw/repo_survey/repo_hosts.R dspace|dataverse|invenio|ckan|eprints
#
# Hosts: every host the survey found running the program (DataCite's
# repositories, repo_tables/datacite_hosts.csv, and Crossref's dataset
# publishers, crossref_probe.csv), and every host metacheck already lists. A
# host metacheck does not list yet is kept only when it passes the check
# metacheck's own code relies on (see `checks` below).
# Prefixes: DataCite's count of DOIs per prefix whose registered address is on
# the host (url:*<host>*, disable-facets=false; one request per host, at most
# one a second). Starting from the host finds prefixes of repositories that
# share a host (comum.rcaap.pt) or have DOIs on several hosts. Asked for every
# host, or for Dataverse only for hosts without a prefix in
# .dataverse_doi_prefix_hosts() (the others were verified 2026-09-19).
#
# Writes repo_tables/<program>_hosts.csv and <program>_host_prefixes.csv;
# working files per host in <program>_hosts/, and a restart skips saved hosts.
Sys.unsetenv(c("GITHUB_PAT", "GITHUB_TOKEN", "OSF_PAT"))
suppressMessages(library(httr2))
suppressMessages(pkgload::load_all(".", quiet = TRUE))
sw <- commandArgs(TRUE)[1]
stopifnot(sw %in% c("dspace", "dataverse", "invenio", "ckan", "eprints"))
sp <- "data-raw/repo_survey/"
out <- "data-raw/repo_tables/"; dir.create(out, showWarnings = FALSE)
wd <- paste0(sp, sw, "_hosts/"); dir.create(wd, showWarnings = FALSE)
ua <- "metacheck repository survey (https://github.com/scienceverse/metacheck)"
rq <- function(u) request(u) |> req_user_agent(ua) |> req_timeout(60) |> req_error(is_error = \(r) FALSE)
get <- function(u) tryCatch(req_perform(rq(u)), error = \(e) NULL)
ok <- function(r, pattern) !is.null(r) && resp_status(r) == 200 &&
  grepl(pattern, tryCatch(resp_body_string(r), error = \(e) ""))
nw <- function(x) sub("^www\\.", "", tolower(x))

# The check each program's code relies on; returns the programming interface
# found, or "none"
checks <- list(
  dspace = function(host) {
    if (ok(get(paste0("https://", host, "/server/api")), '"_links"')) "dspace7" else
      if (ok(get(paste0("https://", host, "/rest/test")), "REST api is running")) "legacy" else "none"
  },
  dataverse = function(host) if (ok(get(paste0("https://", host, "/api/info/version")), '"version"')) "dataverse" else "none",
  invenio = function(host) if (ok(get(paste0("https://", host, "/api/records?size=1")), '"hits"')) "invenio" else "none",
  ckan = function(host) if (ok(get(paste0("https://", host, "/api/3/action/status_show")), "ckan_version")) "ckan" else "none",
  eprints = function(host) if (ok(get(paste0("https://", host, "/cgi/oai2?verb=Identify")), "(?i)eprints")) "eprints" else "none"
)
# Hosts metacheck lists by hand (the survey's own hosts, R/repo-tables.R, are
# not counted as known, so a new survey checks them again)
known <- switch(sw,
  dspace = list(dspace7 = nw(setdiff(.dspace7_hosts(), .dspace7_hosts_survey())),
                legacy = nw(setdiff(.dspace_legacy_hosts(), .dspace_legacy_hosts_survey()))),
  dataverse = list(dataverse = nw(setdiff(.dataverse_hosts(), .dataverse_hosts_survey()))),
  invenio = list(invenio = c("zenodo.org", "sandbox.zenodo.org")),
  eprints = list(eprints = "reshare.ukdataservice.ac.uk"),
  list())
known_all <- unlist(known, use.names = FALSE)
facet_all <- sw != "dataverse"
mapped <- if (sw == "dataverse")
  nw(setdiff(names(.dataverse_doi_prefix_hosts()), names(.dataverse_doi_prefix_hosts_survey()))) else character(0)

h <- read.csv(paste0(out, "datacite_hosts.csv"))
cr <- read.csv(paste0(sp, "crossref_probe.csv"))
g <- tolower(cr$generator)
cr$found <- dplyr::case_when(cr$dspace7 | grepl("dspace", g) | cr$dspace_legacy ~ "dspace",
  cr$dataverse | grepl("dataverse", g) ~ "dataverse", cr$invenio | grepl("invenio", g) ~ "invenio",
  cr$ckan | grepl("ckan", g) ~ "ckan", grepl("eprints", g) ~ "eprints", TRUE ~ "")
survey <- unique(nw(c(h$host[h$found %in% sw], cr$host[cr$found %in% sw])))
hosts <- unique(c(survey, known_all))
cat(sw, "hosts:", length(hosts), "| known:", length(known_all), "\n")

# DataCite's search splits a host at a hyphen, so url:*<host>* finds no DOI
# for a hyphenated host (none of 56, 2026-11-13); a phrase search finds them,
# and gives the same answer as the wildcard for a host without a hyphen
dc_url <- function(host) paste0("https://api.datacite.org/dois?query=",
  utils::URLencode(if (grepl("-", host)) sprintf('url:"%s"', host) else sprintf("url:*%s*", host), reserved = TRUE),
  "&page[size]=1&fields[dois]=url&disable-facets=false")

for (host in hosts) {
  f <- paste0(wd, gsub("[^a-z0-9.-]", "_", host), ".rds")
  api <- NA_character_
  if (file.exists(f)) {
    old <- readRDS(f)
    # a hyphenated host asked before the phrase search was used is asked
    # again; its check is not repeated
    if (!(grepl("-", host) && !isTRUE(old$phrase) && isTRUE(old$host$datacite_ok))) next
    api <- old$host$api; checked <- old$host$checked
  }
  if (is.na(api)) {
    # (CKAN has no host metacheck lists yet: `known` is empty)
    api <- if (length(known)) names(known)[vapply(known, \(k) host %in% k, TRUE)][1] else NA_character_
    checked <- "earlier"
    if (is.na(api)) { api <- checks[[sw]](host); checked <- "now" }
  }
  pre <- data.frame(host = character(0), prefix = character(0), n_dois = integer(0)); total <- NA; dc_ok <- NA
  if (api != "none" && (facet_all || !host %in% mapped)) {
    Sys.sleep(1)
    u <- dc_url(host)
    r <- tryCatch(req_perform(rq(u) |> req_retry(max_tries = 3)), error = \(e) NULL)
    j <- if (!is.null(r) && resp_status(r) == 200) tryCatch(resp_body_json(r, simplifyVector = TRUE), error = \(e) NULL)
    dc_ok <- !is.null(j); total <- j$meta$total %||% NA
    pf <- j$meta$prefixes
    if (is.data.frame(pf) && nrow(pf)) pre <- data.frame(host = host, prefix = tolower(pf$id), n_dois = pf$count)
  }
  saveRDS(list(host = data.frame(host = host, api = api, checked = checked, known = host %in% known_all,
                                 in_survey = host %in% survey, n_dois = total, datacite_ok = dc_ok),
               prefixes = pre, phrase = grepl("-", host)), f)
  cat(host, api, nrow(pre), "prefixes\n")
}
R <- lapply(list.files(wd, full.names = TRUE), readRDS)
H <- dplyr::bind_rows(lapply(R, `[[`, "host")); P <- dplyr::bind_rows(lapply(R, `[[`, "prefixes"))
write.csv(H[order(H$host), ], paste0(out, sw, "_hosts.csv"), row.names = FALSE)
write.csv(P[order(P$host, -P$n_dois), ], paste0(out, sw, "_host_prefixes.csv"), row.names = FALSE)
cat("\nhosts by programming interface (columns: already listed):\n"); print(table(H$api, H$known))
cat("hosts where DataCite failed:", sum(H$datacite_ok %in% FALSE), "\n")
cat("hosts with 10 prefixes (DataCite's limit, the list may be longer):", sum(table(P$host) >= 10), "\n")
cat("distinct prefixes:", length(unique(P$prefix)), "\n")
