# Writes R/repo-tables.R: the repository hosts and DOI prefixes the
# repository survey found (#635, #636), from the tables in data-raw/repo_tables/
# (made by data-raw/repo_survey/repo_hosts.R). Run from the package folder:
#   Rscript data-raw/repo_tables.R
# The hand-written lists (.dspace7_hosts(), .dspace_legacy_hosts(),
# .dataverse_hosts(), .dataverse_doi_prefix_hosts()) keep their entries and
# comments; what is written here is what they do not have.
d <- "data-raw/repo_tables/"
out <- "R/repo-tables.R"
# the hand-written lists call the generated functions: they are first written
# empty, so what the package gives below is the hand-written lists alone
writeLines(c(
  ".dspace7_hosts_survey <- function() character(0)",
  ".dspace_legacy_hosts_survey <- function() character(0)",
  ".dspace7_doi_prefixes <- function() character(0)",
  ".dataverse_hosts_survey <- function() character(0)",
  ".dataverse_doi_prefix_hosts_survey <- function() list()",
  ".eprints_hosts_survey <- function() character(0)",
  ".eprints_doi_prefixes <- function() character(0)",
  ".invenio_hosts_survey <- function() character(0)",
  ".invenio_doi_prefixes <- function() character(0)",
  ".ckan_hosts_survey <- function() character(0)",
  ".ckan_doi_prefixes <- function() character(0)"), out)
suppressMessages(pkgload::load_all(".", quiet = TRUE))
nw <- function(x) sub("^www\\.", "", tolower(x))
S <- read.csv(paste0(d, "datacite_hosts.csv"), colClasses = "character"); S$host <- nw(S$host)
# the prefixes of every repository whose sampled DOIs landed on one of `hosts`
# after following redirects: a repository that registers its DOIs with a
# handle address (hdl.handle.net/20.500.11850/..., ETH Zürich) or an old host
# name (smartech.gatech.edu) is found only this way
repo_prefixes <- function(hosts) tolower(unlist(strsplit(stats::na.omit(S$prefixes[S$host %in% hosts]), " ")))
# the prefixes of the other detectors' own platforms (OSF, Zenodo, Dryad,
# Figshare, 4TU, Mendeley Data, LDbase, DataONE, ReShare, PsychArchives),
# never listed here: one of their DOIs can be registered with an address on a
# repository host (10.6084 on dataverse.no, #434)
dv_hand <- .dataverse_doi_prefix_hosts()
other <- setdiff(.dspace7_skip_doi_prefixes(), c("10.7910", unlist(.dataverse_doi_prefix_hosts(), use.names = FALSE)))
num_order <- function(p) p[order(as.integer(sub("^10\\.", "", p)))]

# ---- DSpace ----
H <- read.csv(paste0(d, "dspace_hosts.csv"), colClasses = "character"); P <- read.csv(paste0(d, "dspace_host_prefixes.csv"), colClasses = "character")
H$known <- H$known %in% "TRUE"
new7 <- sort(H$host[H$api %in% "dspace7" & !H$known])
new_legacy <- sort(H$host[H$api %in% "legacy" & !H$known])
# a DOI is looked up only when it can lead to a DSpace 7 host: the prefixes
# with DOIs registered on one (DataCite's count per host), and those of the
# repositories whose DOIs land on one
h7 <- H$host[H$api %in% "dspace7"]
pre7 <- num_order(setdiff(unique(tolower(c(P$prefix[P$host %in% h7], repo_prefixes(h7)))), other))

# ---- Dataverse ----
HV <- read.csv(paste0(d, "dataverse_hosts.csv"), colClasses = "character"); PV <- read.csv(paste0(d, "dataverse_host_prefixes.csv"), colClasses = "character")
HV$known <- HV$known %in% "TRUE"
dv_ok <- HV$host[HV$api %in% "dataverse"]
new_dv <- sort(HV$host[HV$api %in% "dataverse" & !HV$known])
# per host: the prefixes DataCite counts on it, and those of the repositories
# whose DOIs land on it; less the other platforms' and the ones the
# hand-written table already gives that host
dv_hand_n <- dv_hand; names(dv_hand_n) <- nw(names(dv_hand_n))
dv_map <- list()
for (h in sort(dv_ok)) {
  p <- unique(tolower(c(PV$prefix[PV$host == h], repo_prefixes(h))))
  p <- setdiff(p, c(other, dv_hand_n[[h]]))
  if (length(p)) dv_map[[h]] <- num_order(p)
}

# ---- EPrints ----
# every EPrints host that passes the check: publication repositories hold
# datasets too (eprints.soton.ac.uk, wrap.warwick.ac.uk), and repo_check
# tells a publication from a deposit per record, not per host (#637;
# R/repo-kind.R). eprints_types.csv (repo_types.R) gives each host's share
# of datasets, for information.
HE <- read.csv(paste0(d, "eprints_hosts.csv"), colClasses = "character"); PE <- read.csv(paste0(d, "eprints_host_prefixes.csv"), colClasses = "character")
HE$known <- HE$known %in% "TRUE"
ep_hosts <- HE$host[HE$api %in% "eprints"]
new_ep <- sort(setdiff(ep_hosts, HE$host[HE$known]))
# their prefixes, less the other platforms' and ReShare's own (10.5255, whose
# DOIs carry the eprint id and are read without a lookup)
pre_ep <- num_order(setdiff(unique(tolower(c(PE$prefix[PE$host %in% ep_hosts], repo_prefixes(ep_hosts)))),
                            c(other, "10.5255")))

# ---- Invenio ----
# every Invenio host that passes the check (GET /api/records?size=1 answers
# with search hits), less test and sandbox installations, which hold no real
# research; Zenodo's own prefix (10.5281) is among the other platforms'
HI <- read.csv(paste0(d, "invenio_hosts.csv"), colClasses = "character"); PI <- read.csv(paste0(d, "invenio_host_prefixes.csv"), colClasses = "character")
HI$known <- HI$known %in% "TRUE"
inv_hosts <- HI$host[HI$api %in% "invenio" & !grepl("^(sandbox|test)", HI$host)]
new_inv <- sort(setdiff(inv_hosts, HI$host[HI$known]))
pre_inv <- num_order(setdiff(unique(tolower(c(PI$prefix[PI$host %in% new_inv], repo_prefixes(new_inv)))), other))

# ---- CKAN ----
# every CKAN host that passes the check (GET /api/3/action/status_show names
# ckan_version)
HC <- read.csv(paste0(d, "ckan_hosts.csv"), colClasses = "character"); PC <- read.csv(paste0(d, "ckan_host_prefixes.csv"), colClasses = "character")
ck_hosts <- sort(HC$host[HC$api %in% "ckan"])
pre_ck <- num_order(setdiff(unique(tolower(c(PC$prefix[PC$host %in% ck_hosts], repo_prefixes(ck_hosts)))), other))

# ---- the file ----
vec <- function(x, per_line) {
  q <- paste0('"', x, '"')
  lines <- split(q, ceiling(seq_along(q) / per_line))
  paste0("    ", vapply(lines, paste, "", collapse = ", "), collapse = ",\n")
}
fn <- function(name, x, per_line) c(sprintf("%s <- function() {", name),
  if (length(x)) c("  c(", vec(x, per_line), "  )") else "  character(0)", "}")
day <- function(f) format(file.mtime(paste0(d, f)), "%Y-%m-%d")
txt <- c(
  "# Generated by data-raw/repo_tables.R from data-raw/repo_tables/ -- do not",
  "# edit by hand; change the survey tables and run the script again.",
  "#",
  "# Repository installations found by the repository survey (#635, #636):",
  "# every host the survey of DataCite's repositories and Crossref's dataset",
  "# publishers found running the program, and every host metacheck already",
  "# listed, checked the way metacheck uses them (data-raw/repo_survey/repo_hosts.R).",
  "",
  "# ---- DSpace ----",
  sprintf("# Survey of %s: %d hosts; %d new DSpace 7+ hosts (GET /server/api answers",
          day("dspace_hosts.csv"), nrow(H), length(new7)),
  sprintf("# with its HAL root) and %d new DSpace 6 hosts (GET /rest/test answers", length(new_legacy)),
  "# \"REST api is running.\"), not in .dspace7_hosts()'s and",
  "# .dspace_legacy_hosts()'s hand-written lists.",
  fn(".dspace7_hosts_survey", new7, 2), "",
  fn(".dspace_legacy_hosts_survey", new_legacy, 2), "",
  "# DOI prefixes with DOIs registered on a DSpace 7+ host (DataCite's count per",
  "# host: api.datacite.org/dois?query=url:*<host>* with disable-facets=false),",
  "# or of a repository whose DOIs land on one; never another detector's",
  "# platform's (OSF, Zenodo, Dryad, Figshare, ...). dspace7_links() looks up",
  "# online only DOIs under these prefixes; a looked-up DOI still has to lead to",
  sprintf("# a host in .dspace7_hosts(). %d prefixes.", length(pre7)),
  fn(".dspace7_doi_prefixes", pre7, 6), "",
  "# ---- Dataverse ----",
  sprintf("# Survey of %s: %d hosts; %d new hosts that answer GET /api/info/version",
          day("dataverse_hosts.csv"), nrow(HV), length(new_dv)),
  "# with a version, not in .dataverse_hosts()'s hand-written list.",
  fn(".dataverse_hosts_survey", new_dv, 2), "",
  "# Host -> DOI prefixes, added to .dataverse_doi_prefix_hosts(): the prefixes",
  "# DataCite counts on a Dataverse host, and those of the repositories whose",
  "# DOIs land on it, that the hand-written table does not give that host;",
  "# never another detector's platform's prefix. A prefix shared by several",
  "# hosts is resolved by the DOI's registered address (.dataverse_host_from_doi()).",
  ".dataverse_doi_prefix_hosts_survey <- function() {",
  if (length(dv_map)) c("  list(", paste0("    \"", names(dv_map), "\" = c(",
    vapply(dv_map, \(p) paste0('"', p, '"', collapse = ", "), ""), ")", c(rep(",", length(dv_map) - 1), "")), "  )") else "  list()",
  "}",
  "",
  "# ---- EPrints ----",
  sprintf("# Survey of %s: %d EPrints hosts (GET /cgi/oai2?verb=Identify names EPrints);",
          day("eprints_hosts.csv"), nrow(HE)),
  "# all of them, data and publication repositories alike: repo_check tells a",
  "# publication from a deposit per record (R/repo-kind.R). Those not in",
  "# .eprints_hosts()'s hand-written list:",
  fn(".eprints_hosts_survey", new_ep, 2), "",
  "# DOI prefixes of those hosts (DataCite's count per host, or a repository",
  "# whose DOIs land on one), less the other detectors' platforms' and",
  "# ReShare's own 10.5255. eprints_links() looks up online only DOIs under",
  "# these prefixes, and drops one DataCite calls a publication; a looked-up",
  "# DOI still has to lead to a host in .eprints_hosts().",
  fn(".eprints_doi_prefixes", pre_ep, 6), "",
  "# ---- Invenio ----",
  sprintf("# Survey of %s: %d Invenio hosts (GET /api/records?size=1 answers with search",
          day("invenio_hosts.csv"), nrow(HI)),
  "# hits), less test and sandbox installations. Those not in .invenio_hosts()'s",
  "# hand-written list (Zenodo, Hamburg):",
  fn(".invenio_hosts_survey", new_inv, 2), "",
  "# DOI prefixes of those hosts (DataCite's count per host, or a repository",
  "# whose DOIs land on one), less the other detectors' platforms' (Zenodo's",
  "# 10.5281 among them). invenio_links() looks up online only DOIs under these",
  "# prefixes, and drops one DataCite calls a publication.",
  fn(".invenio_doi_prefixes", pre_inv, 6), "",
  "# ---- CKAN ----",
  sprintf("# Survey of %s: %d CKAN hosts, of which %d answer GET /api/3/action/status_show",
          day("ckan_hosts.csv"), nrow(HC), length(ck_hosts)),
  "# with their ckan_version:",
  fn(".ckan_hosts_survey", ck_hosts, 2), "",
  "# DOI prefixes of those hosts (DataCite's count per host, or a repository",
  "# whose DOIs land on one), less the other detectors' platforms'.",
  "# ckan_links() looks up online only DOIs under these prefixes.",
  fn(".ckan_doi_prefixes", pre_ck, 6)
)
writeLines(txt, out)
cat("DSpace: new 7+ hosts", length(new7), "| new 6 hosts", length(new_legacy), "| prefixes", length(pre7), "\n")
cat("CKAN: hosts", length(ck_hosts), "| prefixes", length(pre_ck), "\n")
cat("Invenio: hosts", length(inv_hosts), "| new", length(new_inv), "| prefixes", length(pre_inv), "\n")
cat("EPrints: hosts", length(ep_hosts), "| new", length(new_ep), "| prefixes", length(pre_ep), "\n")
cat("Dataverse: new hosts", length(new_dv), "| hosts given prefixes", length(dv_map), "| prefixes added",
    length(unlist(dv_map)), "| of them new to the table", length(setdiff(unlist(dv_map), unlist(dv_hand))), "\n")
