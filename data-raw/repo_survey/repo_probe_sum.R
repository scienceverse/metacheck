# Which software runs each DataCite repository: the probe's answers combined
# with DataCite's own "software" field; and the resulting DSpace prefix list.
sp <- "data-raw/repo_survey/"
suppressMessages(pkgload::load_all('.', quiet = TRUE))
p <- do.call(rbind, lapply(list.files(paste0(sp, "probe"), full.names = TRUE), readRDS))
j <- jsonlite::fromJSON(paste0(sp, "dc_sample.json"))
s <- data.frame(doi = j$data$id, url = j$data$attributes$url)
s$prefix <- tolower(sub("/.*", "", s$doi))
s$host0 <- tolower(sub("^https?://([^/:?#]+).*", "\\1", s$url))
r <- readRDS(paste0(sp, "dc_repos.rds"))
pre <- do.call(rbind, lapply(seq_len(nrow(r)), \(i) {
  x <- strsplit(r$prefixes[[i]], " ")[[1]]
  if (length(x)) data.frame(prefix = tolower(x), repo = r$id[[i]], software_dc = r$software[[i]], name = r$name[[i]])
}))
s <- merge(s, pre[!duplicated(pre$prefix), ], by = "prefix", all.x = TRUE)
s <- merge(s, p, by = "host0", all.x = TRUE)
g <- tolower(s$generator)
s$found <- dplyr::case_when(
  s$dspace7 | grepl("dspace", g) | s$dspace_legacy ~ "dspace",
  s$dataverse | grepl("dataverse", g) ~ "dataverse",
  s$invenio | grepl("invenio", g) ~ "invenio",
  s$ckan | grepl("ckan", g) ~ "ckan",
  grepl("eprints", g) ~ "eprints",
  grepl("open journal systems|ojs", g) ~ "ojs",
  grepl("figshare", s$host) ~ "figshare",
  grepl("islandora|samvera|hyrax|drupal|wordpress|omeka|mycore|opus|pure", g) ~ sub(" .*", "", g),
  s$landing_status %in% 200 ~ "other or unknown",
  TRUE ~ "no answer")
cat("repositories with a DOI sampled:", nrow(s), "\n\n")
cat("software found by the probe:\n"); print(sort(table(s$found), decreasing = TRUE))
dc <- tolower(s$software_dc); dc[is.na(dc) | !nzchar(dc)] <- "(none)"
cat("\nDSpace by DataCite's field vs by the probe:\n")
print(table(datacite_says_dspace = dc == "dspace", probe_says_dspace = s$found == "dspace"))
cat("\nDataverse by DataCite's field vs by the probe:\n")
print(table(datacite_says = dc == "dataverse", probe_says = s$found == "dataverse"))
cat("\nInvenio by DataCite's field vs by the probe:\n")
print(table(datacite_says = dc == "invenio", probe_says = s$found == "invenio"))

# DSpace prefixes: every prefix of a repository DataCite or the probe calls DSpace
ds_repo <- unique(s$repo[s$found == "dspace" | dc == "dspace"])
ds_repo <- union(ds_repo, r$id[tolower(r$software) %in% "dspace"])
ds_pre <- unique(pre$prefix[pre$repo %in% ds_repo])
cat("\nDSpace repositories:", length(ds_repo), " their prefixes:", length(ds_pre), "\n")
known <- unique(tolower(sub("^www\\.", "", c(.dspace7_hosts(), .dspace_legacy_hosts()))))
dsh <- unique(sub("^www\\.", "", na.omit(s$host[s$repo %in% ds_repo])))
cat("our known DSpace hosts:", length(known), " DSpace hosts found now:", length(dsh),
    " of which new:", length(setdiff(dsh, known)), "\n")
cat("DSpace versions (API):\n"); print(sort(table(sub("^DSpace ([0-9]+).*", "\\1", na.omit(s$dspace7_version))), decreasing = TRUE))
# the test DOIs
for (d in c("10.13020", "10.35090", "10.23668", "10.17863", "10.5167"))
  cat(d, if (d %in% ds_pre) "in the DSpace prefix list" else "MISSING", "\n")
saveRDS(list(sample = s, dspace_prefixes = ds_pre, dspace_repos = ds_repo), paste0(sp, "repo_probe_sum.rds"))
