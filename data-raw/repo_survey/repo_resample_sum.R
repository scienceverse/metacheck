# Every host of every repository in the resample (repo_resample.R), with the
# program the probe found on it (datacite_probe.csv for hosts probed first,
# probe_rs/ for the new ones), written to data-raw/repo_tables/datacite_hosts.csv.
sp <- "data-raw/repo_survey/"
d <- readRDS(paste0(sp, "dc_resample.rds"))
p1 <- read.csv(paste0(sp, "datacite_probe.csv"))
p2 <- do.call(rbind, lapply(list.files(paste0(sp, "probe_rs"), full.names = TRUE), readRDS))
rp <- read.csv(paste0(sp, "datacite_repositories.csv"))
# the same rule as repo_probe_sum.R
found_of <- function(s) {
  g <- tolower(s$generator)
  dplyr::case_when(
    s$dspace7 | grepl("dspace", g) | s$dspace_legacy ~ "dspace",
    s$dataverse | grepl("dataverse", g) ~ "dataverse",
    s$invenio | grepl("invenio", g) ~ "invenio",
    s$ckan | grepl("ckan", g) ~ "ckan",
    grepl("eprints", g) ~ "eprints",
    grepl("open journal systems|ojs", g) ~ "ojs",
    grepl("figshare", s$host) ~ "figshare",
    s$landing_status %in% 200 ~ "other or unknown",
    TRUE ~ "no answer")
}
cols <- c("host0", "host", "generator", "dspace7", "dspace7_version", "dspace_legacy", "dataverse", "invenio", "ckan", "landing_status")
p1h <- p1[!duplicated(tolower(p1$host0)), cols]; p1h$host0 <- tolower(p1h$host0)
p2h <- p2[, cols]
ph <- rbind(p1h, p2h[!p2h$host0 %in% p1h$host0, ])
ph$found <- found_of(ph)
ph$probed <- ifelse(ph$host0 %in% p1h$host0, "first", "resample")
h <- aggregate(doi ~ repo + host0, d, length); names(h)[3] <- "n_dois"
h <- merge(h, ph, by = "host0", all.x = TRUE)
h$host <- sub("^www\\.", "", tolower(ifelse(is.na(h$host), h$host0, h$host)))
h$name <- rp$name[match(h$repo, rp$id)]
h$software_dc <- rp$software[match(h$repo, rp$id)]
h$prefixes <- rp$prefixes[match(h$repo, rp$id)]
h <- h[order(h$repo, -h$n_dois), c("repo", "name", "software_dc", "prefixes", "host0", "host", "n_dois", "found",
                                    "dspace7_version", "generator", "probed")]
dir.create("data-raw/repo_tables", showWarnings = FALSE)
write.csv(h, "data-raw/repo_tables/datacite_hosts.csv", row.names = FALSE)
cat("repository-host pairs:", nrow(h), " repositories:", length(unique(h$repo)), " hosts:", length(unique(h$host)), "\n")
cat("\nprogram per host (distinct hosts):\n"); print(table(h$found[!duplicated(h$host)], h$probed[!duplicated(h$host)]))
cat("\nthe 4 DSpace prefixes the first probe missed:\n")
for (pf in c("10.57776", "10.34621", "10.15157", "10.23673")) {
  k <- grepl(paste0("(^| )", pf, "( |$)"), h$prefixes)
  cat(pf, ":", if (any(k)) paste(unique(paste0(h$host[k], " [", h$found[k], "]")), collapse = ", ") else "no repository", "\n")
}
