# Crossref dataset publishers: which software do their landing pages run?
sp <- "data-raw/repo_survey/"
p <- do.call(rbind, lapply(list.files(paste0(sp, "probe_cr"), full.names = TRUE), readRDS))
s <- readRDS(paste0(sp, "cr_sample.rds"))
s$host0 <- tolower(sub("^https?://([^/:?#]+).*", "\\1", s$url))
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
  grepl("wordpress|drupal|islandora|samvera|hyrax|omeka|mycore", g) ~ sub(" .*", "", g),
  s$landing_status %in% 200 ~ "other or unknown",
  TRUE ~ "no answer")
cat("publishers sampled:", nrow(s), " dataset/database DOIs they hold:", sum(s$n), "\n\n")
t <- aggregate(n ~ found, s, \(x) c(publishers = length(x), dois = sum(x)))
t <- data.frame(found = t$found, publishers = t$n[, 1], dois = t$n[, 2]); print(t[order(-t$publishers), ], row.names = FALSE)
cat("\nlargest publishers and where their landing pages are:\n")
top <- s[order(-s$n), ][1:25, ]
print(data.frame(publisher = substr(top$publisher, 1, 45), dois = top$n, host = top$host, found = top$found), row.names = FALSE)
cat("\nDSpace publishers:\n"); print(s[s$found == "dspace", c("publisher", "n", "host", "doi")], row.names = FALSE)
