# Up to 10 DOIs per repository, for the repositories that run DSpace,
# Dataverse, Invenio, CKAN or EPrints by the probe or by DataCite's own
# `software` field (#636). The one-request sample of all repositories gives one
# DOI per repository whatever sample size is asked for, so a repository whose
# DOIs lead to several hosts showed only one of them. One request per
# repository, at most one a second; saved per batch of 50 repositories, and a
# restart skips saved batches. Then: Rscript repo_probe.R resample
sp <- "data-raw/repo_survey/"
Sys.unsetenv(c("GITHUB_PAT", "GITHUB_TOKEN", "OSF_PAT"))
suppressMessages(library(httr2))
sws <- c("dspace", "dataverse", "invenio", "ckan", "eprints")
pr <- read.csv(paste0(sp, "datacite_probe.csv"))
rp <- read.csv(paste0(sp, "datacite_repositories.csv"))
ids <- sort(unique(c(pr$repo[pr$found %in% sws | tolower(pr$software_dc) %in% sws],
                     rp$id[tolower(rp$software) %in% sws])))
cat("repositories:", length(ids), "\n")
bd <- paste0(sp, "resample/"); dir.create(bd, showWarnings = FALSE)
ua <- "metacheck repository survey (https://github.com/scienceverse/metacheck)"
batches <- split(ids, ceiling(seq_along(ids) / 50))
for (b in seq_along(batches)) {
  f <- sprintf("%sb%03d.rds", bd, b)
  if (file.exists(f)) next
  rows <- lapply(batches[[b]], \(id) {
    Sys.sleep(1)
    u <- sprintf("https://api.datacite.org/dois?client-id=%s&sample-size=10&fields[dois]=url", id)
    r <- tryCatch(request(u) |> req_user_agent(ua) |> req_timeout(60) |>
                    req_retry(max_tries = 3) |> req_error(is_error = \(r) FALSE) |> req_perform(),
                  error = \(e) NULL)
    if (is.null(r) || resp_status(r) != 200) return(data.frame(repo = id, doi = NA, url = NA, status = if (is.null(r)) NA else resp_status(r)))
    j <- tryCatch(resp_body_json(r, simplifyVector = TRUE), error = \(e) NULL)
    if (is.null(j) || !length(j$data$id)) return(data.frame(repo = id, doi = NA, url = NA, status = 200))
    data.frame(repo = id, doi = j$data$id, url = j$data$attributes$url %||% NA, status = 200)
  })
  d <- do.call(rbind, rows)
  saveRDS(d, f)
  cat("batch", b, "of", length(batches), ":", sum(!is.na(d$doi)), "DOIs\n")
}
d <- do.call(rbind, lapply(list.files(bd, full.names = TRUE), readRDS))
d <- d[!is.na(d$url) & grepl("^https?://", d$url), ]
d$host0 <- tolower(sub("^https?://([^/:?#]+).*", "\\1", d$url))
saveRDS(d, paste0(sp, "dc_resample.rds"))
cat("DOIs:", nrow(d), " repositories with DOIs:", length(unique(d$repo)),
    " distinct hosts:", length(unique(d$host0)), "\n")
