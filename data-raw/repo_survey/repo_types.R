# What kind of records each host of one program holds, by DataCite's count of
# its DOIs per resource type (url:*<host>*, disable-facets=false; one request
# per host, at most one a second). A host most of whose DOIs are datasets is a
# data repository; one that mainly holds papers and theses is not (#637).
# Usage, after repo_hosts.R: Rscript data-raw/repo_survey/repo_types.R eprints
# Writes repo_tables/<program>_types.csv; working files in <program>_types/.
Sys.unsetenv(c("GITHUB_PAT", "GITHUB_TOKEN", "OSF_PAT"))
suppressMessages(library(httr2))
sw <- commandArgs(TRUE)[1]
sp <- "data-raw/repo_survey/"; out <- "data-raw/repo_tables/"
wd <- paste0(sp, sw, "_types/"); dir.create(wd, showWarnings = FALSE)
ua <- "metacheck repository survey (https://github.com/scienceverse/metacheck)"
H <- read.csv(paste0(out, sw, "_hosts.csv"), colClasses = "character")
hosts <- H$host[H$api != "none"]
# a phrase search for a hyphenated host, as in repo_hosts.R
dc_url <- function(host) paste0("https://api.datacite.org/dois?query=",
  utils::URLencode(if (grepl("-", host)) sprintf('url:"%s"', host) else sprintf("url:*%s*", host), reserved = TRUE),
  "&page[size]=1&fields[dois]=url&disable-facets=false")
for (host in hosts) {
  f <- paste0(wd, gsub("[^a-z0-9.-]", "_", host), ".rds")
  # a hyphenated host asked before the phrase search was used is asked again
  if (file.exists(f) && !(grepl("-", host) && !isTRUE(readRDS(f)$phrase[1]))) next
  Sys.sleep(1)
  u <- dc_url(host)
  r <- tryCatch(request(u) |> req_user_agent(ua) |> req_timeout(60) |> req_retry(max_tries = 3) |>
                  req_error(is_error = \(r) FALSE) |> req_perform(), error = \(e) NULL)
  j <- if (!is.null(r) && resp_status(r) == 200) tryCatch(resp_body_json(r, simplifyVector = TRUE), error = \(e) NULL)
  rt <- j$meta$resourceTypes
  n_total <- j$meta$total %||% NA
  n_dataset <- if (is.data.frame(rt) && nrow(rt)) sum(rt$count[tolower(rt$id) == "dataset"]) else 0
  top <- if (is.data.frame(rt) && nrow(rt)) paste0(rt$id, ":", rt$count, collapse = " ") else NA
  saveRDS(data.frame(host = host, n_total = n_total, n_dataset = n_dataset, types = top, datacite_ok = !is.null(j),
                     phrase = grepl("-", host)), f)
  cat(host, n_dataset, "of", n_total, "\n")
}
x <- dplyr::bind_rows(lapply(list.files(wd, full.names = TRUE), readRDS))
x$share_dataset <- ifelse(x$n_total > 0, x$n_dataset / x$n_total, NA)
write.csv(x[order(-x$share_dataset, x$host), ], paste0(out, sw, "_types.csv"), row.names = FALSE)
cat("\nhosts:", nrow(x), "| mostly datasets:", sum(x$share_dataset > 0.5, na.rm = TRUE),
    "| no DataCite DOIs:", sum(x$n_total %in% 0), "| DataCite failed:", sum(!x$datacite_ok), "\n")
