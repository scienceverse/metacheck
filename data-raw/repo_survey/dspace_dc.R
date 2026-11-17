# How complete is DataCite's "software = DSpace" list, and how could it be
# completed? Probes only; results printed.
Sys.unsetenv(c("GITHUB_PAT", "GITHUB_TOKEN", "OSF_PAT"))
suppressMessages(pkgload::load_all('.', quiet = TRUE))
sp <- "data-raw/repo_survey/"
gj <- function(u) jsonlite::fromJSON(u, simplifyVector = FALSE)
host <- function(u) sub("^www\\.", "", tolower(vapply(u, \(x) tryCatch(httr2::url_parse(x)$hostname %||% NA_character_, error = \(e) NA_character_), "")))

# 1. every DataCite repository (paged), with software, url and prefixes
f <- paste0(sp, "dc_repos.rds")
if (!file.exists(f)) {
  out <- list(); u <- "https://api.datacite.org/repositories?page%5Bsize%5D=1000&page%5Bnumber%5D=1"
  for (pg in 1:10) {
    j <- gj(sprintf("https://api.datacite.org/repositories?page%%5Bsize%%5D=1000&page%%5Bnumber%%5D=%d", pg))
    if (!length(j$data)) break
    out <- c(out, lapply(j$data, \(x) data.frame(id = x$id, name = x$attributes$name %||% NA,
      software = x$attributes$software %||% NA, url = x$attributes$url %||% NA,
      type = x$attributes$clientType %||% NA, active = isTRUE(x$attributes$isActive),
      re3data = x$attributes$re3data %||% NA, opendoar = x$attributes$opendoar %||% NA,
      prefixes = paste(vapply(x$relationships$prefixes$data, \(p) p$id, ""), collapse = " "))))
  }
  saveRDS(dplyr::bind_rows(out), f)
}
r <- readRDS(f)
r$host <- host(r$url)
cat("repositories:", nrow(r), " active:", sum(r$active), "\n")
cat("software listed:", sum(!is.na(r$software) & nzchar(r$software)), "\n")
print(sort(table(tolower(r$software)), decreasing = TRUE)[1:25])
cat("with re3data id:", sum(!is.na(r$re3data)), " with opendoar id:", sum(!is.na(r$opendoar)), "\n")

# 2. our DSpace hosts against DataCite's DSpace list
ours <- sub("^www\\.", "", c(.dspace7_hosts(), if (exists(".dspace_legacy_hosts")) .dspace_legacy_hosts()))
ds <- r[tolower(r$software) %in% "dspace", ]
cat("\nDataCite DSpace repositories:", nrow(ds), " prefixes:", length(unlist(strsplit(ds$prefixes, " "))), "\n")
cat("our hosts:", length(ours), " of which on a DataCite DSpace repository's url:", sum(ours %in% ds$host),
    " on any DataCite repository's url:", sum(ours %in% r$host), "\n")
cat("our hosts on a DataCite repository whose software is NOT DSpace:\n")
print(r[r$host %in% ours & !tolower(r$software) %in% "dspace", c("host", "software")])

# 3. a repository's own landing-page address that looks like DSpace
sig <- "/handle/[0-9.]+/|/items/[0-9a-f]{8}-|/xmlui/|/jspui/|/entities/"
cat("\nrepository url with a DSpace-shaped path:", sum(grepl(sig, r$url)), " of which software not DSpace:",
    sum(grepl(sig, r$url) & !tolower(r$software) %in% "dspace"), "\n")

# 4. DOIs whose registered address has a DSpace-shaped path: which repositories?
j <- gj("https://api.datacite.org/dois?query=url:*%5C/handle%5C/*&page%5Bsize%5D=0")
cat("\nDOIs with /handle/ in their address:", j$meta$total, "\n")
print(vapply(j$meta$clients %||% list(), \(x) paste(x$id, x$count), ""))
