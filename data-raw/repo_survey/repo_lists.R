# Markdown tables of the repositories per software, from the DataCite probe
# (and the Crossref probe), for the issues. One table per software.
sp <- "data-raw/repo_survey/"
suppressMessages(pkgload::load_all('.', quiet = TRUE))
x <- readRDS(paste0(sp, "repo_probe_sum.rds"))$sample
r <- readRDS(paste0(sp, "dc_repos.rds"))
x$prefixes <- r$prefixes[match(x$repo, r$id)]
x$dc_field <- tolower(x$software_dc)
x$host <- sub("^www\\.", "", x$host)

known <- list(
  dspace = sub("^www\\.", "", tolower(c(.dspace7_hosts(), .dspace_legacy_hosts()))),
  dataverse = sub("^www\\.", "", tolower(unique(c(.dataverse_hosts(), unlist(.dataverse_doi_prefix_hosts()))))),
  invenio = c("zenodo.org", "sandbox.zenodo.org"),
  ckan = character(0), eprints = character(0))
esc <- function(v) gsub("\\|", "/", ifelse(is.na(v), "", v))

table_for <- function(sw) {
  k <- x$found == sw | x$dc_field %in% sw
  y <- x[k, ]
  y <- y[order(y$found != sw, y$name), ]
  how <- ifelse(y$found == sw & y$dc_field %in% sw, "both", ifelse(y$found == sw, "probe", "DataCite field only"))
  ver <- if (sw == "dspace") ifelse(!is.na(y$dspace7_version), y$dspace7_version, ifelse(grepl("dspace", tolower(y$generator)), y$generator, "")) else ""
  new <- ifelse(y$host %in% known[[sw]], "yes", "")
  hdr <- paste0("| DataCite id | Repository | Host (landing page of a sample DOI) | DOI prefixes | Found by | ",
                if (sw == "dspace") "Version | " else "", "Already in metacheck |\n|---|---|---|---|---|",
                if (sw == "dspace") "---|" else "", "---|\n")
  rows <- paste0("| ", esc(y$repo), " | ", esc(substr(y$name, 1, 70)), " | ", esc(y$host), " | ", esc(y$prefixes), " | ", how,
                 if (sw == "dspace") paste0(" | ", esc(ver)) else "", " | ", new, " |", collapse = "\n")
  list(md = paste0(hdr, rows, "\n"), n = nrow(y), n_probe = sum(y$found == sw), n_new = sum(new == "" & y$found == sw),
       prefixes = unique(unlist(strsplit(na.omit(y$prefixes), " "))))
}
out <- lapply(c("dspace", "dataverse", "invenio", "ckan", "eprints"), table_for)
names(out) <- c("dspace", "dataverse", "invenio", "ckan", "eprints")
for (n in names(out)) {
  writeLines(out[[n]]$md, sprintf("%srepo_table_%s.md", sp, n))
  cat(n, ": rows", out[[n]]$n, " probe", out[[n]]$n_probe, " probe-found not yet in metacheck", out[[n]]$n_new,
      " prefixes", length(out[[n]]$prefixes), " chars", nchar(out[[n]]$md), "\n")
}
writeLines(paste(sort(out$dspace$prefixes), collapse = " "), paste0(sp, "repo_dspace_prefixes.txt"))
# Crossref publishers per software
cr <- readRDS(paste0(sp, "cr_sample.rds")); pc <- do.call(rbind, lapply(list.files(paste0(sp, "probe_cr"), full.names = TRUE), readRDS))
cr$host0 <- tolower(sub("^https?://([^/:?#]+).*", "\\1", cr$url)); cr <- merge(cr, pc, by = "host0", all.x = TRUE)
g <- tolower(cr$generator)
cr$found <- dplyr::case_when(cr$dspace7 | grepl("dspace", g) | cr$dspace_legacy ~ "dspace", cr$dataverse | grepl("dataverse", g) ~ "dataverse",
  cr$invenio | grepl("invenio", g) ~ "invenio", cr$ckan | grepl("ckan", g) ~ "ckan", grepl("eprints", g) ~ "eprints", TRUE ~ "")
for (sw in c("dspace", "dataverse", "invenio", "ckan", "eprints")) {
  y <- cr[cr$found == sw, ]
  md <- paste0("| Crossref publisher | Dataset DOIs | Host | Sample DOI |\n|---|---|---|---|\n",
               paste0("| ", esc(y$publisher), " | ", y$n, " | ", esc(y$host), " | ", esc(y$doi), " |", collapse = "\n"), "\n")
  writeLines(md, sprintf("%srepo_table_cr_%s.md", sp, sw))
  cat("crossref", sw, nrow(y), "\n")
}
