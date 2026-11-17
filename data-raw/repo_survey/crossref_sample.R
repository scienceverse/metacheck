# One Crossref dataset (or database) DOI per publisher, with its registered
# address, in the same form as dc_sample.json, for repo_probe.R. Saved per
# 100 publishers.
Sys.unsetenv(c("GITHUB_PAT", "GITHUB_TOKEN", "OSF_PAT"))
sp <- "data-raw/repo_survey/"
bd <- paste0(sp, "cr_sample/"); dir.create(bd, showWarnings = FALSE)
gj <- function(u) tryCatch(jsonlite::fromJSON(u, simplifyVector = FALSE), error = \(e) NULL)
pubs <- list()
for (ty in c("dataset", "database")) {
  j <- gj(sprintf("https://api.crossref.org/works?filter=type:%s&rows=0&facet=publisher-name:*", ty))
  v <- j$message$facets$`publisher-name`$values
  pubs[[ty]] <- data.frame(type = ty, publisher = names(v), n = unlist(v))
}
pubs <- do.call(rbind, pubs)
pubs <- pubs[order(-pubs$n), ]; pubs <- pubs[!duplicated(pubs$publisher), ]
saveRDS(pubs, paste0(sp, "cr_publishers.rds"))
cat("publishers:", nrow(pubs), " DOIs:", sum(pubs$n), "\n")
b <- split(seq_len(nrow(pubs)), ceiling(seq_len(nrow(pubs)) / 100))
for (k in seq_along(b)) {
  f <- sprintf("%sc%03d.rds", bd, k)
  if (file.exists(f)) next
  out <- lapply(b[[k]], \(i) {
    u <- sprintf("https://api.crossref.org/works?filter=type:%s&query.publisher-name=%s&rows=5&select=DOI,URL,publisher,member,resource",
                 pubs$type[[i]], utils::URLencode(pubs$publisher[[i]], reserved = TRUE))
    it <- gj(u)$message$items
    it <- Filter(\(x) identical(x$publisher, pubs$publisher[[i]]), it)
    if (!length(it)) return(NULL)
    x <- it[[1]]
    data.frame(publisher = pubs$publisher[[i]], n = pubs$n[[i]], type = pubs$type[[i]],
               member = as.character(x$member %||% NA), doi = x$DOI,
               url = x$resource$primary$URL %||% x$URL)
  })
  saveRDS(do.call(rbind, out), f)
  cat("batch", k, "of", length(b), "\n")
}
s <- do.call(rbind, lapply(list.files(bd, full.names = TRUE), readRDS))
saveRDS(s, paste0(sp, "cr_sample.rds"))
cat("sampled:", nrow(s), "\n")
