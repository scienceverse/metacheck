# Benchmark of ref_accuracy on CiteTracer's synthetic citations
#
# CiteTracer (https://github.com/aaFrostnova/CiteTracer, MIT licence) has
# 2,450 machine-learning citations built from real BibTeX entries: 1,023 real
# (R1 exact, R2 reformatted, R3 truncated with "et al."), 271 borderline (P1
# author name variants, P3 fields no source has) and 1,156 with one field
# fabricated or changed by an LLM (H1 title, H2 authors, H3 venue, H4 year,
# H5 DOI, H6 pages, volume or publisher). Each citation becomes a reference
# in a paper of 50 (shuffled across subtypes), is looked up with
# add_bib_match(), and ref_accuracy's flags (incoherent) and not-found notes
# are counted separately. CiteTracer has no entry types, so every citation is
# an article. Network calls only (Crossref, DataCite, Semantic Scholar,
# doi.org); about an hour without a Semantic Scholar key. The looked-up papers
# are saved to `cache` with the date of the lookup, so a rerun after a change
# to the module repeats only the module's own calls (doi.org, Crossref dates,
# title searches), whose answers may change. A failed title search counts as
# "search failed", not as a miss.

devtools::load_all(".")

# the last commit that changed the v2 data (9 May 2026)
sha <- "5dfe2dd71c5c9d9a946653bbabb34ee0e61e6efc"
url <- sprintf("https://raw.githubusercontent.com/aaFrostnova/CiteTracer/%s/data/synthetic_data/v2/", sha)
cache <- file.path(tools::R_user_dir("metacheck", "cache"),
                   sprintf("citetracer_%s_bib_match.rds", substr(sha, 1, 8)))
cites <- jsonlite::fromJSON(paste0(url, "_all_test.json"), simplifyVector = FALSE)
meta <- jsonlite::fromJSON(paste0(url, "meta.json"), simplifyVector = FALSE)

one <- \(x) if (is.null(x) || !length(x) || identical(x, "")) NA_character_ else as.character(x)
# "Ziming Li" as "Li, Ziming"; "others" (BibTeX) and "et al." as printed
person <- \(a) {
  if (tolower(a) %in% c("others", "et al.", "et al")) return("et al.")
  w <- strsplit(trimws(a), "\\s+")[[1]]
  if (length(w) < 2) a else paste0(w[length(w)], ", ", paste(w[-length(w)], collapse = " "))
}
refs <- lapply(cites, \(x) {
  au <- unlist(x$authors)
  data.frame(
    citation_id = x$citation_id,
    label = meta[[x$citation_id]]$label,
    subtype = meta[[x$citation_id]]$subtype,
    authors = if (length(au)) paste(vapply(au, person, ""), collapse = "; ") else NA_character_,
    authors_text = paste(au, collapse = ", "),
    title = one(x$title), year = suppressWarnings(as.integer(one(x$year))),
    container = one(x$venue), doi = one(x$doi), arxiv = one(x$arxiv_id),
    volume = one(x$volume))
}) |> dplyr::bind_rows()
refs$authors_text <- sub(", (others|et al\\.)$", " et al.", refs$authors_text)
refs$text <- with(refs, paste0(
  authors_text, ". ", title, ". ",
  ifelse(is.na(container), "", paste0(container, ", ")), year, ".",
  ifelse(is.na(arxiv), "", paste0(" arXiv:", arxiv, ".")),
  ifelse(is.na(doi), "", paste0(" https://doi.org/", doi))))

set.seed(1)
refs <- refs[sample(nrow(refs)), ]
refs$paper_id <- sprintf("ct%02d", (seq_len(nrow(refs)) - 1) %/% 50)
refs$bib_id <- ave(seq_len(nrow(refs)), refs$paper_id, FUN = seq_along)

template <- demopaper()
papers <- lapply(split(refs, refs$paper_id), \(r) {
  p <- template
  p$paper_id <- r$paper_id[1]
  n <- nrow(r)
  p$bib <- data.frame(
    bib_type = "article", doi = r$doi, title = r$title, authors = r$authors,
    editors = "", publisher = "", year = r$year, volume = r$volume, issue = "",
    first_page = NA_character_, last_page = NA_character_,
    container = r$container, bib_id = r$bib_id, year_suffix = "",
    text_id = 1000L + seq_len(n))
  p$text <- data.frame(text = r$text, text_id = 1000L + seq_len(n),
                       paragraph_id = 1L, section_id = 1L,
                       page_number = NA_integer_, formatted = NA_character_)
  p$xref <- p$xref[0, ]
  p$bib_match <- NULL
  p
}) |> paperlist()

if (file.exists(cache)) {
  papers <- readRDS(cache)
} else {
  papers <- add_bib_match(papers)
  attr(papers, "looked_up") <- Sys.Date()
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(papers, cache)
}
mo <- module_run(papers, "ref_accuracy")

res <- merge(refs[, c("paper_id", "bib_id", "label", "subtype")], mo$table,
             by = c("paper_id", "bib_id"))
res$outcome <- ifelse(res$incoherent, "flagged",
                      ifelse(res$not_found, "not found",
                             ifelse(res$search_failed, "search failed", "neither")))
cat("Looked up", format(attr(papers, "looked_up") %||% NA), "; module run",
    format(Sys.Date()), "; Semantic Scholar key", nzchar(Sys.getenv("SEMANTIC_SCHOLAR_API_KEY")), "\n")
table(res$label, res$outcome) |> prop.table(1) |> round(3)
table(res$subtype, res$outcome)
