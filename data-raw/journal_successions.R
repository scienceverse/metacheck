# Former and later names of journals
#
# Crossref's records of older articles may give the journal's current name,
# when the publisher redeposits its back issues, so a reference that gives
# the name the journal had at the time differs from its record (in Crossref,
# a 1972 article in the Journal of the Royal Statistical Society, Series B
# (Methodological) is in its successor, Series B: Statistical Methodology).
# ref_accuracy accepts a cited journal name that Wikidata gives for the
# record's journal: one of its English names (its label and aliases, as a
# journal renamed under the same ISSN, such as ILR Review, keeps its item),
# or a name of a journal linked to it as predecessor or successor (P155
# "follows", P156 "followed by", P1365 "replaces", P1366 "replaced by"; a
# new name often has a new ISSN and item). This table holds the English
# names of the items with an ISSN that have an alias or such a link, and the
# links. Only direct links are used: a chain of links can join journals that
# split ("Philosophical Transactions" was followed by its series A and B).
# Built from Wikidata (CC0), queried through QLever's copy of Wikidata,
# which follows Wikidata's edits.

sparql <- function(query) {
  resp <- httr2::request("https://qlever.dev/api/wikidata") |>
    httr2::req_url_query(query = query) |>
    httr2::req_headers(Accept = "text/tab-separated-values") |>
    httr2::req_retry(max_tries = 6, backoff = \(i) 30 * i) |>
    httr2::req_perform()
  utils::read.delim(text = httr2::resp_body_string(resp), quote = "",
                    colClasses = "character", check.names = FALSE)
}
prefixes <- paste(
  "PREFIX wdt: <http://www.wikidata.org/prop/direct/>",
  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
  "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>")
qid <- \(x) sub("^<http://www.wikidata.org/entity/(Q\\d+)>$", "\\1", x)
literal <- \(x) gsub('\\\\"', '"', sub('"@en$', "", sub('^"', "", x)))

links <- sparql(paste(prefixes,
  "SELECT DISTINCT ?j ?o WHERE { ?j wdt:P236 ?i1 . ?o wdt:P236 ?i2 .",
  "VALUES ?p { wdt:P155 wdt:P156 wdt:P1365 wdt:P1366 } ?j ?p ?o .",
  "FILTER(?j != ?o) }"))
Sys.sleep(5)
links <- data.frame(from = qid(links[["?j"]]), to = qid(links[["?o"]]))
# each link in both directions, once
links <- unique(rbind(links, data.frame(from = links$to, to = links$from)))

# the names of the items with an English alias, and of the linked items
aliased <- sparql(paste(prefixes,
  "SELECT DISTINCT ?j ?l WHERE { ?j wdt:P236 ?i . ?j skos:altLabel ?a .",
  "FILTER(LANG(?a) = \"en\") { ?j rdfs:label ?l } UNION { ?j skos:altLabel ?l }",
  "FILTER(LANG(?l) = \"en\") }"))
Sys.sleep(5)
# the items at either end of a link, in two queries (one with both timed out)
linked <- lapply(c("?j ?p ?o", "?o ?p ?j"), \(link) {
  Sys.sleep(5)
  sparql(paste(prefixes,
    "SELECT DISTINCT ?j ?l WHERE { ?j wdt:P236 ?i1 . ?o wdt:P236 ?i2 .",
    "VALUES ?p { wdt:P155 wdt:P156 wdt:P1365 wdt:P1366 }", link, ".",
    "{ ?j rdfs:label ?l } UNION { ?j skos:altLabel ?l } FILTER(LANG(?l) = \"en\") }"))
})
names <- do.call(rbind, c(list(aliased), linked))
names <- data.frame(item = qid(names[["?j"]]), name = literal(names[["?l"]]))
names <- unique(names[nzchar(trimws(names$name)), ])

# Names the module does not need are left out, to keep the file small:
# - a name that differs from another name of the journal only in case,
#   punctuation, "&" or the words "of", "the", "and" (the module compares
#   names without them);
# - a name that abbreviates another name of the journal word for word ("J R
#   Stat Soc Series B Stat Methodol"), as the module accepts abbreviations;
# - a name with which another name of the journal begins ("Philosophical
#   Transactions of the Royal Society B" before "... B: Biological
#   Sciences"), as the module accepts a cited name that stops early;
# - web addresses;
# - a journal left with one name and no link to a journal with a name here,
#   which offers no other name.
stop_words <- c("of", "and", "the", "for", "in", "on", "a", "an", "de")
names <- names[!grepl("/|\\.(com|org|net)\\b|^www\\.", names$name), ]
words <- lapply(strsplit(gsub("[^a-z ]", " ", gsub("&", " and ", tolower(names$name))), "\\s+"),
                \(w) w[nzchar(w) & !w %in% stop_words])
key <- vapply(words, paste, "", collapse = " ")
keep <- nzchar(key) & !duplicated(paste(names$item, key))
names <- names[keep, ]
words <- words[keep]
abbreviates <- \(a, b) length(a) == length(b) && !identical(a, b) && all(startsWith(b, a))
begins <- \(a, b) length(a) < length(b) && identical(b[seq_along(a)], a)
redundant <- rep(FALSE, nrow(names))
for (g in split(seq_len(nrow(names)), names$item)) {
  if (length(g) < 2) next
  for (k in g) redundant[k] <- any(vapply(setdiff(g, k), \(o)
    abbreviates(words[[k]], words[[o]]) || begins(words[[k]], words[[o]]), logical(1)))
}
names <- names[!redundant, ]
links <- links[links$from %in% names$item & links$to %in% names$item, ]
several <- names(which(table(names$item) > 1))
names <- names[names$item %in% c(several, links$from, links$to), ]

# Stored without Wikidata's item numbers: the names in order of journal,
# the number of names of each journal, and the links as pairs of journals
# (their positions in that order)
names <- names[order(names$item), ]
journals <- rle(names$item)
links <- matrix(match(c(links$from, links$to), journals$values), ncol = 2)

saveRDS(list(names = names$name, size = journals$lengths, links = links,
             source = "Wikidata (CC0), English names of journals with an ISSN that have an alias or are linked by P155, P156, P1365 or P1366, via qlever.dev",
             date = Sys.Date()),
        "inst/databases/journal_successions.Rds", compress = "xz")
