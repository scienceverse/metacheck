# Journal names that look like conferences
#
# ref_accuracy flags a reference cited in a journal whose work is found only
# as a preprint, but not one cited from a conference, because many
# proceedings have no DOIs. A venue is taken as a conference when its name
# has an acronym ("ICML") or a conference word ("Proceedings"). This list
# holds the journals with such names ("IEEE Transactions on ...", "PLOS
# One", "Proceedings of the National Academy of Sciences"), so that they are
# still treated as journals. It is built from the English names of the
# Wikidata items that have an ISSN (CC0), queried through QLever's copy of
# Wikidata. Conference series with an ISSN ("IEEE International Conference
# on Robotics and Biomimetics, ROBIO") are left out: a name with an event
# word and no journal word, or ending with an acronym after a comma, and
# series of proceedings (below).

query <- paste(
  "PREFIX wdt: <http://www.wikidata.org/prop/direct/>",
  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
  "SELECT DISTINCT ?l WHERE { ?j wdt:P236 ?issn . ?j rdfs:label ?l .",
  "FILTER(LANG(?l) = \"en\") }")
resp <- httr2::request("https://qlever.dev/api/wikidata") |>
  httr2::req_url_query(query = query) |>
  httr2::req_headers(Accept = "text/tab-separated-values") |>
  httr2::req_perform()
titles <- strsplit(httr2::resp_body_string(resp), "\n")[[1]][-1]
titles <- unique(sub('"@en$', "", sub('^"', "", titles)))
titles <- gsub('\\\\"', '"', titles)

# the module's test for a conference-like venue
conference_words <- "(?i)\\b(proc|proceedings|conference|conf|workshops?|symposium|meeting|congress|colloquium|summit|track)\\b|advances in neural information processing"
acronym_word <- "(^|[^[:alnum:]])[[:alnum:]]*[A-Z][[:alnum:]]*[A-Z][[:alnum:]]*($|[^[:alnum:]])"
looks_like_conference <- grepl(conference_words, titles, perl = TRUE) |
  grepl(acronym_word, titles)

event <- "(?i)\\b(conferences?|conferencia|congreso|symposium|workshops?|meetings?|congress|colloquium|summit|digest|convention|forum)\\b"
journal_word <- "(?i)\\b(journal|review|letters|transactions|bulletin|annals|quarterly|magazine)\\b"
is_event <- grepl(event, titles, perl = TRUE) & !grepl(journal_word, titles, perl = TRUE)
ends_with_acronym <- grepl(",\\s*[[:alnum:] -]*[A-Z]{2,}[[:alnum:] -]*$", titles)

# series of conference proceedings with an ISSN ("Proceedings of Machine
# Learning Research" for ICML, "Advances in Neural Information Processing
# Systems" for NeurIPS, "Leibniz International Proceedings in Informatics",
# "Springer Proceedings in Physics")
series <- paste0("(?i)\\bproceedings (in|series)\\b|machine learning research|",
                 "lecture notes|electronic proceedings|conference proceedings|",
                 "\\babstracts\\b|\\bseminar\\b|sinn und bedeutung|",
                 "advances in neural information processing")
is_series <- grepl(series, titles, perl = TRUE)

journal_titles <- sort(titles[looks_like_conference & !is_event & !ends_with_acronym &
                                !is_series])

# journals often cited by their acronym alone
journal_acronyms <- c("PNAS", "JAMA", "BMJ", "NEJM", "PLoS ONE", "PLOS ONE",
                      "JMLR", "TMLR", "TACL", "JAIR", "TPAMI", "IJCV")

saveRDS(list(titles = journal_titles, acronyms = journal_acronyms,
             source = "Wikidata (CC0), items with an ISSN, via qlever.dev",
             date = Sys.Date()),
        "inst/databases/journal_names.Rds", compress = "xz")
