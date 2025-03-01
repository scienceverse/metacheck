{
  "title": "RetractionWatch",
  "description": "Flag any cited papers in the RetractionWatch database",
  "authors": [{
    "orcid": "0000-0002-7523-5539",
    "name":{
      "surname": "DeBruine",
      "given": "Lisa"
    },
    "email": "debruine@gmail.com"
  }],
  "code": {
    "packages": ["papercheck", "dplyr"],
    "code": [
      "refs <- papercheck::concat_tables(paper, c('references'))",
      "rw <- dplyr::inner_join(refs, papercheck::retractionwatch, by = 'doi')",
      "if (nrow(rw) > 0) {",
      "  cites <- papercheck::concat_tables(paper, c('citations'))",
      "  rw <- dplyr::left_join(rw, cites, by = c('id', 'bib_id'))",
      "}",
      "rw"
    ]
  },
  "traffic_light": {
    "found": "yellow",
    "not_found": "green"
  },
  "report": {
    "yellow": "You cited some papers in the Retraction Watch database (as of 2025-02-28). These may be retracted, have corrections, or expressions of concern.",
    "green": "You cited no papers in the Retraction Watch database (as of 2025-02-28)"
  }
}
