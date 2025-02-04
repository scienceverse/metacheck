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
      "refs <- concat_tables(paper, c('references'))",
      "dplyr::semi_join(refs, retractionwatch, by = 'doi')"
    ]
  },
  "traffic_light": {
    "found": "yellow",
    "not_found": "green"
  },
  "report": {
    "yellow": "You cited some papers in the Retraction Watch database; double-check that you are acknowledging their retracted status when citing them.",
    "green": "You cited no papers in the Retraction Watch database"
  }
}
