{
  "title": "Check Status of OSF Links",
  "description": "List all OSF links and whether they are open, closed, or do not exist.",
  "authors": [{
    "orcid": "0000-0002-0247-239X",
    "name":{
      "surname": "Lakens",
      "given": "Daniël"
    },
    "email": "lakens@gmail.com"
  },
  {
    "orcid": "0000-0002-7523-5539",
    "name":{
      "surname": "DeBruine",
      "given": "Lisa"
    },
    "email": "debruine@gmail.com"
  }],
  "code": {
    "packages": ["papercheck", "httr", "dplyr", "progressbar"],
    "path": "osf-check.R"
  },
  "report": {
    "na": "No OSF links were detected",
    "red": "We detected closed OSF links",
    "yellow": "There may be problems with some OSF links",
    "green": "All OSF links are open",
    "fail": "All attempts to check OSF links failed; check if you are offline."
  }
}
