{
  "title": "Reference Consistency",
  "description": "Check if all references are cited and all citations are referenced",
  "authors": [{
    "orcid": "0000-0002-7523-5539",
    "name":{
      "surname": "DeBruine",
      "given": "Lisa"
    },
    "email": "debruine@gmail.com"
  }],
  "code": {
    "packages": ["dplyr"],
    "path": "ref-consistency.R"
  },
  "report": {
    "all": "This module relies on Grobid correctly parsing the references. There may be some false positives.",
    "red": "There are references that are not cited or citations that are not referenced",
    "green": "All references were cited and citations were referenced",
    "na": "No citations/references were detected"
  }
}
