{
  "title": "Imprecise P-Values",
  "description": "List any p-values reported with insufficient precision (e.g., p < .05 or p = n.s.)",
  "example": "module_run(psychsci, \"imprecise-p\")",
  "authors": [{
    "orcid": "0000-0002-7523-5539",
    "name":{
      "surname": "DeBruine",
      "given": "Lisa"
    },
    "email": "debruine@gmail.com"
  }],
  "code": {
    "packages": ["metacheck"],
    "path": "imprecise-p.R"
  },
  "report": {
    "red": "You may have reported some imprecise p-values",
    "yellow": "You may have reported some imprecise p-values",
    "green": "All p-values were reported with standard precision",
    "na": "No p-values were detected"
  }
}
