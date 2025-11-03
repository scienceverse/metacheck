{
  "title": "StatCheck",
  "description": "Check consistency of p-values and test statistics",
  "example": "module_run(psychsci[[1]], \"statcheck\")",
  "code": {
    "packages": ["metacheck"],
    "path": "statcheck.R"
  },
  "report": {
    "na": "No test statistics were detected",
    "red": "We detected possible errors in test statistics",
    "green": "We detected no errors in test statistics",
    "fail": "StatCheck failed"
  }
}
