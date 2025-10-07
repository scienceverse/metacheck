# api.R
# Plumber API for papercheck
# Supports both single-paper analysis and GROBID processing

library(plumber)
library(papercheck)

#* @apiTitle Papercheck API
#* @apiDescription API for analyzing academic papers. Upload GROBID TEI XML to extract metadata, authors, references, and more.

#* @plumber
function(pr) {
  pr %>%
    # Paper analysis endpoints - upload XML to analyze
    pr_mount("/paper", plumber::pr("endpoints/paper.R"))
}

#* Health check endpoint
#* @get /health
#* @serializer json
function() {
  list(status = "ok", timestamp = Sys.time())
}
