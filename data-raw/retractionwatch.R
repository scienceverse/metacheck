## code to prepare `retractionwatch` dataset goes here

library(dplyr)

# download newest RW update
options(timeout=300)
tmp <- tempfile(fileext = ".csv")
url <- "https://api.labs.crossref.org/data/retractionwatch?debruine@gmail.com"
download.file(url, destfile = tmp)

# tmp <- "../retractions.csv"

retractionwatch <- utils::read.csv(tmp) |>
  select(doi = OriginalPaperDOI,
         #pmid = OriginalPaperPubMedID,
         retractionwatch = RetractionNature) |>
  filter(doi != "unavailable") |>
  summarise(retractionwatch = unique(retractionwatch) |> paste(collapse = ";"), .by = doi)

count(retractionwatch, retractionwatch)

usethis::use_data(retractionwatch, overwrite = TRUE, compress = "xz")



