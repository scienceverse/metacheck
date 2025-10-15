## code to prepare `retractionwatch` dataset goes here

library(dplyr)

# download newest RW update
options(timeout=300)
tmp <- tempfile(fileext = ".csv")
url <- paste0("https://api.labs.crossref.org/data/retractionwatch?", email())
download.file(url, destfile = tmp)

# tmp <- "../retractions.csv"

retractionwatch <- utils::read.csv(tmp) |>
  select(doi = OriginalPaperDOI,
         #pmid = OriginalPaperPubMedID,
         retractionwatch = RetractionNature) |>
  filter(doi != "unavailable") |>
  summarise(retractionwatch = unique(retractionwatch) |> paste(collapse = ";"), .by = doi)

count(retractionwatch, retractionwatch)

attr(retractionwatch, "date") <- Sys.Date()

#usethis::use_data(retractionwatch, overwrite = TRUE, compress = "xz")

saveRDS(retractionwatch, "inst/databases/retractionwatch.Rds", compress = "xz")



