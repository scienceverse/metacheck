## code to prepare `retractionwatch` dataset goes here

library(dplyr)

# download newest RW update
options(timeout=300)
tmp <- tempfile(fileext = ".csv")
url <- paste0("https://api.labs.crossref.org/data/retractionwatch?", email())
# when Crossref has retired the Labs API, use its copy on GitLab (#305):
# url <- "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv"
download.file(url, destfile = tmp)

# tmp <- "../retractions.csv"

retractionwatch <- utils::read.csv(tmp) |>
  select(doi = OriginalPaperDOI,
         #pmid = OriginalPaperPubMedID,
         retractionwatch = RetractionNature) |>
  mutate(doi = trimws(doi)) |>
  filter(doi != "unavailable", doi != "", retractionwatch != "") |>   # no blank DOIs (U15)
  summarise(retractionwatch = unique(retractionwatch) |> paste(collapse = ";"), .by = doi)

count(retractionwatch, retractionwatch)

attr(retractionwatch, "date") <- Sys.Date()

#usethis::use_data(retractionwatch, overwrite = TRUE, compress = "xz")

saveRDS(retractionwatch, "inst/databases/retractionwatch.Rds", compress = "xz")



