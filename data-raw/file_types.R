url <- "https://raw.githubusercontent.com/dyne/file-extension-list/refs/heads/master/pub/extensions.json"

types <- jsonlite::read_json(url, simplifyVector = TRUE) |>
  as.data.frame() |>
  t() |>
  dplyr::as_tibble(rownames = "ext") |>
  dplyr::select(ext, type = V2)

types$type[types$type == "sheet"] <- "data"

new <- dplyr::tribble(
  ~ext, ~type,
  "gitignore", "config",
  "git", "config",
  "Rmd", "code",
  "quarto", "code",
  "qmd", "code",
  "rda", "code",
  "Rd", "code",
  "Rds", "code",
  "ico", "image",
  "json", "data",
  "tsv", "sheet",
  "yml", "config",
  "yaml", "config",
  "Rproj", "config",
  "por", "stats",
  "jasp", "stats",
  "sps", "stats",
  "spss", "stats",
  "sav", "data",
  "dta", "data",
  "dat", "data",
  "sas7bdat", "data",
  "DO", "stats"
)

file_types <- dplyr::bind_rows(types, new)
file_types$ext <- tolower(file_types$ext)

usethis::use_data(file_types, overwrite = TRUE, compress = "xz")
usethis::use_r("file_types")
