url <- "https://raw.githubusercontent.com/dyne/file-extension-list/refs/heads/master/pub/extensions.json"

types <- jsonlite::read_json(url, simplifyVector = TRUE) |>
  as.data.frame() |>
  t() |>
  dplyr::as_tibble(rownames = "ext") |>
  dplyr::select(ext, type = V2)

types$type[types$type == "sheet"] <- "data"

extensions <- list()
extensions[["code"]] <- data.frame(
  ext = c("R", "Rmd", "qmd", "quarto",
          "rda", "Rd", "Rds",
          "ado","dss", "jnlp",
          "Rnw", "jl", "ipynb", "mat")
)
extensions[["code"]]$type <- "code"

extensions[["data"]] <- data.frame(
  ext = c("rds", "RData", "sav", "zsav", "por",
          "dta", "sas7bdat", "xpt", "sd7", "jasp",
          "omv", "arff", "sav.gz", "csv.sav", "gen",
          "gdt", "gdtb", "dft", "wf1",
          "dat", "json", "tsv", "csv")
)
extensions[["data"]]$type <- "data"

extensions[["config"]] <- data.frame(
  ext = c("gitignore", "git", "yml", "yaml", "config", "Rproj")
)
extensions[["config"]]$type <- "config"

extensions[["stats"]] <- data.frame(
  ext = c( "sas", "por", "jasp", "sps", "spss", "DO")
)
extensions[["stats"]]$type <- "stats"

extensions[["extra"]] <- data.frame(
  ext = "ico",
  type = "image"
)


file_types <- do.call(dplyr::bind_rows, c(list(types), extensions))
file_types$ext <- tolower(file_types$ext)
file_types <- unique(file_types) |>
  dplyr::summarise(type = paste(type, collapse = ";"), .by = ext)

usethis::use_data(file_types, overwrite = TRUE, compress = "xz")
usethis::use_r("file_types")



