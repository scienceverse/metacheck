## code to prepare `psychsci` dataset goes here

# make relative filename make sense
setwd("vignettes/xml")
psychsci <- read_grobid(".")
setwd("../../")

# for (i in seq_along(psychsci)) {
#   psychsci[[1]]$info$doi <- psychsci[[1]]$info$doi |>
#     gsub("pss\\.", "", x = _) |>
#     gsub("sagepub\\.", "", x = _)
# }

usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")

