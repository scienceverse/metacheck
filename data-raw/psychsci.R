## code to prepare `psychsci` dataset goes here

# make relative filename make sense
dir <- "data-raw/psychsci/grobid_0.8.2/"
setwd(dir)
psychsci <- read_grobid(".")
setwd("../../../")

usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")


# 09567976231188124 is an erratum
# 09567976221139496 and 09567976231217508 are just a corrigendum
# 0956797620955209 and 0956797620939054 have no titles but are research papers (both preregistered direct replications)




# fix corrigenda DOIs ----
info <- info_table(psychsci)
bad_dois <- which(paste0("10.1177/", info$id) != info$doi)
info[bad_dois, c("id", "doi", "title")]


# test full vs light ----
full <- read_grobid("data-raw/psychsci/grobid_0.8.2-full/")
light <- read_grobid("data-raw/psychsci/grobid_0.8.2/")

info <- c("filename", "title", "keywords", "doi", "description")
light_info <- info_table(light, info)
full_info <- info_table(full, info)

all(light_info$id == full_info$id)

doi_mismatch <- data.frame(
  id = light_info$id,
  light = light_info$doi,
  full= full_info$doi
) |>
  dplyr::filter(light != full)

kw_mismatch <- data.frame(
  id = light_info$id,
  light = light_info$keywords,
  full= full_info$keywords
) |>
  dplyr::filter(light != full)

title_mismatch <- data.frame(
  id = light_info$id,
  light = light_info$title,
  full= full_info$title
) |>
  dplyr::filter(light != full)
