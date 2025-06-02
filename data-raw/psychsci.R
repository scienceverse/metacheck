## code to prepare `psychsci` dataset goes here

# make relative filename make sense
setwd("data-raw/psychsci/grobid_0.8.2/")
psychsci <- read_grobid(".")
setwd("../../../")

info <- info_table(psychsci)

usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")

setwd("tests/testthat/psychsci/full")
psychsci_full <- read_grobid(".")
setwd("../../../../")
usethis::use_data(psychsci_full, overwrite = TRUE, compress = "xz")


# test full vs light

xml_files <- list.files("tests/testthat/psychsci/full/")

mismatch <- purrr::map_df(xml_files, \(file) {
  f <- readLines(paste0("tests/testthat/psychsci/full/", file))
  l <- readLines(paste0("tests/testthat/psychsci/light/", file))
  data.frame(
    id = rep(file, sum(f != l)),
    line = which(f != l),
    full_text = f[f != l],
    light_text = l[f != l]
  )
})
