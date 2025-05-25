## code to prepare `psychsci` dataset goes here

# make relative filename make sense
setwd("tests/testthat/psychsci/light")
psychsci <- read_grobid(".")
setwd("../../../../")

# for (i in seq_along(psychsci)) {
#   psychsci[[1]]$info$doi <- psychsci[[1]]$info$doi |>
#     gsub("pss\\.", "", x = _) |>
#     gsub("sagepub\\.", "", x = _)
# }

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
