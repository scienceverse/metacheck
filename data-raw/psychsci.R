## code to prepare `psychsci` dataset goes here

psychsci <- read_grobid("vignettes/xml")

usethis::use_data(psychsci, overwrite = TRUE)
