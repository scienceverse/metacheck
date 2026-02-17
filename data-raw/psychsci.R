## code to prepare `psychsci` dataset goes here

pdf <- "data-raw/psychsci/pdf/"
files <- list.files(pdf, full.names = T)
bibr <- "data-raw/psychsci/bibr"
zip <- bibr_convert(files[9:250], bibr)

# make relative filename make sense
psychsci <- read_bibr(bibr)

usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")



