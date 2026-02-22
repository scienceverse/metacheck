## code to prepare `psychsci` dataset goes here

pdf <- "data-raw/psychsci/pdf/"
files <- list.files(pdf, full.names = T)
bibr <- "data-raw/psychsci/bibr"
zip <- bibr_convert(files[7:10], bibr)

psychsci <- read(bibr)
usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")


files[1:20]
list.files(bibr)
