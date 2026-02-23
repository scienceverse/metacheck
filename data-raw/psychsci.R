## code to prepare `psychsci` dataset goes here

pdf <- "data-raw/psychsci/pdf/"
files <- list.files(pdf, full.names = T)
bibr <- "data-raw/psychsci/bibr"
zip <- platform_bibr_convert(files[6:10], bibr)

psychsci <- read(bibr)
usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")

# copy 3 to test dir
list.files(bibr, full.names = T)[1:3] |>
  file.copy("tests/testthat/fixtures/psychsci/", overwrite = TRUE)
