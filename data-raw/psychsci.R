## code to prepare `psychsci` dataset goes here

# pdf to bibr ----
pdf <- "data-raw/psychsci/pdf/"
files <- list.files(pdf, full.names = T)[1:3]
bibr <- "data-raw/psychsci/bibr"
zip_paths <- platform_bibr_convert(files, bibr)

psychsci <- read(bibr)
usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")

# copy 3 to test dir
list.files(bibr, full.names = T)[1:3] |>
  file.copy("tests/testthat/fixtures/psychsci/", overwrite = TRUE)


# grobid to bibr ----
xml_file <- "data-raw/psychsci/grobid_0.8.2"
save_path <- "data-raw/psychsci/bibr_from_grobid_0.8.2"
zip_paths <- grobid_to_bibr(xml_file, save_path, FALSE)
bibr_files <- list.files(save_path, full.names = T)
psychsci <- read(save_path)
usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")

# copy 3 to test dir
bibr_files[1:3] |>
  file.copy("tests/testthat/fixtures/psychsci/", overwrite = TRUE)
