## code to prepare `psychsci` dataset goes here

# pdf to bibr ----
pdf <- "data-raw/psychsci/pdf"
files <- list.files(pdf, full.names = T)
bibr <- "data-raw/psychsci/bibr"
file_paths <- platform_bibr_convert(files, bibr)

psychsci <- read(bibr, include_images = FALSE)
usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")

# copy 3 to test dir
list.files(bibr, full.names = T)[1:3] |>
  file.copy("tests/testthat/fixtures/psychsci/", overwrite = TRUE)


# # grobid to bibr ----
# xml_file <- list.files("data-raw/psychsci/grobid_0.8.2", full.names = T)
# save_path <- "data-raw/psychsci/bibr_from_grobid_0.8.2"
# zip_paths <- grobid_to_bibr(xml_file[211:250], save_path, FALSE)
# bibr_files <- list.files(save_path, full.names = T)
# psychsci <- read(save_path)
# usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")
#
# # copy 3 to test dir
# bibr_files[1:3] |>
#   file.copy("tests/testthat/fixtures/psychsci/", overwrite = TRUE)
