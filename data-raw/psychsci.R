## code to prepare `psychsci` dataset goes here

# pdf to bibr ----
pdf <- "data-raw/psychsci/pdf"
files <- list.files(pdf, full.names = T)
bibr <- "data-raw/psychsci/bibr"
#file_paths <- platform_bibr_convert(files, bibr)

psychsci <- read(bibr, include_images = FALSE)

# fix names
names <- list.files(bibr) |> gsub("\\.json", "", x = _)
names(psychsci) <- names
for (n in names) psychsci[[n]]$paper_id <- n
usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")

# copy 3 to test dir
list.files(bibr, full.names = T)[1:3] |>
  file.copy("tests/testthat/fixtures/psychsci/", overwrite = TRUE)


# # grobid to bibr ----
grobid <- "data-raw/psychsci/grobid_0.8.2"
xml_file <- list.files(grobid, full.names = T)
save_path <- "data-raw/psychsci/bibr_from_grobid_0.8.2"
#json_paths <- grobid_to_bibr(xml_file, save_path, FALSE)
psychsci2 <- read(save_path)

# fix names
names <- list.files(grobid) |> gsub("\\.xml", "", x = _)
names(psychsci2) <- names
for (n in names) psychsci2[[n]]$paper_id <- n

usethis::use_data(psychsci2, overwrite = TRUE, compress = "xz")
