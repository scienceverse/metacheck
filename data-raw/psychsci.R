## code to prepare `psychsci` dataset goes here

# pdf to bibr ----
pdf <- "data-raw/psychsci/pdf"
files <- list.files(pdf, full.names = T)
bibr <- "data-raw/psychsci/bibr"
file_paths <- convert_bibr(files[1], bibr, backend = "scivrs")

psychsci <- read(bibr, include_images = FALSE)

# fix names
names <- list.files(bibr) |> gsub("\\.json", "", x = _)
names(psychsci) <- names
for (n in names) psychsci[[n]]$paper_id <- n

usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")

# copy 3 to test dir
list.files(bibr, full.names = T)[1:3] |>
  file.copy("tests/testthat/fixtures/psychsci/", overwrite = TRUE)


# -------------------------------------------------------------------
# pdf to grobid
pdf <- "data-raw/psychsci/pdf"
file_path <- list.files(pdf, full.names = T)
save_path <- "data-raw/psychsci/grobid_0.9.0-crf"
api_url <- "http://localhost:8070"
convert_grobid(file_path[199:250], save_path, api_url)

# # grobid to bibr ----
grobid <- "data-raw/psychsci/grobid_0.9.0-crf"
xml_file <- list.files(grobid, full.names = T)
save_path <- "data-raw/psychsci/bibr_from_grobid_0.9.0"
json_paths <- grobid_to_bibr(xml_file, save_path, TRUE)
psychsci <- read(save_path)


# fix names
names <- list.files(grobid) |> gsub("\\.xml", "", x = _)
names(psychsci) <- names
for (n in names) psychsci[[n]]$paper_id <- n

all(sapply(psychsci, paper_validate))


psychsci <- add_bib_match(psychsci)

usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")
