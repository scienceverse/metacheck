## code to prepare `psychsci` dataset goes here

# pdf to bibr ----
pdf <- "data-raw/psychsci/pdf"
files <- list.files(pdf, full.names = T)
bibr <- "data-raw/psychsci/bibr"
#file_paths <- bibr_convert(files, bibr, backend = "scivrs")

psychsci <- read(bibr, include_images = FALSE)

# fix names
names <- list.files(bibr) |> gsub("\\.json", "", x = _)
names(psychsci) <- names
for (n in names) psychsci[[n]]$paper_id <- n

# fix other stuff
ps <- lapply(psychsci, \(x) {
  x$text$raw_text <- NULL
  if (nrow(x$bib)) {
    print(x$paper_id)
    x$bib$authors <- lapply(x$bib$authors, \(a) {
      if (length(a) == 1 && is.na(a)) return(character(0))
      paste(a$family, a$given, sep = ", ")
    })

    x$bib$editors <- lapply(x$bib$editors, \(a) {
      if (length(a) == 1 && is.na(a)) return(character(0))
      paste(a$family, a$given, sep = ", ")
    })

    x$bib_match <- x$bib$match$crossref
    x$bib_match$bib_id <- x$bib$bib_id
    x$bib_match$service <- "crossref"
    x$bib_match$service_id <- x$bib_match$id
    x$bib_match$id <- NULL

    x$bib$year <- x$bib$publication_year
    x$bib$date <- x$bib$publication_date
    x$bib$publication_year <- NULL
    x$bib$publication_date <- NULL

    x$bib_match$year <- x$bib_match$publication_year
    x$bib_match$date <- x$bib_match$publication_date
    x$bib_match$publication_year <- NULL
    x$bib_match$publication_date <- NULL

    x$bib_match <- x$bib_match[!is.na(x$bib_match$score), ]
  }
  x$bib$match <- NULL

  paper_coerce(x)
}) |> paperlist()

all(sapply(ps, paper_validate))
psychsci <- ps

usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")

# copy 3 to test dir
list.files(bibr, full.names = T)[1:3] |>
  file.copy("tests/testthat/fixtures/psychsci/", overwrite = TRUE)


# # grobid to bibr ----
grobid <- "data-raw/psychsci/grobid_0.8.2"
xml_file <- list.files(grobid, full.names = T)
save_path <- "data-raw/psychsci/bibr_from_grobid_0.8.2"
json_paths <- grobid_to_bibr(xml_file, save_path, FALSE)
psychsci2 <- read(save_path)

# fix names
names <- list.files(grobid) |> gsub("\\.xml", "", x = _)
names(psychsci2) <- names
for (n in names) psychsci2[[n]]$paper_id <- n

all(sapply(psychsci2, paper_validate))

usethis::use_data(psychsci2, overwrite = TRUE, compress = "xz")
