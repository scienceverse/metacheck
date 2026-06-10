## code to prepare `psychsci` dataset goes here

# pdf to bibr ----
pdf <- "data-raw/psychsci/pdf"
files <- list.files(pdf, full.names = T)
bibr <- "data-raw/psychsci/bibr"
file_paths <- convert(files[1:10], bibr)

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
save_path <- "data-raw/psychsci/bibr_from_grobid_0.9.0-crf4"
dir.create(save_path, showWarnings = FALSE)
json_paths <- grobid_to_bibr(xml_file, save_path, FALSE)
psychsci <- read(save_path)

# add old bib_match
for ( i in 1:250) {
  psychsci[[i]]$bib_match <- old_ps[[i]]$bib_match
}
paper_write(psychsci, paste0(save_path, "/", names(psychsci)))

# or new bib_match
for (i in seq_along(psychsci)) { # 164
  print(i)
  #if (is.null(psychsci[[i]]$bib_match)) {
    psychsci[[i]] <- add_bib_match(psychsci[[i]])
    paper_write(psychsci[i], save_path = save_path)
 # }
}
# check bib_match 5, 9, 65, 103, 161,


# # fix names
# names <- list.files(grobid) |> gsub("\\.xml", "", x = _)
# names(psychsci) <- names
# for (n in names) psychsci[[n]]$paper_id <- n
#
# all(sapply(psychsci, paper_validate))
#
# psychsci <- add_bib_match(psychsci)

usethis::use_data(psychsci, overwrite = TRUE, compress = "xz")


## abstract checks ----------------------------------------------

psychsci_full <- read("data-raw/psychsci/bibr_from_grobid_0.9.0-full")
psychsci_crf <- read("data-raw/psychsci/bibr_from_grobid_0.9.0-crf")

abs_f <- text_search(psychsci_full) |>
  dplyr::filter(section_type == "abstract") |>
  dplyr::select(paper_id, text, text_id)
abs_c <- text_search(psychsci_crf) |>
  dplyr::filter(section_type == "abstract") |>
  dplyr::select(paper_id, text, text_id)

x <- dplyr::full_join(abs_c, abs_f,
                      by = c("paper_id", "text"),
                      suffix = c(".crf", ".full")) |>
  dplyr::filter(is.na(text_id.crf) | is.na(text_id.full)) |>
  dplyr::arrange(paper_id)

module <- "power"
mo_f <- module_run(psychsci_full, module)
mo_c <- module_run(psychsci_crf, module)

expect_equal(mo_f$summary_table, mo_c$summary_table)



# - bib checks
bm <- paper_table(psychsci, "bib_match")
#saveRDS(bm, "~/Desktop/bm.rds")
bib <- paper_table(psychsci, "bib")
bib_bm <- inner_join(bib, bm, by = c("paper_id", "bib_id"))

text <- paper_table(psychsci, "text") |>
  select(text_id, paper_id, text)

title_mis <- bib_bm |>
  rowwise() |>
  mutate(title.y = title.y |> gsub("\\s+", " ", x = _) |>
           paste0("<p>", x = _, "</p>") |>
           xml2::read_html() |> xml2::xml_text()) |>
  ungroup() |>
  tidyr::separate(title.x, c("title.x", "subtitle.x"), sep = ": ", extra = "merge", fill = "right") |>
  tidyr::separate(title.y, c("title.y", "subtitle.y"), sep = ": ", extra = "merge", fill = "right") |>
  rowwise() |>
  mutate(adist = adist(tolower(title.x), tolower(title.y))[[1]]) |>
  ungroup() |>
  arrange(title.x != "", desc(adist)) |>
  select(adist, starts_with("title"), paper_id, text_id) |>
    left_join(text, by = c("paper_id", "text_id"))

readr::write_excel_csv(title_mis, "~/Desktop/title-mis.csv")


"data-raw/psychsci/grobid_0.9.0-crf/09567976221094782.xml" |> rstudioapi::documentOpen()

)
