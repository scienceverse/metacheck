# render versions from qmd
qmd <- "data-raw/demo/to_err_is_human.qmd"
quarto::quarto_render(qmd, "html")
quarto::quarto_render(qmd, "pdf")
quarto::quarto_render(qmd, "docx")

# convert newest PDF to bibr zip
pdf <- "data-raw/demo/to_err_is_human.pdf"
zip <- platform_bibr_convert(pdf, "data-raw/demo")

# copy to inst
file.copy(zip, "inst/demo/", overwrite = TRUE)
file.copy(pdf, "inst/demo/", overwrite = TRUE)

# copy to tests
docx <- "data-raw/demo/to_err_is_human.docx"
file.copy(zip, "tests/testthat/fixtures/bibr", overwrite = TRUE)
file.copy(zip, "tests/testthat/fixtures/formats/", overwrite = TRUE)
file.copy(pdf, "tests/testthat/fixtures/formats/", overwrite = TRUE)
file.copy(docx, "tests/testthat/fixtures/formats/", overwrite = TRUE)

# generate JSON version
devtools::load_all(".")

paper <- demopaper()
paper2 <- lapply(paper, \(tbl) {
  if (is.data.frame(tbl)) {
    for (col in names(tbl)) {
      #if (any(grepl("^vctrs_unspecified", class(tbl[[col]])))) {
        tbl[[col]] <- unclass(tbl[[col]])
      #}
    }
  }
  tbl
})

paper2$authors$role <- lapply(paper2$authors$role, unclass)

paper2 |>
  jsonlite::toJSON(pretty = TRUE)|>
  write("data-raw/demo/to_err_is_human.json")
