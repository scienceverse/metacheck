# render versions from qmd
quarto::quarto_render("data-raw/demo/to_err_is_human.qmd", "html")
quarto::quarto_render("data-raw/demo/to_err_is_human.qmd", "pdf")
quarto::quarto_render("data-raw/demo/to_err_is_human.qmd", "docx")

# convert newest PDF to bibr zip
pdf <- "data-raw/demo/to_err_is_human.pdf"
zip <- bibr_convert(pdf, "data-raw/demo")

# copy to inst
file.copy(zip, "inst/demo/", overwrite = TRUE)
file.copy(pdf, "inst/demo/", overwrite = TRUE)

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
