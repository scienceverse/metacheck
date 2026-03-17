# render versions from qmd
qmd <- "data-raw/demo/to_err_is_human.qmd"
quarto::quarto_render(qmd, "pdf")
quarto::quarto_render(qmd, "docx")

# convert newest PDF to bibr zip
pdf <- "data-raw/demo/to_err_is_human.pdf"
zip <- platform_bibr_convert(pdf, "data-raw/demo")

# copy to inst
file.copy(zip, "inst/demo/", overwrite = TRUE)
file.copy(pdf, "inst/demo/", overwrite = TRUE)

# copy to tests (fix this redundancy eventually)
docx <- "data-raw/demo/to_err_is_human.docx"
file.copy(zip, "tests/testthat/fixtures/bibr", overwrite = TRUE)
file.copy(zip, "tests/testthat/fixtures/formats/", overwrite = TRUE)
file.copy(pdf, "tests/testthat/fixtures/formats/", overwrite = TRUE)
file.copy(docx, "tests/testthat/fixtures/formats/", overwrite = TRUE)

# generate JSON version
# devtools::load_all(".")
#
# paper <- demopaper()
# validate_paper(paper)
#
# ground_truth <- list.files("data-raw/demo/ground_truth", "\\.csv$", full.names = T)
#
# paper2 <- paper()
# for (gt in ground_truth) {
#   table <- basename(gt) |> gsub("\\.csv$", "", x = _)
#   paper2[[table]] <- read.csv(gt)
# }
# paper2$paper_id <- paper$paper_id
#
# paper2$tables$contents <- list(paper2$table_1)
# paper2$table_1 <- NULL
#
# paper2 |>
#   jsonlite::toJSON(pretty = TRUE)|>
#   write("data-raw/demo/to_err_is_human.json")
#
# paper3 <- jsonlite::read_json("data-raw/demo/to_err_is_human.json", simplifyVector = TRUE)
# validate_paper(paper3)
#
# zip <- "data-raw/demo/to_err_is_human.zip"
# paper_write(paper3, zip)
# file.copy(zip, "tests/testthat/fixtures/bibr", overwrite = TRUE)
# file.copy(zip, "tests/testthat/fixtures/formats/", overwrite = TRUE)
