# render versions from qmd (when qmd changes)
qmd <- "inst/demos/to_err_is_human.qmd"
quarto::quarto_render(qmd, "pdf")
quarto::quarto_render(qmd, "docx")

# convert newest PDF to bibr (when bibr changes)
pdf <- demofile("pdf")
bibr <- convert(pdf, "inst/demos", crossref_lookup = TRUE)

xml <- demofile("xml")
bibr <- convert(xml, "inst/demos", crossref_lookup = TRUE)

# read in and check (when read changes)
bibr <- demofile("json")
demopaper <- read(bibr)
stopifnot(paper_validate(demopaper))

gt <- read(file_path = "data-raw/demo/ground_truth.json")
stopifnot(paper_validate(gt))

View(gt$section)
View(demopaper$section)
# # generate JSON version
# devtools::load_all(".")
#
# paper <- demopaper()
# paper_validate(paper)
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
# paper2 <- .paper_coerce(paper2)
#
# paper_write(paper2, "ground_truth", "data-raw/demo")
#
