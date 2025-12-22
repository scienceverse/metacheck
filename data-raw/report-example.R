devtools::load_all(".")
paper <- read(demoxml())
file <- report(paper,
               output_file = "pkgdown/assets/report-example.qmd",
               output_format = "qmd")

# manually check
browseURL(file)

# automatically generate html and PDF
quarto::quarto_render(file, output_format = "html")
file.copy(file, "docs/report-example.qmd", overwrite = TRUE)
file.copy("pkgdown/assets/report-example.html", "docs/report-example.html", overwrite = TRUE)
