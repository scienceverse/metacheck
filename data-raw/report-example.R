devtools::load_all(".")
llm_use(TRUE)
paper <- read(demoxml())
modules <- c(
  # local only
  "stat_p_exact",
  "stat_p_nonsig",
  "marginal",
  "stat_effect_size",
  "stat_check",
  "open_practices",
  # need external services
  "prereg_check",
  "causal_claims",
  "power",
  "code_check",
  # use ref_doi_check
  "ref_doi_check",
  "ref_accuracy",
  "ref_replication",
  "ref_retraction",
  "ref_pubpeer"
)
file <- report(paper, modules,
               output_file = "pkgdown/assets/report-example.qmd",
               output_format = "qmd")

# manually check
# browseURL(file)

# automatically generate html and PDF
quarto::quarto_render(file, output_format = "html")
file.copy(file, "docs/report-example.qmd", overwrite = TRUE)
file.copy("pkgdown/assets/report-example.html", "docs/report-example.html", overwrite = TRUE)

browseURL("pkgdown/assets/report-example.html")

