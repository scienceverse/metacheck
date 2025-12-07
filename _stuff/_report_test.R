# setup
dir <- "_stuff/report-demo"
dir.create(dir, showWarnings = FALSE)

# choose modules to run
# module_list()
modules <- c(
  # "all_urls",
  # "aspredicted",
  # "causal_claims",
  # "code_check",
  # "power",
  # "prereg_check",
  # "reference_check",

  # checked ----
  "all_p_values",
  "effect_size",
  "exact_p",
  "marginal",
  "miscitation",
  "nonsignificant_pvalue",
  "ref_consistency",
  "retractionwatch",
  "statcheck"
)

# generate reports for a sample of n papers
n <- 1
files <- seq_along(psychsci) |> sample(n) |>
  lapply(\(i) {
    report(psychsci[[i]],
           modules = modules,
           output_file = paste0(dir, "/", psychsci[[i]]$id, ".html"),
           output_format = "html")
  })

# open all files in web browser
sapply(files, browseURL)

