# setup
dir <- "_stuff/report-demo"
dir.create(dir, showWarnings = FALSE)

# choose modules to run
# module_list()
modules <- c(
  # "all_urls",
  # "aspredicted",
   "code_check",
  # "power",

  # checked ----
   # "all_p_values",
   # "effect_size",
   # "exact_p",
   # "marginal",
   # "prereg_check",
   # "reference_check",
  # "miscitation",
   # "nonsignificant_pvalue",
   # "causal_claims",
  # "ref_consistency",
  # "retractionwatch",
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


# Or do demo paper
paper <- read(demoxml())
report(paper,
       modules = modules,
       output_file = paste0(dir, "/", paper$id, ".html"),
       output_format = "html")
browseURL(paste0(dir, "/", paper$id, ".html"))
