# setup
dir <- "_stuff/report-demo"
dir.create(dir, showWarnings = FALSE)

# choose modules to run
# module_list()
modules <- c(
  # "all_urls",
  # "aspredicted",
   "code_check"#,
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
  # "statcheck"
)

# generate reports for a sample of n papers
n <- 1
files <- seq_along(psychsci) |> sample(n) |>
  lapply(\(i) {
    paper <- psychsci[[i]]

    report(paper,
           modules = modules,
           output_file = paste0(dir, "/", paper$id, ".html"),
           output_format = "html")
  })

# open all files in web browser
sapply(files, browseURL)


# Or do demo paper
paper <- read(demoxml())

# example with osf Rmd files and github files:
# paper <- psychsci[[203]]
# example with missing data files:
paper <- psychsci[[221]]
# Many R files, some with library in different places.
# paper <- psychsci[[225]]
# Best example, with many issues, for paper:
# paper <- psychsci[[233]]

osf_api_calls(0)
file <- report(paper,
       modules = modules,
       output_file = paste0(dir, "/", paper$id, ".html"),
       output_format = "html")
osf_api_calls()
browseURL(file)
