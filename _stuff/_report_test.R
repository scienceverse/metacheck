# setup
dir <- "_stuff/report-demo"
dir.create(dir, showWarnings = FALSE)

# choose modules to run
# module_list()
modules <- c(
  # "all_urls",
  # "ref_consistency",
  # "all_p_values",


  # checked ----
  "causal_claims",
  "prereg_check",
  "aspredicted",
  "power",

  "exact_p",
  "nonsignificant_pvalue",
  "marginal",
  "effect_size",

  "code_check",
  "statcheck",

  "reference_check",
  "miscitation",
  "retractionwatch"

)

# generate reports for a sample of n papers
n <- 1
files <- seq_along(psychsci) |> sample(n) |>
  lapply(\(i) {
    #i = which(names(psychsci) == "0956797614557697")
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
# paper <- psychsci[[221]]
# Many R files, some with library in different places.
# paper <- psychsci[[225]]
# Best example, with many issues, for paper:
# paper <- psychsci[[233]]

llm_use(TRUE)

osf_api_calls(0)
file <- report(paper,
       modules = modules,
       output_file = "pkgdown/assets/report-example.qmd",
       output_format = "qmd")
osf_api_calls()
browseURL(file)
