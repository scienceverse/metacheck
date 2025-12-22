# setup
dir <- "_stuff/report-demo"
dir.create(dir, showWarnings = FALSE)

llm_use(TRUE)

# choose modules to run
# module_list()
modules <- c(
  # superceded ----
  # "retractionwatch",
  # "aspredicted",

  # helper modules ----
  # "all_urls",
  # "all_p_values",

  # under development ----
  # "causal_claims",
  # "miscitation",
  # "ref_consistency",

  # in reports ----
  "prereg_check",
  "power",
  "exact_p",
  "nonsig_p",
  "marginal",
  "effect_size",
  "code_check",
  "statcheck",
  "reference_check",
  "replications",
  "retractionwatch",
  "pubpeer"
)

# modules <- c("replications", "fail", "retractionwatch")

modules <- "nonsig_p"

# generate reports for a sample of n papers
n <- 1
output <- "html"
files <- seq_along(psychsci) |> sample(n) |>
  lapply(\(i) {
    i = which(names(psychsci) == "0956797621991137")
    paper <- psychsci[[i]]
    print(paper$id)

    args <- list(
      reference_check = list(crossref_min_score = 75)
    )

    report(paper,
           modules = modules,
           output_file = paste0(dir, "/", paper$id, ".", output),
           output_format = output,
           args = args)
  })

# open all files in web browser
sapply(files, browseURL)
