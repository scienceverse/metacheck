# setup
dir <- "_stuff/report-demo"
dir.create(dir, showWarnings = FALSE)

llm_use(FALSE)

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
  # "miscitation",
  # "ref_consistency",

  # in reports ----
  "prereg_check",
  "causal_claims",
  "open_practices",
  "funding_check",
  "coi_check",
  "power",
  "stat_p_exact",
  "stat_p_nonsig",
  "marginal",
  "stat_effect_size",
  # "code_check",
  "stat_check",
  # "ref_doi_check",
  # "ref_accuracy",
  "ref_replication",
  "ref_retraction",
  "ref_pubpeer"
)

# paper <- psychsci$`0956797621991137`
# mo <- module_run(paper, "power")


# generate reports for a sample of n papers
n <- 1
output <- "html"
files <- seq_along(psychsci) |> sample(n) |>
  lapply(\(i) {
    #i = which(names(psychsci) == "0956797615583071")
    paper <- psychsci[[i]]
    print(paper$id)

    args <- list(
      doi_check = list(crossref_min_score = 50)
    )

    report(paper,
           modules = modules,
           output_file = paste0(dir, "/", paper$id, ".", output),
           output_format = output,
           args = args)
  })

# open all files in web browser
sapply(files, browseURL)
