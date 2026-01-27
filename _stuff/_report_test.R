# setup
dir <- "_stuff/report-demo"
dir.create(dir, showWarnings = FALSE)

llm_use(TRUE)
llm_model("gemini")
llm_model("groq/llama-3.3-70b-versatile")

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
  # "prereg_check",
  # "causal_claims",
  # "open_practices",
  # "funding_check",
  # "coi_check",
  # "power",
  # "stat_p_exact",
  # "stat_p_nonsig",
  # "marginal",
  # "stat_effect_size",
  "repo_check",
  "code_check"
  # "stat_check",
  # "ref_doi_check",
  # "ref_accuracy",
  # "ref_replication",
  # "ref_retraction",
  # "ref_pubpeer",
  # "ref_summary"
)

# paper <- psychsci$`0956797621991137`
# mo <- module_run(paper, "power")


# generate reports for a sample of n papers
n <- 1
output <- "html"
paper <- sample(psychsci, n)
#paper <- psychsci$`09567976241260247` # has two different links to same repo
# paper <- read(demoxml())
args <- list(
  doi_check = list(crossref_min_score = 50)
)

rep <- report(
  paper,
  modules = modules,
  output_file = paste0(dir, "/_.", output),
  output_format = output,
  args = args
)

files <- attr(rep, "save_path")

# open all files in web browser
sapply(files, browseURL)
