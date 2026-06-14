# This script generates inst/extdata/regcheck_examples.rda, which contains
# pre-computed reg_check results used by vignettes/regcheck.Rmd to show real
# output without making live API calls during vignette build.
#
# Run it manually whenever the saved results need to be refreshed (e.g. after
# changes to the reg_check module or the RegCheck API). Requires:
#   - REGCHECK_API_TOKEN set in .Renviron
#   - A local Ollama + RegCheck server running at http://localhost:8000

devtools::load_all()

# Paper with AsPredicted preregistration (sn9xs)
paper_ap <- psychsci[["09567976231223130"]]

# Paper with OSF preregistration (7qcxa)
paper_osf <- psychsci[["0956797620904990"]]

# --- Hosted run: AsPredicted paper (client = "openai") -----------------------
message("Running reg_check (hosted/openai) on AsPredicted paper...")
check_ap <- paper_ap |>
  module_run("prereg_check") |>
  module_run("reg_check", client = "openai")

# --- Hosted run: OSF paper (client = "openai") -------------------------------
message("Running reg_check (hosted/openai) on OSF paper...")
check_osf <- paper_osf |>
  module_run("prereg_check") |>
  module_run("reg_check", client = "openai")

# --- Local run: AsPredicted paper (client = "ollama") ------------------------
message("Running reg_check (local/ollama) on AsPredicted paper...")
check_local <- paper_ap |>
  module_run("prereg_check") |>
  module_run("reg_check", client = "ollama")

save(check_ap, check_osf, check_local,
     file = "inst/extdata/regcheck_examples.rda",
     compress = "xz")

message("Saved inst/extdata/regcheck_examples.rda")
