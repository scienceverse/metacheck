# run_validation_gui.R
# Launch the validation GUI for ground-truth labelling.
# Run from data_check/:  Rscript runners/run_validation_gui.R
#                        — or —
#                        source("runners/run_validation_gui.R")

app_dir <- file.path(dirname(sys.frame(1)$ofile), "..", "tools", "validation_gui")
shiny::runApp(normalizePath(app_dir), launch.browser = FALSE)
