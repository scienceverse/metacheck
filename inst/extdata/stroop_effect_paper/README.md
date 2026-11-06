# Stroop effect reproducibility assignment

A small, fully reproducible analysis of a classroom Stroop-task dataset,
used to teach an open and reproducible data analysis workflow. Students in
an introductory psychology course named colors in congruent trials (e.g.,
the word "red" printed in red) and incongruent trials (e.g., "red" printed
in green); the time to name all words in each block was self-reported in
seconds. The analysis tests whether there is a Stroop effect.

This repository is a Quarto port of
[Lakens/reproducibility_assignment](https://github.com/Lakens/reproducibility_assignment),
and is organized to follow the best practices described in
[Lakens, Mesquida & DeBruine, "Best Practices for Machine Readable Data and
Code Sharing in Psychology"](https://github.com/scienceverse/ms_FAIR_data_and_code).

## Contents

| Folder | Contents |
|---|---|
| `data/` | `stroop_data.csv`, the cleaned, comma-separated analysis dataset; `raw/stroop_raw.csv`, the original download, saved as a comma-separated file with its values unchanged |
| `code/` | `clean_data.R`, which converts the raw data into `data/stroop_data.csv`; `analysis.qmd`, the Quarto manuscript that reads `data/stroop_data.csv` and reports the results |
| `documentation/` | `stroop_data_codebook.csv`, describing every column in `data/stroop_data.csv` |

`dataset_description.json`, at the repository root, is a machine-readable
summary of the dataset (Schema.org `Dataset`), independent of this README.
`renv.lock` pins the exact R package versions this analysis was last run
with (see [Reproducing the analysis](#reproducing-the-analysis) below).

## Reproducing the analysis

1. Open this repository in R by setting it as the working directory. This
   project uses
   [renv](https://rstudio.github.io/renv/) to pin package versions
   (`renv.lock`); on first opening the project, run `renv::restore()` to
   install the exact package versions this analysis was last run with.
2. `Rscript clean_data.R`, run from the `code/` folder, regenerates
   `data/stroop_data.csv` from
   `data/raw/stroop_raw.csv` (already included, so this step is optional
   unless you want to re-run the conversion yourself).
3. Render `code/analysis.qmd` with Quarto to reproduce the manuscript as an
   HTML file:

   ```sh
   quarto render code/analysis.qmd
   ```

Required R packages (pinned in `renv.lock`): `ggplot2`, `reshape2`, `pwr`, `effectsize`.

## License

MIT License — see `LICENSE`.
