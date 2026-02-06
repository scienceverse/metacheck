# Create a Report

Run specified modules on a paper and generate a report in quarto (qmd),
html, or pdf format.

## Usage

``` r
report(
  paper,
  modules = c("prereg_check", "funding_check", "coi_check", "power", "repo_check",
    "code_check", "stat_check", "stat_p_exact", "stat_p_nonsig", "stat_effect_size",
    "marginal", "ref_doi_check", "ref_replication", "ref_retraction", "ref_pubpeer",
    "ref_summary"),
  output_file = paste0(paper$id, "_report.", output_format),
  output_format = c("html", "qmd"),
  args = list()
)
```

## Arguments

- paper:

  a paper object or a paperlist object

- modules:

  a vector of modules to run (names for built-in modules or paths for
  custom modules)

- output_file:

  the name of the output file

- output_format:

  the format to create the report in

- args:

  a list of arguments to pass to modules (see Details)

## Value

the file path the report is saved to

## Details

Pass arguments to modules in a named list of lists, using the same names
as the `modules` argument. You only need to specify modules with
arguments.

    args <- list(power = list(seed = 8675309))

## Examples

``` r
if (FALSE) { # \dontrun{
filename <- demoxml()
paper <- read(filename)
report(paper)
} # }
```
