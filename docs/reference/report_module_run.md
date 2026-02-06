# Run modules for a report

Runs modules in order on the paper and orders by section and traffic
light.

## Usage

``` r
report_module_run(paper, modules, args = list())
```

## Arguments

- paper:

  a paper object

- modules:

  a vector of modules to run

- args:

  optional list of arguments to pass to modules

## Value

a list of module outputs

## Details

Pass arguments to modules in a named list of lists, using the same names
as the `modules` argument. You only need to specify modules with
arguments.

    args <- list(power = list(seed = 8675309))

## Examples

``` r
paper <- read(demoxml())
modules <- c("stat_p_exact", "stat_p_nonsig")
module_output <- report_module_run(paper, modules)
```
