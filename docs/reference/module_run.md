# Run a module

Run a module

## Usage

``` r
module_run(paper, module, ...)
```

## Arguments

- paper:

  a paper object or a list of paper objects

- module:

  the name of a module or path to a module to run on this object

- ...:

  further arguments to the module (e.g., arguments for the
  [`llm()`](https://scienceverse.github.io/metacheck/reference/llm.md)
  function like `params`); these will override any arguments in the
  module

## Value

a list of the returned table and report text

## Examples

``` r
module_run(psychsci[[1]], "all_p_values")
#> List All P-Values: We found 6 p-values
```
