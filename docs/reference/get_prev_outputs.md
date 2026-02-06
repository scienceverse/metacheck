# Get Previous Outputs

A helper for creating modules. Checks for previous module outputs in a
chain and returns the named list item if it exists in any parent
environment.

## Usage

``` r
get_prev_outputs(module, item, parent_n = 2)
```

## Arguments

- module:

  the name of a previously run module

- item:

  the name of the list item to extract

- parent_n:

  the number of parents to traverse up the chain. Noramlly 2 if you are
  calling this from a module function, but maybe more if you are calling
  it from a helper function.

## Value

the extracted list item, or NULL if not found

## Examples

``` r
# .__mc__prev_outputs is usually created by `module_run()`
.__mc__prev_outputs <- list(mod_1 = list(a = 1, b = 2))
f <- function(item) {
  get_prev_outputs("mod_1", item)
}
f("a")
#> [1] 1
f("d")
#> NULL
```
