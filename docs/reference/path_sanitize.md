# Sanitize File Path

Make sure user-input file names are not problematic.

## Usage

``` r
path_sanitize(
  path,
  replacement = "_",
  remove_whitespace = TRUE,
  keep_sep = TRUE
)
```

## Arguments

- path:

  the path to sanitize (can be a vector of paths)

- replacement:

  the character to replace invalid characters with

- remove_whitespace:

  whether to include whitespace as a problem

- keep_sep:

  whether to keep the path separator /

## Value

the sanitized vector

## Examples

``` r
path <- "/My Files/x><y.pdf"
path_sanitize(path)
#> [1] "/My_Files/x_y.pdf"
path_sanitize(path, replacement = "~")
#> [1] "/My~Files/x~y.pdf"
path_sanitize(path, remove_whitespace = FALSE)
#> [1] "/My Files/x_y.pdf"
path_sanitize(path, keep_sep = FALSE)
#> [1] "_My_Files_x_y.pdf"
```
