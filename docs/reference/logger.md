# Log messages

Adds a logging message to the log. Keeps the log as a maximum of 1000
rows.

## Usage

``` r
logger(label = "", contents = list(), logpath = NULL)
```

## Arguments

- label:

  a string with the context (e.g.,module name)

- contents:

  a named list of the log contents

- logpath:

  an optional file path to save the log in

## Value

called for side effects of writing to log, returns logpath

## Examples

``` r
logpath <- tempfile(fileext = ".log")
logger("test", list(x = 1), logpath)
lastlog()
#> $label
#> [1] "test"
#> 
#> $dt
#> [1] "2026-06-25 06:05:03"
#> 
#> $msg
#> [1] "hi again"
#> 
```
