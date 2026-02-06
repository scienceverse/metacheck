# Launch Shiny App

Create a meta-study file interactively in a shiny app that runs locally
in RStudio or your web browser (recommended). It does not connect to the
web at all, so your data are completely private.

## Usage

``` r
metacheck_app(study = NULL, quiet = FALSE, ...)
```

## Arguments

- study:

  optional study to load

- quiet:

  whether to show the debugging messages in the console

- ...:

  arguments to pass to shiny::runApp

## Value

A study object created or edited by the app

## Examples

``` r
if (FALSE) { # \dontrun{
s <- metacheck_app()
} # }
```
