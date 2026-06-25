# Format Reference

Format a reference for display in a report.

## Usage

``` r
format_ref(bib)
```

## Arguments

- bib:

  a bibentry object or list of bibentry objects

## Value

formatted text

## Details

The argument `bib` should be a bibentry object (e.g., like those made by
[`citation()`](https://rdrr.io/r/utils/citation.html), but it can also
handle a bibtex object or a bibtex formatted character vector. If these
do not read in as valid bibtex, the original text of bib will be
returned unformatted.

## Examples

``` r
mc <- citation("metacheck")
format_ref(mc)
#> [1] "DeBruine L, Mesquida C, Werner J, Lakens D (2026). <em>metacheck: Check Research Outputs for Best Practices</em>. <a href=\"https://doi.org/10.5281/zenodo.20704754\">doi:10.5281/zenodo.20704754</a>, R package version 0.1.0, <a href=\"https://scienceverse.org/metacheck\">https://scienceverse.org/metacheck</a>."

# handles bibtext
bib_mc <- utils::toBibtex(mc)
format_ref(bib_mc)
#> [1] "DeBruine L, Mesquida C, Werner J, Lakens D (2026). <em>metacheck: Check Research Outputs for Best Practices</em>. <a href=\"https://doi.org/10.5281/zenodo.20704754\">doi:10.5281/zenodo.20704754</a>, R package version 0.1.0, <a href=\"https://scienceverse.org/metacheck\">https://scienceverse.org/metacheck</a>."

paper <- demopaper()
format_ref(paper$bib$ref[1:2])
#> [1] "NULL"
```
