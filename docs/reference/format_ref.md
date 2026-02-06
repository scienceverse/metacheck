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
#> [1] "DeBruine L, Lakens D (2025). <em>metacheck: Check Research Outputs for Best Practices</em>. R package version 0.0.0.9068, <a href=\"https://github.com/scienceverse/metacheck\">https://github.com/scienceverse/metacheck</a>."

# handles bibtext
bib_mc <- utils::toBibtex(mc)
format_ref(bib_mc)
#> [1] "DeBruine L, Lakens D (2025). <em>metacheck: Check Research Outputs for Best Practices</em>. R package version 0.0.0.9068, <a href=\"https://github.com/scienceverse/metacheck\">https://github.com/scienceverse/metacheck</a>."

paper <- read(demoxml())
format_ref(paper$bib$ref[1:2])
#> [1] "Gangestad SW, Thornhill R (1998). &ldquo;Menstrual cycle variation in women's preferences for the scent of symmetrical men.&rdquo; <em>Proceedings Biological Sciences</em>, <b>22</b>, 927-933. <a href=\"https://doi.org/10.1098/rspb.1998.0380\">doi:10.1098/rspb.1998.0380</a>."
#> [2] "Gino F, Wiltermuth SS (2014). &ldquo;Evil Genius? How Dishonesty Can Lead to Greater Creativity.&rdquo; <em>Psychological Science</em>, <b>25</b>(4), 973-981. <a href=\"https://doi.org/10.1177/0956797614520714\">doi:10.1177/0956797614520714</a>."                              
```
