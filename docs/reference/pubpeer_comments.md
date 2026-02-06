# Get Pubpeer Comments

Takes a DOI, and retrieves information from pubpeer related to
post-publication peer review comments.

## Usage

``` r
pubpeer_comments(doi)
```

## Arguments

- doi:

  a vector of paper DOIs

## Value

a dataframe with information from pubpeer

## Examples

``` r
doi <- c(
  "10.1038/s41598-025-24662-9",
  "10.1177/0146167211398138"
)
pubpeer_comments(doi)
#>                          doi total_comments
#> 1 10.1038/s41598-025-24662-9             18
#> 2   10.1177/0146167211398138              1
#>                                                               url
#> 1 https://pubpeer.com/publications/E1A0A9AC672336E92673154575091E
#> 2 https://pubpeer.com/publications/3B7C004A1663E5D1D7FAAA8AF889D8
#>                                                                                                                                                                                                                                                                                                                              users
#> 1 Actinopolyspora Biskrensis, Nerita Vitiensis, Desmococcus Antarctica, Phanerotoma Behriae, Matt Spick, Castanopsis Indica, Hymenoptychis Sordida, Pandanus Alveolatus, Cosmozosteria Musgravei, Felicia Namaquana, Yucca Louisianensis, Catasetum Purum, Hecatesia Thyridion, Ascorhynchus Japonicus, Cortinarius Austrolimonius
#> 2                                                                                                                                                                                                                                                                                                         Gyliotrachela Transitans
```
