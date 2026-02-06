# Get Person Details for ORCiD

Get Person Details for ORCiD

## Usage

``` r
orcid_person(orcid)
```

## Arguments

- orcid:

  a vector of ORCiDs

## Value

A data frame of details

## Examples

``` r
# \donttest{
orcids <- c("0000-0002-0247-239X", "0000-0002-7523-5539")
orcid_person(orcids)
#> # A tibble: 2 × 7
#>   orcid               given  family   email     country keywords  urls     
#>   <chr>               <chr>  <chr>    <list>    <chr>   <list>    <list>   
#> 1 0000-0002-0247-239X Daniel Lakens   <chr [1]> NL      <chr [4]> <chr [2]>
#> 2 0000-0002-7523-5539 Lisa   DeBruine <chr [2]> GB      <chr [9]> <chr [1]>
# }
```
