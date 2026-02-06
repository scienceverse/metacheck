# Get ORCiD from Name

Get ORCiD from Name

## Usage

``` r
get_orcid(family, given = "*")
```

## Arguments

- family:

  The family (last) name to search for

- given:

  An optional given (first) name to search for. Initials will be
  converted from, e.g., L M to L\\ M\\

## Value

A vector of matching ORCiDs

## Examples

``` r
# \donttest{
get_orcid("DeBruine", "Lisa")
#> [1] "0000-0002-7523-5539"
# }
```
