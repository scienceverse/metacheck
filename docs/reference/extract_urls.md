# Extract URLs

Get a table of URLs from a paper or paperlist. Matches urls that start
with http or doi:

## Usage

``` r
extract_urls(paper)
```

## Arguments

- paper:

  a paper object or paperlist object

## Value

a table

## Examples

``` r
paper <- read(demoxml())
urls <- extract_urls(paper)
```
