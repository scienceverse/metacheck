# Get A list of preprints from the OSF

Get A list of preprints from the OSF

## Usage

``` r
osf_preprint_list(
  provider = NULL,
  date_created = NULL,
  date_modified = NULL,
  page_start = 1,
  page_end = page_start
)
```

## Arguments

- provider:

  a vector of the preprint providers, e.g. psyarxiv, socarxiv, edarxiv
  (see <https://osf.io/preprints/discover>)

- date_created:

  a single date or a vector of two date (min and max)

- date_modified:

  a single date or a vector of two date (min and max)

- page_start:

  the first page of 10 entries

- page_end:

  the last page of 10 entires to read

## Value

a table of preprint info

## Examples

``` r
# \donttest{
dc <- c("2025-09-01", "2025-10-01")
pp <- osf_preprint_list("psyarxiv", date_created = dc)
files <- pp$primary_file
# }
```
