# CrossRef Info from DOI

Valid selects for crossref API are:

## Usage

``` r
crossref_doi(
  doi,
  select = c("DOI", "type", "title", "container-title", "volume", "issue", "page", "URL",
    "abstract", "year", "error")
)
```

## Arguments

- doi:

  the DOI of the paper to get info for

- select:

  what fields to select from the crossref API

## Value

data frame with DOIs and info

## Details

abstract, URL, resource, member, posted, score, created, degree,
update-policy, short-title, license, ISSN, container-title, issued,
update-to, issue, prefix, approved, indexed, article-number,
clinical-trial-number, accepted, author, group-title, DOI,
is-referenced-by-count, updated-by, event, chair, standards-body,
original-title, funder, translator, published, archive, published-print,
alternative-id, subject, subtitle, published-online, publisher-location,
content-domain, reference, title, link, type, publisher, volume,
references-count, ISBN, issn-type, assertion, deposited, page,
content-created, short-container-title, relation, editor

## Examples

``` r
doi <- "10.7717/peerj.4375"
if (FALSE) { # \dontrun{
# cr_info <- crossref_doi(doi)
} # }
```
