# Look up Reference in CrossRef

Look up Reference in CrossRef

## Usage

``` r
crossref_query(
  ref,
  min_score = 50,
  rows = 1,
  select = c("DOI", "score", "type", "title", "author", "container-title", "published",
    "volume", "issue", "page", "URL", "abstract")
)
```

## Arguments

- ref:

  the full text reference of the paper to get info for, see Details

- min_score:

  minimal score that is taken to be a reliable match (default 50)

- rows:

  the maximum number of rows to return per reference (default 1)

- select:

  what fields to select from the crossref API

## Value

doi

## Details

The argument `ref` can take many formats. Crossref queries only look for
authors, title, and container-title (e.g., journal or book), but extra
information doesn't seem to hurt.

- be a text reference or fragment

- a bibentry object (authors, title and container will be extracted)

- a vector of text or bibentry objects

- a paper object (the ref column of the bib table will be extracted)

## Examples

``` r
ref <- paste(
  "Lakens, D., Mesquida, C., Rasti, S., & Ditroilo, M. (2024).",
  "The benefits of preregistration and Registered Reports.",
  "Evidence-Based Toxicology, 2(1)."
)
# \donttest{
cr <- crossref_query(ref)
# }
```
