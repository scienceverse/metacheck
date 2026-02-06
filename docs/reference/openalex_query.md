# Look up a reference in OpenAlex

Look up a reference in OpenAlex

## Usage

``` r
openalex_query(title, source = NA, authors = NA, strict = TRUE)
```

## Arguments

- title:

  The title of the work

- source:

  The source (journal or book)

- authors:

  The authors

- strict:

  Whether to return NULL or the best match if there isn't a single match

## Value

A data frame with citation info

## Examples

``` r
if (FALSE) { # \dontrun{
openalex_query("Sample Size Justification", "Collabra Psychology")
} # }
```
