# Retrieve info from the OSF by ID

Retrieve info from the OSF by ID

## Usage

``` r
osf_retrieve(osf_url, id_col = 1, recursive = FALSE, find_project = FALSE)
```

## Arguments

- osf_url:

  an OSF ID or URL, or a table containing them

- id_col:

  the index or name of the column that contains OSF IDs or URLs, if id
  is a table

- recursive:

  whether to retrieve all children

- find_project:

  DEPRECATED always TRUE now - find the top-level project associated
  with a file (adds 1+ API calls)

## Value

a data frame of information

## Examples

``` r
if (FALSE) { # \dontrun{
# get info on one OSF node
osf_retrieve("pngda")

# also get child nodes and files, and parent project
osf_retrieve("https://osf.io/6nt4v", TRUE, TRUE)
} # }
```
