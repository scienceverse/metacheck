# Expand a JSON column

It is useful to ask an LLM to return data in JSON structured format, but
can be frustrating to extract the data, especially where the LLM makes
syntax mistakes. This function tries to expand a column with a
JSON-formatted response into columns and deals with it gracefully (sets
an 'error' column to "parsing error") if there are errors. It also fixes
column data types, if possible.

## Usage

``` r
json_expand(table, col = "answer", suffix = c("", ".json"))
```

## Arguments

- table:

  the table with a column to expand

- col:

  the name or index of the column to expand (defaults to "answer" or the
  first column)

- suffix:

  the suffix for the extracted columns if they conflict with names in
  the table

## Value

the table plus the expanded columns

## Examples

``` r
table <- data.frame(
  id = 1:5,
  answer = c(
    '{"number": "1", "letter": "A", "bool": true}',
    '{"number": "2", "letter": "B", "bool": "FALSE"}',
    '{"number": "3", "letter": "", "bool": null}',
    "oh no, the LLM misunderstood",
    '{"number": "5", "letter": ["E", "F"], "bool": false}'
  )
)

expanded <- json_expand(table, "answer")
expanded
#>   id                                               answer number letter  bool
#> 1  1         {"number": "1", "letter": "A", "bool": true}      1      A  TRUE
#> 2  2      {"number": "2", "letter": "B", "bool": "FALSE"}      2      B FALSE
#> 3  3          {"number": "3", "letter": "", "bool": null}      3           NA
#> 4  4                         oh no, the LLM misunderstood     NA   <NA>    NA
#> 5  5 {"number": "5", "letter": ["E", "F"], "bool": false}      5    E;F FALSE
#>           error
#> 1          <NA>
#> 2          <NA>
#> 3          <NA>
#> 4 parsing error
#> 5          <NA>
```
