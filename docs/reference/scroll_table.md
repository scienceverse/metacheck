# Make Scroll Table

A helper function for making module reports.

## Usage

``` r
scroll_table(
  table,
  colwidths = "auto",
  maxrows = 2,
  escape = FALSE,
  column = "body"
)
```

## Arguments

- table:

  the data frame to show in a table, or a vector for a list

- colwidths:

  set column widths as a vector of px (number \> 1) or percent (numbers
  \<= 1)

- maxrows:

  if the table has more rows than this, paginate

- escape:

  whether or not to escape the DT (necessary if using raw html)

- column:

  which quarto column to show tables in

## Value

the markdown R chunk to create this table

## Details

See [quarto article
layout](https://quarto.org/docs/authoring/article-layout.html) for
column options. The most common are "body" (centre column), "page" (span
all columns"), and "margin" (only in right margin).

To set colwidths, use a numeric or character vector. For a numeric
vector, numbers greater than 1 will be interpreted as pixels, less than
1 as percents. Character vectors will be passed as is (e.g., "3em"). If
you only want to specify some columns, set the others to NA, like c(200,
NA, 200, NA).

## Examples

``` r
scroll_table(LETTERS)
#> [1] "\n```{r}\n#| echo: false\n\n\n# table data --------------------------------------\ntable <- structure(list(c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\", \"G\", \"H\", \"I\", \n\"J\", \"K\", \"L\", \"M\", \"N\", \"O\", \"P\", \"Q\", \"R\", \"S\", \"T\", \"U\", \"V\", \n\"W\", \"X\", \"Y\", \"Z\")), names = \"\", class = \"data.frame\", row.names = c(NA, \n-26L))\n\n# display table -----------------------------------\nmetacheck::report_table(table, \"auto\", 2, FALSE)\n```\n"
```
