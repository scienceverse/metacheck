# Create a paperlist object

Create a new paperlist object from individual paper objects or lists of
paper objects

## Usage

``` r
paperlist(..., merge_duplicates = FALSE)
```

## Arguments

- ...:

  scivrs_paper objects or lists of paper objects

- merge_duplicates:

  if duplicates exist, merge them

## Value

An object with class scivrs_paperlist

## Examples

``` r

p1 <- psychsci[[1]]
p2 <- psychsci[[2]]
plist <- paperlist(p1, p2)

merged <- paperlist(psychsci[1:2], psychsci[2:3],
                    merge_duplicates = TRUE)
```
