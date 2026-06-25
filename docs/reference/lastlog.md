# Get the last log

Get the last log

## Usage

``` r
lastlog(i = 1, logpath = NULL)
```

## Arguments

- i:

  the indices to return

- logpath:

  an optional file path to read the log from

## Value

a list of the last log item, or a data frame of multiple items

## Examples

``` r
# set up 2 log items
logger("test", list(msg = "hi"))
logger("test", list(msg = "hi again"))

lastlog()
#> $label
#> [1] "test"
#> 
#> $dt
#> [1] "2026-06-25 06:05:03"
#> 
#> $msg
#> [1] "hi again"
#> 
lastlog(2)
#> $label
#> [1] "test"
#> 
#> $dt
#> [1] "2026-06-25 06:05:02"
#> 
#> $msg
#> [1] "hi"
#> 
lastlog(1:2)
#> # A tibble: 2 × 3
#>   label dt                  msg     
#>   <chr> <chr>               <chr>   
#> 1 test  2026-06-25 06:05:03 hi again
#> 2 test  2026-06-25 06:05:02 hi      
```
