# Report from module output

Report from module output

## Usage

``` r
module_report(module_output, header = 3)
```

## Arguments

- module_output:

  the output of a
  [`module_run()`](https://scienceverse.github.io/metacheck/reference/module_run.md)

- header:

  header level (default 2)

## Value

text

## Examples

``` r
paper <- demopaper()
op <- module_run(paper, "stat_p_exact")
module_report(op) |> cat()
#> ### ⚠️ Exact P-Values {#exact-p-values .red}
#> 
#> We found 1 imprecise *p* value out of 3 detected *p* values.
#> 
#> <details><summary>View detailed feedback</summary><div>
#> 
#> Reporting *p* values imprecisely (e.g., *p* < .05) reduces transparency, reproducibility, and re-use (e.g., in *p* value meta-analyses). Best practice is to report exact p-values with three decimal places (e.g., *p* = .032) unless *p* values are smaller than 0.001, in which case you can use *p* < .001.
#> 
#> 
#> ```{r}
#> #| echo: false
#> 
#> 
#> # table data --------------------------------------
#> table <- structure(list("P-Value" = "p > .05", Text = "There was no effect of experience on the reduction in errors when using the tool (p > .05), as the correlation was non-significant (Figure 2)."), row.names = c(NA, 
#> -1L), class = c("tbl_df", "tbl", "data.frame"))
#> 
#> # display table -----------------------------------
#> metacheck::report_table(table, c(0.1, 0.9), 2, FALSE)
#> ```
#> 
#> 
#> ::: {.callout-tip title="Learn More" collapse="true"}
#> 
#> The APA manual states: Report exact *p* values (e.g., *p* = .031) to two or three decimal places. However, report *p* values less than .001 as *p* < .001. However, 2 decimals is too imprecise for many use-cases (e.g., a *p* value meta-analysis), so report *p* values with three digits.
#> 
#> American Psychological Association (2020). <em>Publication manual of the American Psychological Association</em>, 7 edition. American Psychological Association.
#> 
#> :::
#> 
#> 
#> </div></details>
#> 
#> ::: {.callout-note title="How It Works" collapse="true"}
#> 
#> List any p-values reported with insufficient precision (e.g., p < .05 or p = n.s.) or reported as exactly zero (e.g., p = .000).
#> 
#> This module uses regular expressions to identify p-values. It will flag any values reported as p > ? or p < numbers greater than .001. It will also flag p-values reported as exactly zero (e.g., p = .000, p = 0.00), which are mathematically impossible — p-values are never exactly zero and should instead be reported as p < .001.
#> 
#> We try to exclude figure and table notes like "* p < .05", but may not succeed at excluding all false positives.
#> 
#> This module was developed by Lisa DeBruine
#> 
#> :::
#> 
#> 
#> <p class='validation'>**Validation**: In a sample of 225 papers containing 405 instances of non-exact p-values, the module correctly detected 269 cases (true positives) and incorrectly identified 78 (false positives). It missed 136 instances of imprecisely reported p-values (false negatives) and correctly identified 4557 cases of precisely reported p-values (true negative). Additionally, 78% of positive detections were correct (positive predictive value).</p>
```
