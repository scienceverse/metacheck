# Checking Statistical Reporting

``` r

library(metacheck)
#> 
#> 
#> ***********************************************
#> ✅ Welcome to metacheck beta version 0.1.0
#> ✨ Your version newer than the current release
#> 
#> ℹ For support and examples visit:
#> https://scienceverse.github.io/metacheck/
#> 
#> ⚠️ Set an email to use APIs like OpenAlex
#> metacheck::email('your@address.org')
#> 
#> 🧪 This is beta software; please check any
#> results. Check module validation info for
#> false positive and negative rates.
#> ***********************************************
#> 
#> Attaching package: 'metacheck'
#> The following object is masked from 'package:base':
#> 
#>     message
```

## Journal Article Reporting Standards

The Journal Article Reporting Standards (JARS) developed by the American
Psychological Association provide guidance on what information should be
reported in scientific articles to support transparency and
reproducibility \[@appelbaum2018\]. Almost every published article would
improve if researchers diligently followed JARS, but the guidelines are
not widely known, rarely enforced by journals, and time-consuming to
check manually. Automation can help.

Metacheck provides five modules for checking statistical reporting
quality. This vignette demonstrates each one using papers from the
`psychsci` built-in dataset — 250 open-access articles published in
*Psychological Science* between 2004 and 2014.

| Module | What it checks |
|----|----|
| `stat_p_exact` | Flags imprecise p-values (e.g., *p* \< .05 or *p* = .000) |
| `stat_p_nonsig` | Flags non-significant p-values that may be misinterpreted |
| `stat_effect_size` | Checks whether t-tests and F-tests include an effect size |
| `stat_check` | Checks that p-values are mathematically consistent with the reported test statistic |
| `marginal` | Detects language like “marginally significant” or “approaching significance” |

All modules return a list with `$traffic_light` (“green”, “yellow”,
“red”, or “na”), `$summary_text`, and `$table` with detailed results.

------------------------------------------------------------------------

## A paper where everything looks fine

We start with a paper that passes all checks. Paper `0956797613520608`
has 6 detected p-values, all exact, all t-tests and F-tests paired with
effect sizes, and no consistency errors flagged by StatCheck.

``` r

paper_clean <- psychsci[["0956797613520608"]]
```

``` r

module_run(paper_clean, "all_p_values")$summary_text
#> [1] "We found 6 p-values"
```

``` r

module_run(paper_clean, "stat_p_exact")$traffic_light
#> [1] "green"
```

``` r

module_run(paper_clean, "stat_effect_size")$traffic_light
#> [1] "red"
```

``` r

module_run(paper_clean, "stat_check")$traffic_light
#> [1] "green"
```

A green result means the module found no issues. This does not guarantee
the paper is free of reporting problems — the modules use regular
expressions and have known false negative rates — but it is a good sign.

------------------------------------------------------------------------

## Exact p-values (`stat_p_exact`)

The APA Manual states:

> Report exact p values (e.g., *p* = .031) to two or three decimal
> places. However, report p values less than .001 as *p* \< .001.

Exact p-values matter because they allow readers to include results in
p-curve or z-curve analyses \[@simonsohn2014; @bartos2020\], and they
enable tools like StatCheck to verify internal consistency. The
`stat_p_exact` module flags any p-value reported as *p* \< some
threshold greater than .001 (e.g., *p* \< .05) or as exactly zero (e.g.,
*p* = .000, which is mathematically impossible).

Paper `0956797619842261` has 60 detected p-values, 59 of which are
exact. One is imprecise:

``` r

paper_imprecise <- psychsci[["0956797619842261"]]
result_exact <- module_run(paper_imprecise, "stat_p_exact")

result_exact$traffic_light
#> [1] "red"
result_exact$summary_text
#> [1] "We found 1 imprecise *p* value out of 60 detected *p* values."
```

``` r

# The flagged p-value and its context
result_exact$table[result_exact$table$p_comp != "=", c("text", "p_comp", "p_value", "expanded")]
#> # A tibble: 13 × 4
#>    text     p_comp p_value expanded                                             
#>    <chr>    <chr>    <dbl> <chr>                                                
#>  1 p < .001 <        0.001 A Block (after first acquisition vs. after second ac…
#>  2 p < .001 <        0.001 Importantly, after the first and second acquisition …
#>  3 p < .001 <        0.001 As shown in Figure 3, participants rated faces that …
#>  4 p < .05  <        0.05  Error bars show repeated measures standard errors of…
#>  5 p < .001 <        0.001 Error bars show repeated measures standard errors of…
#>  6 p < .001 <        0.001 The Block × Cue Type ANOVA on the unpleasantness rat…
#>  7 p < .001 <        0.001 The CS Type × Trial ANOVA revealed a main effect of …
#>  8 p < .001 <        0.001 The CS Type × Trial ANOVA for the startle responses …
#>  9 p < .05  <        0.05  Error bars in (a) and (b) show repeated measures sta…
#> 10 p < .001 <        0.001 Error bars in (a) and (b) show repeated measures sta…
#> 11 p < .05  <        0.05  Error bars show repeated measures standard errors of…
#> 12 p < .001 <        0.001 Error bars show repeated measures standard errors of…
#> 13 p < .05  <        0.05  Asterisks above the lines indicate significant diffe…
```

The module returns the sentence containing the imprecise p-value
together with the surrounding sentence (the `expanded` column) to help
assess the context.

------------------------------------------------------------------------

## Missing effect sizes (`stat_effect_size`)

Reporting an effect size alongside every test result is a core JARS
requirement. It helps readers assess practical significance and supports
meta-analyses. The `stat_effect_size` module searches for t-tests and
F-tests reported in APA format and checks whether an effect size metric
(Cohen’s *d*, *r*, partial η², *f*, etc.) appears in the same sentence.

The module has been validated on papers from Psychological Science: it
detected 100% of t-tests with effect sizes, 99% of t-tests without, 99%
of F-tests with, and 100% of F-tests without.

The same paper as above (`0956797619842261`) also has one test without
an effect size:

``` r

result_es <- module_run(paper_imprecise, "stat_effect_size")

result_es$traffic_light
#> [1] "red"
result_es$summary_text
#> [1] "We found 2 t-tests and/or F-tests where effect sizes are not reported. Check these tests in the table below, and consider adding effect sizes"
```

``` r

# The test that is missing an effect size
result_es$table[result_es$table$es == FALSE, c("text", "test")]
#>      text test
#> NA   <NA> <NA>
#> NA.1 <NA> <NA>
```

A clean paper for comparison — all tests have effect sizes:

``` r

module_run(paper_clean, "stat_effect_size")$summary_text
#> [1] "All effect sizes were reported, but some appear inconsistent with the test statistic. This can be because the effect size is not clearly labeled (e.g., d, instead of d_rm), because the effect sizes is not reported with enough precisions (e.g., 0.3 instead of 0.32), or because the effect size is incorrectly reported."
```

------------------------------------------------------------------------

## Non-significant p-values (`stat_p_nonsig`)

Non-significant results are frequently misinterpreted as evidence that
an effect is absent, when in fact they only indicate that the data were
insufficient to reject the null hypothesis. The correct approach is to
perform an equivalence test against a smallest effect size of interest
\[@lakens2018\].

The `stat_p_nonsig` module returns all sentences containing
non-significant p-values (p ≥ .05). It does not automatically judge the
interpretation — it surfaces the sentences so researchers can review
them. Paper `0956797618772822` has 28 such sentences:

``` r

paper_nonsig <- psychsci[["0956797618772822"]]
result_nonsig <- module_run(paper_nonsig, "stat_p_nonsig")

result_nonsig$traffic_light
#> [1] "yellow"
result_nonsig$summary_text
#> [1] "We found 28 non-significant p values that should be checked for appropriate interpretation."
```

``` r

# A sample of sentences with non-significant p-values
head(result_nonsig$table[, c("text", "p_value")], 3)
#> # A tibble: 3 × 2
#>   text        p_value
#>   <chr>         <dbl>
#> 1 "p = .673"    0.673
#> 2 "p = .996"    0.996
#> 3 "p = .939 "   0.939
```

------------------------------------------------------------------------

## Marginal significance (`marginal`)

Describing a result as “marginally significant”, “approaching
significance”, or “trending towards significance” implies a gradient of
evidence where none exists under null hypothesis significance testing.
The `marginal` module detects these phrases.

The same paper `0956797619842261` contains two such sentences:

``` r

result_marginal <- module_run(paper_imprecise, "marginal")

result_marginal$traffic_light
#> [1] "red"
result_marginal$summary_text
#> [1] "You described 2 effects with terms related to 'marginally significant'."
```

``` r

result_marginal$table[, "text"]
#> # A tibble: 2 × 1
#>   text                                                                          
#>   <chr>                                                                         
#> 1 The Block × CS Type ANOVA on D1 for acquisition revealed a marginally signifi…
#> 2 At the cardiac level, the aversive CS+ evoked more cardiac deceleration compa…
```

------------------------------------------------------------------------

## Statistical consistency (StatCheck, `stat_check`)

StatCheck \[@nuijten2016\] recalculates the p-value from the reported
test statistic and degrees of freedom and checks whether the reported
p-value is consistent with the recalculation. It supports t-tests and
F-tests in APA format.

**Important caveat:** StatCheck has a known false positive rate of
approximately 43% — that is, roughly 43 out of every 100 flagged tests
are not actually errors. Use the module to identify tests that warrant a
closer look, not as definitive proof of an error. See the [StatCheck
preprint](https://osf.io/preprints/psyarxiv/tcxaj_v1/) for full
validation details.

The clean paper returns green:

``` r

module_run(paper_clean, "stat_check")$summary_text
#> [1] "We detected no errors in t-tests or F-tests."
```

Paper `09567976231223130` triggers multiple flags:

``` r

paper_statcheck <- psychsci[["09567976231223130"]]
result_sc <- module_run(paper_statcheck, "stat_check")

result_sc$traffic_light
#> [1] "red"
result_sc$summary_text
#> [1] "20 possible errors in t-tests or F-tests"
```

``` r

# Rows where an error was flagged — verify manually before concluding anything
errors <- result_sc$table[result_sc$table$error == TRUE,
                          c("raw", "reported_p", "computed_p", "decision_error")]
head(errors, 3)
#>                        raw reported_p computed_p decision_error
#> 4   t(116) = 2.28, p = .16       0.16 0.02443491           TRUE
#> 5   t(116) = .574, p = .98       0.98 0.56707913          FALSE
#> 6 t(116) = -1.712, p = .43       0.43 0.08956887          FALSE
```

The `decision_error` column is `TRUE` when the inconsistency changes the
significance conclusion (i.e., the test would be non-significant if the
computed p-value were used instead of the reported one, or vice versa).
These are higher-priority cases to check manually.

------------------------------------------------------------------------

## Running all modules at once

You can run multiple modules in sequence by passing the output of one
module as the input to the next. The `summary_table` accumulates across
modules:

``` r

result <- psychsci[["0956797619842261"]] |>
  module_run("stat_p_exact") |>
  module_run("stat_effect_size") |>
  module_run("stat_check") |>
  module_run("marginal") |>
  module_run("stat_p_nonsig")

result$summary_table
```

------------------------------------------------------------------------

## References

Appelbaum, M., Cooper, H., Kline, R. B., Mayo-Wilson, E., Nezu, A. M., &
Rao, S. M. (2018). Journal article reporting standards for quantitative
research in psychology. *American Psychologist*, *73*(1), 3–25.
<https://doi.org/10.1037/amp0000191>

Bartoš, F., & Schimmack, U. (2020). Z-curve 2.0: Estimating replication
rates and discovery rates. *Meta-Psychology*.
<https://doi.org/10.15626/MP.2021.2720>

Lakens, D. (2018). Equivalence tests: A practical primer for t-tests,
correlations, and meta-analyses. *Social Psychological and Personality
Science*, *8*(4), 355–362. <https://doi.org/10.1177/1948550617697177>

Nuijten, M. B., Hartgerink, C. H. J., van Assen, M. A. L. M., Epskamp,
S., & Wicherts, J. M. (2016). The prevalence of statistical reporting
errors in psychology (1985–2013). *Behavior Research Methods*, *48*(4),
1205–1226. <https://doi.org/10.3758/s13428-015-0664-2>

Simonsohn, U., Nelson, L. D., & Simmons, J. P. (2014). p-Curve and
effect size. *Perspectives on Psychological Science*, *9*(6), 666–681.
<https://doi.org/10.1177/1745691614553988>
