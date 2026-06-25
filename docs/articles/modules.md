# Modules

``` r

devtools::load_all(".")
library(dplyr)
```

Metacheck is designed modularly, so you can add modules to check for
anything. It comes with a set of pre-defined modules, and we hope people
will share more modules.

## Module List

You can see the list of built-in modules with the function below.

``` r

module_list()
```

\*\*\* GENERAL \*\*\*

- all_urls: List all the URLs in the main text.
- coi_check: Identify and extract Conflicts of Interest (COI)
  statements.
- coi_check_oi: Identify and extract Conflicts of Interest (COI)
  statements.
- funding_check: Identify and extract funding statements.
- funding_check_oi: Identify and extract funding statements.
- open_practices: This module searches for open data, code, materials,
  and registration statements.

\*\*\* METHOD \*\*\*

- causal_claims: Aims to identify the presence of random assignment, and
  lists sentences that make causal claims in title or abstract.
- power: This module uses uses regular expressions to identify sentences
  that contain a statistical power analysis. If specified by the user,
  it also uses a large language module (LLM) to extract information
  reported in power analyses, including the statistical test, sample
  size, alpha level, desired level of power, and magnitude and type of
  effect size.
- prereg_check: Retrieve information from preregistrations in a
  standardised way, and make them easier to check.

\*\*\* RESULTS \*\*\*

- all_p_values: List all p-values in the text, returning the matched
  text (e.g., ‘p = 0.04’) and document location in a table.
- code_check: This module retrieves information from repositories
  checked by repo_check about code files (R, SAS, SPSS, Stata).
- marginal: List all sentences that describe an effect as ‘marginally
  significant’.
- repo_check: This module retrieves information from repositories.
- stat_check: Check consistency of p-values and test statistics
- stat_effect_size: The Effect Size module checks if effect sizes are
  correctly reported in t-tests and F-tests.
- stat_p_exact: List any p-values reported with insufficient precision
  (e.g., p \< .05 or p = n.s.) or reported as exactly zero (e.g., p =
  .000).
- stat_p_nonsig: This module checks for imprecisely reported p values.
  If p \> .05 is detected, it warns for misinterpretations.

\*\*\* REFERENCE \*\*\*

- ref_accuracy: This module checks references for mismatches with
  CrossRef.
- ref_consistency: Check if all references are cited and all citations
  are referenced
- ref_miscitation: Check for frequently miscited papers. This module is
  just a proof of concept – the miscite database is not yet populated
  with real examples.
- ref_pubpeer: This module checks references and warns for citations
  that have comments on pubpeer (excluding Statcheck comments).
- ref_replication: This module checks references and warns for citations
  of original studies for which replication or reproduction studies
  exist in the FLoRA database.
- ref_retraction: This module checks references and warns for citations
  in the RetractionWatch Database.
- ref_summary: Summarise information about each reference in a paper.

Use `module_help("module_name")` for help with a specific module

## Module Output

Module designers can include any information in the returned output, but
we suggest they structure it in a specific way to facilitate creating
reports and summarising many papers in a metascientific workflow.

So most modules output a list with the following named items: module,
title, table, report, traffic_light, summary_text, summary_table, paper.
You probably don’t need to worry about any of this unless you are
designing modules or using metacheck for metascience – the
[`report()`](https://scienceverse.github.io/metacheck/reference/report.md)
function takes care of displaying everything for you when you need to
assess a single paper.

``` r

paper <- demopaper()
mo <- module_run(paper, "stat_p_exact")
```

The `module`, `title`, and `summary_text` give brief information.

``` r

mo$module
```

    #> [1] "stat_p_exact"

``` r

mo$title
```

    #> [1] "Exact P-Values"

``` r

mo$summary_text
```

    #> [1] "We found 1 imprecise *p* value out of 3 detected *p* values."

### Traffic light

The `traffic_light` helps the reports give a quick visual guide to where
there are problems or things to check.

``` r

mo$traffic_light
```

    #> [1] "red"

🟢 no problems detected;  
🟡 something to check;  
🔴 possible problems detected;  
🔵 informational only;  
⚪️ not applicable;  
⚫️ check failed

### Table

The `table` is usually a detailed table in the format returned from
[`text_search()`](https://scienceverse.github.io/metacheck/reference/text_search.md)
or
[`text_expand()`](https://scienceverse.github.io/metacheck/reference/text_expand.md),
containing either text relevant to the module, or a classification of
the text. This table can be of use to further modules in a chain, or to
metascientific users.

``` r

mo$table
```

    #> # A tibble: 3 × 14
    #>   text     text_id paragraph_id section_id page_number formatted paper_id header
    #>   <chr>      <int>        <int>      <int>       <int> <chr>     <chr>    <chr> 
    #> 1 p = 0.0…      15            4          3          NA  NA       to_err_… Proce…
    #> 2 p =0.152      16            5          3          NA  NA       to_err_… Proce…
    #> 3 p > .05       17            6          3          NA "There w… to_err_… Proce…
    #> # ℹ 6 more variables: section_type <chr>, p_comp <chr>, p_value <dbl>,
    #> #   expanded <chr>, imprecise <lgl>, zero <lgl>

### Summary Table

The `summary_table` contains a single row for each paper, and must have
an `id` column that matches the paper IDs. It will also have additional
columns that summarise the results of the module. This is mainly useful
in the metascientific workflow, and this table is appended by each
module in a chain.

``` r

mo$summary_table
```

    #>          paper_id n_imprecise n_zero
    #> 1 to_err_is_human           1      0

### Report

The `report` contains a vector of markdown and R code to be inserted
into a report. The display is usually handled by the
[`module_report()`](https://scienceverse.github.io/metacheck/reference/module_report.md)
function inside the
[`report()`](https://scienceverse.github.io/metacheck/reference/report.md)
function.

``` r

mo$report
```

    #> [1] "Reporting *p* values imprecisely (e.g., *p* < .05) reduces transparency, reproducibility, and re-use (e.g., in *p* value meta-analyses). Best practice is to report exact p-values with three decimal places (e.g., *p* = .032) unless *p* values are smaller than 0.001, in which case you can use *p* < .001."                                                                                                                                                                                                                        
    #> [2] "\n```{r}\n#| echo: false\n\n\n# table data --------------------------------------\ntable <- structure(list(\"P-Value\" = \"p > .05\", Text = \"There was no effect of experience on the reduction in errors when using the tool (p > .05), as the correlation was non-significant (Figure 2).\"), row.names = c(NA, \n-1L), class = c(\"tbl_df\", \"tbl\", \"data.frame\"))\n\n# display table -----------------------------------\nmetacheck::report_table(table, c(0.1, 0.9), 2, FALSE)\n```\n"                                       
    #> [3] "::: {.callout-tip title=\"Learn More\" collapse=\"true\"}\n\nThe APA manual states: Report exact *p* values (e.g., *p* = .031) to two or three decimal places. However, report *p* values less than .001 as *p* < .001. However, 2 decimals is too imprecise for many use-cases (e.g., a *p* value meta-analysis), so report *p* values with three digits.\n\nAmerican Psychological Association (2020). <em>Publication manual of the American Psychological Association</em>, 7 edition. American Psychological Association.\n\n:::\n"

### Paper

The `paper` is just the paper argument to
[`module_run()`](https://scienceverse.github.io/metacheck/reference/module_run.md).
This is mainly used when chaining modules.

``` r

mo$paper
```

    #> ---------------
    #> to_err_is_human
    #> ---------------
    #> 
    #> To Err is Human: An Empirical Investigation
    #> 
    #> * Sections: 14
    #> * Sentences: 37
    #> * Bibliography: 5
    #> * X-Refs: 5

### Previous Outputs

If you run modules in a chain or via the
[`report()`](https://scienceverse.github.io/metacheck/reference/report.md)
function, the output accumulates the outputs of previous modules in this
item. This is so some modules can share resource-intensive parts of
checks rather than repeating them.

``` r

mo <- paper |>
  module_run("stat_p_exact") |>
  module_run("marginal") |>
  module_run("stat_effect_size")

mo$prev_outputs
```

    #> $stat_p_exact
    #> Exact P-Values: We found 1 imprecise *p* value out of 3 detected *p* values.
    #> $marginal
    #> Marginal Significance: You described 2 effects with terms related to 'marginally significant'.

## Built-in Modules

Below, we will demonstrate the use of a few built-in modules, first on a
single paper and then a list of papers, the `psychsci` list of 250
open-access papers from Psychological Science.

``` r

paper <- psychsci$`0956797620955209`
```

### all_p_values

List all p-values in the text, returning the matched text (e.g., ‘p =
0.04’) and document location in a table.

``` r

all_p <- module_run(paper, "all_p_values")

all_p$table # print table
```

    #> # A tibble: 20 × 11
    #>    text    text_id paragraph_id section_id page_number formatted paper_id header
    #>    <chr>     <int>        <int>      <int>       <int> <chr>     <chr>    <chr> 
    #>  1 "p = .…      62           15          6          NA  NA       0956797… Quest…
    #>  2 "p = .…      62           15          6          NA  NA       0956797… Quest…
    #>  3 "p < .…     100           24          9          NA "We obse… 0956797… Resul…
    #>  4 "p < .…     101           24          9          NA "The mai… 0956797… Resul…
    #>  5 "p = .…     101           24          9          NA "The mai… 0956797… Resul…
    #>  6 "p = .…     101           24          9          NA "The mai… 0956797… Resul…
    #>  7 "p = .…     102           24          9          NA  NA       0956797… Resul…
    #>  8 "p = .…     102           24          9          NA  NA       0956797… Resul…
    #>  9 "p = .…     103           24          9          NA "We also… 0956797… Resul…
    #> 10 "p =.3…     103           24          9          NA "We also… 0956797… Resul…
    #> 11 "p = .…     106           25          9          NA "Yes-the… 0956797… Resul…
    #> 12 "p <.0…     106           25          9          NA "Yes-the… 0956797… Resul…
    #> 13 "p = .…     108           26          9          NA  NA       0956797… Resul…
    #> 14 "p = .…     108           26          9          NA  NA       0956797… Resul…
    #> 15 "p = .…     108           26          9          NA  NA       0956797… Resul…
    #> 16 "p = .…     108           26          9          NA  NA       0956797… Resul…
    #> 17 "p < .…     109           26          9          NA "Results… 0956797… Resul…
    #> 18 "p = .…     109           26          9          NA "Results… 0956797… Resul…
    #> 19 "p = .…     109           26          9          NA "Results… 0956797… Resul…
    #> 20 "p = .…     109           26          9          NA "Results… 0956797… Resul…
    #> # ℹ 3 more variables: section_type <chr>, p_comp <chr>, p_value <dbl>

If you run this module on all 250 papers, you will get more rows than
you probably want to print in the full table one row for every p-value
in each paper), so you can print the summary table, which gives you one
row per paper.

``` r

all_p_ps <- module_run(psychsci, "all_p_values")

all_p_ps$summary_table |> head()
```

    #>           paper_id p_values
    #> 1 0956797613520608        6
    #> 2 0956797614522816       39
    #> 3 0956797614527830       13
    #> 4 0956797614557697       24
    #> 5 0956797614560771        4
    #> 6 0956797614566469        0

You can still access the full table for further processing.

``` r

all_p_ps$table |>
  count(text, sort = TRUE) |>
  head()
```

    #> # A tibble: 6 × 2
    #>   text          n
    #>   <chr>     <int>
    #> 1 p < .001   1485
    #> 2 p < .01     138
    #> 3 p < .05     129
    #> 4 p = .001    119
    #> 5 p = .002     93
    #> 6 p < .0001    88

### all_urls

List all the URLs in the main text. There will, of course, be a few
false positives when text in the paper is formatted as a valid URL.

``` r

all_urls <- module_run(paper, "all_urls")

all_urls$table
```

    #> # A tibble: 6 × 9
    #>   text     text_id paragraph_id section_id page_number formatted paper_id header
    #>   <chr>      <int>        <int>      <int>       <int> <chr>     <chr>    <chr> 
    #> 1 3.9.1.7       38            9          3          NA "Accordi… 0956797… Parti…
    #> 2 https:/…      79           19          8          NA "Analysi… 0956797… Analy…
    #> 3 https:/…     129           30         13          NA "All dat… 0956797… Open …
    #> 4 https:/…     130           31         13          NA "The des… 0956797… Open …
    #> 5 http://…     132           31         13          NA "More in… 0956797… Open …
    #> 6 http://…     137           34         16          NA "Additio… 0956797… Suppl…
    #> # ℹ 1 more variable: section_type <chr>

``` r

all_urls_ps <- module_run(psychsci, "all_urls")

all_urls_ps$summary_table
```

    #>              paper_id urls
    #> 1    0956797613520608    1
    #> 2    0956797614522816    0
    #> 3    0956797614527830    1
    #> 4    0956797614557697    6
    #> 5    0956797614560771    0
    #> 6    0956797614566469    5
    #> 7    0956797615569001    7
    #> 8    0956797615569889    3
    #> 9    0956797615583071    4
    #> 10   0956797615588467    2
    #> 11   0956797615603702    0
    #> 12   0956797615615584    2
    #> 13   0956797615617779    1
    #> 14   0956797615620784    4
    #> 15   0956797615625973    5
    #> 16   0956797616631990    6
    #> 17   0956797616634654    2
    #> 18   0956797616634665    1
    #> 19   0956797616636631    6
    #> 20   0956797616647519    8
    #> 21   0956797616657319    4
    #> 22   0956797616661199    5
    #> 23   0956797616663878    5
    #> 24   0956797616665351    7
    #> 25   0956797616667447    1
    #> 26   0956797616669994    1
    #> 27   0956797616671327    2
    #> 28   0956797616671712    2
    #> 29   0956797617692000    7
    #> 30   0956797617693326    1
    #> 31   0956797617694867    9
    #> 32   0956797617702501    6
    #> 33   0956797617702699    5
    #> 34   0956797617705391    4
    #> 35   0956797617705667    4
    #> 36   0956797617707270    7
    #> 37   0956797617710785    6
    #> 38   0956797617714811    1
    #> 39   0956797617716922    3
    #> 40   0956797617716929   10
    #> 41   0956797617724435    9
    #> 42   0956797617736886   10
    #> 43   0956797617737129   31
    #> 44   0956797617739368    9
    #> 45   0956797617740685    3
    #> 46   0956797617744542   23
    #> 47   0956797618755322    7
    #> 48   0956797618760197    4
    #> 49   0956797618772822    4
    #> 50   0956797618773095    1
    #> 51   0956797618785899    8
    #> 52   0956797618795679    3
    #> 53   0956797618796480    6
    #> 54   0956797618804501    2
    #> 55   0956797618815482    0
    #> 56   0956797618815488    1
    #> 57   0956797618823540    2
    #> 58   0956797619830326   18
    #> 59   0956797619830329   10
    #> 60   0956797619831964    4
    #> 61   0956797619833325    2
    #> 62   0956797619835147    9
    #> 63   0956797619837981    1
    #> 64   0956797619841265    8
    #> 65   0956797619842261    7
    #> 66   0956797619842550    6
    #> 67   0956797619844231    7
    #> 68   0956797619851753    4
    #> 69   0956797619866625    6
    #> 70   0956797619866627    8
    #> 71   0956797619869905    5
    #> 72   0956797619876260   10
    #> 73   0956797619881134    7
    #> 74   0956797619890619    7
    #> 75   0956797620903716   21
    #> 76   0956797620904450    4
    #> 77   0956797620904990   18
    #> 78   0956797620915887   25
    #> 79   0956797620916521    3
    #> 80   0956797620916782   14
    #> 81   0956797620927648    5
    #> 82   0956797620927967    8
    #> 83   0956797620929297    2
    #> 84   0956797620929302    7
    #> 85   0956797620931108    4
    #> 86   0956797620939054   15
    #> 87   0956797620941840    6
    #> 88   0956797620948821   20
    #> 89   0956797620951115    7
    #> 90   0956797620954815    1
    #> 91   0956797620955209    6
    #> 92   0956797620957625    9
    #> 93   0956797620958638    2
    #> 94   0956797620958650    3
    #> 95   0956797620959014    2
    #> 96   0956797620959594   19
    #> 97   0956797620960011    7
    #> 98   0956797620963615   11
    #> 99   0956797620965520    4
    #> 100  0956797620965536    6
    #> 101  0956797620967261    5
    #> 102  0956797620968789    2
    #> 103  0956797620970548    5
    #> 104  0956797620970559    3
    #> 105  0956797620971298    8
    #> 106  0956797620971652    4
    #> 107  0956797620972116    4
    #> 108  0956797620972688    2
    #> 109  0956797620975781    6
    #> 110  0956797620984464    5
    #> 111  0956797620985832    9
    #> 112 09567976211001317    6
    #> 113 09567976211005465    3
    #> 114 09567976211005767    7
    #> 115 09567976211007414   15
    #> 116 09567976211007788   15
    #> 117 09567976211010718   10
    #> 118 09567976211011969   18
    #> 119 09567976211013045    1
    #> 120 09567976211015941    5
    #> 121 09567976211015942    6
    #> 122 09567976211016395    1
    #> 123 09567976211016410    9
    #> 124 09567976211017870    8
    #> 125 09567976211018618   17
    #> 126 09567976211019950   11
    #> 127 09567976211024259   17
    #> 128 09567976211024260    4
    #> 129 09567976211024535   10
    #> 130 09567976211026983    5
    #> 131 09567976211028978    5
    #> 132 09567976211030630    4
    #> 133 09567976211032224    6
    #> 134 09567976211032676    5
    #> 135 09567976211037971    9
    #> 136 09567976211040491   14
    #> 137 09567976211040803   11
    #> 138 09567976211043426    6
    #> 139 09567976211043428    4
    #> 140 09567976211046884    3
    #> 141 09567976211048485    1
    #> 142 09567976211049439   16
    #> 143 09567976211051272    9
    #> 144 09567976211052476    7
    #> 145 09567976211055375   13
    #> 146 09567976211059801    5
    #> 147 09567976211061321   14
    #> 148 09567976211068045    3
    #> 149 09567976211068070    3
    #> 150 09567976211068880    3
    #> 151  0956797621991137    3
    #> 152  0956797621991548    6
    #> 153  0956797621995197   11
    #> 154  0956797621995202   16
    #> 155  0956797621996660    7
    #> 156  0956797621996667    8
    #> 157  0956797621997350    4
    #> 158  0956797621997366   11
    #> 159  0956797621998312    5
    #> 160 09567976221079633    2
    #> 161 09567976221082637    7
    #> 162 09567976221082938   11
    #> 163 09567976221082941   11
    #> 164 09567976221083219   10
    #> 165 09567976221086513    4
    #> 166 09567976221089599    7
    #> 167 09567976221094036   13
    #> 168 09567976221094782    9
    #> 169 09567976221101045    8
    #> 170 09567976221114055    8
    #> 171 09567976221116816    2
    #> 172 09567976221116892    7
    #> 173 09567976221116893    6
    #> 174 09567976221119391    7
    #> 175 09567976221121348    5
    #> 176 09567976221131519    5
    #> 177 09567976221131520    9
    #> 178 09567976221134476    7
    #> 179 09567976221139496    1
    #> 180 09567976221140326    1
    #> 181 09567976221140341   16
    #> 182 09567976221145316    5
    #> 183 09567976221147258    2
    #> 184 09567976221147259    6
    #> 185 09567976221150616    5
    #> 186 09567976231151581    2
    #> 187 09567976231154804    3
    #> 188 09567976231156413    6
    #> 189 09567976231156793    1
    #> 190 09567976231158288    6
    #> 191 09567976231158570    3
    #> 192 09567976231160098    1
    #> 193 09567976231160702   10
    #> 194 09567976231161565    5
    #> 195 09567976231164553    3
    #> 196 09567976231165267    1
    #> 197 09567976231170878    2
    #> 198 09567976231172500    3
    #> 199 09567976231173900   14
    #> 200 09567976231173902    3
    #> 201 09567976231177968    2
    #> 202 09567976231179378    2
    #> 203 09567976231180578   10
    #> 204 09567976231180588    3
    #> 205 09567976231180881    5
    #> 206 09567976231184887    8
    #> 207 09567976231185127    3
    #> 208 09567976231185129    8
    #> 209 09567976231188107   12
    #> 210 09567976231188124    0
    #> 211 09567976231190546    7
    #> 212 09567976231192241    1
    #> 213 09567976231194221    1
    #> 214 09567976231194590    5
    #> 215 09567976231196145    6
    #> 216 09567976231198194   11
    #> 217 09567976231198435    4
    #> 218 09567976231199440    4
    #> 219 09567976231203139    1
    #> 220 09567976231204035   12
    #> 221 09567976231207095    8
    #> 222 09567976231213572   10
    #> 223 09567976231217508    1
    #> 224 09567976231218640    2
    #> 225 09567976231220902    7
    #> 226 09567976231221789    2
    #> 227 09567976231222288    4
    #> 228 09567976231222836    4
    #> 229 09567976231223130    7
    #> 230 09567976231223410   10
    #> 231 09567976241227411    7
    #> 232 09567976241228504    4
    #> 233 09567976241232891    4
    #> 234 09567976241235931    4
    #> 235 09567976241235932    0
    #> 236 09567976241239932    4
    #> 237 09567976241239935    9
    #> 238 09567976241242105    5
    #> 239 09567976241243370    4
    #> 240 09567976241245695   10
    #> 241 09567976241246561    3
    #> 242 09567976241249183    5
    #> 243 09567976241254312    4
    #> 244 09567976241258149    9
    #> 245 09567976241260247    3
    #> 246 09567976241263344    9
    #> 247 09567976241263347    7
    #> 248 09567976241266516    5
    #> 249 09567976241267854    5
    #> 250 09567976241279291   11

### stat_p_exact

List any p-values that may have been reported with insufficient
precision (e.g., p \< .05 or p = n.s.).

``` r

imprecise <- module_run(paper, "stat_p_exact")

imprecise$table$text # print table
```

    #>  [1] "p = .003"  "p = .08"   "p < .001 " "p < .025"  "p = .040"  "p = .173" 
    #>  [7] "p = .006"  "p = .02"   "p = .691"  "p =.303"   "p = .023"  "p <.001"  
    #> [13] "p = .006"  "p = .037"  "p = .038"  "p = .358"  "p < .001"  "p = .127" 
    #> [19] "p = .062"  "p = .047"

The `expanded` column has the full sentence for context. Here you can
see that “p \< .025” was not an imprecisely reported p-value, but a
description of the preregistered alpha threshold.

``` r

imprecise$table$expanded[[4]] # print expanded text
```

    #> [1] "The main effect of illness recency did not meet our preregistered threshold (p < .025)-recently ill: M = 661 ms, SD = 197; not recently ill: M = 626 ms, SD = 153, F(1, 400) = 4.23, ηp² = .010, 90% CI = [.000, .039], p = .040-nor did the interaction between illness recency and face type (disfigured vs. typical), F(1, 400) = 1.87, ηp² = .005, 90% CI = [.000, .027], p = .173."

We can investigate the most common imprecise p-values in the PsychSci
set. “p \< .01” and “p \< .05” are probably often describing figures or
tables, but what is the deal with “p \> .25”?

``` r

imprecise_ps <- module_run(psychsci, "stat_p_exact")

imprecise_ps$table |>
  count(text, sort = TRUE) |>
  head()
```

    #> # A tibble: 6 × 2
    #>   text          n
    #>   <chr>     <int>
    #> 1 p < .001   1485
    #> 2 p < .01     138
    #> 3 p < .05     129
    #> 4 p = .001    119
    #> 5 p = .002     93
    #> 6 p < .0001    88

We can expand the text to check the context for “p \> .25”.

``` r

gt.25 <- imprecise_ps$table |>
  filter(grepl("\\.25", text))

gt.25$expanded[1:3] # look at the first 3
```

    #> [1] "There was a significant interactive effect of time and political orientation, b = 0.10, SE = 0.04, 95% CI = [0.03, 0.17], t(1922) = 2.72, p = .007, on endorsement of the fairness foundation (see Table S2 CI = [-0.14, -0.04], t(1922) = -3.59, p < .001, disappeared after 7/7, b = 0.004, SE = 0.02, 95% CI = [-0.04, 0.05], t(1922) = 0.17, p > .250 (see Fig 2)."                            
    #> [2] "Contrary to expectations, our results revealed no significant main effect of time, b = -0.13, SE = 0.22, 95% CI = [-0.55, 0.30], t(1922) = -0.58, p > .250, political orientation, b = 0.05, SE = 0.10, 95% CI = [-0.15, 0.24], t(1922) = 0.47, p > .250, or their interaction, b = 0.04, SE = 0.06, 95% CI = [-0.08, 0.16], t(1922) = 0.67, p > .250, on endorsement of the authority foundation."
    #> [3] "Contrary to expectations, our results revealed no significant main effect of time, b = -0.13, SE = 0.22, 95% CI = [-0.55, 0.30], t(1922) = -0.58, p > .250, political orientation, b = 0.05, SE = 0.10, 95% CI = [-0.15, 0.24], t(1922) = 0.47, p > .250, or their interaction, b = 0.04, SE = 0.06, 95% CI = [-0.08, 0.16], t(1922) = 0.67, p > .250, on endorsement of the authority foundation."

### marginal

List all sentences that describe an effect as ‘marginally significant’.

``` r

marginal <- module_run(paper, "marginal")

marginal # print table
```

Marginal Significance: You described 0 effects with terms related to
‘marginally significant’.

Let’s check how many are in the full set.

``` r

marginal_ps <- module_run(psychsci, "marginal")

marginal_ps$table # print table
```

    #> # A tibble: 102 × 9
    #>    text    text_id paragraph_id section_id page_number formatted paper_id header
    #>    <chr>     <int>        <int>      <int>       <int> <chr>     <chr>    <chr> 
    #>  1 Althou…     128           29         15          NA "Althoug… 0956797… Detai…
    #>  2 A marg…     139           32         18          NA "A margi… 0956797… Postt…
    #>  3 When w…     150           35         19          NA  NA       0956797… The e…
    #>  4 In tha…     154           36         20          NA "In that… 0956797… Condi…
    #>  5 The Co…     110           22          7          NA "The Con… 0956797… Resul…
    #>  6 The tw…     113           24          6          NA "The two… 0956797… Resul…
    #>  7 An omn…     186           41         14          NA  NA       0956797… Resul…
    #>  8 Furthe…     244           63         28          NA  NA       0956797… NA    
    #>  9 The da…     259           65         30          NA "The dag… 0956797… Fig 5.
    #> 10 Given …     166           38         19          NA  NA       0956797… Autho…
    #> # ℹ 92 more rows
    #> # ℹ 1 more variable: section_type <chr>

### stat_check

Check consistency of p-values and test statistics using functions from
[statcheck](https://github.com/MicheleNuijten/statcheck).

``` r

statcheck <- module_run(paper, "stat_check")

statcheck$table
```

    #>   test_type df1   df2 test_comp test_value p_comp reported_p   computed_p
    #> 1         t  NA 248.4         =       2.01      =      0.023 4.551244e-02
    #> 2         t  NA 248.4         =      -4.55      <      0.001 8.397343e-06
    #>                         raw error decision_error one_tailed_in_txt apa_factor
    #> 1 t(248.4) = 2.01, p = .023  TRUE          FALSE             FALSE          1
    #> 2 t(248.4) = -4.55, p <.001 FALSE          FALSE             FALSE          1
    #>                                                                                                                                                                                                                                                                                             text
    #> 1 Yes-the 90% confidence intervals of the difference in attentional bias for participants who were and were not recently ill found here (dz = -0.14, 90% CI = [-0.31, -0.04]) did not overlap with an effect size (dz ) of -0.35, t(248.4) = 2.01, p = .023, or 0.35, t(248.4) = -4.55, p <.001.
    #> 2 Yes-the 90% confidence intervals of the difference in attentional bias for participants who were and were not recently ill found here (dz = -0.14, 90% CI = [-0.31, -0.04]) did not overlap with an effect size (dz ) of -0.35, t(248.4) = 2.01, p = .023, or 0.35, t(248.4) = -4.55, p <.001.
    #>   text_id paragraph_id section_id page_number
    #> 1     106           25          9          NA
    #> 2     106           25          9          NA
    #>                                                                                                                                                                                                                                                                                           formatted
    #> 1 Yes-the 90% confidence intervals of the difference in attentional bias for participants who were and were not recently ill found here (dz = -0.14, 90% CI = [-0.31, -0.04]) did not overlap with an effect size (dz ) of -0.35, t(248.4) = 2.01, p = .023, or 0.35, t(248.4) = -4.55, p &lt;.001.
    #> 2 Yes-the 90% confidence intervals of the difference in attentional bias for participants who were and were not recently ill found here (dz = -0.14, 90% CI = [-0.31, -0.04]) did not overlap with an effect size (dz ) of -0.35, t(248.4) = 2.01, p = .023, or 0.35, t(248.4) = -4.55, p &lt;.001.
    #>           paper_id  header section_type
    #> 1 0956797620955209 Results      results
    #> 2 0956797620955209 Results      results

Here we see a false positive, where the paper reported the results of an
equivalence test, which are meant to be one-tailed, but statcheck did
not detect that this was one-tailed.

In the full PsychSci set, there are more than 27K sentences with numbers
to check, so this takes about a minute to run.

``` r

statcheck_ps <- module_run(psychsci, "stat_check")
```

There will be, of course, some false positives in the full set of 151
flagged values. Let’s look just at the flagged values where the computed
p-value is about double the reported p-value, and this changes the
significance decision (at an alpha of 0.05).

``` r

statcheck_ps$table |>
  filter(decision_error, 
         round(computed_p/reported_p, 1) == 2.0) |>
  select(reported_p, computed_p, raw) |>
  mutate(computed_p = round(computed_p, 4))
```

    #>   reported_p computed_p                        raw
    #> 1     0.0290     0.0589 F(1, 361) = 3.59, p = .029
    #> 2     0.0470     0.0947     t(24) = 1.74, p = .047
    #> 3     0.0270     0.0547     t(24) = 2.02, p = .027
    #> 4     0.0400     0.0797     t(24) = 1.83, p = .040
    #> 5     0.0480     0.0962    t(240) = 1.67, p = .048
    #> 6     0.0460     0.0915     t(32) = 1.74, p = .046
    #> 7     0.0420     0.0846     t(21) = 1.81, p = .042
    #> 8     0.0343     0.0686    t(10) = 2.04, p = .0343
    #> 9     0.0330     0.0654     t(55) = 1.88, p = .033

## Chaining Modules

Modules return a `summary` table as well as the detailed results
`table`, which is automatically added to the summary if you chain
modules.

``` r

ps_metascience <- psychsci[1:10] |>
  module_run("all_p_values") |>
  module_run("stat_p_exact") |>
  module_run("marginal")

ps_metascience$summary_table
```

    #>            paper_id p_values n_imprecise n_zero marginal
    #> 1  0956797613520608        6           0      0        0
    #> 2  0956797614522816       39           0      0        0
    #> 3  0956797614527830       13           2      0        0
    #> 4  0956797614557697       24           8      0        0
    #> 5  0956797614560771        4           1      0        0
    #> 6  0956797614566469        0           0      0        0
    #> 7  0956797615569001       25          20      0        0
    #> 8  0956797615569889       28           0      0        4
    #> 9  0956797615583071       25           2      0        0
    #> 10 0956797615588467       21           4      0        0
