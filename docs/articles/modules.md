# Modules

``` r
library(metacheck)
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

\*\*\* GENERAL \*\* all_urls: List all the URLs in the main text. \*
coi_check: Identify and extract Conflicts of Interest (COI) statements.
\* coi_check_oi: Identify and extract Conflicts of Interest (COI)
statements. \* funding_check: Identify and extract funding statements.
\* funding_check_oi: Identify and extract funding statements. \*
open_practices: This module incorporates ODDPub into metacheck. ODDPub
is a text mining algorithm that detects which publications disseminated
Open Data or Open Code together with the publication.

\*\*\* METHOD \*\* causal_claims: Aims to identify the presence of
random assignment, and lists sentences that make causal claims in title
or abstract. \* power: This module uses a large language module (LLM) to
extract information reported in power analyses, including the
statistical test, sample size, alpha level, desired level of power,and
magnitude and type of effect size.

If you have not set llm_use(TRUE) and supplied a groq API, the module
will return paragraphs that potentially contain power analyses, based on
a regular expression search. \* prereg_check: Retrieve information from
preregistrations in a standardised way, and make them easier to check.

\*\*\* RESULTS \*\* all_p_values: List all p-values in the text,
returning the matched text (e.g., ‘p = 0.04’) and document location in a
table. \* code_check: This module retrieves information from
repositories checked by repo_check about code files (R, SAS, SPSS,
Stata). \* marginal: List all sentences that describe an effect as
‘marginally significant’. \* repo_check: This module retrieves
information from repositories. \* stat_check: Check consistency of
p-values and test statistics \* stat_effect_size: The Effect Size module
checks for effect sizes in t-tests and F-tests. \* stat_p_exact: List
any p-values reported with insufficient precision (e.g., p \< .05 or p =
n.s.) \* stat_p_nonsig: This module checks for imprecisely reported p
values. If p \> .05 is detected, it warns for misinterpretations.

\*\*\* REFERENCE \*\* ref_accuracy: This module checks references for
mismatches with CrossRef. \* ref_consistency: Check if all references
are cited and all citations are referenced \* ref_doi_check: This module
checks references for missing DOIs or DOIs with an invalid format. \*
ref_miscitation: Check for frequently miscited papers. This module is
just a proof of concept – the miscite database is not yet populated with
real examples. \* ref_pubpeer: This module checks references and warns
for citations that have comments on pubpeer (excluding Statcheck
comments). \* ref_replication: This module checks references and warns
for citations of original studies for which replication studies exist in
the Replication Database. \* ref_retraction: This module checks
references and warns for citations in the RetractionWatch Database. \*
ref_summary: Summarise information about each reference in a paper.

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
paper <- read(demoxml())
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

    #> [1] "We found 1 imprecise *p* value out of 3 detected."

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
[`search_text()`](https://scienceverse.github.io/metacheck/reference/search_text.md)
or
[`expand_text()`](https://scienceverse.github.io/metacheck/reference/expand_text.md),
containing either text relevant to the module, or a classification of
the text. This table can be of use to further modules in a chain, or to
metascientific users.

``` r
mo$table
```

    #> # A tibble: 3 × 11
    #>   text  section header   div     p     s id    p_comp p_value expanded imprecise
    #>   <chr> <chr>   <chr>  <dbl> <dbl> <int> <chr> <chr>    <dbl> <chr>    <lgl>    
    #> 1 p = … method  Metho…     2     1    11 to_e… =        0.005 On aver… FALSE    
    #> 2 p = … results Resul…     3     1     1 to_e… =        0.152 On aver… FALSE    
    #> 3 p > … results Resul…     3     1     2 to_e… >        0.05  There w… TRUE

### Summary Table

The `summary_table` contains a single row for each paper, and must have
an `id` column that matches the paper IDs. It will also have additional
columns that summarise the results of the module. This is mainly useful
in the metascientific workflow, and this table is appended by each
module in a chain.

``` r
mo$summary_table
```

    #>                id n_imprecise
    #> 1 to_err_is_human           1

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
    #> [2] "\n```{r}\n#| echo: false\n\n\n# table data --------------------------------------\ntable <- structure(list(\"P-Value\" = \"p > .05\", Sentence = \"There was no effect of experience on the reduction in errors when using the tool (p > .05), as the correlation was non-significant.\"), row.names = c(NA, \n-1L), class = c(\"tbl_df\", \"tbl\", \"data.frame\"))\n\n# display table -----------------------------------\nmetacheck::report_table(table, c(0.1, 0.9), 2, FALSE)\n```\n"                                              
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
    #> * Sections: 4
    #> * Sentences: 28
    #> * Bibliography: 4
    #> * X-Refs: 2

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
    #> Exact P-Values: We found 1 imprecise *p* value out of 3 detected.
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

    #> # A tibble: 20 × 9
    #>    text        section header           div     p     s id        p_comp p_value
    #>    <chr>       <chr>   <chr>          <dbl> <dbl> <int> <chr>     <chr>    <dbl>
    #>  1 "p = .003"  method  Questionnaires     6     2     3 09567976… =        0.003
    #>  2 "p = .08"   method  Questionnaires     6     2     3 09567976… =        0.08 
    #>  3 "p < .001 " results Results            9     2     1 09567976… <        0.001
    #>  4 "p < .025"  results Results            9     2     3 09567976… <        0.025
    #>  5 "p = .040"  results Results            9     2     3 09567976… =        0.04 
    #>  6 "p = .173"  results Results            9     2     3 09567976… =        0.173
    #>  7 "p = .006"  results Results            9     2     4 09567976… =        0.006
    #>  8 "p = .02"   results Results            9     2     4 09567976… =        0.02 
    #>  9 "p = .691"  results Results            9     2     5 09567976… =        0.691
    #> 10 "p = .303"  results Results            9     2     5 09567976… =        0.303
    #> 11 "p = .023"  results Results            9     3     3 09567976… =        0.023
    #> 12 "p < .001"  results Results            9     3     3 09567976… <        0.001
    #> 13 "p = .006"  results Results            9     4     2 09567976… =        0.006
    #> 14 "p = .037"  results Results            9     4     2 09567976… =        0.037
    #> 15 "p = .038"  results Results            9     4     2 09567976… =        0.038
    #> 16 "p = .358"  results Results            9     4     2 09567976… =        0.358
    #> 17 "p < .001"  results Results            9     4     3 09567976… <        0.001
    #> 18 "p = .127"  results Results            9     4     3 09567976… =        0.127
    #> 19 "p = .062"  results Results            9     4     3 09567976… =        0.062
    #> 20 "p = .047"  results Results            9     4     3 09567976… =        0.047

If you run this module on all 250 papers, you will get more rows than
you probably want to print in the full table one row for every p-value
in each paper), so you can print the summary table, which gives you one
row per paper.

``` r
all_p_ps <- module_run(psychsci, "all_p_values")

all_p_ps$summary_table
```

    #>                    id p_values
    #> 1    0956797613520608        6
    #> 2    0956797614522816       39
    #> 3    0956797614527830       13
    #> 4    0956797614557697       27
    #> 5    0956797614560771        4
    #> 6    0956797614566469        0
    #> 7    0956797615569001       25
    #> 8    0956797615569889       26
    #> 9    0956797615583071       24
    #> 10   0956797615588467       21
    #> 11   0956797615603702        8
    #> 12   0956797615615584       26
    #> 13   0956797615617779       34
    #> 14   0956797615620784        9
    #> 15   0956797615625973       11
    #> 16   0956797616631990        8
    #> 17   0956797616634654       22
    #> 18   0956797616634665       18
    #> 19   0956797616636631       26
    #> 20   0956797616647519        8
    #> 21   0956797616657319       17
    #> 22   0956797616661199       13
    #> 23   0956797616663878        5
    #> 24   0956797616665351        8
    #> 25   0956797616667447       17
    #> 26   0956797616669994        2
    #> 27   0956797616671327       37
    #> 28   0956797616671712       25
    #> 29   0956797617692000       39
    #> 30   0956797617693326       37
    #> 31   0956797617694867       51
    #> 32   0956797617702501        0
    #> 33   0956797617702699        3
    #> 34   0956797617705391       16
    #> 35   0956797617705667       31
    #> 36   0956797617707270        8
    #> 37   0956797617710785        8
    #> 38   0956797617714811        1
    #> 39   0956797617716922       14
    #> 40   0956797617716929       24
    #> 41   0956797617724435        1
    #> 42   0956797617736886       12
    #> 43   0956797617737129       37
    #> 44   0956797617739368        6
    #> 45   0956797617740685        9
    #> 46   0956797617744542       19
    #> 47   0956797618755322        5
    #> 48   0956797618760197        3
    #> 49   0956797618772822       49
    #> 50   0956797618773095        1
    #> 51   0956797618785899       26
    #> 52   0956797618795679       17
    #> 53   0956797618796480        7
    #> 54   0956797618804501        0
    #> 55   0956797618815482        4
    #> 56   0956797618815488       28
    #> 57   0956797618823540        0
    #> 58   0956797619830326       17
    #> 59   0956797619830329       32
    #> 60   0956797619831964       21
    #> 61   0956797619833325        0
    #> 62   0956797619835147       41
    #> 63   0956797619837981        5
    #> 64   0956797619841265        7
    #> 65   0956797619842261       58
    #> 66   0956797619842550       39
    #> 67   0956797619844231       37
    #> 68   0956797619851753       21
    #> 69   0956797619866625       25
    #> 70   0956797619866627        3
    #> 71   0956797619869905       24
    #> 72   0956797619876260        9
    #> 73   0956797619881134       48
    #> 74   0956797619890619        0
    #> 75   0956797620903716        9
    #> 76   0956797620904450       14
    #> 77   0956797620904990        0
    #> 78   0956797620915887        1
    #> 79   0956797620916521       28
    #> 80   0956797620916782        4
    #> 81   0956797620927648       34
    #> 82   0956797620927967       11
    #> 83   0956797620929297       16
    #> 84   0956797620929302       21
    #> 85   0956797620931108        8
    #> 86   0956797620939054       21
    #> 87   0956797620941840        0
    #> 88   0956797620948821       17
    #> 89   0956797620951115        0
    #> 90   0956797620954815       39
    #> 91   0956797620955209       20
    #> 92   0956797620957625       62
    #> 93   0956797620958638       95
    #> 94   0956797620958650       38
    #> 95   0956797620959014       18
    #> 96   0956797620959594        4
    #> 97   0956797620960011       20
    #> 98   0956797620963615       25
    #> 99   0956797620965520       19
    #> 100  0956797620965536       32
    #> 101  0956797620967261        7
    #> 102  0956797620968789       15
    #> 103  0956797620970548       32
    #> 104  0956797620970559       10
    #> 105  0956797620971298       41
    #> 106  0956797620971652        0
    #> 107  0956797620972116       61
    #> 108  0956797620972688        6
    #> 109  0956797620975781       29
    #> 110  0956797620984464       52
    #> 111  0956797620985832       27
    #> 112 09567976211001317        0
    #> 113 09567976211005465       33
    #> 114 09567976211005767        4
    #> 115 09567976211007414       10
    #> 116 09567976211007788       18
    #> 117 09567976211010718       15
    #> 118 09567976211011969       33
    #> 119 09567976211013045       11
    #> 120 09567976211015941        1
    #> 121 09567976211015942       32
    #> 122 09567976211016395        6
    #> 123 09567976211016410       11
    #> 124 09567976211017870       17
    #> 125 09567976211018618       22
    #> 126 09567976211019950        0
    #> 127 09567976211024259       12
    #> 128 09567976211024260        2
    #> 129 09567976211024535       23
    #> 130 09567976211026983       40
    #> 131 09567976211028978       10
    #> 132 09567976211030630       44
    #> 133 09567976211032224       17
    #> 134 09567976211032676       15
    #> 135 09567976211037971       21
    #> 136 09567976211040491        8
    #> 137 09567976211040803        5
    #> 138 09567976211043426        0
    #> 139 09567976211043428        6
    #> 140 09567976211046884        0
    #> 141 09567976211048485       17
    #> 142 09567976211049439       13
    #> 143 09567976211051272        2
    #> 144 09567976211052476        2
    #> 145 09567976211055375       24
    #> 146 09567976211059801       32
    #> 147 09567976211061321       39
    #> 148 09567976211068045        0
    #> 149 09567976211068070       27
    #> 150 09567976211068880       41
    #> 151  0956797621991137       32
    #> 152  0956797621991548       14
    #> 153  0956797621995197       47
    #> 154  0956797621995202       33
    #> 155  0956797621996660       23
    #> 156  0956797621996667       46
    #> 157  0956797621997350        6
    #> 158  0956797621997366        2
    #> 159  0956797621998312        0
    #> 160 09567976221079633       18
    #> 161 09567976221082637        2
    #> 162 09567976221082938       31
    #> 163 09567976221082941        6
    #> 164 09567976221083219       27
    #> 165 09567976221086513       66
    #> 166 09567976221089599       15
    #> 167 09567976221094036       29
    #> 168 09567976221094782       48
    #> 169 09567976221101045        0
    #> 170 09567976221114055       39
    #> 171 09567976221116816        1
    #> 172 09567976221116892       34
    #> 173 09567976221116893        1
    #> 174 09567976221119391       14
    #> 175 09567976221121348       55
    #> 176 09567976221131519       15
    #> 177 09567976221131520       30
    #> 178 09567976221134476       19
    #> 179 09567976221139496       16
    #> 180 09567976221140326        0
    #> 181 09567976221140341       24
    #> 182 09567976221145316        5
    #> 183 09567976221147258        8
    #> 184 09567976221147259       29
    #> 185 09567976221150616       33
    #> 186 09567976231151581       27
    #> 187 09567976231154804       24
    #> 188 09567976231156413       61
    #> 189 09567976231156793        4
    #> 190 09567976231158288        6
    #> 191 09567976231158570       24
    #> 192 09567976231160098       30
    #> 193 09567976231160702        5
    #> 194 09567976231161565        1
    #> 195 09567976231164553        0
    #> 196 09567976231165267       32
    #> 197 09567976231170878       16
    #> 198 09567976231172500       29
    #> 199 09567976231173900       57
    #> 200 09567976231173902       14
    #> 201 09567976231177968       27
    #> 202 09567976231179378       31
    #> 203 09567976231180578        8
    #> 204 09567976231180588        9
    #> 205 09567976231180881       16
    #> 206 09567976231184887        5
    #> 207 09567976231185127        8
    #> 208 09567976231185129        1
    #> 209 09567976231188107       23
    #> 210 09567976231188124        0
    #> 211 09567976231190546       31
    #> 212 09567976231192241       28
    #> 213 09567976231194221       14
    #> 214 09567976231194590       25
    #> 215 09567976231196145        7
    #> 216 09567976231198194       21
    #> 217 09567976231198435       30
    #> 218 09567976231199440       13
    #> 219 09567976231203139        0
    #> 220 09567976231204035       47
    #> 221 09567976231207095       27
    #> 222 09567976231213572        3
    #> 223 09567976231217508        0
    #> 224 09567976231218640       35
    #> 225 09567976231220902        8
    #> 226 09567976231221789       31
    #> 227 09567976231222288        0
    #> 228 09567976231222836        1
    #> 229 09567976231223130       70
    #> 230 09567976231223410       18
    #> 231 09567976241227411       45
    #> 232 09567976241228504       23
    #> 233 09567976241232891       15
    #> 234 09567976241235931       28
    #> 235 09567976241235932        0
    #> 236 09567976241239932       12
    #> 237 09567976241239935       17
    #> 238 09567976241242105        3
    #> 239 09567976241243370       11
    #> 240 09567976241245695        0
    #> 241 09567976241246561       39
    #> 242 09567976241249183       21
    #> 243 09567976241254312       20
    #> 244 09567976241258149       20
    #> 245 09567976241260247        8
    #> 246 09567976241263344       25
    #> 247 09567976241263347       53
    #> 248 09567976241266516       37
    #> 249 09567976241267854       46
    #> 250 09567976241279291        3

You can still access the full table for further processing.

``` r
all_p_ps$table |>
  count(text, sort = TRUE) |>
  head()
```

    #> # A tibble: 6 × 2
    #>   text          n
    #>   <chr>     <int>
    #> 1 p < .001   1503
    #> 2 p < .01     137
    #> 3 p < .05     135
    #> 4 p = .001    120
    #> 5 p = .002     93
    #> 6 p < .0001    88

### all_urls

List all the URLs in the main text. There will, of course, be a few
false positives when text in the paper is formatted as a valid URL.

``` r
all_urls <- module_run(paper, "all_urls")

all_urls$table
```

    #> # A tibble: 5 × 7
    #>   text                                    section header   div     p     s id   
    #>   <chr>                                   <chr>   <chr>  <dbl> <dbl> <int> <chr>
    #> 1 3.9.1.7                                 method  Parti…     3     1     5 0956…
    #> 2 https://osf.io/k2dbf                    method  Analy…     8     1     1 0956…
    #> 3 https://osf.io/k2dbf                    funding Open …    14     1     1 0956…
    #> 4 https://osf.io/k2dbf                    funding Open …    14     2     1 0956…
    #> 5 http://www.psychologicalscience.org/pu… funding Open …    14     2     3 0956…

``` r
all_urls_ps <- module_run(psychsci, "all_urls")

all_urls_ps$summary_table
```

    #>                    id urls
    #> 1    0956797613520608    0
    #> 2    0956797614522816    0
    #> 3    0956797614527830    1
    #> 4    0956797614557697    6
    #> 5    0956797614560771    0
    #> 6    0956797614566469    5
    #> 7    0956797615569001    7
    #> 8    0956797615569889    2
    #> 9    0956797615583071    4
    #> 10   0956797615588467    2
    #> 11   0956797615603702    0
    #> 12   0956797615615584    2
    #> 13   0956797615617779    1
    #> 14   0956797615620784    4
    #> 15   0956797615625973    4
    #> 16   0956797616631990    6
    #> 17   0956797616634654    2
    #> 18   0956797616634665    1
    #> 19   0956797616636631    5
    #> 20   0956797616647519    7
    #> 21   0956797616657319    3
    #> 22   0956797616661199    5
    #> 23   0956797616663878    4
    #> 24   0956797616665351    5
    #> 25   0956797616667447    1
    #> 26   0956797616669994    1
    #> 27   0956797616671327    2
    #> 28   0956797616671712    1
    #> 29   0956797617692000    6
    #> 30   0956797617693326    1
    #> 31   0956797617694867    8
    #> 32   0956797617702501    6
    #> 33   0956797617702699    4
    #> 34   0956797617705391    3
    #> 35   0956797617705667    5
    #> 36   0956797617707270    2
    #> 37   0956797617710785    4
    #> 38   0956797617714811    1
    #> 39   0956797617716922    3
    #> 40   0956797617716929   10
    #> 41   0956797617724435    9
    #> 42   0956797617736886    1
    #> 43   0956797617737129    9
    #> 44   0956797617739368    9
    #> 45   0956797617740685    3
    #> 46   0956797617744542    3
    #> 47   0956797618755322    5
    #> 48   0956797618760197    4
    #> 49   0956797618772822    5
    #> 50   0956797618773095    1
    #> 51   0956797618785899    8
    #> 52   0956797618795679    3
    #> 53   0956797618796480    5
    #> 54   0956797618804501    1
    #> 55   0956797618815482    0
    #> 56   0956797618815488    3
    #> 57   0956797618823540    2
    #> 58   0956797619830326   17
    #> 59   0956797619830329    9
    #> 60   0956797619831964    5
    #> 61   0956797619833325    2
    #> 62   0956797619835147    8
    #> 63   0956797619837981    1
    #> 64   0956797619841265    8
    #> 65   0956797619842261    6
    #> 66   0956797619842550    5
    #> 67   0956797619844231    7
    #> 68   0956797619851753    3
    #> 69   0956797619866625    6
    #> 70   0956797619866627    8
    #> 71   0956797619869905    5
    #> 72   0956797619876260   11
    #> 73   0956797619881134    7
    #> 74   0956797619890619    7
    #> 75   0956797620903716   21
    #> 76   0956797620904450    2
    #> 77   0956797620904990   17
    #> 78   0956797620915887   20
    #> 79   0956797620916521    2
    #> 80   0956797620916782   12
    #> 81   0956797620927648    4
    #> 82   0956797620927967    8
    #> 83   0956797620929297    1
    #> 84   0956797620929302    6
    #> 85   0956797620931108    4
    #> 86   0956797620939054   14
    #> 87   0956797620941840    6
    #> 88   0956797620948821    8
    #> 89   0956797620951115    6
    #> 90   0956797620954815    0
    #> 91   0956797620955209    5
    #> 92   0956797620957625    8
    #> 93   0956797620958638    2
    #> 94   0956797620958650    3
    #> 95   0956797620959014    2
    #> 96   0956797620959594   16
    #> 97   0956797620960011    5
    #> 98   0956797620963615   10
    #> 99   0956797620965520    2
    #> 100  0956797620965536    5
    #> 101  0956797620967261    5
    #> 102  0956797620968789    2
    #> 103  0956797620970548    3
    #> 104  0956797620970559    3
    #> 105  0956797620971298    7
    #> 106  0956797620971652    4
    #> 107  0956797620972116    4
    #> 108  0956797620972688    2
    #> 109  0956797620975781    5
    #> 110  0956797620984464    5
    #> 111  0956797620985832    9
    #> 112 09567976211001317    6
    #> 113 09567976211005465    3
    #> 114 09567976211005767    7
    #> 115 09567976211007414   16
    #> 116 09567976211007788   14
    #> 117 09567976211010718    9
    #> 118 09567976211011969    9
    #> 119 09567976211013045    0
    #> 120 09567976211015941    5
    #> 121 09567976211015942    4
    #> 122 09567976211016395    1
    #> 123 09567976211016410    8
    #> 124 09567976211017870    9
    #> 125 09567976211018618   16
    #> 126 09567976211019950   11
    #> 127 09567976211024259   17
    #> 128 09567976211024260    4
    #> 129 09567976211024535   10
    #> 130 09567976211026983    7
    #> 131 09567976211028978    3
    #> 132 09567976211030630    4
    #> 133 09567976211032224    6
    #> 134 09567976211032676    4
    #> 135 09567976211037971    8
    #> 136 09567976211040491   14
    #> 137 09567976211040803   12
    #> 138 09567976211043426    6
    #> 139 09567976211043428    4
    #> 140 09567976211046884    3
    #> 141 09567976211048485    1
    #> 142 09567976211049439   16
    #> 143 09567976211051272    8
    #> 144 09567976211052476    9
    #> 145 09567976211055375   13
    #> 146 09567976211059801    5
    #> 147 09567976211061321   14
    #> 148 09567976211068045    2
    #> 149 09567976211068070    1
    #> 150 09567976211068880    3
    #> 151  0956797621991137    2
    #> 152  0956797621991548    6
    #> 153  0956797621995197   10
    #> 154  0956797621995202   16
    #> 155  0956797621996660    7
    #> 156  0956797621996667    9
    #> 157  0956797621997350    4
    #> 158  0956797621997366   12
    #> 159  0956797621998312    5
    #> 160 09567976221079633    1
    #> 161 09567976221082637    7
    #> 162 09567976221082938   11
    #> 163 09567976221082941   11
    #> 164 09567976221083219   10
    #> 165 09567976221086513    4
    #> 166 09567976221089599    7
    #> 167 09567976221094036   12
    #> 168 09567976221094782    9
    #> 169 09567976221101045    8
    #> 170 09567976221114055    9
    #> 171 09567976221116816    2
    #> 172 09567976221116892    6
    #> 173 09567976221116893    5
    #> 174 09567976221119391    6
    #> 175 09567976221121348    5
    #> 176 09567976221131519    4
    #> 177 09567976221131520    9
    #> 178 09567976221134476    8
    #> 179 09567976221139496    1
    #> 180 09567976221140326    1
    #> 181 09567976221140341   16
    #> 182 09567976221145316    4
    #> 183 09567976221147258    2
    #> 184 09567976221147259    6
    #> 185 09567976221150616    5
    #> 186 09567976231151581    1
    #> 187 09567976231154804    3
    #> 188 09567976231156413    6
    #> 189 09567976231156793    1
    #> 190 09567976231158288    7
    #> 191 09567976231158570    4
    #> 192 09567976231160098    2
    #> 193 09567976231160702   10
    #> 194 09567976231161565    4
    #> 195 09567976231164553    2
    #> 196 09567976231165267    1
    #> 197 09567976231170878    2
    #> 198 09567976231172500    4
    #> 199 09567976231173900   15
    #> 200 09567976231173902    2
    #> 201 09567976231177968    2
    #> 202 09567976231179378    1
    #> 203 09567976231180578   10
    #> 204 09567976231180588    2
    #> 205 09567976231180881    5
    #> 206 09567976231184887    8
    #> 207 09567976231185127    3
    #> 208 09567976231185129    6
    #> 209 09567976231188107   11
    #> 210 09567976231188124    0
    #> 211 09567976231190546    7
    #> 212 09567976231192241    1
    #> 213 09567976231194221    0
    #> 214 09567976231194590   18
    #> 215 09567976231196145    5
    #> 216 09567976231198194   11
    #> 217 09567976231198435    3
    #> 218 09567976231199440    3
    #> 219 09567976231203139    1
    #> 220 09567976231204035   12
    #> 221 09567976231207095    7
    #> 222 09567976231213572    9
    #> 223 09567976231217508    1
    #> 224 09567976231218640    2
    #> 225 09567976231220902    6
    #> 226 09567976231221789    1
    #> 227 09567976231222288    4
    #> 228 09567976231222836    4
    #> 229 09567976231223130    6
    #> 230 09567976231223410    9
    #> 231 09567976241227411    6
    #> 232 09567976241228504    4
    #> 233 09567976241232891    3
    #> 234 09567976241235931    4
    #> 235 09567976241235932    0
    #> 236 09567976241239932    3
    #> 237 09567976241239935    9
    #> 238 09567976241242105    5
    #> 239 09567976241243370    4
    #> 240 09567976241245695    8
    #> 241 09567976241246561    2
    #> 242 09567976241249183    5
    #> 243 09567976241254312    3
    #> 244 09567976241258149    9
    #> 245 09567976241260247    4
    #> 246 09567976241263344    8
    #> 247 09567976241263347    7
    #> 248 09567976241266516    4
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
    #>  [7] "p = .006"  "p = .02"   "p = .691"  "p = .303"  "p = .023"  "p < .001" 
    #> [13] "p = .006"  "p = .037"  "p = .038"  "p = .358"  "p < .001"  "p = .127" 
    #> [19] "p = .062"  "p = .047"

The `expanded` column has the full sentence for context. Here you can
see that “p \< .025” was not an imprecisely reported p-value, but a
description of the preregistered alpha threshold.

``` r
imprecise$table$expanded[[4]] # print expanded text
```

    #> [1] "The main effect of illness recency did not meet our preregistered threshold (p < .025)-recently ill: M = 661 ms, SD = 197; not recently ill: M = 626 ms, SD = 153, F(1, 400) = 4.23, η p 2 = .010, 90% CI = [.000, .039], p = .040-nor did the interaction between illness recency and face type (disfigured vs. typical), F(1, 400) = 1.87, η p 2 = .005, 90% CI = [.000, .027], p = .173."

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
    #> 1 p < .001   1503
    #> 2 p < .01     137
    #> 3 p < .05     135
    #> 4 p = .001    120
    #> 5 p = .002     93
    #> 6 p < .0001    88

We can expand the text to check the context for “p \> .25”.

``` r
gt.25 <- imprecise_ps$table |>
  filter(grepl("\\.25", text))

gt.25$expanded[1:3] # look at the first 3
```

    #> [1] "There was a significant interactive effect of time and political orientation, b = -0.09, SE = 0.04, 95% CI = [-0.16, -0.02], t(1922) = -2.49, p = .013, on the endorsement of the in-group foundation (see Table S2 in CI = [-0.14, -0.04], t(1922) = -3.59, p < .001, disappeared after 7/7, b = 0.004, SE = 0.02, 95% CI = [-0.04, 0.05], t(1922) = 0.17, p > .250 (see Fig."                    
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

    #> # A tibble: 99 × 7
    #>    text                                   section header   div     p     s id   
    #>    <chr>                                  <chr>   <chr>  <dbl> <dbl> <int> <chr>
    #>  1 Although the PTSD group showed a sign… results "Deta…    11     1     4 0956…
    #>  2 A marginally significant negative cor… results "Post…    13     1     4 0956…
    #>  3 When we more closely matched depressi… results "The …    14     3     1 0956…
    #>  4 In that analysis, the group differenc… results "Cond…    15     1     2 0956…
    #>  5 The Congruency × Alignment interactio… results "Resu…     7     2     3 0956…
    #>  6 The twoway interactions between knowl… results "Resu…     6     2     3 0956…
    #>  7 An omnibus 3 (sex ratio) × 2 (partici… results "Resu…    15     2     1 0956…
    #>  8 Further, we observed a marginally sig… fig     ""         3     1     7 0956…
    #>  9 The dagger and asterisks indicate mar… fig     "Fig.…     5     1    13 0956…
    #> 10 Given the unexpected nature of the fi… annex   "Open…    20     1    21 0956…
    #> # ℹ 89 more rows

### ref_consistency

Check if all references are cited and all citations are referenced.

``` r
ref_consistency <- module_run(paper, "ref_consistency")

ref_consistency$table
```

    #>    xref_id
    #> 1      b16
    #> 2      b17
    #> 3       b3
    #> 4      b31
    #> 5     <NA>
    #> 6     <NA>
    #> 7     <NA>
    #> 8     <NA>
    #> 9     <NA>
    #> 10    <NA>
    #>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ref
    #> 1                                                                                                                                                                                                                                                                                                                                           Hormonal correlates of pathogen disgust: Testing the compensatory prophylaxis hypothesis, B, C, Jones, A, C, Hahn, C, I, Fisher, H, Wang, M, Kandrik, J, M, Tybur, L, M, Debruine, Evolution and Human Behavior, 2018, 39, 166-169
    #> 2                                                                                                                                                                                                                                                                                     Reply to Fleischman and Fessler's (2018) comment on "Hormonal correlates of pathogen disgust: Testing the compensatory prophylaxis hypothesis, B, C, Jones, A, C, Hahn, C, I, Fisher, H, Wang, M, Kandrik, J, M, Tybur, L, M, Debruine, Evolution and Human Behavior, 2018b, 39, 470-471
    #> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                             Infection-avoidance behaviour in humans and other animals, V, A, Curtis, Trends in Immunology, 2014, 35, 457-464
    #> 4                                                                                                                                                                                                                                                                                               Parasite stress and pathogen avoidance relate to distinct dimensions of political ideology across 30 nations, J, M, Tybur, Y, Inbar, L, Aarøe, P, Barclay, F, K, Barlow, M, De Barra, ., ., Žeželj, I, Proceedings of the National Academy of Sciences, 2016, 113, 12408-12413
    #> 5  Analyses of reaction times revealed that illness recency interacted with distractor type: The 28 participants who had recently been ill took 38 ms longer to identify the targets after disfigured-face distractors relative to typicalface distractors (SD = 65.25, d z = 0.58), whereas reaction times did not differ across distractor face types for the 66 participants who had not recently been ill (mean difference = 4 ms faster to identify targets after disfiguredface distractors, SD = 58.32, d z = 0.07; J K Maner, personal communication, April 25, 2018).
    #> 6                                                                                                                                                                                                Although the studies summarized above have pointed to a relation between immunological resistance and avoidance, others have found no relation between progesterone and disgust sensitivity ( Jones et al., 2018a), infection history and disgust sensitivity (de Barra, Islam, & Curtis, 2014), and ecological pathogen stress and disgust sensitivity (Tybur et al., 2016).
    #> 7                                                                                                                                                                                                Although the studies summarized above have pointed to a relation between immunological resistance and avoidance, others have found no relation between progesterone and disgust sensitivity ( Jones et al., 2018a), infection history and disgust sensitivity (de Barra, Islam, & Curtis, 2014), and ecological pathogen stress and disgust sensitivity (Tybur et al., 2016).
    #> 8                                                                                                                                                                                                                                                                                                                                                                                             Except where noted, all methodological details-including all stimuli and dot-probe procedures-and all analyses were identical to those used by Miller and Maner (2011, Study 1).
    #> 9                                                                                                                                                                                                                                                                                                                                         All procedures from the original study (e.g., Inquisit files, stimuli, trial order, and order of questionnaires) were confirmed with the senior author from the original study ( J K Maner, personal communication, April 25, 2018).
    #> 10                                                                                                                                                                                                                                                                                                                                 According to an analysis in G*Power (Version 3.9.1.7;Faul, Erdfelder, Buchner, & Lang, 2009), this sample size affords more than 99% power to detect an interaction effect (d) of 0.65 (equivalent to that reported in the original study).
    #>     doi bibtype
    #> 1  <NA> Article
    #> 2  <NA> Article
    #> 3  <NA> Article
    #> 4  <NA> Article
    #> 5  <NA>    <NA>
    #> 6  <NA>    <NA>
    #> 7  <NA>    <NA>
    #> 8  <NA>    <NA>
    #> 9  <NA>    <NA>
    #> 10 <NA>    <NA>
    #>                                                                                                                                            title
    #> 1                                                       Hormonal correlates of pathogen disgust: Testing the compensatory prophylaxis hypothesis
    #> 2  Reply to Fleischman and Fessler's (2018) comment on "Hormonal correlates of pathogen disgust: Testing the compensatory prophylaxis hypothesis
    #> 3                                                                                      Infection-avoidance behaviour in humans and other animals
    #> 4                                   Parasite stress and pathogen avoidance relate to distinct dimensions of political ideology across 30 nations
    #> 5                                                                                                                                           <NA>
    #> 6                                                                                                                                           <NA>
    #> 7                                                                                                                                           <NA>
    #> 8                                                                                                                                           <NA>
    #> 9                                                                                                                                           <NA>
    #> 10                                                                                                                                          <NA>
    #>                                            journal  year
    #> 1                     Evolution and Human Behavior  2018
    #> 2                     Evolution and Human Behavior 2018b
    #> 3                             Trends in Immunology  2014
    #> 4  Proceedings of the National Academy of Sciences  2016
    #> 5                                             <NA>  <NA>
    #> 6                                             <NA>  <NA>
    #> 7                                             <NA>  <NA>
    #> 8                                             <NA>  <NA>
    #> 9                                             <NA>  <NA>
    #> 10                                            <NA>  <NA>
    #>                                                                          authors
    #> 1    B C Jones, A C Hahn, C I Fisher, H Wang, M Kandrik, J M Tybur, L M Debruine
    #> 2    B C Jones, A C Hahn, C I Fisher, H Wang, M Kandrik, J M Tybur, L M Debruine
    #> 3                                                                     V A Curtis
    #> 4  J M Tybur, Y Inbar, L Aarøe, P Barclay, F K Barlow, M De Barra, . . Žeželj, I
    #> 5                                                                           <NA>
    #> 6                                                                           <NA>
    #> 7                                                                           <NA>
    #> 8                                                                           <NA>
    #> 9                                                                           <NA>
    #> 10                                                                          <NA>
    #>                  id missing type                                       contents
    #> 1  0956797620955209   xrefs <NA>                                           <NA>
    #> 2  0956797620955209   xrefs <NA>                                           <NA>
    #> 3  0956797620955209   xrefs <NA>                                           <NA>
    #> 4  0956797620955209   xrefs <NA>                                           <NA>
    #> 5  0956797620955209     bib bibr Maner, personal communication, April 25, 2018)
    #> 6  0956797620955209     bib bibr                         ( Jones et al., 2018a)
    #> 7  0956797620955209     bib bibr                           (Tybur et al., 2016)
    #> 8  0956797620955209     bib bibr               Miller and Maner (2011, Study 1)
    #> 9  0956797620955209     bib bibr Maner, personal communication, April 25, 2018)
    #> 10 0956797620955209     bib bibr                              (Version 3.9.1.7;
    #>    section div  p  s
    #> 1     <NA>  NA NA NA
    #> 2     <NA>  NA NA NA
    #> 3     <NA>  NA NA NA
    #> 4     <NA>  NA NA NA
    #> 5    intro   1  2  5
    #> 6    intro   1  5  2
    #> 7    intro   1  5  2
    #> 8   method   2  1  1
    #> 9   method   2  1  2
    #> 10  method   3  1  5

It looks like there are some references with missing citations. The
first one doesn’t look like a reference, and grobid often parses tables
oddly. You’d need to look at the original PDF to see if the others are
actually missing or false positives. Here, they are all false positives,
based on grobid not being able to match the in-text citation to the
reference list.

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
    #>                          raw error decision_error one_tailed_in_txt apa_factor
    #> 1  t(248.4) = 2.01, p = .023  TRUE          FALSE             FALSE          1
    #> 2 t(248.4) = -4.55, p < .001 FALSE          FALSE             FALSE          1
    #>                                                                                                                                                                                                                                                                                                text
    #> 1 Yes-the 90% confidence intervals of the difference in attentional bias for participants who were and were not recently ill found here (d z = -0.14, 90% CI = [-0.31, -0.04]) did not overlap with an effect size (d z ) of -0.35, t(248.4) = 2.01, p = .023, or 0.35, t(248.4) = -4.55, p < .001.
    #> 2 Yes-the 90% confidence intervals of the difference in attentional bias for participants who were and were not recently ill found here (d z = -0.14, 90% CI = [-0.31, -0.04]) did not overlap with an effect size (d z ) of -0.35, t(248.4) = 2.01, p = .023, or 0.35, t(248.4) = -4.55, p < .001.
    #>   section  header div p s               id
    #> 1 results Results   9 3 3 0956797620955209
    #> 2 results Results   9 3 3 0956797620955209

Here we see a false positive, where the paper reported the results of an
equivalence test, which are meant to be one-tailed, but statcheck did
not detect that this was one-tailed.

In the full PsychSci set, there are more than 27K sentences with numbers
to check, so this takes about a minute to run.

``` r
statcheck_ps <- module_run(psychsci, "statcheck")
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

    #>                  id p_values n_imprecise marginal
    #> 1  0956797613520608        6           0        0
    #> 2  0956797614522816       39           0        0
    #> 3  0956797614527830       13           2        0
    #> 4  0956797614557697       27           8        0
    #> 5  0956797614560771        4           1        0
    #> 6  0956797614566469        0           0        0
    #> 7  0956797615569001       25          20        0
    #> 8  0956797615569889       26           0        4
    #> 9  0956797615583071       24           2        0
    #> 10 0956797615588467       21           4        0
