# Journal Article Reporting Standards

JARS (journal article reporting standards), by the American
Psychological Association offer guidance on the information that should
be reported in scientific articles to enhance their scientific rigour
(Appelbaum et al. 2018). The guidelines for quantitative research are a
set of excellent recommendations, and almost every published scientific
article would be improved if researchers actually followed JARS.

However, as the guidelines are not well known, authors usually do not
implement them, and reviewers do not check if journal article reporting
standards are followed. Furthermore, there are so many guidelines, it
would take a lot of time to check them all manually. Automation can help
increase awareness of JARS by systematically checking if recommendations
are followed, and if not, point out where improvements can be made.
Below we will illustrate how 2 JARS guidelines can be automatically
checked. There are dozens of other potential guidelines for which
dedicated metacheck modules could be created. Anyone who has created an
R package will have the experience of running R CMD check, which
automatically checks dozens of requirements that an R package must
adhere to before it is allowed on CRAN. It should be possible to
automatically check many of the JARS guidelines in a similar manner.

## Exact p-values

The first reporting guideline we will illustrate is to report exact
p-values. The APA Manual states:

> Report exact p values (e.g., p = .031) to two or three decimal places.
> However, report p values less than .001 as p \< .001.

Reporting *p* values precisely allows readers to include the test
results in *p* value meta-analytic tests, such as p-curve, or z-curve
(Simonsohn et al. 2014; Bartoš and Schimmack 2020), and makes it
possible the check the internal coherence of the reported result with
tools such as Statcheck (Nuijten et al. 2015). metacheck has a dedicated
module, “exact-p”, to identify the presence of imprecise p-values. We
can run it on a single paper:

``` r

res_imprecise <- module_run(psychsci$'0956797614560771', "stat_p_exact")

res_imprecise
```

Exact P-Values: We found 1 imprecise *p* value out of 4 detected *p*
values.

The module returns the exact *p* values and the full sentence so that
users can easily examine whether the reported *p* values should have
been precise:

| text | expanded |
|:---|:---|
| p \< .01 | We repeated our analysis including all eight valenced emotions and found that participants’ recognition was again significantly better than chance, both when the distractor and target were of opposite valence, t(28) = 8.30, p \< .001, Cohen’s d = 1.54 (mean difference = 21.37, 95% CI of the difference = \[16.09, 26.64\]), and when they had the same valence, t(28) = 2.79, p \< .01, Cohen’s d = 0.52 (mean difference = 6.71, 95% CI of the difference = \[1.79, 11.63\]; see Fig. 1). |

Luckily, there are also many papers that follow the JARS guideline and
report all *p* values correctly, for example:

``` r

module_run(psychsci$`0956797616665351`, "stat_p_exact")
```

Exact P-Values: We found no imprecise *p* values or *p*-values of
exactly zero out of 8 detected.

## Reporting standardized effect sizes

A second JARS guideline that can be automatically checked is whether
people report effect sizes alongside their test result. Each test (e.g.,
a *t*-test, *F*-test, etc.) should include the corresponding effect size
(e.g., a Cohen’s d, or partial eta-squared). Based on a text search that
uses regular expressions (regex), we can identify *t*-tests and
*F*-tests that are not followed by an effect size, and warn researchers
accordingly.

``` r

module_run(
  paper = psychsci$`0956797616657319`,
  module = "stat_effect_size"
)
```

Effect Sizes in t-tests and F-tests: We found 10 t-tests and/or F-tests
where effect sizes are not reported. Check these tests in the table
below, and consider adding effect sizes

## Checking Multiple Papers

You can also run modules for multiple papers at once and get a summary
table.

``` r

mo <- module_run(psychsci[1:10], "stat_effect_size")

mo$summary_table
```

| paper_id | ttests_with_es | ttests_without_es | Ftests_with_es | Ftests_without_es |
|:---|---:|---:|---:|---:|
| 0956797613520608 | 0 | 0 | 6 | 0 |
| 0956797614522816 | 6 | 2 | 28 | 0 |
| 0956797614527830 | 0 | 0 | 0 | 0 |
| 0956797614557697 | 0 | 2 | 5 | 0 |
| 0956797614560771 | 4 | 0 | 0 | 0 |
| 0956797614566469 | 0 | 0 | 0 | 0 |
| 0956797615569001 | 1 | 1 | 0 | 0 |
| 0956797615569889 | 0 | 0 | 19 | 0 |
| 0956797615583071 | 8 | 7 | 2 | 3 |
| 0956797615588467 | 5 | 5 | 0 | 1 |

This can be useful for meta-scientific research questions, such as
whether there is an increase in the best practice to report effect sizes
for *t*-tests over time. In the plot below we have run the module on
1838 published articles in the journal Psychological Science between
2014 and 2024. We can see that where a decade ago, close to half the
reported t-tests would not be followed by an effect size, but a decade
later, this holds for only around 25% of tests. Perhaps the introduction
of a tool like metacheck can reduce this percentage even further
(although it does not need to be 0, as we discuss below).

Our main point is to demonstrate that it is relatively easy to answer
some meta-scientific questions with metacheck. Editors could easily
replicate the plot below for articles in their own journal, and see
which practices they should improve. As we highlighted in the
[introductory blog
post](https://daniellakens.blogspot.com/2025/06/introducing-metacheck.html),
when modules are used for metascience, they need to be validated, and
have low error rates. We have manually checked the *t*-tests and
*F*-tests in 250 papers in Psychological Science, and our effect_size
module detected 100% of *t*-tests with effect sizes, 99% of *t*-tests
without effect sizes, 99% of *F*-tests with effect sizes, and 100% of
*F*-tests without effect sizes. This is accurate enough for
meta-scientific research.

![Plot of average percentage of t-tests missing effect sizes from 2014
to 2024, showing a downward trend from 60% to around
20%](effect-size-plot.png)

Plot of average percentage of t-tests missing effect sizes from 2014 to
2024, showing a downward trend from 60% to around 20%

## Improving the Modules

These two examples are relatively straightforward examples of text
searches that can identify examples where researchers do not follow
reporting guidelines. Still, these algorithms can be improved.

For example, the module to detect effect sizes following t-tests only
matches standardized effect sizes, but it is not always necessary to
compute a standardized effect size. If a future meta-analysis would be
based on raw scores, and means and standard deviations are reported, it
might not be needed to report an effect size. Alternatively, we might
just accept a tool that has a relatively high Type 1 error rate when
checking our manuscript. After all, a spellchecker has a high Type 1
error rate, underlining many names and abbreviations that are correct,
but that it does not recognize, and most people use spellcheckers all
the time, as any errors they successfully catch make it worthwhile to
read over the Type 1 errors and dismiss them. Despite the room for
improvement, even these simple text searches can already identify places
where published articles could have been improved by adding effect
sizes.

There are many more algorithms that can be added to detect other
information that should be reported according to the JARS guidelines. If
you would like to create and/or validate such a module, do reach out. We
are happy to collaborate.

## References

Appelbaum, Mark, Harris Cooper, Rex B. Kline, Evan Mayo-Wilson, Arthur
M. Nezu, and Stephen M. Rao. 2018. “Journal Article Reporting Standards
for Quantitative Research in Psychology: The APA Publications and
Communications Board Task Force Report.” *American Psychologist* 73 (1):
3–25. <https://doi.org/10.1037/amp0000191>.

Bartoš, František, and Ulrich Schimmack. 2020. *Z-Curve.2.0: Estimating
Replication Rates and Discovery Rates*. January.
<https://doi.org/10.31234/osf.io/urgtn>.

Nuijten, Michèle B., Chris H. J. Hartgerink, Marcel A. L. M. van Assen,
Sacha Epskamp, and Jelte M. Wicherts. 2015. “The Prevalence of
Statistical Reporting Errors in Psychology (1985–2013).” *Behavior
Research Methods*, ahead of print, October.
<https://doi.org/10.3758/s13428-015-0664-2>.

Simonsohn, Uri, Leif D. Nelson, and Joseph P. Simmons. 2014. “P-Curve: A
Key to the File-Drawer.” *Journal of Experimental Psychology: General*
143 (2): 534–47. https://doi.org/<https://doi.org/10.1037/a0033242>.
