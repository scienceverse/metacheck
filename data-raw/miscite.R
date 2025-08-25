miscite <- data.frame(
  doi = c("10.1525/collabra.33267", "10.1016/j.jml.2012.11.001", "10.1371/journal.pone.0090779"),
  reftext = c(
    "Lakens D. (2022). Sample Size Justification. _Collabra Psychology_, *8*(1), 33267.",
    "Barr, D. J., Levy, R., Scheepers, C., & Tily, H. J. (2013). Random effects structure for confirmatory hypothesis testing: Keep it maximal. _Journal of Memory and Language_, *68*(3), 255-278.",
    "McAleer, P., Todorov, A., & Belin, P. (2014). How do you say ‘Hello’? Personality impressions from brief novel voices. _PloS one_, *9*(3), e90779."
  ),
  warning = c(
    "This is often miscited to justify small sample sizes. Authors often only write that the sample size was judged based on feasibility, citing Lakens (2022). But this paper says some sample sizes are too small to provide useful information, and asks authors to additionally consider whether their study is mainly input for a future meta-analysis, whether a decision needs to be made, to report the critical effect size (the smallest effect size that can be significant), the width of the confidence interval, and which effect sizes can be detected with sufficient power. If these points are not considered, the authors are not following the advice of Lakens (2022).",
    "The article by Barr and colleagues is sometimes miscited to justify including all random effects in models of observational data, but it is mainly applicable to experimental data.",
    "This paper is often cited as \"people make rapid judgements of personality impressions from peoples' voices\", however, we never analysed reaction times. More accurately it would be \"people make judgements of personality from short exerpts of peoples' voices\". The voices were short (<500 msecs) but we never looked at how long it take to make those judgements."
  )
)

saveRDS(miscite, "inst/databases/miscite.Rds", compress = "xz")
