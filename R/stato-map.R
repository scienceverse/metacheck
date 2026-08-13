# Column-header -> ontology IRI mapping for statistical result tables extracted
# from JASP (.jasp), jamovi (.omv) and executed R output.
#
# FOUR-TIER typing (see stato_type_column):
#   1. a column name -> a verified STATO class IRI (.STATO_MAP + .STATO_LABELS);
#   2. a column name -> a metacheck-minted term (.MC_STAT_MAP + .MC_STAT_LABELS)
#      for statistics STATO has no class for;
#   3. the same, after stripping a trailing variant/correction suffix
#      (f[gg], p[hf], stat[stud]) — see .stato_strip_variant();
#   4. FALLBACK: no class at all -> the column's own header text is the type
#      (annotationValue = header, no termAccession). Nothing is ever dropped.
#
# The jamovi titles come from the 28 `.r.yaml` results-definitions in the jmv
# package (github.com/jamovi/jmv); the full title list is bundled at
# inst/schema/stato/jamovi-column-titles.txt.
#
# EVERY STATO IRI here was verified via the EBI OLS4 API
# (https://www.ebi.ac.uk/ols4) against the `stato` ontology — they are NOT
# guessed. Verify with the TERM endpoint, not search: OLS4's search index
# returns ids (e.g. STATO:0000669 "sum") whose term lookup 404s, and a
# plausible-looking id can belong to something else entirely — STATO:0000505 and
# STATO:0000506 look like they should be sum-of-squares and mean-square but are
# actually "quadrat sampling" and "cluster sampling". Check with:
#   curl "https://www.ebi.ac.uk/ols4/api/ontologies/stato/terms?iri=\
#   http%3A%2F%2Fpurl.obolibrary.org%2Fobo%2FSTATO_0000033"

# The STATO accession -> canonical label, for the ones we type. Keeping the label
# lets the ISA OntologyAnnotation carry a human-readable annotationValue too.
.STATO_LABELS <- c(
  "STATO:0000176" = "t-statistic",
  "STATO:0000282" = "F-statistic",
  "STATO:0000030" = "Chi-Squared statistic",
  "STATO:0000376" = "Z-statistic",
  "STATO:0000700" = "p-value",
  "STATO:0000069" = "number of degrees of freedom",
  "STATO:0000498" = "numerator degrees of freedom",
  "STATO:0000527" = "denominator degrees of freedom",
  "STATO:0000618" = "Cohen's d",
  "STATO:0000319" = "Hedges's g",
  "STATO:0000320" = "Glass's delta",
  "STATO:0000317" = "eta-squared",
  "STATO:0000318" = "omega-squared",
  "STATO:0000182" = "odds ratio",
  "STATO:0000280" = "Pearson's correlation coefficient",
  "STATO:0000266" = "Bayes factor",
  "STATO:0000401" = "sample mean",
  "STATO:0000684" = "standard deviation for sample",
  "STATO:0000643" = "variance for sample",
  "STATO:0000037" = "standard error of the mean",
  "STATO:0000196" = "confidence interval",
  "STATO:0000088" = "study group population size",
  "STATO:0000047" = "count",
  "STATO:0000639" = "percentage",
  "STATO:0000565" = "regression coefficient",
  "STATO:0000471" = "estimate",
  "STATO:0000656" = "slope",
  "STATO:0000657" = "intercept",
  "STATO:0000562" = "standard error of estimate",
  "STATO:0000550" = "log likelihood",
  "STATO:0000033" = "mode",
  "STATO:0000035" = "range",
  "STATO:0000068" = "skewness",
  "STATO:0000178" = "kurtosis",
  "STATO:0000150" = "minimum observed value",
  "STATO:0000151" = "maximum observed value",
  "STATO:0000164" = "inter quartile range",
  "STATO:0000564" = "coefficient of determination",
  "STATO:0000409" = "likelihood ratio",
  "STATO:0000085" = "effect size estimate",
  "STATO:0000648" = "standard error of the difference between independent means",
  "STATO:0000703" = "posterior probability",
  "STATO:0000377" = "deviance",
  "STATO:0000375" = "residual mean square",
  "STATO:0000236" = "coefficient of variation",
  "STATO:0000325" = "Akaike information criterion",
  "STATO:0000327" = "Bayesian information criterion",
  "STATO:0000702" = "prior probability",
  "STATO:0000201" = "Spearman's rank correlation coefficient",
  # Verified via the OLS4 TERM endpoint (not just search) 2026-07-30: SPSS's
  # GLM "Observed Power" column, e.g. in a Tests of Within-Subjects Effects
  # table.
  "STATO:0000200" = "statistical test power",
  # Verified via OLS4 2026-07-30, for typing SPSS (.spv) row labels (see
  # .spv_stato_type_label()): SPSS's logistic-regression "Wald Chi-Square"
  # column.
  "STATO:0000584" = "Wald statistic",
  # SPSS's "Model Fitting Criterion / -2 Log Likelihood". The underlying
  # quantity IS the log-likelihood; the "-2" is a display/reporting
  # convention (deviance-scaled), not a different statistic.
  "STATO:0000550" = "log likelihood",
  # SPSS's Factor Analysis "% of Variance" column.
  "STATO:0000587" = "percentage of variance",
  # SPSS's Paired Samples Correlations "Correlation" column and Reliability
  # Analysis's "Corrected/Correlated Item-Total Correlation" -- both are
  # ordinary correlation coefficients (the latter specifically a Pearson
  # correlation between one item and the sum of the rest), so the GENERIC
  # correlation class is the right specificity rather than minting a
  # narrower one.
  "STATO:0000142" = "correlation coefficient",
  # SPSS Factor Analysis / PCA's "Eigenvalue" column (Total Variance
  # Explained table). Verified via OLS4 term endpoint 2026-07-30.
  "STATO:0000566" = "eigen value",
  # SPSS Explore's "5% Trimmed Mean". Verified via OLS4 term endpoint
  # 2026-07-30.
  "STATO:0000163" = "trimmed mean",
  # SPSS's GENLIN/GEE "Score" statistic (a likelihood-ratio-test family
  # member alongside Wald). Verified via OLS4 term endpoint 2026-07-30.
  "STATO:0000560" = "Rao's score"
)

# ── metacheck-minted statistic terms ─────────────────────────────────────────
# Statistics that JASP/jamovi/R report routinely but for which the STATO release
# served by OLS4 has NO class. Each was searched for on OLS4 before being minted
# here; the note on each records what was checked and why the nearest STATO term
# does not fit. These carry the metacheck namespace, exactly as the open-science
# booleans in the RO-Crate output do (metacheck:hasSharedData etc.) — they are
# metacheck's own vocabulary, NOT a claim about STATO, and a consumer can tell
# the difference from the IRI alone.
#
# These are all standard statistics whose absence looks like a genuine gap
# rather than a deliberate exclusion, so they are reasonable candidates to
# propose upstream (https://github.com/ISA-tools/STATO). Until/unless STATO
# adopts them, the metacheck IRI is what travels with the data; if STATO later
# mints an equivalent, move the entry to .STATO_MAP and the change is invisible
# to everything downstream.
.MC_STAT_NS <- "https://scienceverse.org/schema/metacheck/statistics/"

.MC_STAT_LABELS <- c(
  # OLS4 has only "residual mean square" (STATO:0000375), which is the residual
  # row specifically and is wrong for a between-groups/effect row.
  "sumOfSquares"           = "sum of squares",
  "meanSquare"             = "mean square",
  # OLS4 returns nothing for either; both are distinct quantities from
  # eta-squared (STATO:0000317) and must not be collapsed into it.
  "partialEtaSquared"      = "partial eta-squared",
  "generalizedEtaSquared"  = "generalized eta-squared",
  # OLS4 has "median difference" (STATO:0000617) but no plain mean difference.
  "meanDifference"         = "mean difference",
  # STATO has no plain "median": searching returns only compounds (median
  # difference, median time-to-event, median of the ratios corrected count).
  "median"                 = "median",
  # Only "inter quartile range" (STATO:0000164) exists — not the quantile
  # values themselves, which jamovi reports as quart1-3 / quant1-3.
  "quantile"               = "quantile",
  # Distinct from a count of observations (STATO:0000047): this counts the
  # cases that were ABSENT, which is a data-quality quantity.
  "missingCount"           = "number of missing values",
  # STATO:0000077 is the Shapiro-Wilk TEST, not its W statistic; typing a value
  # with a test class is the same category error ISA made calling a t-test a
  # "Material".
  "shapiroWilkW"           = "Shapiro-Wilk W statistic",
  # STATO:0000070 is Yate's corrected chi-squared TEST, not the corrected
  # statistic value.
  "correctedChiSquared"    = "continuity-corrected Chi-Squared statistic",
  # Adjusted R² is a different quantity from R² (STATO:0000564) — it penalises
  # for the number of predictors — and OLS4 has no class for it.
  "adjustedRSquared"       = "adjusted coefficient of determination",
  # A contingency table's EXPECTED cell frequency (what the null model predicts)
  # is a different quantity from the observed count (STATO:0000047). OLS4 has no
  # class for it: searching "expected frequency" returns only the
  # Cochran-Armitage trend statistic.
  "expectedCount"          = "expected cell frequency",
  # STATO:0000199 is Mauchly's TEST for sphericity, not the W statistic it
  # yields — the same test-vs-statistic distinction as Shapiro-Wilk above.
  "mauchlyW"               = "Mauchly's W statistic",
  # The Greenhouse-Geisser / Huynh-Feldt sphericity-correction epsilons, as
  # reported in a "Tests of Sphericity" table. These are the CORRECTION FACTORS
  # themselves (a value near 1 means sphericity holds), distinct from the
  # corrected statistics they are applied to. OLS4 has no class for either.
  "greenhouseGeisserEpsilon" = "Greenhouse-Geisser epsilon",
  "huynhFeldtEpsilon"        = "Huynh-Feldt epsilon",
  # OLS4 returns no match for "intraclass correlation".
  "intraclassCorrelation"  = "intraclass correlation coefficient",
  # STATO's search index lists STATO:0000669 "sum", but its term endpoint 404s,
  # so that id cannot be verified and must not be emitted (checked again when
  # this entry was added). The nearest resolvable classes are all specialised
  # (sum contrast, rank sums), none of which is the plain sum of a variable's
  # values that a Descriptives table reports.
  "sum"                    = "sum of values",
  # Test statistics R prints as a bare letter, resolved from the producing call
  # (see .STATO_BY_CALL). STATO has the corresponding TESTS but not the
  # statistics they yield, the same gap as Shapiro-Wilk/Mauchly above.
  "wilcoxonW"              = "Wilcoxon rank sum statistic",
  "wilcoxonV"              = "Wilcoxon signed rank statistic",
  "spearmanS"              = "Spearman's S statistic",
  "kolmogorovSmirnovD"     = "Kolmogorov-Smirnov D statistic",
  # Same gap, for scipy.stats' printed result objects (see .STATO_BY_CALL's
  # "mannwhitneyuresult"/"kruskalresult" entries): OLS4 has "Mann-Whitney
  # U-test" (STATO:0000076) and "Kruskal Wallis test" (STATO:0000094), the
  # TESTS, but no class for the U or H statistic VALUES themselves -- verified
  # directly against OLS4's search (no result for "U statistic"/"H statistic"
  # beyond the test classes already found). Observed in a real ~220-notebook
  # Zenodo sample of scipy.stats/statsmodels/pingouin-using papers.
  "mannWhitneyU"           = "Mann-Whitney U statistic",
  "kruskalWallisH"         = "Kruskal-Wallis H statistic",
  # JASP reports "error %" beside a Bayes factor: the proportional error of the
  # numerical/Monte-Carlo integration used to compute it, i.e. how precisely the
  # BF itself is known. OLS4 has no class for it ("Monte Carlo error" returns
  # nothing); it is not a measurement error in the STATO:0000705 sense.
  "bayesFactorError"       = "Bayes factor proportional error",
  # OLS4 has Pearson's r (STATO:0000280) and Spearman's rho (STATO:0000201) but
  # returns no match for Kendall's tau, which is a distinct coefficient.
  "kendallTau"             = "Kendall's tau correlation coefficient",
  # SPSS's GLM "Noncent. Parameter" (Tests of Within/Between-Subjects
  # Effects). OLS4 has no match for "noncentrality parameter" or bare
  # "noncentrality" (checked 2026-07-30); it is a distinct quantity from the
  # test statistic itself (the noncentral F/chi-squared distribution's shape
  # parameter, used to compute observed power), not a synonym for F or df.
  "noncentralityParameter" = "noncentrality parameter",
  # Logistic regression pseudo-R² statistics: OLS4 has no match for either
  # (checked 2026-07-30). Distinct FORMULAS from each other and from the
  # ordinary/adjusted R² (STATO:0000564 / metacheck:adjustedRSquared) used
  # for linear models, so neither is folded into those.
  "nagelkerkeRSquared"     = "Nagelkerke pseudo R-squared",
  "coxSnellRSquared"       = "Cox and Snell pseudo R-squared",
  # SPSS Reliability Analysis reports Cronbach's alpha and per-item diagnostics
  # (Scale Variance/Mean if Item Deleted). OLS4 has no match for "Cronbach's
  # alpha" or "reliability coefficient" (checked 2026-07-30).
  "cronbachsAlpha"         = "Cronbach's alpha",
  # McDonald's omega: a distinct, increasingly-preferred reliability
  # coefficient, NOT interchangeable with Cronbach's alpha (different
  # assumptions/formula). OLS4 has no match (checked 2026-07-30).
  "mcdonaldsOmega"         = "McDonald's omega",
  # A bootstrap estimate's bias (observed - resampled mean). OLS4 has no
  # class for "bias" as a statistical quantity (checked 2026-07-30) -- its
  # hits are all unrelated (funnel plot, imputation, group randomization).
  "bootstrapBias"          = "bootstrap bias",
  "scaleVarianceIfDeleted" = "scale variance if item deleted",
  "scaleMeanIfDeleted"     = "scale mean if item deleted",
  # SPSS GEE model-fit criteria (Quasi Likelihood under Independence Model
  # Criterion, QIC/QICC/corrected QIC) -- distinct quantities from AIC/BIC
  # (already-typed STATO classes), specific to GEE. OLS4 has no match for
  # "quasi likelihood" or "QIC" (checked 2026-07-30).
  "qic"                    = "Quasi Likelihood under Independence Model Criterion",
  # Regression collinearity diagnostics (a Coefficients table's "Collinearity
  # Statistics / Tolerance" and "/ VIF" columns). OLS4 has no match for
  # either (checked 2026-07-30).
  "tolerance"              = "tolerance (collinearity diagnostic)",
  "vif"                    = "variance inflation factor",
  # Rarer model-selection information criteria (seen in a GENLIN distribution-
  # comparison table). OLS4 has AIC/BIC/AICC/DIC (already mapped) but no
  # match for either (checked 2026-07-30); both are AIC-family variants with
  # a different penalty term, distinct enough from plain AIC/BIC to keep
  # separate rather than collapsing into those classes.
  "bozdoganIC"             = "Bozdogan information criterion",
  "hurvichTsaiIC"          = "Hurvich-Tsai information criterion"
)

.MC_STAT_MAP <- c(
  "ss"                      = "sumOfSquares",
  "sum of squares"          = "sumOfSquares",
  "sum sq"                  = "sumOfSquares",
  "sumsq"                   = "sumOfSquares",
  # R's anova() spellings for the same quantity.
  "rss"                     = "sumOfSquares",
  "sum of sq"               = "sumOfSquares",
  "ms"                      = "meanSquare",
  "mean square"             = "meanSquare",
  "mean sq"                 = "meanSquare",
  "meansq"                  = "meanSquare",
  "parteta"                 = "partialEtaSquared",
  "partialeta"              = "partialEtaSquared",
  "partial eta squared"     = "partialEtaSquared",   # SPSS's own spelling
  "etasqp"                  = "partialEtaSquared",
  "ges"                     = "generalizedEtaSquared",
  "etasqg"                  = "generalizedEtaSquared",
  "md"                      = "meanDifference",
  "mean difference"         = "meanDifference",
  "meandiff"                = "meanDifference",
  "median"                  = "median",
  "med"                     = "median",
  "quart1"                  = "quantile", "quart2" = "quantile", "quart3" = "quantile",
  "quant1"                  = "quantile", "quant2" = "quantile", "quant3" = "quantile",
  "quantile"                = "quantile",
  "missing"                 = "missingCount",
  "n_missing"               = "missingCount",
  "system-missing"          = "missingCount",   # SPSS's own qualifier for the same count
  "user-missing"            = "missingCount",
  "sw"                      = "shapiroWilkW",
  "shapiro-wilk w"          = "shapiroWilkW",
  "chisqcorr"               = "correctedChiSquared",
  "ar2"                     = "adjustedRSquared",
  "adj. r²"                 = "adjustedRSquared",
  "adjusted r²"             = "adjustedRSquared",
  "adjusted r square"       = "adjustedRSquared",   # SPSS's own spelling
  "adj. r-squared"          = "adjustedRSquared",   # statsmodels' .summary() spelling
  "noncent. parameter"      = "noncentralityParameter",
  "nagelkerke r square"     = "nagelkerkeRSquared",
  "cox & snell r square"    = "coxSnellRSquared",
  "cronbach's alpha"        = "cronbachsAlpha",
  "cronbachs alpha"         = "cronbachsAlpha",   # no-apostrophe real-file spelling
  "cronbachs alpha if item deleted" = "cronbachsAlpha",
  "mcdonald omega"          = "mcdonaldsOmega",
  "mcdonald omega if item deleted" = "mcdonaldsOmega",
  "scale variance if item deleted" = "scaleVarianceIfDeleted",
  "scale mean if item deleted"     = "scaleMeanIfDeleted",
  "quasi likelihood under independence model criterion (qic)" = "qic",
  "corrected quasi likelihood under independence model criterion (qicc)" = "qic",
  "bias"                    = "bootstrapBias",   # already Bootstrap-prefix-stripped
  "tolerance"               = "tolerance",
  "vif"                     = "vif",
  "bozdogan ic"             = "bozdoganIC",
  "hurvich tsai ic"         = "hurvichTsaiIC",
  "expected"                = "expectedCount",
  "expected count"          = "expectedCount",
  "mauch"                   = "mauchlyW",
  "mauchly's w"             = "mauchlyW",
  # NOTE: these two keys are only the EPSILON when they stand alone (a Tests of
  # Sphericity column). As a bracket suffix — ss[gg], p[hf] — they name the
  # correction applied to another statistic, and .stato_strip_variant() strips
  # them before lookup, so that case never reaches here.
  "gg"                      = "greenhouseGeisserEpsilon",
  "hf"                      = "huynhFeldtEpsilon",
  "icc"                     = "intraclassCorrelation",
  "intraclass correlation"  = "intraclassCorrelation",   # SPSS's own spelled-out label
  "sum"                     = "sum",
  # Only the explicit "error %" spelling: a bare "err"/"error" also appears in
  # jamovi's t-test tables, where it is not this quantity.
  "error %"                 = "bayesFactorError",
  "tau"                     = "kendallTau"
)

# Column-title (as JASP/jamovi render it, lower-cased for matching) -> STATO id.
# Multiple header spellings can map to the same class (e.g. "sd", "std. deviation").
.STATO_MAP <- c(
  # test statistics
  "t"                       = "STATO:0000176",
  "f"                       = "STATO:0000282",
  "χ²"            = "STATO:0000030",   # chi-square symbol
  "chi-squared"             = "STATO:0000030",
  "chi-square"              = "STATO:0000030",
  "z"                       = "STATO:0000376",
  # p-values
  "p"                       = "STATO:0000700",
  "p-value"                 = "STATO:0000700",
  # degrees of freedom
  "df"                      = "STATO:0000069",
  "hypothesis df"           = "STATO:0000069",   # SPSS GLM Multivariate Tests
  "error df"                = "STATO:0000069",
  "df1"                     = "STATO:0000498",
  "df2"                     = "STATO:0000527",
  "num df"                  = "STATO:0000498",
  "den df"                  = "STATO:0000527",
  # oneway.test()'s Welch one-way test prints its own denominator df header as
  # "denom df", not "den df" — a distinct literal string from the other df
  # keys above (stato_type_column() looks up an exact, case-folded key with no
  # fuzzy matching), so it needs its own entry rather than relying on "den df".
  "denom df"                = "STATO:0000527",
  # effect sizes
  "cohen's d"               = "STATO:0000618",
  "hedges' g"               = "STATO:0000319",
  "hedges's g"              = "STATO:0000319",
  "glass' delta"            = "STATO:0000320",
  "η²"           = "STATO:0000317",   # eta-squared
  "ω²"           = "STATO:0000318",   # omega-squared
  "odds ratio"              = "STATO:0000182",
  "exp(b)"                  = "STATO:0000182",   # SPSS logistic regression's own label
  # descriptives / dispersion
  "mean"                    = "STATO:0000401",
  "sd"                      = "STATO:0000684",
  "std. deviation"          = "STATO:0000684",
  "standard deviation"      = "STATO:0000684",
  "variance"                = "STATO:0000643",
  "se"                      = "STATO:0000037",
  "standard error"          = "STATO:0000037",
  "std. error"              = "STATO:0000037",
  "n"                       = "STATO:0000088",
  "n / valid"               = "STATO:0000088",   # SPSS Frequencies' own compound label
  "n / missing"             = "STATO:0000047",   # a count of ABSENT cases, not sample size
  # correlation
  "r"                       = "STATO:0000280",
  "pearson's r"             = "STATO:0000280",
  "pearson correlation"     = "STATO:0000280",   # SPSS Correlations table's own label
  "correlation"             = "STATO:0000142",   # Paired Samples Correlations' own label
  "correlation coefficient" = "STATO:0000142",
  "correlated item-total correlation" = "STATO:0000142",   # reliability analysis
  "corrected item-total correlation"  = "STATO:0000142",
  # scipy.stats.linregress()'s printed field names (a LinregressResult repr,
  # read via .ipynb_stat_line() in R/stat-tables.R): "rvalue" is the same
  # Pearson's r the "r"/"pearson's r" keys above already map, just under
  # scipy's own field name. "slope"/"intercept" are verified STATO classes in
  # their own right (OLS4 search: "slope" -> STATO:0000656, "intercept" ->
  # STATO:0000657 — general graph/line-fit quantities, not regression-specific
  # classes, but a semantic match for a fitted line's own slope/intercept).
  # "stderr" is linregress's name for the slope estimate's standard error —
  # distinct from the SAMPLE-MEAN standard error "se"/"standard error" already
  # map to (STATO:0000037) above, so it cannot reuse that key without a
  # category mismatch; OLS4 has no regression-slope-specific SE class, so this
  # uses the general "standard error of estimate" (STATO:0000562), verified as
  # the closest class whose own definition is explicitly quantity-agnostic
  # ("uncertainty associated with the estimate", not tied to a sample mean).
  "rvalue"                  = "STATO:0000280",
  "slope"                   = "STATO:0000656",
  "intercept"               = "STATO:0000657",
  "stderr"                  = "STATO:0000562",
  # bayesian
  "bf₁₀"         = "STATO:0000266",   # BF10
  "bf₀₁"         = "STATO:0000266",   # BF01
  "bayes factor"            = "STATO:0000266",
  # R console-output spellings (summary(lm)/aov/anova/t.test/cor.test), so the
  # captured-R-output path types the same statistics as the JASP/jamovi path.
  "t value"                 = "STATO:0000176",   # lm/anova t column
  "z value"                 = "STATO:0000376",   # glm z column
  "f value"                 = "STATO:0000282",   # aov/anova F column
  "pr(>|t|)"                = "STATO:0000700",   # lm p column
  "pr(>|z|)"                = "STATO:0000700",   # glm p column
  # statsmodels' OLSResults/Logit/GLM .summary() printed text (parsed via the
  # shared .r_output_tables() fixed-width parser, R/r-output.R, reused as-is
  # for this Python output the same way .r_output_oneline() is reused for
  # bare scipy Result reprs) — its OWN column spellings, different from R's
  # equivalent lm/glm output above ("coef" not "estimate", "std err" not
  # "std. error", bare "t"/"p>|t|" not "t value"/"pr(>|t|)"). Confirmed real
  # via a Zenodo-sampled notebook's OLS Regression Results table.
  "coef"                    = "STATO:0000471",   # generic estimate, same as R's "estimate"
  "std err"                 = "STATO:0000037",
  "p>|t|"                   = "STATO:0000700",   # OLS/WLS/GLS t-based p column
  "p>|z|"                   = "STATO:0000700",   # Logit/GLM z-based p column
  # statsmodels' .summary() KEY-VALUE header block (parsed by .ipynb_stat_kv(),
  # R/stat-tables.R — a two-"Label: value"-pairs-per-line shape neither
  # .r_output_tables() nor .r_output_oneline() can parse, so it needs its own
  # extractor; see that function's own comment). Manuscript-reportable
  # model-fit statistics ("the model explained significant variance, R² =
  # .84, F(6, 93) = 84.18, p < .001") that were previously not extracted at
  # all. Confirmed real and stable across two independently-sampled Zenodo
  # notebooks' OLS output.
  "r-squared"               = "STATO:0000564",
  "f-statistic"             = "STATO:0000282",
  "prob (f-statistic)"      = "STATO:0000700",
  "log-likelihood"          = "STATO:0000550",
  "no. observations"        = "STATO:0000088",
  "df residuals"            = "STATO:0000069",
  # "adj. r-squared" is METACHECK-minted (.MC_STAT_MAP's "adjustedRSquared" —
  # see .MC_STAT_LABELS's own comment on why OLS4 has no adjusted-R² class),
  # not listed here; "df model" and "covariance type" have no verified STATO/
  # metacheck term and stay untyped nominal labels (df model is a model-level
  # PREDICTOR COUNT, not a test's own degrees of freedom — reusing STATO:0000069
  # for it would be the same category error the file header warns against;
  # covariance type, e.g. "nonrobust"/"cluster"/"HC1", is a categorical
  # setting, not a statistic at all).
  "pr(>f)"                  = "STATO:0000700",   # aov/anova p column
  "pr(>chi)"                = "STATO:0000700",
  "pr(>chisq)"              = "STATO:0000700",
  "p value"                 = "STATO:0000700",
  # More R console-output spellings, from anova()/glm()/htest printouts.
  "deviance"                = "STATO:0000377",
  "resid. dev"              = "STATO:0000377",
  "resid. df"               = "STATO:0000069",
  "res.df"                  = "STATO:0000069",
  "x-squared"               = "STATO:0000030",
  # NOTE: R's htest objects print a bare "W", "S", "V" or "U" for the test
  # statistic, and which quantity that is depends entirely on the test that
  # produced it (W = Shapiro-Wilk's W after shapiro.test, but the rank sum after
  # wilcox.test). The header alone cannot disambiguate them, so they are NOT in
  # this table; they are resolved from the producing CALL instead, via
  # .STATO_BY_CALL below — and stay tier-4 nominal when no call is known.
  # ── jamovi / JASP MACHINE column names ──────────────────────────────────────
  # The structured readers (.jasp_structured_tables / .jmv_structured_tables in
  # R/stat-tables.R) key columns on the format's own machine `name`, not the
  # rendered display title, because a title is often blank or cosmetic. Those
  # names are a different vocabulary from the titles mapped above ("se" not
  # "SE", "md" not "Mean Difference"), so they need their own entries or the
  # structured path would type far less than the HTML path did.
  # NOT typed at all, deliberately: `stat` is jamovi's generic test-statistic
  # column whose meaning changes per row (a t here, a U there), so ANY class —
  # STATO or metacheck — would assert something the data does not support. It
  # stays tier-4 nominal. (`md`, `ss`, `ms` were previously untyped for want of
  # a STATO class; they are now metacheck-minted in .MC_STAT_MAP.)
  # ("se" is already mapped above, in the display-title block — the lower-cased
  # lookup key is identical for both vocabularies, so it is not repeated here.)
  "num"                     = "STATO:0000088",   # N, jamovi descriptives
  # Adjusted p-values: all are p-values (the adjustment is a property of how it
  # was computed, not a different quantity), so all map to the p-value class.
  "pbonferroni"             = "STATO:0000700",
  "pholm"                   = "STATO:0000700",
  "ptukey"                  = "STATO:0000700",
  "pscheffe"                = "STATO:0000700",
  "pnone"                   = "STATO:0000700",
  "pvalue"                  = "STATO:0000700",
  "p.value"                 = "STATO:0000700",
  "pearson_p.value"         = "STATO:0000700",
  "spearman_p.value"        = "STATO:0000700",
  "pearson_estimate"        = "STATO:0000280",   # Pearson's r
  "sample.size"             = "STATO:0000088",
  # jamovi's ANOVA effect-size columns are named for the Greek letter alone;
  # `eta` there is eta-squared and `omega` is omega-squared (the un-squared
  # forms are not what jamovi reports). Generalized (`ges`) and PARTIAL
  # (`parteta`) eta-squared are DISTINCT quantities and are never folded in
  # here — they get their own metacheck terms in .MC_STAT_MAP.
  "eta"                     = "STATO:0000317",
  "etasq"                   = "STATO:0000317",
  "omega"                   = "STATO:0000318",
  "omegasq"                 = "STATO:0000318",
  "lower"                   = "STATO:0000196",   # CI bound
  "upper"                   = "STATO:0000196",
  "cilower"                 = "STATO:0000196",
  "ciupper"                 = "STATO:0000196",
  "cilow"                   = "STATO:0000196",
  "cihig"                   = "STATO:0000196",
  "lower bound"             = "STATO:0000196",
  "upper bound"             = "STATO:0000196",
  "counts"                  = "STATO:0000047",
  "count"                   = "STATO:0000047",
  # jamovi anovaRM's Group Summary reports `n` (cases per group) beside `ex`,
  # the number of cases EXCLUDED from that group — a count, so it takes the
  # count class. (Not the same as `missing`, which counts absent values within
  # a variable and is minted separately as metacheck:missingCount.)
  "ex"                      = "STATO:0000047",
  "pc"                      = "STATO:0000639",   # jamovi: percentage
  "cumpc"                   = "STATO:0000639",   # cumulative percentage
  "percent"                 = "STATO:0000639",   # SPSS Frequencies table
  "valid percent"           = "STATO:0000639",
  "cumulative percent"      = "STATO:0000639",
  "cumulative %"            = "STATO:0000639",
  "marginal percentage"     = "STATO:0000639",
  "frequency"               = "STATO:0000047",   # SPSS Frequencies table's own count
  "total"                   = "STATO:0000047",   # a Frequencies table's own row total
  "total cases"             = "STATO:0000047",
  "excluded cases"          = "STATO:0000047",
  "forecasted cases"        = "STATO:0000047",
  "newly created cases"     = "STATO:0000047",
  "observed n"              = "STATO:0000047",
  "expected n"              = "STATO:0000047",
  "number of subjects"      = "STATO:0000047",
  "number of positive values" = "STATO:0000047",
  "number of negative values" = "STATO:0000047",
  "number of zeros"         = "STATO:0000047",
  "n of items"              = "STATO:0000047",   # reliability analysis
  "numerator df"            = "STATO:0000498",
  "denominator df"          = "STATO:0000527",
  # ── SPSS (.spv) row-label spellings ────────────────────────────────────────
  # A .spv table's statistic identity is a CELL VALUE in a "Statistics"-type
  # dimension column (e.g. a row with Statistics="Sig. (2-tailed)"), not a
  # column header the way JASP/jamovi/R report it — see .spv_stato_type_row()
  # in R/stat-output.R, which looks these same maps up keyed on that cell
  # value instead of a header. SPSS's own phrasing rarely matches the header
  # spellings above verbatim, so its synonyms are collected here rather than
  # scattered across the file.
  "sig."                    = "STATO:0000700",   # SPSS's own p-value label
  "sig. (2-tailed)"         = "STATO:0000700",
  "sig. (1-tailed)"         = "STATO:0000700",
  "sig(2-tailed)"           = "STATO:0000700",   # truncated label with "..." stripped, real files
  "significance"            = "STATO:0000700",
  "significance (2-tailed)" = "STATO:0000700",
  "significance(2-tailed)"  = "STATO:0000700",
  "asymp. sig."             = "STATO:0000700",
  "asymp. sig. (2-sided)"   = "STATO:0000700",
  "approx. sig."            = "STATO:0000700",   # nonparametric tests
  "sig. of change"          = "STATO:0000700",   # stepwise regression's own p
  "hypothesis test / wald chi-square" = "STATO:0000584",
  "wald chi-square"         = "STATO:0000584",
  "std. error mean"         = "STATO:0000037",   # one-sample statistics table
  "std error mean"          = "STATO:0000037",   # UCLA-documented no-period variant
  "std. error of mean"      = "STATO:0000037",
  "std. error of skewness"  = "STATO:0000037",
  "std. error of kurtosis"  = "STATO:0000037",
  "std. error difference"   = "STATO:0000648",
  "std error difference"    = "STATO:0000648",
  "std. error diff"         = "STATO:0000648",   # truncated column heading, real files
  "std. error of the estimate" = "STATO:0000037",
  "standard error of predicted value" = "STATO:0000037",
  "covpar standard error"   = "STATO:0000037",   # mixed-model covariance param SE
  "observed power"          = "STATO:0000200",
  "eta squared"             = "STATO:0000317",
  "approx. chi-square"      = "STATO:0000030",
  "approximate chi-square"  = "STATO:0000030",
  "degrees of freedom"      = "STATO:0000069",   # spelled-out df, several real files
  "asymptotic significance" = "STATO:0000700",
  "r square"                = "STATO:0000564",
  "r square change"         = "STATO:0000564",   # hierarchical regression's own step-change R2
  "squared multiple correlation" = "STATO:0000564",   # reliability analysis
  "-2 log likelihood"       = "STATO:0000550",
  "-2 restricted log likelihood" = "STATO:0000550",
  "akaike ic"               = "STATO:0000325",   # a shorter real-file spelling
  "bayesian ic"             = "STATO:0000327",
  "f change"                = "STATO:0000282",   # hierarchical regression's own step-change F
  "one-sided p"             = "STATO:0000700",
  "two-sided p"             = "STATO:0000700",
  "significance f change"   = "STATO:0000700",
  "levene statistic"        = "STATO:0000282",   # Levene's test is itself an F-test
  "t statistic"             = "STATO:0000176",
  "% of variance"           = "STATO:0000587",
  # Contingency-table cells, surfaced by the wide->long pivot. The row/column/
  # total percentages differ only in their DENOMINATOR, which the row label
  # ("% within row" etc.) already records, so all three take the percentage
  # class rather than three near-identical minted terms.
  "pcrow"                   = "STATO:0000639",
  "pccol"                   = "STATO:0000639",
  "pctot"                   = "STATO:0000639",
  "coefficient"             = "STATO:0000565",
  "b"                       = "STATO:0000565",   # SPSS regression Coefficients table
  "estimate"                = "STATO:0000471",
  "est"                     = "STATO:0000471",
  # Descriptives, as jamovi's Descriptives table names them (surfaced by the
  # wide->long pivot in .jmv_pivot_wide_descriptives()).
  "mode"                    = "STATO:0000033",
  "range"                   = "STATO:0000035",
  "skew"                    = "STATO:0000068",
  "skewness"                = "STATO:0000068",
  "kurt"                    = "STATO:0000178",
  "kurtosis"                = "STATO:0000178",
  "min"                     = "STATO:0000150",
  "minimum"                 = "STATO:0000150",
  "max"                     = "STATO:0000151",
  "maximum"                 = "STATO:0000151",
  "iqr"                     = "STATO:0000164",
  "interquartile range"     = "STATO:0000164",   # SPSS's own spelled-out label
  "eigenvalue"              = "STATO:0000566",
  "s.e."                    = "STATO:0000037",   # SPSS's abbreviated standard error
  "5% trimmed mean"         = "STATO:0000163",
  "score"                   = "STATO:0000560",
  "wald"                    = "STATO:0000584",   # bare form, alongside "wald chi-square"
  # seSkew / seKurt are standard ERRORS of those statistics, so they take the
  # standard-error class rather than the skewness/kurtosis class.
  "seskew"                  = "STATO:0000037",
  "sekurt"                  = "STATO:0000037",
  # R-squared aliases. Adjusted R² is a DIFFERENT quantity and has no STATO
  # class; it is minted as metacheck:adjustedRSquared in .MC_STAT_MAP instead.
  "r2"                      = "STATO:0000564",
  "r²"                      = "STATO:0000564",
  "rsq"                     = "STATO:0000564",
  # Aliases of already-verified classes — no new terms needed, these are just
  # the other spellings JASP/jamovi/R emit for the same quantity.
  "pval"                    = "STATO:0000700",
  "p_value"                 = "STATO:0000700",
  "cil"                     = "STATO:0000196",
  "ciu"                     = "STATO:0000196",
  "ci.lower"                = "STATO:0000196",
  "ci.upper"                = "STATO:0000196",
  "ciles"                   = "STATO:0000196",
  "ciues"                   = "STATO:0000196",
  "lower.cl"                = "STATO:0000196",
  "upper.cl"                = "STATO:0000196",
  # Emitted by the object-capture path (R/r-capture.R) for an htest's conf.int.
  "conf.low"                = "STATO:0000196",
  "conf.high"               = "STATO:0000196",
  # cor.test's `estimate` is named for the coefficient it computed: "cor" for
  # Pearson, "rho" for Spearman, "tau" for Kendall. The first two have their own
  # STATO classes; Kendall's tau has none (OLS4 returns no match) and is minted.
  "cor"                     = "STATO:0000280",
  "rho"                     = "STATO:0000201",
  "spearman_estimate"       = "STATO:0000201",
  # prop.test names its estimates "prop 1", "prop 2", ...; each is a proportion,
  # which is a percentage on a 0-1 scale.
  "prop 1"                  = "STATO:0000639",
  "prop 2"                  = "STATO:0000639",
  "d"                       = "STATO:0000618",
  "cohen d"                 = "STATO:0000618",
  "beta"                    = "STATO:0000565",
  "std.all"                 = "STATO:0000565",
  "standardized coefficient" = "STATO:0000565",
  "stdcoef"                 = "STATO:0000565",   # stdCoef.merMod()'s column name
  "chisq"                   = "STATO:0000030",
  "x2"                      = "STATO:0000030",
  "bf01"                    = "STATO:0000266",
  "var"                     = "STATO:0000643",
  "std"                     = "STATO:0000684",
  "stdev"                   = "STATO:0000684",
  "n_obs"                   = "STATO:0000088",
  "nobs"                    = "STATO:0000088",
  # `length` as an aggregating FUN (aggregate(y ~ g, d, length)) counts the
  # cases per group, i.e. a group N.
  "length"                  = "STATO:0000088",
  # These three DO have STATO classes (verified), so they are typed here rather
  # than minted in .MC_STAT_MAP below.
  "likerat"                 = "STATO:0000409",
  "likelihood ratio"        = "STATO:0000409",
  "es"                      = "STATO:0000085",
  "effect size"             = "STATO:0000085",
  # jamovi's `sed` in a t-test table is the SE of the difference between two
  # independent means, which is exactly STATO:0000648 (the narrower
  # "between independent proportions" class, 0000649, does not apply here).
  "sed"                     = "STATO:0000648",
  # jamovi Bayesian tables: BFM is a Bayes factor like the BF₁₀/BF₀₁ subscripted
  # forms already mapped above (the subscript names which hypothesis pair is
  # being compared, not a different quantity).
  "bfm"                     = "STATO:0000266",
  "bf10"                    = "STATO:0000266",
  # Model / inclusion probabilities in a Bayesian model comparison. The
  # conditioned-on-data forms are POSTERIOR probabilities; the unconditioned
  # P(M) / P(incl) / P(excl) are the PRIORS the analysis started from, which is
  # a different quantity and takes the prior-probability class.
  "p(m|data)"               = "STATO:0000703",
  "p(incl|data)"            = "STATO:0000703",
  "p(excl|data)"            = "STATO:0000703",
  "posterior odds"          = "STATO:0000703",
  "p(m)"                    = "STATO:0000702",
  "p(incl)"                 = "STATO:0000702",
  "p(excl)"                 = "STATO:0000702",
  "prior odds"              = "STATO:0000702",
  # ── JASP machine/column names ─────────────────────────────────────────────
  # JASP's structured analyses.json uses its own column names, a third
  # vocabulary alongside the display titles and jamovi's machine names.
  "bf"                      = "STATO:0000266",
  "bfinclusion"             = "STATO:0000266",
  "bf<sub>inclusion</sub>"  = "STATO:0000266",
  "dof"                     = "STATO:0000069",
  "dfone"                   = "STATO:0000498",
  "dftwo"                   = "STATO:0000527",
  "fstat"                   = "STATO:0000282",
  "chi"                     = "STATO:0000030",
  "qstat"                   = "STATO:0000030",
  "pvl"                     = "STATO:0000700",
  "tukey"                   = "STATO:0000700",   # JASP post-hoc: Tukey-adjusted p
  "t.ratio"                 = "STATO:0000176",
  "etapart"                 = "STATO:0000317",
  "valid"                   = "STATO:0000088",   # JASP Descriptives: N valid
  "coefofvariation"         = "STATO:0000236",
  "aic"                     = "STATO:0000325",
  "bic"                     = "STATO:0000327",
  "dev"                     = "STATO:0000377",
  # CI bounds, in JASP's several spellings.
  "lowerci"                 = "STATO:0000196",
  "upperci"                 = "STATO:0000196",
  "lowercilocationparameter" = "STATO:0000196",
  "uppercilocationparameter" = "STATO:0000196",
  "ciupp"                   = "STATO:0000196",   # ("cilow" already mapped above)
  "lb"                      = "STATO:0000196",
  "ub"                      = "STATO:0000196",
  # CI bounds, psychometric::CI.Rsq()'s spelling ("LCL"/"UCL" columns).
  "lcl"                     = "STATO:0000196",
  "ucl"                     = "STATO:0000196",
  # "stdse"/"SErsq": the standard error of a STANDARDISED coefficient (the
  # widely copied stdCoef.merMod() lme4 helper's column name) and of an R2
  # estimate (psychometric::CI.Rsq()'s column name) — both are still a
  # standard error, the same quantity "se"/"standard error" above already
  # name; these are just the literal headers those two specific functions
  # print, which do not contain either recognised substring.
  "stdse"                   = "STATO:0000037",
  "sersq"                   = "STATO:0000037"
)

# ── call-aware disambiguation (R output) ─────────────────────────────────────
# For executed R code we know EXACTLY which function produced each printed
# block (read_r_output() carries it as `call_fn`, recovered from the echoed
# statement). That resolves the statistic letters an htest printout leaves
# ambiguous: the same "W" is Shapiro-Wilk's W after shapiro.test() and the rank
# sum after wilcox.test(). Keyed by function, then by the printed header.
# Only pairs whose identity is unambiguous GIVEN the call are listed; anything
# absent falls through to the header-only tables.
# A "statistic" key is added below for the tests whose result has exactly ONE
# possible statistic type — rstatix's t_test()/chisq_test()/etc. print their
# tidy tibble's generic value column as literally "statistic" rather than the
# base test's own letter (t.test()'s bare "t", chisq.test()'s "X-squared"),
# so the base-R keys above never match rstatix's column header. Left OFF
# wilcox.test/cor.test: each covers more than one possible statistic (W or V;
# S, t, or z depending on the method actually used), so "statistic" there
# would be a guess this table refuses to make — see the file header comment.
.STATO_BY_CALL <- list(
  "shapiro.test"  = c(w = "mcSTAT:shapiroWilkW", statistic = "mcSTAT:shapiroWilkW"),
  "wilcox.test"   = c(w = "mcSTAT:wilcoxonW", v = "mcSTAT:wilcoxonV"),
  "kruskal.test"  = c(`kruskal-wallis chi-squared` = "STATO:0000030",
                      `chi-squared` = "STATO:0000030", statistic = "STATO:0000030"),
  "bartlett.test" = c(`bartlett's k-squared` = "STATO:0000030",
                      statistic = "STATO:0000030"),
  "friedman.test" = c(`friedman chi-squared` = "STATO:0000030",
                      statistic = "STATO:0000030"),
  "chisq.test"    = c(`x-squared` = "STATO:0000030", statistic = "STATO:0000030"),
  "prop.test"     = c(`x-squared` = "STATO:0000030", statistic = "STATO:0000030"),
  "mcnemar.test"  = c(`mcnemar's chi-squared` = "STATO:0000030",
                      statistic = "STATO:0000030"),
  "cor.test"      = c(s = "mcSTAT:spearmanS", t = "STATO:0000176",
                      z = "STATO:0000376"),
  "var.test"      = c(f = "STATO:0000282", statistic = "STATO:0000282"),
  "t.test"        = c(t = "STATO:0000176", statistic = "STATO:0000176"),
  "ks.test"       = c(d = "mcSTAT:kolmogorovSmirnovD", statistic = "mcSTAT:kolmogorovSmirnovD"),
  # effsize::cohen.d()'s printed "d estimate" — see .r_output_cohend()
  # (R/r-output.R), which names its recovered column plainly "d". Without this,
  # bare "d" is ambiguous with ks.test()'s Kolmogorov-Smirnov D above.
  "cohen.d"       = c(d = "STATO:0000618"),
  # effectsize::cohens_d()/repeated_measures_d() — see .r_output_effectsize_d()
  # (R/r-output.R), a different package's different print shape for the same
  # Cohen's d statistic, also named plainly "d" and so needing the same
  # disambiguation from ks.test()'s D.
  "cohens_d"            = c(d = "STATO:0000618"),
  "repeated_measures_d" = c(d = "STATO:0000618"),
  # ── scipy.stats' printed repr (Python, not R) ──────────────────────────────
  # A notebook's saved cell output prints e.g. "TtestResult(statistic=23.06,
  # pvalue=1.2e-28, df=51)" — read by .ipynb_read_tables() (R/stat-tables.R),
  # which parses this same "name=value" fragment shape via the shared
  # .r_output_oneline() (R/r-output.R). The class name IS the call, unlike R's
  # echoed statement text, so it is matched literally (.ipynb_result_class())
  # rather than recovered from a preceding call. Every class below was
  # confirmed to actually occur across two independent Zenodo samples (170 +
  # 51 real notebooks, from a generic search and a scipy.stats/statsmodels/
  # pingouin-targeted search respectively) — not a guess at scipy's full API
  # surface; expand further only as more classes are actually observed.
  # ttest_1samp()/ttest_ind()/ttest_rel() all return a TtestResult/
  # Ttest_indResult, all Student's t — unambiguous, same reasoning as R's own
  # "t.test" entry above.
  "ttestresult"        = c(statistic = "STATO:0000176"),
  "ttest_indresult"    = c(statistic = "STATO:0000176"),
  # linregress()'s rvalue/slope/intercept/stderr are typed directly by their
  # OWN header text in .STATO_MAP (no call-based disambiguation needed — none
  # of those names collides with another statistic family the way bare "W" or
  # "statistic" do), so no entry is needed here for them; only "pvalue" is
  # generic enough to need no call either (already typed via .STATO_MAP's
  # "pvalue" key). Listed here only for discoverability — linregressresult
  # itself needs NO entry.
  #
  # mannwhitneyu()'s statistic is the rank-sum U (independent samples) — the
  # same family R's wilcox.test() calls "W" for two independent samples
  # (mcSTAT:wilcoxonW; see wilcox.test's entry above and its own header
  # comment on why "statistic" is deliberately absent there — R's wilcox.test
  # is ambiguous between W and V depending on design, but scipy splits that
  # into two separate, unambiguous functions/classes instead).
  "mannwhitneyuresult" = c(statistic = "mcSTAT:mannWhitneyU"),
  # wilcoxon()'s statistic is the signed-rank statistic (paired/one-sample) —
  # confirmed directly against scipy's own docs: "the sum of the ranks of the
  # differences", the same quantity R's wilcox.test calls "V" in that design.
  "wilcoxonresult"     = c(statistic = "mcSTAT:wilcoxonV"),
  # kruskal()'s statistic is the H statistic. OLS4 has "Kruskal Wallis test"
  # (STATO:0000094, the TEST) but no class for the H value itself — verified
  # directly against OLS4's search, the same test-vs-statistic gap R's own
  # kruskal.test() entry above already works around with a metacheck term
  # (there STATO:0000030, chi-squared, since R prints it AS a chi-squared
  # statistic; scipy's own H is the more standard name for the same quantity).
  "kruskalresult"      = c(statistic = "mcSTAT:kruskalWallisH"),
  # kstest()'s statistic is the same Kolmogorov-Smirnov D that R's ks.test()
  # entry above already resolves; scipy additionally returns
  # statistic_location/statistic_sign (newer scipy versions), for which no
  # verified STATO/metacheck term exists yet and which stay untyped nominal
  # labels (the guaranteed stato_type_column() fallback) rather than guessed.
  "kstestresult"       = c(statistic = "mcSTAT:kolmogorovSmirnovD")
)

# Resolve a header GIVEN the producing call. Returns a "STATO:..." id, a
# "mcSTAT:<term>" reference into .MC_STAT_LABELS, or NA when the pair is not
# listed.
.stato_by_call <- function(key, call_fn) {
  if (is.null(call_fn) || !nzchar(call_fn)) return(NA_character_)
  tbl <- .STATO_BY_CALL[[tolower(call_fn)]]
  if (is.null(tbl)) return(NA_character_)
  # `tbl` is a named CHARACTER vector: `[[` on an absent name errors rather
  # than returning NULL, so match by name explicitly.
  i <- match(key, names(tbl))
  if (is.na(i)) NA_character_ else unname(tbl[[i]])
}

#' Type a result-table column by its header, with a guaranteed fallback
#'
#' Maps a JASP/jamovi/R result-table column header to a STATO ontology class
#' when one exists, then to a metacheck-minted statistic term for the standard
#' statistics STATO has no class for, and otherwise falls back to the header
#' text itself as an untyped label. Every column therefore gets a type, so no
#' column is ever dropped from the statistical-output export.
#'
#' `termSource` says which vocabulary answered: `"STATO"` for a verified STATO
#' class, `"metacheck"` for a minted term (see `.MC_STAT_LABELS` for what each
#' one means and why STATO has no equivalent), or `""` when the header could
#' not be typed at all.
#'
#' @param header the column header string as rendered in the result table
#' @param call_fn optional name of the R function that produced this result
#'   (e.g. `"shapiro.test"`), as carried by [read_r_output()]. When supplied it
#'   disambiguates headers that name different quantities in different tests —
#'   R prints a bare `W` for both Shapiro-Wilk's W and the Wilcoxon rank sum —
#'   which no header-only lookup can resolve. Ignored for GUI formats, which
#'   have no call.
#'
#' @returns a list with `annotationValue` (the canonical label, or the header
#'   text when unmapped), `termSource` (`"STATO"`, `"metacheck"`, or `""`), and
#'   `termAccession` (the term IRI, or `""`).
#' @export
stato_type_column <- function(header, call_fn = NULL) {
  key <- tolower(trimws(header %||% ""))
  # R's t.test prints its group means as "mean in group <level>" (and a paired
  # test as "mean difference"), so the LEVEL is baked into the header and no
  # fixed key can match. The quantity is the same sample mean regardless of
  # which group it belongs to — the group is recorded in the row label — so the
  # prefix is normalised away before lookup.
  if (grepl("^mean in group ", key)) key <- "mean"
  if (grepl("^mean of ", key)) key <- "mean"
  # A wide table repeating the same column per variable-pair (a JASP correlation
  # matrix: sample.size, pearson_estimate, ... once per pair) is uniquified by
  # make.unique() into sample.size.1, pearson_estimate.2, ... The trailing index
  # is a disambiguator, not part of the quantity's name, so it is stripped for
  # typing. Guarded to a PURE numeric suffix on a name that has other content,
  # so a genuine key ending in a number (df1, df2, quart1, bf10) is untouched:
  # those have no dot before the digits.
  if (grepl("^.+[a-z_)]\\.[0-9]+$", key)) key <- sub("\\.[0-9]+$", "", key)
  # The call-based capture path (R/r-capture.R) keys a multi-column aggregate as
  # "<fun> (<variable>)" — e.g. "mean (y)" — so several aggregated variables
  # stay distinct in one table. The STATISTIC is the function; the parenthesised
  # variable is which column it was computed on, and is recorded in the label.
  # Skipped when the FULL key (unstripped) is already a known statsmodels
  # header of the exact same shape ("prob (f-statistic)") — that parenthetical
  # names WHICH quantity's p-value this is, not a variable a function was
  # computed over, so stripping it would collapse a real, mapped key ("prob
  # (f-statistic)" -> STATO:0000700) down to a bare "prob" with no entry at
  # all, silently losing a real, verified mapping to the R-capture heuristic.
  if (!key %in% names(.STATO_MAP) && grepl("^[a-z._]+ \\([^)]+\\)$", key))
    key <- sub(" \\(.*$", "", key)
  if (!nzchar(key))
    return(list(annotationValue = "", termSource = "", termAccession = ""))

  # The producing CALL wins when it resolves this header: it is strictly more
  # information than the header alone (we know shapiro.test's W IS Shapiro-
  # Wilk's W), so it settles cases the header-only tables must leave untyped.
  hit <- .stato_by_call(key, call_fn)
  if (!is.na(hit)) {
    if (startsWith(hit, "mcSTAT:")) {
      term <- sub("^mcSTAT:", "", hit)
      return(list(annotationValue = unname(.MC_STAT_LABELS[[term]] %||% header),
                  termSource = "metacheck",
                  termAccession = paste0(.MC_STAT_NS, term)))
    }
    return(list(annotationValue = unname(.STATO_LABELS[[hit]] %||% header),
                termSource = "STATO",
                termAccession = paste0("http://purl.obolibrary.org/obo/",
                                       sub(":", "_", hit))))
  }

  # jamovi's t-test tables (ttestOneS/ttestIS/ttestPS) name their primary
  # test-statistic column literally "stat", with the bracket naming WHICH
  # test produced it. Unlike f[gg]/p[hf]/df[stud]/md[stud]/es[stud] — where
  # the bracket names a correction/variant of the SAME quantity, so stripping
  # it and looking up the bare key is correct — here the bracket changes what
  # the quantity IS, so it must be resolved before the generic strip-and-
  # lookup below discards it (a bare "stat" has no entry in either vocabulary
  # at all). Tags confirmed against a real corpus paper's ttestOneS/ttestIS
  # output (stat[stud], stat[welc]) and this codebase's own note on the third
  # jamovi variant (R/stat-tables.R's "stud/welc/mann/bf" comment): stud/welc
  # are both Student's/Welch's t; mann is Mann-Whitney's U, for which STATO
  # has only the TEST class (STATO:0000076) and no statistic class — the same
  # gap already documented above for wilcox.test(), so it reuses that same
  # minted term (mcSTAT:wilcoxonW — R's wilcox.test() prints the identical
  # rank-sum statistic for two independent samples).
  if (grepl("^stat\\[(stud|welc)\\]$", key))
    return(list(annotationValue = unname(.STATO_LABELS[["STATO:0000176"]]),
                termSource = "STATO",
                termAccession = "http://purl.obolibrary.org/obo/STATO_0000176"))
  if (identical(key, "stat[mann]"))
    return(list(annotationValue = unname(.MC_STAT_LABELS[["wilcoxonW"]]),
                termSource = "metacheck",
                termAccession = paste0(.MC_STAT_NS, "wilcoxonW")))

  # Candidate keys: the header as written, then the same with a trailing
  # variant/correction suffix removed (f[gg], p[hf]) — see
  # .stato_strip_variant(). Both vocabularies are consulted for each, STATO
  # first, so a real STATO class always wins over a minted one.
  keys <- unique(c(key, .stato_strip_variant(key)))

  for (k in keys) {
    if (k %in% names(.STATO_MAP)) {
      id <- .STATO_MAP[[k]]
      return(list(
        annotationValue = unname(.STATO_LABELS[[id]] %||% header),
        termSource = "STATO",
        termAccession = paste0("http://purl.obolibrary.org/obo/",
                               sub(":", "_", id))))
    }
    if (k %in% names(.MC_STAT_MAP)) {
      term <- .MC_STAT_MAP[[k]]
      return(list(
        annotationValue = unname(.MC_STAT_LABELS[[term]] %||% header),
        termSource = "metacheck",
        termAccession = paste0(.MC_STAT_NS, term)))
    }
  }

  # No class in either vocabulary — keep the header as a nominal label.
  list(annotationValue = trimws(header), termSource = "", termAccession = "")
}

#' Type a .spv result row by its statistic-name CELL VALUE, not a header
#'
#' JASP/jamovi/R result tables are WIDE: one row per test, one column per
#' statistic (`t`, `df`, `p`), so [stato_type_column()] types a column by its
#' header. `.spv` tables decode LONG/tidy instead (see R/spv.R): one row
#' per cell, with the statistic's own NAME sitting as a value inside a
#' dimension column (e.g. a row where the "Statistics" column reads
#' `"Sig. (2-tailed)"` and `value` is `0.037`) — there is no header to type at
#' all. This types that row from its OWN category value instead, reusing the
#' SAME `.STATO_MAP`/`.MC_STAT_MAP` dictionaries `stato_type_column()` draws
#' on (SPSS's row labels and JASP/jamovi's column headers name the same
#' statistics, just positioned differently in the table), plus SPSS-specific
#' normalisation `stato_type_column()` has no reason to apply (stripping a
#' leading "Type III "/"Type I "/"Type II " sum-of-squares qualifier, and a
#' `[%1:*...]1`-style templated placeholder SPSS sometimes leaves unresolved
#' in a repeated-measures error-term label).
#'
#' @param label the row's own statistic-name cell value (e.g. from a
#'   `"Statistics"`-labelled dimension column).
#' @return the same shape [stato_type_column()] returns:
#'   `list(annotationValue, termSource, termAccession)`.
#' @keywords internal
.spv_stato_type_label <- function(label) {
  key <- tolower(trimws(label %||% ""))
  if (!nzchar(key)) return(list(annotationValue = "", termSource = "", termAccession = ""))

  # ANOVA/GLM row labels prefix the sum-of-squares TYPE ("Type III Sum of
  # Squares", occasionally "Type I"/"Type II") -- the type is a property of
  # HOW it was computed, not a different quantity, so it is stripped before
  # lookup; the type itself is not lost, since it stays in the table's own
  # SPSS syntax (recorded separately, see R/spv.R's `syntax` field).
  key <- sub("^type\\s+(i|ii|iii)\\s+", "", key)

  # A GENLIN/model-comparison table appends " Reduced" to a Model Fitting
  # Criterion row to mark the reduced-model estimate (vs. the full model's
  # own unsuffixed row) -- WHICH model it belongs to, not a different
  # quantity, so it is stripped for typing (the row still keeps its own
  # untouched original label as `statistic` in stat_results_long()'s output,
  # so "which model" is not actually lost from the data, only from typing).
  key <- sub("\\s+reduced$", "", key)

  # A bootstrap-derived estimate is reported as "Bootstrap / <quantity>"
  # (e.g. "Bootstrap / Std. Error", "Bootstrap / BCa 95% Confidence Interval
  # / Lower") -- "Bootstrap" and the CI's own confidence level/method (BCa)
  # describe HOW the quantity was computed, not a different quantity, so
  # both are stripped before lookup (the method itself is not lost -- the
  # table's own $syntax field records the BOOTSTRAP command that produced
  # it). The bare CI-bound keys ("lower"/"upper") already exist in
  # .STATO_MAP, so the qualifier collapses straight to those rather than a
  # separate "confidence interval / lower" entry.
  key <- sub("^bootstrap\\s*/\\s*", "", key)
  key <- sub("^bca\\s+[0-9]+%\\s+confidence interval\\s*/\\s*(lower|upper).*$", "\\1", key)

  # SPSS truncates an over-wide column HEADING with "..." when the label
  # doesn't fit the printed column width (e.g. "t-test for Equality... /
  # Std. Error Diff...", "Sig(2-tailed)..."), confirmed in real files -- the
  # ellipsis is a DISPLAY artifact, not part of the quantity's name.
  key <- sub("\\.\\.\\.$", "", key)

  # GENERAL RULE: a "/"-joined compound label's LAST segment is the actual
  # quantity; everything before it is a GROUPING label -- which test/
  # statistic family it belongs to ("t-test for Equality of Means",
  # "Change Statistics", "Collinearity Statistics", "Unstandardized
  # Coefficients", "Paired Differences", "95% Confidence Interval for B"),
  # not a different quantity. Confirmed against this corpus's real label
  # variety (osf.io_xzke7 and others): "t-test for Equality... / Std. Error
  # Diff...", "Change Statistics / F Change", "95% Confidence Interval for
  # Exp(B) / Lower Bound" all follow this shape, and none of the prefixes
  # themselves are quantities a stato_type_column()-style header lookup
  # would ever see (they are compound/truncated SPSS super-headers, not
  # column names). Tried ONLY after the exact full key and the two specific
  # strips above fail, so a full-string match (e.g. "sig. (2-tailed)", which
  # has no "/") always wins first.
  full_key <- key
  if (grepl(" / ", key) &&
      !(full_key %in% names(.STATO_MAP)) && !(full_key %in% names(.MC_STAT_MAP))) {
    key <- trimws(sub("^.*/\\s*", "", key))
  }

  # A repeated-measures error-term row is labelled with an unresolved
  # template placeholder when the corresponding factor combination has no
  # simple name, e.g. "Error([%1:*NRHand, posture:]1)" -- confirmed against
  # real files in this corpus (osf.io_xzke7's ANOVA outputs). This is a ROW
  # LABEL (which error term), not a statistic name, and stato_type_column()
  # would never see it (it has no header equivalent); typed as untyped
  # nominal rather than attempting to resolve the bracketed template, since
  # the template resolver in R/spv.R already tried and failed
  # (this string reaching here means it was genuinely unresolvable).
  if (grepl("^error(\\(|$)", key))
    return(list(annotationValue = trimws(label), termSource = "", termAccession = ""))

  hit <- NULL
  if (key %in% names(.STATO_MAP)) hit <- .STATO_MAP[[key]]
  else if (key %in% names(.MC_STAT_MAP)) hit <- paste0("mcSTAT:", .MC_STAT_MAP[[key]])

  if (is.null(hit)) return(list(annotationValue = trimws(label), termSource = "", termAccession = ""))
  if (startsWith(hit, "mcSTAT:")) {
    term <- sub("^mcSTAT:", "", hit)
    return(list(annotationValue = unname(.MC_STAT_LABELS[[term]] %||% label),
                termSource = "metacheck",
                termAccession = paste0(.MC_STAT_NS, term)))
  }
  list(annotationValue = unname(.STATO_LABELS[[hit]] %||% label),
       termSource = "STATO",
       termAccession = paste0("http://purl.obolibrary.org/obo/", sub(":", "_", hit)))
}
