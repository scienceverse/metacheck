test_that("read_stat_tables reads Jupyter notebook outputs", {
  skip_if_not_installed("xml2")
  skip_if_not_installed("rvest")

  file_path <- test_path("fixtures", "notebooks", "notebook_python.ipynb")
  tabs <- read_stat_tables(file_path)

  expect_type(tabs, "list")
  expect_true(length(tabs) > 0)

  # Every element has the same shape the JASP/jamovi paths return, so all
  # downstream consumers work unchanged — except a parsed statistic line
  # (see below), which additionally carries `call_fn` (the Python result
  # class, e.g. "TtestResult"), the same extra field read_r_output() attaches
  # for R console output, and for the same reason: it lets stato_type_column()
  # disambiguate a bare "statistic" header.
  for (tb in tabs) {
    expect_true(all(c("analysis", "title", "data", "table_index") %in% names(tb)))
    expect_s3_class(tb$data, "data.frame")
  }

  # table_index is a 1-based ordinal across the whole document.
  expect_equal(vapply(tabs, `[[`, integer(1), "table_index"),
               seq_along(tabs))

  # The HTML output is parsed as a real table (not as text), through the same
  # .stat_table_parse() the JASP/jamovi HTML path uses.
  html_tab <- Filter(function(x) "t" %in% names(x$data), tabs)
  expect_length(html_tab, 1)
  expect_equal(names(html_tab[[1]]$data), c("cond", "t", "df", "p"))
  expect_equal(html_tab[[1]]$data$t, c("3.41", "2.07"))

  # The cell is identified by number and execution count (a notebook has no
  # heading to use the way a JASP analysis does).
  expect_match(html_tab[[1]]$analysis, "^Cell [0-9]+")

  # A printed test result ("TtestResult(statistic=..., pvalue=..., df=...)")
  # is parsed into a real statistic/pvalue/df table (via .ipynb_stat_line(),
  # the same "name [(df)] op value" fragment parser read_r_output() uses on R
  # console output) rather than kept as opaque text — a single "text" column
  # has no header stat_results_long() can type, so leaving it as text would
  # silently drop the statistic from the extracted output entirely.
  stat_tab <- Filter(function(x) "statistic" %in% names(x$data), tabs)
  expect_length(stat_tab, 1)
  expect_equal(stat_tab[[1]]$call_fn, "TtestResult")
  expect_equal(as.character(stat_tab[[1]]$data$statistic), "4.8977")
  expect_equal(as.character(stat_tab[[1]]$data$pvalue), "0.00007")
  expect_equal(as.character(stat_tab[[1]]$data$df), "22")

  # The call_fn round-trips through stat_results_long() into a real STATO
  # type (.STATO_BY_CALL's "ttestresult" entry): a bare "statistic" header
  # has no type on its own (unlike R's "t", which .STATO_MAP recognises
  # directly), so without call_fn this would fall back to an untyped nominal
  # label instead of Student's t-statistic.
  long <- stat_results_long(tabs, source_file = "notebook_python.ipynb")
  stat_row <- long[long$statistic == "statistic", ]
  expect_equal(nrow(stat_row), 1)
  expect_equal(stat_row$stato_iri,
              "http://purl.obolibrary.org/obo/STATO_0000176")
})


test_that(".ipynb_stat_line parses scipy result classes beyond TtestResult", {
  # Confirmed to actually occur across two independent Zenodo samples (170 +
  # 51 real notebooks: one from a generic "jupyter notebook" search, one
  # targeted at scipy.stats/statsmodels/pingouin) -- these six classes plus
  # TtestResult (covered above) are what was actually observed, not a guess
  # at scipy's full API. Examples are the REAL text these notebooks printed,
  # including newer numpy's "np.float64(...)"-wrapped scalars, which
  # .ipynb_strip_numpy_scalars() must unwrap before the shared
  # "name op value" fragment pattern (.r_stat_pattern(), R/r-output.R) can
  # match -- confirmed as a real gap: half the TtestResult/LinregressResult/
  # Ttest_indResult occurrences in the sample used this newer repr, and
  # without the strip they silently fell back to unparsed raw text.
  linreg <- .ipynb_stat_line(
    "LinregressResult(slope=np.float64(11.474329080951929), intercept=np.float64(46.43030811550318), rvalue=np.float64(0.4052714777933583), pvalue=np.float64(0.004269632365840751), stderr=np.float64(3.8162))")
  expect_length(linreg, 1)
  expect_equal(linreg[[1]]$call_fn, "LinregressResult")
  expect_equal(as.character(linreg[[1]]$data$slope), "11.474329080951929")
  expect_equal(as.character(linreg[[1]]$data$stderr), "3.8162")

  ttest_ind <- .ipynb_stat_line(
    "Ttest_indResult(statistic=np.float64(5.324149292705737), pvalue=np.float64(3.6340850924488865e-07))")
  expect_length(ttest_ind, 1)
  expect_equal(ttest_ind[[1]]$call_fn, "Ttest_indResult")

  mwu <- .ipynb_stat_line("MannwhitneyuResult(statistic=1949.0, pvalue=0.0013180093601592962)")
  kruskal <- .ipynb_stat_line("KruskalResult(statistic=4.552258064516124, pvalue=0.10268091290330437)")
  wilcoxon <- .ipynb_stat_line("WilcoxonResult(statistic=0.0, pvalue=0.001953125)")
  kstest <- .ipynb_stat_line("KstestResult(statistic=1.0, pvalue=0.0, statistic_location=1683.95, statistic_sign=-1)")
  for (r in list(mwu = mwu, kruskal = kruskal, wilcoxon = wilcoxon, kstest = kstest)) {
    expect_length(r, 1)
    expect_true("statistic" %in% names(r[[1]]$data))
  }

  # Each class's "statistic" resolves to its OWN distinct STATO/metacheck
  # type via .STATO_BY_CALL, not a shared/generic one -- the whole point of
  # keying by call_fn rather than the (identical, ambiguous) bare header.
  long <- stat_results_long(c(linreg, ttest_ind, mwu, kruskal, wilcoxon, kstest),
                            source_file = "test.ipynb")
  # Each result contributes exactly one "statistic"/"slope" row here, so
  # filtering on the resolved label alone identifies it.
  expect_equal(long$stato_iri[long$row_label == "" & long$statistic == "slope"],
              "http://purl.obolibrary.org/obo/STATO_0000656")
  mwu_row <- long[long$statistic == "statistic" &
                    grepl("mannWhitneyU", long$stato_iri), ]
  expect_equal(nrow(mwu_row), 1)
  kruskal_row <- long[long$statistic == "statistic" &
                        grepl("kruskalWallisH", long$stato_iri), ]
  expect_equal(nrow(kruskal_row), 1)
  wilcoxon_row <- long[long$statistic == "statistic" &
                        grepl("wilcoxonV", long$stato_iri), ]
  expect_equal(nrow(wilcoxon_row), 1)
  kstest_row <- long[long$statistic == "statistic" &
                       grepl("kolmogorovSmirnovD", long$stato_iri), ]
  expect_equal(nrow(kstest_row), 1)
  # kstest's newer auxiliary fields have no verified STATO/metacheck term and
  # stay untyped nominal labels (never dropped, never guessed).
  expect_true(any(long$statistic == "statistic_location" & long$stato_iri == ""))
})


test_that(".ipynb_stat_table parses statsmodels .summary() coefficient tables", {
  # Real statsmodels OLS Regression Results output (confirmed the MOST common
  # real-world pattern in a Zenodo sample: 39/51 sampled notebooks call
  # smf.ols, 30 call sm.OLS -- far more than any bare *Result() repr). The
  # SAME fixed-width shape R's own summary(lm) prints, parsed by the shared
  # .r_output_tables() (R/r-output.R) once its "---" divider (which sits
  # BETWEEN header and data here, unlike R's own footnote-only placement) is
  # recognised -- see that function's own comment on the fix this needed.
  summary_text <- c(
    "                            OLS Regression Results",
    "==============================================================================",
    "Dep. Variable:         Square Footage   R-squared:                       0.844",
    "Model:                            OLS   Adj. R-squared:                  0.834",
    "Method:                 Least Squares   F-statistic:                     84.18",
    "=============================================================================================",
    "                                coef    std err          t      P>|t|      [0.025      0.975]",
    "---------------------------------------------------------------------------------------------",
    "const                      -4.62e+04   4449.507    -10.384      0.000    -5.5e+04   -3.74e+04",
    "Number of Occupants        -188.2184     20.093     -9.367      0.000    -228.118    -148.318")

  tabs <- .ipynb_stat_table(summary_text)
  # Two DIFFERENT shapes in this one block, both real results now: the
  # coefficient table (header "coef  std err  t  P>|t|  ...", recognised by
  # that header set) AND the key-value header block above it ("Dep.
  # Variable: ... R-squared: 0.844  Model: OLS  Adj. R-squared: 0.834 ...",
  # two label:value pairs per line -- a shape .r_output_tables() cannot parse
  # at all, so .ipynb_stat_kv() extracts it separately). Manuscript-
  # reportable model-fit statistics (R², F-statistic, ...) that were
  # previously not extracted at all.
  expect_length(tabs, 2)
  coef_tab <- Filter(function(t) "coef" %in% names(t$data), tabs)[[1]]
  kv_tab   <- Filter(function(t) "R-squared" %in% names(t$data), tabs)[[1]]

  expect_true(all(c("coef", "std err", "t", "P>|t|") %in% names(coef_tab$data)))
  expect_equal(nrow(coef_tab$data), 2)
  expect_equal(as.character(coef_tab$data$coef), c("-4.62e+04", "-188.2184"))

  # The key-value block's own descriptive/categorical fields (Dep. Variable's
  # NAME, Model class, Method name) are metadata, not statistics, and are
  # dropped rather than kept as untyped columns -- see .ipynb_stat_kv()'s own
  # comment on why: they would otherwise pollute every OTHER statistic's
  # row_label via stat_results_long()'s all-text-column concatenation.
  expect_false(any(c("Dep. Variable", "Model", "Method") %in% names(kv_tab$data)))
  expect_equal(as.character(kv_tab$data$`R-squared`), "0.844")

  long <- stat_results_long(tabs, source_file = "test.ipynb")
  const_row <- long[long$row_label == "const", ]
  expect_equal(const_row$stato_iri[const_row$statistic == "coef"],
              "http://purl.obolibrary.org/obo/STATO_0000471")
  expect_equal(const_row$stato_iri[const_row$statistic == "std err"],
              "http://purl.obolibrary.org/obo/STATO_0000037")
  expect_equal(const_row$stato_iri[const_row$statistic == "t"],
              "http://purl.obolibrary.org/obo/STATO_0000176")
  expect_equal(const_row$stato_iri[const_row$statistic == "P>|t|"],
              "http://purl.obolibrary.org/obo/STATO_0000700")
  # The key-value block's row_label is empty (single-row result, nothing to
  # key by) -- confirmed as a real bug this exact way before the metadata
  # columns were dropped (they were being concatenated into every row_label).
  kv_rows <- long[long$statistic %in% c("R-squared", "Adj. R-squared", "F-statistic"), ]
  expect_true(all(kv_rows$row_label == ""))
  expect_equal(kv_rows$stato_iri[kv_rows$statistic == "R-squared"],
              "http://purl.obolibrary.org/obo/STATO_0000564")
  expect_equal(kv_rows$stato_iri[kv_rows$statistic == "Adj. R-squared"],
              "https://scienceverse.org/schema/metacheck/statistics/adjustedRSquared")
  expect_equal(kv_rows$stato_iri[kv_rows$statistic == "F-statistic"],
              "http://purl.obolibrary.org/obo/STATO_0000282")

  # A block with no recognisable coefficient-table header AND no statsmodels
  # key-value shape (plain prose) yields NULL, so the caller falls back to
  # raw text rather than fabricating a table.
  expect_null(.ipynb_stat_table(c("This model fit the data reasonably well.")))
})


test_that(".ipynb_stat_kv extracts statsmodels' full key-value header block", {
  # All fields from a real statsmodels OLS .summary(), confirmed stable
  # across two independently-sampled Zenodo notebooks (an energy-consumption
  # OLS and an export/democracy panel-data OLS) -- same field set and layout
  # both times.
  summary_text <- c(
    "Dep. Variable:         Square Footage   R-squared:                       0.844",
    "Model:                            OLS   Adj. R-squared:                  0.834",
    "Method:                 Least Squares   F-statistic:                     84.18",
    "Date:                Mon, 10 Mar 2025   Prob (F-statistic):           2.16e-35",
    "Time:                        01:17:53   Log-Likelihood:                -1000.9",
    "No. Observations:                 100   AIC:                             2016.",
    "Df Residuals:                      93   BIC:                             2034.",
    "Df Model:                           6",
    "Covariance Type:            nonrobust")

  kv <- .ipynb_stat_kv(summary_text)
  expect_equal(kv$title, "Model fit statistics")
  d <- kv$data
  expect_equal(as.character(d$`R-squared`), "0.844")
  expect_equal(as.character(d$`Adj. R-squared`), "0.834")
  expect_equal(as.character(d$`F-statistic`), "84.18")
  expect_equal(as.character(d$`Prob (F-statistic)`), "2.16e-35")
  expect_equal(as.character(d$`Log-Likelihood`), "-1000.9")
  expect_equal(as.character(d$`No. Observations`), "100")
  expect_equal(as.character(d$AIC), "2016.")
  expect_equal(as.character(d$`Df Residuals`), "93")
  expect_equal(as.character(d$BIC), "2034.")
  # Descriptive/categorical metadata is dropped (see .ipynb_stat_table()'s
  # own test for why); "Df Model" (a predictor COUNT, not a real degrees-of-
  # freedom or estimate quantity) is deliberately kept but stays untyped.
  expect_false(any(c("Dep. Variable", "Model", "Method", "Date", "Time",
                     "Covariance Type") %in% names(d)))
  expect_true("Df Model" %in% names(d))

  long <- stat_results_long(list(kv), source_file = "test.ipynb")
  expect_equal(long$stato_iri[long$statistic == "Log-Likelihood"],
              "http://purl.obolibrary.org/obo/STATO_0000550")
  expect_equal(long$stato_iri[long$statistic == "No. Observations"],
              "http://purl.obolibrary.org/obo/STATO_0000088")
  expect_equal(long$stato_iri[long$statistic == "AIC"],
              "http://purl.obolibrary.org/obo/STATO_0000325")
  expect_equal(long$stato_iri[long$statistic == "BIC"],
              "http://purl.obolibrary.org/obo/STATO_0000327")
  expect_equal(long$stato_iri[long$statistic == "Df Residuals"],
              "http://purl.obolibrary.org/obo/STATO_0000069")
  expect_equal(long$stato_iri[long$statistic == "Df Model"], "")

  # Prose with NO statsmodels-shaped key-value pairs (gated on Model/Method
  # both present, statsmodels' own two universal fields across every model
  # type) yields NULL rather than misreading arbitrary "Word: value" text.
  expect_null(.ipynb_stat_kv(c("This model fit the data reasonably well.")))
  expect_null(.ipynb_stat_kv(c("Dep. Variable: Square Footage   R-squared: 0.844")))
})


test_that("read_stat_tables filters notebook output noise", {
  skip_if_not_installed("xml2")
  skip_if_not_installed("rvest")

  file_path <- test_path("fixtures", "notebooks", "notebook_python.ipynb")
  tabs <- read_stat_tables(file_path)
  all_text <- unlist(lapply(tabs, function(x)
    if (identical(names(x$data), "text")) x$data$text else character(0)))

  # matplotlib's figure repr is machinery, not a result. In a 30-notebook
  # corpus sample these were 129 of 383 text outputs.
  expect_false(any(grepl("Figure size", all_text)))
})


test_that(".ipynb_is_noise drops machinery but keeps statistics", {
  # Known noise shapes, all measured in the real corpus.
  expect_true(.ipynb_is_noise("<Figure size 432x288 with 1 Axes>"))
  expect_true(.ipynb_is_noise("plot without title"))
  expect_true(.ipynb_is_noise("<AxesSubplot: xlabel='cond'>"))
  expect_true(.ipynb_is_noise("0%|          | 0/200 [00:00<?, ?it/s]"))
  expect_true(.ipynb_is_noise(
    c("/opt/py/scipy/stats/_continuous_distns.py:6832: RuntimeWarning: overflow",
      "  return np.exp(x)")))
  expect_true(.ipynb_is_noise(""))

  # Real output must survive, including a complete test at full precision --
  # exactly what match_reported_output() exists to match.
  expect_false(.ipynb_is_noise(
    "TtestResult(statistic=4.897732993778993, pvalue=6.750826167315465e-05, df=22)"))
  expect_false(.ipynb_is_noise(
    c("       T  dof alternative     p-val", "  4.898   22   two-sided  0.00007")))
  expect_false(.ipynb_is_noise(
    "Generalized linear mixed model fit by maximum likelihood (Laplace)"))
})


test_that("read_stat_tables handles notebooks with no usable output", {
  skip_if_not_installed("xml2")
  skip_if_not_installed("rvest")

  # An R-kernel notebook: its stdout is a real result, parsed into a
  # statistic table (t/df/p-value) via the same one-line fragment parser as
  # the Python case above, rather than left as opaque text.
  r_nb <- test_path("fixtures", "notebooks", "notebook_r.ipynb")
  r_tabs <- read_stat_tables(r_nb)
  expect_true(length(r_tabs) > 0)
  r_stat_tab <- Filter(function(x) "t" %in% names(x$data), r_tabs)
  expect_length(r_stat_tab, 1)
  expect_equal(as.character(r_stat_tab[[1]]$data$t), "2.8134")
  expect_equal(as.character(r_stat_tab[[1]]$data$df), "47.2")
  expect_equal(as.character(r_stat_tab[[1]]$data$`p-value`), "0.007123")

  # Valid JSON that is not a notebook -> empty list, not an error (the same
  # contract the archive paths honour).
  bad <- withr::local_tempfile(fileext = ".ipynb")
  writeLines('{"foo": 1}', bad)
  expect_equal(read_stat_tables(bad), list())

  # A notebook stripped of outputs (nbstripout) yields nothing.
  stripped <- withr::local_tempfile(fileext = ".ipynb")
  jsonlite::write_json(
    list(cells = list(list(cell_type = "code", source = list("x = 1"),
                           outputs = list())),
         nbformat = 4L),
    stripped, auto_unbox = TRUE)
  expect_equal(read_stat_tables(stripped), list())

  expect_error(read_stat_tables("no_such_file.ipynb"), "File not found")
})
