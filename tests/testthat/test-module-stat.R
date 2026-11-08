test_that("stat_p_exact", {
  paper <- test_paper("The p = .0123")

  module <- "stat_p_exact"
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "green")
  expect_equal(nrow(mod_output$table), 1)

  # add imprecise p-values
  paper <- test_paper(c(
    "Bad p-value example (p < .05)",
    "Bad p-value example (p<.05)",
    "Bad p-value example (p < 0.05)",
    "Bad p-value example; p < .05",
    "Bad p-value example (p < .005)",
    "Bad p-value example (p > 0.05)",
    "Bad p-value example (p > .1)",
    "Bad p-value example (p = n.s.)",
    "Bad p-value example; p=ns",
    "Bad p-value example; p \u2264 .001",
    "OK p-value example; p < .001",
    "OK p-value example; p < .0005"
  ))

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 12)
  expect_equal(sum(mod_output$table$imprecise), 10)

  # zero p-values
  paper <- test_paper(c(
    "Significant result (p = .000)",
    "Significant result (p = 0.000)",
    "Significant result (p = 0.00)"
  ))

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 3)
  expect_true(all(mod_output$table$zero))
  expect_false(any(mod_output$table$imprecise))
  expect_equal(mod_output$summary_table$n_zero, 3)

  # imprecise and zero together
  paper <- test_paper(c(
    "Imprecise p-value example (p < .05)",
    "Zero p-value example (p = .000)"
  ))

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 2)
  expect_equal(sum(mod_output$table$imprecise), 1)
  expect_equal(sum(mod_output$table$zero), 1)

  # iteration
  skip_no_psychsci()
  paper <- psychsci
  mod_output <- module_run(paper, module)
  lt05 <- grepl("p < .05", mod_output$table$text) |> sum()
  # expect_equal(lt05, 174)
  expect_equal(mod_output$table$p_comp[[1]], "<")
  expect_equal(mod_output$table$p_value[[1]], 0.001)
})

test_that("marginal", {
  module <- "marginal"

  # no relevant text
  paper <- test_paper("This effect is absent.")
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), 0)

  # add marginal text
  paper <- test_paper(c(
    "This effect was marginally significant (p = .065).",
    "This effect approached significance (p = 0.34)."
  ))

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 2)

  # iteration
  skip_no_psychsci()
  mod_output <- module_run(psychsci, module)
  # expect_true(unique(mod_output$table$id) |> length() > 1)
})

# test_that("sample-size", {
#   skip("python install is messed up")
#   skip_on_cran()
#   model_dir <- system.file("modules/sample-size", package = "metacheck")
#
#   if (model_dir == "") {
#     skip("needs big classifier: sample-size")
#   }
#
#   paper <- demopaper() |>
#     search_text(".{30, }", section = "method", return = "sentence")
#   module <- "sample-size-ml"
#
#   mod_output <- module_run(paper, module)
#   expect_equal(mod_output$traffic_light, "green")
#   expect_equal(nrow(mod_output$table), 2)
#   expect_equal(mod_output$module, module)
# })



test_that("stat_check", {
  paper <- test_paper(c(
    "This is right: t(17.4), p = 0.137",
    "This is wrong: t(97.2) = -1.96, p = 0.152"
  ))
  module <- "stat_check"

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 1)
  expect_equal(mod_output$table$raw[[1]],
               "t(97.2) = -1.96, p = 0.152")
  expect_equal(mod_output$module, module)

  # iteration
  paper <- paperlist(
    test_paper("This is right: t(17.4), p = 0.137"),
    test_paper("This is wrong: t(97.2) = -1.96, p = 0.152")
  )
  expect_no_error(
    mod_output <- module_run(paper, module)
  )
  expect_equal(nrow(mod_output$table), 1)
  expect_equal(mod_output$table$paper_id, names(paper)[[2]])
})



test_that("stat_p_nonsig", {
  paper <- test_paper(c("Significant; p \u2264 .01", "Nonsignificant; p = .20"))
  mod_output <- module_run(paper, "stat_p_nonsig")
  expect_equal(mod_output$table$text, "p = .20")
})

test_that("stat_p_exact: p < .000 is zero, == is =, and a starred footnote hides only itself (U127)", {
  s <- c("The effect was strong, p < .000.",
         "Another, p == .000.",
         "Group A differed, p < .05, and group B differed, p < .01 (*p < .05).")
  mo <- module_run(test_paper(s), "stat_p_exact")
  expect_equal(mo$summary_table$n_zero, 2)
  expect_equal(mo$summary_table$n_imprecise, 2)   # .05 and .01, not the starred footnote
})

test_that("stat_p_exact leaves out table notes, legends and thresholds, not results (#604)", {
  # each sentence from a real paper (PsyArXiv reports and the open-access
  # collections); the p-value in the second column
  cases <- list(
    # notes with markers
    list("Note. ** = p < .01, * = p < .05.", "p < .05", "marker"),
    list("Data are presented as mean ± SD. ns, p > 0.05; **p < 0.01; ***p < 0.001.", "p > 0.05", "marker"),
    list("Error bars indicate standard error. + p < 0.10, * p <0.05.", "p < 0.10", "marker"),
    list("Model fit: χ2 = 129.59, df = 89, p < .01, CFI = 0.95, SRMR = .06, N = 135, *p <.05.", "p <.05", "marker"),
    list("Error bars represent ±1 SEM.* p < 0.05 (two-tailed).", "p < 0.05", "marker"),
    # thresholds
    list("A p-value < 0.05 was considered statistically significant.", "p-value < 0.05", "threshold"),
    list("The significance level was set at p <.05.", "p <.05", "threshold"),
    list("All the reported clusters survived a cFWE-corrected threshold of P < 0.05.", "P < 0.05", "threshold"),
    list("Contrasts, with a Bonferroni corrected alpha of p<.02 for 4 comparisons, showed an interaction.", "p<.02", "threshold"),
    # legends
    list("Asterisks indicate significant differences between conditions (p < .05).", "p < .05", "legend"),
    list("Note: Correlations significant at the p < .05 level are highlighted in bold.", "p < .05", "legend"),
    list("Black and white markers indicate reported effects that did (black) and did not (white) differ significantly (p < 0.05) from the corresponding expected effect.", "p < 0.05", "legend"),
    # results, which stay
    list("More importantly, there was a significant interaction [F(1,18) ¼ 6.05, P < 0.05, g2 ¼ 0.25].", "P < 0.05", NA),
    list("The indirect effect through hopelessness was significant (β = 0.115*, p < 0.05).", "p < 0.05", NA),
    list("Trials were more variable (paired t-test, n = 95, t52 = 6.92, ***p<<0.001, and for monkey D).", "p<<0.001", NA),
    list("Silencing (overground n = 135/600 vs treadmill n = 22/151, *p<0.05 [z = 2.38]; hindlimbs).", "p<0.05", NA),
    list("Tor1a +/+ p<0.05; Tor1a +/Dgag p<0.01.", "p<0.05", NA),
    list("No other main effects or interactions reached conventional significance levels (all ps > 0.09).", "ps > 0.09", NA),
    list("This effect remained significant when the maps were thresholded at varying levels (all Ps < 0.002).", "Ps < 0.002", NA),
    list("Participants showed a higher exit threshold (P < 0.05).", "P < 0.05", NA),
    list("The Kansas and Florida lines differed in DT but not PA (t-test, P>0.05, data not shown).", "P>0.05", NA),
    list("Alpha power was higher in the online group than in the offline group (p < .05).", "p < .05", NA),
    list("Participants who met the inclusion criteria scored higher (p < .05).", "p < .05", NA)
  )
  p <- data.frame(paper_id = "x", text_id = seq_along(cases),
                  expanded = vapply(cases, `[[`, "", 1), text = vapply(cases, `[[`, "", 2))
  expect_equal(.p_not_test(p), vapply(cases, function(x) as.character(x[[3]]), ""))

  # the same note under two tables: both copies are left out, not one (b92ku_v1)
  note <- "The table reports standard errors and asymptotic p-values. * p < .05. * * p < .01. * * * p < .001."
  mo <- module_run(test_paper(c(note, "Some results.", note)), "stat_p_exact")
  expect_equal(mo$summary_table$n_imprecise %||% 0, 0)
  # a legend joined to a table's cells (z2hpt_v1)
  mo <- module_run(test_paper(paste(cases[[12]][[1]], ".064 (.006; .122)* .008 (-.017; .033)")), "stat_p_exact")
  expect_equal(mo$summary_table$n_imprecise %||% 0, 0)
  # a result reported imprecisely is still found, next to a note
  mo <- module_run(test_paper(c("Group A differed, F(1, 30) = 5.20, p < .05.", note)), "stat_p_exact")
  expect_equal(mo$summary_table$n_imprecise, 1)
  expect_equal(sum(!is.na(mo$table$not_a_test)), 3)
  # the report says where the threshold words can be wrong
  expect_true(any(grepl("sometimes flags false positives when sentences discuss how p-value thresholds were set",
                        mo$report, fixed = TRUE)))
})

test_that("stat_check without statistics gives every paper of a list a summary row (U124)", {
  p1 <- test_paper("no stats"); p1$paper_id <- "a"
  p2 <- test_paper("none"); p2$paper_id <- "b"
  mo <- module_run(paperlist(p1, p2), "stat_check")
  expect_equal(mo$summary_table$paper_id, c("a", "b"))
})
