test_that("types", {
  skip_if_offline("api.test.osf.io")
  reg_ids <- list(
    # withdrawn = "",
    # prsp = "",
    oer = "5xysn",
    prc = "jez3g",
    osf_pr_28 = "g59u6",
    osf_pr_31 = "7qcxa", # "9bg3z"
    prap = "7v28u",
    rrbrandt = "vzb48"
  )

  paper <- read(demoxml())
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$template_name, c("OSF Preregistration", "AsPredicted"))
  expect_equal(mo$table$id, c("48ncu", "by8i8v"))

  paper$full_text <- paper$full_text[1, ]

  # oer
  guid <- reg_ids$oer
  paper$full_text$text[[1]] <- paste0("https://osf.io/", guid)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Open-Ended Registration")
  expect_equal(mo$table$id, guid)

  # prc
  guid <- reg_ids$prc
  paper$full_text$text[[1]] <- paste0("https://osf.io/", guid)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Prereg Challenge")
  expect_equal(mo$table$id, guid)

  # osf_pr_28
  guid <- reg_ids$osf_pr_28
  paper$full_text$text[[1]] <- paste0("https://osf.io/", guid)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "OSF Preregistration")
  expect_equal(mo$table$id, guid)

  # osf_pr_31
  guid <- reg_ids$osf_pr_31
  paper$full_text$text[[1]] <- paste0("https://osf.io/", guid)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "OSF Preregistration")
  expect_equal(mo$table$id, guid)

  # prap
  guid <- reg_ids$prap
  paper$full_text$text[[1]] <- paste0("https://osf.io/", guid)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Preregistration Template from AsPredicted.org")
  expect_equal(mo$table$id, guid)

  # rrbrandt
  guid <- reg_ids$rrbrandt
  paper$full_text$text[[1]] <- paste0("https://osf.io/", guid)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Replication Recipe (Brandt et al., 2013): Pre-Registration")
  expect_equal(mo$table$id, guid)
})
