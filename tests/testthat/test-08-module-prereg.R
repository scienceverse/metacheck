# httptest::start_capturing()
httptest::use_mock_api()

test_that("multiple prereg", {
  paper <- read(demoxml())
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$template_name, c("OSF Preregistration", "AsPredicted"))
  expect_equal(mo$table$id, c("48ncu", "by8i8v"))
})

test_that("oer", {
  paper <- paper()
  paper$full_text <- data.frame(text = "")

  guid <- "5xysn"
  paper$full_text$text[[1]] <- paste0("https://osf.io/", guid)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Open-Ended Registration")
  expect_equal(mo$table$id, guid)
})

test_that("prc", {
  paper <- paper()
  paper$full_text <- data.frame(text = "")

  guid <- "jez3g"
  paper$full_text$text[[1]] <- paste0("https://osf.io/", guid)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Prereg Challenge")
  expect_equal(mo$table$id, guid)
})

test_that("osf_pr_28", {
  paper <- paper()
  paper$full_text <- data.frame(text = "")

  guid <- "g59u6"
  paper$full_text$text[[1]] <- paste0("https://osf.io/", guid)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "OSF Preregistration")
  expect_equal(mo$table$id, guid)
})

test_that("osf_pr_31", {
  paper <- paper()
  paper$full_text <- data.frame(text = "")

  guid <- "7qcxa"
  paper$full_text$text[[1]] <- paste0("https://osf.io/", guid)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "OSF Preregistration")
  expect_equal(mo$table$id, guid)
})

test_that("prap", {
  paper <- paper()
  paper$full_text <- data.frame(text = "")

  guid <- "7v28u"
  paper$full_text$text[[1]] <- paste0("https://osf.io/", guid)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Preregistration Template from AsPredicted.org")
  expect_equal(mo$table$id, guid)
})

test_that("rrbrandt", {
  paper <- paper()
  paper$full_text <- data.frame(text = "")

  guid <- "vzb48"
  paper$full_text$text[[1]] <- paste0("https://osf.io/", guid)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Replication Recipe (Brandt et al., 2013): Pre-Registration")
  expect_equal(mo$table$id, guid)
})

httptest::stop_mocking()
# httptest::stop_capturing()
