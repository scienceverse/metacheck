#httptest2::start_capturing()
httptest2::use_mock_api()

testthat::local_mocked_bindings(
  online = \(...) TRUE
)

test_that("multiple prereg", {
  paper <- demopaper()
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$template_name, c("OSF Preregistration", "AsPredicted"))
  expect_equal(mo$table$id, c("48ncu", "by8i8v"))
})

test_that("oer", {
  guid <- "5xysn"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Open-Ended Registration")
  expect_equal(mo$table$id, guid)
})

test_that("prc", {
  guid <- "jez3g"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Prereg Challenge")
  expect_equal(mo$table$id, guid)
})

test_that("osf_pr_28", {
  guid <- "g59u6"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "OSF Preregistration")
  expect_equal(mo$table$id, guid)
})

test_that("osf_pr_31", {
  guid <- "7qcxa"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "OSF Preregistration")
  expect_equal(mo$table$id, guid)
})

test_that("osf_pre", {
  guid <- "dr42m"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "OSF-Standard Pre-Data Collection Registration")
  expect_equal(mo$table$id, guid)
})

test_that("prap", {
  guid <- "7v28u"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Preregistration Template from AsPredicted.org")
  expect_equal(mo$table$id, guid)
})

test_that("rrbrandt", {
  guid <- "vzb48"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Replication Recipe (Brandt et al., 2013): Pre-Registration")
  expect_equal(mo$table$id, guid)
})

test_that("multiple papers", {
  guid1 <- "48ncu"
  text1 <- paste0("https://osf.io/", guid1)
  paper1 <- test_paper(url = text1)

  guid2 <- "by8i8v"
  text2 <- paste0("https://aspredicted.org/", guid2, ".pdf")
  paper2 <- test_paper(url = text2)

  paper <- paperlist(paper1, paper2)

  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 2)
  expect_setequal(mo$table$template_name,
                  c("OSF Preregistration", "AsPredicted"))
  expect_setequal(mo$table$id, c(guid1, guid2))
  ids <- paper_id(paper)$paper_id
  expect_setequal(mo$table$paper_id, ids)
  expect_setequal(mo$summary_table$paper_id, ids)
  expect_setequal(mo$summary_table$preregistration, c(1,1))
})

httptest2::stop_mocking()
#httptest2::stop_capturing()
