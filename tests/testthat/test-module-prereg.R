test_that("multiple prereg", {
  paper <- demopaper()
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$table$template_name, c("OSF Preregistration", "AsPredicted"))
  expect_equal(mo$table$id, c("48ncu", "by8i8v"))
}, "mock")

test_that("oer", {
  guid <- "5xysn"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Open-Ended Registration")
  expect_equal(mo$table$id, guid)
}, "mock")

test_that("prc", {
  guid <- "jez3g"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Prereg Challenge")
  expect_equal(mo$table$id, guid)
}, "mock")

test_that("osf_pr_28", {
  guid <- "g59u6"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "OSF Preregistration")
  expect_equal(mo$table$id, guid)
}, "mock")

test_that("osf_pr_31", {
  guid <- "7qcxa"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "OSF Preregistration")
  expect_equal(mo$table$id, guid)
}, "mock")

test_that("osf_pre", {
  guid <- "dr42m"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "OSF-Standard Pre-Data Collection Registration")
  expect_equal(mo$table$id, guid)
}, "mock")

test_that("prap", {
  guid <- "7v28u"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Preregistration Template from AsPredicted.org")
  expect_equal(mo$table$id, guid)
}, "mock")

test_that("rrbrandt", {
  guid <- "vzb48"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "Replication Recipe (Brandt et al., 2013): Pre-Registration")
  expect_equal(mo$table$id, guid)
}, "mock")

test_that("vant veer (prsp)", {
  guid <- "r5bme"
  text <- paste0("https://osf.io/", guid)
  paper <- test_paper(url = text)
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(
    mo$table$template_name,
    "Pre-Registration in Social Psychology (van 't Veer & Giner-Sorolla, 2016): Pre-Registration"
  )
  expect_equal(mo$table$id, guid)
}, "mock")

# The unified, schema-driven extractor reads each field's label from the
# registration's schema (display_text for blocks-format, question title for
# pages-format) and maps it onto canonical prereg_schema fields. These tests
# check that the mapped fields hold the expected content, across both formats.

test_that("blocks-format extraction maps to canonical fields", {
  # 9h2pj is a current OSF Preregistration (v4, blocks-format)
  paper <- test_paper(url = "https://osf.io/9h2pj")
  mo <- module_run(paper, "prereg_check")

  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$template_name, "OSF Preregistration")

  # research-core fields are recognised and populated
  expect_contains(
    names(mo$table),
    c("research_questions", "sample_size", "statistical_tests",
      "inference_criteria", "data_exclusion_criteria")
  )
  expect_equal(mo$table$sample_size, "The total sample size will be 400.")
}, "mock")

test_that("pages-format extraction maps to canonical fields", {
  # g59u6 is an older OSF Preregistration (pages-format, qN keys)
  paper <- test_paper(url = "https://osf.io/g59u6")
  mo <- module_run(paper, "prereg_check")

  # the same canonical field names are produced from the pages schema's titles
  expect_contains(
    names(mo$table),
    c("research_questions", "sample_size", "statistical_tests")
  )
  expect_true(nzchar(mo$table$sample_size))
}, "mock")

test_that("AsPredicted-on-OSF extraction maps to canonical fields", {
  # 7v28u is an AsPredicted-on-OSF registration (pages, semantic word keys).
  # Its word-style keys (hypothesis, sample, ...) map onto canonical fields.
  paper <- test_paper(url = "https://osf.io/7v28u")
  mo <- module_run(paper, "prereg_check")

  expect_contains(
    names(mo$table),
    c("research_questions", "sample_size")
  )
  expect_true(nzchar(mo$table$sample_size))
}, "mock")

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
  expect_contains(names(mo$table), "paper_id")
  expect_setequal(mo$table$id, c(guid1, guid2))
  ids <- paper_id(paper)
  expect_setequal(mo$table$paper_id, ids)
  expect_setequal(mo$summary_table$paper_id, ids)
  expect_setequal(mo$summary_table$preregistration, c(1,1))
}, "mock")

test_that("combine >10 OSF registrations", {
  # https://github.com/scienceverse/metacheck/issues/262

  # with >10 reg, osf_get_all_pages will paginate
  # and the different reg forms often return incompatible DFs
  reg_urls <- c("https://osf.io/bdvxs",
               "https://osf.io/wrh4x",
               "https://osf.io/trwb4",
               "https://osf.io/z2bsa",
               "https://osf.io/jez3g",
               "https://osf.io/9bg3z",
               "https://osf.io/7qcxa",
               "https://osf.io/a6y7r",
               "https://osf.io/4v3sg",
               "https://osf.io/hwu9x",
               "https://osf.io/yab8q")
  paper <- test_paper(url = reg_urls)
  mo <- module_run(paper, "prereg_check")

  expect_equal(mo$summary_table$preregistration, 11)
  expect_equal(nrow(mo$table), 11)
  expect_contains(names(mo$table), "paper_id")
}, "mock")
