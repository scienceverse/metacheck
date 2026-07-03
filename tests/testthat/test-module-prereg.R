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

test_that("oer text-gate excludes an uncorroborated open-ended registration", {
  guid <- "5xysn"
  url <- paste0("https://osf.io/", guid)
  paper <- test_paper(text = paste("Data and materials are available at", url),
                      url = url)
  mo <- module_run(paper, "prereg_check", oer = "text")
  expect_null(mo$table)
  expect_equal(mo$summary_table$preregistration, 0)
}, "mock")

test_that("oer text-gate counts a corroborated open-ended registration", {
  guid <- "5xysn"
  url <- paste0("https://osf.io/", guid)
  paper <- test_paper(
    text = paste("The hypotheses and analyses were preregistered at", url),
    url = url)
  mo <- module_run(paper, "prereg_check", oer = "text")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$id, guid)
}, "mock")

test_that("oer text-gate attributes to the link (a prereg elsewhere does not count)", {
  guid <- "5xysn"
  url <- paste0("https://osf.io/", guid)
  paper <- test_paper(
    text = c("The analyses were preregistered on AsPredicted.",
             paste("Data and materials are available at", url)),
    url = url)
  mo <- module_run(paper, "prereg_check", oer = "text")
  expect_null(mo$table)
}, "mock")

test_that("oer text_broad counts a bare 'registered' statement that strict excludes", {
  guid <- "5xysn"
  url <- paste0("https://osf.io/", guid)
  paper <- test_paper(text = paste("The study was registered at", url), url = url)
  expect_null(module_run(paper, "prereg_check", oer = "text")$table)
  mo_broad <- module_run(paper, "prereg_check", oer = "text_broad")
  expect_equal(nrow(mo_broad$table), 1)
}, "mock")

test_that("oer = 'llm' falls back to text-gating when llm_use is off", {
  guid <- "5xysn"
  url <- paste0("https://osf.io/", guid)
  paper <- test_paper(text = paste("Data at", url), url = url)
  llm_use(FALSE)
  mo <- suppressMessages(module_run(paper, "prereg_check", oer = "llm"))
  expect_null(mo$table)
}, "mock")
