test_that("coi_check: a short heading as the last sentence, and no pasted NA (U97)", {
  # nchar(NA) as the condition stopped the module, and a read past the end
  # pasted "NA" into the statement
  llm_use(FALSE)
  mo <- suppressMessages(module_run(test_paper(text = "Conflict of interest"), "coi_check"))
  expect_equal(mo$table$text, "Conflict of interest")
  mo <- suppressMessages(module_run(
    test_paper(text = c("Conflict of interest", "The authors declare none")), "coi_check"))
  expect_false(grepl("NA", mo$table$text, fixed = TRUE))
})

test_that("coi_check keeps 'Nil.' and 'nothing to declare' statements (U95)", {
  # the clean-up kept only 4 of its 11 groups, so these became "" (red light)
  for (s in c("Conflict of interest: Nil. We thank our colleagues.",
              "Conflict of interest statement: The authors have nothing to declare. We thank our colleagues.")) {
    mo <- module_run(test_paper(s), "coi_check")
    expect_equal(mo$traffic_light, "green")
    expect_false(grepl("We thank", mo$table$text))
  }
})

test_that("coi_check takes the statement under a conflict-of-interest heading (#695, point 19)", {
  # a statement without the words "conflict of interest" ("None declared.")
  # under such a heading was missed; only the section's first paragraph is
  # taken, and a bare "Disclosure" heading counts only when it is about interests
  llm_use(FALSE)
  coi_paper <- function(header, paragraphs) {
    p <- test_paper(c("We ran a study.", paragraphs))
    p$text$section_id <- c(1, rep(2, length(paragraphs)))
    p$text$paragraph_id <- c(1, seq_along(paragraphs) + 1)
    p$section <- dplyr::tibble(section_id = 1:2, header = c("Method", header),
                               parent_section_id = NA_integer_,
                               section_type = "unknown", classification_score = 0)
    p
  }
  mo <- suppressMessages(module_run(coi_paper("Conflicts of Interest", c("None declared.", "Publisher's note: maps.")), "coi_check"))
  expect_equal(mo$traffic_light, "green")
  expect_equal(mo$table$text, "Conflicts of Interest None declared.")
  # letters split apart by the PDF reading
  mo <- suppressMessages(module_run(coi_paper("Co n fli c t o f i nte r e s t", "None."), "coi_check"))
  expect_equal(mo$traffic_light, "green")
  mo <- suppressMessages(module_run(coi_paper("Disclosure", "The authors have no financial ties to report."), "coi_check"))
  expect_equal(mo$traffic_light, "green")
  mo <- suppressMessages(module_run(coi_paper("Disclosure", "Statement of inclusion: our study brings together authors from several countries."), "coi_check"))
  expect_equal(mo$traffic_light, "red")
  mo <- suppressMessages(module_run(coi_paper("Financial disclosure", "None."), "coi_check"))
  expect_equal(mo$traffic_light, "red")
})

test_that("coi_check's report shows at most 5 sentences and 1,000 characters (#695, point 19)", {
  # the reading can let the paragraph under the heading run on into an
  # appendix's tables; the table keeps the whole statement
  llm_use(FALSE)
  tables <- paste0("Table A", 1:8, " lists values.")
  p <- test_paper(c("We ran a study.", "None declared.", tables))
  p$text$section_id <- c(1, rep(2, 9))
  p$text$paragraph_id <- c(1, rep(2, 9))
  p$section <- dplyr::tibble(section_id = 1:2, header = c("Method", "Conflict of interest"),
                             parent_section_id = NA_integer_,
                             section_type = "unknown", classification_score = 0)
  mo <- suppressMessages(module_run(p, "coi_check"))
  shown <- paste(unlist(mo$report), collapse = " ")
  expect_true(grepl("Table A4 lists values.\u2026", shown, fixed = TRUE))
  expect_false(grepl("Table A5", shown, fixed = TRUE))
  expect_true(grepl("Table A8", mo$table$text, fixed = TRUE))
  env <- new.env(parent = asNamespace("metacheck"))
  sys.source(metacheck:::module_find("coi_check"), envir = env)
  long <- env$.coi_short_text(paste("Conflict of interest", strrep("x", 1500)))
  expect_equal(nchar(long), 1000)
})

test_that("coi_check: the statement an article declares wins over a search that finds nothing", {
  # no sentence about interests in the text; the publisher's XML declares one
  p <- test_paper("We ran a study.")
  mo <- module_run(p, "coi_check")
  expect_equal(mo$traffic_light, "red")
  p$info$coi_statement <- "None."
  mo <- module_run(p, "coi_check")
  expect_equal(mo$traffic_light, "green")
  expect_equal(mo$table$text, "None.")
  expect_equal(mo$summary_table$coi_source, "metadata")

  # found in the text and declared: both; found in the text only: text
  p <- read(test_path("fixtures", "formats", "jats_cases.xml"))
  expect_equal(module_run(p, "coi_check")$summary_table$coi_source, "both")
  g <- test_paper("The authors declare no conflict of interest.")
  expect_equal(module_run(g, "coi_check")$summary_table$coi_source, "text")
})

test_that("a notice about another article needs no statement (.paper_notice)", {
  p <- test_paper("In the original article, Table 1 was wrong.")
  expect_true(is.na(.paper_notice(p)))
  expect_equal(module_run(p, "coi_check")$traffic_light, "red")
  expect_equal(module_run(p, "funding_check")$traffic_light, "red")

  # a correction: without a statement "na", not red
  p$info$article_type <- "correction"
  expect_equal(unname(.paper_notice(p)), "a correction notice")
  for (m in c("coi_check", "funding_check", "open_practices")) {
    mo <- module_run(p, m)
    expect_equal(mo$traffic_light, "na")
    expect_match(mo$summary_text, "this is a correction notice")
  }
  # the other kinds, and the names other publishers use
  kinds <- c(retraction = "a retraction notice", "partial-retraction" = "a retraction notice",
             corrigendum = "a correction notice", erratum = "a correction notice",
             addendum = "an addendum", "expression-of-concern" = "an expression of concern")
  for (k in names(kinds)) {
    p$info$article_type <- k
    expect_equal(unname(.paper_notice(p)), kinds[[k]])
  }
  # an article, an editorial: not a notice
  for (k in c("research-article", "editorial", "review-article")) {
    p$info$article_type <- k
    expect_true(is.na(.paper_notice(p)))
  }

  # a statement that is there is still reported
  p <- test_paper("The authors declare no conflict of interest.")
  p$info$article_type <- "correction"
  expect_equal(module_run(p, "coi_check")$traffic_light, "green")
})
