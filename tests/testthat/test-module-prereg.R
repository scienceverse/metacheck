test_that("multiple prereg", {
  # the recorded AsPredicted web page, not the API
  withr::local_options(metacheck.researchbox.token = "")
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
  # the recorded AsPredicted web page, not the API
  withr::local_options(metacheck.researchbox.token = "")
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

test_that("inaccessible registration link is reported, not dropped silently", {
  # https://github.com/scienceverse/metacheck/issues/361
  # osf_type() returns "inaccessible" for a validly-formed OSF id that
  # cannot be read (private, embargoed, withdrawn, deleted) -- mocked here
  # since a real private resource cannot be safely recorded as a fixture.
  #
  # Uses with_mocked_bindings() (explicit scope), not local_mocked_bindings():
  # this project's custom test_that() wrapper in helper.R does not restore
  # local_mocked_bindings()'s deferred cleanup correctly -- confirmed the
  # mock leaks into the NEXT test's real calls when tried that way (same
  # class of problem test-archive-osf.R's .osf_status_error test comment
  # already documents for req_perform()).
  guid <- "abcde"
  paper <- test_paper(url = paste0("https://osf.io/", guid))
  mo <- testthat::with_mocked_bindings(
    module_run(paper, "prereg_check"),
    osf_type = function(guid) "inaccessible",
    .package = "metacheck"
  )

  # not the "no registrations" branch -- this is a real, distinct finding
  expect_false(grepl("no registrations", mo$summary_text))
  expect_match(mo$summary_text, "could not be accessed")
  expect_true(any(grepl("private, embargoed, or withdrawn", mo$report)))
  expect_true(any(grepl(guid, mo$report)))
})

test_that("registration that type-checks but fails to fetch is reported", {
  # https://github.com/scienceverse/metacheck/issues/361
  # osf_type() can say "registrations" while the later fetch still fails
  # (osf_get_all_pages() tags that with an osf_error attribute) -- confirm
  # this is tracked the same way as an osf_type()-detected inaccessible link.
  # See the note above on why with_mocked_bindings() (not
  # local_mocked_bindings()) is used here.
  guid <- "fghij"
  paper <- test_paper(url = paste0("https://osf.io/", guid))
  mo <- testthat::with_mocked_bindings(
    module_run(paper, "prereg_check"),
    osf_type = function(guid) "registrations",
    osf_get_all_pages = function(url, page_end = Inf) {
      metacheck:::.osf_error_result("forbidden")
    },
    .package = "metacheck"
  )

  expect_match(mo$summary_text, "could not be accessed")
  expect_true(any(grepl(guid, mo$report)))
})

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

test_that("AsPredicted preregistrations: one row each, AsPredicted-only papers, OSF-only offline (U109)", {
  # an AsPredicted-only paper stopped on osf_type(character(0)); several
  # AsPredicted preregistrations were pasted into one row with paper_id NA;
  # aspredicted_info() was called without AsPredicted links
  llm_use(FALSE)
  p1 <- test_paper(url = "https://aspredicted.org/abc123.pdf")
  p1$paper_id <- p1$info$paper_id <- "paper1"
  p2 <- test_paper(url = "https://aspredicted.org/xyz789.pdf")
  p2$paper_id <- p2$info$paper_id <- "paper2"
  called <- 0
  local_mocked_bindings(
    aspredicted_info = function(urls, ...) {
      called <<- called + 1
      data.frame(ap_url = gsub("^(https://)?", "https://", urls),
                 AP_title = c("Study A", "Study B")[seq_along(urls)],
                 AP_created = "2020-01-01", AP_data = NA, AP_hypotheses = "H1",
                 AP_key_dv = "DV", AP_conditions = "C", AP_analyses = "t-test",
                 AP_outliers = NA, AP_sample_size = c("100", "200")[seq_along(urls)],
                 AP_anything_else = NA)
    },
    osf_type = function(ids, ...) rep("nodes", length(ids))
  )
  mo <- suppressMessages(module_run(p1, "prereg_check"))
  expect_equal(mo$table$title, "Study A")

  mo <- suppressMessages(module_run(paperlist(p1, p2), "prereg_check"))
  expect_equal(mo$table$paper_id, c("paper1", "paper2"))
  expect_equal(mo$table$sample_size, c("100", "200"))
  expect_true(all(is.na(mo$table$existing_data_explanation)))   # not "NA"
  expect_equal(mo$summary_table$preregistration, c(1, 1))

  called <- 0
  mo <- suppressMessages(module_run(test_paper(url = "https://osf.io/abcde"), "prereg_check"))
  expect_equal(called, 0)
})

test_that("a registration is joined to its paper by OSF id, and counted once (U110)", {
  # an http:// or upper-case link matched no paper, and a link given twice
  # counted the registration twice
  paper <- test_paper(url = c("https://osf.io/r5bme", "https://osf.io/r5bme",
                              "http://OSF.IO/r5bme/"))
  mo <- module_run(paper, "prereg_check")
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$table$paper_id, paper$paper_id)
  expect_equal(mo$summary_table$preregistration, 1)
}, "mock")

test_that("prsp reads response keys exactly, and a U+FFFE label does not stop the module (U111)", {
  # `$` on the list matched partially ("84-5" returned "84-56"), and
  # tolower() stopped on U+FFFE
  env <- new.env(parent = asNamespace("metacheck"))
  sys.source(metacheck:::module_find("prereg_check"), envir = env)
  info <- list(id = "abcde", attributes = list(
    registration_responses = list(`84-56` = "an answer to another question")))
  expect_false(grepl("another question", env$prsp(info)$research_questions))
  expect_equal(env$osf_label_to_field("Study Design\ufffe"),
               env$osf_label_to_field("Study Design"))
})

test_that("an AsPredicted link that could not be read is named with its reason, not counted", {
  # it was a row with every field empty, counted as a preregistration found
  llm_use(FALSE)
  local_mocked_bindings(
    aspredicted_info = function(urls, ...) {
      rows <- lapply(urls, \(u) if (grepl("abc12", u)) {
        data.frame(ap_url = u, AP_title = "Study A", AP_created = "2017/08/18 06:55 (PT)",
                   AP_registered = "2017-08-18", AP_analyses = "ANOVA", AP_outliers = NA,
                   AP_secondary = "Exploratory: domains", AP_sample_size = "35",
                   error = NA_character_)
      } else {
        data.frame(ap_url = u, error = "Not Found")
      })
      dplyr::bind_rows(rows)
    },
    osf_type = function(ids, ...) rep("nodes", length(ids))
  )
  urls <- c("https://aspredicted.org/abc12.pdf", "https://aspredicted.org/zzz99.pdf")
  mo <- suppressMessages(module_run(test_paper(url = urls), "prereg_check"))
  expect_equal(nrow(mo$table), 1)
  expect_equal(mo$summary_table$preregistration, 1)
  expect_match(mo$summary_text, "1 AsPredicted link could not be read")
  expect_true(any(grepl("zzz99.pdf (Not Found)", mo$report, fixed = TRUE)))
  # version 1.05 of AsPredicted's questions asks about secondary analyses
  # where version 2.00 asks about outliers
  expect_equal(mo$table$exploratory_analyses, "Exploratory: domains")
  expect_equal(mo$table$date_registered, "2017-08-18")

  # nothing could be read: not "0 OSF links"
  mo <- suppressMessages(module_run(test_paper(url = urls[2]), "prereg_check"))
  expect_match(mo$summary_text,
               "^We found 1 OSF or AsPredicted link, but no accessible registrations")
  expect_equal(mo$summary_table$preregistration, 0)
  expect_true(any(grepl("zzz99.pdf (Not Found)", mo$report, fixed = TRUE)))
})

test_that("without a ResearchBox key, the report says AsPredicted's web pages were read", {
  llm_use(FALSE)
  local_mocked_bindings(
    aspredicted_info = function(urls, ...) {
      dplyr::bind_rows(lapply(urls, \(u) if (grepl("abc12", u)) {
        data.frame(ap_url = u, AP_title = "Study A", error = NA_character_)
      } else {
        data.frame(ap_url = u, error = "captcha")
      }))
    },
    osf_type = function(ids, ...) rep("nodes", length(ids))
  )
  note <- "were read from AsPredicted's web pages, because no ResearchBox API key is set"
  urls <- c("https://aspredicted.org/abc12.pdf", "https://aspredicted.org/zzz99.pdf")

  # in R: set a key with rbox_token()
  withr::local_options(metacheck.researchbox.token = "", metacheck.keys.shared = NULL)
  mo <- suppressMessages(module_run(test_paper(url = urls), "prereg_check"))
  r <- paste(unlist(mo$report), collapse = "\n")
  expect_match(r, note, fixed = TRUE)
  expect_match(r, "Set a key with `rbox_token()` (make one at https://researchbox.org/api).",
               fixed = TRUE)
  # also when none could be read
  mo <- suppressMessages(module_run(test_paper(url = urls[2]), "prereg_check"))
  expect_match(paste(unlist(mo$report), collapse = "\n"), note, fixed = TRUE)

  # in the apps: the key field
  withr::local_options(metacheck.keys.shared = TRUE)
  mo <- suppressMessages(module_run(test_paper(url = urls), "prereg_check"))
  expect_match(paste(unlist(mo$report), collapse = "\n"),
               "You can enter your own ResearchBox key under \"Your own API keys\"",
               fixed = TRUE)

  # not with a key
  withr::local_options(metacheck.researchbox.token = "test-token")
  mo <- suppressMessages(module_run(test_paper(url = urls), "prereg_check"))
  expect_false(grepl(note, paste(unlist(mo$report), collapse = "\n"), fixed = TRUE))
})
