test_that("ref_consistency", {
  module <- "ref_consistency"
  mods <- module_list()
  expect_true(module %in% mods$name)

  paper <- demopaper()

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")   # yellow, not red (#695, point 2)
  expect_equal(nrow(mod_output$table), 4)
  expect_equal(mod_output$module, module)

  # iteration
  skip_no_psychsci()
  paper <- psychsci[c(23, 25)]
  mod_output1 <- module_run(paper[[1]], module)
  mod_output2 <- module_run(paper[[2]], module)
  mod_output3 <- module_run(paper, module)
  t12 <- rbind(mod_output1$table, mod_output2$table)
  diffs <- dplyr::setdiff(t12, mod_output3$table)
  expect_equal(nrow(diffs), 0)
})

test_that("ref_miscitation", {
  module <- "ref_miscitation"
  mods <- module_list()
  expect_true(module %in% mods$name)

  paper <- demopaper()

  ## custom db
  test_doi <- "10.1037/0003-066x.54.6.408"
  db <- data.frame(
    doi = test_doi,
    reftext = "The full reference (this is a test)",
    warning = "Lorem ipsum this is a test..."
  )

  mod_output <- module_run(paper, module, db = db)
  expect_equal(mod_output$table$doi[[1]], test_doi)
  expect_equal(mod_output$summary_table$`miscite_10.1037/0003-066x.54.6.408`,
               1)

  ## all matched papers are reported
  db <- data.frame(
    doi = unique(na.omit(paper$bib$doi))[1:2],
    reftext = c("Reference A", "Reference B"),
    warning = c("Warning A", "Warning B")
  )
  mod_output <- module_run(paper, module, db = db)
  expect_length(mod_output$report, 2)
})


test_that("ref_accuracy", {
  module <- "ref_accuracy"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no references
  paper <- demopaper()
  paper$bib <- paper$bib[c(), ]
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # no bib_match
  paper <- demopaper()
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)
  expect_match(mod_output$summary_text, "not looked up at Crossref")

  # relevant references
  paper <- demopaper()
  mod_output <- module_run(paper, module)
  expect_equal(nrow(mod_output$table), nrow(paper$bib))

  # references that supplied a DOI are checked; the demo paper includes two
  # references with a DOI that does not resolve, which are flagged as incoherent
  st <- mod_output$summary_table
  expect_equal(st$refs_checked, 4)
  expect_equal(st$no_doi, 0)
  expect_equal(st$incoherent, 2)
  expect_equal(mod_output$traffic_light, "yellow")

  # those two are the "unresolved" tier: a cited DOI CrossRef could not find
  unresolved <- mod_output$table[mod_output$table$tier == "unresolved", ]
  expect_equal(nrow(unresolved), 2)
  expect_true(all(unresolved$incoherent))

  # a checked reference (one whose DOI resolves) whose cited journal differs
  # from the record its DOI points to is also incoherent. Corrupt the journal
  # of a "provided" reference and confirm one more reference is flagged.
  checked_id <- mod_output$table$bib_id[mod_output$table$tier == "provided"][[1]]
  bad <- demopaper()
  bad$bib$container[bad$bib$bib_id == checked_id] <-
    "A Completely Different Journal Name"
  bad_out <- module_run(bad, module)
  expect_equal(bad_out$traffic_light, "yellow")
  expect_equal(bad_out$summary_table$incoherent, 3)

  # min_mismatches controls whether a single title/author mismatch flags. A lone
  # author mismatch flags at the default (1) but not when set to 2.
  auth <- demopaper()
  auth$bib$authors[auth$bib$bib_id == checked_id] <- "Nonexistent, A; Madeup, B"
  loose  <- module_run(auth, module)                       # default = 1
  strict <- module_run(auth, module, min_mismatches = 2)   # conservative
  # the two unresolved references are always incoherent; the lone author
  # mismatch adds a third only at the sensitive setting
  expect_equal(loose$summary_table$incoherent, 3)
  expect_equal(strict$summary_table$incoherent, 2)
})

test_that("ref_replication", {
  module <- "ref_replication"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no references
  paper <- demopaper()
  paper$bib <- paper$bib[c(), ]
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # no DOIs
  paper <- demopaper()
  paper$bib$doi[[1]] <- NA
  paper$bib <- paper$bib[is.na(paper$bib$doi), ]
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # relevant references
  paper <- demopaper()
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(nrow(mod_output$table), 1)

  mod_output <- module_run(paper, module, show_outcomes = TRUE)
  expect_equal(mod_output$table$replication_outcome, "failed")
  expect_equal(mod_output$table$replication_type, "replication")
})

test_that("ref_retraction", {
  module <- "ref_retraction"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no references
  paper <- demopaper()
  paper$bib <- paper$bib[c(), ]
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # no DOIs
  paper <- demopaper()
  paper$bib <- paper$bib[is.na(paper$bib$doi), ]
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # relevant references
  paper <- demopaper()
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(nrow(mod_output$table), 1)
  expect_equal(mod_output$table$retractionwatch, "Retraction")
})

test_that("ref_pubpeer", {
  skip_no_psychsci()
  module <- "ref_pubpeer"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no references
  paper <- demopaper()
  paper$bib <- paper$bib[c(), ]
  paper$bib_match <- paper$bib_match[c(), ]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # no bib_match
  paper <- demopaper()
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  # 2, not 3: one of the 3 comments on 10.1177/0956797614520714 is Statcheck's,
  # which is left out of the count (#695, point 14); PubPeer's own data, so
  # this changes when someone comments on the cited papers
  expect_equal(sum(mod_output$table$total_comments), 2)

  # no DOIs
  paper <- demopaper()
  paper$bib <- paper$bib[is.na(paper$bib$doi), ]
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # relevant references
  paper <- demopaper()
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(sum(mod_output$table$total_comments), 2)   # see above

  # multiple papers
  paper <- psychsci[c(4, 9)]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_gte(nrow(mod_output$table), 2)
  expect_equal(mod_output$summary_table$paper_id, names(psychsci)[c(4, 9)])
}, "mock")


test_that("ref_summary", {
  module <- "ref_summary"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # relevant references
  paper <- demopaper()
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(nrow(mod_output$table), 5)
  expect_in("doi", names(mod_output$table))
  expect_disjoint("crossref_doi_mismatch", names(mod_output$table))
  expect_disjoint("retractionwatch", names(mod_output$table))

  # chaining 1
  paper <- demopaper()
  mod_output <- paper |>
    module_run("ref_accuracy") |>
    module_run(module)
  expect_in("accuracy_mismatch", names(mod_output$table))
  expect_disjoint("retractionwatch", names(mod_output$table))
})

test_that("ref_pubpeer reports a failed PubPeer request instead of stopping (U23)", {
  # pubpeer_comments() gives NULL for an error answer; the join stopped on it
  local_mocked_bindings(pubpeer_comments = function(doi) NULL)
  mo <- module_run(demopaper(), "ref_pubpeer")
  expect_equal(mo$traffic_light, "fail")
  expect_match(mo$summary_text, "PubPeer could not be reached", fixed = TRUE)
})

test_that("ref_consistency reports a citation to a missing reference, and reads 'bib' xrefs (U115, U207)", {
  # a citation with a bib_id that is not in the bibliography was never
  # reported, and an older bibr export ("bib" xrefs) had no citations at all
  p <- demopaper()
  x <- p$xref[p$xref$xref_type == "bibr", ][1, ]
  x$xref_id <- 99L; x$contents <- "(Nobody, 1900)"
  p$xref <- rbind(p$xref, x)
  mo <- module_run(p, "ref_consistency")
  expect_equal(mo$summary_table$n_missing, 1)
  expect_true(99L %in% mo$table$bib_id)

  p2 <- demopaper()
  n_cited <- module_run(p2, "ref_consistency")$summary_table$n_xrefs
  p2$xref$xref_type[p2$xref$xref_type == "bibr"] <- "bib"
  expect_equal(module_run(p2, "ref_consistency")$summary_table$n_xrefs, n_cited)
})

test_that("ref_accuracy counts a reference with several bib_match rows once, and ignores footnote markers in titles (U114)", {
  # three joins multiplied duplicate bib_match rows (3 copies: 31 rows for 5
  # references); <sup> markers were compared as part of the title
  p <- demopaper()
  n <- nrow(module_run(p, "ref_accuracy")$table)
  p$bib_match <- rbind(p$bib_match, p$bib_match[1, ], p$bib_match[1, ])
  expect_equal(nrow(module_run(p, "ref_accuracy")$table), n)

  p <- demopaper()
  i <- match(p$bib_match$bib_id[1], p$bib$bib_id)
  before <- module_run(p, "ref_accuracy")$table
  p$bib$title[i] <- paste0(p$bib$title[i], "<sup>1</sup>")
  after <- module_run(p, "ref_accuracy")$table
  expect_equal(after$title_mismatch, before$title_mismatch)
})

test_that("ref_pubpeer counts a DOI cited three times once, and runs without url fields (U118)", {
  # 3 citations x 3 lookups gave 9 rows ("You cited 9 references", 18
  # comments); without a url the module stopped
  p <- test_paper("x")
  local_mocked_bindings(
    ref_table = function(paper) data.frame(paper_id = paper$paper_id, bib_id = 1:3,
                                           doi = "10.1/x", text = c("A", "A2", "A3")),
    pubpeer_comments = function(doi) data.frame(doi = doi, total_comments = 2, users = "someone"))
  mo <- module_run(p, "ref_pubpeer")
  expect_match(mo$summary_text, "You cited 1 reference", fixed = TRUE)
  expect_equal(mo$summary_table$pubpeer_comments, 2)
})

test_that("ref_retraction compares DOIs without case (U157)", {
  # a lower-case citation of a RetractionWatch DOI with capitals was missed
  p <- test_paper("x")
  local_mocked_bindings(ref_table = function(paper)
    data.frame(paper_id = paper$paper_id, bib_id = 1, doi = "10.1016/s0140-6736(97)11096-0",
               text = "Wakefield"))
  mo <- module_run(p, "ref_retraction")
  expect_equal(mo$table$doi, "10.1016/s0140-6736(97)11096-0")
  expect_match(mo$table$retractionwatch, "Retraction", fixed = TRUE)
})

test_that("ref_miscitation matches DOIs without case, and a missing DOI matches nothing (U117, U157)", {
  paper <- demopaper()
  test_doi <- "10.1037/0003-066x.54.6.408"
  db <- data.frame(doi = c(toupper(test_doi), NA), reftext = c("Ref", "None"),
                   warning = c("Warn", "No DOI"))
  mo <- module_run(paper, "ref_miscitation", db = db)
  expect_equal(unique(mo$table$doi), test_doi)
  expect_false("No DOI" %in% mo$table$warning)
})

test_that("ref_replication drops a citation of the original only when the same paper cites the replication (U116)", {
  # paper B citing a replication hid paper A's citation of the original
  fl <- FLoRA()
  row <- fl[!is.na(fl$doi_r) & nzchar(fl$doi_r) & !is.na(fl$doi_o) & nzchar(fl$doi_o), ][1, ]
  pa <- test_paper("x"); pa$paper_id <- "A"
  pb <- test_paper("y"); pb$paper_id <- "B"
  local_mocked_bindings(ref_table = function(paper)
    data.frame(paper_id = c("A", "B"), bib_id = 1L, doi = c(row$doi_o, row$doi_r),
               text = c("original", "replication")))
  mo <- module_run(paperlist(pa, pb), "ref_replication")
  # the replication paper B cites is still listed for paper A
  expect_true(tolower(row$doi_r) %in% tolower(mo$table$replication_doi[mo$table$paper_id == "A"]))
})

test_that("ref_replication does not suggest the citing paper itself (#695)", {
  # a replication report cites its original, and was told to cite itself
  fl <- FLoRA()
  row <- fl[!is.na(fl$doi_r) & nzchar(fl$doi_r) & !is.na(fl$doi_o) & nzchar(fl$doi_o), ][1, ]
  n_rep <- sum(tolower(fl$doi_o) == tolower(row$doi_o))
  pa <- test_paper("x"); pa$paper_id <- "A"
  pa$info$doi <- paste0("https://doi.org/", toupper(row$doi_r))
  pb <- test_paper("y"); pb$paper_id <- "B"
  local_mocked_bindings(ref_table = function(paper)
    data.frame(paper_id = c("A", "B"), bib_id = 1L, doi = row$doi_o, text = "original"))
  mo <- module_run(paperlist(pa, pb), "ref_replication")
  expect_false(tolower(row$doi_r) %in% tolower(mo$table$replication_doi[mo$table$paper_id == "A"]))
  expect_equal(sum(mo$table$paper_id == "A"), n_rep - 1)
  # another paper citing the same original is still told about it
  expect_true(tolower(row$doi_r) %in% tolower(mo$table$replication_doi[mo$table$paper_id == "B"]))
})

test_that("ref_replication keeps the individual report's URL in a project paper's reference", {
  # "Individual report available at <url>" lost its URL, which was taken for a
  # repeat of the link to the project paper's DOI
  fl <- FLoRA()
  row <- fl[grepl("available at https?://[^[:space:]]+$", fl$apa_ref_r), ][1, ]
  report_url <- sub(".*available at (https?://[^[:space:]]+)$", "\\1", row$apa_ref_r)
  p <- test_paper("x")
  local_mocked_bindings(ref_table = function(paper)
    data.frame(paper_id = p$paper_id, bib_id = 1L, doi = row$doi_o, text = "original"))
  mo <- module_run(p, "ref_replication")
  refs <- mo$table$replication_ref[mo$table$replication_doi %in% row$doi_r]
  expect_true(any(endsWith(refs, report_url)))
  report <- paste(unlist(mo$report), collapse = " ")
  expect_match(report, paste0("href='", report_url, "'"), fixed = TRUE)
})

test_that("ref_replication counts a replication as cited by any DOI FLoRA gives for it", {
  # an Open Science Collaboration row: doi_r is the project paper, url_r the
  # study's own report, as a DOI link
  fl <- FLoRA()
  rpp <- fl[fl$doi_r %in% "10.1126/science.aac4716" & grepl("doi.org/", fl$url_r), ]
  orig <- names(which(table(rpp$doi_o) >= 2))[1]
  rows <- rpp[rpp$doi_o == orig, ]
  report_doi <- sub("^https?://doi.org/", "", rows$url_r[1])
  p <- test_paper("x")
  local_mocked_bindings(ref_table = function(paper)
    data.frame(paper_id = p$paper_id, bib_id = 1:2, doi = c(orig, tolower(report_doi)),
               text = c("original", "report")))
  mo <- module_run(p, "ref_replication")
  # the cited study report is left out; the other study of the same original stays
  expect_false(rows$url_r[1] %in% mo$table$replication_url)
  expect_true(rows$url_r[2] %in% mo$table$replication_url)

  # the citing paper is a preprint listed in alt_identifier_r
  row <- fl[grepl("^10\\.31234/", fl$alt_identifier_r) & !grepl(",", fl$alt_identifier_r) &
              !is.na(fl$doi_r), ][1, ]
  p$info$doi <- paste0("https://doi.org/", row$alt_identifier_r)
  local_mocked_bindings(ref_table = function(paper)
    data.frame(paper_id = p$paper_id, bib_id = 1L, doi = row$doi_o, text = "original"))
  mo <- module_run(p, "ref_replication")
  expect_false(row$doi_r %in% mo$table$replication_doi)
})

test_that("ref_replication matches identifiers and titles as FLoRA writes them (#716)", {
  sici <- "10.1002/(sici)1099-0771(199806)11:2<107::aid-bdm292>3.0.co;2-y"
  fake <- dplyr::tibble(
    doi_o = c("10.1/orig", "10.1/orig", "10.1/orig", sici, "10.1/twin", "10.1/twin", "10.1/asch"),
    apa_ref_o = "Original",
    doi_r = c("10.2/rep1", "10.2/rep2", "10.2/rep4", "10.2/rep3",
              "10.2/twin1", "10.2/twin2", "10.2/nauts"),
    apa_ref_r = c("Rep 1", "Jones, A., & Lee, B. (2021). A replication of the original effect in a new sample. J.",
                  "Rep 4", "Rep 3",
                  "Brown, A. (2020). Reproduction of a twin study in management science. M.",
                  "Kim, B. (2021). Reproduction of a twin study in management science. M.",
                  "Nauts, S. (2014). Forming Impressions of Personality. Social Psychology."),
    url_r = NA_character_,
    title_r = c("Rep one", "A replication of the original effect in a new sample", "Rep four",
                "Rep three", rep("Reproduction of a twin study in management science", 2),
                "Forming Impressions of Personality"),
    alt_identifier_r = c("10.3/pre1, 10.3/pre2", NA, NA, NA, NA, NA, NA),
    outcome = "failed", outcome_quote = NA_character_, type = "replication"
  )
  p <- test_paper("x")
  local_mocked_bindings(FLoRA = function() fake)
  refs <- \(doi, text) data.frame(paper_id = p$paper_id, bib_id = seq_along(doi), doi = doi, text = text)
  local_mocked_bindings(ref_table = function(paper) refs(
    c("10.1/orig", "10.3/PRE2", NA, sici,
      "https://doi.org/10.1002/(sici)1099-0771(199806)11:2%3C107::aid-bdm292%3E3.0.co;2-y"),
    c("original", "preprint",
      "Jones, A. & Lee, B. (2021). A Replication of the Original Effect in a New Sample. Journal of Tests, 1.",
      "Hsee", "Hsee again")))
  mo <- module_run(p, "ref_replication")
  # rep1 is cited as its second alternative identifier; rep2 by its title and
  # first author without a DOI
  expect_setequal(mo$table$replication_doi, c("10.2/rep4", "10.2/rep3"))
  # an original cited twice, once percent-encoded, is one original with one
  # replication
  expect_equal(sum(mo$table$replication_doi == "10.2/rep3"), 2)
  expect_match(mo$summary_text, "We found 2 replications for 2 originals")
  expect_equal(mo$summary_table$replications, 2)

  # two reproductions with one title: the cited one is found by its first
  # author, and a third author's work with that title hides neither
  local_mocked_bindings(ref_table = function(paper) refs(
    c("10.1/twin", NA, NA),
    c("original", "Brown, A. (2020). Reproduction of a twin study in management science. M.",
      "Lee, C. (2019). Reproduction of a twin study in management science. Working paper.")))
  mo <- module_run(p, "ref_replication")
  expect_equal(mo$table$replication_doi, "10.2/twin2")

  # another version of the replication (a preprint with its own DOI) counts
  local_mocked_bindings(ref_table = function(paper) refs(
    c("10.1/orig", "10.31234/osf.io/abcde"),
    c("original", "Jones, A. (2020). A replication of the original effect in a new sample. PsyArXiv.")))
  mo <- module_run(p, "ref_replication")
  expect_false("10.2/rep2" %in% mo$table$replication_doi)

  # the paper is the replication, with a DOI FLoRA does not list
  p$info$title <- "A Replication of the Original Effect in a New Sample"
  p$info$doi <- "10.5/published"
  local_mocked_bindings(ref_table = function(paper) refs("10.1/orig", "original"))
  mo <- module_run(p, "ref_replication")
  expect_false("10.2/rep2" %in% mo$table$replication_doi)

  # a replication titled like its original is not cited by citing the original
  local_mocked_bindings(ref_table = function(paper) refs(
    "10.1/asch", "Asch, S. E. (1946). Forming impressions of personality. JASP, 41, 258."))
  mo <- module_run(p, "ref_replication")
  expect_equal(mo$table$replication_doi, "10.2/nauts")
})

test_that("reference modules run on a paper whose reference list could not be read", {
  # bib has no rows and only bib_id; ref_table() stopped at its join
  p <- test_paper("x")
  p$bib <- data.frame(bib_id = integer(0))
  rt <- ref_table(p)
  expect_equal(nrow(rt), 0)
  expect_equal(names(rt), c("paper_id", "bib_id", "doi", "text"))
  mo <- module_run(p, "ref_replication")
  expect_equal(mo$traffic_light, "na")
})

test_that("ref_summary labels unmatched references and keeps one row per reference (U119)", {
  # no_match with every *_mismatch FALSE gave NA; two replications repeated
  # the reference
  p <- demopaper()
  pid <- p$paper_id; b <- unique(ref_table(p)$bib_id)
  acc <- data.frame(paper_id = pid, bib_id = b[1:2], no_match = c(TRUE, FALSE),
                    title_mismatch = c(FALSE, TRUE), doi_mismatch = FALSE)
  rep_tbl <- data.frame(paper_id = pid, bib_id = b[3], replication_type = c("direct", "conceptual"))
  fake <- structure(list(module = "ref_replication", table = rep_tbl, paper = p,
                         summary_table = data.frame(paper_id = pid),
                         prev_outputs = list(ref_accuracy = list(table = acc))),
                    class = "metacheck_module_output")
  mo <- module_run(fake, "ref_summary")
  expect_equal(nrow(mo$table), length(b))
  expect_equal(mo$table$accuracy_mismatch[mo$table$bib_id == b[1]], "no match")
})

test_that("ref_pubpeer leaves Statcheck's comment out of the count (#695, point 14)", {
  p <- demopaper()
  dois <- unique(stats::na.omit(p$bib$doi))[1:3]
  local_mocked_bindings(pubpeer_comments = function(doi) data.frame(
    doi = dois, total_comments = c(1, 3, 2), url = "https://pubpeer.com/x",
    users = c("Statcheck", "Statcheck, A. Reader", "A. Reader, B. Writer")))
  mo <- module_run(p, "ref_pubpeer")
  # Statcheck alone: left out; with others: one comment less, and not a commenter
  expect_setequal(mo$table$doi, dois[2:3])
  expect_equal(mo$table$total_comments[mo$table$doi == dois[2]], 2)
  expect_equal(mo$table$users[mo$table$doi == dois[2]], "A. Reader")
  expect_match(paste(mo$report, collapse = " "), "Commenters", fixed = TRUE)
})
