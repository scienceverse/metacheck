test_that("ethics_check", {
  module <- "ethics_check"
  mods <- module_list()
  expect_true(module %in% mods$name)
})


# a small paper with a title, an abstract and one more part, as the rule
# weighs where a sentence stands (#695, point 18)
ds_paper <- function(abstract = character(0), body = character(0),
                     header = "Method", title = "Test Paper") {
  p <- test_paper(c(abstract, body))
  p$text$section_id <- c(rep(0, length(abstract)), rep(1, length(body)))
  p$section <- dplyr::tibble(
    section_id = c(0, 1), header = c("Abstract", header),
    parent_section_id = NA_integer_, section_type = c("abstract", "method"),
    classification_score = 0
  )
  p$info$title <- title
  p
}

test_that("the authors' own data, with and without a statement", {
  module <- "ethics_check"
  own <- "Participants were recruited online and gave informed consent."
  mo <- module_run(ds_paper(body = own, header = "Participants"), module)
  expect_equal(mo$summary_table$data_source, "own")
  expect_equal(mo$summary_table$needs_ethics, TRUE)
  expect_equal(mo$summary_table$ethics_approved, FALSE)
  expect_equal(mo$traffic_light, "yellow")   # yellow, not red (#695, point 2)
  expect_match(mo$summary_table$data_source_signs, "consent; recruitment", fixed = TRUE)
  expect_equal(mo$summary_table$data_source_statements[[1]], own)
  expect_match(mo$report, "collected from people or animals, but we found no ethics statement", fixed = TRUE)
  expect_match(mo$report, "describe someone else's study in the same words", fixed = TRUE)
  expect_match(mo$report, own, fixed = TRUE)

  mo <- module_run(ds_paper(body = c(own, "This study was approved by the institutional review board."),
                            header = "Participants"), module)
  expect_equal(mo$summary_table$ethics_approved, TRUE)
  expect_equal(mo$traffic_light, "green")
  expect_match(mo$report, "and it has an ethics statement", fixed = TRUE)
  expect_match(mo$report, "institutional review board", fixed = TRUE)
  # "this study was approved" is not listed as a reason for the class "own"
  expect_no_match(mo$summary_table$data_source_signs, "records")
})

test_that("one sign of data collection needs a method part or a second sign", {
  module <- "ethics_check"
  s <- "Participants were recruited online."
  mo <- module_run(ds_paper(body = s, header = "Participants"), module)
  expect_equal(mo$summary_table$data_source, "own")
  mo <- module_run(ds_paper(body = s, header = "Discussion"), module)
  expect_equal(mo$summary_table$data_source, "none")
  expect_equal(mo$traffic_light, "na")
  expect_match(mo$report, "at most one sign that the authors collected data", fixed = TRUE)
  # a negation near a participant phrase is no statement that there are no data
  mo <- module_run(ds_paper(body = c(s, "No participants were excluded."), header = "Participants"), module)
  expect_equal(mo$summary_table$data_source, "own")
})

test_that("sentences about other people's studies are not the authors' own (#695, point 18)", {
  module <- "ethics_check"
  mo <- module_run(ds_paper(body = "For instance, in a study by Mazar and Zhong (2010, study 3) participants were recruited online and gave informed consent.",
                            header = "Introduction"), module)
  expect_equal(mo$summary_table$data_source, "none")
  # a year is not a citation
  mo <- module_run(ds_paper(body = "In 2021, participants were recruited online and gave informed consent.",
                            header = "Introduction"), module)
  expect_equal(mo$summary_table$data_source, "own")
})

test_that("papers without data about people need no statement", {
  module <- "ethics_check"
  mo <- module_run(ds_paper(abstract = "We conducted a systematic review and meta-analysis of 35 studies.",
                            body = c("Databases were searched in May 2024.",
                                     "The 21 included studies enrolled a total of 7,035 participants who gave informed consent.")), module)
  expect_equal(mo$summary_table$data_source, "none")
  expect_equal(mo$summary_table$needs_ethics, FALSE)
  expect_match(mo$report, "a review or meta-analysis named in the title or abstract", fixed = TRUE)

  mo <- module_run(ds_paper(abstract = "In this commentary, we argue that preregistration is useful.",
                            body = "Participants were recruited online in the original study (Smith, 2019)."), module)
  expect_equal(mo$summary_table$data_source, "none")

  mo <- module_run(ds_paper(body = c("No human subjects were involved in this study.",
                                     "We analysed country-level mortality data.",
                                     "Aggregate data were taken from Eurostat.")), module)
  expect_equal(mo$summary_table$data_source, "none")
  expect_match(mo$summary_table$data_source_signs, "figures about groups", fixed = TRUE)

  mo <- module_run(test_paper("This paper presents a theoretical model of decision-making."), module)
  expect_equal(mo$summary_table$data_source, "none")
  expect_true(is.na(mo$summary_table$ethics_statements[[1]]))
  expect_equal(mo$traffic_light, "na")
})

test_that("open resources need no statement; obtained records do", {
  module <- "ethics_check"
  for (src in c("UK Biobank", "British Cohort Study")) {
    mo <- module_run(ds_paper(abstract = paste("We analysed data from the", src, "."),
                              body = c(paste("Data were obtained from the", src, "resource."),
                                       "Participants had a mean age of 56 years.")), module)
    expect_equal(mo$summary_table$data_source, "open_resource", info = src)
    expect_equal(mo$summary_table$needs_ethics, FALSE, info = src)
    expect_match(mo$report, "open resource, which needs no ethics statement", fixed = TRUE)
  }

  mo <- module_run(ds_paper(abstract = "This retrospective cohort study used electronic health records.",
                            body = c("We retrospectively reviewed the records of 412 patients treated at the university hospital.",
                                     "Consent was waived because the data were anonymised.")), module)
  expect_equal(mo$summary_table$data_source, "existing_records")
  expect_equal(mo$summary_table$needs_ethics, TRUE)
  expect_equal(mo$traffic_light, "yellow")
  expect_match(mo$report, "records about individual people that the authors obtained for this study", fixed = TRUE)

  # existing data from a source the module cannot identify: the report says so
  mo <- module_run(ds_paper(abstract = "This is a secondary analysis of existing data.",
                            body = c("We used data from the Healthy Minds Study.",
                                     "The analytic sample comprised 2,004 students.")), module)
  expect_equal(mo$summary_table$data_source, "existing_unclear")
  expect_equal(mo$summary_table$needs_ethics, TRUE)
  expect_match(mo$report, "we could not tell where the data came from", fixed = TRUE)
  expect_match(mo$report, "UK Biobank or NHANES", fixed = TRUE)
})

test_that("protocols, uncertain papers and papers without text", {
  module <- "ethics_check"
  mo <- module_run(ds_paper(body = "Participants will be recruited online and will be asked to give informed consent."), module)
  expect_equal(mo$summary_table$data_source, "own")
  expect_match(mo$summary_table$data_source_signs, "future tense", fixed = TRUE)

  mo <- module_run(ds_paper(body = "We tested the model on new data."), module)
  expect_equal(mo$summary_table$data_source, "uncertain")
  expect_true(is.na(mo$summary_table$needs_ethics))
  expect_equal(mo$traffic_light, "yellow")
  expect_match(mo$report, "We could not tell whether this paper reports data about people or animals", fixed = TRUE)
  expect_match(mo$summary_text, "we could not tell whether it reports data", fixed = TRUE)

  mo <- module_run(paper("empty"), module)
  expect_equal(mo$summary_table$data_source, "no_text")
  expect_equal(mo$traffic_light, "na")
})

test_that("ethics_check reads the results of modules that ran before it", {
  p <- ds_paper(body = c("Participants were recruited online.",
                         "The groups differed, t(28) = 2.40, p = .023."),
                header = "Participants")
  out <- report_module_run(p, c("stat_check", "ethics_check"))
  expect_match(out$ethics_check$summary_table$data_source_signs,
               "results of other modules", fixed = TRUE)
})

test_that("ethics_check paperlist", {
  module <- "ethics_check"
  paper <- paperlist(
    ds_paper(body = c("Participants were recruited and gave informed consent.",
                      "This study was approved by the ethics committee."),
             header = "Participants"),
    test_paper("This paper presents a theoretical model of decision-making.")
  )
  mo <- module_run(paper, module)
  expect_equal(nrow(mo$summary_table), 2)
  expect_equal(mo$summary_table$ethics_approved, c(TRUE, FALSE))
  expect_equal(mo$summary_table$needs_ethics, c(TRUE, FALSE))
  expect_equal(mo$traffic_light, "green")
})

test_that("approved but does not need ethics", {
  module <- "ethics_check"
  paper <- test_paper("No ethical approval was required for the completion of the study as there were no human or animal subjects used for the conduct of the research.")
  mo <- module_run(paper, module)
  expect_equal(mo$summary_table$needs_ethics, FALSE)
})

test_that("waiver and exemption phrasing", {
  module <- "ethics_check"
  paper <- test_paper("The study was deemed exempt by the institutional review board.")
  mo <- module_run(paper, module)
  expect_true(any(mo$table$ethics))
})

test_that("Declaration of Helsinki", {
  module <- "ethics_check"
  paper <- test_paper("The experiment was conducted in accordance with the Declaration of Helsinki.")
  mo <- module_run(paper, module)
  expect_true(any(mo$table$ethics))
})

test_that("animal research committee", {
  module <- "ethics_check"
  paper <- test_paper("All procedures were approved by the Animal Ethics Committee.")
  mo <- module_run(paper, module)
  expect_true(any(mo$table$ethics))
})

test_that("REC/REB/DEC abbreviations match only near ethics context", {
  module <- "ethics_check"

  # genuine approvals using short abbreviations should still match
  approved <- paperlist(
    test_paper("REB approval for this study was obtained from Carleton University (#118953)."),
    test_paper("The protocol was approved by the Research Ethics Board (REB) of the university."),
    test_paper("Approval was obtained from the local research ethics committee (REC)."),
    test_paper("Ethical approval was granted by the Dierexperimentencommissie (DEC) of Utrecht University.")
  )
  mo <- module_run(approved, module)
  expect_equal(mo$summary_table$ethics_approved, c(TRUE, TRUE, TRUE, TRUE))

  # unrelated jargon that happens to contain REC/DEC/REB should NOT match
  not_approved <- paperlist(
    test_paper("We used the recognition heuristic (REC/basic) to forecast the rank order of the election outcome."),
    test_paper("The number of the decision task (DEC) was logged for each trial."),
    test_paper("Regret priming has been shown to reduce decisional errors (Connolly & Reb, 2012).")
  )
  mo2 <- module_run(not_approved, module)
  expect_equal(mo2$summary_table$ethics_approved, c(FALSE, FALSE, FALSE))
})

test_that("approval phrasing without a leading ethics/irb/institutional word", {
  module <- "ethics_check"

  # institution-name-first approvals, and "received approval from" phrasing,
  # found to be missed during corpus validation (no "ethics/irb/institutional"
  # word immediately after "approved by")
  approved <- paperlist(
    test_paper("All studies were approved by the Scientific Council of Research and Creation at the West University of Timisoara regarding compliance with ethical aspects in scientific research."),
    test_paper("The research received approval from the Basque Center on Cognition, Brain and Language (BCBL)'s Ethics and Scientific Committee (ref.: 030522SM)."),
    test_paper("All experiments were approved by the University of Western Australia's Human Research Ethics Office.")
  )
  mo <- module_run(approved, module)
  expect_equal(mo$summary_table$ethics_approved, c(TRUE, TRUE, TRUE))

  # near-miss phrasing that should NOT match
  not_approved <- paperlist(
    test_paper("Approved the submitted version for publication: FGH, JDH, MP, AR, and FAS."),
    test_paper("The current research was performed in compliance with ethical guidelines at the Faculty of Psychology, University of Bergen."),
    test_paper("CloudResearch approved participants for high-quality survey responses.")
  )
  mo2 <- module_run(not_approved, module)
  expect_equal(mo2$summary_table$ethics_approved, c(FALSE, FALSE, FALSE))
})

test_that("error: argument is of length zero", {
  module <- "ethics_check"
  paper <- psychsci$`0956797617714811`
  mo <- module_run(paper, module)

  expect_equal(mo$summary_table$ethics_approved, FALSE)
})

test_that("ethics_check quotes a statement once, and keeps a paper with an empty info table (U102)", {
  # a statement found at several text_ids was quoted each time; paper_id()
  # read ids from the info tables, so a paper without info rows had none
  s <- "We obtained ethics approval from the IRB."
  mo <- module_run(test_paper(c(s, s)), "ethics_check")
  expect_equal(lengths(regmatches(mo$report, gregexpr("obtained ethics approval", mo$report))), 1L)
  pl <- paperlist(paper("empty"), test_paper(s))
  expect_equal(paper_id(pl)[1], "empty")
})


test_that("more committee names and exemptions count as statements (#695, point 18)", {
  module <- "ethics_check"
  approved <- paperlist(
    test_paper("This study was evaluated and approved by the ETH Zurich Ethics Commission as proposal EK-2023-N-258."),
    test_paper("The empirical study was approved by the Bioethics Commission of the university."),
    test_paper("It was approved by the Regional Committees for Medical and Health Research Ethics (2017/2205)."),
    test_paper("All procedures were approved by the University of Nebraska-Lincoln Internal Review Board."),
    test_paper("The Swedish Ethical Review Authority approved the study protocol."),
    test_paper("The data were collected under a protocol approved by the Office for Human Subjects Protection at Florida State University."),
    test_paper("This secondary analysis did not require further ethical review."),
    test_paper("As such, this work was exempt from formal human research ethics review."),
    test_paper("This study was approved by the XXX University ethics committee."),
    test_paper("The study received ethical approval from the institutional review board (EK 227052019). [INSERT TABLE 1 HERE]")
  )
  mo <- module_run(approved, module)
  expect_equal(mo$summary_table$ethics_approved, rep(TRUE, 10))

  # a technical standard and a template line are not statements
  not_approved <- paperlist(
    test_paper("B was the IEC 62471 blue-light hazard function."),
    test_paper("RT was recorded in milliseconds. [Add ethics approval, recruitment, and stimulus details here.]")
  )
  mo2 <- module_run(not_approved, module)
  expect_equal(mo2$summary_table$ethics_approved, c(FALSE, FALSE))
})

test_that("ethics_check: the statement an article declares wins over a search that finds nothing", {
  # participants, and no sentence the search reads as an ethics statement
  txt <- c("We recruited 120 participants through Prolific and paid each of them 2 pounds.",
           "Participants gave informed consent and completed the questionnaire.")
  p <- test_paper(txt)
  mo <- module_run(p, "ethics_check")
  expect_false(mo$summary_table$ethics_approved)
  expect_true(is.na(mo$summary_table$ethics_source))

  # the publisher's XML declares one: it is the statement
  p$info$ethics_statement <- "The procedure was cleared by the faculty's review panel (no. 2021-17)."
  mo <- module_run(p, "ethics_check")
  expect_true(mo$summary_table$ethics_approved)
  expect_equal(mo$summary_table$ethics_source, "metadata")
  expect_equal(mo$summary_table$ethics_statements[[1]], p$info$ethics_statement)
  expect_true(any(grepl("review panel (no. 2021-17)", mo$report, fixed = TRUE)))

  # found in the text: "text"; found and declared: "both"
  q <- test_paper(c(txt, "The study was approved by the ethics committee of Test University."))
  expect_equal(module_run(q, "ethics_check")$summary_table$ethics_source, "text")
  q$info$ethics_statement <- "The study was approved by the ethics committee of Test University."
  expect_equal(module_run(q, "ethics_check")$summary_table$ethics_source, "both")
})
