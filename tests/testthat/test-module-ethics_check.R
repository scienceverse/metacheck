test_that("ethics_check", {
  module <- "ethics_check"
  mods <- module_list()
  expect_true(module %in% mods$name)
})

test_that("approved and needs ethics", {
  module <- "ethics_check"
  paper <- test_paper(c(
    "Participants were recruited from a local community sample and gave informed consent.",
    "This study was approved by the institutional review board."
  ))
  mo <- module_run(paper, module)
  expect_equal(mo$summary_table$ethics_approved, TRUE)
  expect_equal(mo$summary_table$needs_ethics, TRUE)
  expect_equal(mo$traffic_light, "green")

  live_statements <- mo$summary_table$live_data_statements[[1]]
  expect_true(grepl("Participants were recruited", live_statements))
  expect_match(mo$report, "we would expect an ethics approval statement, and it was present", fixed = TRUE)
  expect_match(mo$report, "Participants were recruited", fixed = TRUE)
  expect_match(mo$report, "institutional review board", fixed = TRUE)
})

test_that("not approved but needs ethics", {
  module <- "ethics_check"
  paper <- test_paper("Participants were recruited from a local community sample and gave informed consent.")
  mo <- module_run(paper, module)
  expect_equal(mo$summary_table$ethics_approved, FALSE)
  expect_equal(mo$summary_table$needs_ethics, TRUE)
  expect_equal(mo$traffic_light, "red")

  live_statements <- mo$summary_table$live_data_statements[[1]]
  expect_true(grepl("Participants were recruited", live_statements))
  expect_match(mo$report, "we would expect an ethics approval statement, but it was not present", fixed = TRUE)
  expect_match(mo$report, "Participants were recruited", fixed = TRUE)
})

test_that("not approved and does not need ethics", {
  module <- "ethics_check"
  paper <- test_paper("This paper presents a theoretical model of decision-making.")
  mo <- module_run(paper, module)
  expect_equal(mo$summary_table$ethics_approved, FALSE)
  expect_equal(mo$summary_table$needs_ethics, FALSE)
  expect_true(is.na(mo$summary_table$live_data_statements[[1]]))
  expect_true(is.na(mo$summary_table$ethics_statements[[1]]))
  expect_equal(mo$traffic_light, "na")
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

test_that("ethics_check paperlist", {
  module <- "ethics_check"
  paper <- paperlist(
    test_paper(c(
      "Participants were recruited and gave informed consent.",
      "This study was approved by the ethics committee."
    )),
    test_paper("This paper presents a theoretical model of decision-making.")
  )
  mo <- module_run(paper, module)
  expect_equal(nrow(mo$summary_table), 2)
  expect_equal(mo$summary_table$ethics_approved, c(TRUE, FALSE))
  expect_equal(mo$summary_table$needs_ethics, c(TRUE, FALSE))
})

test_that("error: argument is of length zero", {
  module <- "ethics_check"
  paper <- psychsci$`0956797617714811`
  mo <- module_run(paper, module)

  expect_equal(mo$summary_table$ethics_approved, FALSE)
})
