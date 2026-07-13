#' Ethics Approval Check
#'
#' @description
#' This module searches for statements of ethics approval, IRB approval,
#' institutional review board approval, and related terms.
#'
#' @details
#' Patterns are designed to capture:
#'
#' **Human research committee names**
#' - "Institutional Review Board" / IRB (US standard)
#' - "Independent Ethics Committee" / IEC (ICH-GCP international clinical trial term)
#' - "Ethical/Ethics Review Board" (generic)
#' - "Ethics (Advisory/Review) Committee/Board/Panel/Sub-committee"
#' - "Research Ethics Committee/Board/Panel" (UK/Canada/Australia)
#' - "Medical Ethics Committee"
#' - "Human Subjects/Research Committee" / "Human Research Ethics (Advisory) Panel"
#' - "Committee for/on the Protection of Human Subjects"
#' - "Committee on (Health) Research Ethics"
#' - "Office of Research Ethics"
#' - Abbreviations: IEC, REC, REB, ERB, METC, DEC. REC/REB/ERB/DEC are short
#'   enough to collide with unrelated jargon (e.g. "REC" as a recognition-heuristic
#'   model abbreviation, "DEC" as a decision-task label, "Reb" as an author surname),
#'   so these four require an ethics/approval/committee/board/panel/institutional/
#'   protocol word within ~30 characters in the same sentence. IRB and IEC are kept
#'   unconstrained as they are not observed to collide in practice.
#' - Dutch: "Medisch-ethische toetsingscommissie" (METC)
#' - French: "Comité d'éthique" / "Comité d'Ethique"
#'
#' **Animal research committee names**
#' - "Institutional Animal Care and Use Committee" / IACUC (US standard)
#' - Any sentence containing "animal(s)" and "committee" (or "board"/"panel") within ~5 words
#'   of each other — catches "Experimental Animal Care and Use Committee",
#'   "Animal Ethics Committee", "Animal Welfare Board", "Animal Welfare Ethical Review Body", etc.
#' - UK: "Animal Welfare Ethical Review Body" / AWERB
#' - Dutch: "Dierexperimentencommissie" / DEC (see DEC constraint above); "Instantie voor Dierenwelzijn" / IvD
#' - German: "Tierversuchskommission" / TvK (advisory committee under §15 Tierschutzgesetz)
#' - French: "Comité d'éthique en expérimentation animale" / CEEA; "Comité d'éthique animale"
#' - Spanish: "Comité de ética en experimentación animal" / CEEA
#'
#' **Approval phrasing**
#' - `Ethics/Ethical (approval|clearance) [was] (granted|obtained|received|given|secured)`
#' - "Ethically approved"
#' - "Approved by the (ethics|irb|institutional|independent|local|review|research) ..."
#' - `This/The study/protocol/experiment/procedures/methods/research [was] (approved|reviewed) by`
#' - "Study was ethically approved"
#' - "(Followed|met|follow) the ethical (guidelines|standards|requirements)"
#' - "In accordance with ... ethical (guidelines|standards)"
#' - "Ethics protocol"
#' - "Approved under (protocol|reference|number)"
#'
#' **Waiver / exemption**
#' - "Ethics/Ethical (waiver|exemption)"
#' - "IRB (waiver|exemption|exempt)"
#' - "Exempted from (ethics|irb|institutional review)"
#' - "Waived by the (ethics|irb|institutional|review) (committee|board)"
#' - "Deemed exempt"
#'
#' **Declaration of Helsinki**
#' - "Declaration of Helsinki" / "Helsinki Declaration"
#'
#' Patterns are NOT designed to capture:
#' - General ethical considerations or ethical dilemmas in study content
#' - Mentions of "ethics" in theoretical or philosophical contexts
#' - Author approval of a manuscript (e.g. "all authors approved the final version")
#' - Ethical behaviour as a measured variable (e.g. "ethical decision-making")
#' - URLs containing "ethics" in path components
#'
#' Live data collection is detected by the internal helper `.detect_live_data()`,
#' which searches for sentences indicating that data were collected directly from
#' human or animal participants. Papers without any live-data signal are not
#' flagged as missing ethics approval. The helper uses conservative patterns to
#' avoid flagging secondary data analyses, computational experiments, or
#' economics papers that analyse existing survey data. See `.detect_live_data()`
#' for the full list of patterns and intentional exclusions.
#'
#' @keywords general
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
ethics_check <- function(paper) {

  ## patterns ----
  ethics_words <- c(
    # committee name variants — human research
    "institutional\\s+review\\s+board",
    "independent\\s+ethics\\s+committee",
    "ethical?\\s+review\\s+board",
    "ethics\\s+(advisory\\s+|review\\s+)?(committee|board|panel|sub-?committee)",
    "ethical\\s+(advisory\\s+|review\\s+)?(committee|board|panel|sub-?committee)",
    "research\\s+ethics\\s+(committee|board|panel)",
    "medical\\s+ethics\\s+committee",
    "human\\s+(subjects?|research)\\s+(committee|review\\s+board)",
    "human\\s+research\\s+ethics\\s+(advisory\\s+)?(panel|committee|board)",
    "committee\\s+(for|on)\\s+(the\\s+)?protection\\s+of\\s+human\\s+subjects",
    "committee\\s+on\\s+(health\\s+)?research\\s+ethics",
    "office\\s+of\\s+research\\s+ethics",
    "\\birb\\b",
    "\\biec\\b",
    "\\b(rec|reb|erb|dec)\\b.{0,30}\\b(ethic|approv|committee|board|panel|institutional|protocol)",
    "\\b(ethic|approv|committee|board|panel|institutional|protocol).{0,30}\\b(rec|reb|erb|dec)\\b",
    "\\bmetc\\b",
    "medisch[- ]ethische\\s+toetsingscommissie",
    "comit[eé]\\s+d.[ée]thique",
    # committee name variants — animal research
    "\\biacuc\\b",
    "\\bawerb\\b",
    "animals?\\s+(\\w+\\s+){0,4}(committee|board|panel)",
    "(committee|board|panel)\\s+(\\w+\\s+){0,4}animals?",
    "dierexperimentencommissie",
    "instantie\\s+voor\\s+dierenwelzijn",
    "\\bivd\\b",
    "tierversuchskommission",
    "\\btvk\\b",
    "comit[eé]\\s+d.[ée]thique\\s+(en\\s+exp[eé]rimentation\\s+animale|animale)",
    "\\bceea\\b",
    # approval phrasing
    "ethics\\s+(approval|clearance)\\s+(was\\s+)?(granted|obtained|received|given|secured)",
    "ethics\\s+(approval|clearance)",
    "ethical\\s+(approval|clearance)\\s+(was\\s+)?(granted|obtained|received|given|secured)",
    "ethical\\s+(approval|clearance)",
    "ethically\\s+approved",
    "ethical\\s+approval\\s+was\\s+(granted|obtained|received|given|secured)",
    "ethics\\s+approval\\s+was\\s+(granted|obtained|received|given|secured)",
    "received\\s+ethical?\\s+approval",
    "approved\\s+by\\s+(the\\s+)?(ethics|irb|institutional|independent|local|review|research)",
    "approved\\s+by\\s+.{0,60}ethics\\s+(committee|board|panel|sub-?committee)",
    "approved\\s+by\\s+.{0,80}(ethics|research)\\s+.{0,30}(committee|board|panel|sub-?committee|council|office)",
    "approved\\s+by\\s+.{0,80}(committee|board|panel|sub-?committee|council)\\s+.{0,40}(ethic|research)",
    "(received|obtained)\\s+(ethics|ethical)?\\s*(approval|clearance)\\s+from\\s+.{0,80}(ethics|irb|institutional|research|review)\\s+.{0,30}(committee|board|panel|sub-?committee|office)",
    "this\\s+study\\s+(was\\s+)?(approved|reviewed)\\s+by",
    "the\\s+study\\s+(was\\s+)?(approved|reviewed)\\s+by",
    "the\\s+protocol\\s+(was\\s+)?(approved|reviewed)\\s+by",
    "experiment\\s+(was\\s+)?(approved|reviewed)\\s+by",
    "procedures?\\s+(was\\s+|were\\s+)?(approved|reviewed)\\s+by",
    "methods?\\s+(was\\s+|were\\s+|reported\\s+in\\s+.{0,30})?approved\\s+by",
    "research\\s+(was\\s+)?(approved|reviewed)\\s+by",
    "study\\s+was\\s+ethically\\s+approved",
    "(followed|met|follow)\\s+(the\\s+)?ethical\\s+(guidelines|standards|requirements)",
    "in\\s+accordance\\s+with\\s+.{0,40}ethical\\s+(guidelines|standards)",
    "ethics\\s+protocol",
    "approved\\s+under\\s+(protocol|reference|number)",
    # waiver / exemption
    "ethics\\s+(waiver|exemption)",
    "ethical\\s+(waiver|exemption)",
    "\\birb\\s+(waiver|exemption|exempt)",
    "exempt(ed)?\\s+from\\s+(ethics|irb|institutional\\s+review)",
    "waived?\\s+(by\\s+)?(the\\s+)?(ethics|irb|institutional|review)\\s+(committee|board|review)?",
    "deemed\\s+exempt",
    # Helsinki
    "declaration\\s+of\\s+helsinki",
    "helsinki\\s+declaration"
  )

  table <- paper |>
    text_search(ethics_words)
  table$ethics <- TRUE
  if (!"text" %in% names(table)) table$text <- character(0)

  live_table <- .detect_live_data(paper)
  live_table$live_data <- TRUE

  # re-order by paper_id and text_id
  paper_ids <- paper_id(paper)
  table$paper_id <- factor(table$paper_id, paper_ids)
  table <- dplyr::arrange(table, paper_id, text_id)
  table$paper_id <- as.character(table$paper_id)

  live_table$paper_id <- factor(live_table$paper_id, paper_ids)
  live_table <- dplyr::arrange(live_table, paper_id, text_id)
  live_table$paper_id <- as.character(live_table$paper_id)

  # summary_table ----
  all_ids <- data.frame(paper_id = paper_ids)

  if (nrow(table) > 0) {
    ethics_summary <- table |>
      summarise(
        ethics_approved = any(ethics),
        ethics_statements = list(unique(text[ethics])),
        .by = paper_id
      )
    ethics_na <- sapply(ethics_summary$ethics_statements, length) == 0
    ethics_summary$ethics_statements[ethics_na] <- NA_character_
  } else {
    ethics_summary <- dplyr::tibble(
      paper_id = character(0),
      ethics_approved = logical(0),
      ethics_statements = list()
    )
  }

  if (nrow(live_table) > 0) {
    live_summary <- live_table |>
      summarise(
        needs_ethics = any(live_data),
        live_data_statements = list(unique(text[live_data])),
        .by = paper_id
      )
    live_na <- sapply(live_summary$live_data_statements, length) == 0
    live_summary$live_data_statements[live_na] <- NA_character_
  } else {
    live_summary <- dplyr::tibble(
      paper_id = character(0),
      needs_ethics = logical(0),
      live_data_statements = list()
    )
  }

  summary_table <- all_ids |>
    dplyr::left_join(ethics_summary, by = "paper_id") |>
    dplyr::left_join(live_summary, by = "paper_id")
  summary_table$ethics_approved[is.na(summary_table$ethics_approved)] <- FALSE
  summary_table$needs_ethics[is.na(summary_table$needs_ethics)] <- FALSE
  ethics_null <- sapply(summary_table$ethics_statements, is.null)
  summary_table$ethics_statements[ethics_null] <- NA_character_
  live_null <- sapply(summary_table$live_data_statements, is.null)
  summary_table$live_data_statements[live_null] <- NA_character_

  # traffic_light + summary_text ----
  n_needs   <- sum(summary_table$needs_ethics)
  n_missing <- sum(summary_table$needs_ethics & !summary_table$ethics_approved)
  tl <- if (n_needs == 0) {
    "na"
  } else if (n_missing == 0) {
    "green"
  } else {
    "red"
  }
  summary_text <- if (n_missing > 0) {
    sprintf(
      "%d of %d paper%s appeared to involve live data collection and lacked an ethics approval statement.",
      n_missing, nrow(summary_table), plural(nrow(summary_table))
    )
  } else if (n_needs > 0) {
    "All papers that appeared to involve live data collection contained an ethics approval statement."
  } else {
    "No papers appeared to involve live data collection requiring an ethics approval statement."
  }

  # report ----
  report <- NULL

  if (nrow(summary_table) == 1) {
    approved <- summary_table$ethics_approved
    needs    <- summary_table$needs_ethics
    live_statements <- summary_table$live_data_statements[[1]]

    if (!needs) {
      if (approved) {
        report <- sprintf(
          "An ethics approval statement was detected, based on the following text:\n\n> %s",
          paste(table$text[table$ethics], collapse = "\n\n> ")
        )
      } else {
        report <- "We did not detect an ethics approval statement, and this paper does not appear to involve live data collection from human or animal participants. Ethics approval is typically not required for theoretical work, simulations, reviews, meta-analyses, or secondary analyses of published data."
      }
    } else {
      live_quote <- paste(live_statements, collapse = "\n\n> ")
      if (approved) {
        report <- sprintf(
          "Based on the following text, we would expect an ethics approval statement, and it was present:\n\n> %s\n\nEthics approval statement:\n\n> %s",
          live_quote,
          paste(table$text[table$ethics], collapse = "\n\n> ")
        )
      } else {
        report <- sprintf(
          "Based on the following text, we would expect an ethics approval statement, but it was not present:\n\n> %s",
          live_quote
        )
      }
    }
  }

  # return a list ----
  list(
    table = dplyr::bind_rows(table, live_table),
    summary_table = summary_table,
    na_replace = list(ethics_approved = FALSE),
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}
