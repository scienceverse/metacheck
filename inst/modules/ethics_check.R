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
#' - `Institutional Review Board` / IRB (US standard), also `Internal Review Board`
#' - `Independent Ethics Committee` / IEC (ICH-GCP international clinical trial
#'   term); not `IEC` followed by a number, which is a technical standard
#' - `Ethical/Ethics Review Board` (generic)
#' - `Ethics (Advisory/Review) Committee/Board/Panel/Sub-committee/Commission`,
#'   in the singular or plural
#' - `Research Ethics Committee/Board/Panel` (UK/Canada/Australia)
#' - `Medical Ethics Committee`, `Bioethics Committee/Commission`, `Ethikkommission`
#' - `Ethical Review Authority/Unit/Office/Agency`
#' - `Human Subjects/Research Committee` / `Human Research Ethics (Advisory) Panel`
#' - `Human Research Protection Office/Program`, `Office for Human Subjects Protection`
#' - `Committee for/on the Protection of Human Subjects`
#' - `Committee(s) on/for (Medical and Health) Research Ethics`
#' - `Office of Research Ethics`
#' - Abbreviations: IEC, REC, REB, ERB, METC, DEC. REC/REB/ERB/DEC are short
#'   enough to collide with unrelated jargon (e.g. `REC` as a recognition-heuristic
#'   model abbreviation, `DEC` as a decision-task label, `Reb` as an author surname),
#'   so these four require an ethics/approval/committee/board/panel/institutional/
#'   protocol word within ~30 characters in the same sentence. IRB and IEC are kept
#'   unconstrained as they are not observed to collide in practice.
#' - Dutch: `Medisch-ethische toetsingscommissie` (METC)
#' - French: `Comité d'éthique` / `Comité d'Ethique`
#'
#' **Animal research committee names**
#' - `Institutional Animal Care and Use Committee` / IACUC (US standard)
#' - Any sentence containing `animal(s)` and `committee` (or `board`/`panel`) within ~5 words
#'   of each other — catches `Experimental Animal Care and Use Committee`,
#'   `Animal Ethics Committee`, `Animal Welfare Board`, `Animal Welfare Ethical Review Body`, etc.
#' - UK: `Animal Welfare Ethical Review Body` / AWERB
#' - Dutch: `Dierexperimentencommissie` / DEC (see DEC constraint above); `Instantie voor Dierenwelzijn` / IvD
#' - German: `Tierversuchskommission` / TvK (advisory committee under §15 Tierschutzgesetz)
#' - French: `Comité d'éthique en expérimentation animale` / CEEA; `Comité d'éthique animale`
#' - Spanish: `Comité de ética en experimentación animal` / CEEA
#'
#' **Approval phrasing**
#' - `Ethics/Ethical (approval|clearance) [was] (granted|obtained|received|given|secured)`
#' - `Ethically approved`
#' - `Approved by the (ethics|irb|institutional|independent|local|review|research) ...`
#' - `This/The study/protocol/experiment/procedures/methods/research [was] (approved|reviewed) by`
#' - `Study was ethically approved`
#' - `(Followed|met|follow) the ethical (guidelines|standards|requirements)`
#' - `In accordance with ... ethical (guidelines|standards)`
#' - `Ethics protocol`
#' - `Approved under (protocol|reference|number)`
#'
#' **Waiver / exemption**
#' - `Ethics/Ethical (waiver|exemption)`
#' - `IRB (waiver|exemption|exempt)`
#' - `Exempted from (ethics|irb|institutional review)`
#' - `Waived by the (ethics|irb|institutional|review) (committee|board)`
#' - `Deemed exempt`
#' - Ethics review that was not needed: `Ethics review was not required`,
#'   `did not require further ethical review`, `exempt from formal human
#'   research ethics review`, `no new institutional ethics review`
#'
#' A template line the authors have not filled in ("[Add ethics approval ...
#' here.]") is not a statement.
#'
#' **Declaration of Helsinki**
#' - `Declaration of Helsinki` / `Helsinki Declaration`
#'
#' Patterns are NOT designed to capture:
#' - General ethical considerations or ethical dilemmas in study content
#' - Mentions of "ethics" in theoretical or philosophical contexts
#' - Author approval of a manuscript (e.g. "all authors approved the final version")
#' - Ethical behaviour as a measured variable (e.g. "ethical decision-making")
#' - URLs containing "ethics" in path components
#'
#' **When a statement is expected**
#'
#' The module sorts each paper by where its data came from (the internal
#' `.data_source()`; the signs and how the rule was tested are described in
#' `R/data-source.R`):
#' - `own`: the authors collected data from people or animals, for this paper,
#'   in an earlier study of their own, or (a protocol) will collect them;
#' - `existing_records`: records about individual people that the authors
#'   obtained for this study, such as hospital records or a cohort;
#' - `existing_unclear`: existing data about people from a source the module
#'   could not identify;
#' - `open_resource`: an open resource any researcher can use, such as UK
#'   Biobank or NHANES;
#' - `none`: no data about people or animals (a review, simulation,
#'   commentary, theory, tutorial, coding of published papers, or figures
#'   about groups);
#' - `uncertain`: the signs were missing or pointed both ways;
#' - `no_text`: the paper has no text.
#'
#' A statement is expected for `own`, `existing_records` and
#' `existing_unclear`. A paper without a statement in these classes, or in
#' `uncertain`, is flagged. The module reads the text, and the results of
#' `power`, `stat_p_exact`, `stat_check`, `repo_check`, `data_check` and
#' `prereg_check` when they ran before it (run it after them); without
#' `stat_p_exact`, it finds the p-values in the text.
#'
#' On 199 PsyArXiv and medRxiv preprints labelled after the rule was fixed,
#' the module decided right whether a statement is needed for 86.9% (the
#' earlier live-data search: 83.4%). Its main errors: other people's studies
#' described in the words of a new study, records and specimens described
#' only as counts, and open resources the paper does not call open.
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
    "(institutional|internal)\\s+review\\s+boards?",
    "independent\\s+ethics\\s+committee",
    "ethical?\\s+review\\s+boards?",
    "ethics\\s+(advisory\\s+|review\\s+)?(committees?|boards?|panels?|sub-?committees?|commission)",
    "ethical\\s+(advisory\\s+|review\\s+)?(committees?|boards?|panels?|sub-?committees?|commission)",
    "research\\s+ethics\\s+(committees?|boards?|panels?)",
    "medical\\s+ethics\\s+committee",
    "bioethic(s|al)?\\s+(committee|commission|board)",
    "ethik-?kommission",
    "ethic(s|al)\\s+review\\s+(authority|unit|office|agency)",
    "human\\s+(subjects?|research)\\s+(committee|review\\s+board)",
    "human\\s+research\\s+ethics\\s+(advisory\\s+)?(panel|committee|board)",
    "human\\s+(subjects?|research)\\s+protections?\\s+(office|program|programme|committee)",
    "office\\s+(for|of)\\s+(the\\s+)?(protection\\s+of\\s+)?human\\s+(subjects?|research)(\\s+protections?)?",
    "committee\\s+(for|on)\\s+(the\\s+)?protection\\s+of\\s+human\\s+subjects",
    "committees?\\s+(for|on)\\s+(\\w+\\s+){0,4}research\\s+ethics",
    "office\\s+of\\s+research\\s+ethics",
    "\\birb\\b",
    # not a technical standard ("IEC 62471")
    "\\biec\\b($|[^ 0-9-]| +[^ 0-9])",
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
    "approved\\s+by\\s+.{0,80}(ethics|research)\\s+.{0,30}(committees?|boards?|panels?|sub-?committees?|council|office|commission)",
    "approved\\s+by\\s+.{0,80}(committees?|boards?|panels?|sub-?committees?|council|commission)\\s+.{0,40}(ethic|research)",
    "(received|obtained)\\s+(ethics|ethical)?\\s*(approval|clearance)\\s+from\\s+.{0,80}(ethics|irb|institutional|research|review)\\s+.{0,30}(committees?|boards?|panels?|sub-?committees?|office|commission)",
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
    # ethics review that was not needed ("did not require further ethical
    # review", "exempt from formal human research ethics review")
    "(ethics|ethical|irb|institutional)\\s+review\\s+(was|is)\\s+(not\\s+(required|needed|necessary|sought)|waived)",
    "not\\s+(require|requiring|required|need|needed)\\s+(an?\\s+)?(further\\s+|formal\\s+|additional\\s+|separate\\s+)?(ethics|ethical|irb)\\s+(review|approval|clearance)",
    "exempt(ed|ion)?\\s+from\\s+(formal\\s+|further\\s+)?(\\w+\\s+){0,3}(ethics|ethical|irb)\\s+review",
    "\\bno\\s+(new\\s+|further\\s+)?(institutional\\s+)?ethics\\s+review",
    "scope\\s+of\\s+.{0,40}ethical\\s+review",
    # Helsinki
    "declaration\\s+of\\s+helsinki",
    "helsinki\\s+declaration"
  )

  table <- paper |>
    text_search(ethics_words)
  # a template line the authors have not filled in yet ("[Add ethics
  # approval, recruitment, and stimulus details here.]") is not a statement;
  # an anonymised one ("approved by the XXX University ethics committee") is,
  # and so is one followed by a layout note ("[INSERT TABLE 1 HERE]")
  if (nrow(table) > 0) {
    placeholder <- grepl("\\[\\s*(add|insert)\\b[^]]*(ethic|approv|irb|committee|board)",
                         table$text, ignore.case = TRUE, perl = TRUE)
    table <- table[!placeholder, ]
  }
  table$ethics <- TRUE
  if (!"text" %in% names(table)) table$text <- character(0)

  # where the data came from ----
  # read from the text, and from the results of the modules that ran before
  # this one (see .data_source() in R/data-source.R). A for loop, not
  # lapply(): get_prev_outputs() looks two frames up for the chain.
  prev <- list()
  for (m in c("power", "stat_p_exact", "stat_check", "repo_check", "data_check", "prereg_check")) {
    if (is.null(get_prev_outputs(m, "traffic_light"))) next
    prev[[m]] <- list(table = get_prev_outputs(m, "table"),
                      ownership = get_prev_outputs(m, "ownership"))
  }
  ds <- .data_source(paper, prev = prev)
  source_table <- ds$evidence
  # the sentences that show the authors' own data collection, under the
  # name the run scripts of the preprint repositories count (totals.py)
  source_table$live_data <- source_table$sign %in% "own"

  # re-order by paper_id and text_id
  paper_ids <- paper_id(paper)
  table$paper_id <- factor(table$paper_id, paper_ids)
  table <- dplyr::arrange(table, paper_id, text_id)
  table$paper_id <- as.character(table$paper_id)

  # summary_table ----
  all_ids <- data.frame(paper_id = paper_ids)

  if (nrow(table) > 0) {
    ethics_summary <- table |>
      summarise(
        ethics_approved = any(ethics),
        ethics_statements = list(unique(text[ethics])),
        .by = paper_id
      )
    ethics_na <- lengths(ethics_summary$ethics_statements) == 0
    ethics_summary$ethics_statements[ethics_na] <- NA_character_
  } else {
    ethics_summary <- dplyr::tibble(
      paper_id = character(0),
      ethics_approved = logical(0),
      ethics_statements = list()
    )
  }

  source_summary <- ds$summary
  names(source_summary)[names(source_summary) == "signs"] <- "data_source_signs"
  source_summary$data_source_statements <- lapply(source_summary$paper_id, \(id) {
    s <- unique(source_table$text[source_table$paper_id == id])
    if (length(s)) s else NA_character_
  })

  summary_table <- all_ids |>
    dplyr::left_join(ethics_summary, by = "paper_id") |>
    dplyr::left_join(source_summary, by = "paper_id")
  summary_table$ethics_approved[is.na(summary_table$ethics_approved)] <- FALSE
  # vapply()/lengths(), not sapply(): for no papers sapply() returned list(),
  # and indexing with it stopped ("invalid subscript type 'list'", U79)
  ethics_null <- vapply(summary_table$ethics_statements, is.null, logical(1))
  summary_table$ethics_statements[ethics_null] <- NA_character_

  # The ethics statement the article declares (a paper read from a
  # publisher's XML has it in `info$ethics_statement`) wins over a search
  # that found nothing: it is the statement. `ethics_source` says where the
  # statement is from: "text", "metadata" or "both"; NA when neither.
  declared <- .ethics_declared(paper)
  k <- match(summary_table$paper_id, declared$paper_id)
  in_meta <- !is.na(k)
  in_text <- summary_table$ethics_approved
  summary_table$ethics_source <- ifelse(in_text & in_meta, "both",
                                        ifelse(in_meta, "metadata", ifelse(in_text, "text", NA_character_)))
  only_meta <- in_meta & !in_text
  summary_table$ethics_approved[only_meta] <- TRUE
  summary_table$ethics_statements[only_meta] <- as.list(declared$text[k[only_meta]])

  # traffic_light + summary_text ----
  # A statement is expected for the authors' own data and for records
  # obtained for the study; a paper whose data source is uncertain is
  # flagged too, as we could not rule out that it needs one.
  needs     <- summary_table$needs_ethics %in% TRUE
  uncertain <- summary_table$data_source %in% "uncertain"
  approved  <- summary_table$ethics_approved
  n_check   <- sum(needs | uncertain)
  n_missing <- sum(needs & !approved)
  n_unsure  <- sum(uncertain & !approved)
  tl <- if (n_missing + n_unsure > 0) {
    # yellow, not red, until a validation shows people agree with most of its red findings (#695, point 2)
    "yellow"
  } else if (n_check > 0) {
    "green"
  } else {
    "na"
  }
  n <- nrow(summary_table)
  summary_text <- if (n_missing + n_unsure > 0) {
    paste0(
      if (n_missing > 0) sprintf(
        "%d of %d paper%s appeared to report data about people or animals that need an ethics statement, and lacked one.",
        n_missing, n, plural(n)),
      if (n_missing > 0 && n_unsure > 0) " ",
      if (n_unsure > 0) sprintf(
        "For %d of %d paper%s we could not tell whether %s data about people or animals, and found no ethics statement.",
        n_unsure, n, plural(n), if (n_unsure == 1) "it reports" else "they report")
    )
  } else if (n_check > 0) {
    "All papers that appeared to report data about people or animals that need an ethics statement contained one."
  } else {
    "No papers appeared to report data about people or animals that need an ethics statement."
  }

  # report ----
  # each statement quoted once: one found at several text_ids was quoted
  # each time (U102)
  report <- NULL

  if (nrow(summary_table) == 1) {
    quote <- \(x, max = 3) paste0("> ", utils::head(unique(x), max), collapse = "\n\n")
    src <- summary_table$data_source[[1]]
    is_approved <- summary_table$ethics_approved[[1]]
    statements <- unique(table$text[table$ethics])
    # the declared statement, when the search found none
    if (!length(statements) && is_approved) statements <- unlist(summary_table$ethics_statements[[1]])
    evidence <- source_table$text
    signs <- summary_table$data_source_signs[[1]]

    main <- switch(src,
      own = if (is_approved) {
        "The paper appears to report data that the authors collected from people or animals, and it has an ethics statement."
      } else {
        "The paper appears to report data that the authors collected from people or animals, but we found no ethics statement. Such data need one: approval by an ethics committee, or a reason why none was needed. We based this on the sentences below. A paper can describe someone else's study in the same words; if these data are not the authors' own, no statement is needed."
      },
      existing_records = if (is_approved) {
        "The paper appears to analyse records about individual people that the authors obtained for this study, and it has an ethics statement."
      } else {
        "The paper appears to analyse records about individual people that the authors obtained for this study, such as hospital records or a cohort, but we found no ethics statement. Such analyses usually need an approval or a waiver from an ethics committee."
      },
      existing_unclear = if (is_approved) {
        "The paper appears to analyse data about people, from a source we could not identify, and it has an ethics statement."
      } else {
        "The paper appears to analyse data about people, but we could not tell where the data came from. If the authors collected them, or obtained records for this study, an ethics statement is needed. If they come from an open resource such as UK Biobank or NHANES, it is not. We found no ethics statement."
      },
      open_resource = "The paper appears to analyse an open resource, which needs no ethics statement of its own.",
      none = sprintf("The paper does not appear to report data about individual people or animals (%s), so we do not expect an ethics statement.",
                     if (nzchar(signs)) paste("we found:", signs) else
                       "we found no method part, no sample description and no statistical results, and at most one sign that the authors collected data"),
      uncertain = if (is_approved) {
        "We could not tell whether this paper reports data about people or animals, but it has an ethics statement."
      } else {
        "We could not tell whether this paper reports data about people or animals: the signs we look for were missing or pointed both ways. If it does, it needs an ethics statement, and we found none."
      },
      no_text = "The paper has too little text to check."
    )
    parts <- main
    if (length(evidence) > 0) {
      parts <- c(parts, paste0("We based this on:\n\n", quote(evidence)))
    } else if (nzchar(signs) && !src %in% "none") {
      parts <- c(parts, paste0("We based this on: ", signs, "."))
    }
    if (is_approved) {
      parts <- c(parts, paste0("Ethics statement:\n\n", quote(statements, Inf)))
    }
    report <- paste(parts, collapse = "\n\n")
  }

  # return a list ----
  list(
    table = dplyr::bind_rows(table, source_table),
    summary_table = summary_table,
    na_replace = list(ethics_approved = FALSE),
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}

#' The ethics statement an article declares
#'
#' @param paper a paper object or paperlist object
#' @returns a data frame: `paper_id` and `text`, a row per paper whose info
#'   table has a declared statement (a paper read from a publisher's XML)
#' @noRd
.ethics_declared <- function(paper) {
  empty <- data.frame(paper_id = character(0), text = character(0))
  info <- tryCatch(paper_table(paper, "info"), error = function(e) NULL)
  if (!is.data.frame(info) || !all(c("paper_id", "ethics_statement") %in% names(info))) return(empty)
  has <- !is.na(info$ethics_statement) & nzchar(info$ethics_statement)
  data.frame(paper_id = as.character(info$paper_id[has]), text = as.character(info$ethics_statement[has]))
}
