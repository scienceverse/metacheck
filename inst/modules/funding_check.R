#' Funding Check
#'
#' @description
#' Identify and extract funding statements.
#'
#' @details
#' The Funding Check module uses regular expressions to check sentences for words related to funding statements. It will return the sentence in which the funding statement was found. The function is adapted from [rtransparent](https://github.com/serghiou/rtransparent), which is no longer maintained.
#'
#' The regular expressions can miss information, or falsely detect sentences as a funding statement. For the validation, see [the paper](https://doi.org/10.1371/journal.pbio.3001107).
#'
#' @keywords general
#'
#' @author Stylianos Serghiou
#' @author Daniel Lakens (\email{d.lakens@tue.nl})
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
#'
funding_check <- function(paper) {

  index <- integer()
  disclosure <- integer()
  diff <- integer()

  # Index sets (any)
  index_any <- list()
  index_any[["support_1"]]    <- get_support_1(paper$full_text$text)
  # index_any[["support_2"]] <- get_support_2(paragraphs_pruned)
  index_any[["support_3"]]    <- get_support_3(paper$full_text$text)
  index_any[["support_4"]]    <- get_support_4(paper$full_text$text)
  index_any[["support_5"]]    <- get_support_5(paper$full_text$text)
  index_any[["support_6"]]    <- get_support_6(paper$full_text$text)
  index_any[["support_7"]]    <- get_support_7(paper$full_text$text)
  index_any[["support_8"]]    <- get_support_8(paper$full_text$text)
  index_any[["support_9"]]    <- get_support_9(paper$full_text$text)
  index_any[["support_10"]]   <- get_support_10(paper$full_text$text)
  index_any[["developed_1"]]  <- get_developed_1(paper$full_text$text)
  index_any[["received_1"]]   <- get_received_1(paper$full_text$text)
  index_any[["received_2"]]   <- get_received_2(paper$full_text$text)
  index_any[["recipient_1"]]  <- get_recipient_1(paper$full_text$text)
  index_any[["authors_1"]]    <- get_authors_1(paper$full_text$text)
  index_any[["authors_2"]]    <- get_authors_2(paper$full_text$text)
  index_any[["thank_1"]]      <- get_thank_1(paper$full_text$text)
  index_any[["thank_2"]]      <- get_thank_2(paper$full_text$text)
  index_any[["fund_1"]]       <- get_fund_1(paper$full_text$text)
  index_any[["fund_2"]]       <- get_fund_2(paper$full_text$text)
  index_any[["fund_3"]]       <- get_fund_3(paper$full_text$text)
  index_any[["supported_1"]]  <- get_supported_1(paper$full_text$text)
  index_any[["financial_1"]]  <- get_financial_1(paper$full_text$text)
  index_any[["financial_2"]]  <- get_financial_2(paper$full_text$text)
  index_any[["financial_3"]]  <- get_financial_3(paper$full_text$text)
  index_any[["grant_1"]]      <- get_grant_1(paper$full_text$text)
  index_any[["french_1"]]     <- get_french_1(paper$full_text$text)
  index_any[["common_1"]]     <- get_common_1(paper$full_text$text)
  index_any[["common_2"]]     <- get_common_2(paper$full_text$text)
  index_any[["common_3"]]     <- get_common_3(paper$full_text$text)
  index_any[["common_4"]]     <- get_common_4(paper$full_text$text)
  index_any[["common_5"]]     <- get_common_5(paper$full_text$text)
  index_any[["acknow_1"]]     <- get_acknow_1(paper$full_text$text)
  index_any[["disclosure_1"]] <- get_disclosure_1(paper$full_text$text)
  index_any[["disclosure_2"]] <- get_disclosure_2(paper$full_text$text)

  index <- sort(unique(unlist(index_any)))

  # Remove potential mistakes (absence)
  if (length(index) > 0) {
    is_absent <- negate_absence_1(paper$full_text$text[index])
    index <- index[!is_absent]
  }

  # Identify potentially missed signals within Acknowledgements
  index_fund <- list()
  if (length(index) == 0) {
    from <- .where_acknows_txt(paper$full_text$text)
    to <- .where_refs_txt(paper$full_text$text) - 1
    if (length(from) > 0 && length(to) > 0) {
      diff <- to - from
      if (diff < 0) {
        to <- min(length(paper$full_text$text), from + 100)
        diff <- to - from
      }
      if (diff <= 100) {
        index_fund[["fund"]]    <- get_fund_acknow(paper$full_text$text[from:to])
        index_fund[["project"]] <- get_project_acknow(paper$full_text$text[from:to])
        adjusted <- unlist(index_fund) + (from - 1)
        index <- adjusted
      }
    }
  }

  index <- sort(unique(index))
  is_funded_pred <- length(index) > 0
  funding_text <- paste(paper$full_text$text[index], collapse = " ")

  # Summaries of index sets (lengths)
  index_any  <- purrr::map(index_any,  length)
  index_fund <- purrr::map(index_fund, length)

  id <- paper$id
  doi <- paper$info$doi

  summary_table <- tibble::tibble(id = id, doi = doi, is_funded_pred = is_funded_pred, funding_text = funding_text)

  # Normalize the index lists to data frames with the same row count as results
  index_any_df <- if (length(index_any) == 0L) {
    summary_table[, FALSE]  # 0-column data frame with nrow(results) rows
  } else {
    as.data.frame(index_any, check.names = FALSE)
  }

  index_fund_df <- if (length(index_fund) == 0L) {
    summary_table[, FALSE]
  } else {
    as.data.frame(index_fund, check.names = FALSE)
  }

  # Bind
  table <- cbind(summary_table, index_any_df, index_fund_df)

  # traffic light ----
  tl <- ifelse(is_funded_pred, "green", "red")

  # report ----
  if (tl == "green") {
    report <- sprintf("The following funding statement was detected: \n%s",
                      funding_text)
    summary_text <- "A funding statement was detected"
  } else if (tl == "red") {
    report <- "No funding statement was detected. Consider adding one."
    summary_text <- "No funding statement was detected"
  }

  # return list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}



# -------------------------------
# Utilities
# -------------------------------

# Cache for synonyms to avoid recomputing
.syn_cache <- new.env(parent = emptyenv())

.create_synonyms <- function() {
  if (!is.null(.syn_cache$syn)) return(.syn_cache$syn)

  synonyms <- list()

  synonyms[["txt"]] <- "[a-zA-Z0-9\\s,()\\[\\]/:-]*"  # order matters

  synonyms[["This"]] <- c("This", "These", "The", "Our", "All")
  synonyms[["This_singular"]] <- c("This", "The", "Our")
  synonyms[["These"]] <- c("These", "Our", "Research", "All")
  synonyms[["this"]] <- c("[Tt]his", "[Tt]hese", "[Tt]he", "[Oo]ur")
  synonyms[["this_singular"]] <- c("this", "the")
  synonyms[["these"]] <- c("these")

  synonyms[["is"]] <- c("is", "are", "was", "were", "been")
  synonyms[["is_singular"]] <- c("is", "was", "have", "has")
  synonyms[["are"]] <- c("are", "were", "have", "had")
  synonyms[["have"]] <- c("have", "has", "had")
  synonyms[["is_have"]] <- c(synonyms[["is"]], synonyms[["have"]])

  synonyms[["We"]] <- c("We")

  synonyms[["by"]] <- c("by", "from", "within", "under")
  synonyms[["and"]] <- c("and", "&", "or")
  synonyms[["for"]] <- c("for")
  synonyms[["of"]] <- c("of", "about")
  synonyms[["for_of"]] <- c(synonyms[["for"]], synonyms[["of"]])

  synonyms[["no"]] <- c("[Nn]o", "[Nn]il", "[Nn]one", "[Nn]othing")
  synonyms[["No"]] <- c("N(?i)o(?-i)", "N(?i)il(?-i)", "N(?i)one(?-i)", "N(?i)othing(?-i)")
  synonyms[["not"]] <- c("not")

  synonyms[["author"]] <- c(
    "author(|s|\\(s\\))", "researcher(|s|\\(s\\))",
    "investigator(|s|\\(s\\))", "scientist(|s|\\(s\\))"
  )

  synonyms[["research"]] <- c(
    "[Ww]ork(|s)", "[Rr]esearch", "[Ss]tud(y|ies)", "[Pp]roject(|s)",
    "[Tt]rial(|s)", "[Pp]ublication(|s)", "[Rr]eport(|s)", "[Pp]rogram(|s)",
    "[Pp]aper(|s)", "[Mm]anuscript(|s)", "[Aa]nalys(is|es)", "[Ii]nvestigation(|s)",
    "[Pp]rotocol(|s)", "[Cc]ohort(|s)", "[Cc]ollaboration(|s)"
  )
  synonyms[["research_strict"]] <- c(
    "[Ww]ork(|s)", "[Rr]esearch", "[Ss]tud(y|ies)", "[Pp]roject(|s)",
    "[Tt]rial(|s)", "[Pp]rogram(|s)", "[Aa]nalys(is|es)", "[Ii]nvestigation(|s)",
    "[Pp]rotocol(|s)", "[Cc]ohort(|s)", "[Cc]ollaboration(|s)"
  )
  synonyms[["research_lower"]] <- c(
    "work(|s)", "research", "stud(y|ies)", "project(|s)", "trial(|s)",
    "publication(|s)", "report(|s)", "program(|s)", "paper(|s)",
    "manuscript(|s)", "analys(is|es)", "investigation(|s)", "protocol(|s)",
    "cohort(|s)", "collaboration(|s)"
  )
  synonyms[["research_lower_strict"]] <- c(
    "work(|s)", "research", "stud(y|ies)", "project(|s)", "trial(|s)",
    "program(|s)", "analys(is|es)", "investigation(|s)", "protocol(|s)",
    "cohort(|s)", "collaboration(|s)"
  )
  synonyms[["Research"]] <- c(
    "Work(|s)", "Research", "Stud(y|ies)", "Project(|s)", "Trial(|s)",
    "Publication(|s)", "Report(|s)", "Program(|s)", "Paper(|s)",
    "Manuscript(|s)", "Analys(is|es)", "Investigation(|s)"
  )
  synonyms[["Research_strict"]] <- c(
    "Work(|s)", "Research", "Stud(y|ies)", "Project(|s)", "Trial(|s)",
    "Program(|s)", "Analys(is|es)", "Investigation(|s)"
  )
  synonyms[["research_singular"]] <- c(
    "[Ww]ork", "[Rr]esearch", "[Ss]tudy", "[Pp]roject", "[Tt]rial",
    "[Pp]ublication", "[Rr]eport", "[Pp]rogram", "[Pp]aper",
    "[Mm]anuscript", "[Aa]nalysis", "[Ii]nvestigation"
  )
  synonyms[["researches"]] <- c(
    "[Ww]orks", "[Ss]tudies", "[Pp]rojects", "[Tt]rials", "[Pp]ublications",
    "[Rr]eports", "[Pp]rograms", "[Pp]papers", "[Mm]anuscripts",
    "[Aa]nalyses", "[Ii]nvestigations"
  )

  synonyms[["funder"]] <- c("[Ff]under", "[Ss]ponsor", "[Ss]upporter")
  synonyms[["funds"]] <- c("[Ff]und(|s)", "[Ff]unding", "[Ss]elf-funding")
  synonyms[["funded"]] <- c(
    "[Ff]unded", "[Ss]elf-funded", "[Ff]inanced", "[Ss]upported",
    "[Ss]ponsored", "[Rr]esourced", "[Aa]ided"
  )
  synonyms[["funding"]] <- c(
    "[Ff]unding", "[Ff]unds", "[Ss]elf-funding", "[Ff]und support(|s)",
    "[Ss]upport", "[Ss]ponsorship", "\\b[Aa]id", "[Rr]esources"
  )
  synonyms[["funded_funding"]] <- c(synonyms[["funded"]], synonyms[["funding"]])
  synonyms[["funding_financial"]] <- c(synonyms[["funding"]], "[Ff]inancial")

  synonyms[["funding_title"]] <- c(
    "F(?i)unding(?-i)", "F(?i)unding/Support(?-i)", "F(?i)unding source(|s)(?-i)",
    "S(?i)ource(|s) of funding(?-i)", "F(?i)unding source(|s) for the stud(y|ies)(?-i)",
    "F(?i)unding information(?-i)", "F(?i)unding statement(|s)(?-i)",
    "S(?i)upport statement(|s)(?-i)", "S(?i)ource(|s) of support(|s)(?-i)",
    "S(?i)ource(|s) of funding(?-i)",
    "F(?i)unding and (|potential )(conflict(|s)|competing) (|of )interest(|s)(?-i)"
  )

  synonyms[["financial"]] <- c(
    "[Ff]inancial support(|s)", "[Ff]inancial source(|s)",
    "[Ff]inancial or other support(|s)", "[Ff]inancial assistance",
    "[Ff]inancial aid(|s)", "[Ff]inancial sponsorship(|s)",
    "[Ff]inancial support(|s) and sponsorship(|s)",
    "[Gg]rant support(|s)", "[Gg]rant assistance", "[Gg]rant aid(|s)",
    "[Gg]rant sponsorship(|s)"
  )

  synonyms[["financial_title"]] <- c(
    "F(?i)inancial support(|s)(?-i)", "F(?i)inancial source(|s)(?-i)",
    "S(?i)ource(|s) of financial support(|s)(?-i)", "F(?i)inancial or other support(|s)(?-i)",
    "F(?i)inancial assistance(?-i)", "F(?i)inancial aid(?-i)",
    "F(?i)inancial sponsorship(|s)(?-i)",
    "F(?i)inancial support(|s) and sponsorship(|s)(?-i)",
    "F(?i)inancial source(|s) for the stud(y|ies)(?-i)",
    "F(?i)inancial information(?-i)", "F(?i)inancial statement(|s)(?-i)",
    "F(?i)inanciamento(?-i)"
  )

  synonyms[["any_title"]] <- c(
    synonyms[["funding_title"]], synonyms[["financial_title"]],
    "G(?i)rant(|s)(?-i)", "G(?i)rant sponsor(|s|ship(|s))(?-i)",
    "G(?i)rant support(|s)(?-i)", "G(?i)rant assistance(?-i)", "G(?i)rant aid(|s)(?-i)"
  )

  synonyms[["disclosure_title"]] <- c(
    "F(?i)unding disclosure(|s)(?-i)", "F(?i)inancial disclosure(|s)(?-i)",
    "F(?i)inancial declaration(|s)(?-i)",
    "F(?i)inancial (&|and) competing interests disclosure(|s)(?-i)",
    "F(?i)inancial (&|and) competing interests declaration(|s)(?-i)",
    "D(?i)isclosure(|s)(?-i)", "D(?i)eclaration(|s)(?-i)"
  )

  synonyms[["support"]] <- c("[Ss]upport(|s)", "\\b[Aa]id(|s)", "[Aa]ssistance", "[Ss]ponsorship(|s)")
  synonyms[["support_only"]] <- c("[Ss]upport(|s)")
  synonyms[["Supported"]] <- c("Supported")

  synonyms[["award"]] <- c(
    "[Gg]rant(|s)", "(?<![Ss]pecialty) [Ff]ellowship(|s)", "[Aa]ward(|s|ing)",
    "[Ss]cholar(|s|ship|ships)", "[Ee]ndowment(|s)", "[Ss]tipend(|s)", "[Bb]ursar(y|ies)"
  )

  synonyms[["grant_title"]] <- c(
    "G(?i)rant(|s)(?-i)", "G(?i)rant sponsor(|s|ship(|s))(?-i)",
    "G(?i)rant support(|s)(?-i)", "G(?i)rant assistance(?-i)", "G(?i)rant aid(|s)(?-i)",
    "^[A-Z]\\w+ grant sponsor(|s|ship(|s))(?-i)"
  )

  synonyms[["funds_award_financial"]] <- c(synonyms[["funds"]], synonyms[["financial"]], synonyms[["award"]])
  synonyms[["funding_financial_award"]] <- c(synonyms[["funding"]], synonyms[["financial"]], synonyms[["award"]])

  synonyms[["receive"]] <- c("received", "own(|s)", "hold(|s)", "ha(s|ve)", "charged", "invent(ed|or|ors)", "declare")
  synonyms[["received"]] <- c(
    "[Rr]eceived", "[Aa]ccepted", "[Aa]cquired", "[Pp]rovided", "[Gg]ranted",
    "[Aa]warded", "[Gg]iven", "[Oo]ffered", "[Aa]llotted", "[Dd]isclosed",
    "[Dd]eclared", "[Ss]upplied", "[Pp]resented"
  )
  synonyms[["received_strict"]] <- c("[Rr]eceived", "[Aa]ccepted", "[Aa]cquired", "[Gg]iven", "[Oo]ffered", "[Dd]isclose(|d)", "[Dd]eclare(|d)")

  synonyms[["recipient"]] <- c("[Rr]ecipient(|s)", "[Aa]wardee(|s)", "[Gg]rantee(|s)")

  synonyms[["provide"]] <- c("provid(ed|ing)", "g(ave|iving)", "award(ed|ing)")

  synonyms[["thank"]] <- c("[Tt]hank(|ful)", "[Aa]cknowledge", "[Dd]isclose", "[Gg]rateful")

  synonyms[["info"]] <- c("info(|rmation)\\b", "detail(|s)", "particulars", "data\\b", "material\\b")

  synonyms[["acknowledge"]] <- c("acknowledge", "recognize", "disclose", "declare", "report", "appreciate")
  synonyms[["acknowledged"]] <- c("acknowledged", "recognized", "disclosed", "declared", "reported", "appreciated")

  synonyms[["foundation"]] <- c(
    "Ffoundation(|s)", "Institut(e|es|ution)", "Universit", "Universit(y|ies)",
    "Academ(y|ies)", "Ministr(y|ies)", "[Gg]overnment(|s)", "Council(|s)",
    "National", "NIH", "NSF", "HHMI", "Trust(|s)", "Association(|s)",
    "Societ(y|ies)", "College(|s)", "Commission(|s)", "Center(|s)",
    "[Oo]ffice(|s)", "[Pp]rogram(|s)", "[Aa]lliance(|s)", "[Aa]gency"
  )
  synonyms[["foundation_award"]] <- c(synonyms[["foundation"]], synonyms[["award"]])

  synonyms[["References"]] <- c(
    "R(?i)eferences(?-i)", "L(?i)terature(?-i)", "L(?i)iterature Cited(?-i)",
    "N(?i)otes and References(?-i)", "W(?i)orks Cited(?-i)", "^C(?i)itations(?-i)",
    "B(?i)ibliograpy(?-i)", "B(?i)ibliographic references(?-i)", "R(?i)eferences and recommended reading(?-i)"
  )

  synonyms[["Methods"]] <- c(
    ".{0,4}M(?i)ethod(|s)(?-i)", ".{0,4}O(?i)nline method(|s)(?-i)",
    ".{0,4}M(?i)aterial(|s) and Method(|s)(?-i)", ".{0,4}M(?i)aterial(|s)/Method(|s)(?-i)",
    ".{0,4}M(?i)ethod(|s) and Material(|s)(?-i)", ".{0,4}M(?i)ethod(|s)/Material(|s)(?-i)",
    ".{0,4}S(?i)ubjects and method(|s)(?-i)", ".{0,4}P(?i)atients and method(|s)(?-i)",
    ".{0,4}P(?i)articipants and method(|s)(?-i)", ".{0,4}M(?i)ethods and preliminary analysis(?-i)",
    ".{0,4}E(?i)xperimental section(?-i)", ".{0,4}M(?i)ethodology(?-i)",
    ".{0,4}M(?i)etodologia(?-i)", ".{0,4}M(?i)etodologie(?-i)"
  )
  synonyms[["Abstract"]] <- c("Abstract", "Synopsis", "Summary")
  synonyms[["Introduction"]] <- c("Introduction", "Background")
  synonyms[["Results"]] <- c("Results", "Findings")
  synonyms[["Conclusion"]] <- c("Conclusion", "Interpretation")

  synonyms[["sources"]] <- c("source(|s)")
  synonyms[["register"]] <- c("register")
  synonyms[["registered"]] <- c("registered")
  synonyms[["registration"]] <- c("registration")
  synonyms[["registry"]] <- c("[Rr]egistr(y|ies)")
  synonyms[["registered_registration"]] <- c(synonyms[["registered"]], synonyms[["registration"]])

  synonyms[["conflict_title"]] <- c(
    "C(?i)onflict(|s) of interest(|s)(?-i)",
    "C(?i)onflict(|s) of interest(|s) declaration(?-i)",
    "C(?i)onflicting interest(|s)(?-i)",
    "C(?i)onflicting interest(|s) declaration(?-i)",
    "C(?i)onflicting financial intere",
    "C(?i)onflicting of interest(|s)(?-i)",
    "C(?i)onflits d'int(?-i)",
    "C(?i)onflictos de Inter(?-i)",
    "C(?i)ompeting interest(|s)(?-i)",
    "C(?i)ompeting interest(|s) declaration(?-i)",
    "C(?i)ompeting of interest(|s)(?-i)",
    "C(?i)ompeting financial interest(|s)(?-i)",
    "D(?i)eclaration of interest(|s)(?-i)",
    "D(?i)eclaration of conflicting interest(|s)(?-i)",
    "D(?i)uality of interest(|s)(?-i)",
    "S(?i)ource(|s) of bias(?-i)",
    "F(?i)unding and (|\\w+ )(conflict(|s)|competing) (|of )interest(|s)(?-i)"
  )

  synonyms[["conflict_title_strict"]] <- c(
    "(|\\w+ )C(?i)onflict(|s) of interest(|s)(?-i)(| \\(?COI\\)?| \\w+)",
    "(|\\w+ )C(?i)onflicting interest(|s)(?-i)(| \\(?COI\\)?| \\w+)",
    "(|\\w+ )C(?i)onflicting financial interest(|s)(?-i)(| \\w+)",
    "(|\\w+ )C(?i)onflicting of interest(|s)(?-i)(| \\(?COI\\)?| \\w+)",
    "(|\\w+ )C(?i)onflits d'int(?-i)",
    "(|\\w+ )C(?i)onflictos de Inter(?-i)",
    "(|\\w+ )C(?i)ompeting interest(|s)(?-i)(| \\w+)",
    "(|\\w+ )C(?i)ompeting of interest(|s)(?-i)(| \\(?COI\\)?| \\w+)",
    "(|\\w+ )C(?i)ompeting financial interest(|s)(?-i)(| \\w+)",
    "D(?i)eclaration of interest(|s)(?-i)(| \\w+)",
    "D(?i)eclaration of conflicting interest(|s)(?-i)",
    "D(?i)uality of interest(|s)(?-i)(| \\w+)",
    "(|\\w+ )S(?i)ource(|s) of bias(?-i)(| \\w+)",
    "F(?i)unding and (|\\w+ )(conflict(|s)|competing) (|of )interest(|s)(?-i)"
  )

  synonyms[["conflict"]] <- c("[Cc]onflict(|ing)(|s)", "[Cc]ompet(e|ing)", "source(|s) of bias", "[Cc]onflits", "[Cc]onflictos")
  synonyms[["disclose"]] <- c("disclose(|s)", "declare(|s)", "state(|s)", "disclaim(|s)", "report(|s)", "acknowledge(|s)")
  synonyms[["disclosure"]] <- c("disclosure", "declaration", "statement", "disclaimer", "acknowledgement", "reporting")

  synonyms[["disclosure_coi_title"]] <- c(
    "F(?i)inancial disclosure(|s)(?-i)", "F(?i)inancial declaration(|s)(?-i)",
    "F(?i)inancial (&|and) competing interests disclosure(|s)(?-i)",
    "F(?i)inancial (&|and) competing interests declaration(|s)(?-i)",
    "D(?i)isclosure(|s)(?-i)", "D(?i)eclaration(|s)(?-i)"
  )

  synonyms[["no_financial_disclosure"]] <- c(
    "[Nn]othing to (disclose|declare|report|acknowledge)",
    "[Nn]o financial disclosure(|s)",
    "[Nn]o [a-z]+ financial disclosure(|s)",
    "[Nn]o [a-z]+ [a-z]+ financial disclosure(|s)"
  )

  synonyms[["commercial"]] <- c("commercial(|ly)", "financial(|y)")
  synonyms[["commercial_strict"]] <- c("commercial(|ly)")

  synonyms[["relationship"]] <- c("relation(|s|ship(|s))", "connection(|s)", "association(|s)", "involvement(|s)", "tie(|s)", "contract(|s)")
  synonyms[["relationship_strict"]] <- c("relation(|s|ship(|s))", "connection(|s)", "association(|s)", "involvement(|s)", "tie(|s)")
  synonyms[["related_adjectives"]] <- c("related", "connected", "associated", "involved", "tied")

  synonyms[["interests"]] <- c("gain(|s)", "benefit(|s)", "interest(|s)")
  synonyms[["stock"]] <- c("stock(|s)", "shares", "bonds")
  synonyms[["fees"]] <- c("fe(e|es)", "compensation", "payment", "honorari(um|a)", "sponsorship")
  synonyms[["consultant"]] <- c("consultant(|s)", "advis(or(|s)|er(|s))", "board member(|s)", "member(|s) of the board")
  synonyms[["consult"]] <- c("consult(|s|ing)", "advise(|s)", "counsel(|s)")
  synonyms[["consult_all"]] <- c("consult(|s|ant(|s))", "advis(e(|s)|or(|s)|er(|s))", "counsel(|s)")
  synonyms[["speaker"]] <- c("speaker", "presenter", "lectur(e|es|ing)", "employee(|s)")
  synonyms[["proprietary"]] <- c("proprietary", "patent(|s)", "copyright(|s)", "license(|s)", "rights", "permit(|s)", "priviledge(|s)", "franchise(|s)")
  synonyms[["founder"]] <- c("founder", "co(|-)founder", "founding member")
  synonyms[["played"]] <- c("played", "had")
  synonyms[["role"]] <- c("role", "hand", "part", "involvement")

  .syn_cache$syn <- synonyms
  return(synonyms)
}

# Helper: encase vector into a single capturing group joined by OR
.encase <- function(x) {
  return(paste0("(", paste0(x, collapse = "|"), ")"))
}

# Helper: place word boundaries
.bound <- function(x, location = "end") {
  if (location == "both") {
    return(paste0("\\b[[:alnum:]]{0,1}", x, "\\b"))
  }
  if (location == "end") {
    return(paste0(x, "\\b"))
  }
  if (location == "start") {
    return(paste0("\\b[[:alnum:]]{0,1}", x))
  }
  stop("Unknown location in .bound")
}

# Helper: allow up to n_max words between tokens
.max_words <- function(x, n_max = 3, space_first = TRUE) {
  if (space_first) {
    return(paste0(x, "(?:\\s+\\w+){0,", n_max, "}"))
  } else {
    return(paste0(x, "(?:\\w+\\s+){0,", n_max, "}"))
  }
}

# Title helpers
.title <- function(x, within_text = FALSE) {
  if (within_text) {
    return(paste0(x, "(|:|\\.)"))
  } else {
    return(paste0("^", x, "(|:|\\.)$"))
  }
}

.title_strict <- function(x, within_text = FALSE) {
  if (within_text) {
    return(paste0(x, "( [A-Z][a-zA-Z]|:|\\.|\\s*-+)"))
  } else {
    return(paste0("^.{0,4}", x, ".{0,4}$"))
  }
}

# Case handling
.first_capital <- function(x, location = "both") {
  if (location == "both") {
    return(gsub("^([A-Z])(.*)$", "\\1(?i)\\2(?-i)", x))
  }
  if (location == "start") {
    return(gsub("^(.)(.*)$", "\\1(?i)\\2", x))
  }
  if (location == "end") {
    return(gsub("^(.*)$", "\\1(?-i)", x))
  }
  stop("Unknown location in .first_capital")
}

# -------------------------------
# Text obliteration/cleanup
# -------------------------------

# Remove full stops unlikely to be end-of-sentence
obliterate_fullstop_1 <- function(article) {
  stopifnot(is.character(article))
  patterns <- c(
    "([A-Z])(\\.)\\s*([A-Z])(\\.)\\s*([A-Z])(\\.)" = "\\1 \\3 \\5",
    "([A-Z])(\\.)\\s*([A-Z])(\\.)"                     = "\\1 \\3",
    "(\\s[A-Z])(\\.) ([A-Z][a-z]+)"                      = "\\1 \\3",
    "\\.\\s*([a-z0-9])"                                   = " \\1",
    "\\.([A-Z])"                                          = " \\1",
    "\\.\\s*([A-Z]+[0-9])"                               = " \\1",
    "\\.([^\\s0-9\\[])"                                 = "\\1",
    "\\.\\s+(\\()"                                      = " \\1",
    "([0-9])\\.([0-9])"                                   = "\\1\\2",
    "\\.(\\s*[[:punct:]])"                              = "\\1"
  )
  return(stringr::str_replace_all(article, patterns))
}

.obliterate_semicolon_1 <- function(article) {
  stopifnot(is.character(article))
  return(stringr::str_replace_all(article, "(\\(.*); (.*\\))", "\\1 - \\2"))
}

.obliterate_comma_1 <- function(article) {
  stopifnot(is.character(article))
  return(gsub(", ", " ", article, fixed = TRUE))
}

.obliterate_apostrophe_1 <- function(article) {
  stopifnot(is.character(article))
  x <- stringr::str_replace_all(article, "([a-zA-Z])'([a-zA-Z])", "\\1\\2")
  x <- stringr::str_replace_all(x, "[a-z]+s'", "s")
  return(x)
}

.obliterate_hash_1 <- function(article) {
  stopifnot(is.character(article))
  return(gsub("#", "", article, fixed = TRUE))
}

.obliterate_punct_1 <- function(article) {
  stopifnot(is.character(article))
  punct <- '[~@#$%^&*{}_+"<>?/=]'
  return(stringr::str_replace_all(article, punct, ""))
}

.obliterate_line_break_1 <- function(article) {
  stopifnot(is.character(article))
  return(gsub("\n", " ", article, fixed = TRUE))
}

.obliterate_refs_1 <- function(article) {
  stopifnot(is.character(article))
  article <- gsub("^.*\\([0-9]{4}\\).*$", "References", article)
  article <- gsub("^.* et al\\..*$", "References", article)
  return(article)
}

# Remove mentions of COIs that may cause false positives
obliterate_conflict_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  financial_1 <- "(funding|financial|support)"
  financial_2 <- "(financial|support)"
  relationship <- .encase(.bound(synonyms$relationship))
  conflict <- .encase(.bound(synonyms$conflict))
  financial_interest <- "(financial(?:\\s+\\w+){0,3} interest)"

  regex_1 <- paste(financial_1, relationship, conflict, sep = synonyms$txt)
  regex_2 <- paste(conflict, relationship, financial_2, sep = synonyms$txt)
  regex_3 <- paste(relationship, financial_interest, sep = synonyms$txt)

  pattern <- paste(.encase(c(regex_1, regex_2, regex_3)), collapse = "|")
  return(gsub(pattern, "", article, perl = TRUE))
}

# Remove misleading disclosure sentences (e.g., Disclosure ... not funded ...)
obliterate_disclosure_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("conflict", "and", "not", "funded")
  parts <- .encase(.bound(unlist(synonyms[words])))
  pattern <- paste0(synonyms$txt, parts, synonyms$txt, "($|.)")
  return(gsub(pattern, "", article, perl = TRUE))
}

# -------------------------------
# Locators (references, acknowledgements, methods)
# -------------------------------

.where_refs_txt <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("References")
  next_sentence <- "((|:|\\.)|(|:|\\.) [A-Z0-9]+.*)$"

  pats <- lapply(synonyms[words], function(x) paste0(x, next_sentence))
  pats <- lapply(pats, .encase)
  pattern <- paste(unlist(pats), collapse = "|")
  ref_index <- grep(pattern, article, perl = TRUE)

  if (length(ref_index) > 0) {
    ref_index <- ref_index[length(ref_index)]
  } else {
    ref_index <- grep("^1(|\\.)\\s+[A-Z]", article)
    if (length(ref_index) > 0) ref_index <- ref_index[length(ref_index)]
  }
  return(ref_index)
}

.where_acknows_txt <- function(article) {
  stopifnot(is.character(article))
  acknow_index <- get_acknow_2(article)
  fund_index <- get_fund_2(article)
  finance_index <- get_financial_1(article)
  grant_index <- get_grant_1(article)

  if (length(acknow_index) > 0) acknow_index <- acknow_index[length(acknow_index)]
  if (length(fund_index) > 0) fund_index <- fund_index[1]
  if (length(finance_index) > 0) finance_index <- finance_index[1]
  if (length(grant_index) > 0) grant_index <- grant_index[1]

  all <- c(acknow_index, fund_index, finance_index, grant_index)
  from <- integer()
  if (length(all) > 0) {
    all_max <- max(all)
    all_min <- min(all)
    if ((all_max - all_min) <= 10) {
      from <- all_min
    } else {
      from <- all_max
    }
  }
  return(from)
}

.where_methods_txt <- function(article) {
  stopifnot(is.character(article))
  method_index <- integer()
  synonyms <- .create_synonyms()
  words <- c("Methods", "Abstract", "Results", "Conclusion")

  pats <- lapply(synonyms[words[1]], .title_strict)
  pats <- lapply(pats, function(s) stringr::str_sub(s, end = -2))
  pats <- lapply(pats, .encase)
  pattern <- paste(unlist(pats), collapse = "|")
  idx <- grep(pattern, article, perl = TRUE)
  if (length(idx) > 0) {
    method_index <- idx[length(idx)]
    return(method_index)
  }

  pats2 <- lapply(synonyms[words[1]], .title_strict, within_text = TRUE)
  pats2 <- lapply(pats2, function(s) paste(s, "[A-Z]", sep = "\\s*"))
  pats2 <- lapply(pats2, .encase)
  pattern2 <- paste(unlist(pats2), collapse = "|")
  idx <- grep(pattern2, article, perl = TRUE)
  if (length(idx) > 0) {
    method_index <- idx[length(idx)]
  }
  return(method_index)
}

# -------------------------------
# Patterned builders
# -------------------------------

get_support_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("This_singular", "research_singular", "is_singular", "funded_funding", "by")

  parts1 <- lapply(synonyms[words[1:2]], .bound)
  parts1 <- lapply(parts1, .encase)
  this_research <- paste(unlist(parts1), collapse = ".{0,15}")

  parts2 <- lapply(synonyms[words[3:5]], .bound)
  parts2 <- lapply(parts2, .encase)
  was_funded_by <- paste(unlist(parts2), collapse = synonyms$txt)

  singular <- paste(this_research, was_funded_by, sep = synonyms$txt)
  singular_idx <- grep(singular, article, perl = TRUE)
  if (length(singular_idx) > 0) return(singular_idx)

  words <- c("These", "researches", "are", "funded_funding", "by")
  parts <- lapply(synonyms[words], .bound, location = "end")
  parts <- lapply(parts, .encase)
  pattern <- paste(unlist(parts), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_support_2 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("funded_funding", "this_singular", "research_singular")
  parts <- lapply(synonyms[words], .bound, location = "end")
  parts <- lapply(parts, .encase)
  pattern <- paste(unlist(parts), collapse = synonyms$txt)
  idx <- grep(pattern, article, perl = TRUE)
  if (length(idx) > 0) return(idx)

  words <- c("funded", "these", "researches")
  parts <- lapply(synonyms[words], .bound, location = "end")
  parts <- lapply(parts, .encase)
  pattern <- paste(unlist(parts), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_support_3 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("research", "is_have", "funded", "by")

  parts1 <- lapply(synonyms[words[1:2]], .bound, location = "end")
  parts1 <- lapply(parts1, .encase)
  research_is <- paste(unlist(parts1), collapse = " ")

  parts2 <- lapply(synonyms[words[3:4]], .bound, location = "end")
  parts2 <- lapply(parts2, .encase)
  pattern <- paste(unlist(parts2), collapse = synonyms$txt)
  pattern <- paste(research_is, pattern, sep = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_support_4 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("funded", "by", "award")
  parts <- lapply(synonyms[words], .bound)
  parts <- lapply(parts, .encase)
  pattern <- paste(unlist(parts), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_support_5 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("funded", "by", "foundation")
  parts <- lapply(synonyms[words], .bound)
  parts <- lapply(parts, .encase)
  funded_by_award <- paste(unlist(parts), collapse = synonyms$txt)
  start_of_sentence <- "(^\\s*|(:|\\.)\\s*|[A-Z][a-zA-Z]+\\s*)"
  pattern <- paste0(.max_words(start_of_sentence, n_max = 4, space_first = FALSE), funded_by_award)
  return(grep(pattern, article, perl = TRUE))
}

get_support_6 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words_1 <- c("acknowledge", "support_only", "foundation_award")
  words_2 <- c("support_only", "foundation_award", "acknowledged")

  acknowledge <- lapply(synonyms[words_1[1]], .bound)
  acknowledge <- lapply(acknowledge, .encase)
  acknowledge <- lapply(acknowledge, .max_words)

  support_foundation <- lapply(synonyms[words_1[2:3]], .bound)
  support_foundation <- lapply(support_foundation, .encase)
  support_foundation <- paste(unlist(support_foundation), collapse = synonyms$txt)

  a <- paste(c(acknowledge, support_foundation), collapse = " ")
  a_idx <- grep(a, article, perl = TRUE)

  parts <- lapply(synonyms[words_2], .bound)
  parts <- lapply(parts, .encase)
  b <- paste(unlist(parts), collapse = synonyms$txt)
  b_idx <- grep(b, article, perl = TRUE)

  return(unique(c(a_idx, b_idx)))
}

get_support_7 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  a <- "Support"
  b <- .encase(synonyms$received)
  d <- .encase(synonyms$by)
  pattern <- paste(a, b, d, sep = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_support_8 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("foundation", "provide", "funding_financial", "for_of", "research")
  parts <- lapply(synonyms[words], .bound)
  parts <- lapply(parts, .encase)
  pattern <- paste(unlist(parts), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_support_9 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("foundation", "funded", "research")

  foundation <- lapply(synonyms[words[1]], .bound)
  foundation <- lapply(foundation, .encase)

  funding <- lapply(synonyms[words[2]], function(v) grep("upport", v, value = TRUE, invert = TRUE))
  funding <- lapply(funding, .bound)
  funding <- lapply(funding, .encase)

  research <- lapply(synonyms[words[3]], .bound)
  research <- lapply(research, .encase)
  research <- paste0(unlist(research), ".{0,20}\\.")

  pattern <- paste(unlist(c(foundation, funding, research)), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_support_10 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("thank", "foundation", "funding_financial", "research")
  parts <- lapply(synonyms[words], .bound)
  parts <- lapply(parts, .encase)
  pattern <- paste(unlist(parts), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_developed_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("This", "research", "is", "developed", "by", "foundation")

  p1 <- lapply(synonyms[words[1:3]], .bound)
  p1 <- lapply(p1, .encase)
  this_research_is <- paste(unlist(p1), collapse = synonyms$txt)

  developed <- words[4]

  p2 <- lapply(synonyms[words[5:6]], .bound)
  p2 <- lapply(p2, .encase)
  by_foundation <- paste(unlist(p2), collapse = synonyms$txt)

  pattern <- paste(this_research_is, developed, by_foundation, sep = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_received_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("received", "funds_award_financial", "by")
  parts <- lapply(synonyms[words], .bound)
  parts <- lapply(parts, .encase)
  pattern <- paste(unlist(parts), collapse = " ")
  return(grep(pattern, article, perl = TRUE))
}

get_received_2 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("received", "funding_financial", "by", "foundation_award")
  parts <- lapply(synonyms[words], .bound)
  parts <- lapply(parts, .encase)
  pattern <- paste(unlist(parts), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_recipient_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("recipient", "award")
  parts <- lapply(synonyms[words], .bound)
  parts <- lapply(parts, .encase)
  pattern <- paste(unlist(parts), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_authors_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("This", "author", "funds_award_financial")
  parts <- lapply(synonyms[words], .bound)
  parts <- lapply(parts, .encase)
  pattern <- paste(unlist(parts), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_authors_2 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("This", "author", "have", "no", "funding_financial_award")
  parts <- lapply(synonyms[words], .bound)
  parts <- lapply(parts, .encase)
  pattern <- paste(unlist(parts), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_thank_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  synonyms$financial <- c(synonyms$financial, "for supporting")
  words <- c("We", "thank", "financial")
  parts <- lapply(synonyms[words], .bound)
  parts <- lapply(parts, .encase)
  pattern <- paste(unlist(parts), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_thank_2 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("thank")
  thank <- lapply(synonyms[words], .bound)
  thank <- lapply(thank, .encase)
  thank <- paste(unlist(thank), collapse = synonyms$txt)
  txt <- "[a-zA-Z0-9\\s,()/:;-]*"
  pattern <- paste(thank, "[0-9]{5}", sep = txt)
  return(grep(pattern, article, perl = TRUE))
}

get_fund_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("funding_financial_award", "for", "research", "received")

  p1 <- lapply(synonyms[words[1:2]], .bound)
  p1 <- lapply(p1, .encase)
  funding_for <- paste(unlist(p1), collapse = " ")

  p2 <- lapply(synonyms[words[3:4]], .bound)
  p2 <- lapply(p2, .encase)
  pattern <- paste(unlist(p2), collapse = synonyms$txt)
  pattern <- paste(funding_for, pattern, sep = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_fund_2 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("funding_title")

  p1 <- lapply(synonyms[words], .title)
  p1 <- lapply(p1, .encase)
  pattern <- paste(unlist(p1), collapse = "|")
  a <- grep(pattern, article, perl = TRUE)

  if (length(a) > 0) {
    if (!is.na(article[a + 1])) {
      if (nchar(article[a + 1]) == 0) {
        return(c(a, a + 2))
      } else {
        return(c(a, a + 1))
      }
    } else {
      return(a)
    }
  } else {
    p2 <- lapply(synonyms[words], .title, within_text = TRUE)
    p2 <- lapply(p2, .encase)
    pattern2 <- paste(unlist(p2), collapse = "|")
    return(grep(pattern2, article, perl = TRUE))
  }
}

get_fund_3 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("any_title")
  pats <- lapply(synonyms[words], .encase)
  pattern <- paste(unlist(pats), collapse = "|")
  pattern <- paste(pattern, "[A-Z]")
  return(grep(pattern, article, perl = TRUE))
}

get_fund_acknow <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("funded", "funds_award_financial")
  funded_synonyms <- lapply(synonyms[words], .bound)
  funded_synonyms <- unlist(funded_synonyms)
  pats <- c(funded_synonyms, "NIH (|\\()(?:(R|P))[0-9]{2}", "awarded by")
  pattern <- .encase(pats)
  return(grep(pattern, article, perl = TRUE, ignore.case = TRUE))
}

get_fund_acknow_new <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("acknowledge", "support_only", "grant|foundation|institute|organization")
  pats <- lapply(synonyms[words], .bound)
  pats <- unlist(pats)
  pattern <- .encase(pats)
  return(grep(pattern, article, perl = TRUE, ignore.case = TRUE))
}

get_supported_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("Supported", "by")
  pats <- lapply(synonyms[words], .bound)
  pats <- lapply(pats, .encase)
  pattern <- paste(unlist(pats), collapse = " ")
  pattern <- paste(pattern, "[a-zA-Z]+")
  return(grep(pattern, article, perl = TRUE))
}

get_financial_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("financial_title")

  p1 <- lapply(synonyms[words], .title)
  p1 <- lapply(p1, .encase)
  pattern <- paste(unlist(p1), collapse = "|")
  a <- grep(pattern, article, perl = TRUE)

  if (length(a) > 0) {
    if (!is.na(article[a + 1])) {
      if (nchar(article[a + 1]) == 0) {
        return(c(a, a + 2))
      } else {
        return(c(a, a + 1))
      }
    } else {
      return(a)
    }
  } else {
    p2 <- lapply(synonyms[words], .title, within_text = TRUE)
    p2 <- lapply(p2, .encase)
    pattern2 <- paste(unlist(p2), collapse = "|")
    return(grep(pattern2, article, perl = TRUE))
  }
}

get_financial_2 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("financial_title", "No")
  pats <- lapply(synonyms[words], .bound)
  pats <- lapply(pats, .encase)
  pattern <- paste(unlist(pats), collapse = " ")
  return(grep(pattern, article, perl = TRUE))
}

get_financial_3 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("financial_title", "this", "research")
  pats <- lapply(synonyms[words], .bound)
  pats <- lapply(pats, .encase)
  pattern <- paste(unlist(pats), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_disclosure_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("disclosure_title")
  p1 <- lapply(synonyms[words], .title)
  p1 <- lapply(p1, .encase)
  pattern <- paste(unlist(p1), collapse = "|")
  a <- grep(pattern, article, perl = TRUE)

  out <- integer()
  if (length(a) > 0) {
    for (i in seq_along(a)) {
      next_line <- a[i] + 1
      if (!is.na(article[next_line]) && nchar(article[next_line]) == 0) next_line <- a[i] + 2
      d <- lapply(.create_synonyms()$funding_financial_award, .bound)
      d <- unlist(d)
      d_pat <- paste(d, collapse = "|")
      hit <- grep(d_pat, article[next_line], perl = TRUE)
      if (length(hit) > 0) out <- c(out, a[i], next_line)
    }
  }
  return(out)
}

get_disclosure_2 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("disclosure_title", "funding_financial_award")
  disclosure <- lapply(synonyms[words[1]], .title, within_text = TRUE)
  funding <- lapply(synonyms[words[2]], .bound)
  pats <- lapply(c(disclosure, funding), .encase)
  pattern <- paste(unlist(pats), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_grant_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("grant_title")
  p1 <- lapply(synonyms[words], .title)
  p1 <- lapply(p1, .encase)
  pattern <- paste(unlist(p1), collapse = "|")
  a <- grep(pattern, article, perl = TRUE)

  if (length(a) > 0) {
    if (!is.na(article[a + 1])) {
      if (nchar(article[a + 1]) == 0) {
        return(c(a, a + 2))
      } else {
        return(c(a, a + 1))
      }
    } else {
      return(a)
    }
  } else {
    grant <- c("G(?i)rant ", "^[A-Z](?i)\\w+ grant ", "Contract grant ")

    support <- c(unlist(lapply(synonyms[c("support", "funder")], function(v) .title(v, within_text = TRUE))))
    combos <- unlist(lapply(grant, function(g) paste0(g, support)))
    pattern2 <- .encase(combos)
    return(grep(pattern2, article, perl = TRUE))
  }
}

get_french_1 <- function(article) {
  stopifnot(is.character(article))
  return(grep("Cette.*tude.*financ.*par", article, perl = TRUE, ignore.case = TRUE))
}

get_project_acknow <- function(article) {
  stopifnot(is.character(article))
  return(grep("project (no|num)", article, perl = TRUE, ignore.case = TRUE))
}

get_common_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("no", "funding_financial_award", "is", "received")

  p1 <- lapply(synonyms[words[1:2]], .bound)
  p1 <- lapply(p1, .encase)
  no_funding <- paste(unlist(p1), collapse = " ")

  p2 <- lapply(synonyms[words[3:4]], .bound)
  p2 <- lapply(p2, .encase)
  was_received <- paste(unlist(p2), collapse = " ")

  pattern <- paste(no_funding, was_received, sep = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_common_2 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("No", "funding_financial_award", "received")
  pats <- lapply(synonyms[words], .bound)
  pats <- lapply(pats, .encase)
  pattern <- paste(unlist(pats), collapse = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_common_3 <- function(article) {
  stopifnot(is.character(article))
  return(grep("required to disclose.*disclosed none", article))
}

get_common_4 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("no", "funding_financial_award", "for", "this", "research")

  p1 <- lapply(synonyms[words[1:2]], .bound)
  p1 <- lapply(p1, .encase)
  no_funding <- paste(unlist(p1), collapse = synonyms$txt)

  p2 <- lapply(synonyms[words[3:4]], .bound)
  p2 <- lapply(p2, .encase)
  for_this <- paste(unlist(p2), collapse = " ")

  p3 <- lapply(synonyms[words[5]], .bound)
  p3 <- lapply(p3, .encase)
  research <- paste(unlist(p3), collapse = "|")

  pattern <- paste(no_funding, for_this, research, sep = synonyms$txt)
  return(grep(pattern, article, perl = TRUE))
}

get_common_5 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("no", "sources", "funding_financial")
  pats <- lapply(synonyms[words], .bound)
  pats <- lapply(pats, .encase)
  pattern <- paste(unlist(pats), collapse = .max_words(" ", space_first = FALSE))
  return(grep(pattern, article, perl = TRUE))
}

# -------------------------------
# Negation/predicate helpers
# -------------------------------

negate_disclosure_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()

  txt <- "[a-zA-Z0-9\\s,()-]*"
  disclose_synonyms <- c("[Dd]isclose(|s)(|:|\\.)", "[Dd]isclosure(|s)(|:|\\.)")
  conflict_synonyms <- c("conflict(|s) of interest", "conflicting interest", "conflicting financial interest", "conflicting of interest", "conflits d'int", "conflictos de Inter")
  compete_synonyms <- c("competing interest", "competing of interest", "competing financial interest")
  and_synonyms <- c("and", "&", "or")
  not_synonyms <- c("not")
  funded_synonyms <- c("\\bfunded", "\\bfinanced", "\\bsupported", "\\bsponsored", "\\bresourced")

  disclose <- .encase(disclose_synonyms)
  conflict <- .encase(c(conflict_synonyms, compete_synonyms))
  and <- .encase(and_synonyms)
  not <- .encase(not_synonyms)
  funded <- .encase(funded_synonyms)

  regex <- paste(disclose, conflict, and, not, funded, sep = txt)
  a <- grepl(regex, article, perl = TRUE)
  if (any(a)) {
    return(a)
  } else {
    funded <- .encase(c(funded_synonyms, synonyms$funding))
    regex <- paste(disclose, funded, conflict, sep = txt)
    return(grepl(regex, article, perl = TRUE))
  }
}

negate_disclosure_2 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()

  Disclosure_synonyms <- c(
    "F(?i)inancial disclosure(|s)(?-i)(|:|\\.)",
    "F(?i)inancial declaration(|s)(?-i)(|:|\\.)",
    "Disclosure(|:|\\.)",
    "Declaration(|:|\\.)"
  )
  disclosure_synonyms <- c("financial disclosure(|s)", "financial declaration(|s)", "disclosure", "declaration")
  disclose_synonyms <- c("to disclose", "to declare", "to report")

  Disclosure <- .encase(Disclosure_synonyms)
  disclosure <- .encase(disclosure_synonyms)
  disclose   <- .encase(disclose_synonyms)
  no <- .encase(synonyms$No)
  no_1 <- "(no|not have any)"
  no_2 <- "(nil|nothing)"

  regex_1 <- .encase(paste(Disclosure, no))
  regex_2 <- .encase(paste(Disclosure, paste(no_1, disclosure), sep = synonyms$txt))
  regex_3 <- .encase(paste(Disclosure, paste(no_2, disclose),   sep = synonyms$txt))

  regex <- paste(regex_1, regex_2, regex_3, sep = "|")
  return(grepl(regex, article, perl = TRUE))
}

negate_conflict_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("conflict_title")
  pats <- lapply(synonyms[words], .title)
  pats <- lapply(pats, .encase)
  pattern <- paste(unlist(pats), collapse = "|")
  return(grepl(pattern, article, perl = TRUE))
}

negate_absence_1 <- function(article) {
  stopifnot(is.character(article))
  synonyms <- .create_synonyms()
  words <- c("No", "info", "of", "funding_financial_award", "is", "received")
  pats <- lapply(synonyms[words], .bound)
  pats <- lapply(pats, .encase)
  pattern <- paste(unlist(pats), collapse = synonyms$txt)
  return(grepl(pattern, article, perl = TRUE))
}

# -------------------------------
# Acknowledgements section
# -------------------------------

get_acknow_1 <- function(article) {
  stopifnot(is.character(article))
  txt <- "[a-zA-Z0-9\\s,()-]*"
  txt_0 <- "^Acknowledg(|e)ment(|s)"
  txt_1 <- "(of|and)"
  txt_2 <- "([Ss]upport |\\b[Ff]unding|\\b[Ff]inancial)"
  total_txt <- c(txt_0, txt_1, txt_2)
  indicator_regex <- paste0(total_txt, collapse = " ")
  a <- grep(indicator_regex, article, perl = TRUE, ignore.case = TRUE)
  if (length(a) > 0) {
    if (!is.na(article[a + 1])) {
      if (nchar(article[a + 1]) == 0) {
        return(c(a, a + 2))
      } else {
        return(c(a, a + 1))
      }
    } else {
      return(a)
    }
  } else {
    return(a)
  }
}

get_acknow_2 <- function(article) {
  stopifnot(is.character(article))
  txt_0 <- "(^A(?i)cknowledg(|e)ment(|s)(?-i))"
  txt_1 <- "(^Acknowledg(|e)ment(|s)"
  txt_2 <- "(of|and)"
  txt_3 <- "([Ss]upport |\\b[Ff]unding|\\b[Ff]inancial))"
  total_txt <- c(txt_1, txt_2, txt_3)
  indicator_regex <- paste0(total_txt, collapse = " ")
  indicator_regex <- paste(txt_0, indicator_regex, sep = "|")
  return(grep(indicator_regex, article, perl = TRUE))
}
