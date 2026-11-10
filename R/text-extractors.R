#' Extract URLs
#'
#' Get a table of URLs from a paper or paperlist. Matches urls that start with http or doi:
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table
#' @export
#'
#' @examples
#' paper <- demopaper()
#' urls <- extract_urls(paper)
extract_urls <- function(paper) {
  pattern <- "\\b((?:doi:)?(?:https?://)?(?:(?:www\\.)?(?:[\\da-z\\.-]+)\\.(?:[a-z]{2,6})|(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)|(?:(?:[0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|(?:[0-9a-fA-F]{1,4}:){1,7}:|(?:[0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|(?:[0-9a-fA-F]{1,4}:){1,5}(?::[0-9a-fA-F]{1,4}){1,2}|(?:[0-9a-fA-F]{1,4}:){1,4}(?::[0-9a-fA-F]{1,4}){1,3}|(?:[0-9a-fA-F]{1,4}:){1,3}(?::[0-9a-fA-F]{1,4}){1,4}|(?:[0-9a-fA-F]{1,4}:){1,2}(?::[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:(?:(?::[0-9a-fA-F]{1,4}){1,6})|:(?:(?::[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(?::[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(?:ffff(?::0{1,4}){0,1}:){0,1}(?:(?:25[0-5]|(?:2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(?:25[0-5]|(?:2[0-4]|1{0,1}[0-9]){0,1}[0-9])|(?:[0-9a-fA-F]{1,4}:){1,4}:(?:(?:25[0-5]|(?:2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(?:25[0-5]|(?:2[0-4]|1{0,1}[0-9]){0,1}[0-9])))(?::[0-9]{1,4}|[1-5][0-9]{4}|6[0-4][0-9]{3}|65[0-4][0-9]{2}|655[0-2][0-9]|6553[0-5])?(?:/[\\w\\.-]*)*/?)\\b"
  # simplified version without strict IPv4 validation or IPv6 support
  # these are very unlikely to be in papers
  pattern <- "\\b((doi:)?(https?://)?(([\\w.-]+\\.[a-z]{2,})|(\\d{1,3}(\\.\\d{1,3}){3}))(:\\d+)?(/[^\\s]*)?)\\b"
  # An e-mail address is matched whole and skipped, so neither its local
  # part nor its domain is listed as a URL (U206); a host name right after
  # "et " is "et al." glued to the next word ("et al.Inhibitory"; U158).
  pattern <- paste0("[\\w.+-]+@[\\w-]+(?:\\.[\\w-]+)+(*SKIP)(*FAIL)|(?<!\\bet\\s)",
                    pattern)
  table <- text_search(paper, pattern, return = "match", "perl" = TRUE)

  return(table)
}


# Eta, or the Latin letter that looks like it (U+019E), with an optional
# squared sign: before a p, it is partial eta squared written with its
# subscript after it ("η² p = .06"), which extract_p_values() and
# extract_eq() do not read as a p-value (#336)
.eta_p_prefix <- "[ηƞ]\\s*[²2₂]?\\s*"

#' Extract P-Values
#'
#' List all p-values in the text, returning the matched text (e.g., 'p = 0.04') and document location in a table.
#'
#' @details
#' Note that this will not catch p-values reported like "the p-value is 0.03" because that results in a ton of false positives when papers discuss p-value thresholds. If you need to detect text like that, use the `text_search()` function and a custom pattern.
#'
#' This will catch most comparators like =<>~ and most versions of scientific notation like 5.0 x 10^-2 or 5.0e-2. If you find any formats that are not correctly handled by this function, please contact the author.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table
#' @export
#'
#' @examples
#' paper <- demopaper()
#' p_values <- extract_p_values(paper)
extract_p_values <- function(paper) {
  # a character vector is searched as a text table, one row per string:
  # text_search() returns plain matches for one, and p$text stopped with "$
  # operator is invalid for atomic vectors" (U150)
  if (is.character(paper))
    paper <- data.frame(text = paper, text_id = seq_along(paper))

  # set up pattern
  operators <- c(
    "=", "<", ">", "~",
    "\u2248", # ~~
    "\u2260", # !=
    "\u2264", # <=
    "\u2265", # >=
    "\u226A", # <<
    "\u226B" # >>
  ) |> paste(collapse = "")

  # p may be written "ps", "p's" or "p-values", and scientific notation may
  # use the multiplication sign, leave out "^" and use the Unicode minus:
  # "ps < .05" gave no row, and "p = 1.8 \u00d7 10 -6" was read as 1.8
  # (U204, U205)
  # A p right after eta (or the Latin letter that looks like it), with or
  # without a squared sign, is partial eta squared written with its subscript
  # after it ("\u03b7\u00b2 p = .06", "\u019e\u00b2p = .95"), not a p-value: the
  # pattern takes the eta into the match, and those matches are left out
  # (#336; 2,026 sentences in 240 of 13,706 papers)
  pattern <- paste0(
    "(?:", .eta_p_prefix, ")?",
    "\\b[pP](?:-?values?|s|['\u2019]s)?\\s*", # ways to write p (or P)
    "[", operators, "]{1,2}\\s*", # 1-2 operators
    "(n\\.?s\\.?|\\d?\\.\\d+)", # ns or valid numbers
    "\\s*(e\\s*[-\u2212]\\d+)?", # also match scientific notation
    "(\\s*[x\\*\u00d7]\\s*10\\s*\\^?\\s*[-\u2212]\\s*\\d+)?"
  )

  p <- text_search(paper, pattern,
    return = "match",
    perl = TRUE, ignore.case = FALSE
  )
  p <- p[!grepl("^[\u03b7\u019e]", p$text), , drop = FALSE]

  # get operator
  pattern <- paste0("[", operators, "]{1,2}")
  matches <- gregexpr(pattern, p$text, perl = TRUE)
  p$p_comp <- regmatches(p$text, matches) |> sapply(`[[`, 1)

  # get value
  s <- strsplit(p$text, paste0("\\s*[", operators, "]{1,2}\\s*"))
  pvals <- sapply(s, \(x) x[[2]]) |>
    gsub("\\s", "", x = _) |>
    gsub("\u2212", "-", x = _) |>
    gsub("[x*\u00d7]10\\^?", "e", x = _)
  p$p_value <- suppressWarnings(as.numeric(pvals))

  return(p)
}


#' Extract Equations
#'
#' List all equations in the text, returning the matched text (e.g., 't(28) = 2.4', 'p = 0.04') and document location in a table. This is the canonical extractor for reported statistics and effect sizes; modules that need statistics should read from this table rather than re-scanning the text.
#'
#' @details
#' This will catch most comparators like =<>~and most versions of scientific notation like 5.0 x 10^-2 or 5.0e-2. If you find any formats that are not correctly handled by this function, please contact the author.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a data frame with one row per equation and the columns `lhs` (the statistic name, e.g. "t", "F", "p"), `df` (parenthetical degrees of freedom such as "(28)" or "(2, 57)", otherwise NA), `comp` (the comparator, e.g. "="), `rhs` (the reported value as text), `grp_id` (groups equations in the same sentence), `text_id`, and `paper_id`.
#' @export
#'
#' @examples
#' paper <- demopaper()
#' equations <- extract_eq(paper)
extract_eq <- function(paper) {
  paper_id <- text_id <- grp_id <- NULL # CMD-check :(
  # a character vector is searched as a text table, one row per string:
  # text_search() returns plain matches for one, which stopped bind_rows()
  # below (U150)
  if (is.character(paper))
    paper <- data.frame(text = paper, text_id = seq_along(paper))

  # set up pattern
  operators <- c(
    "=", "<", ">", "~",
    "\u2248", # ~~
    "\u2260", # !=
    "\u2264", # <=
    "\u2265", # >=
    "\u226A", # <<
    "\u226B" # >>
  )
  greek <- c(
    beta  = "\u03B2",  # regression coefficient
    eta   = "\u03B7",  # effect size (η²)
    alpha = "\u03B1",  # significance level
    delta = "\u03B4",  # small delta
    Delta = "\u0394",  # big delta
    chi   = "\u03C7",  # chi
    mu    = "\u03BC",  # population mean
    sigma = "\u03C3",  # population SD
    rho   = "\u03C1",  # population correlation
    theta = "\u03B8",  # generic parameter
    lambda= "\u03BB",  # rate (e.g., Poisson)
    gamma = "\u03B3",  # shape/scale parameter
    tau   = "\u03C4",  # precision (1/variance)
    pi    = "\u03C0"   # proportion
  )

  op <- operators |> paste(collapse = "")
  gr <- "\u0370-\u03FF" # greek |> paste(collapse = "")
  # A real df-parenthetical is built from digits/commas/periods/whitespace,
  # optionally interleaved with an "N = <number>" / "n = <number>" qualifier
  # (as in "chi2(1, N = 200)") -- in EITHER order relative to the bare
  # digits. What must NEVER appear inside is a DIFFERENT statistic's own
  # name followed by its own operator (e.g. "beta = 0.74" or "t(260) ="),
  # which is what an adjacent, unrelated clause parenthesised alongside this
  # one looks like -- confirmed as a real bug: "significant (beta = 0.74,
  # t(260) = 11.32)" matched the OLD unconstrained "(?:\\([^)]*\\))?" (any
  # text up to the first literal ")", which is t(260)'s own close-paren, not
  # the outer one) as if it were ONE statistic's df, fusing "significant",
  # "beta = 0.74" and "t(260) =" into a single garbled, unrecognisable lhs
  # and losing both real statistics entirely. Scoped narrowly (digits/N=
  # only) so the still-common "chi2(1, N = 200)" shape keeps matching as one
  # fragment exactly as before.
  df_inner <- "(?:[0-9,\\.\\s]+|[nN]\\s*=\\s*[0-9]+)*"
  # A bare "g"/"d" immediately followed by one of its own known subscript
  # words ("av", "z", "rm", "s") -- catches a subscript GROBID collapsed to
  # plain space with no "Hedges'"/"Cohen's" prefix word before it (e.g. a
  # table cell or a second mention: "g av = 0.43"). Scoped to exactly these
  # letter+word pairs, not "one extra word after any statistic name", because
  # the latter would also fuse unrelated adjacent words that happen to
  # precede a number ("sample size n = 40" -> "size n", "mean age years = 25"
  # -> "age years" -- confirmed these are what the engine already backtracks
  # to for the bare, unscoped case).
  # \\b: the g or d is a word of its own, else "and z = 3.1" was "d z" (U218)
  bare_subscript <- paste0("\\b[gd]\\s+(?:av|z|rm|s)(?=\\s*[", op, "])")
  # partial eta squared with its subscript after it, "η² p = .06",
  # is one statistic, not "p = .06" (#336)
  bare_subscript <- paste0(bare_subscript, "|", .eta_p_prefix, "[pP](?=\\s*[", op, "])")
  pattern <- paste0(
    "(?:(Hedge.{0,3}|Cronbach.{0,2}|Cohen.{0,2}|\\d{1,2}%)\\s+)?", # common prefix
    "(?:", bare_subscript, "|",
    "[", gr, "\u00B2a-zA-Z-_\\.0-9\\{\\}\\^\\\\]+", # statistic name
    # one extra short all-letter word, catching a subscript GROBID collapsed
    # to plain space ("Hedges' g av" -> "g" + "av"), ONLY when the Hedges'/
    # Cohen's prefix above actually matched -- otherwise "beta = 0.74, t(260)"
    # style adjacent clauses would start swallowing the next word as if it
    # were part of this statistic's own name (see the df_inner note above for
    # the matching concern with parenthetical df).
    "(?(1)(?:\\s+[a-zA-Z]{1,3}(?=\\s*[", op, "]))?)",
    ")",
    "\\s*", # statistic name
    "(?:\\(", df_inner, "\\))?\\s*", # optional df-shaped parentheses
    "[", op , "]{1,3}\\s*", # 1-3 operators
    # valid numbers or anything in [] or NS; a number may have the Unicode
    # minus, a leading one with a space after it ("r = \u2212 0.12"): those
    # statistics were lost (U205)
    "((?:\u2212\\s?)?[0-9\\.,+\u2212-]*[0-9]|\\[[^\\]]+\\]|n\\.?\\s*s\\.?)",
    "\\s*(e\\s*[-\u2212]\\d+)?", # also match scientific notation
    "(\\s*[x\\*\u00d7]\\s*10\\s*\\^?\\s*[-\u2212]\\s*\\d+)?"
  )

  eq <- paper |>
    text_search(operators) |>
    text_search(pattern,
                return = "match",
                perl = TRUE,
                ignore.case = TRUE)

  if (nrow(eq) == 0) {
    return(data.frame(
      text_id = integer(0),
      grp_id = integer(0),
      lhs = character(0),
      df = character(0),
      comp = character(0),
      rhs = character(0),
      paper_id = character(0)
    ))
  }

  # get df
  pattern <- "(?:\\([^)]*\\))"
  matches <- gregexpr(pattern, eq$text, perl = TRUE)
  df <- regmatches(eq$text, matches)
  eq$df <- NA_character_
  for (i in seq_along(df)) {
    if (length(df[[i]]) > 0) {
      eq$df[[i]] <- df[[i]][[1]]
      eq$text[[i]] <- eq$text[[i]] |>
        sub(df[[i]][[1]], "", x = _, fixed = TRUE) |>
        gsub("\\s+", " ", x = _)
    }
  }

  # get operator
  pattern <- paste0("[", op, "]{1,2}")
  matches <- gregexpr(pattern, eq$text, perl = TRUE)
  eq$comp <- regmatches(eq$text, matches) |> sapply(`[[`, 1)

  # get lhs & rhs
  s <- strsplit(eq$text, paste0("\\s*[", op, "]{1,2}\\s*"))
  eq$lhs <- sapply(s, \(x) x[[1]]) |> trimws()

  # the Unicode minus as "-" ("\u2212 0.12" gives "-0.12"; U205)
  eq$rhs <- sapply(s, \(x) x[[2]]) |> trimws() |>
    gsub("\u2212\\s?", "-", x = _)
  #   gsub("\\s", "", x = _) |>
  #   gsub("[x*]10\\^", "e", x = _)
  # eq$rhs <- suppressWarnings(as.numeric(rhs))

  # set group equal to sentence for now
  # identical(), not != / ==: a text table without a paper_id (or text_id)
  # has NA there, and NA != NA stopped with "missing value where TRUE/FALSE
  # needed" (U150)
  # Each sentence numbered within its own paper, in text order: counted in
  # search order, the count restarted whenever the search moved to another
  # paper, so sentences of a paper in a list shared grp_ids (U10)
  pkey <- paste(eq$paper_id)
  eq$grp_id <- NA_real_
  for (k in unique(pkey)) {
    r <- which(pkey == k)
    tid <- eq$text_id[r]
    eq$grp_id[r] <- match(tid, unique(tid[order(tid)]))
  }

  cols <- c("text_id", "grp_id", "lhs", "df", "comp", "rhs", "paper_id")
  numeric_lhs <- grepl("^[0-9]$", eq$lhs)

  eq <- eq[!numeric_lhs, cols] |>
    dplyr::arrange(paper_id, text_id, grp_id)

  return(eq)
}


#' Detect Live Data Collection
#'
#' Search a paper or paperlist for sentences indicating that data were collected
#' directly from human or animal participants. Returns a table of matching
#' sentences in the same format as [text_search()].
#'
#' Patterns cover:
#' - Participant/subject/volunteer actions ("participants were recruited", "were randomised")
#' - Researcher actions ("we recruited", "we collected data from participants")
#' - Informed consent ("gave informed consent", "signed consent", "consent was obtained")
#' - Online recruitment platforms (MTurk, Prolific, SONA, CloudResearch)
#' - Student participant pools
#' - Inclusion/exclusion criteria combined with participant language
#' - "Our participants" / "our subjects"
#' - Animal data collection ("mice were housed", "were anesthetized")
#'
#' Patterns intentionally avoid:
#' - "respondents" alone (common in economics/social science secondary survey analysis)
#' - "were tested" alone (triggers on algorithm/model evaluation in CS/AI papers)
#' - "we conducted experiments" (triggers on computational experiments in AI/CS)
#' - "data were collected from" alone (triggers on existing database/dataset studies)
#' - "our sample" alone (too generic; present in all empirical work including meta-analyses)
#'
#' Used by the `ethics_check` module to determine whether an ethics statement is expected.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a data frame of matching sentences (same structure as [text_search()])
#' @keywords internal
.detect_live_data <- function(paper) {
  live_words <- c(
    # participant/subject/volunteer actions — strong verbs indicating direct recruitment
    "(participants?|subjects?|volunteers?)\\s+(were|are|had|gave|provided|completed|filled|signed|took\\s+part|participated|recruited|paid|compensated|randomly\\s+assigned|assigned\\s+to|enrolled|debriefed)",
    # respondents only with strong recruitment verbs (not just "were asked" / "were grouped")
    "respondents?\\s+(were|are)\\s+(recruited|interviewed|enrolled|randomly\\s+assigned|compensated|paid|debriefed)",
    # researcher actively recruiting
    "we\\s+(recruited|enrolled|debriefed|ran\\s+(participants?|subjects?))",
    # researcher collecting data from live humans (require participant/subject/human qualifier)
    "we\\s+collected\\s+data\\s+from\\s+(participants?|subjects?|volunteers?|humans?|patients?|children|students?)",
    "we\\s+conducted\\s+(the\\s+)?(study|survey|interview|clinical\\s+trial)",
    "were\\s+recruited",
    "were\\s+enrolled",
    "were\\s+debriefed",
    "were\\s+randomis(ed|ing)",
    "were\\s+randomiz(ed|ing)",
    # consent
    "(gave|give|given|signed|provided)\\s+(written\\s+|oral\\s+|verbal\\s+)?informed\\s+consent",
    "signed\\s+(informed\\s+)?consent",
    "consent\\s+was\\s+obtained",
    "informed\\s+consent\\s+was\\s+(obtained|provided|given|collected)",
    # online recruitment platforms — imply live, direct recruitment
    "\\bmturk\\b",
    "mechanical\\s+turk",
    "prolific\\s+academic",
    "\\bprolific\\b",
    "cloudresearch",
    "turkprime",
    "sona\\s+systems?",
    "research\\s+participation\\s+scheme",
    "(subject|participant)\\s+pool",
    # student samples
    "(undergraduate|graduate|university|college)\\s+students?\\s+(who|were|participated|completed|filled|took\\s+part)",
    # inclusion/exclusion criteria paired with participant/patient language
    "(participant|patient|subject)\\s+(inclusion|exclusion)\\s+criteri",
    "(inclusion|exclusion)\\s+criteri.{0,60}(participants?|patients?|subjects?)",
    # "our participants" / "our subjects" (not "our sample" alone — too generic)
    "our\\s+(participants?|subjects?|volunteers?)",
    # animal data collection
    "(mice|rats?|monkeys?|pigeons?|zebrafish|ferrets?|rabbits?|hamsters?|gerbils?|guinea\\s+pigs?|macaques?|marmosets?)\\s+(were|had)\\s+(used|housed|trained|tested|implanted|injected|anesthetized|anaesthetized|sacrificed|perfused|killed)",
    "were\\s+(implanted|surgically\\s+prepared|injected|anesthetized|anaesthetized|sacrificed|perfused)"
  )

  text_search(paper, live_words)
}

#' Detect a Statement That No Participants Took Part
#'
#' Search a paper or paperlist for sentences stating that no human or animal
#' subjects took part in the study: "no human subjects were involved", "did
#' not involve human participants", "no animals nor human participants were
#' involved", or that people took part "not as subjects" (PsyArXiv dq96z_v2:
#' "The professionals described their own work as collaborators in the
#' projects, not as subjects recruited for a study."). Such a statement
#' overrides [.detect_live_data()] in the `ethics_check` module.
#'
#' A negation near a participant phrase is not enough: "No participants were
#' excluded", "only children without G6PD deficiency were enrolled" and "No
#' participants were minors" report studies with participants. In 17 local
#' collections (13,706 papers), a negation in the three words before a
#' participant phrase would have changed the verdict of 18 papers, 6 of them
#' wrongly; these patterns, which require "human" or "animal" (or "not as
#' subjects"), matched 15 statements, all of papers without participants,
#' and in the 85 PsyArXiv reports only dq96z_v2's.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a data frame of matching sentences (same structure as [text_search()])
#' @keywords internal
.detect_no_participants <- function(paper) {
  none_words <- c(
    "\\bno\\s+(identifiable\\s+)?(human|animal)\\s+(research\\s+)?(subjects?|participants?|volunteers?)\\s+(were|was|are|is)\\s+(involved|used)",
    "\\b(did|does|do)\\s+not\\s+(involve|include)\\s+(any\\s+)?(human|animal)\\s+(subjects?|participants?)",
    "\\bno\\s+(animals|humans)\\s+(nor|or|and)\\s+(human\\s+)?(participants?|subjects?)\\s+were\\s+involved",
    "\\bnot\\s+as\\s+(research\\s+)?(subjects?|participants?)\\b"
  )
  text_search(paper, none_words, perl = TRUE)
}
