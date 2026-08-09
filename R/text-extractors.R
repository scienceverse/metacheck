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
  table <- text_search(paper, pattern, return = "match", "perl" = TRUE)

  return(table)
}


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

  pattern <- paste0(
    "\\bp-?(value)?\\s*", # ways to write p
    "[", operators, "]{1,2}\\s*", # 1-2 operators
    "(n\\.?s\\.?|\\d?\\.\\d+)", # ns or valid numbers
    "\\s*(e\\s*-\\d+)?", # also match scientific notation
    "(\\s*[x\\*]\\s*10\\s*\\^\\s*-\\d+)?"
  )

  p <- text_search(paper, pattern,
    return = "match",
    perl = TRUE, ignore.case = FALSE
  )

  # get operator
  pattern <- paste0("[", operators, "]{1,2}")
  matches <- gregexpr(pattern, p$text, perl = TRUE)
  p$p_comp <- regmatches(p$text, matches) |> sapply(`[[`, 1)

  # get value
  s <- strsplit(p$text, paste0("\\s*[", operators, "]{1,2}\\s*"))
  pvals <- sapply(s, \(x) x[[2]]) |>
    gsub("\\s", "", x = _) |>
    gsub("[x*]10\\^", "e", x = _)
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
  pattern <- paste0(
    "(?:(Hedge.{0,3}|Cronbach.{0,2}|Cohen.{0,2}|\\d{1,2}%)\\s+)?", # common prefix
    "[", gr, "\u00B2a-zA-Z-_\\.0-9\\{\\}\\^\\\\]+\\s*", # statistic name
    "(?:\\(", df_inner, "\\))?\\s*", # optional df-shaped parentheses
    "[", op , "]{1,3}\\s*", # 1-3 operators
    "([0-9\\.,+-]*[0-9]|\\[[^\\]]+\\]|n\\.?\\s*s\\.?)", # valid numbers or anything in [] or NS
    "\\s*(e\\s*-\\d+)?", # also match scientific notation
    "(\\s*[x\\*]\\s*10\\s*\\^\\s*-\\d+)?"
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

  eq$rhs <- sapply(s, \(x) x[[2]]) |> trimws()
  #   gsub("\\s", "", x = _) |>
  #   gsub("[x*]10\\^", "e", x = _)
  # eq$rhs <- suppressWarnings(as.numeric(rhs))

  # set group equal to sentence for now
  eq$grp_id <- 1
  for (i in seq_along(eq$text_id)) {
    if (i == 1 || eq$paper_id[[i]] != eq$paper_id[[i-1]]) {
      eq$grp_id[[i]] <- 1
    } else if (eq$text_id[[i]] == eq$text_id[[i-1]]) {
      eq$grp_id[[i]] <- eq$grp_id[[i-1]]
    } else {
      eq$grp_id[[i]] <- eq$grp_id[[i-1]] + 1
    }
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
