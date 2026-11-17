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
# subscript after it ("eta-squared p = .06"), which extract_p_values() and
# extract_eq() do not read as a p-value (#336)
.eta_p_prefix <- "[\u03b7\u019e]\\s*[\u00b22\u2082]?\\s*"

# Names written after p, glued or after a space, hyphen or underscore, that
# make it a corrected or otherwise named p-value ("pFWE = .006", "p adj =
# .034", "P MCMC = 0.27", "p interaction = 0.003", "pMantel < .01"):
# correction methods, names of p-values and names of tests. A fixed list,
# so that "PSA (ng/ml) = 0.5" or "SD = 0" are not read as p-values (#21,
# #642). Not GLM: "p GLM < 7.1" lost its exponent.
.p_subscripts <- paste0(
  "(?i:adj(?:usted)?|corr(?:ected)?|uncorr(?:ected)?|unc|fwer?|fdr|",
  "bonf(?:erroni)?|holm|tukey|perm(?:uted|utation)?|boot|mcmc|",
  "int(?:eraction)?|trend|het(?:erogeneity)?|cluster|svc|",
  # tests
  "anova|mantel|wilcox(?:on)?|mann.?whitney|mwu?|kruskal(?:.?wallis)?|kw|",
  "kolmogorov(?:.?smirnov)?|ks|fisher|binom(?:ial)?|chi.?(?:2|sq(?:uared?)?)|",
  "\u03c7\u00b2?2?|lrt|wald|log.?rank|t.?test|welch|dunn|games.?howell|",
  "spearman|pearson)(?![\\w])")

# The correction or test name after the p of each p-value, as
# extract_p_values() matched it ("FWE" in "pFWE < .05", "adj" in "p adj =
# .03"); NA for a plain p (#642)
.p_name <- function(text) {
  re <- paste0("^[pP](?:-?values?|s|['\u2019]s)?[\\s_-]?(", .p_subscripts, ").*$")
  text <- trimws(text)
  out <- rep(NA_character_, length(text))
  named <- grepl(re, text, perl = TRUE)
  out[named] <- sub(re, "\\1", text[named], perl = TRUE)
  out
}

#' Extract P-Values
#'
#' List all p-values in the text, returning the matched text (e.g., 'p = 0.04') and document location in a table.
#'
#' @details
#' Note that this will not catch p-values reported like "the p-value is 0.03" because that results in a ton of false positives when papers discuss p-value thresholds. If you need to detect text like that, use the `text_search()` function and a custom pattern.
#'
#' This will catch most comparators like =<>~ and most versions of scientific notation like 5.0 x 10^-2, 5.0e-2, 5E-2, 1e-10 and 10^-15 written as "10 -15". If you find any formats that are not correctly handled by this function, please contact the author.
#'
#' A p may carry the name of a correction, of the kind of p-value or of a test, glued to it or after a space ("pFWE = .006", "p adj = .034", "p Bonferroni < .05", "P MCMC = 0.27", "p interaction = .003", "pMantel < .01", "p Wilcoxon = .021"). The column `p_name` holds that name as written ("FWE", "adj"), and is NA for a plain p, so that a raw and a corrected p-value of the same test can be told apart.
#'
#' A p-value written as a whole 0 or 1 ("p = 1", "p = 0") is found only after "=", "~" or the approximately-equal sign, when a bracket, a semicolon or a comma follows it (after 1 also a full stop), and when its sentence reports a test: a test statistic ("t(20) = 0.3"), another p-value with a decimal point or an exponent, or a word that names a test or its outcome ("Fisher's exact test", "not significant", "did not differ"). Otherwise "p = 1" is often a probability or a model parameter, and "p < 0." is a p-value whose digits were lost when the PDF was read.
#'
#' A "p" that is the subscript of another symbol is not a p-value: after a standing r or R, or a Greek letter, with "=" ("r p = .64" for a correlation, "R p = 3.83%", "alpha p = .95"), and written against a superscript 2 or a Delta with any comparison ("2p = .02" for partial eta squared whose eta was lost, "Delta p = .33" for a contingency). With "<" or ">" such a letter marks a significance level in a note ("a p < 0.05", "alpha p < 0.05"), and the p-value is kept. A value below 0 or above 1 is not a p-value when a unit follows it ("P = 2.9 um", "P = 6.02 MPa"), a logarithm comes before it ("-log10 P = 7.5") or the p is part of a ratio or a formula ("Ca/P = 1.67", "1-p").
#'
#' A p-value written with a minus ("p = -.22") is not returned: no p-value is negative. `stat_p_exact` lists those as impossible.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with one row per p-value: the matched text (`text`), its location, the comparison (`p_comp`), the value (`p_value`) and the name after the p (`p_name`, NA for a plain p)
#' @export
#'
#' @examples
#' paper <- demopaper()
#' p_values <- extract_p_values(paper)
extract_p_values <- function(paper) {
  .p_extract(paper)
}

# A symbol standing right before a p (#351): a standing r or R, a Greek
# letter, a superscript 2, or a Delta. With "=" the p is that symbol's
# subscript, not a p-value: a correlation ("r p = .64"), a reliability factor
# ("R p = 3.83%"), Cronbach's alpha or a model parameter ("\u03B1 p = .95"). With
# "<" or ">" the symbol marks a significance level in a note ("a p < 0.05",
# "\u03B1 p < 0.05", "\u0394\u0394 p < 0.01"), and the p is a p-value. Written
# against the p, a superscript 2 is partial eta squared whose eta was lost
# ("\u00B2p = 0.02"), and a Delta a change in p or a contingency ("\u0394p = .33"),
# whatever the comparison. In 18 corpora (16,847 papers), r and R gave 53 of
# 55 subscripts, a Greek letter with "=" 10 of 10 and with "<" none of 4, the
# superscript 2 16 of 16, and the Delta written against the p 11 of 13.
.p_symbol_prefix <- paste0(
  "(?<![\\w.'\u2019-])[rR\u0391-\u03A9\u03B1-\u03C9]\\s+",  # a standing letter
  "|\u00B2\\s*|[\u0394\u2206]")                               # against the p

# The p-values of a paper, as extract_p_values() returns them; with
# `negative` TRUE, only the p-values written with a minus ("p = -.22"), which
# no p-value can be: the minus of a statistic copied into it, or a typing
# error (#351). These have the value of the number with its minus, and only
# those from -1 to 0 are kept, the numbers that are a p-value without it.
.p_extract <- function(paper, negative = FALSE) {
  # a character vector is searched as a text table, one row per string:
  # text_search() returns plain matches for one, and p$text stopped with "$
  # operator is invalid for atomic vectors" (U150)
  if (is.character(paper))
    paper <- data.frame(text = paper, text_id = seq_along(paper))
  # "p = 0\u00B70079" in the style of The Lancet (#648)
  paper <- .stat_decimal_point(paper)

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
  #
  # Numbers without a decimal point (#642): a whole first number before
  # scientific notation ("p < 1e-10", "p = 5 x 10 -8"), a capital E
  # ("p = 2.4E-08" was read as 2.4), 10 to a power with nothing before it
  # ("p < 10 -15"), and a whole 0 or 1 ("p = 1"). GROBID reads the minus
  # sign as A with a grave accent and the multiplication sign as A with a
  # circumflex or a tilde in some PDFs ("p<2.2\u00c210 \u00c016" was read as 2.2).
  minus <- "[-\u2212\u00c0]"
  sci_e <- paste0("[eE]\\s*", minus, "\\d+")
  sci_x <- paste0("[x\\*\u00d7\u00c2\u00c3]\\s*10\\s*\\^?\\s*", minus, "\\s*\\d+")
  # what may follow a whole 0 or 1: a bracket, a semicolon, or a comma with
  # no digit after it; after 1 also a full stop or the end of the text. Not
  # "p < 0." (the digits of "p < 0.05" were lost), "p = 0,05",
  # "p = 1/6", "P = 1 bar" or "p = 1 \u00d7 10\u207b\u2076" (an exponent that cannot be read).
  # "p = 0\u00b705", in the style of The Lancet, is read as 0.05: the text is
  # changed first (.stat_decimal_point(); #648)
  end01 <- "\\s*(?:[)\\];]|,(?!\\s*\\d))"
  number <- paste0(
    "n\\.?s\\.?",                                   # ns
    "|\\d?\\.\\d+",                                 # a number with a decimal point
    "|[1-9](?=\\s*(?:", sci_e, "|", sci_x, "))",    # 1e-10, 5 x 10^-8
    "|10\\s*\\^?\\s*", minus, "\\s*\\d+",           # 10^-15, written "10 -15"
    "|0(?=", end01, ")",                            # p = 0
    "|1(?=", end01, "|\\s*\\.(?!\\s*\\d)|\\s*$)"    # p = 1
  )
  # a p-value with a minus: the minus (also an en dash, or GROBID's A with a
  # grave accent) and a number with a decimal point (#351)
  sign <- "[-\u2212\u2013\u00c0]"
  if (negative) number <- paste0(sign, "\\s*\\d?\\.\\d+")
  pattern <- paste0(
    "(?:", .eta_p_prefix, "|", .p_symbol_prefix, ")?",
    "\\b[pP](?:-?values?|s|['\u2019]s)?", # ways to write p (or P)
    "(?:[\\s_-]?", .p_subscripts, ")?\\s*", # pFWE, p adj
    "[", operators, "]{1,2}\\s*", # 1-2 operators
    "(", number, ")", # ns or valid numbers
    if (!negative) paste0(
      "\\s*(", sci_e, ")?", # also match scientific notation
      "(\\s*", sci_x, ")?")
  )

  p_pattern <- pattern
  p <- text_search(paper, pattern,
    return = "match",
    perl = TRUE, ignore.case = FALSE
  )
  p <- p[!grepl("^[\u03b7\u019e]", p$text), , drop = FALSE]

  # get operator
  op_pattern <- paste0("[", operators, "]{1,2}")
  matches <- gregexpr(op_pattern, p$text, perl = TRUE)
  # vapply: with no match, sapply() gave a list, which a table of p-values
  # could not be joined to (#351)
  p$p_comp <- vapply(regmatches(p$text, matches), `[[`, "", 1)

  # a symbol before the p: its subscript with "=", or a note's marker (see
  # .p_symbol_prefix); a marker leaves the match (#351)
  sym <- regmatches(p$text, regexpr(paste0("^(?:", .p_symbol_prefix, ")"), p$text, perl = TRUE))
  has_sym <- grepl(paste0("^(?:", .p_symbol_prefix, ")"), p$text, perl = TRUE)
  if (any(has_sym)) {
    against <- grepl("^(?:\u00b2|[\u0394\u2206](?!\\s))", p$text[has_sym], perl = TRUE)
    subscript <- against | p$p_comp[has_sym] %in% c("=", "==", "\u2248", "~")
    p$text[has_sym] <- substring(p$text[has_sym], nchar(sym) + 1L)
    drop <- has_sym
    drop[has_sym] <- subscript
    p <- p[!drop, , drop = FALSE]
  }

  # get value
  s <- strsplit(p$text, paste0("\\s*[", operators, "]{1,2}\\s*"))
  pvals <- vapply(s, \(x) x[[2]], "")
  if (negative) {
    p$p_value <- -suppressWarnings(as.numeric(sub(paste0("^", sign, "\\s*"), "", pvals)))
  } else {
    # the one reader of written scientific notation, shared with the matcher's
    # .norm_value() (R/stat_helpers.R) (#642)
    p$p_value <- suppressWarnings(as.numeric(.sci_text(pvals, ten_power = TRUE)))
  }
  # the name after the p: a raw and a corrected p-value of one test are two
  # rows, which this tells apart (#642)
  p$p_name <- .p_name(p$text)

  # the sentence of each row in `rows`, from the sentences with a match:
  # text_expand() rebuilt the text table of a whole paper list for a few
  # sentences, and took 13 of 14 seconds for 1,000 papers
  sentence_of <- function(rows) {
    sent <- text_search(paper, p_pattern, return = "sentence",
                        perl = TRUE, ignore.case = FALSE)
    by <- intersect(c("paper_id", "section_id", "paragraph_id", "text_id"),
                    intersect(names(p), names(sent)))
    at <- match(do.call(paste, c(as.list(p[rows, by, drop = FALSE]), sep = "\r")),
                do.call(paste, c(as.list(sent[, by, drop = FALSE]), sep = "\r")))
    sent$text[at]
  }

  # A value below 0 or above 1 is not a p-value when a unit follows it, a
  # logarithm comes before it, or the p is part of a name or a formula
  # ("P = 2.9 \u00b5m", "-log10 P = 7.5", "Ca/P = 1.67"; .p_not_a_value(); #351)
  odd <- !is.na(p$p_value) & (p$p_value < 0 | p$p_value > 1)
  if (any(odd)) {
    # every p-value of those sentences, so .p_clause() counts their places
    key <- paste(p$paper_id, p$text_id, sep = "\r")
    in_sent <- key %in% key[odd]
    q <- p[in_sent, , drop = FALSE]
    q$expanded <- sentence_of(which(in_sent))
    cl <- .p_clause(q)
    not_p <- !is.na(.p_not_a_value(cl$before, cl$after)) & odd[in_sent]
    drop <- in_sent
    drop[in_sent] <- not_p
    p <- p[!drop, , drop = FALSE]
  }
  if (negative) {
    p <- p[!is.na(p$p_value) & p$p_value >= -1, , drop = FALSE]
    return(p)
  }

  # A whole 0 or 1 is a p-value only after "=", "\u2248" or "~" ("0 < p < 1"
  # bounds a probability, and "p < 1" says nothing), and only when its
  # sentence reports a test: "p = 1" is otherwise often a probability or a
  # model parameter ("with p = 1", "ARIMA values p = 0"). In 6,280 papers no
  # rule was written from, 26 of 114 were not p-values without that, and 11
  # of 93 with it, which kept 82 of the 88 p-values (#642)
  whole <- grepl(paste0("[", operators, "]\\s*[01]$"), trimws(p$text))
  if (any(whole)) {
    ok <- p$p_comp[whole] %in% c("=", "==", "\u2248", "~")
    sentence <- sentence_of(which(whole))
    ok <- ok & .reports_test(sentence, nonzero = p$p_value[whole] == 0)
    whole[whole] <- !ok
    p <- p[!whole, , drop = FALSE]
  }

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
  # "p = 0\u00B70079" in the style of The Lancet (#648)
  paper <- .stat_decimal_point(paper)
  # "\u03C7 2 (10) = 9.61": the chi joined to its 2 (#351)
  paper <- .stat_chi_join(paper)

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
  # One character at a time, not "[0-9,\\.\\s]+" repeated: a part that can
  # repeat inside a repeated group let the engine try every way of dividing a
  # bracket of numbers ("(n = 308, 1,168, 564, 654, 1,492)") and stop at its
  # step limit, which emptied the search of the whole paper list (#645). The
  # same texts match.
  df_inner <- "(?:[0-9,.\\s]|[nN]\\s*=\\s*[0-9]+)*"
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
  # partial eta squared with its subscript after it, "eta-squared p = .06",
  # is one statistic, not "p = .06" (#336)
  bare_subscript <- paste0(bare_subscript, "|", .eta_p_prefix, "[pP](?=\\s*[", op, "])")
  # a p with a correction or test name after a space, "p adj = .034", is one
  # statistic: the name was "adj" (#642)
  bare_subscript <- paste0(bare_subscript, "|\\b[pP]s?[\\s_-]", .p_subscripts,
                           "(?=\\s*[", op, "])")
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
    # 10 to a power with nothing before it, "p < 10 -15", was "10": kept
    # below only for a p-value, as elsewhere "10 -15" can be a range (#642)
    "((?<=(?<![\\d.])10)\\s*\\^?\\s*[-\u2212\u00c0]\\s*\\d+)?",
    # also match scientific notation, with GROBID's A-accent readings of
    # the minus and multiplication signs ("2.2\u00c210 \u00c016"; #642)
    "\\s*(e\\s*[-\u2212\u00c0]\\d+)?",
    "(\\s*[x\\*\u00d7\u00c2\u00c3]\\s*10\\s*\\^?\\s*[-\u2212\u00c0]\\s*\\d+)?"
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
  # "10 -15" is 10 to a power only for a p-value; for another statistic the
  # value stays "10", as before (#642)
  is_p <- grepl(paste0("^[pP](?:s|-?values?|['\u2019]s)?(?:[\\s_-]?", .p_subscripts, ")?$"),
                eq$lhs, perl = TRUE)
  eq$rhs[!is_p] <- sub("^10(?=\\s|\\^|\u00c0)\\s*\\^?\\s*[-\u00c0]\\s*\\d+$", "10",
                       eq$rhs[!is_p], perl = TRUE)
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
