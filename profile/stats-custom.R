# this script contains the regular expressions that statcheck uses to extract
# NHST results from text

# test types
RGX_T <- "t"
RGX_R <- "r"
RGX_Q <- "Q\\s?-?\\s?(w|W|(w|W)ithin|b|B|(b|B)etween)?"
RGX_F <- "F"
RGX_Z <- "([^a-z](z|Z))"
# for chi2: effectively extract everything that is NOT a t, r, F, z, Q, W, n, or
# D, followed by *maybe* a 2 (and later followed by a result in a chi2 layout)
RGX_CHI2 <- "((\\s[^trFzZQWnD ]\\s?)|([^trFzZQWnD ]2\\s?))2?"

# degrees of freedom
# the way dfs are reported differs per test type, except for t, r, and Q, where
# they are always in the format "(28)". The regex for these tests can therefore
# be combined
# z-tests do not have dfs
RGX_DF_T_R_Q <- "\\(\\s?\\d*\\.?\\d+\\s?\\)"
RGX_DF_F <- "\\(\\s?\\d*\\.?(I|l|\\d+)\\s?,\\s?\\d*\\.?\\d+\\s?\\)"
RGX_DF_CHI2 <- "\\(\\s?\\d*\\.?\\d+\\s?(,\\s?(N|n)\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\)"

# combine test types with the correct type of df
# put regex between () to create regex groups
RGX_T_DF <- paste0("(", RGX_T, "\\s?", RGX_DF_T_R_Q, ")")
RGX_R_DF <- paste0("(", RGX_R, "\\s?", RGX_DF_T_R_Q, ")")
RGX_Q_DF <- paste0("(", RGX_Q, "\\s?", RGX_DF_T_R_Q, ")")
RGX_F_DF <- paste0("(", RGX_F, "\\s?", RGX_DF_F, ")")
RGX_CHI2_DF <- paste0("(", RGX_CHI2, "\\s?", RGX_DF_CHI2, ")")

RGX_TEST_DF <- paste0("(", RGX_T_DF, "|", RGX_R_DF, "|", RGX_Q_DF, "|", RGX_F_DF,
                      "|", RGX_CHI2_DF, "|", RGX_Z, ")")

# test value
# this is the same for every type of test
# the part "[^a-zA-Z\\d\\.]{0,3}" is to extract punctuation marks that could
# signal a weirdly encoded minus sign
# note that impossible values such as r > 1 are excluded at a later stage
RGX_TEST_VALUE <- "[<>=]\\s?[^a-zA-Z\\d\\.]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,"

# p-values
# this is the same for every type of test
RGX_NS <- "([^a-z]n\\.?s\\.?)"
RGX_P <- "(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*)"
RGX_P_NS <- paste0("(", RGX_NS, "|", RGX_P, ")")

# full result
RGX_NHST <- paste(RGX_TEST_DF, RGX_TEST_VALUE, RGX_P_NS, sep = "\\s?")

################################################################################

# regex to recognize test type

# match everything up until the first occurence of a "(" with a positive look
# ahead. A "(" signals the start of the degrees of freedom, so everything before
# that should be the test statistic. Also match the regex for a z-test
# (because a z-test has no df)

RGX_OPEN_BRACKET <- "(.+?(?=\\())"
RGX_TEST_TYPE <- paste(RGX_Z, RGX_OPEN_BRACKET, sep = "|")

# regex for Q-test

# for the Q-test, we also need to distinguish between Q, Qw, and Qb
# select all raw_nhst results that seem to have a Q-test in them
# it suffices to simply search for the letters "w" and "b"
RGX_QW <- "w"
RGX_QB <- "b"

# regex for degrees of freedom

# combine the separate regexes for the different types of dfs
# in one all-encompassing regex. Group the df-types with parentheses and
# separate with an OR sign
RGX_DF <- paste0("(", RGX_DF_T_R_Q, ")|(", RGX_DF_F, ")|(", RGX_DF_CHI2, ")")

# regex for comparison symbols
RGX_COMP <- "[<>=]"

# regex for thousands separator
# this regex matches commas flanked by digits on both sides
RGX_1000_SEP <- "(?<=\\d),(?=\\d+)"

# regex for numbers after a point
# used to determine number of decimals
RGX_DEC <- "\\.\\d+"

# regex for weird symbols that should be a minus sign
# match potentially a space, followed by one or more characters that are not a
# digit, period, or space, followed by a digit or period (using a positive
# lookahead)
RGX_WEIRD_MINUS <- "\\s?[^\\d\\.\\s]+(?=\\d|\\.)"

# regex for weird df1 in F-tests
# for some reason, typesetting in articles sometimes goes wrong with
# F-tests and when df1 == 1, it gets typeset as the letter l or I
RGX_DF1_I_L <- "I|l"




#' Check Stats
#'
#' @param text the search table (or list of paper objects)
#' @param ... arguments to pass to statcheck()
#'
#' @return a table of statistics
#' @export
#'
#' @examples
#' filename <- system.file("grobid", "incest.xml", package="papercheck")
#' study <- read(filename)
#' stats(study)
stats <- function(text, ...) {
  if (!is.data.frame(text)) {
    text <- search_text(text)
  }

  n <- nrow(text)
  if (n == 0) return(data.frame())

  checks <- statcheck(text$text, ...)

  if (nrow(checks) == 0) return(checks)

  text$source = seq_along(text$text)

  stat_table <- dplyr::left_join(checks, text, by = "source")
  rownames(stat_table) <- NULL
  stat_table$source <- NULL

  return(stat_table)
}


#' Check p-values
#'
#' @param text the search table (or list of paper objects)
#'
#' @return a table of p-values
#' @export
#'
#' @examples
#' filename <- system.file("grobid", "incest.xml", package="papercheck")
#' study <- read(filename)
#' check_p_values(study)
check_p_values <- function(text) {
  p <- stats(text, AllPValues = TRUE)

  p$imprecise <- p$p_comp == "<" & p$reported_p > .001
  p$imprecise <- p$imprecise | p$p_comp == ">"

  return(p)
}

#' Statcheck (papercheck version)
#'
#' @inheritParams statcheck::statcheck
#'
#' @return data frame
#' @keywords internal
#'
statcheck <- function (texts,
                       stat = c("t", "F", "cor", "chisq", "Z", "Q"),
                       OneTailedTests = FALSE,
                       alpha = 0.05,
                       pEqualAlphaSig = TRUE,
                       pZeroError = TRUE,
                       OneTailedTxt = FALSE,
                       AllPValues = FALSE) {
  pRes <- extract_p_value(texts)

  if (AllPValues) {
    if (nrow(pRes) > 0) {
      pRes <- pRes[, c("Source", "p_comp", "p_value", "p_dec")]
      colnames(pRes) <- c(VAR_SOURCE, VAR_P_COMPARISON,
                          VAR_REPORTED_P, VAR_P_DEC)
      return(pRes)
    } else {
      return(data.frame())
    }
  }


  # set up progress bar ----
  if (verbose()) {
    pb <- progress::progress_bar$new(
      total = length(texts), clear = FALSE,
      format = "Checking stats [:bar] :current/:total :elapsedfull"
    )
    pb$tick(0)
    Sys.sleep(0.2)
    pb$tick(0)
  }

  Res <- data.frame(NULL)
  nhst <- extract_stats(txt = txt, stat = stat)

  for (i in seq_along(texts)) {
    txt <- texts[[i]]

    nhst <- statcheck:::extract_stats(txt = txt, stat = stat)
    if (nrow(nhst) > 0) {
      nhst$Source <- i
      nhst$OneTailedInTxt <- statcheck:::extract_1tail(txt)
      Res <- rbind(Res, nhst)
    }

    if (verbose()) pb$tick()
  }

  # process all values
  if (nrow(Res) > 0) {
    Res$Computed <- rep(NA, nrow(Res))
    Res$Error <- rep(NA, nrow(Res))
    Res$DecisionError <- rep(NA, nrow(Res))
    for (i in seq_len(nrow(Res))) {
      result <- statcheck:::process_stats(
        test_type = Res$Statistic[i],
        test_stat = Res$Value[i],
        df1 = Res$df1[i], df2 = Res$df2[i],
        reported_p = Res$Reported.P.Value[i],
        p_comparison = Res$Reported.Comparison[i],
        test_comparison = Res$Test.Comparison[i],
        p_dec = Res$dec[i],
        test_dec = Res$testdec[i],
        OneTailedInTxt = Res$OneTailedInTxt[i],
        two_tailed = !OneTailedTests,
        alpha = alpha,
        pZeroError = pZeroError,
        pEqualAlphaSig = pEqualAlphaSig,
        OneTailedTxt = OneTailedTxt,
        OneTailedTests = OneTailedTests)
      Res$Computed[i] <- result$computed_p
      Res$Error[i] <- result$error
      Res$DecisionError[i] <- result$decision_error
    }

    Res$APAfactor <- statcheck:::calc_APA_factor(pRes, Res)
    Res <- Res[, c("Source", "Statistic", "df1", "df2", "Test.Comparison",
                   "Value", "Reported.Comparison", "Reported.P.Value",
                   "Computed", "Raw", "Error", "DecisionError", "OneTailedInTxt",
                   "APAfactor")]
    colnames(Res) <- c(VAR_SOURCE, VAR_TYPE, VAR_DF1, VAR_DF2,
                       VAR_TEST_COMPARISON, VAR_TEST_VALUE, VAR_P_COMPARISON,
                       VAR_REPORTED_P, VAR_COMPUTED_P, VAR_RAW, VAR_ERROR,
                       VAR_DEC_ERROR, VAR_1TAILTXT, VAR_APAFACTOR)

    class(Res) <- c("statcheck", "data.frame")

  }

  return(Res)
}

extract_p_value <- function (raw) {
  matches <- regexpr(pattern = RGX_P_NS, raw, ignore.case = TRUE)
  df <- data.frame(
    Source = which(matches != -1),
    p_raw = regmatches(raw, matches)
  )

  # get symbol
  df$p_comp <- df$p_raw
  matches <- regexpr(pattern = RGX_COMP, df$p_raw, ignore.case = TRUE)
  regmatches(df$p_comp, matches, invert = TRUE) <- ""

  # get p-value
  df$p_value <- strsplit(df$p_raw, RGX_COMP) |>
    sapply(\(x) ifelse(length(x) == 2, x[[2]], NA)) |>
    trimws(which = "both")

  # get precision of p-value before converting to numeric
  df$p_dec <- attr(regexpr(RGX_DEC, df$p_value), "match.length") - 1
  df$p_dec[df$p_dec < 0] <- 0

  df$p_value <- as.numeric(df$p_value)

  # handle ns
  is_ns <- grepl(RGX_NS, df$p_raw, ignore.case = TRUE)
  df$p_comp[is_ns] <- "ns"
  df$p_value[is_ns] <- NA # probably not necessary
  df$p_dec[is_ns] <- NA # probably not necessary

  return(df[, c("Source", "p_comp", "p_value", "p_dec")])
}

extract_stats <- function (txt, stat) {
  matches <- regexpr(RGX_NHST, txt, ignore.case = FALSE, perl = TRUE)
  df <- data.frame(
    Source = which(matches != -1),
    nhst_raw = regmatches(txt, matches)
  )
  if (nrow(nhst_raw) == 0) {
    return(data.frame(NULL))
  }

  matches <- regexpr(RGX_TEST_TYPE, df$nhst_raw, perl = TRUE)
  df$test_raw <- regmatches(df$nhst_raw, matches) |> trimws()

  df$test_type <- NA
  Q <- grepl(pattern = RGX_Q, x = df$test_raw)
  df$test_type[Q] <- "Q"
  df$test_type[Q & grepl(pattern = RGX_QB, x = df$test_raw)] <- "Qb"
  df$test_type[Qw & grepl(pattern = RGX_QW, x = df$test_raw)] <- "Qw"
  df$test_type[grepl(pattern = RGX_T, x = df$test_raw)] <- "t"
  df$test_type[grepl(pattern = RGX_F, x = df$test_raw)] <- "F"
  df$test_type[grepl(pattern = RGX_R, x = df$test_raw)] <- "r"
  df$test_type[grepl(pattern = RGX_Z, x = df$test_raw)] <- "Z"
  df$test_type[grepl(pattern = RGX_CHI2, x = df$test_raw)] <- "Chi2"

  for (i in seq_along(nhst_raw)) {
    # gave up refactoring here...
    dfs <- extract_df(raw = nhst_raw[i], test_type = test_type[i])
    df_result <- rbind(df_result, dfs)
    test <- extract_test_stats(raw = nhst_raw[i])
    if (nrow(test) > 1) {
      test <- data.frame(test_comp = NA, test_value = NA,
                         test_dec = NA)
    }
    test_stats <- rbind(test_stats, test)
    p <- extract_p_value(raw = nhst_raw[i])
    if (nrow(p) > 1) {
      p <- data.frame(p_comp = NA, p_value = NA, p_dec = NA)
    }
    pvals <- rbind(pvals, p)
  }

  nhst_parsed <- data.frame(Raw = trimws(nhst_raw, which = "both"),
                            Statistic = test_type, df1 = df_result$df1, df2 = df_result$df2,
                            Test.Comparison = test_stats$test_comp, Value = test_stats$test_value,
                            testdec = test_stats$test_dec, Reported.Comparison = pvals$p_comp,
                            Reported.P.Value = pvals$p_value, dec = pvals$p_dec,
                            stringsAsFactors = FALSE)
  if (nrow(nhst_parsed) > 0) {
    nhst_parsed <- nhst_parsed[nhst_parsed$Reported.P.Value <=
                                 1 | is.na(nhst_parsed$Reported.P.Value), ]
    nhst_parsed <- nhst_parsed[!(nhst_parsed$Statistic ==
                                   "r" & (nhst_parsed$Value > 1 | nhst_parsed$Value <
                                            -1)), ]
    nhst_parsed <- nhst_parsed[!is.na(nhst_parsed$Value),
    ]
    nhst_parsed <- nhst_parsed[!is.na(nhst_parsed$Test.Comparison) &
                                 !is.na(nhst_parsed$Reported.Comparison), ]
    types <- as.vector(nhst_parsed$Statistic)
    types[types == "r"] <- "cor"
    types[types == "Chi2"] <- "chisq"
    types[types == "Z"] <- "Z"
    types[types == "Qw" | types == "Qb"] <- "Q"
    nhst_parsed <- nhst_parsed[types %in% stat, ]
  }
  class(nhst_parsed) <- c("statcheck", "data.frame")
  return(nhst_parsed)
}
