#' GRIM Test
#'
#' @description
#' Check whether reported means are mathematically possible given the sample
#' size (GRIM test; Brown & Heathers, 2017).
#'
#' @details
#' The GRIM (Granularity-Related Inconsistency of Means) test checks whether a
#' mean reported to d decimal places could result from averaging n integer
#' values: the mean must be within rounding distance of k/n for some integer k.
#' The test only has diagnostic value when n < 10^d (e.g., n < 100 for means
#' reported to 2 decimals).
#'
#' This module extracts means (e.g., "M = 4.32") and pairs them with candidate
#' sample sizes from three sources, in order of preference: (1) sample sizes
#' in the same sentence (e.g., "n = 28", "28 participants"), including
#' sample sizes recovered from the degrees of freedom of t tests and
#' F(1, df) tests in the sentence (df + 1 for paired/one-sample designs,
#' df + 2 for independent designs; Welch-corrected fractional df are
#' skipped); (2) if none, sample sizes and test df found in the surrounding
#' paragraph. A mean is only flagged as inconsistent if it fails the GRIM
#' test for *every* candidate sample size. Sentences mentioning units that
#' imply non-integer data (time, distance, money, rates) are skipped,
#' because GRIM only applies to means of integer (e.g., Likert or count)
#' data.
#'
#' Flagged means are not necessarily errors: the mean may be based on a
#' multi-item scale (granularity 1/(n*items)), on a subsample, or on
#' non-integer data not caught by the unit filter. The table reports the
#' smallest number of scale items (1-4) that would make each mean consistent.
#' Means from multi-item scales need manual checking against the number of
#' scale items reported in the paper.
#'
#' The GRIM consistency math, total-n dispersal, and item-granularity
#' calculations are performed by the \pkg{scrutiny} package.
#'
#' **Optional LLM use.** The most common reason a flagged mean is not a real
#' error is that it summarises *non-integer* data (e.g., reaction times,
#' EEG amplitudes, difference scores, proportions) that the unit filter did
#' not catch. If `use_llm = TRUE` and an LLM is enabled (`llm_use(TRUE)`),
#' the module sends *only the already-flagged sentences* to a (preferably
#' local, GDPR-compliant) LLM, which classifies whether the underlying data
#' are integers. Sentences the LLM judges to be non-integer data are
#' demoted from "inconsistent" to "not applicable". The LLM is used **only
#' to suppress false positives** — it never creates a new flag and never
#' changes the GRIM math. With `use_llm = FALSE` (the default), the module
#' is fully deterministic and offline.
#'
#' This classification needs reasonable world knowledge (distinguishing,
#' e.g., a Likert response from an EEG amplitude in microvolts), so use a
#' capable model. In testing, small models (3-4B parameters) defaulted
#' almost everything to "non-integer" and suppressed genuine flags, whereas
#' an 8B model (e.g., `llm_model("ollama/llama3.1:8b")`) correctly kept
#' integer-data flags while removing clear non-integer false positives.
#' The classifier can only judge from the flagged sentence, so means whose
#' data type is described elsewhere in the paper may still be misjudged.
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @references
#' # Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple Technique Detects Numerous Anomalies in the Reporting of Results in Psychology. Social Psychological and Personality Science, 8(4), 363-369. https://doi.org/10.1177/1948550616673876
#'
#' @import dplyr
#' @import scrutiny
#'
#' @param paper a paper object or paperlist object
#' @param use_llm whether to use an LLM to suppress false positives by
#'   classifying flagged means as integer or non-integer data (requires
#'   `llm_use(TRUE)`; see Details). Defaults to FALSE.
#'
#' @returns a list
grim <- function(paper, use_llm = FALSE) {
  # GRIM consistency via scrutiny ----
  # scrutiny::grim() takes the mean as a string (preserving reported
  # decimals) and handles rounding regimes; grim_probability() <= 0 means
  # the test has no diagnostic value for this n (returns NA)
  grim_consistent <- function(mean_str, n, items = 1) {
    if (is.na(n) || n <= 0) return(NA)
    p <- tryCatch(
      scrutiny::grim_probability(mean_str, as.integer(n), items = items),
      error = \(e) NA_real_
    )
    if (is.na(p) || p <= 0) return(NA)
    unname(scrutiny::grim(mean_str, as.integer(n), items = items))
  }

  # smallest items (1-4) under which the mean is GRIM-consistent;
  # when the test has no power the mean cannot be ruled out at that
  # granularity (treat as consistent)
  min_items <- function(mean_str, n) {
    for (it in 1:4) {
      ok <- grim_consistent(mean_str, n, items = it)
      if (is.na(ok) || ok) return(it)
    }
    NA_integer_
  }

  # total-n dispersal (scrutiny's *_map_total_n logic): when a sentence
  # reports two group means but only a total N, test whether ANY split
  # of N across the groups (n/2 +/- 5, both assignments) makes both
  # means consistent
  total_n_consistent <- function(m1_str, m2_str, N) {
    half <- floor(N / 2)
    for (k in max(5, half - 5):half) {
      n1 <- k
      n2 <- N - k
      ok11 <- isTRUE(grim_consistent(m1_str, n1)) &
        isTRUE(grim_consistent(m2_str, n2))
      ok12 <- isTRUE(grim_consistent(m1_str, n2)) &
        isTRUE(grim_consistent(m2_str, n1))
      if (ok11 || ok12) return(TRUE)
      # splits beyond testability cannot be ruled out
      if (is.na(grim_consistent(m1_str, n1)) &&
          is.na(grim_consistent(m2_str, n2))) return(NA)
    }
    FALSE
  }

  # optional LLM false-positive filter ----
  # classifies whether each flagged mean summarises integer data; returns
  # a logical vector (TRUE = non-integer, so the flag should be suppressed).
  # Used ONLY to remove false positives -- never to create a flag.
  llm_is_noninteger <- function(reported, sentences) {
    sys <- paste(
      "You are screening flagged statistics from psychology papers.",
      "For each TARGET mean reported in the SENTENCE, decide whether the",
      "values that were averaged to produce it are whole numbers (integers).",
      "Integer data: Likert/rating scale responses, counts, number of",
      "items/errors/trials, age in whole years.",
      "Non-integer data: reaction or response times, durations, EEG/ERP",
      "amplitudes (uV/mV), difference scores, proportions or percentages of",
      "trials, physical measurements, money, scores already averaged across",
      "multiple scale items.",
      "Answer with a JSON object {\"integer_data\": true} or",
      "{\"integer_data\": false}. If you cannot tell, answer true",
      "(only suppress a flag when you are confident the data are non-integer).",
      sep = " "
    )
    text <- sprintf("TARGET MEAN: %s\n\nSENTENCE: %s", reported, sentences)
    res <- tryCatch(
      llm(text = text, system_prompt = sys, params = list(seed = 8675309)),
      error = \(e) NULL
    )
    if (is.null(res)) return(rep(FALSE, length(reported)))
    res <- json_expand(res, suffix = c("", ".llm"))
    int <- res$integer_data
    # suppress only when the LLM is confident the data are NOT integers
    !is.na(int) & int %in% c(FALSE, "false", "FALSE", 0)
  }

  # extraction patterns ----
  # group 1: currency prefix; group 2: the mean; group 3: trailing unit;
  # group 4: trailing age unit
  mean_rx <- paste0(
    "\\b(?:M|Mage|[Mm]ean(?:\\s+ages?)?)\\s*[=:]\\s*",
    "([€$£]?)\\s*[-−]?\\s*(\\d+\\.\\d+)\\s*",
    "(%|ms\\b|msec\\b|s\\b|sec\\b|seconds\\b|min\\b|minutes\\b|",
    "cm\\b|mm\\b|kg\\b|[Hh]z\\b|°|[µu]V\\b|mV\\b)?\\s*",
    "(years?\\b|months?\\b|days?\\b)?"
  )

  # sentence-initial sample sizes are spelled out ("Sixty participants");
  # convert number words to digits before n extraction
  words_to_digits <- function(s) {
    tens <- c(twenty = 20, thirty = 30, forty = 40, fifty = 50,
              sixty = 60, seventy = 70, eighty = 80, ninety = 90)
    ones <- c(one = 1, two = 2, three = 3, four = 4, five = 5,
              six = 6, seven = 7, eight = 8, nine = 9)
    teens <- c(ten = 10, eleven = 11, twelve = 12, thirteen = 13,
               fourteen = 14, fifteen = 15, sixteen = 16,
               seventeen = 17, eighteen = 18, nineteen = 19)
    for (w in names(tens)) {
      for (o in names(ones)) {
        s <- gsub(paste0("\\b", w, "-", o, "\\b"), tens[[w]] + ones[[o]],
                  s, ignore.case = TRUE)
      }
      s <- gsub(paste0("\\b", w, "\\b"), tens[[w]], s, ignore.case = TRUE)
    }
    for (w in names(teens)) {
      s <- gsub(paste0("\\b", w, "\\b"), teens[[w]], s, ignore.case = TRUE)
    }
    s
  }

  # sample sizes: "n = 24", "24 (healthy adult) participants",
  # "sample of 24", "24 in Experiment 1b"
  # lookbehinds stop "Study 2 participants" being read as n = 2
  no_label <- paste0("(?<![Ss]tudy )(?<![Ee]xperiment )(?<![Ww]ave )",
                     "(?<![Pp]hase )(?<![Ss]ession )(?<![Tt]ime )",
                     "(?<![Gg]rade )(?<![Vv]ersion )")
  n_rxs <- c(
    "\\b[Nn]s?\\s*=\\s*(\\d{1,3}(?:,\\d{3})*)\\b",
    paste0("\\b", no_label, "(\\d{1,3}(?:,\\d{3})*)\\s+(?:[A-Za-z-]+\\s+){0,2}",
           "(?:participants?|subjects?|respondents?|students?|",
           "undergraduates?|adults?|children|individuals|people|",
           "volunteers?|patients?|infants?|toddlers?|couples?|dyads?|",
           "raters?|observers?|mice|rats)\\b"),
    "\\bsamples?\\s+of\\s+(\\d{1,3}(?:,\\d{3})*)\\b",
    "\\b(\\d{1,3}(?:,\\d{3})*)\\s+in\\s+(?:Experiments?|Study|Studies|each)\\b"
  )

  # counts preceded by exclusion/attrition language are not sample sizes
  bad_n_context <- paste0("exclu|remov|withdraw|dropp|discard|miss|unknown|",
                          "unavailable|fail|did not|duplicate|attrition")

  # extract candidate ns from a sentence, dropping exclusion counts
  # ("we excluded 3 participants", "13 participants were dropped") and
  # implausibly small values (parsing artifacts like '1 participant who...')
  extract_ns <- function(s) {
    out <- numeric(0)
    for (rx in n_rxs) {
      nm <- gregexpr(rx, s, perl = TRUE)[[1]]
      if (nm[[1]] == -1) next
      starts <- as.integer(nm)
      ends <- starts + attr(nm, "match.length") - 1
      matches <- regmatches(s, list(nm))[[1]]
      vals <- sub(rx, "\\1", matches, perl = TRUE) |>
        gsub(",", "", x = _) |>
        as.numeric()
      pre <- substr(s, pmax(1, starts - 40), pmax(1, starts - 1))
      post <- substr(s, ends + 1, ends + 45)
      # post check needs verb adjacency: "13 participants were dropped",
      # but not "final sample of 35 after exclusions"
      post_bad_rx <- paste0("^[^.;]{0,25}(?:(?:were|was)\\s+(?:dropp|exclud|",
                            "remov|withdraw|discard|lost)|did not|failed)")
      bad <- grepl(bad_n_context, pre, ignore.case = TRUE) |
        grepl(post_bad_rx, post, ignore.case = TRUE)
      out <- c(out, vals[!bad])
    }
    unique(out[out >= 5])
  }

  # latency/duration language implies non-integer data for ALL means in
  # the sentence (these often lack a trailing unit on the mean itself)
  latency_rx <- paste0(
    "\\b(?:reaction[- ]time|response[- ]time|latenc\\w+|duration\\w*|",
    "fixation|looking[- ]time|millisecond\\w*|\\bRT\\b)"
  )

  # find sentences with a mean ----
  # (the n may be in the same sentence, the same paragraph, or recoverable
  # from test-statistic degrees of freedom, so don't require it here)
  sentences <- paper |>
    text_search(mean_rx, return = "sentence", perl = TRUE, ignore.case = FALSE) |>
    text_search(latency_rx, exclude = TRUE, perl = TRUE)

  st <- data.frame()
  if (nrow(sentences) > 0) {
    # paragraph context for the sample-size fallback
    sentences <- text_expand(sentences, paper, expand_to = "paragraph")

    # test statistics in those paragraphs -> df-derived N candidates:
    # independent t has df = n1 + n2 - 2, paired/one-sample df = n - 1,
    # F(1, df) behaves like t; skip fractional (Welch-corrected) df
    all_text <- text_search(paper)
    par_key <- function(df) paste(df$paper_id, df$section_id, df$paragraph_id)
    par_rows <- all_text[par_key(all_text) %in% par_key(sentences), ]
    st <- tryCatch(
      suppressMessages(stats(par_rows)),
      error = \(e) data.frame()
    )
    if (nrow(st) > 0) {
      # t tests only: F(1, df) is usually a repeated-measures effect whose
      # condition means are continuous (d', difference scores), not GRIM-able
      usable <- !is.na(st$df2) & st$df2 == round(st$df2) &
        st$test_type == "t"
      st <- st[usable, , drop = FALSE]
    }
  }

  # extract mean/n pairs per sentence ----
  rows <- lapply(seq_len(nrow(sentences)), \(i) {
    s <- sentences$text[[i]]

    m <- gregexpr(mean_rx, s, perl = TRUE)[[1]]
    if (m[[1]] == -1) return(NULL)
    starts <- as.integer(m)
    mean_match <- regmatches(s, list(m))[[1]]
    currency <- sub(mean_rx, "\\1", mean_match, perl = TRUE)
    mean_str <- sub(mean_rx, "\\2", mean_match, perl = TRUE)
    unit     <- sub(mean_rx, "\\3", mean_match, perl = TRUE)
    age_unit <- sub(mean_rx, "\\4", mean_match, perl = TRUE)

    # age means: children's ages are usually non-integer underlying
    # (years + months), so drop age means under 18; ages in months or
    # days (infant studies) are usually computed from birth dates
    pre <- substr(s, pmax(1, starts - 16), pmax(1, starts - 1))
    is_age <- grepl("age|year", pre, ignore.case = TRUE) |
      grepl("age", mean_match, ignore.case = TRUE) |
      grepl("^year", age_unit)
    child_age <- is_age & as.numeric(mean_str) < 18
    nonyear_age <- grepl("^(month|day)", age_unit)

    # means followed by a standard error are usually repeated-measures
    # condition means of continuous outcomes; means with > 2 decimals are
    # rarely means of integer data (e.g., bootstrap distributions)
    ends <- starts + nchar(mean_match) - 1
    post_mean <- substr(s, ends + 1, ends + 15)
    se_paired <- grepl("^\\s*[,;(]?\\s*SEM?\\s*[=:]", post_mean)
    too_precise <- nchar(sub("^\\d+\\.", "", mean_str)) > 2
    # 'M = 1.15 x 10^6' style scientific notation: the regex only captured
    # the mantissa; psychophysics units imply continuous data
    sci_or_unit <- grepl("^\\s*[x\\u00d7]\\s*10|^\\s*(arcmin|deg|dB)\\b",
                         post_mean)

    # drop means with currency prefixes or non-integer units
    keep <- currency == "" & unit == "" & !child_age & !nonyear_age &
      !se_paired & !too_precise & !sci_or_unit
    mean_match <- trimws(mean_match[keep])
    mean_str <- mean_str[keep]
    if (length(mean_str) == 0) return(NULL)

    # df-derived candidates (df + 1, df + 2) -- unless two stated ns
    # already sum to df + 2, which confirms an independent two-group
    # design where the group ns are the correct pairing
    df_candidates <- function(dfs, ns) {
      out <- numeric(0)
      for (d in dfs) {
        explained <- length(ns) >= 2 &&
          any(utils::combn(ns, 2, sum) == d + 2)
        if (!explained) out <- c(out, d + 1, d + 2)
      }
      out
    }

    # candidate ns: same sentence first (regex ns + df-derived ns),
    # then fall back to the whole paragraph; n_source records the pairing
    # confidence (sentence > df > paragraph)
    row_i <- sentences[i, ]
    sent_ns <- extract_ns(words_to_digits(s))
    df_sent <- st$df2[st$paper_id %in% row_i$paper_id &
                        st$text_id %in% row_i$text_id]
    n_vals <- unique(c(sent_ns, df_candidates(df_sent, sent_ns)))
    n_source <- if (length(sent_ns)) "sentence" else "df"
    if (length(n_vals) == 0) {
      # paragraph fallback is mispairing-prone, so only use it when the
      # paragraph offers exactly ONE candidate source (one stated n, or
      # one t test) -- ambiguous paragraphs are skipped, not guessed
      par_ns <- extract_ns(words_to_digits(row_i$expanded %||% s))
      df_par <- st$df2[st$paper_id %in% row_i$paper_id &
                         st$section_id %in% row_i$section_id &
                         st$paragraph_id %in% row_i$paragraph_id] |>
        unique()
      if (length(par_ns) + length(df_par) == 1) {
        n_vals <- unique(c(par_ns, df_candidates(df_par, par_ns)))
        n_source <- "paragraph"
      }
    }
    n_vals <- n_vals[n_vals >= 5]
    if (length(n_vals) == 0) return(NULL)

    data.frame(
      sentence_row = i,
      reported = mean_match,
      mean_str = mean_str,
      mean = as.numeric(mean_str),
      digits = nchar(sub("^\\d+\\.", "", mean_str)),
      n_source = n_source,
      ns = I(rep(list(n_vals), length(mean_match)))
    )
  }) |>
    do.call(rbind, args = _)

  # run GRIM ----
  if (is.null(rows) || nrow(rows) == 0) {
    table <- data.frame(
      text = character(0), reported = character(0),
      mean = numeric(0), n = character(0), n_source = character(0),
      consistent = logical(0), min_items = integer(0),
      llm_noninteger = logical(0), paper_id = character(0)
    )
  } else {
    res <- lapply(seq_len(nrow(rows)), \(i) {
      ns <- rows$ns[[i]]
      checkable <- ns[ns < 10^rows$digits[[i]]]
      if (length(checkable) == 0) {
        return(list(consistent = NA, min_items = NA_integer_, n = ns))
      }
      # consistent if the mean passes GRIM for ANY n in the sentence
      cons <- sapply(checkable, \(n) {
        grim_consistent(rows$mean_str[[i]], n)
      })
      mi <- sapply(checkable, \(n) {
        min_items(rows$mean_str[[i]], n)
      })
      list(
        consistent = any(cons %in% TRUE),
        min_items = if (all(is.na(mi))) NA_integer_ else min(mi, na.rm = TRUE),
        n = checkable
      )
    })

    # total-n dispersal: a sentence with exactly two means and one
    # candidate n often reports group means with only the total N;
    # rescue the pair if any plausible split makes both consistent
    for (sr in unique(rows$sentence_row)) {
      idx <- which(rows$sentence_row == sr)
      if (length(idx) != 2) next
      ns0 <- rows$ns[[idx[1]]]
      if (length(ns0) != 1) next
      if (!all(sapply(idx, \(j) res[[j]]$consistent %in% FALSE))) next
      split_ok <- total_n_consistent(rows$mean_str[[idx[1]]],
                                     rows$mean_str[[idx[2]]], ns0)
      if (isTRUE(split_ok) || is.na(split_ok)) {
        for (j in idx) res[[j]]$consistent <- TRUE
      }
    }

    sent_cols <- sentences[rows$sentence_row,
                           c("text", "text_id", "section_id", "header",
                             "section_type", "paper_id")]
    table <- data.frame(
      sent_cols,
      reported = rows$reported,
      mean = rows$mean,
      n = sapply(res, \(x) paste(x$n, collapse = ", ")),
      n_source = rows$n_source,
      consistent = sapply(res, `[[`, "consistent"),
      min_items = sapply(res, `[[`, "min_items")
    )
    # drop means where no n was checkable (test has no power)
    table <- table[!is.na(table$consistent), , drop = FALSE]
    rownames(table) <- NULL
  }

  # optional LLM false-positive suppression ----
  # only runs on already-flagged means; demotes non-integer data to "na"
  table$llm_noninteger <- NA
  if (isTRUE(use_llm) && llm_use() && nrow(table) > 0) {
    flag_idx <- which(table$consistent %in% FALSE)
    if (length(flag_idx) > 0) {
      noninteger <- llm_is_noninteger(table$reported[flag_idx],
                                      table$text[flag_idx])
      table$llm_noninteger[flag_idx] <- noninteger
      # suppress: NA means "GRIM not applicable (non-integer data)"
      table$consistent[flag_idx[noninteger]] <- NA
    }
  }

  inconsistent <- table[table$consistent %in% FALSE, , drop = FALSE]

  # summary_table ----
  checked_summary <- dplyr::count(table, paper_id, name = "grim_checked")
  inconsistent_summary <- dplyr::count(inconsistent, paper_id,
                                       name = "grim_inconsistent")
  summary_table <- dplyr::full_join(checked_summary, inconsistent_summary,
                                    by = "paper_id")
  summary_table$grim_inconsistent[is.na(summary_table$grim_inconsistent)] <- 0

  n_suppressed <- sum(table$llm_noninteger %in% TRUE)
  llm_note <- if (n_suppressed > 0) {
    sprintf(
      " An LLM judged %d further flagged mean%s to be based on non-integer data (e.g., reaction times, amplitudes), so %s were not reported as GRIM-inconsistent.",
      n_suppressed, plural(n_suppressed),
      ifelse(n_suppressed == 1, "it", "they")
    )
  } else ""

  # traffic light ----
  if (nrow(table) == 0) {
    tl <- "na"
  } else if (nrow(inconsistent) == 0) {
    tl <- "green"
  } else {
    tl <- "yellow"
  }

  # report / summary_text ----
  if (tl == "na") {
    summary_text <- "We found no means reported with a checkable sample size (GRIM requires a mean and n in the same sentence, with n smaller than 10^decimals)."
    report <- summary_text
  } else if (tl == "green") {
    summary_text <- paste0(sprintf(
      "All %d mean%s reported with a checkable sample size passed the GRIM test.",
      nrow(table), plural(nrow(table))
    ), llm_note)
    report <- summary_text
  } else {
    summary_text <- paste0(sprintf(
      "We found %d mean%s (out of %d checkable) that may be inconsistent with the reported sample size (GRIM test); check whether these are based on integer data and verify the sample size.",
      nrow(inconsistent), plural(nrow(inconsistent)), nrow(table)
    ), llm_note)

    report_text <- c(
      "The GRIM test (Brown & Heathers, 2017) checks whether a reported mean is mathematically possible: a mean of n integer values (e.g., Likert responses, counts, age in years) must equal an integer divided by n. The means below failed this test for every sample size reported in the same sentence.",
      "A flagged mean is not necessarily an error. It may be based on (1) a multi-item scale (see the 'items' column for the smallest number of scale items that would make the mean possible), (2) a subsample with a different n than reported in the sentence, or (3) non-integer data. Please verify the sample size, the number of scale items, and the mean."
    )

    guidance <- c(
      "GRIM only applies to means of integer data, and only has diagnostic value when n < 10^decimals (e.g., n < 100 for two-decimal means). For the original method and validation, see:",
      format_ref(grim_ref),
      "For an R implementation with extensions (GRIMMER, DEBIT), see the [scrutiny](https://lhdjung.github.io/scrutiny/) package."
    )

    cols <- c("reported", "n", "n_source", "min_items", "text")
    report_table <- inconsistent[, cols, drop = FALSE]
    colnames(report_table) <- c("Mean", "N", "N Source", "Items", "Sentence")

    report <- c(
      report_text,
      scroll_table(report_table, colwidths = c(.12, .08, .08, .72)),
      collapse_section(guidance)
    )
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

grim_ref <- bibentry(
  bibtype = "Article",
  title = "The GRIM Test: A Simple Technique Detects Numerous Anomalies in the Reporting of Results in Psychology",
  author = c(
    person("N. J. L.", "Brown"),
    person("J. A. J.", "Heathers")
  ),
  journal = "Social Psychological and Personality Science",
  year = 2017,
  volume = 8,
  number = 4,
  pages = "363--369",
  doi = "10.1177/1948550616673876"
)
