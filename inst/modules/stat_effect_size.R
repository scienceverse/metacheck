#' Effect Sizes in t-tests and F-tests
#'
#' @description
#' The Effect Size module checks if effect sizes are correctly reported in t-tests and F-tests.
#'
#' @details
#' The Effect Size check searches for regular expressions that match typical ways in which effect sizes are reported. It subsequently checks different ways in which Cohen's d, g, ηp2, and ωp2 can be computed against the reported value. If effects are missing, or might be incorrect, you the module provides a warning. The module was validated on APA reported statistical tests, and might miss effect sizes that were reported in other reporting styles. It was validated by the Metacheck team on papers published in Psychological Science.
#'
#' <validation>In a sample of 161 papers with 1469 tests, this module correctly detected 1106 reported effect sizes (true positives) and correctly identified 295 cases where no effect size was present (true negatives). However, it missed 23 that were reported (false negatives), and incorrectly identified 45 effect sizes when none were reported (false positives). Among all instances detected by the module, 96% were true cases (positive predictive value). In a validation against 221 reported Cohen's d effect sizes, it correctly indicated coherence in 218 cases (99%). In a validation against 485 partial eta-squared effect sizes, it correctly indicated coherence in 480 (99%) </validation>
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#' @import tidyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
stat_effect_size <- function(paper) {
  # paper <- psychsci[[9]] # to test

  parse_t_stats <- function(test_text) {
    if (is.na(test_text) || !nzchar(test_text)) {
      return(data.frame(match_text = character(0), t_value = numeric(0), df = numeric(0)))
    }

    patt <- "\\bt\\s*\\(\\s*([0-9]+(?:\\.[0-9]+)?)\\s*\\)\\s*=\\s*([-+]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][-+]?\\d+)?)"
    m <- gregexpr(patt, test_text, perl = TRUE)
    hits <- regmatches(test_text, m)[[1]]
    if (length(hits) == 0) {
      return(data.frame(match_text = character(0), t_value = numeric(0), df = numeric(0)))
    }

    vals <- lapply(hits, function(x) {
      mm <- regexec(patt, x, perl = TRUE)
      g <- regmatches(x, mm)[[1]]
      data.frame(
        match_text = g[1],
        t_value = as.numeric(g[3]),
        df = as.numeric(g[2])
      )
    })

    dplyr::bind_rows(vals)
  }

  parse_d_stats <- function(es_text) {
    if (is.na(es_text) || !nzchar(es_text)) {
      return(data.frame(label = character(0), d_value = numeric(0), d_text = character(0)))
    }

    patt <- paste0(
      "(?i)\\b",
      "(cohen(?:'|’)?s\\s+d\\s*z|cohen(?:'|’)?s\\s+d|d\\s*z|d|ds)",
      "\\b\\s*[=≈<>≤≥]{1,3}\\s*",
      "([-+]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][-+]?\\d+)?)"
    )
    m <- gregexpr(patt, es_text, perl = TRUE)
    hits <- regmatches(es_text, m)[[1]]
    if (length(hits) == 0) {
      return(data.frame(label = character(0), d_value = numeric(0), d_text = character(0)))
    }

    vals <- lapply(hits, function(x) {
      mm <- regexec(patt, x, perl = TRUE)
      g <- regmatches(x, mm)[[1]]
      data.frame(
        label = tolower(trimws(g[2])),
        d_value = as.numeric(g[3]),
        d_text = x
      )
    })

    out <- dplyr::bind_rows(vals)
    out$label <- gsub("\\s+", " ", out$label)
    out
  }

  parse_f_stats <- function(test_text) {
    if (is.na(test_text) || !nzchar(test_text)) {
      return(data.frame(match_text = character(0), f_value = numeric(0), df1 = numeric(0), df2 = numeric(0)))
    }

    patt <- "\\bF\\s*\\(\\s*([0-9]+(?:\\.[0-9]+)?)\\s*,\\s*([0-9]+(?:\\.[0-9]+)?)\\s*\\)\\s*=\\s*([-+]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][-+]?\\d+)?)"
    m <- gregexpr(patt, test_text, perl = TRUE)
    hits <- regmatches(test_text, m)[[1]]
    if (length(hits) == 0) {
      return(data.frame(match_text = character(0), f_value = numeric(0), df1 = numeric(0), df2 = numeric(0)))
    }

    vals <- lapply(hits, function(x) {
      mm <- regexec(patt, x, perl = TRUE)
      g <- regmatches(x, mm)[[1]]
      data.frame(
        match_text = g[1],
        f_value = as.numeric(g[4]),
        df1 = as.numeric(g[2]),
        df2 = as.numeric(g[3])
      )
    })

    dplyr::bind_rows(vals)
  }

  parse_eta_stats <- function(es_text) {
    if (is.na(es_text) || !nzchar(es_text)) {
      return(data.frame(label = character(0), eta_value = numeric(0), eta_text = character(0)))
    }

    parts <- trimws(strsplit(es_text, "\\s*;\\s*", perl = TRUE)[[1]])
    if (length(parts) == 0) {
      return(data.frame(label = character(0), eta_value = numeric(0), eta_text = character(0)))
    }

    patt <- paste0(
      "(?i)^\\s*",
      "([^=≈<>≤≥;]+?)",
      "\\s*[=≈<>≤≥]{1,3}\\s*",
      "([-+]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][-+]?\\d+)?)",
      "\\s*$"
    )

    vals <- lapply(parts, function(x) {
      mm <- regexec(patt, x, perl = TRUE)
      g <- regmatches(x, mm)[[1]]
      if (length(g) == 0) {
        return(NULL)
      }
      raw_label <- tolower(trimws(g[2]))
      raw_label <- gsub("\\s+", "", raw_label)
      raw_label <- gsub("²", "2", raw_label)
      label <- if (grepl("^ξ|^bf", raw_label)) {
        "non_checkable"
      } else if (grepl("^f2?$", raw_label) || grepl("cohen", raw_label)) {
        "cohens_f"
      } else if (grepl("ω", raw_label) || grepl("omega", raw_label)) {
        # Omega squared (partial or not) depends on the total sample size N which cannot yet be recovered. 
        "non_checkable"
      } else if ((grepl("η", raw_label) || grepl("eta", raw_label)) &&
                 (grepl("partial", raw_label) || grepl("p", raw_label))) {
        "partial_eta_squared"
      } else {
        "eta_squared"
      }
      data.frame(
        label = label,
        eta_value = as.numeric(g[3]),
        eta_text = x
      )
    })

    dplyr::bind_rows(vals)
  }


  # tol = 0.01 rationale: for 2dp reporting, rounding of reported d contributes ±0.005 and
  # propagated rounding of t (dd/dt = d/t) adds at most ~0.003 at small n — combined max ~0.008.
  classify_d_coherence <- function(test, test_text, es_text, tol = 0.01) {
    out <- list(
      d_reported = NA_character_,
      d_reported_text = NA_character_,
      t_value = NA_character_,
      df = NA_character_,
      d_implied_paired_dz = NA_character_,
      d_implied_paired_drm_r05 = NA_character_,
      d_implied_indep_equal_n = NA_character_,
      d_implied_indep_unequal_min = NA_character_,
      d_implied_indep_unequal_max = NA_character_,
      d_implied_n = NA_character_,
      d_coherence = NA_character_,
      d_coherence_assumption = NA_character_,
      d_coherence_note = NA_character_
    )

    if (is.na(test) || test != "t-test") {
      return(out)
    }

    t_stats <- parse_t_stats(test_text)

    if (nrow(t_stats) == 0) {
      out$d_coherence <- "indeterminate"
      out$d_coherence_assumption <- "none"
      out$d_coherence_note <- "No parseable t(df)=value found."
      return(out)
    }

    d_stats <- parse_d_stats(es_text)
    if (nrow(d_stats) == 0) {
      out$d_coherence <- "indeterminate"
      out$d_coherence_assumption <- "none"
      out$d_coherence_note <- "No parseable d effect size found."
      return(out)
    }

    t_value <- t_stats$t_value[1]
    df <- t_stats$df[1]
    out$t_value <- as.character(t_value)
    out$df <- as.character(df)

    if (is.na(df)) {
      out$d_coherence <- "indeterminate"
      out$d_coherence_assumption <- "none"
      out$d_coherence_note <- "Missing df for t-test."
      return(out)
    }

    if (abs(df - round(df)) > 1e-8) {
      out$d_coherence <- "indeterminate"
      out$d_coherence_assumption <- "none"
      out$d_coherence_note <- "Non-integer df indicates Welch's t-test (unequal variances); sample sizes cannot be determined."
      return(out)
    }

    abs_t <- abs(t_value)

    # Paired-samples dz
    d_paired_dz <- abs_t / sqrt(df + 1)

    # Paired-samples drm assuming r = 0.5
    r_assumed <- 0.5
    d_paired_drm <- d_paired_dz / sqrt(1 - r_assumed)

    # Independent-samples equal-n
    d_equal <- 2 * abs_t / sqrt(df + 2)

    out$d_implied_paired_dz <- as.character(d_paired_dz)
    out$d_implied_paired_drm_r05 <- as.character(d_paired_drm)
    out$d_implied_indep_equal_n <- as.character(d_equal)

    n_total <- df + 2
    use_unequal <- is.finite(n_total) && abs(n_total - round(n_total)) < 1e-8 && n_total >= 4
    d_unequal_min <- NA_real_
    d_unequal_max <- NA_real_
    if (use_unequal) {
      n_total <- as.integer(round(n_total))
      n1 <- 2:(n_total - 2)
      n2 <- n_total - n1
      d_vals <- abs_t * sqrt(1 / n1 + 1 / n2)
      d_unequal_min <- min(d_vals)
      d_unequal_max <- max(d_vals)
      out$d_implied_indep_unequal_min <- as.character(d_unequal_min)
      out$d_implied_indep_unequal_max <- as.character(d_unequal_max)
    }

    d_stats$abs_d <- abs(d_stats$d_value)
    out$d_reported <- as.character(d_stats$d_value[1])
    out$d_reported_text <- d_stats$d_text[1]

    paired_match <- any(abs(d_stats$abs_d - d_paired_dz) <= tol)
    equal_match <- any(abs(d_stats$abs_d - d_equal) <= tol)
    unequal_match <- FALSE
    if (use_unequal) {
      unequal_match <- any(
        d_stats$abs_d >= (d_unequal_min - tol) &
          d_stats$abs_d <= (d_unequal_max + tol)
      )
    }

    if (paired_match) {
      # Within-subjects design: n = df + 1. This is obvious from the df, so it
      # is recorded but not spelled out in the note.
      out$d_coherence <- "match_under_assumptions"
      out$d_coherence_assumption <- "paired_dz"
      out$d_implied_n <- sprintf("n = %d", as.integer(df + 1))
      out$d_coherence_note <- "Match under paired-samples dz assumption."
    } else if (equal_match) {
      # Two equal-sized independent groups: N = df + 2, so n1 = n2 = (df + 2) / 2.
      out$d_coherence <- "match_under_assumptions"
      out$d_coherence_assumption <- "independent_equal_n"
      n_total_eq <- df + 2
      n_each <- n_total_eq / 2
      out$d_implied_n <- sprintf("n1 = n2 = %s, N = %s", format(n_each), format(n_total_eq))
      out$d_coherence_note <- sprintf(
        "Match under independent-samples equal-n assumption (n1 = n2 = %s, N = %s).",
        format(n_each), format(n_total_eq)
      )
    } else if (unequal_match) {
      # Report the single group-size split whose implied d is closest to the
      # reported d, so users can manually check the assumed sample sizes.
      out$d_coherence <- "match_under_assumptions"
      out$d_coherence_assumption <- "independent_unequal_n_range"
      matched_d <- d_stats$abs_d[
        d_stats$abs_d >= (d_unequal_min - tol) & d_stats$abs_d <= (d_unequal_max + tol)
      ][1]
      best <- which.min(abs(d_vals - matched_d))
      out$d_implied_n <- sprintf("n1 = %d, n2 = %d, N = %d", n1[best], n2[best], n_total)
      out$d_coherence_note <- sprintf(
        "Match under independent-samples unequal-n range assumption (closest split n1 = %d, n2 = %d, N = %d).",
        n1[best], n2[best], n_total
      )
    } else {
      out$d_coherence <- "no_match"
      out$d_coherence_assumption <- "none"
      out$d_coherence_note <- paste0(
        "No match under tested assumptions (paired dz, independent equal-n, independent unequal-n range). ",
        "Tolerance = ", tol, ". A no-match can occur when fewer than 2 decimal places are reported."
      )
    }

    out
  }

  # tol = 0.01 rationale: for 2dp reporting, rounding of reported ηp²/ωp² contributes ±0.005 and
  # propagated rounding of F (dη/dF = df1·df2/(df1·F+df2)²) adds at most ~0.001 — combined max ~0.006.
  classify_f_coherence <- function(test, test_text, es_text, tol = 0.01) {
    out <- list(
      f_reported = NA_character_,
      f_reported_text = NA_character_,
      df1 = NA_character_,
      df2 = NA_character_,
      eta_implied_partial = NA_character_,
      omega_implied_partial = NA_character_,
      eta_coherence = NA_character_,
      eta_coherence_assumption = NA_character_,
      eta_coherence_note = NA_character_
    )

    if (is.na(test) || test != "F-test") {
      return(out)
    }

    f_stats <- parse_f_stats(test_text)
    if (nrow(f_stats) == 0) {
      out$eta_coherence <- "indeterminate"
      out$eta_coherence_assumption <- "none"
      out$eta_coherence_note <- "No parseable F(df1,df2)=value found."
      return(out)
    }

    f_value <- f_stats$f_value[1]
    df1 <- f_stats$df1[1]
    df2 <- f_stats$df2[1]
    out$f_reported <- as.character(f_value)
    out$f_reported_text <- f_stats$match_text[1]
    out$df1 <- as.character(df1)
    out$df2 <- as.character(df2)

    if (is.na(df1) || is.na(df2)) {
      out$eta_coherence <- "indeterminate"
      out$eta_coherence_assumption <- "none"
      out$eta_coherence_note <- "Missing df1 or df2 for F-test."
      return(out)
    }

    eta_implied <- (df1 * abs(f_value)) / (df1 * abs(f_value) + df2)
    out$eta_implied_partial <- as.character(eta_implied)

    omega_implied <- (df1 * (abs(f_value) - 1)) / (df1 * abs(f_value) + df2 + 1)
    out$omega_implied_partial <- as.character(omega_implied)

    eta_stats <- parse_eta_stats(es_text)
    if (nrow(eta_stats) == 0) {
      out$eta_coherence <- "indeterminate"
      out$eta_coherence_assumption <- "none"
      out$eta_coherence_note <- "No parseable eta-squared effect size found."
      return(out)
    }

    # Remove ES types not verifiable from F and dfs:
    # Cohen's f (ambiguous eta basis), ξ, BF, and similar
    cohens_f_present <- any(eta_stats$label == "cohens_f")
    eta_stats <- eta_stats[!eta_stats$label %in% c("cohens_f", "non_checkable"), , drop = FALSE]
    if (nrow(eta_stats) == 0) {
      out$eta_coherence <- "indeterminate"
      out$eta_coherence_assumption <- "none"
      out$eta_coherence_note <- if (cohens_f_present) {
        "Cohen's f not checked: cannot determine whether based on eta squared or partial eta squared."
      } else {
        "Effect size reported but not verifiable from F and degrees of freedom alone."
      }
      return(out)
    }

    # If only eta-squared (not partial) is reported, coherence cannot be assessed
    if (all(eta_stats$label == "eta_squared")) {
      out$eta_coherence <- "indeterminate"
      out$eta_coherence_assumption <- "eta_squared"
      out$eta_coherence_note <- "Eta-squared reported; cannot be reconstructed from F and dfs in factorial or repeated-measures designs."
      return(out)
    }

    # Evaluate coherence for partial eta squared and/or partial omega squared
    eta_partial   <- eta_stats[eta_stats$label == "partial_eta_squared",  , drop = FALSE]
    omega_partial <- eta_stats[eta_stats$label == "partial_omega_squared", , drop = FALSE]

    if (nrow(eta_partial) == 0 && nrow(omega_partial) == 0) {
      out$eta_coherence <- "indeterminate"
      out$eta_coherence_assumption <- "none"
      out$eta_coherence_note <- "Only eta-squared (not partial) reported; cannot test coherence."
      return(out)
    }

    if (any(eta_stats$label == "partial_eta_squared") && any(eta_stats$label == "eta_squared")) {
      out$eta_coherence_note <- paste(
        "Both eta-squared and partial eta-squared reported;",
        "coherence evaluated only for partial eta-squared."
      )
    }

    # Check partial eta squared coherence
    if (nrow(eta_partial) > 0) {
      eta_partial$abs_eta <- abs(eta_partial$eta_value)
      if (any(abs(eta_partial$abs_eta - eta_implied) <= tol)) {
        out$eta_coherence <- "match_under_assumptions"
        out$eta_coherence_assumption <- "partial_eta_squared"
        out$eta_coherence_note <- "Match under partial eta-squared formula from F and dfs."
      } else {
        out$eta_coherence <- "no_match"
        out$eta_coherence_assumption <- "partial_eta_squared"
        out$eta_coherence_note <- paste0(
          "No match under partial eta-squared formula from F and dfs. ",
          "Tolerance = ", tol, ". A no-match can occur when fewer than 2 decimal places are reported."
        )
      }
    }

    # Check partial omega squared coherence
    # When omega_implied < 0, reporting convention is to report 0; match if reported value is 0.
    if (nrow(omega_partial) > 0) {
      omega_match <- if (omega_implied < 0) {
        any(abs(omega_partial$eta_value) <= tol)
      } else {
        any(abs(omega_partial$eta_value - omega_implied) <= tol)
      }
      if (omega_match) {
        out$eta_coherence <- "match_under_assumptions"
        out$eta_coherence_assumption <- "partial_omega_squared"
        out$eta_coherence_note <- "Match under partial omega-squared formula from F and dfs."
      } else {
        out$eta_coherence <- "no_match"
        out$eta_coherence_assumption <- "partial_omega_squared"
        out$eta_coherence_note <- paste0(
          "No match under partial omega-squared formula from F and dfs. ",
          "Tolerance = ", tol, ". A no-match can occur when fewer than 2 decimal places are reported."
        )
      }
    }

    out
  }

  # detect tests and effect sizes ----
  # extract_eq() already pulls every "name <comparator> value" out of the text,
  # so tests (t, F) and effect sizes are read from there rather than re-scanned.
  by <- c("paper_id", "section_id", "paragraph_id", "text_id")
  text_tbl <- paper |>
    text_search("[0-9]") |>
    dplyr::select(dplyr::all_of(c(by, "text")))

  eq <- extract_eq(paper)

  # label each equation row as a test, an effect size, or neither. The specific
  # effect-size type (d vs eta) is re-derived from the text by the coherence
  # checks below, so here we only need "test or effect size?". F-tests are only
  # recognised with a two-integer (df1, df2), matching the validated detection.
  label_lhs <- function(lhs, df) {
    if (grepl("^t$", lhs)) return("t-test")
    if (grepl("^F$", lhs)) {
      return(if (!is.na(df) && grepl("^\\(\\s*[0-9]+\\s*,\\s*[0-9]+\\s*\\)$", df)) {
        "F-test"
      } else {
        NA_character_
      })
    }
    raw <- gsub("[[:space:]]+", "", tolower(lhs))
    raw <- gsub("²", "2", raw) # squared symbol to 2
    is_es <-
      grepl("^(cohen.{0,3})?d(_?(z|s|av|rm))?$", raw) || # Cohen's d / dz / ds / dav / drm
      grepl("^(hedge.{0,3})?g$", raw) ||                 # Hedges' g
      grepl("^f2?$", raw) || grepl("cohen", raw) ||      # Cohen's f
      grepl("ω|omega", raw) ||                           # omega
      grepl("η|eta", raw) ||                             # eta family
      grepl("^ξ$|^β$|^b$|^r$", raw)                      # xi, beta, b, r
    if (is_es) "es" else NA_character_ # p, df, and anything else: ignore
  }

  # build one row per test from the equations in a single sentence.
  # When a sentence lists several tests and the same number of effect sizes
  # (e.g. "t(23)=2.7; t(23)=3.0, d=.56; d=.61"), they are paired by position.
  # Otherwise every test carries all the effect sizes in the sentence.
  build_rows <- function(rows) {
    is_test <- rows$kind %in% c("t-test", "F-test")
    is_es   <- !is.na(rows$kind) & !is_test
    tests   <- rows[is_test, , drop = FALSE]
    es      <- rows[is_es, , drop = FALSE]
    if (nrow(tests) == 0) return(NULL)

    df_part <- ifelse(is.na(tests$df), "", tests$df)
    test_text <- paste0(tests$lhs, df_part, " ", tests$comp, " ", tests$rhs)
    es_text   <- if (nrow(es) == 0) {
      character(0)
    } else {
      paste0(es$lhs, " ", es$comp, " ", es$rhs)
    }

    paired <- nrow(tests) > 1 && nrow(tests) == length(es_text)
    es_col <- if (paired) {
      es_text
    } else if (length(es_text) == 0) {
      rep(NA_character_, nrow(tests))
    } else {
      rep(paste(es_text, collapse = "; "), nrow(tests))
    }

    data.frame(
      paper_id  = tests$paper_id,
      text_id   = tests$text_id,
      test      = tests$kind,
      test_text = test_text,
      es        = es_col
    )
  }

  if (nrow(eq) == 0) {
    table <- data.frame()
  } else {
    eq$kind <- mapply(label_lhs, eq$lhs, eq$df, USE.NAMES = FALSE)
    # process each sentence in turn (split preserves within-sentence row order),
    # then restore the original paper/sentence order, which extract_eq() and
    # split() would otherwise reorder by paper_id.
    paper_order <- unique(text_tbl$paper_id)
    table <- eq |>
      split(~ paper_id + text_id, drop = TRUE) |>
      lapply(build_rows) |>
      dplyr::bind_rows() |>
      dplyr::left_join(text_tbl, by = c("paper_id", "text_id"))
    table <- table[order(match(table$paper_id, paper_order), table$text_id), , drop = FALSE]
    rownames(table) <- NULL
  }


  # handle no detected t-tests or F-tests ----
  # Return early so the coherence pipeline below (which assumes coherence
  # columns exist) never runs on an empty table.
  if (nrow(table) == 0) {
    msg <- "No t-tests or F-tests were detected."
    return(list(
      table = data.frame(),
      summary_table = data.frame(paper_id = paper$paper_id),
      na_replace = 0,
      traffic_light = "na",
      summary_text = msg,
      report = msg
    ))
  }

  # Coherence checks for effect sizes in t-tests and F-tests ----
  coherence <- lapply(seq_len(nrow(table)), function(i) {
    classify_d_coherence(
      test = table$test[i],
      test_text = table$test_text[i],
      es_text = table$es[i]
    )
  })

  f_coherence <- lapply(seq_len(nrow(table)), function(i) {
    classify_f_coherence(
      test = table$test[i],
      test_text = table$test_text[i],
      es_text = table$es[i]
    )
  })

  coherence <- dplyr::bind_rows(lapply(coherence, as.data.frame))
  f_coherence <- dplyr::bind_rows(lapply(f_coherence, as.data.frame))
  table <- dplyr::bind_cols(table, coherence, f_coherence)

  table_missing <- subset(table, is.na(es))

  ## summary table ----
  summary_table <- table |>
    dplyr::summarise(
      ttests_with_es = sum(test == "t-test" & !is.na(es)),
      ttests_without_es = sum(test == "t-test" & is.na(es)),
      Ftests_with_es = sum(test == "F-test" & !is.na(es)),
      Ftests_without_es = sum(test == "F-test" & is.na(es)),
      .by = "paper_id"
    )

  # coherence counts (needed for traffic light and report) ----
  count_coh <- function(col, value) {
    sum(vapply(strsplit(as.character(col), ";"),
               function(x) sum(trimws(x) == value), integer(1)), na.rm = TRUE)
  }
  t_rows <- table[table$test == "t-test" & !is.na(table$es), ]
  f_rows <- table[table$test == "F-test" & !is.na(table$es), ]
  t_nomatch <- count_coh(t_rows$d_coherence,   "no_match")
  f_nomatch <- count_coh(f_rows$eta_coherence, "no_match")
  has_nomatch <- (t_nomatch + f_nomatch) > 0

  # traffic light ----
  total_n <- nrow(table)
  noes_n <- is.na(table$es) |> sum()
  tl <- dplyr::case_when(
    total_n == 0      ~ "na",
    noes_n == total_n ~ "red",
    has_nomatch       ~ "red",
    noes_n > 0        ~ "yellow",
    .default          = "green"
  )

  # report / summary_text ----
  if (tl == "na") {
    report <- "No t-tests or F-tests were detected."
    summary_text <- report
  } else {

    t_match <- count_coh(t_rows$d_coherence,   "match_under_assumptions")
    t_indet <- count_coh(t_rows$d_coherence,   "indeterminate")
    f_match <- count_coh(f_rows$eta_coherence, "match_under_assumptions")
    f_indet <- count_coh(f_rows$eta_coherence, "indeterminate")

    format_coherence_text <- function(match, no_match, indet, test_label, es_label) {
      if (match + no_match + indet == 0) return(NULL)
      parts <- c(
        if (match    > 0) sprintf("%d match%s under assumptions", match,    if (match    == 1) "" else "es"),
        if (no_match > 0) sprintf("%d no match%s",               no_match, if (no_match == 1) "" else "es"),
        if (indet    > 0) sprintf("%d indeterminate case%s",      indet,    if (indet    == 1) "" else "s")
      )
      sprintf("For %s with a reported %s, coherence checks yielded %s.",
              test_label, es_label, paste(parts, collapse = ", "))
    }

    coherence_text   <- format_coherence_text(t_match, t_nomatch, t_indet,
                                              "t-tests", "d")
    f_coherence_text <- format_coherence_text(f_match, f_nomatch, f_indet,
                                              "F-tests", "eta-squared effect size")

    has_missing  <- nrow(table_missing) > 0
    has_nomatch  <- (t_nomatch + f_nomatch) > 0

    guidance <- c(
      "For metascientific articles demonstrating that effect sizes are often not reported:",
      "* Peng, C.-Y. J., Chen, L.-T., Chiang, H.-M., & Chiang, Y.-C. (2013). The Impact of APA and AERA Guidelines on Effect Size Reporting. Educational Psychology Review, 25(2), 157–209. doi:[10.1007/s10648-013-9218-2](https://doi.org/10.1007/s10648-013-9218-2).",
      "For educational material on reporting effect sizes:",
      "* [Guide to Effect Sizes and Confidence Intervals](https://matthewbjane.quarto.pub/guide-to-effect-sizes-and-confidence-intervals/)"
    )

    # select cols for the report table
    cols <- c(
      "text", "es", "test_text", "test",
      "d_coherence", "d_coherence_assumption", "d_coherence_note",
      "eta_coherence", "eta_coherence_assumption", "eta_coherence_note"
    )
    report_table <- table[, cols, drop = FALSE]
    colnames(report_table) <- c(
      "Sentence", "Effect Size", "Reported Test", "Test Type",
      "d Coherence", "d Assumption", "d Coherence Note",
      "eta Coherence", "eta Assumption", "eta Coherence Note"
    )
    detail_table <- scroll_table(report_table, maxrows = 5) |>
      collapse_section("All detected and assessed stats")

    if (!has_missing && !has_nomatch) {
      summary_text <- "All detected t-tests and F-tests had an effect size reported in the same sentence."
      report <- c(
        summary_text,
        coherence_text,
        f_coherence_text,
        detail_table
      )
    } else {
      summary_text <- if (has_missing) {
        sprintf(
          "We found %1$d t-test%2$s and/or F-test%2$s where effect sizes are not reported. Check these tests in the table below, and consider adding effect sizes",
          nrow(table_missing), ifelse(nrow(table_missing) == 1, "", "s")
        )
      } else {
        "All effect sizes were reported, but some appear inconsistent with the test statistic. This can be because the effect size is not clearly labeled (e.g., d, instead of d_rm), because the effect sizes is not reported with enough precisions (e.g., 0.3 instead of 0.32), or because the effect size is incorrectly reported."
      }

      report_text <- if (has_missing) {
        "We recommend checking the sentences below, and add any missing effect sizes."
      } else {
        "All tests had effect sizes, but some effect sizes do not match the reported test statistic."
      }

      report <- c(
        report_text,
        coherence_text,
        f_coherence_text,
        if (has_missing) scroll_table(table_missing$text),
        collapse_section(guidance),
        detail_table
      )
    }
  }

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}
