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
      } else if ((grepl("ω", raw_label) || grepl("omega", raw_label)) &&
                 (grepl("partial", raw_label) || grepl("p", raw_label))) {
        "partial_omega_squared"
      } else if (grepl("ω", raw_label) || grepl("omega", raw_label)) {
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

  expand_multi_f_rows <- function(df) {
    if (nrow(df) == 0) {
      return(df)
    }

    rows <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, , drop = FALSE]
      if (is.na(row$test) || row$test != "F-test") {
        return(row)
      }

      f_stats   <- parse_f_stats(row$test_text)
      eta_stats <- parse_eta_stats(row$es)

      n_f   <- nrow(f_stats)
      n_eta <- nrow(eta_stats)
      if (n_f > 1 && n_f == n_eta) {
        expanded <- lapply(seq_len(n_f), function(j) {
          new_row <- row
          new_row$test_text <- f_stats$match_text[j]
          new_row$es        <- eta_stats$eta_text[j]
          new_row
        })
        return(dplyr::bind_rows(expanded))
      }

      row
    })

    dplyr::bind_rows(rows)
  }

  expand_multi_t_rows <- function(df) {
    if (nrow(df) == 0) {
      return(df)
    }

    rows <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, , drop = FALSE]
      if (is.na(row$test) || row$test != "t-test") {
        return(row)
      }

      t_stats <- parse_t_stats(row$test_text)
      d_stats <- parse_d_stats(row$es)

      n_t <- nrow(t_stats)
      n_d <- nrow(d_stats)
      if (n_t > 1 && n_t == n_d) {
        expanded <- lapply(seq_len(n_t), function(j) {
          new_row <- row
          new_row$test_text <- t_stats$match_text[j]
          new_row$es <- d_stats$d_text[j]
          new_row
        })
        return(dplyr::bind_rows(expanded))
      }

      row
    })

    dplyr::bind_rows(rows)
  }


  # tol = 0.01 rationale: for 2dp reporting, rounding of reported d contributes ±0.005 and
  # propagated rounding of t (dd/dt = d/t) adds at most ~0.003 at small n — combined max ~0.008.
  classify_d_coherence <- function(test, test_text, es_text, sentence_text = NULL, tol = 0.01) {
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

    if (nrow(t_stats) > 1) {
      if (is.null(sentence_text)) {
        out$d_coherence <- "indeterminate"
        out$d_coherence_assumption <- "none"
        out$d_coherence_note <- "Multiple t(df)=value tests in one sentence."
        return(out)
      }

      # Each t-test owns the first d that appears after it, before the next t-test
      t_patt <- "\\bt\\s*\\(\\s*([0-9]+(?:\\.[0-9]+)?)\\s*\\)\\s*=\\s*([-+]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][-+]?\\d+)?)"
      d_patt <- paste0(
        "(?i)\\b(cohen(?:'|')?s\\s+d\\s*z|cohen(?:'|')?s\\s+d|d\\s*z|d|ds)\\b\\s*",
        "[=≈<>≤≥]{1,3}\\s*",
        "([-+]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][-+]?\\d+)?)"
      )
      t_m   <- gregexpr(t_patt, sentence_text, perl = TRUE)[[1]]
      d_m   <- gregexpr(d_patt, sentence_text, perl = TRUE)[[1]]
      t_pos <- as.integer(t_m)
      t_len <- attr(t_m, "match.length")
      d_pos <- if (d_m[1] == -1L) integer(0) else as.integer(d_m)
      d_len <- if (length(d_pos) == 0) integer(0) else attr(d_m, "match.length")

      n_t     <- length(t_pos)
      results <- vector("list", n_t)

      for (j in seq_len(n_t)) {
        window_start <- t_pos[j] + t_len[j]
        window_end   <- if (j < n_t) t_pos[j + 1L] - 1L else nchar(sentence_text)
        d_idx        <- which(d_pos >= window_start & d_pos <= window_end)

        if (length(d_idx) == 0) {
          results[[j]] <- out  # all-NA template; set coherence fields only
          results[[j]]$d_coherence           <- "indeterminate"
          results[[j]]$d_coherence_assumption <- "none"
          results[[j]]$d_coherence_note       <- "No d found after this t-test."
        } else {
          t_text_j     <- substr(sentence_text, t_pos[j],        t_pos[j]        + t_len[j]        - 1L)
          d_text_j     <- substr(sentence_text, d_pos[d_idx[1]], d_pos[d_idx[1]] + d_len[d_idx[1]] - 1L)
          results[[j]] <- classify_d_coherence(test = test, test_text = t_text_j, es_text = d_text_j, tol = tol)
        }
      }

      paste_num <- function(field) {
        vals <- sapply(results, function(r) r[[field]])
        vals <- vals[!is.na(vals)]
        if (length(vals) == 0) NA_character_ else paste(vals, collapse = "; ")
      }

      out$d_coherence                 <- paste(sapply(results, `[[`, "d_coherence"),            collapse = "; ")
      out$d_coherence_assumption      <- paste(sapply(results, `[[`, "d_coherence_assumption"), collapse = "; ")
      out$d_coherence_note            <- paste(sapply(results, `[[`, "d_coherence_note"),        collapse = "; ")
      out$d_reported                  <- paste_num("d_reported")
      out$d_reported_text             <- paste_num("d_reported_text")
      out$t_value                     <- paste_num("t_value")
      out$df                          <- paste_num("df")
      out$d_implied_paired_dz         <- paste_num("d_implied_paired_dz")
      out$d_implied_paired_drm_r05    <- paste_num("d_implied_paired_drm_r05")
      out$d_implied_indep_equal_n     <- paste_num("d_implied_indep_equal_n")
      out$d_implied_indep_unequal_min <- paste_num("d_implied_indep_unequal_min")
      out$d_implied_indep_unequal_max <- paste_num("d_implied_indep_unequal_max")
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
      out$d_coherence <- "match_under_assumptions"
      out$d_coherence_assumption <- "paired_dz"
      out$d_coherence_note <- "Match under paired-samples dz assumption."
    } else if (equal_match) {
      out$d_coherence <- "match_under_assumptions"
      out$d_coherence_assumption <- "independent_equal_n"
      out$d_coherence_note <- "Match under independent-samples equal-n assumption."
    } else if (unequal_match) {
      out$d_coherence <- "match_under_assumptions"
      out$d_coherence_assumption <- "independent_unequal_n_range"
      out$d_coherence_note <- "Match under independent-samples unequal-n range assumption."
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
  classify_f_coherence <- function(test, test_text, es_text, sentence_text = NULL, tol = 0.01) {
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

    if (nrow(f_stats) > 1) {
      if (is.null(sentence_text)) {
        out$eta_coherence <- "indeterminate"
        out$eta_coherence_assumption <- "none"
        out$eta_coherence_note <- "Multiple F(df1,df2)=value tests in one sentence."
        return(out)
      }

      f_patt <- "\\bF\\s*\\(\\s*([0-9]+(?:\\.[0-9]+)?)\\s*,\\s*([0-9]+(?:\\.[0-9]+)?)\\s*\\)\\s*=\\s*([-+]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][-+]?\\d+)?)"
      eta_potentials <- c(
        "η\\s*p*\\s*(2|²)", "(P|p)artial\\s+η\\s*(2|²)",
        "(O|o)mega\\s*(2|²)?", "ω\\s*(2|²)?",
        "(C|c)ohen(‘|’)?s\\s+f", "f\\s*(2|²)?",
        "(C|c)ohen(‘|’)?s\\s+d", "d", "β",
        "η\\s*G\\s*(2|²)", "R\\s*(2|²)", "R", "r",
        "ω\\s*p\\s*(2|²)?"
      )
      eta_patt <- paste0(
        "(?<![a-zA-Z0-9_])",
        "(", paste(eta_potentials, collapse = "|"), ")",
        "\\s*[=≈<>≤≥]{1,3}\\s*",
        "([-+]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][-+]?\\d+)?)"
      )

      f_m   <- gregexpr(f_patt,   sentence_text, perl = TRUE)[[1]]
      eta_m <- gregexpr(eta_patt, sentence_text, perl = TRUE)[[1]]

      f_pos   <- as.integer(f_m);   f_len   <- attr(f_m,   "match.length")
      eta_pos <- if (eta_m[1] == -1L) integer(0) else as.integer(eta_m)
      eta_len <- if (length(eta_pos) == 0) integer(0) else attr(eta_m, "match.length")

      n_f <- length(f_pos)
      results <- vector("list", n_f)

      for (j in seq_len(n_f)) {
        window_start <- f_pos[j] + f_len[j]
        window_end   <- if (j < n_f) f_pos[j + 1L] - 1L else nchar(sentence_text)
        eta_idx      <- which(eta_pos >= window_start & eta_pos <= window_end)

        f_text_j <- substr(sentence_text, f_pos[j], f_pos[j] + f_len[j] - 1L)
        if (length(eta_idx) == 0) {
          results[[j]] <- classify_f_coherence(test = test, test_text = f_text_j, es_text = NA_character_, tol = tol)
          results[[j]]$eta_coherence           <- "indeterminate"
          results[[j]]$eta_coherence_assumption <- "none"
          results[[j]]$eta_coherence_note       <- "No effect size found after this F-test."
        } else {
          eta_text_j <- substr(sentence_text, eta_pos[eta_idx[1]], eta_pos[eta_idx[1]] + eta_len[eta_idx[1]] - 1L)
          results[[j]] <- classify_f_coherence(test = test, test_text = f_text_j, es_text = eta_text_j, tol = tol)
        }
      }

      paste_num <- function(field) {
        vals <- sapply(results, function(r) r[[field]])
        vals <- vals[!is.na(vals)]
        if (length(vals) == 0) NA_character_ else paste(vals, collapse = "; ")
      }

      out$eta_coherence            <- paste(sapply(results, `[[`, "eta_coherence"),            collapse = "; ")
      out$eta_coherence_assumption <- paste(sapply(results, `[[`, "eta_coherence_assumption"), collapse = "; ")
      out$eta_coherence_note       <- paste(sapply(results, `[[`, "eta_coherence_note"),        collapse = "; ")
      out$f_reported               <- paste_num("f_reported")
      out$f_reported_text          <- paste_num("f_reported_text")
      out$df1                      <- paste_num("df1")
      out$df2                      <- paste_num("df2")
      out$eta_implied_partial      <- paste_num("eta_implied_partial")
      out$omega_implied_partial    <- paste_num("omega_implied_partial")
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

  # Narrow down to sentences that could contain stats
  stat_sentences <- paper |>
    text_search("=") |> # sentences with an equal sign
    text_search("[0-9]") # sentences with numbers

  # t-tests ----

  ## detect tests ----
  test_regex <- paste0(
    "\\bt\\s*", # word border and t
    "(\\(\\s*\\d+(\\.\\d+)?\\s*\\))?", # optional df
    "\\s*=\\s*", # comparator
    "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?" # number
  )
  text_found_test <- stat_sentences |>
    text_search(test_regex, perl = TRUE, ignore.case = FALSE) |>
    dplyr::select(paper_id, text, section_id, paragraph_id, text_id)

  ## detect relevant effect sizes ----
  potentials <- c(
    "cohen('|\u2019)?s\\s+d",
    "cohen('|\u2019)?s\\s+d\\s*z",
    "d", "d\\s*z", "ds",
    "hedges?('|\u2019)?s?\\s+g",
    "g", "b", "r", "β",
    "ξ"
  )

  es_regex <- paste0(
    "(?<![a-zA-Z0-9_])", # not preceded by ASCII word char (handles Unicode symbols like xi, beta)
    "(", paste(potentials, collapse = "|"), ")",
    "(?![a-zA-Z0-9_])", # not followed by ASCII word char
    "\\s*[=≈<>\u2264\u2265]{1,3}\\s*", # comparators
    "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?" # number
  )

  by <- c("paper_id", "section_id", "paragraph_id", "text_id")
  text_found_es <- text_search(text_found_test, es_regex,
    return = "match", perl = TRUE
  ) |>
    dplyr::summarise(
      es = paste(text, collapse = "; "),
      .by = dplyr::all_of(by)
    )

  ## add exact text ----
  test_match <- text_search(text_found_test, test_regex,
    return = "match",
    perl = TRUE, ignore.case = FALSE
  ) |>
    dplyr::summarise(
      test_text = paste(text, collapse = "; "),
      .by = dplyr::all_of(by)
    )
  t_table <- text_found_test |>
    dplyr::left_join(text_found_es, by = by) |>
    dplyr::left_join(test_match, by = by)
  t_table$test <- "t-test"

  # F-tests -----

  ## detect tests ----
  test_regex <- paste0(
    "\\bF\\s*", # word border and F
    "\\(\\s*\\d+\\s*,\\s*\\d+\\s*\\)", # df (must be 2 integers)
    "\\s*=\\s*", # comparator
    "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?" # number
  )

  # sentences with a relevant test
  text_found_test <- stat_sentences |>
    text_search(test_regex, perl = TRUE, ignore.case = FALSE) |>
    dplyr::select(paper_id, text, section_id, paragraph_id, text_id)

  ## detect relevant effect sizes ----
  potentials <- c(
    "(C|c)ohen('|\u2019)?s\\s+f",
    "f\\s*(2|²)?",
    "η\\s*p*\\s*(2|²)",
    "(P|p)artial\\s+η\\s*(2|²)",
    "(O|o)mega\\s*(2|²)?",
    "ω\\s*(2|²)?",
    "(C|c)ohen('|\u2019)?s\\s+d",
    "d",
    "β",
    "η\\s*G\\s*(2|²)",
    "R\\s*(2|²)",
    "R", "r",
    "ω\\s*p\\s*(2|²)?"
  )

  es_regex <- paste0(
    "(?<![a-zA-Z0-9_])", # not preceded by ASCII word char (handles Unicode symbols like η, ω, ξ)
    "(", paste(potentials, collapse = "|"), ")",
    "\\s*[=≈<>\u2264\u2265]{1,3}\\s*", # comparators
    "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?" # number
  )

  text_found_es <- text_search(text_found_test, es_regex,
    return = "match", perl = TRUE
  ) |>
    dplyr::summarise(
      es = paste(text, collapse = "; "),
      .by = dplyr::all_of(by)
    )

  ## add exact text ----
  test_match <- text_search(text_found_test, test_regex,
    return = "match",
    perl = TRUE, ignore.case = FALSE
  ) |>
    dplyr::summarise(
      test_text = paste(text, collapse = "; "),
      .by = dplyr::all_of(by)
    )
  f_table <- text_found_test |>
    dplyr::left_join(text_found_es, by = by) |>
    dplyr::left_join(test_match, by = by)
  f_table$test <- "F-test"

  # combine tests ----
  table <- dplyr::bind_rows(t_table, f_table)

  table <- expand_multi_t_rows(table)
  table <- expand_multi_f_rows(table)

  # Coherence checks for effect sizes in t-tests and F-tests ----
  coherence <- lapply(seq_len(nrow(table)), function(i) {
    classify_d_coherence(
      test = table$test[i],
      test_text = table$test_text[i],
      es_text = table$es[i],
      sentence_text = table$text[i]
    )
  })

  f_coherence <- lapply(seq_len(nrow(table)), function(i) {
    classify_f_coherence(
      test = table$test[i],
      test_text = table$test_text[i],
      es_text = table$es[i],
      sentence_text = table$text[i]
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
