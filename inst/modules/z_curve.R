#' Z-Curve Analysis
#'
#' @description
#' Estimates the expected discovery rate (EDR) and expected replication rate
#' (ERR) for a paper's focal statistical results using z-curve analysis.
#'
#' @details
#' The module extracts all p-values from the paper with `extract_p_values()`,
#' then isolates the sections of text that contain those p-values and sends
#' each section to an LLM. The LLM classifies each p-value as focal (directly
#' testing a claim in the abstract) or non-focal (e.g., manipulation checks,
#' secondary analyses). Only focal p-values are passed to
#' `zcurve::zcurve()` for analysis.
#'
#' Without an LLM, the module cannot classify p-values and returns `na`.
#'
#' Z-curve uses the distribution of significant z-scores (converted from
#' p-values) to estimate the EDR — the proportion of all tested hypotheses
#' that are true — and the ERR — the mean power of the studies selected
#' for significance. A large gap between EDR and ERR suggests publication bias
#' or selective reporting.
#'
#' Requires the `zcurve` package.
#'
#' @keywords results
#'
#' @references
#' Bartos F, Schimmack U (2022). "Z-curve 2.0: Estimating replication rates and
#' discovery rates." _Meta-Psychology_, *6*.
#' \doi{10.15626/MP.2021.2720}
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param seed a seed for the LLM
#'
#' @returns a list
z_curve <- function(paper, seed = 8675309) {
  # paper <- psychsci[[218]] # to test

  # extract all p-values ----
  p_vals <- extract_p_values(paper)

  if (nrow(p_vals) == 0 || !llm_use()) {
    tl <- if (nrow(p_vals) == 0) "na" else "na"
    summary_text <- if (nrow(p_vals) == 0) {
      "No p-values detected."
    } else {
      "An LLM is required to classify focal p-values. Set llm_use(TRUE) to enable."
    }
    return(list(
      table = data.frame(),
      summary_table = data.frame(paper_id = paper_id(paper),
                                 z_curve_focal = NA_integer_,
                                 z_curve_edr = NA_real_,
                                 z_curve_err = NA_real_),
      na_replace = c(z_curve_focal = NA_integer_,
                     z_curve_edr = NA_real_,
                     z_curve_err = NA_real_),
      traffic_light = "na",
      summary_text = summary_text,
      report = summary_text
    ))
  }

  # isolate sections containing p-values ----
  # get each unique section that contains at least one p-value
  sections <- text_search(paper, extract_p_values_pattern_(), return = "section",
                          perl = TRUE, ignore.case = FALSE)

  # get abstract for LLM context
  abstract_text <- tryCatch(
    text_search(paper, ".*", return = "section") |>
      dplyr::filter(.data$section_type == "abstract") |>
      dplyr::pull(.data$text) |>
      paste(collapse = " "),
    error = \(e) ""
  )
  if (length(abstract_text) == 0) abstract_text <- ""

  # build prompts: prepend abstract + section text for each section ----
  sections$prompt_text <- paste0(
    if (nzchar(abstract_text)) paste0("ABSTRACT:\n", abstract_text, "\n\n") else "",
    "RESULTS SECTION:\n", sections$text
  )

  # LLM classification ----
  system_prompt <- paste(
    "You are a research methods expert helping to identify focal statistical",
    "results in psychology papers for z-curve analysis.",
    "",
    "You will receive an abstract and a section of text from a research paper.",
    "The section contains one or more p-values.",
    "For each p-value in the section, classify it as focal or non-focal.",
    "",
    "A p-value is FOCAL if the statistical test directly tests a hypothesis",
    "or claim stated or implied in the abstract (e.g., the main effect of",
    "interest, a primary outcome). A p-value is NON-FOCAL if it is a",
    "manipulation check, attention check, demographic comparison,",
    "secondary/exploratory analysis, robustness check, or a follow-up",
    "contrast after an omnibus test that is already included.",
    "",
    "Return a JSON array of objects, one per p-value found in the section.",
    "Each object must have:",
    "  - \"p_text\": the exact text snippet containing the p-value (e.g. 'p = 0.03')",
    "  - \"focal\": true or false",
    "  - \"reason\": one sentence explaining why",
    "",
    "Return only the JSON array, no markdown fences."
  )

  llm_results <- llm(
    text = sections,
    system_prompt = system_prompt,
    text_col = "prompt_text",
    params = list(seed = seed)
  )
  llm_model_used <- attr(llm_results, "llm")$model

  # parse LLM JSON answers ----
  classifications <- lapply(seq_len(nrow(llm_results)), function(i) {
    ans <- llm_results$answer[[i]]
    if (is.na(ans)) return(data.frame())
    tryCatch({
      parsed <- jsonlite::fromJSON(ans, simplifyDataFrame = TRUE)
      if (is.data.frame(parsed) && "p_text" %in% names(parsed) &&
          "focal" %in% names(parsed)) {
        parsed$section_id <- llm_results$section_id[[i]]
        parsed$paper_id   <- llm_results$paper_id[[i]]
        parsed
      } else {
        data.frame()
      }
    }, error = \(e) data.frame())
  })
  class_df <- dplyr::bind_rows(classifications)

  # match classifications back to extracted p-values ----
  # join on the p_text string to recover numeric p_value
  if (nrow(class_df) == 0 || !"focal" %in% names(class_df)) {
    return(.z_curve_na(paper, "LLM returned no parseable classifications."))
  }

  # normalise p_text for matching (strip spaces)
  class_df$p_text_norm <- gsub("\\s+", "", class_df$p_text)
  p_vals$p_text_norm   <- gsub("\\s+", "", p_vals$text)

  table <- dplyr::left_join(p_vals, class_df, by = "p_text_norm",
                            relationship = "many-to-many") |>
    dplyr::select(-"p_text_norm") |>
    dplyr::distinct()

  # focal subset with valid p-values ----
  focal <- dplyr::filter(table, .data$focal == TRUE,
                         !is.na(.data$p_value),
                         .data$p_value > 0,
                         .data$p_value < 1)

  n_focal <- nrow(focal)

  if (n_focal < 10) {
    return(.z_curve_na(paper,
      sprintf("Only %d focal p-value%s detected; z-curve requires at least 10.",
              n_focal, plural(n_focal))))
  }

  # convert to z-scores and run zcurve ----
  zscores <- stats::qnorm(focal$p_value / 2, lower.tail = FALSE)
  zscores <- zscores[is.finite(zscores)]

  if (length(zscores) < 10) {
    return(.z_curve_na(paper, "Too few finite z-scores after conversion."))
  }

  if (!requireNamespace("zcurve", quietly = TRUE)) {
    return(.z_curve_na(paper,
      "The 'zcurve' package is required. Install it with install.packages('zcurve')."))
  }

  zc <- tryCatch(
    zcurve::zcurve(zscores),
    error = \(e) NULL
  )

  if (is.null(zc)) {
    return(.z_curve_na(paper, "Z-curve fitting failed."))
  }

  zc_sum <- tryCatch(summary(zc), error = \(e) NULL)

  edr_val <- tryCatch(zc_sum$coefficients["EDR", "Estimate"], error = \(e) NA_real_)
  err_val <- tryCatch(zc_sum$coefficients["ERR", "Estimate"], error = \(e) NA_real_)
  edr_ci  <- tryCatch(unname(zc_sum$coefficients["EDR", c("l.CI", "u.CI")]), error = \(e) c(NA_real_, NA_real_))
  err_ci  <- tryCatch(unname(zc_sum$coefficients["ERR", c("l.CI", "u.CI")]), error = \(e) c(NA_real_, NA_real_))

  # traffic light ----
  # Large EDR-ERR gap suggests publication bias
  gap <- if (!is.na(err_val) && !is.na(edr_val)) err_val - edr_val else NA_real_
  tl <- if (is.na(gap)) "na" else if (gap > 0.15) "red" else "green"

  # summary_table ----
  summary_table <- data.frame(
    paper_id     = paper_id(paper),
    z_curve_focal = n_focal,
    z_curve_edr   = if (!is.na(edr_val)) round(edr_val, 3) else NA_real_,
    z_curve_err   = if (!is.na(err_val)) round(err_val, 3) else NA_real_
  )

  # summary_text ----
  summary_text <- sprintf(
    "Z-curve on %d focal p-value%s: EDR = %.0f%% [%.0f%%, %.0f%%], ERR = %.0f%% [%.0f%%, %.0f%%].",
    n_focal, plural(n_focal),
    edr_val * 100, edr_ci[1] * 100, edr_ci[2] * 100,
    err_val * 100, err_ci[1] * 100, err_ci[2] * 100
  )

  # report ----
  focal_table <- dplyr::select(focal, dplyr::any_of(
    c("text", "section_type", "focal", "reason", "p_value")
  ))
  names(focal_table)[names(focal_table) == "text"] <- "P-value text"
  names(focal_table)[names(focal_table) == "section_type"] <- "Section"
  names(focal_table)[names(focal_table) == "focal"] <- "Focal"
  names(focal_table)[names(focal_table) == "reason"] <- "Reason"
  names(focal_table)[names(focal_table) == "p_value"] <- "p"

  all_class_table <- dplyr::select(table, dplyr::any_of(
    c("text", "section_type", "focal", "reason")
  ))
  names(all_class_table)[names(all_class_table) == "text"] <- "P-value text"
  names(all_class_table)[names(all_class_table) == "section_type"] <- "Section"
  names(all_class_table)[names(all_class_table) == "focal"] <- "Focal"
  names(all_class_table)[names(all_class_table) == "reason"] <- "Reason"

  edr_pct <- sprintf("%.0f%% [%.0f%%, %.0f%%]",
                     edr_val * 100, edr_ci[1] * 100, edr_ci[2] * 100)
  err_pct <- sprintf("%.0f%% [%.0f%%, %.0f%%]",
                     err_val * 100, err_ci[1] * 100, err_ci[2] * 100)

  report_text <- sprintf(
    paste0("We used '%s' to classify p-values as focal or non-focal. ",
           "%d of %d detected p-value%s were classified as focal. ",
           "Z-curve estimates: EDR = %s, ERR = %s."),
    llm_model_used,
    n_focal, nrow(p_vals), plural(nrow(p_vals)),
    edr_pct, err_pct
  )

  guidance <- c(
    "The **Expected Discovery Rate (EDR)** estimates the proportion of all tested hypotheses that are true, corrected for publication bias.",
    "The **Expected Replication Rate (ERR)** estimates the mean power of the studies that were selected for significance (i.e., how often a significant result would replicate).",
    "A large gap between ERR and EDR suggests selective reporting or publication bias.",
    "Z-curve requires focal tests to be independent. Results should be interpreted cautiously when studies share samples.",
    format_ref(BartoSchimmack2022)
  )

  report <- c(
    report_text,
    scroll_table(all_class_table),
    collapse_section(guidance)
  )

  # return ----
  list(
    table         = table,
    summary_table = summary_table,
    na_replace    = c(z_curve_focal = NA_integer_,
                      z_curve_edr = NA_real_,
                      z_curve_err = NA_real_),
    traffic_light = tl,
    summary_text  = summary_text,
    report        = report
  )
}

# helper: uniform na return ----
.z_curve_na <- function(paper, msg) {
  list(
    table = data.frame(),
    summary_table = data.frame(paper_id = paper_id(paper),
                               z_curve_focal = NA_integer_,
                               z_curve_edr = NA_real_,
                               z_curve_err = NA_real_),
    na_replace = c(z_curve_focal = NA_integer_,
                   z_curve_edr = NA_real_,
                   z_curve_err = NA_real_),
    traffic_light = "na",
    summary_text = msg,
    report = msg
  )
}

# helper: expose p-value regex for section search ----
.z_curve_pval_sections <- function(paper) {
  text_search(paper, extract_p_values_pattern_(), return = "section",
              perl = TRUE, ignore.case = FALSE)
}

extract_p_values_pattern_ <- function() {
  operators <- paste(c("=", "<", ">", "~",
                       "≈", "≠", "≤", "≥",
                       "≪", "≫"),
                     collapse = "")
  paste0(
    "\\bp-?(value)?\\s*",
    "[", operators, "]{1,2}\\s*",
    "(n\\.?s\\.?|\\d?\\.\\d+)",
    "\\s*(e\\s*-\\d+)?",
    "(\\s*[x\\*]\\s*10\\s*\\^\\s*-\\d+)?"
  )
}

# references ----
BartoSchimmack2022 <- bibentry(
  bibtype = "Article",
  title   = "Z-curve 2.0: Estimating replication rates and discovery rates",
  author  = c(person("F.", "Bartos"), person("U.", "Schimmack")),
  year    = 2022,
  journal = "Meta-Psychology",
  volume  = "6",
  doi     = "10.15626/MP.2021.2720"
)
