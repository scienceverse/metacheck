#' Accuracy
#'
#' Signal detection values for modules that classify papers as having a feature or not
#'
#' @param expected a vector of logical values for the expected values
#' @param observed a vector of logical values for the observed values
#'
#' @returns a list of accuracy parameters
#' @export
accuracy <- function(expected, observed) {
  # categorise the sample
  hit <- sum(expected & observed)
  miss <- sum(expected & !observed)
  fa <- sum(!expected & observed)
  cr <- sum(!expected & !observed)

  # Convert counts to proportions
  hit_rate <- hit / (hit + miss)
  fa_rate <- fa / (fa + cr)

  # Adjust for extreme values (avoid infinite z-scores)
  hit_rate <- ifelse(hit_rate == 1, 1 - 0.5 / (hit + miss), hit_rate)
  hit_rate <- ifelse(hit_rate == 0, 0.5 / (hit + miss), hit_rate)
  fa_rate <- ifelse(fa_rate == 1, 1 - 0.5 / (fa + cr), fa_rate)
  fa_rate <- ifelse(fa_rate == 0, 0.5 / (fa + cr), fa_rate)

  # Compute d-prime and beta
  d_prime <- stats::qnorm(hit_rate) - stats::qnorm(fa_rate)
  beta <- exp((stats::qnorm(fa_rate)^2 -
    stats::qnorm(hit_rate)^2) / 2)

  # return accuracy measures
  measures <- list(
    hits = hit,
    misses = miss,
    false_alarms = fa,
    correct_rejections = cr,
    accuracy = (hit + cr) / (hit + cr + fa + miss),
    sensitivity = hit_rate,
    specificity = fa_rate,
    d_prime = d_prime,
    beta = beta
  )

  class(measures) <- "metacheck_accuracy_measures"

  return(measures)
}


#' Validate
#'
#' @param gt a data frame or vector of text
#' @param module the module
#' @param compare name of the module output table for comparison
#'
#' @returns something
#' @export
#'
#' @examples
#' validate("p < .05", "stat_p_exact")
validate <- function(gt, module, compare = "table") {
  # convert vector of text to table
  if (!is.data.frame(gt)) {
    gt <- data.frame(
      paper_id = seq_along(gt) |> as.character(),
      text = gt
    )
  }

  # create a test paper for each id containing the text
  paper <- lapply(gt$paper_id, \(paper_id) {
    t <- gt[gt$paper_id == paper_id, "text"]
    p <- test_paper(t)
    p$paper_id <- paper_id
    p
  }) |>
  paperlist()

  # run the module on the test papers
  mo <- module_run(paper, module)

  # get table for comparison to text
  comp_table <- dplyr::full_join(
    gt, mo[[compare]],
    by = c("paper_id", "text"),
    suffix = c(".gt", ".mod")
  )

  # compare columns with .gt/.mod suffix
  comp_cols <- names(comp_table) |>
    grep("\\.mod$", x = _, value = TRUE) |>
    gsub("\\.mod$", "", x = _)

  for (col in comp_cols) {
    v_col <- paste0(col, ".valid")
    gt_col <- comp_table[[paste0(col, ".gt")]]
    mod_col <- comp_table[[paste0(col, ".mod")]]
    comp_table[[v_col]] <- gt_col == mod_col
  }

  comp_table
}

