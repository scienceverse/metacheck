#' Print Validation List Object
#'
#' @param x The metacheck_validate object
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
print.metacheck_validate <- function(x, ...) {
  txt <- sprintf("Validated matches for module `%s`:\n\n", x$module)
  txt <- sprintf("%s* N in validation sample: %i", txt, x$stats$n_papers)
  for (stat in names(x$stats[-1])) {
    txt <- sprintf("%s\n* %s: ", txt, stat)

    # set different formats for integer vs decimal stats
    all_integers <- sapply(x$stats[[stat]], \(x) x == as.integer(x)) |> all()
    fmt <- ifelse(all_integers, "%s\n  * %s: %i", "%s\n  * %s: %.2f")

    for (item in names(x$stats[[stat]])) {
      value <- x$stats[[stat]][[item]]
      txt <- sprintf(fmt, txt, item, value)
    }
  }
  cat("", txt)
}



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
  fa_rate  <- ifelse(fa_rate  == 1, 1 - 0.5 / (fa + cr), fa_rate)
  fa_rate  <- ifelse(fa_rate  == 0, 0.5 / (fa + cr), fa_rate)

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
    accuracy = (hit + cr)/(hit+cr+fa+miss),
    sensitivity = hit_rate,
    specificity = fa_rate,
    d_prime = d_prime,
    beta = beta
  )

  class(measures) <- "metacheck_accuracy_measures"

  return(measures)
}

#' Compare Tables for Validation
#'
#' @param expected the expected table
#' @param observed the observed table
#' @param match_cols which columns should be used to determine identification
#' @param comp_cols which columns should be compared for classification
#'
#' @returns a list of comparisons
#' @export
#'
#' @examples
#' expected <- data.frame(id = 1:2, text = c("A", "B"), value = c(10, 20))
#' observed <- data.frame(id = 1:2, text = c("A", "B"), value = c(10, 25))
#' compare_tables(expected, observed)
compare_tables <- function(expected, observed,
                           match_cols = c("id", "text"),
                           comp_cols = NULL) {
  .temp_id. <- NULL

  # error checking
  if (!all(match_cols %in% colnames(expected)) |
      !all(match_cols %in% colnames(observed))) {
    stop("All match_cols need to be in both expected and observed tables.")
  }

  # calculate or check for comp_cols
  if (is.null(comp_cols)) {
    # get col names that match between exp and obs
    comp_cols <- intersect(colnames(observed), colnames(expected)) |>
      setdiff(match_cols)
  } else {
    if (!all(comp_cols %in% colnames(expected)) |
        !all(comp_cols %in% colnames(observed))) {
      stop("All comp_cols need to be in both expected and observed tables.")
    }
  }

  # assume repeat columns are in same order in exp and obs
  # add temp ID for matching multi-row returns
  exp <- expected |>
    dplyr::mutate(.temp_id. = dplyr::row_number(),
                  .by = dplyr::all_of(match_cols))

  obs <- observed |>
    dplyr::mutate(.temp_id. = dplyr::row_number(),
                  .by = dplyr::all_of(match_cols))

  join_cols <- c(match_cols, ".temp_id.")

  # metrics of validation ----

  ## identification ----
  exp2 <- exp[, join_cols]
  exp2$exp <- TRUE

  obs2 <- obs[, join_cols]
  obs2$obs <- TRUE

  v_tbl <- dplyr::full_join(exp2, obs2, by = join_cols)
  v_tbl$exp       <- sapply(v_tbl$exp, isTRUE)
  v_tbl$obs       <- sapply(v_tbl$obs, isTRUE)
  v_tbl$true_pos  <- v_tbl$exp & v_tbl$obs
  v_tbl$false_pos <- v_tbl$obs & !v_tbl$exp
  v_tbl$false_neg <- v_tbl$exp & !v_tbl$obs

  vars <- c("exp", "obs", "true_pos", "false_pos", "false_neg")
  v_ident <- apply(v_tbl[, vars], 2, sum)

  ## classification ----
  if (length(comp_cols)) {
    comp_results <- dplyr::full_join(exp, obs,
                                     by = join_cols,
                                     suffix = c(".exp", ".obs"))

    # check for matches
    obs_match <- comp_results[paste0(comp_cols, ".obs")]
    exp_match <- comp_results[paste0(comp_cols, ".exp")]

    # check if values match (NA => FALSE)
    match_val <- apply(obs_match == exp_match, c(1, 2), isTRUE)
    # check if both values are NA
    match_na <- is.na(obs_match) & is.na(exp_match)
    # if values match or both NA, set to TRUE
    match <- match_val | match_na
    colnames(match) <- comp_cols

    v_classification <- apply(match, 2, mean)

    # make details table
    cc <- rep(comp_cols, each = 2) |> paste0(".", c("exp", "obs"))
    details_tbl <- comp_results[, c(join_cols, cc)] |>
      dplyr::left_join(v_tbl, by = join_cols) |>
      dplyr::select(-.temp_id.)
  } else {
    # no comp_cols
    v_classification <- list()
    details_tbl <- v_tbl |>
      dplyr::select(-.temp_id.)
  }

  list(
    identification = v_ident,
    classification = v_classification,
    table = details_tbl
  )
}
