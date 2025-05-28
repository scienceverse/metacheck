#' Validate a module
#'
#' @param paper a paper object or a list of paper objects
#' @param module The name of a module or path to a module
#' @param ... further arguments to match to the module output
#'
#' @returns A validation list object
#' @export
validate <- function(paper, module, ...) {
  # run the module ----
  module_path <- module_find(module)
  results <- module_run(paper, module)

  # check matches ----
  expected <- list(...)
  to_check <- intersect(names(results), names(expected))
  if (length(to_check) == 0) {
    stop("The results of this module did not return any objects named: ",
         paste(names(expected), collapse = ", "))
  }

  matches <- list()
  stats <- list(n_papers = length(paper))
  for (check in to_check) {
    match_stats <- list()

    if (is.data.frame(expected[[check]])) {
      exp_names <- names(expected[[check]])

      # must have an id column
      if (!"id" %in% exp_names) {
        stop("The `", check, "` table must have an `id` column")
      }

      # check only matching columns from results
      if (!all(exp_names %in% names(results[[check]]))) {
        stop("The `", check, "` table did not have the same columns as the expected table")
      }

      result_actual <- results[[check]][exp_names] |> unique()
      result_expected <- expected[[check]][exp_names] |> unique()

      # combine actual and expected results
      if (nrow(result_actual) == nrow(result_expected) &
          all(names(paper) %in% result_expected$id)) {
        # a summary table with one row per ID
        match_res <- dplyr::full_join(result_expected,
                                      result_actual,
                                      by = "id",
                                      suffix = c(".expected", ".actual"))

        for (col in exp_names[exp_names != "id"]) {
          match_res[[col]] <- sapply(
            match_res[[paste0(col, ".expected")]] ==
              match_res[[paste0(col, ".actual")]],
            isTRUE
          )
          match_stats[[col]] <- mean(match_res[[col]])
        }
      } else {
        # a table with 0+ rows per id
        true_pos <- dplyr::inner_join(result_expected,
                                  result_actual,
                                  by = exp_names)
        false_neg <- dplyr::anti_join(result_expected,
                                   result_actual,
                                  by = exp_names)
        false_pos <- dplyr::anti_join(result_actual,
                                    result_expected,
                                    by = exp_names)
        true_pos$expected  = rep(TRUE, nrow(true_pos))
        true_pos$actual    = rep(TRUE, nrow(true_pos))
        false_neg$expected = rep(TRUE, nrow(false_neg))
        false_neg$actual   = rep(FALSE, nrow(false_neg))
        false_pos$expected = rep(FALSE, nrow(false_pos))
        false_pos$actual   = rep(TRUE, nrow(false_pos))

        match_res <- list(true_pos, false_neg, false_pos) |>
          do.call(dplyr::bind_rows, args = _)
        match_res$match <- match_res$expected == match_res$actual

        match_stats$true_positive = nrow(true_pos)
        match_stats$false_positive = nrow(false_pos)
        match_stats$false_negative = nrow(false_neg)
      }

      matches[[check]] <- match_res
      stats[[check]] <- match_stats
    } else {
      # check full object
      matches[[check]] <- all(expected[[check]] == results[[check]])
    }
  }

  # organise info and return ----
  info <- list(
    module = module,
    actual = results[to_check],
    matches = matches,
    stats = stats
  )

  class(info) <- "ppchk_validate"

  return(info)
}


#' Print Validation List Object
#'
#' @param x The ppchk_validate object
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
print.ppchk_validate <- function(x, ...) {
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
#' @param actual a vector of logical values for the actual values
#'
#' @returns a list of accuracy parameters
#' @export
accuracy <- function(expected, actual) {
  # categorise the sample
  hit <- sum(expected & actual)
  miss <- sum(expected & !actual)
  fa <- sum(!expected & actual)
  cr <- sum(!expected & !actual)

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

  class(measures) <- "ppchk_accuracy_measures"

  return(measures)
}
