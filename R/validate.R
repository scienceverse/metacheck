#' Validate a module
#'
#' @param module The name of a module or path to a module
#' @param sample A data frame or path to a tabular data file (csv or excel) for the sample to validate against, with required column id, and optional columns table, report, traffic_light
#' @param expected A data frame or the path to a file that contains the expected results table values in tabular form; if NULL, the table column of the sample table will be used or table checks omitted
#' @param path The base directory for relative paths in the sample table; if NULL and the sample is a file path, it will use the base directory of the file
#'
#' @returns A validation list object
#' @export

validate <- function(module, sample, expected = NULL, path = NULL) {
  # check module exists ----
  module_path <- module_find(module)

  # get sample ----
  # (data frame with cols id, table, report, traffic_light)
  if (!is.data.frame(sample)) {
    ## read sample data from file ----
    filename <- sample
    sample <- read_file(filename)
    sample$report[is.na(sample$report)] <- ""

    # set path from file base ----
    if (is.null(path)) {
      path <- dirname(filename) |> normalizePath()
    }
  }

  if (is.null(path)) { path = "." }

  # check sample is set up right ----
  if (nrow(sample) == 0) stop("The sample has no rows")
  if (!"id" %in% names(sample)) stop("The sample needs a column called id")

  # strip .xml from sample$id
  sample$id <- gsub("\\.xml$", "", x = sample$id)
  xml_exist <- file.path(path, sample$id) |>
    paste0(".xml") |>
    file.exists()
  if (all(!xml_exist)) {
    stop("None of the xml files could be found; check the format of your id column, or set the path argument")
  } else if (!all(xml_exist)) {
    n_missing <- sum(xml_exist == FALSE)
    missing <- sample$id[!xml_exist] |> paste(collapse = ", ")
    stop("Some (", n_missing, ") of the xml files could not be found: ", missing)
  }

  # expected ----
  onerow <- is.null(expected) && "table" %in% names(sample)

  if (is.data.frame(expected)) {
    res_match <- expected
    # strip .xml from expected$id
    res_match$id <- gsub("\\.xml$", "", res_match$id)
  } else if (onerow) {
    res_match <- data.frame(
      id = sample$id,
      text = sample$table
    )
  } else if (is.character(expected) &&
             file.exists(file.path(path, expected))) {
    res_match <- file.path(path, expected) |> read_file()
  } else {
    # no match for results - omit results
    res_match <- data.frame()
  }

  # iterate module over the papers in the data frame ----
  sample_res <- data.frame()
  sample$report_ver <- NA
  sample$tl_ver <- NA
  for (i in 1:nrow(sample)) {
    ## read in the paper ----
    paper <- file.path(path, sample$id[[i]]) |>
      read_grobid()

    # run the module
    results <- module_run(paper, module)
    if (onerow && nrow(results$table) > 1) {
      results$table <- results$table[1, ]
    }

    # add sample verification results ----
    sample$report_ver[i] <- results$report
    sample$tl_ver[i] <- results$traffic_light

    # add to sample results table
    results$table$id = sample$id[[i]]
    sample_res <- dplyr::bind_rows(sample_res, results$table)
  }

  # organise info and return ----
  table_matched <- NA
  if (nrow(res_match) > 0) {
    id <- NULL
    missing <- dplyr::anti_join(res_match, sample_res,
                                by = names(res_match)) |>
      dplyr::count(id, name = "misses")
    extra <- dplyr::anti_join(sample_res, res_match,
                                by = names(res_match)) |>
      dplyr::count(id, name = "false_alarms")

    sample <- sample |>
      dplyr::left_join(missing, by = "id") |>
      dplyr::left_join(extra, by = "id") |>
      dplyr::mutate(dplyr::across(c("misses", "false_alarms"),
                           ~ tidyr::replace_na(.x, 0)))
    sample$table_check <- (sample$misses +
                             sample$false_alarms) == 0

    table_matched <- mean(sample$table_check)

    expected_actual <- sample_res[names(res_match)]
  } else {
    expected_actual <- sample_res
  }

  report_matched <- NA
  if ("report" %in% names(sample)) {
    sample$report_check <- sample$report == sample$report_ver
    report_matched <- mean(sample$report_check)
  }

  tl_matched <- NA
  if ("traffic_light" %in% names(sample)) {
    sample$tl_check     <- sample$traffic_light == sample$tl_ver
    tl_matched <- mean(sample$tl_check)
  }

  info <- list(
    module = module,
    sample = sample,
    table = expected_actual,
    table_matched = table_matched,
    report_matched = report_matched,
    tl_matched = tl_matched
  )
  class(info) <- "ppchk_validate"

  return(info)
}


#' Read file of unknown type
#'
#' @param path the path to the file
#'
#' @returns a data frame
#' @keywords internal
read_file <- function(path) {
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    file <- utils::read.csv(path)
  } else if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    file <- readxl::read_excel(path)
  } else {
    file <- data.frame(
      text = readLines(path)
    )
  }

  return(file)
}


#' Traffic Light Accuracy
#'
#' @param validation the validation object
#' @param yes the value of traffic lights for detection (usually "green")
#' @param no the value of traffic lights for no detection (usually "red")
#'
#' @returns a list of accuracy parameters
#' @export
tl_accuracy <- function(validation, yes = "green", no = "red") {
  target <- validation$sample$traffic_light
  sample <- validation$sample$tl_ver

  # categorise the sample
  hit <- sum(target == yes & sample == yes)
  miss <- sum(target == yes & sample == no)
  fa <- sum(target == no & sample == yes)
  cr <- sum(target == no & sample == no)

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
    accuracy = (hit + cr)/(hit+cr+fa+miss),
    sensitivity = hit_rate,
    specificity = fa_rate,
    d_prime = d_prime,
    beta = beta
  )

  class(measures) <- "ppchk_accuracy_measures"

  return(measures)
}
