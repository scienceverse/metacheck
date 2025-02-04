#' Validate a module
#'
#' @param module The name of a module or path to a module
#' @param sample A data frame or path to a tabular data file (csv or excel) for the sample to validate against, with columns xml, table, report, traffic_light
#' @param results_table The column name to match the module results table to, or a data frame or the path to a file that contains this info in tabular form
#' @param path The base directory for relative paths in the sample table; if NULL and the sample is a file path, it will use the base directory of the file
#'
#' @returns A validation list object
#' @export

validate <- function(module, sample, results_table = "table", path = NULL) {
  # get sample ----
  # (data frame with cols xml, table, report, traffic_light)
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
  if (!"xml" %in% names(sample)) stop("The sample needs a column called xml")
  xml_exist <- file.path(path, sample$xml) |> file.exists()
  if (all(!xml_exist)) {
    stop("None of the xml files could be found; check the format of your xml column, or set the path argument")
  } else if (!all(xml_exist)) {
    n_missing <- sum(xml_exist == FALSE)
    missing <- sample$xml[!xml_exist] |> paste(collapse = ", ")
    stop("Some (", n_missing, ") of the xml files could not be found: ", missing)
  }

  # results_table ----
  if (is.data.frame(results_table)) {
    res_match <- results_table
  } else if (is.numeric(results_table)) {
    res_match <- data.frame(
      xml = sample$xml,
      text = sample[[results_table]]
    )
  } else if (is.character(results_table) &&
             results_table %in% names(sample)) {
    res_match <- data.frame(
      xml = sample$xml,
      text = sample[[results_table]]
    )
  } else if (is.character(results_table) &&
             file.exists(file.path(path, results_table))) {
    res_match <- file.path(path, results_table) |> read_file()
  }

  # iterate module over the papers in the data frame ----
  sample_res <- data.frame()
  sample$report_ver <- NA
  sample$tl_ver <- NA
  for (i in 1:nrow(sample)) {
    ## read in the paper ----
    paper <- file.path(path, sample$xml[[i]]) |> read_grobid()

    # run the module
    results <- module_run(paper, module)

    # add sample verification results ----
    sample$report_ver[i] <- results$report
    sample$tl_ver[i] <- results$traffic_light

    # add to sample results table
    results$table$xml = sample$xml[[i]]
    sample_res <- dplyr::bind_rows(sample_res, results$table)
  }

  # organise info and return ----
  xml <- NULL
  missing <- dplyr::anti_join(res_match, sample_res,
                              by = names(res_match)) |>
    dplyr::count(xml, name = "results_missing")
  extra <- dplyr::anti_join(res_match, sample_res,
                              by = names(res_match)) |>
    dplyr::count(xml, name = "results_extra")

  sample <- sample |>
    dplyr::left_join(missing, by = "xml") |>
    dplyr::left_join(extra, by = "xml") |>
    dplyr::mutate(dplyr::across(c("results_missing", "results_extra"),
                         ~ tidyr::replace_na(.x, 0)))
  sample$table_check <- (sample$results_missing +
                           sample$results_extra) == 0

  sample$report_check <- sample$report == sample$report_ver
  sample$tl_check     <- sample$traffic_light == sample$tl_ver

  info <- list(
    sample = sample,
    results_table = sample_res[names(res_match)],
    tables_matched = mean(sample$table_check),
    reports_matched = mean(sample$report_check),
    tl_matched = mean(sample$tl_check)
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
