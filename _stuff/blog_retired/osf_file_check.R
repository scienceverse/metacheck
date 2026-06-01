#' OSF File Check
#'
#' @description
#' A short description of the module
#'
#' @author Daniel Lakens (\email{lakens@gmail.com})
#'
#' @import dplyr
#'
#' @keywords method
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
osf_file_check <- function(paper) {
  links <- osf_links(paper)
  info <- osf_info(links, id_col = "href", recursive = TRUE)
  osf_files_summary <- file_category(info)
  report <- osf_report(osf_files_summary)
  r_file_results <- check_r_files(osf_files_summary)

  list(
    traffic_light = report$traffic_light,
    table = r_file_results,
    report = report$report,
    summary = osf_files_summary
  )
}

check_r_files <- function(summary) {
  r_files <- summary |>
    dplyr::filter(osf_type == "files",
                  grepl("\\.R(md)?", name, ignore.case = TRUE)) |>
    dplyr::mutate(abs_report = NA,
                  pkg_report = NA,
                  session_report = NA)

  report <- lapply(r_files$osf_id, \(id) {
    report <- dplyr::filter(r_files, osf_id == !!id)
    # Try downloading the R file
    file_url <- paste0("https://osf.io/download/", id)
    r_code <- tryCatch(
      readLines(url(file_url), warn = FALSE),
      error = function(e) return(NULL)
    )

    if (is.null(r_code)) return("")

    # absolute paths
    abs_path <- grep("[\"\']([A-Z]:|\\/|~)", r_code)
    report$abs <- dplyr::case_when(
      length(abs_path) == 0 ~ "\u2705 No absolute paths were detected",
      length(abs_path) > 0 ~ paste("\u274C Absolute paths found at lines: ",
                                   paste(abs_path, collapse = ", "))
    )

    # package loading
    pkg <- grep("\\b(library|require)\\(", r_code)
    report$pkg<- dplyr::case_when(
      length(pkg) == 0 ~ "\u26A0\uFE0F️ No packages are specified in this script.",
      length(pkg) == 1 ~ "\u2705 Packages are loaded in a single block.",
      all(diff(pkg) < 5) ~ "\u2705 Packages are loaded in a single block.",
      .default = paste(
        "\u274C Packages are loaded in multiple places: lines " ,
        paste(pkg, collapse = ", ")
      )
    )

    # session info
    session <- grep("\\bsession_?[Ii]nfo\\(", r_code)
    report$session <- dplyr::case_when(
      length(session) == 0 ~ "\u274C️ No session info was found in this script.",
      length(session) > 0 ~ paste(
        "\u2705 Session info was found on line",
        paste(session, collapse = ", "))
    )

    return(report)
  }) |>
    do.call(dplyr::bind_rows, args = _)

  return(report)
}


osf_report <- function(summary) {
  files <- dplyr::filter(summary, osf_type == "files")
  data <- dplyr::filter(files, file_category == "data") |> nrow()
  code <- dplyr::filter(files, file_category == "code") |> nrow()
  codebook <- dplyr::filter(files, file_category == "codebook") |> nrow()
  readme <- dplyr::filter(files, file_category == "readme") |> nrow()

  traffic_light <- dplyr::case_when(
    data == 0 & code == 0 & readme == 0 ~ "red",
    data == 0 | code == 0 | readme == 0 ~ "yellow",
    data > 0 & code > 0 & readme > 0 ~ "green"
  )

  data_report <- dplyr::case_when(
    data == 0 ~ "\u26A0\uFE0F There was no data detected. Are you sure you cannot share any of the underlying data? If you did share the data, consider naming the file(s) or file folder with 'data'.",
    data > 0 ~ "\u2705 Data file(s) were detected. Great job making your research more transparent and reproducible!"
  )

  codebook_report <- dplyr::case_when(
    codebook == 0 ~ "\u26A0\uFE0F️ No codebooks or data dictionaries were found. Consider adding one to make it easier for others to know which variables you have collected, and how to re-use them. The codebook package in R can automate a substantial part of the generation of a codebook: https://rubenarslan.github.io/codebook/",
    codebook > 0 ~ "\u2705 Codebook(s) were detected. Well done!"
  )

  code_report <- dplyr::case_when(
    code == 0 ~ "\u26A0\uFE0F️ No code files were found. Are you sure there is no code related to this manuscript? If you shared code, consider naming the file or file folder with 'code' or 'script'.",
    code > 0 ~ "\u2705 Code file(s) were detected. Great job making it easier to  reproduce your results!"
  )

  readme_report <- dplyr::case_when(
    readme == 0 ~ "\u26A0\uFE0F No README files were identified. A read me is best practice to facilitate re-use. If you have a README, please name it explicitly (e.g., README.txt or _readme.pdf).",
    readme > 0 ~ "\u2705 README detected. Great job making it easier to understand how to re-use files in your repository!"
  )

  report_message <- c(
    readme_report,
    data_report,
    codebook_report,
    code_report,
    "Learn more about reproducible data practices: https://www.projecttier.org/tier-protocol/"
  )

  return(list(
    traffic_light = traffic_light,
    report = report_message
  ))
}
