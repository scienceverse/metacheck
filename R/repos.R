#' Categorise files
#'
#' @param contents a table with columns name, path such as from `osf_contents()`
#'
#' @returns the table with new column file_category
#' @export
#' @examples
#' contents <- c("script.R", "data.csv", "README", "codebook.csv")
#' file_category(contents)
file_category <- function(contents) {
  if (is.atomic(contents)) {
    contents <- data.frame(
      name = contents
    )
  }

  if (is.null(contents$filetype)) {
    file_types <- metacheck::file_types

    contents$filetype <- lapply(contents$name, \(nm) {
      types <- file_types$ext |>
        gsub("\\.", "\\\\.", x = _) |>
        paste0("\\.", x = _ , "$") |>
        sapply(grepl, x = nm, ignore.case = TRUE) |>
        which() |>
        unname()
      file_types[types, ]$type |>
        unique() |>
        paste(collapse = ",")
    })
  }

  nm <- contents$name
  cat <- contents$category %||% NA
  ft <- contents$filetype

  # category is from OSF, so can be: analysis, communication, data, hypothesis, instrumentation, methods and measures, procedure, project, software, other, but mostly uncategorized (NA)

  # hard rules
  sure_class <- dplyr::case_when(
    ft == "stats" ~ "code",
    ft == "data" ~ "data",
    ft == "code" ~ "code"
  )

  is_readme <- grepl("read[ _-]?me", contents$name, ignore.case = TRUE)

  # data
  is_data <- dplyr::case_when(
    cat == "data" ~ TRUE,
    ft == "data" ~ TRUE,
    grepl("data", nm, ignore.case = TRUE) ~ TRUE,
    .default = FALSE
  )

  # code
  is_code <- dplyr::case_when(
    cat == "code" ~ TRUE,
    ft == "code" ~ TRUE,
    grepl("code|script", nm, ignore.case = TRUE) ~ TRUE,
    .default = FALSE
  )

  # codebook
  is_codebook <- dplyr::case_when(
    cat == "codebook" ~ TRUE,
    grepl("code[ _]?book", nm, ignore.case = TRUE) ~ TRUE,
    grepl("data[ _]?dict", nm, ignore.case = TRUE) ~ TRUE,
    .default = FALSE
  )

  contents$file_category <- dplyr::case_when(
    is_readme ~ "readme",
    is_codebook ~ "codebook",
    !is.na(sure_class) ~ sure_class,
    # is_code ~ "code",
    # is_data ~ "data",
    .default = NA_character_
  )

  return(contents)
}
