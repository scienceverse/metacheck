#' Psychometric Scale Dictionary
#'
#' A curated dictionary of psychometric instruments, used by `codebook_check` to
#' identify named scales in shared data by matching column-name prefixes /
#' acronyms and confirming them against codebook item wording and the paper text.
#' Combines the OpenScales community repository with curated additions of
#' widely-used instruments not in OpenScales. Acronyms that collide (e.g. `AQ` =
#' Autism Spectrum Quotient and Aggression Questionnaire) are kept and
#' disambiguated at match time; genuine duplicates are removed.
#'
#' @format A data frame with one row per instrument and 4 columns:
#' \describe{
#'   \item{name}{Canonical full name of the instrument.}
#'   \item{acronym}{Short trigger acronym (>= 2 characters, not a common English
#'     word); empty when none is safe. May be shared by several instruments.}
#'   \item{code}{OpenScales code (empty for curated additions); links to the
#'     OpenScales `.osd` definition for subscale / scoring / reverse-coding
#'     detail.}
#'   \item{source}{`"openscales"` or `"curated"`.}
#' }
#' @source OpenScales (\url{https://github.com/stmueller/OpenScales}); curated
#'   additions from published scale indexes. Rebuild with `data-raw/scales.R`.
#' @keywords internal
"scales"
