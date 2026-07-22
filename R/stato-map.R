# Column-header -> STATO ontology IRI mapping for statistical result tables
# extracted from JASP (.jasp) and jamovi (.omv) files.
#
# THREE-TIER typing (see stato_type_column):
#   1. jamovi/JASP column TITLE -> a verified STATO class IRI (this table);
#   2. common JASP-only statistics added to the same table;
#   3. FALLBACK: no STATO class -> the column's own header text is the type
#      (annotationValue = header, no termAccession). Nothing is ever dropped.
#
# The jamovi titles come from the 28 `.r.yaml` results-definitions in the jmv
# package (github.com/jamovi/jmv); the full title list is bundled at
# inst/schema/isa-json/jamovi-column-titles.txt. STATO IRIs were each verified
# via the EBI OLS4 API (https://www.ebi.ac.uk/ols4) against the `stato` ontology
# — they are NOT guessed. Statistics with no STATO class (R², partial/generalized
# eta-squared, RMSEA, TLI, AIC, BIC, McDonald's omega, ...) are deliberately
# absent here and handled by the fallback.

# The STATO accession -> canonical label, for the ones we type. Keeping the label
# lets the ISA OntologyAnnotation carry a human-readable annotationValue too.
.STATO_LABELS <- c(
  "STATO:0000176" = "t-statistic",
  "STATO:0000282" = "F-statistic",
  "STATO:0000030" = "Chi-Squared statistic",
  "STATO:0000376" = "Z-statistic",
  "STATO:0000700" = "p-value",
  "STATO:0000069" = "number of degrees of freedom",
  "STATO:0000498" = "numerator degrees of freedom",
  "STATO:0000527" = "denominator degrees of freedom",
  "STATO:0000618" = "Cohen's d",
  "STATO:0000319" = "Hedges's g",
  "STATO:0000320" = "Glass's delta",
  "STATO:0000317" = "eta-squared",
  "STATO:0000318" = "omega-squared",
  "STATO:0000182" = "odds ratio",
  "STATO:0000280" = "Pearson's correlation coefficient",
  "STATO:0000266" = "Bayes factor",
  "STATO:0000401" = "sample mean",
  "STATO:0000684" = "standard deviation for sample",
  "STATO:0000643" = "variance for sample",
  "STATO:0000037" = "standard error of the mean",
  "STATO:0000196" = "confidence interval",
  "STATO:0000088" = "study group population size"
)

# Column-title (as JASP/jamovi render it, lower-cased for matching) -> STATO id.
# Multiple header spellings can map to the same class (e.g. "sd", "std. deviation").
.STATO_MAP <- c(
  # test statistics
  "t"                       = "STATO:0000176",
  "f"                       = "STATO:0000282",
  "χ²"            = "STATO:0000030",   # chi-square symbol
  "chi-squared"             = "STATO:0000030",
  "chi-square"              = "STATO:0000030",
  "z"                       = "STATO:0000376",
  # p-values
  "p"                       = "STATO:0000700",
  "p-value"                 = "STATO:0000700",
  # degrees of freedom
  "df"                      = "STATO:0000069",
  "df1"                     = "STATO:0000498",
  "df2"                     = "STATO:0000527",
  "num df"                  = "STATO:0000498",
  "den df"                  = "STATO:0000527",
  # effect sizes
  "cohen's d"               = "STATO:0000618",
  "hedges' g"               = "STATO:0000319",
  "hedges's g"              = "STATO:0000319",
  "glass' delta"            = "STATO:0000320",
  "η²"           = "STATO:0000317",   # eta-squared
  "ω²"           = "STATO:0000318",   # omega-squared
  "odds ratio"              = "STATO:0000182",
  # descriptives / dispersion
  "mean"                    = "STATO:0000401",
  "sd"                      = "STATO:0000684",
  "std. deviation"          = "STATO:0000684",
  "standard deviation"      = "STATO:0000684",
  "variance"                = "STATO:0000643",
  "se"                      = "STATO:0000037",
  "standard error"          = "STATO:0000037",
  "std. error"              = "STATO:0000037",
  "n"                       = "STATO:0000088",
  # correlation
  "r"                       = "STATO:0000280",
  "pearson's r"             = "STATO:0000280",
  # bayesian
  "bf₁₀"         = "STATO:0000266",   # BF10
  "bf₀₁"         = "STATO:0000266",   # BF01
  "bayes factor"            = "STATO:0000266",
  # R console-output spellings (summary(lm)/aov/anova/t.test/cor.test), so the
  # captured-R-output path types the same statistics as the JASP/jamovi path.
  "t value"                 = "STATO:0000176",   # lm/anova t column
  "z value"                 = "STATO:0000376",   # glm z column
  "f value"                 = "STATO:0000282",   # aov/anova F column
  "pr(>|t|)"                = "STATO:0000700",   # lm p column
  "pr(>|z|)"                = "STATO:0000700",   # glm p column
  "pr(>f)"                  = "STATO:0000700",   # aov/anova p column
  "pr(>chi)"                = "STATO:0000700",
  "pr(>chisq)"              = "STATO:0000700",
  "p value"                 = "STATO:0000700"
)

#' Type a result-table column by its header, with a guaranteed fallback
#'
#' Maps a JASP/jamovi result-table column header to a STATO ontology class when
#' one exists; otherwise falls back to the header text itself as an untyped
#' label. Every column therefore gets a type — semantic (STATO) or nominal — so
#' no column is dropped from the ISA-JSON export.
#'
#' @param header the column header string as rendered in the result table
#'
#' @returns a list with `annotationValue` (STATO label, or the header text when
#'   unmapped), `termSource` (`"STATO"` or `""`), and `termAccession` (the STATO
#'   IRI, or `""`). The shape matches an ISA `OntologyAnnotation`.
#' @export
stato_type_column <- function(header) {
  key <- tolower(trimws(header %||% ""))
  id  <- if (nzchar(key) && key %in% names(.STATO_MAP)) .STATO_MAP[[key]] else NA_character_
  if (!is.na(id)) {
    list(annotationValue = unname(.STATO_LABELS[[id]] %||% header),
         termSource = "STATO",
         termAccession = paste0("http://purl.obolibrary.org/obo/",
                                sub(":", "_", id)))
  } else {
    # Tier 3: no STATO class — keep the header as a nominal (untyped) label.
    list(annotationValue = trimws(header %||% ""),
         termSource = "",
         termAccession = "")
  }
}
