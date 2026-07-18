# A native-R validator for Behaverse `trial` documents. This is NOT a JSON-Schema
# engine and does NOT execute the schema: it reads the required-field lists and
# property types out of the vendored, pinned schema (inst/schema/behaverse-trial-
# v26.0608.json) and checks a TrialData document against them in native R. This
# mirrors R/psychds-validate.R, which reimplements the Psych-DS checks rather than
# running the upstream (Deno/TypeScript) validator. A real JSON-Schema validator
# is used only as a development oracle in the tests, to confirm agreement.
#
# The pinned schema declares draft 2019-09 but uses no 2019-09-only keyword (only
# `$defs` + JSON-Pointer `$ref`), so a scalar/required-field check reproduces its
# verdicts. See inst/schema/behaverse-trial-PROVENANCE.md.

# Basename of the vendored schema. Bump alongside R/behaverse-convert.R.
.behaverse_schema_file <- "schema/behaverse-trial-v26.0608.json"

# Load and cache the vendored schema (a parsed list). Read once per session.
.behaverse_schema_cache <- new.env(parent = emptyenv())
.behaverse_schema <- function() {
  if (is.null(.behaverse_schema_cache$schema)) {
    path <- system.file(.behaverse_schema_file, package = "metacheck")
    if (!nzchar(path) || !file.exists(path))
      stop("Vendored Behaverse schema not found: ", .behaverse_schema_file,
           call. = FALSE)
    .behaverse_schema_cache$schema <- jsonlite::fromJSON(
      path, simplifyVector = FALSE)
  }
  .behaverse_schema_cache$schema
}

# The `$defs` class definition for one Behaverse table (e.g. "Response",
# "Instrument"), or NULL when the schema has no such class.
.behaverse_class <- function(name, schema = .behaverse_schema()) {
  schema[["$defs"]][[name]]
}

# The JSON-Schema type(s) a property permits, as a character vector. The schema
# writes nullable scalars as `"type": ["integer", "null"]`; a plain required
# scalar as `"type": "string"`. Returns character(0) when no type is declared
# (e.g. an $ref-only property), which the checker treats as "any".
.behaverse_prop_types <- function(prop) {
  ty <- prop[["type"]]
  if (is.null(ty)) return(character(0))
  as.character(unlist(ty))
}

# Does one R value satisfy one JSON-Schema scalar type? `NULL`/`NA` satisfy a
# type set that includes "null"; otherwise the R value's class must map to the
# JSON type. jsonlite unboxes scalars, so we test the atomic value.
.behaverse_value_ok <- function(value, types) {
  if (length(types) == 0) return(TRUE)                 # untyped -> any
  is_null <- is.null(value) || (length(value) == 1L && is.na(value))
  if (is_null) return("null" %in% types)
  ok_one <- function(ty) switch(ty,
    string  = is.character(value),
    integer = is.numeric(value) && all(value == round(value)),
    number  = is.numeric(value),
    boolean = is.logical(value),
    array   = is.list(value) || length(value) > 1L,
    object  = is.list(value),
    null    = FALSE,
    TRUE)                                              # unknown type -> permissive
  any(vapply(types, ok_one, logical(1)))
}

# One validator issue (same shape as .psychds_issue).
.behaverse_issue <- function(code, severity, reason, table = NA_character_) {
  list(code = code, severity = severity, reason = reason, table = table)
}

#' Validate a Behaverse `trial` (TrialData) document
#'
#' Checks a `TrialData` object (a named list mapping Behaverse table names to a
#' list of row objects, e.g. `list(Instrument = list(...), Response = list(...))`)
#' against the pinned Behaverse `trial` schema. Verifies, for each table present,
#' that every row carries the schema's required fields and that field values match
#' the declared scalar types (nullable fields may be `NULL`/`NA`). This is a
#' native-R reimplementation of the checks; it does not execute the JSON Schema.
#'
#' @param doc a `TrialData` document as an R list (tables -> list of row lists),
#'   or a length-1 character path / JSON string to parse first.
#'
#' @returns a list with `valid` (TRUE when no error-severity issues), `issues`
#'   (a list of issues), and `summary` (counts). Printed as a compact report.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' doc <- convert_behaverse(df, instrument_id = "psqi")
#' behaverse_validate(doc)
#' }
behaverse_validate <- function(doc) {
  if (is.character(doc) && length(doc) == 1L) {
    src <- if (file.exists(doc)) doc else textConnection(doc)
    doc <- tryCatch(jsonlite::fromJSON(src, simplifyVector = FALSE),
                    error = function(e) NULL)
    if (is.null(doc))
      return(structure(list(
        valid = FALSE,
        issues = list(.behaverse_issue("JsonInvalid", "error",
          "Input is not valid JSON.")),
        summary = list(n_errors = 1L, n_warnings = 0L, n_tables = 0L)),
        class = "behaverse_validation"))
  }

  schema <- .behaverse_schema()
  issues <- list()
  add <- function(...) issues[[length(issues) + 1L]] <<- .behaverse_issue(...)

  if (!is.list(doc) || is.null(names(doc)) || !any(nzchar(names(doc)))) {
    add("TrialDataInvalid", "error",
        "A TrialData document must be a named list of tables (e.g. Response).")
    return(.behaverse_result(issues))
  }

  # Only the tables the TrialData root recognises are checked; unknown top-level
  # keys are ignored (the root permits additional properties).
  known_tables <- names(schema[["$defs"]][["TrialData"]][["properties"]])

  for (tbl in names(doc)) {
    if (!tbl %in% known_tables) next
    rows <- doc[[tbl]]
    cls  <- .behaverse_class(tbl, schema)
    if (is.null(cls)) next
    required <- as.character(unlist(cls[["required"]]))
    props    <- cls[["properties"]] %||% list()

    # A table must be an ARRAY of row objects (a list of lists), not a single
    # object. jsonlite reads a JSON array of objects as an unnamed list.
    if (!is.list(rows) || (length(rows) > 0 && !is.null(names(rows)) &&
                           any(nzchar(names(rows))))) {
      add("TableNotArray", "error",
          sprintf("Table \"%s\" must be an array of row objects.", tbl), tbl)
      next
    }

    for (row in rows) {
      # Required-field presence.
      missing <- required[!vapply(required, function(f)
        !is.null(row[[f]]), logical(1))]
      if (length(missing))
        add("RequiredFieldMissing", "error",
            sprintf("Table \"%s\": row missing required field%s %s.",
                    tbl, plural(length(missing)),
                    paste(missing, collapse = ", ")), tbl)

      # Type agreement for every present field the schema declares.
      for (f in names(row)) {
        p <- props[[f]]
        if (is.null(p)) next                            # unknown field -> ignore
        if (!.behaverse_value_ok(row[[f]], .behaverse_prop_types(p)))
          add("FieldTypeMismatch", "error",
              sprintf("Table \"%s\": field \"%s\" has the wrong type (expected %s).",
                      tbl, f, paste(.behaverse_prop_types(p), collapse = "/")), tbl)
      }
    }
  }

  .behaverse_result(issues)
}

# Assemble the classed result object from the accumulated issues.
.behaverse_result <- function(issues) {
  severities <- vapply(issues, function(x) x$severity, character(1))
  res <- list(
    valid = !any(severities == "error"),
    issues = issues,
    summary = list(
      n_errors   = sum(severities == "error"),
      n_warnings = sum(severities == "warning"),
      n_tables   = length(unique(vapply(issues, function(x) x$table,
                                        character(1))))
    ))
  class(res) <- "behaverse_validation"
  res
}

#' @export
print.behaverse_validation <- function(x, ...) {
  cat(if (x$valid) "✓ VALID" else "✗ INVALID", "Behaverse trial document",
      sprintf("(%d error%s, %d warning%s)\n",
              x$summary$n_errors, plural(x$summary$n_errors),
              x$summary$n_warnings, plural(x$summary$n_warnings)))
  # Collapse repeated (code, table) issues — a bad field repeats per row.
  seen <- character(0)
  for (iss in x$issues) {
    key <- paste(iss$code, iss$table, iss$reason)
    if (key %in% seen) next
    seen <- c(seen, key)
    mark <- if (iss$severity == "error") "  [ERROR]  " else "  [warn]   "
    cat(mark, iss$code, ": ", iss$reason, "\n", sep = "")
  }
  invisible(x)
}
