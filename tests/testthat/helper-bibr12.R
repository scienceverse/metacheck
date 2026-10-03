# bibr export schema 12.0: the fixtures in fixtures/bibr12 are exports from
# bibr, and bibr-export-v12.schema.json is a copy of bibr's strict (producer)
# schema, docs/schema/bibr-export-v12.schema.json.

# Validate a JSON file against the strict bibr 12.0 schema (needs jsonvalidate)
expect_valid_bibr12 <- function(path) {
  schema <- testthat::test_path("fixtures", "bibr12",
                                "bibr-export-v12.schema.json")
  valid <- jsonvalidate::json_validate(path, schema, engine = "ajv",
                                       verbose = TRUE, greedy = TRUE)
  errors <- attr(valid, "errors")
  details <- if (is.null(errors)) "" else
    paste(utils::head(paste(errors$instancePath, errors$message), 10),
          collapse = "\n")
  testthat::expect(isTRUE(as.vector(valid)),
                   paste(basename(path), "is not valid bibr 12.0:\n", details))
  invisible(path)
}
