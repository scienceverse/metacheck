# bibr export schema 12.0: the fixtures in fixtures/bibr12 are exports from
# bibr, and bibr-export-v12.schema.json is a copy of bibr's strict (producer)
# schema, docs/schema/bibr-export-v12.schema.json.
#
# platform_12_1.json is a 12.1 export, as convert_bibr(backend = "scivrs")
# saves it from the ScienceVerse Platform, of Sanchez Medero G (2026) Power
# concentration and political dynamics in liquid democracy, Open Research
# Europe 6:335, https://doi.org/10.12688/openreseurope.23863.2 (CC BY 4.0).

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
