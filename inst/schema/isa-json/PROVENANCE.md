# ISA-JSON schemas (bundled)

The 21 `*.json` files in this directory are the official **ISA Model v1.0** JSON
Schemas, used to emit and validate metacheck's statistical-result exports as
ISA-JSON (see the JASP/jamovi -> ISA-JSON export path).

- **Source:** ISA-tools `isa-api`,
  `isatools/resources/schemas/isa_model_version_1_0_schemas/core/`
- **Upstream URL:** https://github.com/ISA-tools/isa-api (branch `master`)
- **Retrieved:** 2026-07 via the GitHub contents API + raw file download.
- **Why bundled:** the schemas `$ref` each other by relative filename, so
  offline validation (`jsonvalidate`) needs the whole set locally; fetching from
  GitHub at run time would add a network dependency.

Values in an ISA result export are typed with the **STATO** ontology (Statistical
Methods Ontology); STATO term IRIs are declared in the export's
`ontologySourceReferences` and referenced from each value's
`category.characteristicType.termAccession`. See the STATO IRI mapping used by
the exporter.

## Local normalizations (applied for offline `jsonvalidate` compatibility)

The upstream schemas were adjusted in two harmless, standards-preserving ways so
that `jsonvalidate`'s ajv engine resolves the cross-file `$ref`s from local disk:

1. **`$id` set to the bare filename** in every schema (upstream used full raw
   GitHub URLs). Two upstream files also had a *wrong* `$id` basename, now
   corrected to match their filename:
   `ontology_source_reference_schema.json` (was `ontology_source_schema.json`)
   and `material_attribute_value_schema.json` (was `material_attribute_schema.json`).
2. **Trailing `#` stripped from every `$ref`** (`"study_schema.json#"` ->
   `"study_schema.json"`). A bare `#` fragment means the document root, so this
   is semantically identical; jsonvalidate's file resolver otherwise tried to
   open a file literally named `...json#`.

## Bundled single-file schema (used for validation)

`isa_bundled_schema.json` is a generated, self-contained merge of all 21 core
schemas: each is placed under `$defs` keyed by its filename, and every
`"$ref": "x_schema.json"` is rewritten to `"#/$defs/x_schema.json"`. This is the
schema `stat_output_validate()` uses. It exists because jsonvalidate's ajv engine
(v1.5.0) throws `TypeError: e.replace is not a function` when the same schema is
both auto-resolved via a `$ref` AND passed again in the `reference` list (a
double `$id` registration). A single self-contained schema removes all
cross-file refs, so validation needs no reference list and no network — and
avoids that bug. Regenerate it from the 21 core schemas whenever they change.

A hand-built example (`sample.jasp` paired t-test) AND the live emitter output
for real jamovi and JASP files all validate `TRUE` against the bundled schema —
see `example-jasp-ttest.json` and `stat_output_validate()`.

To refresh: re-download the same `core/` folder from the upstream repo, re-apply
the two normalizations above, and re-run validation against the bundled example.
