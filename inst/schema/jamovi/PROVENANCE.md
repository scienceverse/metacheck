# Vendored jamovi protobuf schema

`jamovi.proto` is a pinned copy of the jamovi communications/results schema.

- Upstream: https://github.com/jamovi/jamovi (`jmvcore/inst/jamovi.proto`)
- Pinned commit: `044224c4b848ce4cdd8506e56179c88412449804` (2026-07-28)
- `jmvcore` version at that commit: 2.7.38
- Upstream licence: GPL (>= 2)

## Why it is vendored, and how metacheck uses it

A `.omv` archive stores each analysis as a protobuf-serialised
`AnalysisResponse` message at `<index> <name>/analysis` inside the zip. Those
messages carry the analysis' results *structurally* — which is strictly better
than scraping the rendered `index.html` that sits alongside them:

* every `ResultsColumn` has a machine `name` (field 1) as well as a display
  `title` (field 2). The HTML renders only the title, which is frequently blank
  or cosmetic, so a blank one yielded an unnamed column and a generated junk key
  (`v1`, `v2`, ...). The `name` is stable and descriptive — `stat[stud]`,
  `err[stud]` — and even encodes the test variant;
* `ResultsCell` stores a native `int32` / `double` / `string` (fields 1-3), so a
  value is read at full stored precision: `6.99867707633638`, where the HTML
  shows a rounded display string;
* a missing cell is an explicit `ResultsCell.Other.MISSING` (field 4), so empty
  cells are identified structurally rather than by pattern-matching the em dash
  the HTML renders them as;
* sub-tables (a normality test nested under a t-test) keep their parent
  relationship, which the flattened HTML loses.

This file is **reference documentation, not executed**. metacheck decodes the
messages with a small native protobuf wire-format reader in
`R/stat-tables.R` (`.pb_fields()` and friends), rather than depending on
`RProtoBuf` — which is how jamovi's own R code reads these, but which requires
the protobuf C++ system libraries and is a heavy install, especially on Windows.
The decoder only needs the wire format (varint / length-delimited / fixed64),
plus the field numbers recorded here; it does not need a protobuf compiler.

The same approach is taken for the Psych-DS and Behaverse schemas: the upstream
contract is vendored for provenance and read by hand-written R, rather than
executing an upstream validator or code generator.

## Field numbers metacheck relies on

Taken from this pinned `jamovi.proto`. If a future jamovi release renumbers
these (a breaking protobuf change, and therefore unlikely), update the pin and
the constants in `R/stat-tables.R` together.

| Message            | Field            | No. | Wire type        |
|--------------------|------------------|-----|------------------|
| `AnalysisResponse` | `name`           | 3   | length-delimited |
| `AnalysisResponse` | `results`        | 7   | length-delimited |
| `ResultsElement`   | `name`           | 1   | length-delimited |
| `ResultsElement`   | `title`          | 2   | length-delimited |
| `ResultsElement`   | `table`          | 6   | length-delimited |
| `ResultsElement`   | `group`          | 8   | length-delimited |
| `ResultsElement`   | `array`          | 9   | length-delimited |
| `ResultsGroup`     | `elements`       | 1   | length-delimited |
| `ResultsArray`     | `elements`       | 1   | length-delimited |
| `ResultsTable`     | `columns`        | 1   | length-delimited |
| `ResultsColumn`    | `name`           | 1   | length-delimited |
| `ResultsColumn`    | `title`          | 2   | length-delimited |
| `ResultsColumn`    | `cells`          | 7   | length-delimited |
| `ResultsCell`      | `i` (int32)      | 1   | varint           |
| `ResultsCell`      | `d` (double)     | 2   | fixed64          |
| `ResultsCell`      | `s` (string)     | 3   | length-delimited |
| `ResultsCell`      | `o` (Other/enum) | 4   | varint           |

## Updating the pin

Replace `jamovi.proto` with the newer upstream copy, update the commit/version
above, re-check the field-number table, and re-run the extraction over the
`.omv` files in the test corpus to confirm the decoded tables are unchanged.
