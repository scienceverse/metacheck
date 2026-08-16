# Vendored Behaverse `trial` schema

`behaverse-trial-v26.0608.json` is a pinned copy of the Behaverse Data Model
`trial` schema.

- Upstream: https://github.com/behaverse/schemas (`trial/schema.json`)
- Schema `$id`: `https://behaverse.org/schemas/trial/v26.0608/schema.json`
- Pinned version: **26.0608**
- Upstream licence: CC BY 4.0
- Declared JSON Schema draft: 2019-09

## Why it is vendored, and how metacheck uses it

metacheck emits per-instrument paradata (response times, trial/stimulus/option
channels) as Behaverse `TrialData` documents (see `R/behaverse-convert.R`). The
vendored schema is the pinned contract those documents are written and validated
against. It is **read, not executed**: `R/behaverse-validate.R` reads the
required-field lists and property types out of this file and checks documents in
native R, exactly as `R/psychds-validate.R` reimplements the Psych-DS checks
rather than running the upstream (Deno/TypeScript) validator.

Although the schema declares draft 2019-09, it uses no 2019-09-only keyword — the
only non-draft-07 construct is `$defs` with internal JSON-Pointer `$ref`s, which
draft-07 semantics resolve identically. Validation results are therefore the same
under either draft.

## Updating the pin

To bump to a newer Behaverse `trial` version: replace this file with the new
`trial/schema.json`, rename it to `behaverse-trial-v<version>.json`, update the
version referenced in `R/behaverse-convert.R` / `R/behaverse-validate.R`, and
re-run the validator's oracle test (which cross-checks the native-R checks
against a real JSON-Schema validator on the same documents).
