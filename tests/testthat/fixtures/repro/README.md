# Reproducibility Check fixtures

Small base-R scripts used by `test-module-reproducibility_check.R`. Kept to base
R only (no `library()` calls) so `execute = TRUE` tests never need
`install_missing = TRUE` / network access — every script here runs with
whatever R is already installed.

- `ok.R` — reads `data.csv`, prints a `t.test()`. Runs cleanly.
- `writes_then_reads.R` / `reads_written.R` — a two-script pipeline: the first
  writes `intermediate.csv`, the second reads it back. Exercises run ordering
  from a real read-after-write dependency.
- `missing_input.R` — reads a file that is not in the fixture directory.
- `bad_setwd.R` — calls `setwd()` to an absolute path before reading data.
- `errors.R` — raises a real runtime error (`stop()`).
- `undefined_var.R` — references a variable no script in this fixture defines.
- `model_object.R` — the multi-statement shape the `.r_call_object_ref()` fix
  targets: a model fit once (`m <- lm(...)`), then described by `anova(m)`
  (a plain `fn(bare_name)` call) and `s$coefficients[2, , drop = FALSE]`
  (a `$`/`[`-chain reached through a one-hop rename, `s <- summary(m)`), so
  `model_ref` must resolve both to the same fitted model for
  `match_reported_output()` to see them as one candidate site. Uses table-
  shaped output (not a bare scalar/named-vector print) because
  `read_r_output()`'s extractors only recognise fixed-width tables and
  `stat = value` one-liners — a bare `[1] 0.75` or a named-vector print
  produces no result row at all, so nothing for `model_ref` to attach to.
