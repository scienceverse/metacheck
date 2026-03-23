# Research: Fix CSV Codebook Parsing Robustness

**Feature**: 018-fix-csv-codebook-parsing
**Date**: 2026-03-23

## Findings from Code Inspection

### Current structured parsing path (helper.R)

`parse_codebook()` (line 526) routes CSV/TSV/DAT files through:

1. `sniff_delimiter()` — reads up to 10 lines to find the first non-blank line, probes for `,;|\t`
2. `read.delim(path, sep, check.names=FALSE, stringsAsFactors=FALSE)` — reads the **entire file**,
   treating row 1 as the header unconditionally
3. `.extract_structured_codebook(df, src)` — calls `.find_codebook_cols()` on `names(df)`

### `.find_codebook_cols()` regex (lines 380–391)

**Variable column regex**: `(?i)^(var(iable)?|name|column|field|variable_?name|varname)$`
**Label column regex**: `(?i)^(label|description|desc|definition|meaning|explanation|text)$`

These only match exact single-word tokens. Common real-world variants that fail:
- "variable name" (space), "var_label", "question", "item", "Question Text", "Variable Description"

### Multi-level header problem

When a CSV has two header rows before data rows begin, `read.delim()` reads row 1 as headers.
Row 1 typically contains section/group labels ("Retrieval", "Demographics") rather than
column names. `names(df)` does not match the regex → returns NULL → silent skip of structured
path → LLM fallback.

**Fix decision**: Try all rows 1 through N (configurable, default = 5) as the candidate header
row, by calling `read.delim()` with `skip = k - 1` and `nrows` (or reading lines and re-parsing).
More precisely: re-read with `header = FALSE` and then scan rows for matching columns.

Simpler approach that avoids re-reading the file multiple times:
- Read with `header = FALSE` so all rows are data rows (no special treatment of row 1)
- Scan rows 1..N for one that matches the codebook-column regex
- Use that row as the header, treat rows below it as data

**Rationale**: Avoids multiple `read.delim` calls. One read with `header=FALSE` + scan.

### Empty description drop

Line 399: `rows <- df[nzchar(trimws(as.character(df[[cols$var_col]]))), , drop = FALSE]`

This correctly filters on non-empty **variable name**. But the description column is not filtered —
empty descriptions pass through and become `""` in the output, which is acceptable.

**Wait** — re-reading the spec issue: the problem is that empty description cells cause variable
rows to be dropped. Let me re-check...

Actually line 399 only drops rows where the **variable name** is empty. The description is
preserved as-is (including blank strings). So the silent drop of description-empty rows is NOT
the current behaviour. The real problem is the header detection failure producing an empty result.

### Encoding (latin1 fallback)

`read_data_head()` (line 46–55) already has a latin1 fallback. `parse_codebook()` uses a bare
`read.delim()` call without the same guard. The fix should mirror `read_data_head()`'s encoding
retry logic.

### Delimiter sniffing and comment/metadata rows

`sniff_delimiter()` skips blank lines but not comment/metadata rows. If a CSV opens with a line
like `# This codebook describes variables used in...` the sniffer uses that line for delimiter
detection. A `#` comment row has no `,` characters → sniffer returns `,` as default, but the
actual separator may be wrong. Fix: also skip lines starting with `#`.

### `parse_method` field

Currently `parse_codebook()` returns a plain `data.frame` with no provenance metadata. To add
`parse_method`, two options:

1. Add a `parse_method` column to the returned data.frame (value = `"structured"` or `"llm"`)
2. Return a list with `$result` and `$parse_method`

**Decision**: Option 1 — add `parse_method` column directly to the data.frame. It propagates
naturally through `do.call(rbind, ...)` in the caller, and adding to `coverage_df` is trivial.
The caller in `2_codebook_label.R` already constructs `coverage_df` from `codebook_vars_df`;
adding `parse_method` to that construction is a one-line addition.

### LLM fallback guarantee

Current code (line 583–602): if structured path returns a non-empty data.frame, LLM is skipped.
If it returns NULL or an empty data.frame, `readLines(path)` feeds `.run_llm_chunk_loop()`.

The fallback already runs — it is NOT currently silent-skipped. The bug is that structured parsing
sometimes returns a **non-empty** data.frame with junk rows (section labels as variable names)
rather than NULL. This happens when:
- The CSV has recognisable-looking headers but they happen to match the regex by coincidence
- Or the LLM fallback is not invoked because NULL was returned, but `readLines` fails

Actually the most common case: structured parsing returns NULL (because `.find_codebook_cols()`
fails to match), which DOES trigger LLM fallback. But the LLM fallback uses `readLines(path)`
which inherits the same encoding issues. The LLM then sees garbled text and returns fewer/no vars.

**So the actual chain of failure for the multi-level header case**:
1. Row 1 = section labels → `names(df)` = `c("Retrieval", "...2", "...3")`
2. `.find_codebook_cols()` finds no match → returns NULL
3. `result = NULL` → LLM fallback runs
4. LLM sees the raw CSV text including the multi-level header structure
5. LLM may or may not extract variables correctly (inconsistent)

**Root cause**: The structured path fails for multi-level CSVs, falling back to LLM, which is
less reliable and consumes LLM call budget.

## Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Header lookahead strategy | Read with `header=FALSE`, scan rows 1–N for matching columns | Single file read; no repeated I/O |
| N (lookahead depth) | 5 rows | Covers all observed real cases; bounded cost |
| Extended column name patterns | Add: "variable name", "var_label", "question", "item", "variable description", "variable label" | Observed in real codebooks during exploration |
| Encoding fallback | Mirror `read_data_head()` latin1 retry in `parse_codebook()` | Consistency; already proven pattern |
| `parse_method` placement | Column on returned data.frame, propagated to `coverage_df` | Simplest; no interface change to caller |
| Comment row skip in sniffer | Skip lines starting with `#` in `sniff_delimiter()` | Cheap; prevents delimiter mis-detection |
| Empty description rows | Keep current behaviour (retain rows with empty description) | Already correct; no change needed |
