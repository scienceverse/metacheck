# Feature Specification: Fix File Misclassification and CSV Read Errors

**Feature Branch**: `013-fix-r-file-misclassification`
**Created**: 2026-03-18
**Status**: Draft

## Background

Four distinct bugs surface when `run_index()` processes paper `09567976211040491`, a repository with 8 zipped sub-studies and multiple aggregate folders:

1. **Aggregate folder type inheritance**: When a folder is detected as an "aggregate" (>50 files) it is collapsed to a single sentinel path that the LLM classifies once. Every individual file in that folder then inherits the sentinel's type — including `.R` scripts and `.jpeg` images that should be `code` or `asset`, not `data`. This causes 378 files to reach the column extraction step, most of them unreadable (scripts, images), wasting time and emitting misleading "skipping (unreadable or empty)" messages.

2. **Empty / header-only CSV crash in delimiter sniffer**: `sniff_delimiter()` reads the first non-blank line with `readLines(con, n = 1)`. If the file is empty or has only blank lines, `readLines` returns `character(0)` rather than `""`, and the subsequent `if (nchar(trimws(line)) > 0) break` condition throws "argument is of length zero" — crashing the CSV read attempt for that file (e.g. `PickupsBehavProf.csv`).

3. **Duplicate files inflating extraction workload**: Unzipping multiple study archives produces folders whose contents are identical (same filenames, same byte sizes, same data). For this paper, the `Distributions_Pickups_Analysis` folder appears twice with ~97 matching CSVs each. These duplicates consume redundant LLM classification calls and column extraction time without adding any new information.

4. **Unbounded total data read per paper causing hangs**: The column extraction step calls `read_data_head(path, n_rows = Inf)` — loading every row — with no cap on cumulative data volume across all files in a paper. The aggregate folders for this paper contain CSVs of up to 172 MB / 10 million rows each. Processing ~215 such files totals tens of gigabytes of I/O, causing the pipeline to hang indefinitely.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Correct type for files inside aggregate folders (Priority: P1)

A researcher runs the pipeline on a paper whose study sub-folders each contain scripts, plots, and CSVs alongside each other. After the aggregate-folder sentinel is classified by the LLM, individual files expanded from it should be reclassified based on their own file extension where the extension is unambiguous — so that `.R` files are listed as `code`, `.jpeg`/`.png` files as `asset`, and data-bearing CSVs remain `data`.

**Why this priority**: This is the root cause of the 378-file column-extraction waste and the "skipping" noise. Fixing it makes file inventories accurate and reduces unnecessary processing.

**Independent Test**: Run `run_index("09567976211040491", download = FALSE)` on the already-downloaded paper. Check the structure output — `.R` and image files must not appear with `type = "data"`.

**Acceptance Scenarios**:

1. **Given** an aggregate folder whose sentinel is classified as `data`, **When** the sentinel is expanded back to individual files, **Then** files with unambiguously non-data extensions (`.R`, `.py`, `.jpg`, `.jpeg`, `.png`, `.gif`, `.bmp`, `.svg`, `.mp4`, `.mp3`, `.wav`) are reclassified to their correct type (`code` or `asset`) regardless of the sentinel's type.
2. **Given** an aggregate folder classified as `data`, **When** it contains CSV and R files, **Then** the CSV files retain `type = "data"` and the `.R` files are reclassified to `type = "code"`.
3. **Given** an aggregate folder classified as `code`, **When** it contains `.sav` and `.R` files, **Then** `.sav` files are reclassified to `type = "data"` and `.R` files retain `type = "code"`.

---

### User Story 2 - Graceful handling of empty or blank-only CSV files (Priority: P2)

The pipeline encounters CSVs that are empty or contain only blank lines (e.g., placeholder files, failed exports). Instead of throwing an internal "argument is of length zero" crash that surfaces as a confusing warning, the pipeline should detect the empty condition early and skip the file with a clear "unreadable or empty" message — no internal error.

**Why this priority**: The current error is caught by `tryCatch` so the pipeline continues, but the error message leaks an internal R condition and misleads operators into thinking something is structurally wrong. This is a UX/observability fix.

**Independent Test**: Create a zero-byte CSV and a blank-lines-only CSV in a test directory; run `read_data_head()` on each; confirm both return `NULL` without any warning about "argument is of length zero".

**Acceptance Scenarios**:

1. **Given** a CSV file that is completely empty (zero bytes), **When** `read_data_head()` is called on it, **Then** it returns `NULL` and emits "skipping (unreadable or empty)" without any "argument is of length zero" warning.
2. **Given** a CSV file containing only blank lines, **When** `read_data_head()` is called on it, **Then** it returns `NULL` cleanly.
3. **Given** a valid CSV with no trailing newline on the last line, **When** `read_data_head()` is called on it, **Then** it reads successfully and the "incomplete final line" warning from `read.table` is suppressed so it does not appear in pipeline output.

---

### User Story 3 - Deduplicate identical files before LLM classification (Priority: P2)

Before any LLM calls, the pipeline scans all files and identifies duplicates — files with the same basename and byte size. For text-based formats (CSV, TSV, TXT, DAT) the first 3 lines are also compared to confirm the match; for binary formats, name and size alone are sufficient. Only the first occurrence of each duplicate group is kept; the rest are silently dropped before the file tree is built and before the LLM is invoked.

**Why this priority**: Duplicate files waste LLM quota and column extraction time without adding information. For this paper, ~97 identical CSVs are processed twice, doubling the I/O load.

**Independent Test**: Run `run_index("09567976211040491", download = FALSE)` and confirm a "Removed N duplicate file(s)" message appears and the total path count sent to the LLM is reduced accordingly.

**Acceptance Scenarios**:

1. **Given** two folders with identical CSVs (same name, size, and first 3 lines), **When** the pipeline builds its file tree, **Then** only one copy of each duplicate appears in the file list sent to the LLM.
2. **Given** two files with the same name and size but different content (first 3 lines differ), **When** dedup runs, **Then** both files are kept.
3. **Given** a paper with no duplicate files, **When** dedup runs, **Then** no files are removed and no message is emitted.

---

### User Story 4 - Enforce per-paper cumulative data read cap (Priority: P3)

After deduplication, the column extraction step enforces a total data volume cap of 10 GB across all data files for a single paper. Files are processed in order; when adding the next file's size would push the cumulative total over 10 GB, that file and all subsequent ones are skipped, and a single log message records how much was already read. Files already skipped by the per-file 500 MB limit do not count toward the cap.

**Why this priority**: Without a total cap, papers with hundreds of large CSVs cause the pipeline to hang for hours. A 10 GB cap ensures the bulk runner makes forward progress on every paper.

**Independent Test**: Run `run_index("09567976211040491", download = FALSE)` after dedup and confirm the run completes in a reasonable time with a "stopping column extraction: total data read would exceed 10 GB" message if the aggregate volume exceeds the cap.

**Acceptance Scenarios**:

1. **Given** a paper whose data files sum to more than 10 GB, **When** column extraction runs, **Then** it halts at the file that would cross the cap, emits exactly one "stopping column extraction" message, and returns successfully with results from the files already read.
2. **Given** a paper whose data files sum to less than 10 GB, **When** column extraction runs, **Then** all data files are processed and no cap message appears.
3. **Given** the cap is crossed mid-paper, **Then** `structure.csv` and `columns.csv` are still written with the data collected up to that point.

---

### Edge Cases

- Aggregate folder whose sentinel type is `other` or `supplemental` — individual files with data-bearing extensions (`.csv`, `.sav`) should be promoted to `data`, not left as `other`/`supplemental`.
- A `.txt` file inside an aggregate folder — `.txt` is deliberately left as ambiguous in the existing rule set; these should NOT be overridden and should retain the sentinel's type.
- A CSV whose first 10 lines are all blank but the 11th has data — `sniff_delimiter` should still find a valid delimiter.
- Aggregate folders nested inside other aggregate folders — the fix must not double-apply type overrides.
- Two files with the same name but different sizes — not duplicates; both are kept.
- A paper where dedup removes files that would have pushed it over the LLM path limit — dedup runs first, so the reduced path count may allow a paper that previously hit `too_large` to succeed.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: After expanding aggregate sentinels back to individual files, the pipeline MUST apply extension-based type overrides that correct obviously wrong type assignments: script extensions (`.r`, `.rmd`, `.py`, `.m`, `.do`, `.sps`, `.jl`, `.js`, `.sh`) → `code`; image/audio/video extensions (`.jpg`, `.jpeg`, `.png`, `.gif`, `.bmp`, `.svg`, `.tiff`, `.mp4`, `.avi`, `.mp3`, `.wav`) → `asset`; recognized data extensions (`.csv`, `.sav`, `.dta`, `.sas7bdat`, `.xlsx`, `.xls`, `.rds`) → `data`.
- **FR-002**: The extension override in FR-001 MUST be bidirectional — it corrects both false positives (non-data file inside a `data`-classified aggregate) and false negatives (data file inside a `code`- or `supplemental`-classified aggregate).
- **FR-003**: Extension overrides MUST NOT apply to non-aggregate files — those files are already individually classified by the LLM and their classification must be preserved.
- **FR-004**: `sniff_delimiter()` MUST handle the case where `readLines()` returns `character(0)` (empty file or exhausted connection) without throwing "argument is of length zero".
- **FR-005**: When a file passed to `read_data_head()` is empty, `read_data_head()` MUST return `NULL` without emitting internal R error messages about zero-length arguments.
- **FR-006**: The "incomplete final line found by readTableHeader" warning from base R's `read.table` MUST be suppressed inside `read_data_head()` so it does not appear in pipeline output.
- **FR-007**: Files with extensions not in the override lists (e.g., `.txt`, `.dat`, unknown extensions) inside aggregate folders MUST retain the sentinel's inherited type unchanged.
- **FR-008**: Before building the file tree and before any LLM call, the pipeline MUST identify and remove duplicate files. Two files are duplicates if they share the same basename and byte size; for text-based formats (csv, tsv, txt, dat) the first 3 lines must also match. Only the first occurrence of each duplicate group is kept.
- **FR-009**: During column extraction, the pipeline MUST enforce a per-paper cumulative data read cap (`MAX_TOTAL_DATA_MB`, default 10 GB). When the next file's on-disk size would push the running total over this cap, that file and all subsequent data files MUST be skipped. Exactly one "stopping column extraction" log message MUST be emitted when the cap is first reached.
- **FR-010**: Files skipped by the per-file 500 MB limit MUST NOT count toward the cumulative cap (they were never read).

### Key Entities

- **Aggregate sentinel**: A synthetic path of the form `[N_files.ext]` that represents a collapsed folder with more than `AGGREGATE_THRESHOLD` files. It is classified by the LLM as a unit and its type is then propagated to all real files in the folder.
- **Extension override map**: A lookup table mapping file extensions to their definitive type (`code`, `asset`, or `data`). Extensions absent from this map are considered ambiguous and retain their inherited type.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: For paper `09567976211040491`, the column extraction step attempts zero `.R` and zero `.jpeg` files — the file inventory shows `code` and `asset` entries for these file types instead.
- **SC-002**: `PickupsBehavProf.csv` (an empty or malformed CSV) produces exactly one "skipping (unreadable or empty)" log message and zero "argument is of length zero" warnings.
- **SC-003**: No "incomplete final line" warning appears in pipeline output for any paper during a bulk run.
- **SC-004**: The total number of files reaching the column extraction step for paper `09567976211040491` drops from 378 to the true count of data files only (expected: well under 100).
- **SC-005**: Bulk runner passes paper `09567976211040491` without hanging or requiring a `^C` interrupt — the run completes and writes a result to `bulk_summary.csv`.
- **SC-006**: For paper `09567976211040491`, a "Removed N duplicate file(s)" message appears confirming at least one duplicate group was eliminated before LLM classification.
- **SC-007**: For papers whose aggregate data volume exceeds 10 GB, the pipeline emits exactly one "stopping column extraction" message and completes the run rather than hanging.

## Assumptions

- The extension override map is defined as a constant in `0_index.R` (alongside `ARCHIVE_EXTS`, `LLM_BATCH_SIZE`, etc.) so it is easy to extend in future.
- `.txt` and `.dat` are intentionally excluded from the override map because they are ambiguous (can be data or other content); this matches the existing `classify_by_rules` design.
- The fix does not change aggregate detection thresholds or sentinel generation — only the post-expansion type assignment step is modified.
- The "incomplete final line" suppression is applied only inside `read_data_head()`, not globally, to avoid hiding genuine warnings from other pipeline steps.
- Dedup runs on the full file list after unpacking and Excel explosion, before the file tree is built — this ensures both LLM classification and column extraction benefit from the reduction.
- The 10 GB cap (`MAX_TOTAL_DATA_MB`) is a constant in `0_index.R` and can be adjusted without code changes elsewhere.
- Files skipped by the per-file 500 MB limit are not counted toward the 10 GB cap because they are never actually read into memory.
