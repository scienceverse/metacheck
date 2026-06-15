# GRIM Module — Development Log

- **Date:** 2026-06-10
- **Status:** working prototype; deterministic core validated (26/26 ground truth), optional LLM false-positive filter added; not yet formally validated for release
- **Module:** `inst/modules/grim.R`
- **Dependencies:** `scrutiny` (GRIM math, total-n dispersal); optional `ellmer`/ollama for the LLM filter
- **Support files:** `_stuff/grim_validate.R` (psychsci evaluation), `_stuff/grim_groundtruth.R` (26-case labeled test set), `_stuff/grim_diag.R` (extraction diagnostics), `_stuff/grim_llm_schema.json` + `_stuff/grim_llm_experiment.R` (LLM paragraph-extraction experiment), `_stuff/grim_llm_8b.R` (model-comparison test), `_stuff/grim_psychsci_table.rds` (results table)

## Update 2026-06-10 (round 2): scrutiny + optional LLM filter

**scrutiny integration.** The hand-rolled GRIM math was replaced by `scrutiny::grim()` / `grim_probability()` (string means preserve reported decimals; probability <= 0 is the testability gate). Added **total-n dispersal** (`grim_map_total_n` logic): a sentence with exactly two means and one candidate N is only flagged if no plausible split (n/2 +/- 5, both assignments) makes both means consistent. **GRIMMER was deliberately not added** — there is an open correctness bug in the scrutiny issue tracker; revisit when fixed. Ground truth held at 26/26 after the swap.

**df-based N recovery** (from round-1 suggestion 3) is in and pulls N from t-test degrees of freedom via `metacheck::stats()`/statcheck (df+1 paired, df+2 independent; fractional Welch df skipped; F(1,df) dropped because its condition means are usually continuous). A design-detection rule suppresses the df+1 candidate when two stated n's already sum to df+2.

**Paragraph fallback** (round-1 suggestion 2) is in but restricted to paragraphs offering exactly one N source. On psychsci it roughly tripled checkable means (44 -> ~160) but the paragraph tier is the least reliable (~2/3 of its flags are mispairings: scale means vs. "the 10 items", stimulus properties vs. participant counts). An `n_source` column (sentence/df/paragraph) records pairing confidence.

**Optional LLM false-positive filter** (`use_llm = FALSE` default). Runs *only on already-flagged means*, sends each flagged sentence to an LLM that classifies integer vs. non-integer data, and demotes confident non-integer judgments from "inconsistent" to "na". It can only ever *remove* flags, never add them; with `use_llm = FALSE` the module is fully deterministic and offline.

- **Key finding — model size matters a lot.** qwen2.5:3b and qwen3.5:4b returned `{"integer_data": false}` for *everything*, including a working-memory Likert mean and mean-age-in-years — they would suppress all of GRIM's true positives. Verified via raw-answer dump that this is the model's judgment, not a parsing bug. **llama3.1:8b** scored 5/5 on the discrimination test: kept the Likert and age flags, suppressed EEG amplitudes (uV), multi-rater ratings, and lexical-frequency properties. Module docs now recommend an 8B+ model.
- **Inherent limit confirmed.** The classifier only sees the flagged sentence. A lexical-frequency rating (M = 0.84) was suppressed when the prompt said "averaged across raters" but kept when the real sentence omitted that phrase. Consistent with the round-1 paragraph-extraction experiment: data-type info often lives in the Measures section, out of reach of local context.
- **Earlier LLM experiment** (`grim_llm_experiment.R`, schema in `grim_llm_schema.json`) showed paragraph-level extraction of *n* and *scale items* is unreliable (items almost never recoverable locally; n sometimes mis-paired). Conclusion that shaped the design: use the LLM as a **flag filter** (integer/continuous judgment from the sentence), not as an n/items extractor.

**Net effect on the FP taxonomy:** the EEG/amplitude and continuous-condition-mean false positives (the dominant class in the df and paragraph tiers) are removable with an 8B LLM filter, at zero recall cost to the integer-data true positives, but only when the sentence itself reveals the data type.

## Update 2026-06-10 (round 3): can we recover distant sample sizes?

**Question explored.** Most GRIM-checkable means have no same-sentence N. Could we recover it by searching the whole paper for "participant sentences" (subject nouns x reporting verbs + an integer) and pairing each mean with the nearest *earlier* such N — optionally letting the LLM pick? Tested on psychsci. Scripts: `grim_nsearch_explore.R`, `grim_nsearch_accuracy.R`, `grim_llm_nselect.R`.

**Structural findings (psychsci, 544 mean-sentences):**
- Only **19%** of mean-sentences carry a same-sentence N — the distance problem is dominant.
- Of the 81% without one, **95% have *an* earlier participant-N sentence** — the information almost always exists in the paper.
- But the median gap is **24 sentences** (mean 36), with a **median of 5 competing earlier N-sources** (up to 31). So the problem is *selection*, not *availability*.

**Accuracy against ground truth** (means with a known same-sentence `n=`, hidden, then recovered; n = 15-16 cases — small, treat as indicative):
- Nearest-earlier participant-N is correct **~44-47%**; the correct N is not among *any* earlier participant sentence **~44%** of the time (subgroup means' N often appears only in their own sentence).
- LLM (llama3.1:8b) selecting among the 8 nearest candidates: **60%** — a real but modest lift over nearest-earlier, on a tiny sample.

**Decision: do NOT use recovered distant Ns for flagging.** 60% is far too unreliable to act on, and the error is in the dangerous direction — a wrong N either *fabricates* an inconsistency in a clean paper or *masks* a real one. This breaks the safety asymmetry that makes the existing LLM filter acceptable (that filter only ever *removes* flags; a mistake costs a missed detection, never a false accusation). N-selection to *raise* a flag would let the LLM create false accusations, which is unacceptable for an integrity tool.

**What stays:** flagging remains on the reliable tiers only — same-sentence regex Ns and df-derived Ns (`stats()`/statcheck) — plus the safe optional LLM *removal* filter. The participant-sentence + nearest-N machinery and the LLM N-selector are kept in `_stuff/` as exploratory code, not wired into the module.

**Possible safe future use (not implemented):** an LLM-selected N could *confirm* a flag already raised by a reliable tier, or populate an info-only "candidate N for manual check" column — neither asserts a new inconsistency. Revisit only with a larger labelled set; 15 cases can't distinguish 60% from a coin flip with confidence.

## Update 2026-06-10 (round 4): full-text LLM extraction — the key finding

Round 3 was re-examined and partly **invalidated**, then the stronger idea (send the LLM the actual paper text, not regex snippets) was tested. Scripts: `grim_nselect_errors.R`, `grim_fulltext_extract.R`.

**Round-3's "60%" was a measurement artifact.** Inspecting the errors: the ground-truth set (means with a same-sentence `n=`) was dominated by `Mantel-Cox χ²(1, n=NN)` and `Wald` tests whose "means" are reaction times in seconds (`M = 307.10 s`) — not GRIM-checkable at all. And "truth among candidates" was only 60%, i.e. the **regex N-extractor never even captured the right number 40% of the time**. The bottleneck was candidate extraction, not LLM selection. So round-3's pessimistic conclusion was based on a bad benchmark.

**Full-text extraction works — and beats regex on the core problem.** Sending the whole paper text to llama3.1:8b (native ollama API, `num_ctx = 16384`) and asking for each mean + the N it was computed from + integer/continuous type, parsed as free-text JSON:

- **Correct subgroup pairing from prose alone.** Working-memory paper: returned `M = 3.03, n = 22 (girls)` and `M = 2.63, n = 31 (boys)` and flagged 2.63 — the right N per subgroup, which proximity/regex could not do. Self-esteem paper: `M = 2.75, n = 94` and `M = 1.24, n = 111` correctly paired to the two groups (matches hand-verified truth).
- **Correct data-type gating.** EEG paper: reaction times and N1pc amplitudes typed `continuous` and given n = 0, i.e. the LLM declined to GRIM-check them. Desired safety behaviour.

**But it introduces a worse failure mode: hallucination.** The same runs produced means that do not appear in the papers (`M = 7`, `M = 9`, `M = 4.00`), mangled means (2-decimal precision dropped), confabulated Ns (`n = 476`, `n = 1266` for studies that size), and inconsistent typing (age called continuous in one paper, integer in another). **A GRIM flag on a hallucinated mean is a false accusation invented from nothing** — unacceptable for an integrity tool, and strictly worse than the deterministic module's failure mode (a missed or mispaired real mean).

**Conclusion / design decision.** Do not let the LLM *extract the means*. The deterministic regex never hallucinates a mean; that is its key virtue. The LLM should be confined to the job it is genuinely good at and which regex/proximity fail at: supplying the **N and data-type for a mean the regex already found verbatim**. This is the hybrid worth building next:

> **Hybrid (future, RAM-permitting):** regex extracts the verbatim mean strings (no invented numbers) -> for means lacking a same-sentence/df N, send the paper text + the *specific extracted mean* to the LLM and ask only "what n was THIS mean computed from, and is it integer data?" -> GRIM-check with the returned n. The LLM can mis-pair (a wrong N), but it can never invent a mean to accuse, and a mis-paired N can still be shown as `n_source = "llm (unverified)"` with a conservative traffic light.

**Practical blocker:** llama3.1:8b at the context needed for a full psychsci paper (~15k tokens, `num_ctx >= 16384`) needs ~6.5 GiB free RAM; this machine hovered at ~4-4.5 GiB and mostly returned HTTP 500 (one run squeaked through). The full-text/hybrid path needs a machine with more memory (or a smaller-but-capable model). `grim_fulltext_extract.R` has a preflight RAM warning and is ready to rerun when memory allows.

## Update 2026-06-10 (round 5): slice-based hybrid (memory workaround) + verdict

The full-text approach (round 4) was blocked by RAM. Workaround built and tested: don't send the full text — send a **targeted slice**. Scripts: `grim_context_budget.R`, `grim_hybrid.R`.

**Context budget (psychsci).** A targeted slice = participant/method sentences (where total & subgroup N live) + a +/-2-sentence window around each mean. Median **3,336 chars, 15x smaller than full text**; fits an 8k ollama context in **96%** of papers. So the memory wall is avoidable without a GPU.

**GPU note.** The machine has an RTX A1000 6GB, and ollama supports CUDA, but a Python process (PID 12016) was holding ~5.5 GB of VRAM, leaving 513 MiB — which is why ollama ran on (constrained) system RAM. Freeing that VRAM would let the 8B run on-GPU and make full-text feasible too, but the slice approach was chosen as it works regardless.

**Hybrid design (safety-preserving).** REGEX extracts the verbatim mean strings (LLM never invents a mean to accuse). For a mean lacking a reliable N, the LLM gets only the slice + that one mean and returns the **N only** — *not* the data-type. Two reasons the LLM does N-only:
- It is genuinely good at N-pairing (the part regex/proximity fail at).
- It is *unreliable* at integer/continuous typing: asked in the abstract, the 8B answers `integer:false` even for a clear Likert working-memory mean of 3.03 (raw output confirmed, consistent with rounds 2 & 4). Data-type therefore stays with the module's deterministic unit filter (+ the separate shipped LLM integer-*filter*).

**Safety rails (both fire correctly in testing):**
- *n-in-slice guard*: the LLM's returned n is accepted only if regex actually found that number in a participant sentence in the slice. On the EEG paper, all 25 continuous means were rejected (guessed Ns not in valid sentences) — zero false flags.
- *deterministic type gate*: means in non-integer-unit contexts (uV, ms, %, ...) are excluded before GRIM regardless of the LLM.

**Verdict (after testing the idea three ways: candidate-list selection, full text, targeted slice).** The architecture is sound and the safety rails work — no hallucinated means, continuous data correctly excluded, and the genuine true positive (self-esteem M = 2.75, n = 94) was recovered. **But the core N-pairing is still not reliable enough to flag on when a paper reports multiple sibling group Ns** (the common case). Across runs the 8B wobbled between the two group sizes (94 vs 111) for the same means — roughly a coin flip on which group a mean belongs to. A wrong-but-valid N silently mis-scores GRIM (passes a real inconsistency or fails a clean mean).

**Therefore:** keep LLM-recovered distant Ns **out of the flagging path**. The deterministic same-sentence + df tiers remain the only flag sources; the shipped LLM integer-*filter* (removal only) stays. The slice-hybrid is preserved in `_stuff/` as the best-available exploratory approach and the right design *if* pairing reliability improves (larger/instruction-tuned model, or constraining the choice to the two nearest group Ns and reporting both). Recommended safe use today: surface an LLM-recovered N as an **info-only "candidate N (unverified)"** column for manual checking — never as an automatic flag.

---
*Round 1 log below (same date, earlier).*


## What the module does

Implements the GRIM test (Brown & Heathers, 2017): a mean of n integer values reported to d decimals must be within rounding distance of k/n for some integer k. The test only has diagnostic value when `n * items < 10^d` (so in practice: 2-decimal means with n < 100).

Pipeline:
1. Find sentences containing both a mean (`M = 4.32`, `mean age = 23.5`) and a sample size (`n = 24`, `24 participants`, `sample of 24`, `24 in Experiment 1b`).
2. Filter out means with non-integer units (time, physical, currency, %), latency/duration sentences, children's ages (< 18; year-fractions), and ages in months/days (infant studies; computed from birth dates).
3. Filter out N-candidates that are exclusion counts ("we excluded 3 participants", "Thirteen participants were dropped"), design labels ("Study 2 participants"), or parsing artifacts (n < 5).
4. A mean is flagged only if GRIM-inconsistent with **every** candidate N in the sentence (lenient any-N rule).
5. The table reports `min_items`: the smallest number of scale items (1–4) that would make the mean possible — no-power granularities count as "cannot rule out".
6. Traffic light: `na` (nothing checkable), `green` (all pass), `yellow` (flags — deliberately not red, because a flag means "verify the scale and n", not "error").

No new dependencies; fully offline (GDPR-clean). GRIM math is ~15 lines, verified against hand-computed cases (5.19/28 inconsistent, 5.18/28 consistent, etc.).

## Performance (current)

**Ground truth (22 labeled sentences: 8 planted inconsistencies, 6 known-consistent, 8 false-positive traps):**

| Metric | Value |
|---|---|
| Precision | **1.00** |
| Specificity | **1.00** |
| Sensitivity | 0.62 (5/8; see known misses below) |

**psychsci (250 papers):** 25 papers with checkable means, 44 checkable means, **6 flagged — all six hand-verified as correctly paired, genuine GRIM inconsistencies** (4 adult-age means, 2 score/scale means; every flag shows `min_items` 2–3, i.e., explainable by a multi-item scale).

## Iteration history (what moved the numbers)

| Iter | Change | Effect |
|---|---|---|
| 1 | First version | Unit blacklist contained `rate\w*` → matched "**rated** items", excluding exactly the Likert sentences GRIM targets |
| 2 | Fix blacklist; fix data.frame recycling for multi-mean sentences | Ran, but only **2 checkable means in 250 papers** |
| 3 | Diagnosis: only 37/756 mean-sentences contain "n =" — psych papers write "24 participants (age: M = 25.1)". Added participant-count nouns, "sample of N", per-mean (not per-sentence) %/currency exclusion | 66 checkable, 29 flagged — but most flags were wrong-N pairings |
| 4 | Excluded exclusion-counts ("excluded **3** participants"), `Study 2` lookbehinds, n ≥ 5 floor, ≤ 2 intervening adjectives ("70 TS control participants"), `min_items` no-power bug fixed | 93 checkable, 19 flagged — 14 of 19 were age means tested against **gender subgroup counts** |
| 5 | Removed gender counts as N-candidates; word-number conversion ("Sixty participants"); trailing `years/months/days` age detection; child-age and infant-age exclusions | 47 checkable, 8 flagged |
| 6 | Post-context exclusion verbs ("Thirteen participants **were dropped**") | 42 checkable, 7 flagged — one new FP from over-aggressive post-context |
| 7 | Post-context requires verb adjacency ("were dropped" yes; "35 **after exclusions**" no — that 35 is a legitimate final N) | **44 checkable, 6 flagged, all verified correct** |

## Insights

1. **Pairing N with the mean is the entire problem.** The GRIM math is trivial; ~90% of development effort went into not pairing a mean with the wrong number. Every false positive in every iteration was a pairing error, never a math error.
2. **The systematic FP taxonomy** (worth reusing for any mean/SD/N extraction module — GRIMMER, effect-size recomputation, etc.):
   - exclusion/attrition counts (verb before *or after* the number)
   - design labels ("Study 2", "Experiment 1")
   - subgroup counts (gender breakdowns are the killer for age means)
   - spelled-out sentence-initial numbers (APA style forces "Sixty participants")
   - children's ages (year-fractions), infant ages in days/months
   - units: time/physical/currency/% — but per-mean, not per-sentence (demographic sentences mix "% female" with checkable age means)
3. **Lenient any-N consistency is the right default**: flag only if inconsistent with *every* candidate. It converts pairing uncertainty into missed detections instead of false alarms — the correct trade for a tool whose flags request manual verification.
4. **`min_items` makes flags honest.** Every psychsci flag was explainable by a 2–3-item scale; reporting that in the table (and using yellow, not red) sets the right expectation. A flag means "verify scale items and n", not "fabrication".
5. **Age means are the main GRIM substrate in practice.** 4 of 6 psychsci flags are adult age means — integer-years assumption is usually but not always right (DOB-computed ages break it). The report text says so.
6. **psychsci is an excellent iteration corpus** — real grobid-converted text with all the messiness (spelled-out numbers, multi-experiment demographic mega-sentences, mixed-unit sentences).

## Known limitations / misses

1. **Spelled-out Ns don't enter the sentence filter** (2 of 3 ground-truth misses). `words_to_digits()` exists and works, but runs *after* the digit-requiring sentence filter. Fix: apply conversion before filtering, or add word-number alternations to `n_rxs`. Expected effect: sensitivity 0.62 → ~0.88. **This is the first thing to fix.**
2. **Lenient any-N rule misses cross-group matches** (1 of 3 misses): "n = 22, M = 4.13; n = 31, M = 4.39" — 4.13 is impossible for 22 but possible for 31, so it passes. Positional pairing (nearest-N, or N-before-mean-within-parentheses) could recover these, at precision cost. Measure before adopting.
3. **N must be in the same sentence.** Method-section Ns are not carried to Results-section means. A paper-level fallback (unique total N from the participants paragraph) would expand coverage substantially but needs careful subgroup handling.
4. **Means in tables** are effectively unreachable (grobid table cells, no sentence structure). Different extraction problem entirely.
5. **No GRIMMER** (SD granularity) — same extraction, more math; natural extension once pairing is trusted.

## Suggestions for future development

1. **Fix the word-number ordering bug** (one-line concept; rerun both validation scripts).
2. **Expand the N search window with `expand_text()`** (addresses limitation 3): after identifying a sentence with M/SD but no N, run `text_expand(..., plus = 1, minus = 1)` (or `expand_to = "paragraph"`) and apply `extract_ns()` to the expanded context. The lenient any-N rule makes this degrade gracefully: a wider window adds candidates, which makes flagging *more* conservative, so coverage grows without sacrificing precision. Start with ±1 sentence and measure the checkable-means yield before going to paragraph level.
3. **Recover N from t-test degrees of freedom**: independent-samples t has df = n1 + n2 − 2 (confirmed on psychsci flag [1]: n = 22 and 31, t(51) = 22 + 31 − 2), paired/one-sample df = n − 1. Since the test type is unknown from the regex, add both df + 1 and df + 2 as lenient N-candidates for means in the same sentence as the test. Caveats: (a) Welch-corrected t gives fractional df (psychsci flag [2]: t(23.92) with N = 56) — skip non-integer df; (b) df recovers the *total* N, while group means belong to group n's — as lenient candidates this is still safe, and for one-sample/paired designs the df-derived n is exactly the right n. Implementation note: `metacheck::stats()` already extracts test statistics with df, so in a report chain the values are available via `get_prev_outputs("stat_check", "table")` at no extra cost — and a mismatch between df-derived N and stated N is itself a reportable inconsistency (the "N-vs-df consistency" module idea).
2. **Validate for release** following the team's standard: run on a held-out manually-coded sample (the psychsci table in `_stuff/grim_psychsci_table.rds` is a start, but the 22-case ground truth was written by the developer — an independent coder should label flags/misses), then fill in the `<validation>` tag in the module docs.
3. **Consider `scrutiny` as a cross-check, not a dependency**: run `scrutiny::grim_map()` on the extracted (mean, n) pairs in the validation script to confirm the in-module implementation agrees (it implements more rounding variants).
4. **GRIMMER next**: the extraction already captures SDs incidentally (they're in the same parentheticals); `scrutiny`'s GRIMMER logic or a direct implementation would roughly double the value of the same extraction work.
5. **Percentage GRIM**: "48% female" with known N is GRIM-testable (k/N). Same pairing machinery; percentages were excluded in v1 for precision.
6. **Share the N-extraction helper**: `extract_ns()` + `words_to_digits()` + the FP taxonomy (exclusion verbs, design labels, subgroup counts) belong in `R/` as a shared helper — the effect-size module, power-reproduction module, and any future GRIMMER module all need exactly this.
7. **LLM-assisted pairing as an optional layer** (like `power` does): regex finds candidates, a local LLM resolves which N belongs to which mean in multi-group sentences. Keeps the offline default, improves recall when enabled.
8. **Report wording**: current text explains the multi-item-scale caveat. Once validated, add expected false-flag rates to the "How It Works" section per team convention.
