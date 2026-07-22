# Cognitive tasks in OSD

How to express a behavioural/cognitive task (Stroop, IAT, n-back, Raven's) in
the **official Open Scale Definition format**, version 1.1.0
(<https://openscales.net/spec.php>).

**There is no second format.** An earlier draft of this document proposed one
("OTD"). It was wrong: a Stroop — parameters, conditions, contrast, norms and
all — validates as OSD 1.1.0 with **zero spec errors**. Everything below uses
only keys the published spec defines.

---

## 1. The mapping

A task is not a different kind of object from a scale; it is a scale whose
items are trials and whose subscales are conditions.

| Task concept | OSD key (official) | Notes |
| --- | --- | --- |
| Task identity, citation, licence | `scale_info` | `code` must be uppercase/digits/hyphens: `STROOP`, `IAT`, `NBACK` |
| **Design properties** (ISI, SOA, set size, load, modality…) | `parameters` | The taxonomy lives here. See §2 |
| Trial types (congruent / incongruent / neutral) | `items` | `type: "multi"` with an `option_set` for forced choice |
| Response alternatives | `option_sets` | Shared across trial types |
| Per-condition outcomes (RT, accuracy) | `dimensions` | One dimension per condition × indicator |
| **The contrast** (the DV) | `scoring` | `sum_coded` over `scores` with signed `items` coding. See §3 |
| Interpretation bands | `scoring.*.norms` | thresholds → labels |
| Trial-level output columns | `data_output.columns` | `rt` is **already** a standard OSD column |
| Instructions, condition wording | `translations` | Same `text_key` mechanism |

Nothing is dropped, renamed, or invented. `likert_options` is set to `null`
(the spec requires the key, and permits null when no `likert` items exist).

## 2. The task taxonomy goes in `parameters`

OSD's `parameters` block (spec §6) declares named, runtime-overridable values
with `type` / `default` / `description` / `options`. That is exactly the shape
of a task's design properties, and it is the key insight: **a Stroop with a
200 ms ISI and one with 2000 ms are the same task, differently parameterised.**

```json
"parameters": {
  "isi_ms":             {"type": "integer", "default": 1000, "description": "Inter-stimulus interval (ms)."},
  "soa_ms":             {"type": "integer", "default": 0,    "description": "Stimulus onset asynchrony (ms)."},
  "feedback_delay_ms":  {"type": "integer", "default": 0},
  "n_trials":           {"type": "integer", "default": 100},
  "n_stimuli":          {"type": "integer", "default": 4,    "description": "State-space complexity."},
  "n_response_options": {"type": "integer", "default": 4,    "description": "Action-space complexity."},
  "working_memory_load":{"type": "integer", "default": 0,    "description": "Items maintained (0-back = 0)."},
  "rule_complexity":    {"type": "integer", "default": 1,    "description": "Number of S-R rules."},
  "task_switching":     {"type": "boolean", "default": false, "description": "Operator/rule changes."},
  "stimulus_modality":  {"type": "choice",  "default": "visual",
                         "options": ["visual","auditory","tactile","audiovisual"]},
  "response_modality":  {"type": "choice",  "default": "keypress",
                         "options": ["keypress","vocal","mouse","saccade"]},
  "response_timing":    {"type": "choice",  "default": "speeded",
                         "options": ["speeded","self_paced","deadline"]},
  "predictability":     {"type": "choice",  "default": "unpredictable",
                         "options": ["predictable","unpredictable","cued"]},
  "signal_quality":     {"type": "choice",  "default": "clear",
                         "options": ["clear","degraded","masked"]},
  "feedback":           {"type": "choice",  "default": "none",
                         "options": ["none","trial","block","monetary"]},
  "workload":           {"type": "choice",  "default": "single_task",
                         "options": ["single_task","dual_task"]},
  "environmental_dynamics": {"type": "choice", "default": "static",
                         "options": ["static","dynamic","adaptive"]}
}
```

Why this is the right home rather than a new `taxonomy` block:

- It is **already in the spec**, so any OSD reader ingests it and unknown-key
  forward-compatibility (spec §1) is not even needed.
- Parameters are **overridable at runtime**, which is precisely how these
  properties behave: they are the knobs a replication turns.
- `working_memory_load: 0` vs `2` distinguishes a 0-back from a 2-back — the
  same Cognitive Atlas entry, the same name, a different task. A format that
  cannot express that difference cannot check it.

Countable properties (`isi_ms`, `n_stimuli`, `n_response_options`,
`working_memory_load`, `rule_complexity`) are objective. The ordinal ones
(`signal_quality`, `predictability`) are author judgements; they use `choice`
with a closed vocabulary so they are at least consistent, and they are optional.

## 3. A contrast is just signed scoring

The one thing that looked like it needed a new format — the dependent variable —
turns out to be plain OSD. Spec §9 allows `scores` (references to other
dimensions' computed scores) with an `items` object supplying signed coding.
A difference contrast is exactly that:

```json
"stroop_effect": {
  "method": "sum_coded",
  "scores": ["rt_incongruent", "rt_congruent"],
  "items":  {"rt_incongruent": 1, "rt_congruent": -1},
  "description": "RT incongruent - RT congruent (ms).",
  "norms": {"thresholds": [
    {"min": 0,  "max": 50,  "label": "Small interference"},
    {"min": 51, "max": 150, "label": "Typical interference"}]}
}
```

Accuracy uses the spec's `sum_correct` + `correct_answers` — a method OSD uses
for only 4 blocks across 1,095 scales, but which tasks need constantly.

## 4. The one genuine gap

OSD expresses **difference** contrasts natively. It cannot express a
**standardised** contrast.

The IAT D-score is `(M_incongruent − M_congruent) / SD_pooled`:

- numerator — `sum_coded` over `scores` with `{incong: 1, cong: -1}` — **fine**
- denominator — a pooled SD across trials — **not expressible**
- the division — there is no `divide` method and no ratio-of-scores — **not expressible**

`scoring.transform` (used 79× upstream, e.g. `[{"op":"multiply","value":6.25}]`)
applies a **constant** operation; it cannot reference another score. So the
Stroop, flanker, Simon and n-back all fit today; the IAT's headline DV does
not. That is a concrete, minimal proposal to put to the OSD maintainer — a
`divide_by_score` transform op, or a `ratio` method — rather than a reason to
fork the format.

## 5. What the Cognitive Atlas can fill

Full harvest of the Atlas `/task` API (n = 857, 2026-07-15):

| OSD field | Atlas source | Coverage |
| --- | --- | --- |
| `scale_info.name`, `description` | `name`, `definition_text` | 857 (100%) |
| `scale_info.url` (+ Atlas id) | `id` | 857 (100%) |
| `dimensions` / `scoring` (named) | `contrasts` | 529 (62%) |
| `scale_info.citation` | `citation` (+pmid) | 470 (55%) |
| `dimensions` (RT / accuracy) | `indicators` | 317 (37%) |
| `items` (conditions) | `conditions` | 272 (32%) |
| `scale_info.abbreviation` | `alias` | 259 (30%) → 113 usable acronyms |
| **`scoring` (computable)** | weighted `contrasts` | **7 (1%)** |
| **`parameters` (the taxonomy)** | — | **0 (0%)** |
| `scale_info.license` | — | **0 (0%)** |

I probed all 857 definitions with regexes for each taxonomy property. **Not one
property is evidenced in even 20% of tasks**; 15 of 24 are under 5% (`isi` 1%,
`soa` 0%, `task_switching` 0%, `working_memory_load` 1%). And a mention is not
a value: the letter n-back's definition says "identify letter repetitions that
occur n-trials preceding" — it never says what *n* is.

**Conclusion: the Atlas gives names, citations and design vocabulary. The
parameters are 100% authored, in any format.** That is the strongest argument
for staying inside OSD: if we must curate the content anyway, inventing a
format to hold it buys nothing.

Practical consequences:

- Harvest the Atlas for `scale_info` + `items`/`dimensions` skeletons (cheap,
  857 tasks, CC-BY), and treat every `parameters` block as curated.
- Mark provenance in `definition.metacheck` (metacheck's namespaced extension,
  ignored by other readers per spec §1 forward-compatibility):
  `task_source: "cognitive_atlas" | "curated"`.
- **Licensing:** the Atlas has no per-task licence field. Its content is CC-BY,
  but tasks it *names* (WAIS, Conners) are proprietary. `scale_info.license`
  describes the definition record, never a right to use the task.

Data quirks: `definition_text` is sometimes the literal string `"None"`;
`&#39;` occurs 272× and needs decoding; citations are **unordered** (the IAT's
first is not Greenwald 1998), so never treat `citation[0]` as canonical.

## 6. Two spec deviations found in the existing corpus

Reconciling the published spec against the 1,095 harvested `.osd` files turned
up two mismatches worth knowing:

1. **`method: "sum"` is not in the spec** (valid: `sum_coded`, `mean_coded`,
   `weighted_sum`, `sum_correct`, `max`, `min`) — yet 753 of 2,772 scoring
   blocks upstream use it. Readers must tolerate it; writers should not emit it.
2. **`item.reverse` is not a spec field.** The spec declares reverse-coding via
   signed weights in `scoring.items`, and explicitly notes the per-item
   `likert_reverse` boolean is *deprecated*. 32 items upstream still carry
   `reverse`. **metacheck's own `.osd` exporter emits `item.reverse`** — see
   `.scales_to_osd()` in `inst/modules/codebook_check.R`. It should additionally
   (or instead) emit a `scoring` block with the signed `items` map, which is the
   authoritative form. Tracked as a follow-up.

## 7. Decisions

### 7.1 Scope: harvest all 857 tasks

Every task in the Cognitive Atlas is harvested, including the fMRI paradigms
that no behavioural corpus will ever cite. There is no domain gate.

This is the opposite of the decision taken for PhenX, and the reason is that
the two risks are different. PhenX entries are gated by `text_ok` because their
names are topic labels, so scanning them against a manuscript produces false
positives. An Atlas task named "motor fMRI task paradigm" carries no such risk:
the name is distinctive enough that it will simply never match, and an entry
that never matches costs nothing but a row. Harvesting everything also keeps
the record complete, so a paper that does use an unusual paradigm is still
recognised.

### 7.2 Tasks that are also scales: the OSD definition wins

Some entries in the Cognitive Atlas are questionnaires rather than tasks. The
Atlas lists the Barratt Impulsiveness Scale, the CES-D, the Emotion Regulation
Questionnaire, the MMPI and the Pittsburgh Sleep Quality Index as "tasks",
because it is an ontology of things participants do, not a taxonomy of
instrument types.

When an entry exists in both sources, the OpenScales definition takes
precedence and the Atlas record is dropped. The reason is that the two records
are not of equal quality. An OpenScales definition contains the actual items,
the subscale structure, the scoring weights and often a reliability estimate.
The corresponding Atlas record contains a prose definition and, at best, a
named contrast. Keeping the Atlas version alongside it would add nothing and
would create a duplicate that the scale matcher then has to disambiguate.

Measured overlap: 19 of the 857 Atlas task names match a name in the `scales`
dictionary exactly, and a further 5 match after normalisation of variant
spellings. Examples of the near-misses are "Conners Comprehensive Behavior
Rating Scales" against "Conners Comprehensive Behavior Rating Scale", and
"Kessler Psychological Distress Scale (K6+)" against "Kessler Psychological
Distress Scale".

The dedupe rule must therefore normalise names rather than compare them for
equality. The normalisation drops case, punctuation and whitespace, and then
treats one name as a match for another when either is a prefix of the other and
the shorter name is longer than eight characters. The eight-character floor
exists because short names produce spurious prefix matches. One Atlas entry has
an empty name, which matches every dictionary entry under a naive prefix test,
so empty names must be rejected before the comparison runs.

### 7.3 Batteries are ignored

113 Atlas tasks record membership of a battery. OSD has no concept of one
definition containing another, and we do not add one. The battery relationship
is dropped on harvest and is not recorded anywhere.

### 7.4 Parameters are left blank

The `parameters` block is omitted from every harvested definition. Parameters
are written by hand, one task at a time, when a real task description is
available to write them from.

The Atlas has no `parameters` field. It does state some parameter values in its
prose, so the obvious idea is to extract those with regular expressions and
copy them. That was tried and rejected. The reason is worth recording, because
the idea will otherwise be proposed again.

An extractor was built for eight parameters and run over all 857 tasks. It
found 39 candidate values across 38 tasks, after correctly discarding tasks
that state several values for one parameter. Each of the 39 was then checked
against the sentence it came from, and **10 of them, which is 26 per cent, were
wrong or unverifiable**.

| Task | Extracted value | What the source actually says |
| --- | --- | --- |
| Short Penn Continuous Performance Test | `stimulus_duration_ms: 700` | The stimulus lasts 300 ms. The 700 ms is the blank page that follows it. |
| Montreal Cognitive Assessment | `working_memory_load: 1` | The phrase "vigilance (1-back)" appears in a screening questionnaire. The MoCA is not an n-back task. |
| Rapid automatized naming test | `n_stimuli: 10` | The source reads "five rows of 10 stimuli from a category of five items". The number 10 is a row length. |
| Working memory fMRI task paradigm | `n_trials: 10` | The number was matched inside "(10 seconds)", which is block timing. |
| Dimensions task | `n_trials: 25` | The number was matched inside "After 15-25", which is a range. |
| Reaction Time | `n_response_options: 5` | The pattern matched the word "five" inside prose about the task's history. |

These failures share one cause. A task description contains many numbers, and a
regular expression cannot tell which quantity a number belongs to. The Short
Penn Continuous Performance Test is the clearest case: its description contains
both 300 ms and 700 ms, and the extractor chose the wrong one while producing a
source quote that made the value look verified.

The yield does not justify the risk. Setting aside the errors, the extractor
recovered 29 plausible values across 28 of 857 tasks, which is roughly 3 per
cent. The n-back family, which motivated the attempt, yielded nothing usable at
all: the letter, spatial and multi-class n-back tasks each state four loads, so
all four are correctly rejected as variants rather than defaults.

A value extracted this way is worse than no value. It carries a source quote,
which makes it look checked, while being wrong a quarter of the time. Writing
`stimulus_duration_ms: 700` for a task whose stimulus lasts 300 ms is a
specific false claim about a published paradigm.

If the 29 plausible values are ever wanted, the honest use for them is as a
prompt for authoring rather than as data. The task and the sentence are shown
to a person, and the person supplies the value. That costs the same human
attention as authoring from scratch and removes the parser entirely.

Section 2 shows what a filled-in block looks like, using the Stroop, and
`STROOP.osd` in this directory is a complete instance. Both are references for
how to write a block, not a claim that any harvested task has one.

A harvested definition therefore carries `scale_info`, `items`, `dimensions`,
`scoring` and `translations`, and omits `parameters` entirely. Omitting the key
is valid: the specification lists `parameters` as optional in section 3, and a
reader that never sees it uses its own defaults.
