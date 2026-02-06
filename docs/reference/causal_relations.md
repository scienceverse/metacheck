# Extract causal relations from sentence(s) via a Hugging Face Space

Sends one or more input sentences to a public Gradio app hosted on
Hugging Face (the *lakens-causal-sentences* Space), created based on
code by Rasoul Norouzi, retrieves the result via Server-Sent Events
(SSE), and returns a tidy data frame with one row per detected
cause–effect relation.

## Usage

``` r
causal_relations(
  sentence,
  rel_mode = "auto",
  rel_threshold = 0.5,
  cause_decision = "cls+span",
  timeout = 10,
  verbose = FALSE
)
```

## Arguments

- sentence:

  A character vector of one or more sentences to analyze for causal
  relations.

- rel_mode:

  Relation extraction mode. Options are `"auto"` (default) or
  `"neural_only"`.

- rel_threshold:

  Numeric threshold (default `0.5`) for deciding whether a relation is
  considered causal.

- cause_decision:

  Strategy for cause/effect detection. Options: `"cls_only"`,
  `"span_only"`, or `"cls+span"` (default).

- timeout:

  Maximum time (in seconds) to wait for the Hugging Face Space to return
  a result via SSE before aborting. Default is `10`.

- verbose:

  Logical; if `TRUE`, prints diagnostic information (URLs, status codes,
  event progression). Default `FALSE`.

## Value

A base `data.frame` with columns:

- `sentence` (character): the original input sentence,

- `causal` (logical): whether the sentence is causal per the model,

- `cause` (character): extracted cause span (or `NA`),

- `effect` (character): extracted effect span (or `NA`).

## Details

The function uses Gradio’s two-step queue API: (1) a POST request
enqueues the job and returns an `event_id`; (2) a GET request streams
`text/event-stream` frames until `event: complete`. Many Gradio apps
emit a **double-encoded** completion payload of the form
`["<final JSON string>"]`. This function unwraps that to obtain the
final JSON structure (an array of items containing `causal` and
`relations`) before parsing.

If a sentence has **no relations**, the output includes one row with
`cause = NA`, `effect = NA`, and the sentence’s `causal` flag (as
returned by the model). For sentences with **multiple relations**, the
function returns one row per relation.

## References

Norouzi, R., Kleinberg, B., Vermunt, J. K., & van Lissa, C. J. (2025).
Capturing causal claims: A fine-tuned text mining model for extracting
causal sentences from social science papers. *Research Synthesis
Methods*, 16(1), 139–156. https://doi.org/10.1017/rsm.2024.13

Hugging Face Model Card: rasoultilburg/SocioCausaNet
https://huggingface.co/rasoultilburg/SocioCausaNet

## Examples

``` r
if (FALSE) { # \dontrun{
# Single sentence
df1 <- causal_relations("Smoking causes cancer")
print(df1)

# Multiple sentences (batch)
df2 <- causal_relations(c("Insomnia causes depression.", "Rain leads to flooding."))
print(df2)

# Custom parameters and verbose diagnostics
df3 <- causal_relations(
  sentence = "Stress increases blood pressure.",
  rel_mode = "auto",
  rel_threshold = 0.4,
  cause_decision = "cls+span",
  timeout = 10,
  verbose = TRUE
)
print(df3)
} # }
```
