# Query an LLM

Ask a large language model (LLM) any question you want about a vector of
text or the text from a search_text().

## Usage

``` r
llm(
  text,
  system_prompt,
  text_col = "text",
  model = llm_model(),
  params = list()
)
```

## Arguments

- text:

  The text to send to the LLM (vector of strings, or data frame with the
  text in a column)

- system_prompt:

  A system prompt to set the behavior of the assistant

- text_col:

  The name of the text column if text is a data frame

- model:

  the LLM model name (see
  [`llm_model_list()`](https://scienceverse.github.io/metacheck/reference/llm_model_list.md))
  in the format "provider" or "provider/model"

- params:

  a named list to pass to
  [`ellmer::params()`](https://ellmer.tidyverse.org/reference/params.html)

## Value

a list of results

## Details

You will need to get your own API key from
<https://console.groq.com/keys>. To avoid having to type it out, add it
to the .Renviron file in the following format (you can use
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)
to access the .Renviron file)

GROQ_API_KEY="key_value_asdf"

See <https://console.groq.com/docs> for more information

## Examples

``` r
if (FALSE) { # \dontrun{
text <- c("hello", "number", "ten", 12)
system_prompt <- "Is this a number? Answer only 'TRUE' or 'FALSE'"
is_number <- llm(text, system_prompt)
is_number
} # }
```
