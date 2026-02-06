# List LLM Models

List available LLM models for the specified platform.

## Usage

``` r
llm_model_list(platform = NULL)
```

## Arguments

- platform:

  The platform. If NULL, checks all platforms for which you have a valid
  API_KEY.

## Value

a data frame of models and info

## Details

For platforms other than groq, returns the value from the corresponding
ellmer::models_platform function.

## Examples

``` r
if (FALSE) { # \dontrun{
llm_model_list()
} # }
```
