# Local MetaCheckLLM

> Small guide into using local LLM's via Ollama to run the llm() command 


Written by a local LLM and Levi Baruch

---
## Switching to a Local Ollama Model

`llm.R` can query any OpenAI‑compatible endpoint. Ollama exposes a local HTTP API that follows the same specification, so you can simply point `ellmer` to it.
This is only useful if you have enough VRAM and a competent GPU, or if you have a Macbook with enough RAM. 

The reason for doing this is to escape the rate limiting of the groq client (when using the free version)

### 1. Install Ollama

```bash
# macOS
brew install ollama

# Linux (Ubuntu/Debian)
curl -fsSL https://ollama.com/install.sh | sh

# Windows (WSL or native)
# download from https://ollama.com/download
```

### 2. Pull a Model from Ollama

```bash
ollama pull llama3.1
# or any model you want
```

### 3. Set Up `ellmer` to Use the Local Endpoint

Enable the Ollama output (Settings -> Expose Ollama to the network and expand context)
`ellmer` automatically uses the `OPENAI_API_BASE` environment variable if it’s set. Add the following to your `.Renviron` (or set it in the R session):

```bash
# ~/.Renviron
OPENAI_API_BASE="http://localhost:11434/v1"
OPENAI_API_KEY="ollama"   # the key value is ignored by Ollama
```

Or, if you only want to do it temporarily:
```r
Sys.setenv(
  OPENAI_API_BASE = "http://localhost:11434/v1",
  OPENAI_API_KEY  = "ollama"
)
```

### 4. Update the Default Model

```r
# Set the model name to the Ollama model you pulled. Its important to add the ollama/ prefix, since otherwise it will not be recognised.
llm_model("ollama/llama3.1")
```

### 5. Run `llm()` As Usual

```r
answers <- llm(
  text = c("What is 2 + 2?", "Tell me a joke."),
  system_prompt = "You are a helpful assistant."
)

print(answers)
```

The queries will now be served locally, with no external API cost. 

### Notes on Ollama

You can download different models, with differing sizes. Generally smaller models are quicker while larger models are better. Try and use what fits your machine. 

Note that initially local models might seem to be slower; this is because the model needs to initialize before running prompts. Try not to reload models many times since this wastes a lot of your time.


## Advanced Configuration

```r

# Query with custom parameters (e.g., temperature, max_tokens)
answers <- llm(
  text = "Explain recursion.",
  system_prompt = "Provide a concise explanation.",
  params = list(
    temperature = 0.2,
    max_tokens = 150
  )
)
```
