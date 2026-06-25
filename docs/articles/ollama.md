# Local AI with Ollama

The `metacheck` power module can use an AI language model to read
sections of a research paper and extract information about power
analyses — things like which statistical test was used, what sample size
was planned, and what effect size was assumed. How well this works
depends on the AI model you use, and the general ability of the model to
understand scientific text.

By default, metacheck expects a cloud API to do this. But there can be
cases where you want to make sure that data does not leave your
computer. This guide explains how to run AI models locally on your own
computer, using a free, open-source tool called
[Ollama](https://ollama.com). Running locally means:

- No internet connection required after the initial setup
- No API keys or accounts needed
- No usage limits or costs
- Your papers and any information processed by the AI stay on your own
  machine

The trade-off is that your computer does the work, so it will be slower
— especially the first time, while the model loads into memory.

## Setup

You need to repeat these step every time you want to use Ollama with
Metacheck on a new machine.

### What you need

- A computer with at least **8 GB of RAM** (16 GB recommended)
- R with the `metacheck` package installed
- At least **2–5 GB of free disk space** for the AI model (and more if
  you want to try larger models)

You do **not** need a graphics card (GPU). Ollama works on ordinary
laptop hardware.

### Step 1: Install Ollama

Ollama is a free program that lets you download and run AI models on
your computer. It runs in the background like a web server, and R talks
to it automatically.

**Windows:**

1.  Go to <https://ollama.com/download>
2.  Click **Download for Windows**
3.  Run the installer and follow the prompts
4.  Once installed, Ollama starts automatically in the background. You
    will see its icon in the system tray (bottom-right corner of your
    screen)

**Mac:**

1.  Go to <https://ollama.com/download>
2.  Click **Download for Mac**
3.  Open the downloaded `.dmg` file and move Ollama.app to your
    Applications folder
4.  You can eject the disk image and delete it now.
5.  Open Ollama from Applications — it will appear in your menu bar
6.  When you first open Ollama, it will add background items that you
    can manage in Login Items & Extensions
7.  Ignore the “Launch” interface and open a Terminal window (or use the
    terminal in RStudio – not the console) and use the terminal
    instructions below.

### Step 2: Download an AI model

Ollama needs to download the actual AI model before it can be used.
Think of this like downloading an app: you do it once, and then it is
stored on your computer.

The simplest way to do this is in the app itself: click the “Add Model”
button, search for “qwen2.5”, and click “Download” next to the
`qwen2.5:3b` model (or select the model, and ask a question).

If you feel comfortable using the command line, you can also do this
with a terminal command. Open a **Terminal** (Mac) or **Command Prompt /
PowerShell** (Windows) and run:

    ollama pull qwen2.5:3b

This downloads the **Qwen 2.5 (3 billion parameter)** model — a compact,
capable model that works well for the power module and is about 2 GB in
size. The download may take a while depending on your internet
connection.

> **Tip:** If you have more RAM (16 GB+) and want better accuracy, you
> can use a larger model (but it will be slower and take more disk
> space). For example: - `ollama pull qwen2.5:7b` – 4.7 GB, noticeably
> more capable - `ollama pull llama3.1` – 4.7 GB, good general-purpose
> alternative - `ollama pull smollm:135m` – 91 MB, tiny model for
> quickly testing workflows

To check which models you have downloaded, run:

    ollama list

------------------------------------------------------------------------

### Step 3: Verify Ollama is running

Ollama should start automatically when you install it. Anytime you want
to use ollama from metacheck, make sure it is running in the background.
You can check this by looking for the Ollama icon (in the system tray on
Windows, or menu bar on Mac).

To check, open your web browser and visit:

    http://localhost:11434

If you see a message like `Ollama is running`, you are ready to go.

If nothing appears, open the Ollama application manually (from
Applications on Mac, or search for “Ollama” in the Windows Start menu).

------------------------------------------------------------------------

### Step 4: Configure metacheck to use Ollama

In R, run the following three lines before using the power module. These
tell metacheck to use the AI, which model to use, and how many calls it
is allowed to make (which in the case of a free local model can be set
as high as you want):

``` r

llm_use(TRUE)                          # turn on LLM support
llm_model("ollama/qwen2.5:3b")         # use the local Ollama model
llm_max_calls(5000)                     # maximum number of AI queries per run
```

The model name must start with `ollama/` — this tells metacheck to send
queries to your local Ollama installation rather than a cloud service.

> **Note:** If you downloaded a different model in Step 2, replace
> `qwen2.5:3b` with the name you used (for example,
> `"ollama/qwen2.5:7b"` or `"ollama/llama3.1"`).

------------------------------------------------------------------------

## A simple first example

Here is a quick example to confirm that everything is working. We will
search a paper for any paragraph that mentions participants, then ask
the local AI model to describe who those participants were.

``` r

library(metacheck)

llm_use(TRUE)
llm_model("ollama/qwen2.5:3b")
llm_max_calls(5000)

# Load a single paper from the built-in psychsci dataset
paper <- psychsci[[84]]

# Find all paragraphs in the paper that mention participants
participant_paragraphs <- search_text(paper, "particip", return = "paragraph")

# Combine them into one block of text to send to the AI
combined_text <- paste(participant_paragraphs$text, collapse = "\n\n")

# Ask the AI to describe the participants
result <- llm(
  text = combined_text,
  system_prompt = "Based on these excerpts from a scientific paper, briefly describe the study participants: how many there were, who they were (e.g. students, patients, age group), and any other relevant characteristics reported."
)

cat(result$answer)
```

**The first query will feel slow** — this is normal. Ollama needs a
moment to load the model into memory the first time. Subsequent queries
in the same R session will be much faster.

If you see a short paragraph describing the participants, your setup is
working correctly and you are ready to move on to the power module.

## Run the power module

### On a single paper

``` r

library(metacheck)

llm_use(TRUE)
llm_model("ollama/qwen2.5:3b")
llm_max_calls(5000)

# Load a single paper (this uses paper number 84 from the built-in psychsci dataset)
paper <- psychsci[[84]]

# Run the power module
result <- module_run(paper, "power")

# See the result
result$traffic_light   # "green", "red", or "na"
result$summary_text    # a plain-English summary
result$table           # the full extracted data
```

**The first query will feel slow** — this is normal. Ollama needs a
moment to load the model into memory. Subsequent queries on the same
session will be much faster.

### On the full psychsci dataset

``` r

library(metacheck)

llm_use(TRUE)
llm_model("ollama/qwen2.5:3b")
llm_max_calls(5000)

paper <- psychsci
result <- module_run(paper, "power")
```

Processing the full dataset takes considerably longer (and could take
hours, depending on your hardware, the number of power related sentences
detected, and model size).

## Troubleshooting

### “Ollama is not running” or connection errors

- Open the Ollama application and wait a few seconds for it to start
- On Windows, look for the Ollama icon in the system tray; right-click
  and choose “Start”
- On Mac, launch Ollama.app and click the Ollama icon in the menu bar

### The model seems to freeze or takes very long

- This is normal on the first query while the model loads. Wait at least
  60 seconds before concluding something has gone wrong
- Larger models take longer; try `qwen2.5:3b` if you just want to check
  if your setup is working, as it is faster than the larger models.

### “Model not found” error

- Make sure the model name in
  [`llm_model()`](https://scienceverse.github.io/metacheck/reference/llm_model.md)
  exactly matches the name shown in `ollama list`
- The name must start with `ollama/`, for example `"ollama/qwen2.5:3b"`

### Out of memory errors

- Try a smaller model: `ollama pull qwen2.5:3b` and use
  `llm_model("ollama/qwen2.5:3b")`
- Close other applications to free up RAM

### Switching back to a cloud model

``` r

llm_model("groq")   # or another model name
```

## Quick reference

``` r

# Minimal setup to run the power module locally
library(metacheck)
llm_use(TRUE)
llm_model("ollama/qwen2.5:3b")
llm_max_calls(5000)
paper <- demopaper()
result <- module_run(paper, "power")
```
