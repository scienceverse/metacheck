# Plumber API Structure

This directory contains the Plumber API for the metacheck package.

The API wraps metacheck functionality to provide endpoints that can be accessed via HTTP requests, accepting bibr JSON files and mostly optional parameters.

This enables the use metacheck as a web service, as part of various pipelines, with other services, or a with a frontend.

## Running the API

### Using the prebuilt Docker Image
```bash
docker run -p 2005:2005 scienceverse/metacheck:latest
```

### From R

```r
# install.packages(c("plumber", "logger"), repos = "https://cloud.r-project.org/") # getting the necessary packages to run the API
library(plumber)
pr <- plumb("inst/plumber/api.R")
pr$run(host = "0.0.0.0", port = 2005)
```

### Using Docker Compose

```bash
cd inst/plumber
docker compose up --build
```


## Directory Structure

```
inst/plumber/
├── api.R                    # Main API entry point
├── run_api.R                # R script to run the API
├── run_api.sh               # Shell script to run the API
├── docker-compose.yml       # Docker Compose configuration
├── Dockerfile               # Docker image definition
├── endpoints/               # API endpoint definitions
│   ├── paper.R             # Paper analysis endpoints
└── utils/                   # Shared utility functions
    ├── validators.R        # Validation functions
    └── helpers.R           # Helper functions
```

## API Endpoints

### Paper Analysis (`/paper/*`)

These endpoints all accept **uploaded bibr JSON files** for analysis:

- `POST /paper/info` - Extract paper information (title, keywords, DOI, etc.)
- `POST /paper/authors` - Get author table
- `POST /paper/references` - Get bibliography/references
- `POST /paper/cross-references` - Get in-text citation cross-references
- `POST /paper/search` - Search text within the paper (requires `q` parameter)
- `POST /paper/module` - Run a specific metacheck module on the paper (requires `name` parameter)
- `POST /paper/check` - Get all metadata + run all/select metacheck modules on the paper (optional `modules` parameter)


## Key Features

### bibr JSON Input

Paper analysis endpoints accept **bibr JSON files** — the output of the
[bibr](https://github.com/scienceverse/bibr) extraction pipeline
(`POST /papers/extract`). They are read with metacheck's internal bibr
reader. GROBID XML is no longer accepted by the API (the metacheck R package
still reads it via `read()`).

### Module Support

Shadows the available metacheck modules as API endpoints.
The `/paper/module` endpoint allows you to run any metacheck module dynamically. Available modules are automatically detected from the package installation.
You can also use the `/paper/check` endpoint to run multiple/all available checking modules at once.

## LLM configuration

Set `GEMINI_API_KEY` to enable LLM-backed modules (provider
`google_gemini`). Optional: `METACHECK_LLM_MODEL` (default
`google_gemini/gemini-3.1-flash-lite-preview`) and `METACHECK_LLM_MAX_CALLS`
(default 200). Without the key, LLM modules fall back to non-LLM behavior.

## Example Usage

### Analyze a bibr JSON File

```bash
curl -X POST http://localhost:2005/paper/info \
  -F "file=@paper.json" \
  -F "fields=title,doi,keywords"
```

### Get Authors from JSON

```bash
curl -X POST http://localhost:2005/paper/authors \
  -F "file=@paper.json"
```

### Search Paper Text

```bash
curl -X POST http://localhost:2005/paper/search \
  -F "file=@paper.json" \
  -F "pattern=statistics"
```

### Run a Module

```bash
curl -X POST http://localhost:2005/paper/module \
  -F "file=@paper.json" \
  -F "name=ref_doi_check"
```

### Run Multiple Checking Modules

```bash
# Run all available modules
curl -X POST http://localhost:2005/paper/check \
  -F "file=@paper.json"

# Run specific modules
curl -X POST http://localhost:2005/paper/check \
  -F "file=@paper.json" \
  -F "modules=stat_p_exact,stat_check"
```


## Files

### `api.R`

Main entry point that mounts endpoint groups.

### `endpoints/paper.R`

Paper analysis endpoints - handles bibr JSON file uploads, reads papers via `read_paper()`, and runs metacheck functions/modules.
