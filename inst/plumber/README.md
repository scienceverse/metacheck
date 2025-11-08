# Plumber API Structure

This directory contains the Plumber API for the metacheck package.

The API wraps metacheck functionality to provide endpoints that can be accessed via HTTP requests, accepting GROBID XML files and mostly optional parameters.

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

These endpoints all accept **uploaded GROBID XML files** for analysis:

- `POST /paper/info` - Extract paper information (title, keywords, DOI, etc.)
- `POST /paper/authors` - Get author table
- `POST /paper/references` - Get bibliography/references
- `POST /paper/cross-references` - Get in-text citation cross-references
- `POST /paper/search` - Search text within the paper (requires `q` parameter)
- `POST /paper/module` - Run a specific metacheck module on the paper (requires `name` parameter)
- `POST /paper/check` - Get all metadata + run all/select metacheck modules on the paper (optional `modules` parameter)


## Key Features

### GROBID XML Input

Paper analysis endpoints accept **GROBID XML files** which are directly analyzed with `metacheck::read_grobid()`. You can get the GROBID TEI XML by processing PDFs with a GROBID server, for example using the pdf2grobid in metacheck function,
(not available in the API due to strategic reasons) or the official GROBID client.

### Module Support

Shadows the available metacheck modules as API endpoints.
The `/paper/module` endpoint allows you to run any metacheck module dynamically. Available modules are automatically detected from the package installation.
You can also use the `/paper/check` endpoint to run multiple/all available checking modules at once.

## Example Usage

### Analyze a GROBID XML File

```bash
curl -X POST http://localhost:2005/paper/info \
  -F "file=@paper.xml" \
  -F "fields=title,doi,keywords"
```

### Get Authors from XML

```bash
curl -X POST http://localhost:2005/paper/authors \
  -F "file=@paper.xml"
```

### Search Paper Text

```bash
curl -X POST http://localhost:2005/paper/search \
  -F "file=@paper.xml" \
  -F "q=statistics"
```

### Run a Module

```bash
curl -X POST http://localhost:2005/paper/module \
  -F "file=@paper.xml" \
  -F "name=check_dois"
```

### Run Multiple Checking Modules

```bash
# Run all available modules
curl -X POST http://localhost:2005/paper/check \
  -F "file=@paper.xml"

# Run specific modules
curl -X POST http://localhost:2005/paper/check \
  -F "file=@paper.xml" \
  -F "modules=exact_p,statcheck,osf_check"
```


## Files

### `api.R`

Main entry point that mounts endpoint groups.

### `endpoints/paper.R`

Paper analysis endpoints - handles GROBID XML file uploads, reads papers via `read_paper()`, and runs metacheck functions/modules.


