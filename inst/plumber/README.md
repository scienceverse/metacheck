# Plumber API Structure

This directory contains the Plumber API for the papercheck package.

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
│   └── grobid.R            # GROBID PDF processing endpoint
└── utils/                   # Shared utility functions
    ├── validators.R        # Validation functions
    └── helpers.R           # Helper functions
```

## API Endpoints

### Paper Analysis (`/paper/*`)

These endpoints accept **uploaded files** (PDF or GROBID XML) for analysis:

- `POST /paper/info` - Extract paper information (title, keywords, DOI, etc.)
- `POST /paper/authors` - Get author table
- `POST /paper/references` - Get bibliography/references
- `POST /paper/cross-references` - Get in-text citation cross-references
- `POST /paper/search` - Search text within the paper (requires `q` parameter)
- `POST /paper/module` - Run a specific papercheck module on the paper (requires `name` parameter)
- `POST /paper/check` - Get all metadata + run all checking modules on the paper (optional `modules` parameter)

### GROBID Processing (`/grobid/*`)

- `POST /grobid/pdf2grobid` - Convert PDF to GROBID XML format

## Key Features

### Flexible Input

Paper analysis endpoints accept **either**:

1. **PDF files** - Automatically processed via GROBID, then analyzed
2. **GROBID XML files** - Directly analyzed with `papercheck::read_grobid()`

### Module Support

The `/paper/module` endpoint allows you to run any papercheck module dynamically. Available modules are automatically detected from the package installation.

### Consistent Error Handling

All endpoints use standardized validation and error responses with appropriate HTTP status codes.

### Logging

Request tracking with unique IDs for debugging and monitoring.

## Example Usage

### Analyze a PDF

```bash
curl -X POST http://localhost:2005/paper/info \
  -F "file=@paper.pdf" \
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
  -F "file=@paper.pdf" \
  -F "q=statistics"
```

### Run a Module

```bash
curl -X POST http://localhost:2005/paper/module \
  -F "file=@paper.pdf" \
  -F "name=check_dois"
```

### Run Multiple Checking Modules

```bash
# Run all available modules
curl -X POST http://localhost:2005/paper/check \
  -F "file=@paper.pdf"

# Run specific modules
curl -X POST http://localhost:2005/paper/check \
  -F "file=@paper.pdf" \
  -F "modules=exact_p,statcheck,osf_check"
```

### Convert PDF to GROBID XML

```bash
curl -X POST http://localhost:2005/grobid/pdf2grobid \
  -F "file=@paper.pdf" \
  -F "consolidateHeader=1"
```

## Files

### `api.R`

Main entry point that mounts endpoint groups.

### `endpoints/paper.R`

Paper analysis endpoints - handles file uploads, reads papers via `read_paper()`, and applies papercheck functions. Supports dynamic module execution.

### `endpoints/grobid.R`

PDF to GROBID XML conversion endpoint.

### `utils/validators.R`

Validation functions:

- `validate_file_upload()` - Validate uploaded PDF or XML files
- `validate_pdf_upload()` - Validate PDF-specific uploads
- `is_pdf_file()` / `is_xml_file()` - File type checks
- `validate_grobid_params()` - Validate GROBID parameters

### `utils/helpers.R`

Helper functions:

- `read_paper()` - Read PDF or XML files into papercheck objects
- `process_pdf_with_grobid()` - Process PDF files with GROBID
- `parse_grobid_params()` - Parse GROBID parameters from form data
- `error_response()` - Create standardized error responses
- `extract_uploaded_file()` - Extract file paths from multipart data
- `nz()` - Normalize zero-length values to NULL

## Running the API

### From R

```r
library(plumber)
pr <- plumb("inst/plumber/api.R")
pr$run(host = "0.0.0.0", port = 2005)
```

### Using Docker Compose

```bash
cd inst/plumber
docker compose up --build
```
