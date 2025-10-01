# endpoints/paper.R
# Paper analysis endpoints - can work with uploaded PDFs or GROBID XML

library(papercheck)
library(logger)

# Source utility functions
source("../utils/validators.R", local = TRUE)
source("../utils/helpers.R", local = TRUE)

AVAILABLE_MODULES <- list.files(
    path = system.file("modules", package = "papercheck"),
    pattern = "\\.R$",
    full.names = FALSE
) |>
    tools::file_path_sans_ext()

logger::log_info("Loaded {length(AVAILABLE_MODULES)} modules: {paste(AVAILABLE_MODULES, collapse = ', ')}")

#* Process a paper and return info table
#* @post /info
#* @param file:file PDF or GROBID XML file to process
#* @param fields:[string] Comma-separated fields to include (optional, e.g., "title,keywords,doi,description")
#* @param grobidUrl:[string] URL to GROBID server (optional, defaults to "https://kermitt2-grobid.hf.space")
#* @param start:[int] First page to process (optional, defaults to -1 for all pages)
#* @param end:[int] Last page to process (optional, defaults to -1 for all pages)
#* @param consolidateCitations:[int] Whether to consolidate citations (0 or 1, optional)
#* @param consolidateHeader:[int] Whether to consolidate header (0 or 1, optional)
#* @param consolidateFunders:[int] Whether to consolidate funders (0 or 1, optional)
#* @serializer json
function(req, res) {
    request_id <- uuid::UUIDgenerate()
    logger::log_info("Request started (info): {request_id}")

    # Parse multipart form data
    mp <- mime::parse_multipart(req)
    uploaded_file <- extract_uploaded_file(mp)

    # Validate file upload
    validation <- validate_file_upload(uploaded_file)
    if (!validation$valid) {
        return(error_response(res, validation$status, validation$message))
    }

    # Parse GROBID parameters
    grobid_params <- parse_grobid_params(mp)

    # Read paper
    paper_obj <- read_paper(uploaded_file, request_id, grobid_params)
    if (!paper_obj$success) {
        return(error_response(res, 500, paper_obj$error))
    }

    # Parse fields parameter
    fields <- if (!is.null(mp$fields)) {
        strsplit(mp$fields, ",")[[1]]
    } else {
        c("title", "keywords", "doi", "description")
    }

    info_table(paper_obj$paper, fields)
}

#* Get author table from a paper
#* @post /authors
#* @param file:file PDF or GROBID XML file to process
#* @param grobidUrl:[string] URL to GROBID server (optional, defaults to "https://kermitt2-grobid.hf.space")
#* @param start:[int] First page to process (optional, defaults to -1 for all pages)
#* @param end:[int] Last page to process (optional, defaults to -1 for all pages)
#* @param consolidateCitations:[int] Whether to consolidate citations (0 or 1, optional)
#* @param consolidateHeader:[int] Whether to consolidate header (0 or 1, optional)
#* @param consolidateFunders:[int] Whether to consolidate funders (0 or 1, optional)
#* @serializer json
function(req, res) {
    request_id <- uuid::UUIDgenerate()
    logger::log_info("Request started (authors): {request_id}")

    mp <- mime::parse_multipart(req)
    uploaded_file <- extract_uploaded_file(mp)

    validation <- validate_file_upload(uploaded_file)
    if (!validation$valid) {
        return(error_response(res, validation$status, validation$message))
    }

    # Parse GROBID parameters
    grobid_params <- parse_grobid_params(mp)

    paper_obj <- read_paper(uploaded_file, request_id, grobid_params)
    if (!paper_obj$success) {
        return(error_response(res, 500, paper_obj$error))
    }

    author_table(paper_obj$paper)
}

#* Get references from a paper
#* @post /references
#* @param file:file PDF or GROBID XML file to process
#* @param grobidUrl:[string] URL to GROBID server (optional, defaults to "https://kermitt2-grobid.hf.space")
#* @param start:[int] First page to process (optional, defaults to -1 for all pages)
#* @param end:[int] Last page to process (optional, defaults to -1 for all pages)
#* @param consolidateCitations:[int] Whether to consolidate citations (0 or 1, optional)
#* @param consolidateHeader:[int] Whether to consolidate header (0 or 1, optional)
#* @param consolidateFunders:[int] Whether to consolidate funders (0 or 1, optional)
#* @serializer json
function(req, res) {
    request_id <- uuid::UUIDgenerate()
    logger::log_info("Request started (references): {request_id}")

    mp <- mime::parse_multipart(req)
    uploaded_file <- extract_uploaded_file(mp)

    validation <- validate_file_upload(uploaded_file)
    if (!validation$valid) {
        return(error_response(res, validation$status, validation$message))
    }

    # Parse GROBID parameters
    grobid_params <- parse_grobid_params(mp)

    paper_obj <- read_paper(uploaded_file, request_id, grobid_params)
    if (!paper_obj$success) {
        return(error_response(res, 500, paper_obj$error))
    }

    paper_obj$paper$bib
}

#* Get cross-references from a paper
#* @post /cross-references
#* @param file:file PDF or GROBID XML file to process
#* @param grobidUrl:[string] URL to GROBID server (optional, defaults to "https://kermitt2-grobid.hf.space")
#* @param start:[int] First page to process (optional, defaults to -1 for all pages)
#* @param end:[int] Last page to process (optional, defaults to -1 for all pages)
#* @param consolidateCitations:[int] Whether to consolidate citations (0 or 1, optional)
#* @param consolidateHeader:[int] Whether to consolidate header (0 or 1, optional)
#* @param consolidateFunders:[int] Whether to consolidate funders (0 or 1, optional)
#* @serializer json
function(req, res) {
    request_id <- uuid::UUIDgenerate()
    logger::log_info("Request started (cross-references): {request_id}")

    mp <- mime::parse_multipart(req)
    uploaded_file <- extract_uploaded_file(mp)

    validation <- validate_file_upload(uploaded_file)
    if (!validation$valid) {
        return(error_response(res, validation$status, validation$message))
    }

    # Parse GROBID parameters
    grobid_params <- parse_grobid_params(mp)

    paper_obj <- read_paper(uploaded_file, request_id, grobid_params)
    if (!paper_obj$success) {
        return(error_response(res, 500, paper_obj$error))
    }

    paper_obj$paper$xrefs
}

#* Search text in a paper
#* @post /search
#* @param file:file PDF or GROBID XML file to process
#* @param q:[string] Search query
#* @param grobidUrl:[string] URL to GROBID server (optional, defaults to "https://kermitt2-grobid.hf.space")
#* @param start:[int] First page to process (optional, defaults to -1 for all pages)
#* @param end:[int] Last page to process (optional, defaults to -1 for all pages)
#* @param consolidateCitations:[int] Whether to consolidate citations (0 or 1, optional)
#* @param consolidateHeader:[int] Whether to consolidate header (0 or 1, optional)
#* @param consolidateFunders:[int] Whether to consolidate funders (0 or 1, optional)
#* @serializer json
function(req, res) {
    request_id <- uuid::UUIDgenerate()
    logger::log_info("Request started (search): {request_id}")

    mp <- mime::parse_multipart(req)
    uploaded_file <- extract_uploaded_file(mp)

    validation <- validate_file_upload(uploaded_file)
    if (!validation$valid) {
        return(error_response(res, validation$status, validation$message))
    }

    if (is.null(mp$q) || mp$q == "") {
        return(error_response(res, 400, "Query parameter 'q' is required"))
    }

    # Parse GROBID parameters
    grobid_params <- parse_grobid_params(mp)

    paper_obj <- read_paper(uploaded_file, request_id, grobid_params)
    if (!paper_obj$success) {
        return(error_response(res, 500, paper_obj$error))
    }

    search_text(paper_obj$paper, query = mp$q, return = "div")
}


#* Run a specific module on the paper
#* @post /module
#* @param file:file PDF or GROBID XML file to process
#* @param name:[string] Name of the module to run (required)
#* @param grobidUrl:[string] URL to GROBID server (optional, defaults to "https://kermitt2-grobid.hf.space")
#* @param start:[int] First page to process (optional, defaults to -1 for all pages)
#* @param end:[int] Last page to process (optional, defaults to -1 for all pages)
#* @param consolidateCitations:[int] Whether to consolidate citations (0 or 1, optional)
#* @param consolidateHeader:[int] Whether to consolidate header (0 or 1, optional)
#* @param consolidateFunders:[int] Whether to consolidate funders (0 or 1, optional)
#* @serializer json
function(req, res) {
    request_id <- uuid::UUIDgenerate()
    logger::log_info("Request started (module): {request_id}")

    mp <- mime::parse_multipart(req)
    uploaded_file <- extract_uploaded_file(mp)

    validation <- validate_file_upload(uploaded_file)
    if (!validation$valid) {
        return(error_response(res, validation$status, validation$message))
    }

    if (is.null(mp$name) || mp$name == "") {
        return(error_response(res, 400, "Module name parameter 'name' is required"))
    }

    if (!(mp$name %in% AVAILABLE_MODULES)) {
        return(error_response(res, 400, paste0("Module '", mp$name, "' not found. Available modules: ", paste(AVAILABLE_MODULES, collapse = ", "))))
    }

    # Parse GROBID parameters
    grobid_params <- parse_grobid_params(mp)

    paper_obj <- read_paper(uploaded_file, request_id, grobid_params)
    if (!paper_obj$success) {
        return(error_response(res, 500, paper_obj$error))
    }

    # Dynamically source and run the module
    module_path <- system.file("modules", paste0(mp$name, ".R"), package = "papercheck")
    if (module_path == "") {
        return(error_response(res, 500, paste0("Module file for '", mp$name, "' not found.")))
    }

    source(module_path, local = TRUE)
    if (!exists(mp$name)) {
        return(error_response(res, 500, paste0("Module '", mp$name, "' does not define a '", mp$name, "' function.")))
    }

    tryCatch(
        {
            result <- get(mp$name)(paper_obj$paper)
            result
        },
        error = function(e) {
            error_response(res, 500, paste0("Error running module '", mp$name, "': ", e$message))
        }
    )
}

#* Get all relevant metadata from a paper, and run papercheck modules on it
#* @post /check
#* @param file:file PDF or GROBID XML file to process
#* @param modules:[string] Comma-separated list of modules to run (optional, defaults to all)
#* @param grobidUrl:[string] URL to GROBID server (optional, defaults to "https://kermitt2-grobid.hf.space")
#* @param start:[int] First page to process (optional, defaults to -1 for all pages)
#* @param end:[int] Last page to process (optional, defaults to -1 for all pages)
#* @param consolidateCitations:[int] Whether to consolidate citations (0 or 1, optional)
#* @param consolidateHeader:[int] Whether to consolidate header (0 or 1, optional)
#* @param consolidateFunders:[int] Whether to consolidate funders (0 or 1, optional)
#* @serializer json
function(req, res) {
    request_id <- uuid::UUIDgenerate()
    logger::log_info("Request started (check): {request_id}")

    mp <- mime::parse_multipart(req)
    uploaded_file <- extract_uploaded_file(mp)

    validation <- validate_file_upload(uploaded_file)
    if (!validation$valid) {
        return(error_response(res, validation$status, validation$message))
    }

    # Parse modules parameter
    modules <- if (!is.null(mp$modules) && mp$modules != "") {
        strsplit(mp$modules, ",")[[1]] |> trimws()
    } else {
        AVAILABLE_MODULES
    }

    # Validate that all requested modules exist
    invalid_modules <- setdiff(modules, AVAILABLE_MODULES)
    if (length(invalid_modules) > 0) {
        return(error_response(res, 400, paste0("Invalid modules: ", paste(invalid_modules, collapse = ", "), ". Available modules: ", paste(AVAILABLE_MODULES, collapse = ", "))))
    }

    # Parse GROBID parameters
    grobid_params <- parse_grobid_params(mp)

    paper_obj <- read_paper(uploaded_file, request_id, grobid_params)
    if (!paper_obj$success) {
        return(error_response(res, 500, paper_obj$error))
    }

    logger::log_info("Paper processed successfully, extracting metadata: {request_id}")
    authors <- author_table(paper_obj$paper)
    references <- paper_obj$paper$bib
    cross_references <- paper_obj$paper$xrefs

    logger::log_info("Running {length(modules)} module(s): {paste(modules, collapse = ', ')} - {request_id}")
    # Run all requested modules using module_run
    module_output <- lapply(modules, function(module_name) {
        tryCatch(
            {
                result <- module_run(paper_obj$paper, module_name)
                # Convert ppchk_module_output to plain list for JSON serialization
                list(
                    module = result$module,
                    title = result$title,
                    table = result$table,
                    summary = result$summary,
                    report = result$report,
                    traffic_light = result$traffic_light
                )
            },
            error = function(e) {
                list(
                    module = module_name,
                    title = module_name,
                    table = NULL,
                    report = paste0("Error running module '", module_name, "': ", e$message),
                    traffic_light = "fail"
                )
            }
        )
    })

    # Set module names as keys
    names(module_output) <- modules
    
    logger::log_info("Request completed successfully: {request_id}")
    # Return the aggregated report
    list(
        paper_info = info_table(paper_obj$paper, c("title", "doi", "keywords")),
        authors = authors,
        references = references,
        cross_references = cross_references,
        modules_run = modules,
        results = module_output
    )
}
