# endpoints/paper.R
# Paper analysis endpoints - accept bibr JSON uploads

library(metacheck)
library(logger)

# Source utility functions
source("../utils/validators.R", local = TRUE)
source("../utils/helpers.R", local = TRUE)

AVAILABLE_MODULES <- list.files(
    path = system.file("modules", package = "metacheck"),
    pattern = "\\.R$",
    full.names = FALSE
) |>
    tools::file_path_sans_ext()

logger::log_info("Loaded {length(AVAILABLE_MODULES)} modules: {paste(AVAILABLE_MODULES, collapse = ', ')}")

#* Process a paper and return info table
#* @post /info
#* @param file:file bibr JSON file to process
#* @param fields:[string] Comma-separated fields to include (optional, e.g., "title,keywords,doi,description")
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

    # Save the uploaded file as a temporary file, then unlink after processing
    #
    # This block may seem redundant to have in every endpoint, but harder than
    # expected to abstract it away
    tmp_json <- tempfile(fileext = ".json")
    on.exit(unlink(tmp_json), add = TRUE)
    file.copy(uploaded_file, tmp_json)
    uploaded_file <- tmp_json


    # Read paper
    paper_obj <- read_paper(uploaded_file, request_id)
    if (!paper_obj$success) {
        return(error_response(res, 400, paper_obj$error))
    }

    # Parse fields parameter
    fields <- if (!is.null(mp$fields)) {
        strsplit(mp$fields, ",")[[1]]
    } else {
        c("title", "keywords", "doi", "description")
    }

    info_fields(paper_obj$paper, fields)
}

#* Get author table from a paper
#* @post /authors
#* @param file:file bibr JSON file to process
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

    # Save the uploaded file as a temporary file, then unlink after processing
    #
    # This block may seem redundant to have in every endpoint, but harder than
    # expected to abstract it away
    tmp_json <- tempfile(fileext = ".json")
    on.exit(unlink(tmp_json), add = TRUE)
    file.copy(uploaded_file, tmp_json)
    uploaded_file <- tmp_json


    paper_obj <- read_paper(uploaded_file, request_id)
    if (!paper_obj$success) {
        return(error_response(res, 400, paper_obj$error))
    }

    paper_table(paper_obj$paper, "author")
}

#* Get references from a paper
#* @post /references
#* @param file:file bibr JSON file to process
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

    # This block may seem redundant to have in every endpoint, but harder than
    # expected to abstract it away
    #
    # Save the uploaded file as a temporary file, then unlink after processing
    tmp_json <- tempfile(fileext = ".json")
    on.exit(unlink(tmp_json), add = TRUE)
    file.copy(uploaded_file, tmp_json)
    uploaded_file <- tmp_json

    paper_obj <- read_paper(uploaded_file, request_id)
    if (!paper_obj$success) {
        return(error_response(res, 400, paper_obj$error))
    }

    paper_obj$paper$bib
}

#* Get cross-references from a paper
#* @post /cross-references
#* @param file:file bibr JSON file to process
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

    # Save the uploaded file as a temporary file, then unlink after processing
    #
    # This block may seem redundant to have in every endpoint, but harder than
    # expected to abstract it away
    tmp_json <- tempfile(fileext = ".json")
    on.exit(unlink(tmp_json), add = TRUE)
    file.copy(uploaded_file, tmp_json)
    uploaded_file <- tmp_json

    paper_obj <- read_paper(uploaded_file, request_id)
    if (!paper_obj$success) {
        return(error_response(res, 400, paper_obj$error))
    }

    paper_obj$paper$xref
}

#* Search text in a paper
#* @post /search
#* @param file:file bibr JSON file to process
#* @param pattern the regex pattern to search for (required)
#* @param section the section(s) to search in (optional)
#* @param return the kind of text to return: "sentence", "paragraph", "div", "section", "match", or "paper_id" (optional, defaults to "sentence")
#* @param ignore.case whether to ignore case when text searching (optional, defaults to TRUE)
#* @param fixed logical. If TRUE, pattern is a string to be matched as is (optional, defaults to FALSE)
#* @param perl logical. Should Perl-compatible regexps be used? (optional, defaults to FALSE)
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

    # Save the uploaded file as a temporary file, then unlink after processing
    #
    # This block may seem redundant to have in every endpoint, but harder than
    # expected to abstract it away
    tmp_json <- tempfile(fileext = ".json")
    on.exit(unlink(tmp_json), add = TRUE)
    file.copy(uploaded_file, tmp_json)
    uploaded_file <- tmp_json

    if (is.null(mp$pattern) || mp$pattern == "") {
        return(error_response(res, 400, "Query parameter 'pattern' is required"))
    }

    paper_obj <- read_paper(uploaded_file, request_id)
    if (!paper_obj$success) {
        return(error_response(res, 400, paper_obj$error))
    }

    # Prepare optional parameters with defaults
    search_params <- list(
        paper = paper_obj$paper,
        pattern = mp$pattern
    )

    if (!is.null(mp$section)) search_params$section <- mp$section
    if (!is.null(mp$return)) search_params$return <- mp$return
    if (!is.null(mp$ignore.case)) search_params$ignore.case <- as.logical(mp$ignore.case)
    if (!is.null(mp$fixed)) search_params$fixed <- as.logical(mp$fixed)
    if (!is.null(mp$perl)) search_params$perl <- as.logical(mp$perl)

    do.call(text_search, search_params)
}


#* Run a specific module on the paper
#* @post /module
#* @param file:file bibr JSON file to process
#* @param name:[string] Name of the module to run (required)
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

    # Save the uploaded file as a temporary file, then unlink after processing
    #
    # This block may seem redundant to have in every endpoint, but harder than
    # expected to abstract it away
    tmp_json <- tempfile(fileext = ".json")
    on.exit(unlink(tmp_json), add = TRUE)
    file.copy(uploaded_file, tmp_json)
    uploaded_file <- tmp_json

    if (is.null(mp$name) || mp$name == "") {
        return(error_response(res, 400, "Module name parameter 'name' is required"))
    }

    if (!(mp$name %in% AVAILABLE_MODULES)) {
        return(error_response(res, 400, paste0("Module '", mp$name, "' not found. Available modules: ", paste(AVAILABLE_MODULES, collapse = ", "))))
    }

    paper_obj <- read_paper(uploaded_file, request_id)
    if (!paper_obj$success) {
        return(error_response(res, 400, paper_obj$error))
    }

    # Dynamically source and run the module
    module_path <- system.file("modules", paste0(mp$name, ".R"), package = "metacheck")
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

#* Get all relevant metadata from a paper, and run metacheck modules on it
#* @post /check
#* @param file:file bibr JSON file to process
#* @param modules:[string] Comma-separated list of modules to run (optional, defaults to all)
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

    # Save the uploaded file as a temporary file, then unlink after processing
    #
    # This block may seem redundant to have in every endpoint, but harder than
    # expected to abstract it away
    tmp_json <- tempfile(fileext = ".json")
    on.exit(unlink(tmp_json), add = TRUE)
    file.copy(uploaded_file, tmp_json)
    uploaded_file <- tmp_json

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

    paper_obj <- read_paper(uploaded_file, request_id)
    if (!paper_obj$success) {
        return(error_response(res, 400, paper_obj$error))
    }

    logger::log_info("Paper processed successfully, extracting metadata: {request_id}")
    authors <- paper_table(paper_obj$paper, "author")
    references <- paper_obj$paper$bib
    cross_references <- paper_obj$paper$xref

    logger::log_info("Running {length(modules)} module(s): {paste(modules, collapse = ', ')} - {request_id}")
    # Run each module once, keeping the FULL metacheck_module_output object: the
    # HTML report renderer needs fields (e.g. $section) that the JSON-safe view
    # below drops. A per-module failure is contained as a synthetic fail object,
    # so one bad module sinks neither the JSON response nor the rendered report.
    full_output <- lapply(modules, function(module_name) {
        tryCatch(
            module_run(paper_obj$paper, module_name),
            error = function(e) {
                structure(
                    list(
                        module = module_name,
                        title = module_name,
                        table = NULL,
                        summary_table = NULL,
                        summary_text = "Error running module",
                        report = paste0("Error running module '", module_name, "': ", e$message),
                        traffic_light = "fail",
                        section = "general"
                    ),
                    class = "metacheck_module_output"
                )
            }
        )
    })
    names(full_output) <- modules

    # JSON-safe view for the API response: plain lists with only the
    # serialisable fields (metacheck_module_output objects don't round-trip).
    module_output <- lapply(full_output, function(result) {
        list(
            module = result$module,
            title = result$title,
            table = result$table,
            summary_table = result$summary_table,
            summary_text = result$summary_text,
            report = result$report,
            traffic_light = result$traffic_light
        )
    })

    # metacheck's native HTML report, rendered from the modules just run (NO
    # re-run, no extra LLM calls). Best-effort: "" when Quarto can't render it,
    # which the platform treats as "no HTML available" (the JSON check is
    # unaffected).
    report_html <- render_report_html(full_output, paper_obj$paper, request_id)

    logger::log_info("Request completed successfully: {request_id}")
    # Return the aggregated report
    list(
        metacheck_version = as.character(utils::packageVersion("metacheck")),
        paper_info = info_fields(
            paper_obj$paper,
            c("title", "keywords", "doi", "submission", "received", "accepted")
        ),
        authors = authors,
        references = references,
        cross_references = cross_references,
        modules_run = modules,
        results = module_output,
        report_html = report_html
    )
}
