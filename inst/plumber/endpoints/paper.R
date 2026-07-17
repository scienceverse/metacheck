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

# Parse the optional comma-separated `modules` form field into a character
# vector, defaulting to every available module. Defined here (not in helpers.R)
# because it closes over AVAILABLE_MODULES.
.parse_modules <- function(modules_param) {
    if (!is.null(modules_param) && nzchar(modules_param)) {
        trimws(strsplit(modules_param, ",")[[1]])
    } else {
        AVAILABLE_MODULES
    }
}

#* List the metacheck modules this API can run
#* @get /modules
#* @serializer json
function() {
    # The authoritative whitelist /module and /check validate against, so
    # discovery matches enforcement exactly.
    list(modules = AVAILABLE_MODULES, count = length(AVAILABLE_MODULES))
}

#* Process a paper and return info table
#* @post /info
#* @param file:file bibr JSON file to process
#* @param fields:[string] Comma-separated fields to include (optional, e.g., "title,keywords,doi,description")
#* @serializer json
function(req, res) {
    with_uploaded_paper(req, res, "info", function(paper, mp, request_id) {
        fields <- if (!is.null(mp$fields)) {
            strsplit(mp$fields, ",")[[1]]
        } else {
            c("title", "keywords", "doi", "description")
        }
        info_fields(paper, fields)
    })
}

#* Get author table from a paper
#* @post /authors
#* @param file:file bibr JSON file to process
#* @serializer json
function(req, res) {
    with_uploaded_paper(req, res, "authors", function(paper, mp, request_id) {
        paper_table(paper, "author")
    })
}

#* Get references from a paper
#* @post /references
#* @param file:file bibr JSON file to process
#* @serializer json
function(req, res) {
    with_uploaded_paper(req, res, "references", function(paper, mp, request_id) {
        paper$bib
    })
}

#* Get cross-references from a paper
#* @post /cross-references
#* @param file:file bibr JSON file to process
#* @serializer json
function(req, res) {
    with_uploaded_paper(req, res, "cross-references", function(paper, mp, request_id) {
        paper$xref
    })
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
    with_uploaded_paper(
        req, res, "search",
        prevalidate = function(mp) {
            # Reject a missing pattern before paying the bibr parse cost.
            if (is.null(mp$pattern) || mp$pattern == "") {
                list(status = 400, message = "Query parameter 'pattern' is required")
            }
        },
        handler = function(paper, mp, request_id) {
            # Prepare optional parameters with defaults
            search_params <- list(paper = paper, pattern = mp$pattern)

            if (!is.null(mp$section)) search_params$section <- mp$section
            if (!is.null(mp$return)) search_params$return <- mp$return
            if (!is.null(mp$ignore.case)) search_params$ignore.case <- as.logical(mp$ignore.case)
            if (!is.null(mp$fixed)) search_params$fixed <- as.logical(mp$fixed)
            if (!is.null(mp$perl)) search_params$perl <- as.logical(mp$perl)

            do.call(text_search, search_params)
        }
    )
}

#* Run a specific module on the paper
#* @post /module
#* @param file:file bibr JSON file to process
#* @param name:[string] Name of the module to run (required)
#* @serializer json
function(req, res) {
    with_uploaded_paper(
        req, res, "module",
        prevalidate = function(mp) {
            # Validate the module name against the whitelist before parsing.
            if (is.null(mp$name) || mp$name == "") {
                return(list(status = 400, message = "Module name parameter 'name' is required"))
            }
            if (!(mp$name %in% AVAILABLE_MODULES)) {
                return(list(status = 400, message = paste0(
                    "Module '", mp$name, "' not found. Available modules: ",
                    paste(AVAILABLE_MODULES, collapse = ", ")
                )))
            }
            NULL
        },
        handler = function(paper, mp, request_id) {
            # Dynamically source and run the (whitelisted) module
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
                    result <- get(mp$name)(paper)
                    # Strip S3 classes jsonlite can't encode (e.g. `ellmer_output`
                    # columns from LLM modules) before the response is serialized.
                    json_safe(result)
                },
                error = function(e) {
                    error_response(res, 500, paste0("Error running module '", mp$name, "': ", e$message))
                }
            )
        }
    )
}

#* Get all relevant metadata from a paper, and run metacheck modules on it
#* @post /check
#* @param file:file bibr JSON file to process
#* @param modules:[string] Comma-separated list of modules to run (optional, defaults to all)
#* @param report whether to render metacheck's HTML report (optional, "true"/"false", defaults to TRUE; "false" skips the Quarto render and returns "" for report_html)
#* @serializer json
function(req, res) {
    with_uploaded_paper(
        req, res, "check",
        prevalidate = function(mp) {
            # Validate requested modules against the whitelist before parsing.
            invalid_modules <- setdiff(.parse_modules(mp$modules), AVAILABLE_MODULES)
            if (length(invalid_modules) > 0) {
                list(status = 400, message = paste0(
                    "Invalid modules: ", paste(invalid_modules, collapse = ", "),
                    ". Available modules: ", paste(AVAILABLE_MODULES, collapse = ", ")
                ))
            }
        },
        handler = function(paper, mp, request_id) {
            modules <- .parse_modules(mp$modules)
            include_report <- parse_bool(mp$report, default = TRUE)

            logger::log_info("Paper processed successfully, extracting metadata: {request_id}")
            authors <- paper_table(paper, "author")
            references <- paper$bib
            cross_references <- paper$xref

            logger::log_info("Running {length(modules)} module(s): {paste(modules, collapse = ', ')} - {request_id}")
            # Run each module once, keeping the FULL metacheck_module_output object: the
            # HTML report renderer needs fields (e.g. $section) that the JSON-safe view
            # below drops. A per-module failure is contained as a synthetic fail object,
            # so one bad module sinks neither the JSON response nor the rendered report.
            full_output <- lapply(modules, function(module_name) {
                tryCatch(
                    module_run(paper, module_name),
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
            # json_safe() also strips S3 classes jsonlite can't encode (e.g. the
            # `ellmer_output` columns LLM modules embed in their tables) — without it
            # the whole response aborts with "No method asJSON S3 class: ellmer_output".
            module_output <- lapply(full_output, function(result) {
                json_safe(list(
                    module = result$module,
                    title = result$title,
                    table = result$table,
                    summary_table = result$summary_table,
                    summary_text = result$summary_text,
                    report = result$report,
                    traffic_light = result$traffic_light
                ))
            })

            # metacheck's native HTML report, rendered from the modules just run (NO
            # re-run, no extra LLM calls). Best-effort: "" when Quarto can't render it,
            # which the platform treats as "no HTML available" (the JSON check is
            # unaffected). Skipped entirely when report=false so JSON-only clients
            # don't pay the (multi-second, single-threaded) Quarto render.
            report_html <- if (include_report) {
                render_report_html(full_output, paper, request_id)
            } else {
                logger::log_info("Report render skipped (report=false): {request_id}")
                ""
            }

            logger::log_info("Request completed successfully: {request_id}")
            # Return the aggregated report
            list(
                metacheck_version = as.character(utils::packageVersion("metacheck")),
                paper_info = info_fields(
                    paper,
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
    )
}
