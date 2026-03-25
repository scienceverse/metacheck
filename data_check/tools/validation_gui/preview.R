# preview.R
# File preview rendering for the validation GUI.
# Sources pipeline/helper.R for read_data_head().

local({
  helper_path <- file.path(getOption("dc_root", "."), "pipeline", "helper.R")
  if (file.exists(helper_path)) source(helper_path, local = FALSE)
})

PREVIEW_TIMEOUT_SEC <- 5L

# ── Text preview (T024) ───────────────────────────────────────────────────────
# Used for: csv, tsv, txt, dat (n=50) and script files (n=80)

preview_text <- function(path, n_lines = 50L) {
  tryCatch({
    setTimeLimit(elapsed = PREVIEW_TIMEOUT_SEC, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    lines <- readLines(path, n = n_lines, warn = FALSE)
    paste(lines, collapse = "\n")
  }, error = function(e) {
    paste0("[Preview error: ", conditionMessage(e), "]")
  })
}

# ── Structured data preview (T025) ────────────────────────────────────────────
# Used for: sav, dta, sas7bdat, xlsx, xls via read_data_head()

preview_structured <- function(path) {
  tryCatch({
    setTimeLimit(elapsed = PREVIEW_TIMEOUT_SEC, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    df <- read_data_head(path, n_rows = 5L)
    if (is.null(df)) return("[No tabular data — may be a saved plot object]")
    paste(capture.output(print(df, max = 100)), collapse = "\n")
  }, error = function(e) {
    paste0("[Preview error: ", conditionMessage(e), "]")
  })
}

# ── R object preview (T026) ───────────────────────────────────────────────────
# Used for: rds, rda, rdata

preview_r_object <- function(path, ext) {
  tryCatch({
    setTimeLimit(elapsed = PREVIEW_TIMEOUT_SEC, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    if (tolower(ext) == "rds") {
      obj <- readRDS(path)
      paste(c(
        paste("class:", paste(class(obj), collapse = ", ")),
        capture.output(str(obj, max.level = 2, give.attr = FALSE, strict.width = "cut"))
      ), collapse = "\n")
    } else {
      e   <- new.env()
      load(path, envir = e)
      nms <- ls(e)
      if (length(nms) == 0) return("[Empty environment — no objects loaded]")
      paste(vapply(nms, function(nm) {
        obj <- get(nm, envir = e)
        paste0(nm, "  [", paste(class(obj), collapse = "/"), "]")
      }, character(1)), collapse = "\n")
    }
  }, error = function(e) {
    paste0("[Preview error: ", conditionMessage(e), "]")
  })
}

# ── Document text preview (T027) ─────────────────────────────────────────────
# Used for: pdf, docx

preview_document <- function(path, ext) {
  if (tolower(ext) == "pdf") {
    preview_pdf(path)
  } else {
    tryCatch({
      setTimeLimit(elapsed = PREVIEW_TIMEOUT_SEC, transient = TRUE)
      on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
      doc  <- officer::read_docx(path)
      summ <- officer::docx_summary(doc)
      rows <- summ[summ$content_type %in% c("paragraph", "table cell"), ]
      substr(trimws(paste(rows$text, collapse = "\n")), 1L, 2000L)
    }, error = function(e) {
      paste0("[Preview error: ", conditionMessage(e), "]")
    })
  }
}

# ── PDF preview: text extraction with image fallback for scanned PDFs ─────────

preview_pdf <- function(path) {
  # 1. Try text extraction (works for text-based PDFs)
  text <- tryCatch({
    setTimeLimit(elapsed = PREVIEW_TIMEOUT_SEC, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    pages <- pdftools::pdf_text(path)
    # Collapse runs of spaces (pdftools pads with spaces for column layout)
    pages <- gsub("[ \t]{2,}", " ", pages)
    paste(pages[seq_len(min(5L, length(pages)))], collapse = "\n")
  }, error = function(e) NULL)

  n_chars <- nchar(trimws(text %||% ""))

  if (n_chars >= 80L) {
    # Enough real text — return it (trim leading/trailing, cap at 3000 chars)
    return(substr(trimws(text), 1L, 3000L))
  }

  # 2. Scanned / image-based PDF — render page 1 as inline PNG thumbnail
  img_tag <- tryCatch({
    setTimeLimit(elapsed = PREVIEW_TIMEOUT_SEC, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    tmp <- tempfile()
    out <- pdftools::pdf_convert(path, format = "png", pages = 1L,
                                 dpi = 100L, verbose = FALSE,
                                 filenames = paste0(tmp, ".png"))
    on.exit({ if (file.exists(out)) unlink(out) }, add = TRUE)
    uri <- base64enc::dataURI(file = out, mime = "image/png")
    htmltools::tags$img(
      src   = uri,
      style = "max-width:100%; max-height:420px; border:1px solid #dee2e6; border-radius:4px;"
    )
  }, error = function(e) NULL)

  if (!is.null(img_tag)) return(img_tag)

  "[No extractable text — PDF appears to be scanned or image-based. Cannot render preview.]"
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ── Image preview tag (T028) ──────────────────────────────────────────────────
# Used for: jpg, jpeg, png, gif, svg

preview_image_tag <- function(path, ext) {
  tryCatch({
    ext <- tolower(ext)
    if (ext == "svg") {
      # SVG can be embedded inline from a file URL served by Shiny
      htmltools::tags$img(
        src   = paste0("paper_files/", basename(path)),
        style = "max-height:300px; max-width:100%;"
      )
    } else {
      mime <- switch(ext,
        jpg = , jpeg = "image/jpeg",
        png  = "image/png",
        gif  = "image/gif",
        "image/png"
      )
      # base64enc is a transitive dependency of shiny via htmltools
      uri <- base64enc::dataURI(file = path, mime = mime)
      htmltools::tags$img(src = uri, style = "max-height:300px; max-width:100%;")
    }
  }, error = function(e) {
    htmltools::tags$p(paste0("[Image preview error: ", conditionMessage(e), "]"))
  })
}

# ── Archive member list (T029) ────────────────────────────────────────────────
# Used for: zip, tar, tgz, gz, bz2, xz

preview_archive <- function(path, ext) {
  tryCatch({
    ext <- tolower(ext)
    entries <- switch(ext,
      zip = {
        info <- utils::unzip(path, list = TRUE)
        info$Name
      },
      tar = , tgz = {
        utils::untar(path, list = TRUE)
      },
      gz = , bz2 = , xz = {
        return("[Single compressed file — decompress to inspect contents]")
      },
      return("[Unsupported archive format]")
    )
    if (is.null(entries) || length(entries) == 0) return("[Empty archive]")
    shown   <- entries[seq_len(min(100L, length(entries)))]
    header  <- sprintf("[Archive: showing %d of %d entries]", length(shown), length(entries))
    paste(c(header, shown), collapse = "\n")
  }, error = function(e) {
    paste0("[Archive preview error: ", conditionMessage(e), "]")
  })
}

# ── HTML table preview ───────────────────────────────────────────────────────
# Parses tabular files and renders as a scrollable HTML table.

preview_table <- function(path, n_rows = 25L) {
  tryCatch({
    setTimeLimit(elapsed = PREVIEW_TIMEOUT_SEC, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    df <- read_data_head(path, n_rows = n_rows)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    # Coerce all columns to character for display
    df[] <- lapply(df, function(x) as.character(x))
    header_cells <- lapply(names(df), function(col) {
      htmltools::tags$th(style = "white-space:nowrap; background:#f0f4f8;", col)
    })
    body_rows <- lapply(seq_len(nrow(df)), function(i) {
      htmltools::tags$tr(
        lapply(df[i, , drop = FALSE], function(val) {
          htmltools::tags$td(style = "white-space:nowrap;", val)
        })
      )
    })
    htmltools::tags$div(
      style = "overflow-x:auto; overflow-y:auto; max-height:360px;",
      htmltools::tags$table(
        class = "table table-sm table-bordered table-striped mb-0",
        style = "font-size:0.78em;",
        htmltools::tags$thead(do.call(htmltools::tags$tr, header_cells)),
        do.call(htmltools::tags$tbody, body_rows)
      )
    )
  }, error = function(e) NULL)
}

# ── Legacy Word / RTF / ODT via macOS textutil ───────────────────────────────
# textutil is built into every macOS installation — no extra packages needed.
# Handles: doc, odt, rtf, wordml, and as a bonus: htm/html, ppt (partially).

preview_textutil <- function(path) {
  tryCatch({
    setTimeLimit(elapsed = PREVIEW_TIMEOUT_SEC, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    lines <- system2("textutil", c("-convert", "txt", "-stdout", path),
                     stdout = TRUE, stderr = FALSE)
    if (length(lines) == 0) return("[textutil returned no text]")
    text <- paste(lines, collapse = "\n")
    substr(trimws(text), 1L, 800L)
  }, error = function(e) {
    paste0("[textutil error: ", conditionMessage(e), "]")
  })
}

# ── PowerPoint preview via officer ───────────────────────────────────────────

preview_pptx <- function(path) {
  tryCatch({
    setTimeLimit(elapsed = PREVIEW_TIMEOUT_SEC, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    prs  <- officer::read_pptx(path)
    summ <- officer::pptx_summary(prs)
    rows <- summ[!is.na(summ$text) & nchar(trimws(summ$text)) > 0, ]
    if (nrow(rows) == 0) return("[No text content found in presentation]")
    text <- paste(rows$text, collapse = "\n")
    substr(trimws(text), 1L, 800L)
  }, error = function(e) {
    paste0("[Preview error: ", conditionMessage(e), "]")
  })
}

# ── HTML: strip tags ──────────────────────────────────────────────────────────

preview_html <- function(path) {
  tryCatch({
    setTimeLimit(elapsed = PREVIEW_TIMEOUT_SEC, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    lines <- readLines(path, n = 300L, warn = FALSE)
    text  <- paste(lines, collapse = "\n")
    # Remove scripts and style blocks entirely
    text <- gsub("(?s)<script[^>]*>.*?</script>", "", text, perl = TRUE)
    text <- gsub("(?s)<style[^>]*>.*?</style>",   "", text, perl = TRUE)
    # Strip remaining tags
    text <- gsub("<[^>]+>", " ", text)
    # Collapse whitespace
    text <- gsub("[ \t]+", " ", text)
    text <- gsub("\n{3,}", "\n\n", text)
    substr(trimws(text), 1L, 800L)
  }, error = function(e) {
    paste0("[Preview error: ", conditionMessage(e), "]")
  })
}

# ── Paper XML text extraction ────────────────────────────────────────────────
# Reads a GROBID TEI XML file and extracts title, abstract, and body text.
# Returns a list(title, abstract, body) or NULL if the file is absent/unreadable.

XML_DIR <- "/Volumes/Models/expanded_xml"

load_paper_xml <- function(paper_id) {
  path <- file.path(XML_DIR, paste0(paper_id, ".xml"))
  if (!file.exists(path)) return(NULL)
  tryCatch({
    doc <- xml2::read_xml(path)
    ns  <- c(tei = "http://www.tei-c.org/ns/1.0")

    # Title: prefer analytic title, fall back to any title
    t_node <- xml2::xml_find_first(doc, ".//tei:titleStmt/tei:title[@level='a']", ns)
    if (length(t_node) == 0 || is.na(xml2::xml_text(t_node))) {
      t_node <- xml2::xml_find_first(doc, ".//tei:titleStmt/tei:title", ns)
    }
    title <- trimws(xml2::xml_text(t_node))
    if (is.na(title)) title <- ""

    # Abstract paragraphs
    abs_nodes <- xml2::xml_find_all(doc, ".//tei:abstract//tei:p", ns)
    abstract  <- trimws(paste(xml2::xml_text(abs_nodes), collapse = "\n\n"))

    # Body paragraphs (cap at 200 for performance)
    body_nodes <- xml2::xml_find_all(doc, ".//tei:body//tei:p", ns)
    n_body     <- min(200L, length(body_nodes))
    body       <- trimws(paste(xml2::xml_text(body_nodes[seq_len(n_body)]), collapse = "\n\n"))

    list(title = title, abstract = abstract, body = body)
  }, error = function(e) NULL)
}

# ── Hex dump fallback ─────────────────────────────────────────────────────────

preview_hex <- function(path) {
  tryCatch({
    raw_bytes <- readBin(path, "raw", n = 256L)
    hex_str   <- paste(format(as.hexmode(as.integer(raw_bytes)), width = 2L), collapse = " ")
    paste0("[Preview not available — first 256 bytes as hex]\n\n", hex_str)
  }, error = function(e) {
    paste0("[Cannot read file: ", conditionMessage(e), "]")
  })
}

# ── Master dispatcher (T030) ──────────────────────────────────────────────────

render_preview <- function(path, ext) {
  ext <- tolower(trimws(ext))

  if (!file.exists(path)) {
    return(htmltools::HTML(
      "<p class='text-warning fw-bold'>File not found on disk.</p>"
    ))
  }

  pre_wrap <- function(txt) {
    htmltools::HTML(paste0(
      "<pre style='font-size:0.8em; white-space:pre-wrap; word-break:break-all;'>",
      htmltools::htmlEscape(txt),
      "</pre>"
    ))
  }

  # ── Tabular data ──────────────────────────────────────────────────────────────
  if (ext %in% c("csv", "tsv")) {
    tbl <- preview_table(path)
    if (!is.null(tbl)) tbl else pre_wrap(preview_text(path, 50L))

  } else if (ext %in% c("sav", "dta", "sas7bdat", "xlsx", "xls")) {
    tbl <- preview_table(path)
    if (!is.null(tbl)) tbl else pre_wrap(preview_structured(path))

  } else if (ext %in% c("txt", "dat")) {
    tbl <- tryCatch(preview_table(path), error = function(e) NULL)
    if (!is.null(tbl)) tbl else pre_wrap(preview_text(path, 50L))

  # ── Plain-text formats (source code, markup, config, data) ──────────────────
  } else if (ext %in% c(
      # Scripts
      "r", "rmd", "qmd", "py", "do", "sps", "sh", "bash", "zsh",
      "sql", "pl", "rb", "cpp", "c", "h", "java", "scala", "jl",
      "js", "ts", "m",
      # Markup / config
      "md", "markdown", "tex", "bib", "yaml", "yml", "toml",
      "ini", "cfg", "conf", "properties", "env",
      # Data interchange
      "json", "xml", "ndjson",
      # Logs / output
      "log", "out", "lst", "lis",
      # Qualtrics / misc
      "qsf"
    )) {
    pre_wrap(preview_text(path, 100L))

  # ── R objects ────────────────────────────────────────────────────────────────
  } else if (ext == "rds") {
    pre_wrap(preview_r_object(path, "rds"))
  } else if (ext %in% c("rda", "rdata")) {
    pre_wrap(preview_r_object(path, ext))

  # ── Modern Office documents ──────────────────────────────────────────────────
  } else if (ext %in% c("pdf", "docx")) {
    result <- preview_document(path, ext)
    if (inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list") ||
        inherits(result, "html")) result else pre_wrap(result)
  } else if (ext == "pptx") {
    pre_wrap(preview_pptx(path))

  # ── Legacy Office + RTF + ODT (macOS textutil) ───────────────────────────────
  } else if (ext %in% c("doc", "odt", "rtf", "ppt", "wordml")) {
    pre_wrap(preview_textutil(path))

  # ── HTML ─────────────────────────────────────────────────────────────────────
  } else if (ext %in% c("html", "htm")) {
    pre_wrap(preview_html(path))

  # ── Images ───────────────────────────────────────────────────────────────────
  } else if (ext %in% c("jpg", "jpeg", "png", "gif", "svg", "bmp", "tiff", "tif", "webp")) {
    if (ext %in% c("bmp", "tiff", "tif", "webp")) {
      # Not base64-embeddable in all browsers — show path info
      pre_wrap(sprintf("[Image file: %s  (%s)]\nOpen externally to view.",
                       basename(path), toupper(ext)))
    } else {
      preview_image_tag(path, ext)
    }

  # ── Archives ─────────────────────────────────────────────────────────────────
  } else if (ext %in% c("zip", "tar", "tgz", "gz", "bz2", "xz")) {
    pre_wrap(preview_archive(path, ext))

  # ── Known binary formats with no readable preview ────────────────────────────
  } else if (ext %in% c("spv", "spw",          # SPSS output/workspace
                         "por",                  # SPSS portable
                         "mdb", "accdb",         # Access databases
                         "sas7bcat",             # SAS catalog
                         "mp3", "mp4", "wav", "avi", "mov", "flac",
                         "exe", "dll", "so", "dylib")) {
    pre_wrap(sprintf("[Binary file: .%s — no text preview available]\nFile: %s",
                     ext, basename(path)))

  # ── Unknown: hex dump ────────────────────────────────────────────────────────
  } else {
    pre_wrap(preview_hex(path))
  }
}
