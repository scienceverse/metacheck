# sanitize_html.R
# Make a Quarto self-contained HTML report safe to email/share:
#   - Rebuilds interactive DataTables (DT / htmlwidgets) as plain static
#     <table>s so the full data survives once the scripts are removed.
#   - Removes all <script> tags, base64-encoded JavaScript data URIs, inline
#     on* event handlers, and javascript: hrefs.
#   - KEEPS all CSS: <style> blocks and <link> stylesheets whose href is a
#     data:text/css URI (Quarto inlines its theme, fonts, and callout styling
#     this way). CSS is not what gets the file flagged, so the report keeps
#     its original look.
#   - Removes only <link> tags that reference JavaScript or remote/unknown
#     resources.
#
# Pure R: depends only on xml2 and jsonlite. jsonlite is already loaded in
# your script; xml2 ships with the rmarkdown/quarto stack. If needed:
#   install.packages(c("xml2", "jsonlite"))

library(xml2)
library(jsonlite)

sanitize_html <- function(html_path, output_path = html_path) {

  esc <- function(s) {
    s <- as.character(s)
    s[is.na(s)] <- ""
    s
  }

  add_style_block <- function(parent, css_text) {
    style <- xml_add_child(parent, "style", type = "text/css")
    xml_text(style) <- css_text
    style
  }

  doc <- read_html(html_path, encoding = "UTF-8")

  # ---- 1. Rebuild every htmlwidget DataTable as a static HTML table ---------
  widgets <- xml_find_all(
    doc, "//script[@type='application/json' and @data-for]"
  )

  for (w in widgets) {
    wid <- xml_attr(w, "data-for")
    payload <- tryCatch(
      fromJSON(xml_text(w), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(payload) || is.null(payload$x) || is.null(payload$x$data)) {
      next
    }
    x <- payload$x

    # Headers: the <th> cells inside the stored container HTML
    headers <- character(0)
    if (!is.null(x$container)) {
      cont <- tryCatch(read_html(x$container), error = function(e) NULL)
      if (!is.null(cont)) headers <- xml_text(xml_find_all(cont, "//th"))
    }

    # htmlwidgets DT stores data COLUMN-MAJOR: each top-level element is a
    # column (list of cell values). Transpose to rows.
    cols <- x$data
    ncol <- length(cols)
    nrow <- if (ncol > 0) max(vapply(cols, length, integer(1))) else 0

    getcell <- function(ci, ri) {
      col <- cols[[ci]]
      if (ri <= length(col) && !is.null(col[[ri]])) {
        v <- col[[ri]]
        if (length(v) == 0) "" else esc(v)[1]
      } else {
        ""
      }
    }

    # Assemble the table as an HTML string (cell values keep their own markup).
    # Use Quarto/Bootstrap "table" classes so the rebuilt table inherits the
    # report's existing theme styling instead of looking foreign.
    parts <- c('<table class="table table-sm table-striped dt-static">')
    if (length(headers) > 0 && any(nzchar(headers))) {
      parts <- c(parts, "<thead><tr>",
                 paste0("<th>", esc(headers), "</th>"), "</tr></thead>")
    }
    parts <- c(parts, "<tbody>")
    if (nrow > 0) {
      for (ri in seq_len(nrow)) {
        parts <- c(parts, "<tr>")
        for (ci in seq_len(ncol)) {
          parts <- c(parts, paste0("<td>", getcell(ci, ri), "</td>"))
        }
        parts <- c(parts, "</tr>")
      }
    }
    parts <- c(parts, "</tbody></table>")
    table_html <- paste(parts, collapse = "")

    # Parse the table fragment and insert it where the widget div lives
    tbl_doc <- tryCatch(read_html(table_html), error = function(e) NULL)
    if (is.null(tbl_doc)) next
    tbl_node <- xml_find_first(tbl_doc, "//table")
    if (inherits(tbl_node, "xml_missing")) next

    target <- xml_find_first(doc, sprintf("//*[@id='%s']", wid))
    if (!inherits(target, "xml_missing")) {
      for (kid in xml_contents(target)) xml_remove(kid)
      xml_add_child(target, tbl_node)
    } else {
      xml_add_sibling(w, tbl_node, .where = "before")
    }
  }

  # ---- 2. Remove scripts; keep CSS stylesheets -----------------------------
  # Scripts always go. Keep CSS link tags, because Quarto's self-contained
  # theme and icon styling is already embedded there and is not what trips the
  # email filters we are working around.
  xml_remove(xml_find_all(doc, "//script"))
  head_node <- xml_find_first(doc, "//head")
  for (lnk in xml_find_all(doc, "//link")) {
    href <- trimws(xml_attr(lnk, "href"))
    href_lc <- tolower(href)

    if (!nzchar(href)) {
      xml_remove(lnk)
      next
    }

    if (startsWith(href_lc, "data:text/css") ||
        (grepl("\\.css([?#].*)?$", href_lc) &&
         !grepl("^(https?:|//|javascript:)", href_lc))) {
      next
    }

    xml_remove(lnk)
  }

  # ---- 2b. Normalize JS-only Quarto/Bootstrap states -----------------------
  # After removing scripts, Quarto sidebars and Bootstrap collapse widgets can
  # be left in an uninitialized state. Force a stable static layout instead.
  body_node <- xml_find_first(doc, "//body")
  if (!inherits(body_node, "xml_missing")) {
    body_class <- xml_attr(body_node, "class")
    body_class <- trimws(paste(body_class, "email-safe-report"))
    xml_set_attr(body_node, "class", body_class)
  }

  for (node in xml_find_all(doc, "//*[contains(concat(' ', normalize-space(@class), ' '), ' collapse ')]")) {
    classes <- strsplit(xml_attr(node, "class"), "\\s+")[[1]]
    classes <- classes[!(classes %in% c("collapse", "collapsed"))]
    xml_set_attr(node, "class", paste(classes[nzchar(classes)], collapse = " "))
    style <- trimws(xml_attr(node, "style"))
    if (is.na(style)) style <- ""
    style <- paste(c(style, "display: block;"), collapse = if (nzchar(style)) " " else "")
    xml_set_attr(node, "style", trimws(style))
  }

  for (node in xml_find_all(doc, "//*[@data-bs-toggle='collapse' or @data-bs-target or @data-bs-parent]")) {
    xml_set_attr(node, "data-bs-toggle", NULL)
    xml_set_attr(node, "data-bs-target", NULL)
    xml_set_attr(node, "data-bs-parent", NULL)
    xml_set_attr(node, "aria-expanded", "true")
  }

  info_node <- xml_find_first(doc, "//*[@id='info']")
  margin_sidebar <- xml_find_first(doc, "//*[@id='quarto-margin-sidebar']")
  if (!inherits(info_node, "xml_missing") && !inherits(margin_sidebar, "xml_missing")) {
    moved_info <- read_html(as.character(info_node), encoding = "UTF-8")
    moved_info <- xml_find_first(moved_info, "//*[@id='info']")
    xml_add_child(margin_sidebar, moved_info)
    xml_remove(info_node)
  }

  toc_node <- xml_find_first(doc, "//*[@id='TOC']")
  toc_title <- xml_find_first(doc, "//*[@id='toc-title']")
  emoji_key <- xml_find_first(doc, "//*[@id='TOC']//*[contains(concat(' ', normalize-space(@class), ' '), ' emoji-key ')]")
  if (!inherits(toc_node, "xml_missing") && !inherits(emoji_key, "xml_missing")) {
    moved_key <- read_html(as.character(emoji_key), encoding = "UTF-8")
    moved_key <- xml_find_first(moved_key, "//*[contains(concat(' ', normalize-space(@class), ' '), ' emoji-key ')]")
    for (kid in xml_children(moved_key)) {
      if (xml_name(kid) != "li") xml_remove(kid)
    }
    xml_add_sibling(toc_node, moved_key, .where = "before")
    xml_remove(emoji_key)
  }
  if (!inherits(toc_title, "xml_missing")) {
    for (kid in xml_contents(toc_title)) xml_remove(kid)
    xml_text(toc_title) <- "Table of Contents"
  }

  # ---- 3. Strip inline event handlers and dangerous URIs -------------------
  for (node in xml_find_all(doc, "//*")) {
    attrs <- xml_attrs(node)
    if (length(attrs) == 0) next
    for (a in names(attrs)) {
      la <- tolower(a)
      if (startsWith(la, "on")) {
        xml_set_attr(node, a, NULL)
      } else if (la == "style") {
        value <- trimws(attrs[[a]])
        value <- gsub("^NA\\s*", "", value)
        value <- trimws(value)
        if (!nzchar(value)) {
          xml_set_attr(node, a, NULL)
        } else {
          xml_set_attr(node, a, value)
        }
      } else if (la %in% c("href", "src")) {
        lv <- tolower(trimws(attrs[[a]]))
        if (startsWith(lv, "javascript:") ||
              grepl("javascript;base64", lv, fixed = TRUE) ||
              grepl("data:n/a", lv, fixed = TRUE)) {
          xml_set_attr(node, a, NULL)
        }
      }
    }
  }

  # ---- 4. Static layout fallback styling -----------------------------------
  # The rebuilt tables use Bootstrap/Quarto "table" classes and inherit the
  # theme. The extra CSS below only stabilizes the layout once the Quarto and
  # Bootstrap scripts have been removed.
  if (!inherits(head_node, "xml_missing")) {
    add_style_block(head_node, paste0(
      "table.dt-static{border-collapse:collapse;margin:1em 0;width:100%}",
      "table.dt-static th,table.dt-static td{",
      "border:1px solid var(--bs-border-color,#dee2e6);",
      "padding:6px 8px;text-align:left;vertical-align:top}",
      "table.dt-static thead th{",
      "background:var(--bs-tertiary-bg,#f2f2f2);font-weight:600}",
      "body.email-safe-report{overflow-x:auto}",
      "body.email-safe-report #quarto-content{",
      "max-width:1400px;margin:0 auto;padding:1rem;display:flex;gap:2rem;align-items:flex-start}",
      "body.email-safe-report main.content,",
      "body.email-safe-report #quarto-document-content{min-width:0;max-width:100%;margin:0}",
      "body.email-safe-report #quarto-margin-sidebar,",
      "body.email-safe-report .margin-sidebar{",
      "flex:0 0 280px;position:sticky;top:1rem;max-height:calc(100vh - 2rem);overflow:auto;",
      "width:280px;max-width:280px;margin:0;padding:.25rem .5rem .75rem;",
      "background:var(--bs-body-bg,#fff);border-right:1px solid var(--bs-border-color,#dee2e6)}",
      "body.email-safe-report main.content{flex:1 1 auto;padding-left:0}",
      "body.email-safe-report #quarto-document-content>*:first-child{margin-top:0}",
      "body.email-safe-report .column-page,",
      "body.email-safe-report .column-page-left,",
      "body.email-safe-report .column-page-right,",
      "body.email-safe-report .column-body,",
      "body.email-safe-report .page-columns{max-width:100%;margin-left:0;margin-right:0}",
      "body.email-safe-report .emoji-key{list-style:none;margin:0 0 1rem;padding:0}",
      "body.email-safe-report .emoji-key li{list-style:none}",
      "body.email-safe-report nav#TOC{margin-bottom:1rem}",
      "body.email-safe-report #toc-title{margin:0 0 .75rem;font-size:1rem;font-weight:700}",
      "body.email-safe-report #quarto-margin-sidebar p,",
      "body.email-safe-report #quarto-margin-sidebar ul{font-size:.9rem}",
      "body.email-safe-report .callout{margin:1rem 0}",
      "body.email-safe-report .callout-header{cursor:default}",
      "body.email-safe-report .callout-title-container{display:flex;align-items:center;gap:.5rem}",
      "body.email-safe-report details>summary{cursor:pointer}",
      "body.email-safe-report #info,",
      "body.email-safe-report .emoji-key{font-size:.85rem;line-height:1.35}",
      "body.email-safe-report #info{position:static !important;margin:0;max-width:100%;color:inherit}",
      "body.email-safe-report #info a{color:inherit !important}",
      "body.email-safe-report #info ul,",
      "body.email-safe-report .emoji-key ul{margin:.25rem 0 0 1rem;padding:0}",
      "body.email-safe-report #info li,",
      "body.email-safe-report .emoji-key li{margin:.125rem 0}",
      "@media (max-width: 991.98px){",
      "body.email-safe-report #quarto-content{display:block;padding:.75rem}",
      "body.email-safe-report main.content{padding-left:0}",
      "body.email-safe-report #quarto-margin-sidebar,",
      "body.email-safe-report .margin-sidebar{position:static;max-width:none;width:auto;bottom:auto;overflow:visible;border-right:0;margin:1rem 0 0;padding:0}",
      "body.email-safe-report #info{margin:1rem 0 0}",
      "}"
    ))
  }

  # ---- 5. Write out, scrubbing any residual base64 JS data URIs ------------
  out <- as.character(doc)
  out <- gsub("data:application/javascript;base64,[A-Za-z0-9+/=]+", "", out)
  out <- gsub("data:text/javascript;base64,[A-Za-z0-9+/=]+", "", out)
  out <- gsub("data:n/a;base64,[A-Za-z0-9+/=]+", "", out)
  writeLines(out, output_path, useBytes = TRUE)

  invisible(output_path)
}
