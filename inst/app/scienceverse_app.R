## scienceverse_app.R — Browse & search a scienceverse SQLite archive. ##
##
## A companion to the trove app, but backed by the SQLite database that
## add_to_scienceverse() builds rather than an in-memory folder index. It reads
## the archive at the path in the global `.scienceverse.db.` (set by
## scienceverse_app()) and lets a researcher search four surfaces the archive
## exposes:
##
##   Papers   — metadata + manuscript full-text search (FTS5), with per-paper
##              detail: studies, checks (traffic lights), files.
##   Findings — the per-text-unit check results, split by domain, with a
##              free-text match AND numeric filter controls (e.g. F > 5, p < .05)
##              — the capability the trove app does not have.
##   Scales   — identified scales/tasks across the corpus.
##   Files    — the download manifest (which papers shared code/data/... where).
##
## Text search uses the same Google-style field:value grammar as the trove app
## (bare words search everything; scale:panas restricts a field; quote for
## phrases). Numeric constraints live in the filter controls, not the text.
##
## All queries run live against SQLite via the metacheck::scienceverse_* helpers,
## so startup is instant and the app scales to a large corpus.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
})

options(scipen = 10, digits = 4)

# ── Locate the archive and open one shared connection ────────────────────────
.db <- if (exists(".scienceverse.db.", envir = .GlobalEnv)) {
  get(".scienceverse.db.", envir = .GlobalEnv)
} else {
  stop("No scienceverse database path set. Launch via scienceverse_app().")
}

.ns <- asNamespace("metacheck")
sv_connect     <- get("scienceverse_connect",     envir = .ns)
sv_papers      <- get("scienceverse_papers",      envir = .ns)
sv_findings    <- get("scienceverse_findings",    envir = .ns)
sv_scales      <- get("scienceverse_scales",      envir = .ns)
sv_scale_items <- get("scienceverse_scale_items", envir = .ns)
sv_files       <- get("scienceverse_files",       envir = .ns)
sv_checks      <- get("scienceverse_checks",      envir = .ns)
sv_summary     <- get("scienceverse_summary",     envir = .ns)
sv_checks_of   <- get("scienceverse_checks_of",   envir = .ns)
sv_check_table <- get("scienceverse_check_table", envir = .ns)
sv_counts      <- get(".sv_counts",               envir = .ns)

message("scienceverse: opening archive at ", .db, " ...")
CON <- sv_connect(.db)
onStop(function() try(DBI::dbDisconnect(CON), silent = TRUE))

COUNTS <- sv_counts(CON)

# The findings domains and the numeric columns each exposes as filter controls.
FINDING_TABLES <- c(
  "Statistics"     = "stat_findings",
  "Code"           = "code_findings",
  "Data / codebook" = "data_findings",
  "Excel"          = "excel_findings",
  "Other"          = "other_findings")
STAT_NUM  <- c("f_reported", "t_value", "p_value", "df1", "df2", "es")

# ── Theme (shared with the trove app for a consistent metacheck look) ─────────
theme <- bs_theme(
  version = 5,
  bg = "#f6f8fb", fg = "#1c2733",
  primary = "#4f46e5", secondary = "#0ea5e9", success = "#10b981",
  base_font = font_google("Inter", local = FALSE),
  heading_font = font_google("Space Grotesk", local = FALSE),
  "border-radius" = "0.8rem")

sv_css <- HTML("
  body { background:#f6f8fb; }
  .sv-hero {
    background:linear-gradient(120deg,#4f46e5,#7c3aed 45%,#0ea5e9);
    color:#fff; border-radius:1.1rem; padding:1.5rem 1.8rem; margin-bottom:1.1rem;
    box-shadow:0 18px 40px -18px rgba(79,70,229,.6); }
  .sv-hero h1 { font-weight:700; letter-spacing:-.02em; margin:0; font-size:1.6rem; }
  .sv-hero p  { margin:.35rem 0 0; opacity:.9; font-size:.93rem; }
  .sv-stats { display:flex; gap:.8rem; flex-wrap:wrap; margin-top:.9rem; }
  .sv-stat { flex:1 1 130px; background:rgba(255,255,255,.15);
    border:1px solid rgba(255,255,255,.22); border-radius:.75rem;
    padding:.6rem .8rem; color:#fff; }
  .sv-stat .v { font-size:1.35rem; font-weight:700; font-family:'Space Grotesk',sans-serif; }
  .sv-stat .l { font-size:.7rem; text-transform:uppercase; letter-spacing:.06em; opacity:.85; }
  .card { border:none !important; box-shadow:0 10px 30px -20px rgba(28,39,51,.4); }
  .sv-search input { border-radius:.8rem; border:1.5px solid #e2e8f0; padding:.6rem 1rem; }
  .sv-search input:focus { border-color:#4f46e5; box-shadow:0 0 0 4px rgba(79,70,229,.12); }
  .count-note { color:#64748b; font-size:.85rem; margin:.4rem 0; }
  .field-chip { display:inline-block; font-size:.72rem; font-family:monospace;
    background:#eef2ff; color:#4338ca; border:1px solid #c7d2fe; border-radius:.5rem;
    padding:.12rem .45rem; margin:.15rem .2rem 0 0; cursor:pointer; }
  .field-chip:hover { background:#4f46e5; color:#fff; }
  .tl { display:inline-block; width:.7rem; height:.7rem; border-radius:50%; margin-right:.3rem; }
  .tl-green{background:#10b981}.tl-red{background:#ef4444}.tl-yellow{background:#f59e0b}
  .tl-na{background:#94a3b8}.tl-info{background:#0ea5e9}
  table.dataTable tbody tr:hover { background:#eef2ff !important; }
  .sv-fulltext {
    max-height:45vh; overflow-y:auto; white-space:pre-wrap; word-wrap:break-word;
    font-size:.85rem; line-height:1.5; background:#f8fafc;
    border:1px solid #e2e8f0; border-radius:.6rem; padding:.8rem 1rem; }
  .sv-fulltext mark { background:#fde68a; padding:0 .05em; border-radius:2px; }
")

stat_box <- function(v, l) div(class = "sv-stat",
  div(class = "v", format(v, big.mark = ",")), div(class = "l", l))

# Clickable chips that append `field:` into a search box.
field_chips <- function(fields, input_id) {
  tagList(
    tags$span(class = "count-note", "Restrict to a field: "),
    lapply(fields, function(f) tags$span(class = "field-chip",
      onclick = sprintf(
        "var el=document.getElementById('%s');
         el.value=(el.value?el.value.trim()+' ':'')+'%s:'; el.focus();
         el.dispatchEvent(new Event('input',{bubbles:true}));", input_id, f),
      paste0(f, ":"))))
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "scienceverse", theme = theme, fillable = FALSE,
  header = tags$head(tags$style(sv_css), tags$script(HTML("
    // Show a dynamically-inserted modal STACKED on top of the currently-open
    // modal (Shiny's own modal stays underneath). Raise z-index so the second
    // modal and its backdrop sit above the first, and remove the element from
    // the DOM once dismissed so it can be rebuilt fresh next time.
    Shiny.addCustomMessageHandler('svShowStacked', function(id){
      var el = document.getElementById(id);
      if(!el) return;
      var m = new bootstrap.Modal(el);
      el.addEventListener('shown.bs.modal', function(){
        // Bump this modal above any already-open modal + its backdrop.
        var open = document.querySelectorAll('.modal.show');
        var base = 1055 + open.length * 20;
        el.style.zIndex = base + 10;
        var bds = document.querySelectorAll('.modal-backdrop');
        if(bds.length){ bds[bds.length-1].style.zIndex = base; }
      });
      el.addEventListener('hidden.bs.modal', function(){
        m.dispose();
        el.remove();
        // If a modal remains open, Bootstrap may have cleared the scroll lock;
        // re-add it so the underlying paper modal still scrolls correctly.
        if(document.querySelectorAll('.modal.show').length){
          document.body.classList.add('modal-open');
        }
      });
      m.show();
    });

    // Highlight all occurrences of the (space-separated) search terms in the
    // full-text box, and report a match count. The box's escaped text is kept
    // in a data attribute so we always re-highlight from the clean original
    // (never from already-marked HTML).
    function svEscapeRegex(s){ return s.replace(/[.*+?^${}()|[\\]\\\\]/g,'\\\\$&'); }
    function svHighlightFulltext(query){
      var box = document.getElementById('sv-ft-body');
      if(!box) return;
      if(box.getAttribute('data-orig') === null || box.getAttribute('data-orig') === undefined){
        box.setAttribute('data-orig', box.innerHTML);
      }
      var orig = box.getAttribute('data-orig');
      var terms = (query||'').trim().split(/\\s+/).filter(function(t){return t.length>0;});
      var countEl = document.getElementById('sv-ft-count');
      if(terms.length === 0){ box.innerHTML = orig; if(countEl) countEl.textContent='0'; return; }
      var pattern = terms.map(svEscapeRegex).join('|');
      var re = new RegExp('('+pattern+')','gi');
      var n = 0;
      box.innerHTML = orig.replace(re, function(m){ n++; return '<mark>'+m+'</mark>'; });
      if(countEl) countEl.textContent = String(n);
      // Scroll to the first match, if any.
      var first = box.querySelector('mark');
      if(first){ first.scrollIntoView({block:'center'}); }
    }
    window.svHighlightFulltext = svHighlightFulltext;
  "))),

  # Papers -------------------------------------------------------------------
  nav_panel(
    title = "Papers", icon = shiny::icon("book"),
    div(class = "sv-hero",
      h1("Search the scienceverse archive"),
      p("Everything metacheck extracted from each paper — full text, variables,",
        " statistics, scales, code and data — searchable in one database."),
      div(class = "sv-stats",
        stat_box(COUNTS[["papers"]],    "Papers"),
        stat_box(COUNTS[["studies"]],   "Studies"),
        stat_box(COUNTS[["variables"]], "Variables"),
        stat_box(COUNTS[["scales"]],    "Scales"),
        stat_box(COUNTS[["findings"]],  "Findings"),
        stat_box(COUNTS[["files"]],     "Files"))),
    card(card_header("Search papers (metadata + full text)"),
      card_body(
        div(class = "sv-search",
          textInput("papers_query", NULL, width = "100%",
            placeholder = "e.g.  stress   ·   title:cognitive   ·   authors:smith")),
        field_chips(c("title", "authors", "keywords", "doi"), "papers_query"),
        div(class = "count-note",
            "Click a paper to open its full detail in a pop-up."),
        div(class = "count-note", textOutput("papers_count", inline = TRUE)),
        DT::dataTableOutput("papers_table")))),

  # Findings -----------------------------------------------------------------
  nav_panel(
    title = "Findings", icon = shiny::icon("magnifying-glass-chart"),
    card(card_header("Search extracted findings, with numeric filters"),
      card_body(
        layout_columns(col_widths = c(4, 8),
          selectInput("find_table", "Finding type", choices = FINDING_TABLES),
          div(class = "sv-search",
            textInput("find_text", "Text contains", width = "100%",
              placeholder = "e.g.  interaction   ·   main effect"))),
        # Numeric filters (only meaningful for stat findings; shown always,
        # ignored by the query when the table has no such column).
        conditionalPanel("input.find_table == 'stat_findings'",
          layout_columns(col_widths = c(2, 2, 2, 2, 2, 2),
            numericInput("f_min", "F ≥", NA), numericInput("f_max", "F ≤", NA),
            numericInput("t_min", "|t| ≥", NA), numericInput("t_max", "|t| ≤", NA),
            numericInput("p_min", "p ≥", NA), numericInput("p_max", "p ≤", NA))),
        div(class = "count-note", textOutput("find_count", inline = TRUE)),
        DT::dataTableOutput("find_table_out"))),
    card(card_header("Finding detail"), card_body(uiOutput("find_detail")))),

  # Scales -------------------------------------------------------------------
  nav_panel(
    title = "Scales & Tasks", icon = shiny::icon("ruler-combined"),
    card(card_header("Search identified scales & tasks"),
      card_body(
        div(class = "sv-search",
          textInput("scales_query", NULL, width = "100%",
            placeholder = "e.g.  stress   ·   scale:panas   ·   confidence:high")),
        field_chips(c("scale", "code", "source", "confidence"), "scales_query"),
        div(class = "count-note", textOutput("scales_count", inline = TRUE)),
        DT::dataTableOutput("scales_table"),
        uiOutput("scale_items_head"),
        DT::dataTableOutput("scale_items_table")))),

  # Files --------------------------------------------------------------------
  nav_panel(
    title = "Files", icon = shiny::icon("folder-open"),
    card(card_header("Search the download manifest"),
      card_body(
        layout_columns(col_widths = c(8, 4),
          div(class = "sv-search",
            textInput("files_query", NULL, width = "100%",
              placeholder = "e.g.  analysis   ·   file_name:.R   ·   status:downloaded")),
          selectInput("files_type", "Data type",
            choices = c("(any)" = "", "code", "data", "documentation",
                        "materials", "output", "unknown"))),
        div(class = "count-note", textOutput("files_count", inline = TRUE)),
        DT::dataTableOutput("files_table")))),

  nav_spacer(),
  nav_item(tags$span(class = "count-note", style = "padding-right:1rem;",
    sprintf("%s papers in archive", format(COUNTS[["papers"]], big.mark = ",")))))

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  ## Papers ------------------------------------------------------------------
  papers_df <- reactive(sv_papers(CON, input$papers_query))

  output$papers_count <- renderText({
    n <- nrow(papers_df())
    sprintf("%d matching paper%s (searching title, authors, keywords, DOI and full text).",
            n, if (n == 1) "" else "s")
  })

  output$papers_table <- DT::renderDataTable({
    df <- papers_df()[, c("doi", "title", "authors", "n_studies",
                          "n_variables", "n_scales")]
    names(df) <- c("DOI", "Title", "Authors", "Studies", "Variables", "Scales")
    DT::datatable(df, selection = "single", rownames = FALSE,
      options = list(pageLength = 12, scrollX = TRUE, dom = "tip"))
  })

  # The DOI of the paper selected in the papers table (NULL if none). Reactive
  # so the modal's outputs can depend on it.
  sel_doi <- reactiveVal(NULL)

  # Open the detail modal when a paper row is clicked. The detail lives in a
  # modal (its own scrollable overlay), NOT stacked in the card's flex body, so
  # the tables cannot collapse onto one another.
  observeEvent(input$papers_table_rows_selected, {
    sel <- input$papers_table_rows_selected
    if (is.null(sel) || !length(sel)) return()
    doi <- papers_df()$doi[sel]
    title <- papers_df()$title[sel]
    sel_doi(doi)

    modules <- sv_checks_of(CON, doi)$module

    # Full text, escaped for safe HTML embedding (highlighting happens client-
    # side on this escaped text, so no markup can be injected or broken).
    ft <- DBI::dbGetQuery(CON, "SELECT fulltext FROM papers WHERE doi = ?",
                          params = list(doi))$fulltext
    ft <- if (length(ft) && !is.na(ft[[1]])) ft[[1]] else ""
    ft_esc <- htmltools::htmlEscape(ft)
    # Seed the in-text search with the bare terms from the main papers search
    # (strip any field: prefixes and quotes), so matches are pre-highlighted.
    seed <- gsub('"', "", input$papers_query %||% "")
    seed <- gsub("[a-z_]+:", "", seed, ignore.case = TRUE)
    seed <- trimws(seed)

    showModal(modalDialog(
      title = title, size = "xl", easyClose = TRUE, footer = modalButton("Close"),
      tags$p(tags$b("DOI: "), tags$a(href = doi, doi, target = "_blank")),

      tags$hr(),
      tags$h5("Full text"),
      tags$input(id = "sv-ft-search", type = "text", class = "form-control",
                 placeholder = "Search within the full text …",
                 oninput = "svHighlightFulltext(this.value)",
                 value = seed, style = "margin-bottom:.5rem;"),
      tags$div(class = "count-note",
               tags$span(id = "sv-ft-count"), " matches"),
      tags$div(id = "sv-ft-body", class = "sv-fulltext",
               HTML(ft_esc)),
      # Run the initial highlight once the modal has rendered.
      tags$script(HTML(sprintf(
        "setTimeout(function(){ svHighlightFulltext(%s); }, 60);",
        jsonlite::toJSON(seed, auto_unbox = TRUE)))),

      tags$hr(),
      tags$h5("Summary"),
      DT::dataTableOutput("m_summary_table"),

      tags$h5("Studies"),
      DT::dataTableOutput("m_studies_table"),

      tags$hr(),
      tags$h5("Checks"),
      tags$div(class = "count-note",
               "Click a check to open its full result table."),
      DT::dataTableOutput("m_checks_table"),

      tags$hr(),
      tags$h5("Files"),
      DT::dataTableOutput("m_files_table")
    ))

    # Clear the row selection so clicking the SAME paper again reopens the modal
    # (a repeated identical selection would not re-fire this observer otherwise).
    DT::selectRows(DT::dataTableProxy("papers_table"), NULL)
  }, ignoreInit = TRUE)

  ## Modal: summary ----------------------------------------------------------
  output$m_summary_table <- DT::renderDataTable({
    doi <- sel_doi(); if (is.null(doi)) return(NULL)
    sm <- sv_summary(CON, doi)
    if (!nrow(sm)) return(DT::datatable(data.frame(Note = "No summary recorded."),
      rownames = FALSE, options = list(dom = "t")))
    DT::datatable(sm, rownames = FALSE, colnames = c("Metric", "Value"),
      options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
  })

  ## Modal: studies ----------------------------------------------------------
  output$m_studies_table <- DT::renderDataTable({
    doi <- sel_doi(); if (is.null(doi)) return(NULL)
    st <- DBI::dbGetQuery(CON, paste("SELECT study_group, title, n_variables,",
      "schema_version FROM studies WHERE doi = ?"), params = list(doi))
    if (!nrow(st)) return(DT::datatable(data.frame(Note = "No studies."),
      rownames = FALSE, options = list(dom = "t")))
    DT::datatable(st, rownames = FALSE, options = list(dom = "t", pageLength = 25))
  })

  ## Modal: checks overview (module + traffic light + summary), row-clickable -
  m_checks_df <- reactive({
    doi <- sel_doi(); if (is.null(doi)) return(NULL)
    sv_checks_of(CON, doi)
  })
  output$m_checks_table <- DT::renderDataTable({
    ch <- m_checks_df()
    if (is.null(ch) || !nrow(ch))
      return(DT::datatable(data.frame(Note = "No checks recorded."),
                           rownames = FALSE, options = list(dom = "t")))
    tl_dot <- function(x) sprintf("<span class='tl tl-%s'></span>%s", x, x)
    disp <- data.frame(
      Module  = ch$module,
      Light   = vapply(ch$traffic_light, tl_dot, character(1)),
      Summary = ch$summary_text,
      check.names = FALSE, stringsAsFactors = FALSE)
    DT::datatable(disp, rownames = FALSE, escape = FALSE, selection = "single",
      options = list(pageLength = 30, dom = "t"))
  })

  # Clicking a check row opens a SECOND modal, stacked on top of the paper modal.
  # We build it as raw Bootstrap markup with the check's table RENDERED INLINE
  # (a DT htmlwidget baked into the inserted HTML), rather than relying on
  # pre-defined output slots that need Shiny to bind/flush into freshly-inserted
  # containers — that binding is what left the modal empty before.
  observeEvent(input$m_checks_table_rows_selected, {
    sel <- input$m_checks_table_rows_selected
    ch  <- m_checks_df()
    doi <- sel_doi()
    if (is.null(sel) || !length(sel) || is.null(ch) || is.null(doi)) return()
    module <- ch$module[sel]
    tbl <- sv_check_table(CON, doi, module)

    body <- if (!nrow(tbl))
      tags$em("This check produced no result rows for this paper.")
    else tagList(
      tags$div(class = "count-note",
               sprintf("%d row%s", nrow(tbl), if (nrow(tbl) == 1) "" else "s")),
      DT::datatable(tbl, rownames = FALSE, filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)))

    removeUI(selector = "#sv-check-modal", immediate = TRUE)  # drop any previous
    insertUI("body", "beforeEnd", immediate = TRUE, ui = tags$div(
      id = "sv-check-modal", class = "modal", tabindex = "-1",
      `data-bs-backdrop` = "true",
      tags$div(class = "modal-dialog modal-xl modal-dialog-scrollable",
        tags$div(class = "modal-content",
          tags$div(class = "modal-header",
            tags$h5(class = "modal-title",
                    sprintf("Check: %s", module)),
            tags$button(type = "button", class = "btn-close",
                        `data-bs-dismiss` = "modal", `aria-label` = "Close")),
          tags$div(class = "modal-body", body),
          tags$div(class = "modal-footer",
            tags$button(type = "button", class = "btn btn-secondary",
                        `data-bs-dismiss` = "modal", "Back to paper"))))))
    # Show it (stacked) and clean it up from the DOM when dismissed.
    session$sendCustomMessage("svShowStacked", "sv-check-modal")

    # Clear selection so re-clicking the same check reopens it.
    DT::selectRows(DT::dataTableProxy("m_checks_table"), NULL)
  }, ignoreInit = TRUE)

  ## Modal: files ------------------------------------------------------------
  output$m_files_table <- DT::renderDataTable({
    doi <- sel_doi(); if (is.null(doi)) return(NULL)
    fl <- DBI::dbGetQuery(CON,
      "SELECT file_name, data_type, status FROM files WHERE doi = ?",
      params = list(doi))
    if (!nrow(fl)) return(DT::datatable(data.frame(Note = "No files."),
      rownames = FALSE, options = list(dom = "t")))
    DT::datatable(fl, rownames = FALSE,
      options = list(dom = "tp", pageLength = 10, scrollX = TRUE))
  })

  ## Findings ----------------------------------------------------------------
  find_df <- reactive({
    ranges <- list(
      f_reported = c(input$f_min, input$f_max),
      t_value    = c(input$t_min, input$t_max),
      p_value    = c(input$p_min, input$p_max))
    sv_findings(CON, table = input$find_table, text = input$find_text,
                ranges = ranges, limit = 2000)
  })

  output$find_count <- renderText({
    n <- nrow(find_df())
    sprintf("%d matching finding%s in %s.", n, if (n == 1) "" else "s",
            names(FINDING_TABLES)[FINDING_TABLES == input$find_table])
  })

  output$find_table_out <- DT::renderDataTable({
    df <- find_df()
    # show the most useful columns first; drop all-NA columns for readability
    keep <- names(df)[colSums(!is.na(df)) > 0]
    df <- df[, keep, drop = FALSE]
    DT::datatable(df, selection = "single", rownames = FALSE, filter = "top",
      options = list(pageLength = 15, scrollX = TRUE))
  })

  output$find_detail <- renderUI({
    sel <- input$find_table_out_rows_selected
    if (is.null(sel) || !length(sel))
      return(tags$em("Select a finding to see its full text and paper."))
    row <- find_df()[sel, ]
    tagList(
      tags$p(tags$b("Paper: "),
             tags$a(href = row$doi, row$doi, target = "_blank")),
      if (!is.null(row$text)) tags$blockquote(row$text),
      tags$pre(paste(sprintf("%s: %s", names(row),
        vapply(row, function(x) as.character(x %||% ""), character(1))),
        collapse = "\n")))
  })

  ## Scales ------------------------------------------------------------------
  scales_df <- reactive(sv_scales(CON, input$scales_query))
  output$scales_count <- renderText({
    n <- nrow(scales_df())
    sprintf("%d matching scale%s/task%s.", n, if (n == 1) "" else "s",
            if (n == 1) "" else "s")
  })
  output$scales_table <- DT::renderDataTable({
    s <- scales_df()
    df <- data.frame(
      `Scale / task` = s$scale,
      Papers         = s$n_papers,
      Items          = ifelse(!is.na(s$n_items) & s$n_items > 0, s$n_items, 0L),
      `Items available` = ifelse(!is.na(s$n_items) & s$n_items > 0, "yes", "no"),
      Source         = s$source,
      Confidence     = s$confidence,
      Code           = s$code,
      check.names = FALSE, stringsAsFactors = FALSE)
    DT::datatable(df, rownames = FALSE, selection = "single",
      options = list(pageLength = 20, scrollX = TRUE, dom = "tip"))
  })

  # Clean up item wording stored with literal escapes / wrapping quotes in the
  # source OSD (e.g.  'It really bothers...'  and  \\'going around\\' ).
  clean_item_text <- function(x) {
    x <- as.character(x)
    x <- gsub("\\\\'", "'", x)          # \'  -> '
    x <- gsub('\\\\"', '"', x)          # \"  -> "
    x <- sub("^'\\s*", "", x)           # strip a leading quote
    x <- sub("\\s*'$", "", x)           # strip a trailing quote
    trimws(x)
  }

  # Items of the scale clicked above: a heading (renderUI) + its own table slot
  # (dataTableOutput), so the table reserves its height and never overlaps.
  scale_items_r <- reactive({
    sel <- input$scales_table_rows_selected
    if (is.null(sel) || !length(sel)) return(NULL)
    row <- scales_df()[sel, ]
    items <- if (is.na(row$n_items) || row$n_items == 0) data.frame()
             else sv_scale_items(CON, row$code)
    list(scale = row$scale, items = items)
  })
  output$scale_items_head <- renderUI({
    d <- scale_items_r(); if (is.null(d)) return(NULL)
    if (!nrow(d$items))
      return(tags$em(sprintf("“%s” has no stored items.", d$scale)))
    tags$h5(sprintf("Items in “%s” (%d)", d$scale, nrow(d$items)))
  })
  output$scale_items_table <- DT::renderDataTable({
    d <- scale_items_r(); if (is.null(d) || !nrow(d$items)) return(NULL)
    disp <- data.frame(
      `#`      = d$items$position,
      Item     = d$items$item_id,
      Question = clean_item_text(d$items$text),
      check.names = FALSE, stringsAsFactors = FALSE)
    DT::datatable(disp, rownames = FALSE,
      options = list(pageLength = 25, scrollX = TRUE, dom = "tip"))
  })

  ## Files -------------------------------------------------------------------
  files_df <- reactive(sv_files(CON, input$files_query,
                                data_type = input$files_type))
  output$files_count <- renderText({
    n <- nrow(files_df())
    sprintf("%d matching file%s.", n, if (n == 1) "" else "s")
  })
  output$files_table <- DT::renderDataTable({
    df <- files_df()
    # Prefer the direct file download URL; fall back to the repository URL so
    # the link always leads somewhere. Render as a clickable "Download" link.
    url <- ifelse(!is.na(df$file_url) & nzchar(df$file_url),
                  df$file_url, df$repo_url)
    link <- ifelse(is.na(url) | !nzchar(url), "",
      sprintf('<a href="%s" target="_blank" rel="noopener">Download</a>', url))
    out <- data.frame(
      DOI      = df$doi,
      File     = df$file_name,
      Type     = df$data_type,
      Format   = df$data_format,
      Bytes    = df$file_size,
      Local    = ifelse(df$downloaded == 1, "yes", "no"),
      Status   = df$status,
      Download = link,
      check.names = FALSE, stringsAsFactors = FALSE)
    # escape = -ncol keeps every column escaped except the last (the Download
    # link), so only that column renders HTML.
    DT::datatable(out, rownames = FALSE, filter = "top", escape = -ncol(out),
      options = list(pageLength = 20, scrollX = TRUE))
  })
}

# `%||%` for the detail pretty-print above.
`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)
