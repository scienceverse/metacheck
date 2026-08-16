## trove_app.R — Browse & search a corpus of metacheck Psych-DS collections. ##
##
## An R/Shiny port of the PsychTrove viewer (https://github.com/levibaruch/
## PsychTrove). It scans a root directory for metacheck collection folders
## (each with a collection.json + study-*/dataset_description.json), builds an
## in-memory index (see trove_index()), and lets a researcher browse papers and
## search across every variable and every identified scale/task in the corpus.
##
## Search supports a faceted, prefixed syntax: a bare word searches every
## field, while `field:term` (e.g. scale:panas, concept:likert, level:ordinal)
## restricts a term to one JSON category. Quote to include spaces
## (scale:"just world"). Terms are AND-combined.
##
## The UI is built on {bslib} for a modern, animated look (gradient hero,
## value-box stats, hover-lift cards, fade-in tab transitions).
##
## The corpus root is passed in via the global `.trove.root.` set by
## trove_app(); if absent, it defaults to the working directory.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
})

options(scipen = 10, digits = 4)

# ── Locate the corpus root and build the index once, at startup ──────────────
.root <- if (exists(".trove.root.", envir = .GlobalEnv)) {
  get(".trove.root.", envir = .GlobalEnv)
} else {
  getwd()
}

.trove_index_fun <- tryCatch(
  get("trove_index", envir = asNamespace("metacheck")),
  error = function(e) NULL
)
if (is.null(.trove_index_fun)) {
  stop("metacheck::trove_index() not found; load the metacheck package first.")
}
.parse_query <- get("trove_parse_query", envir = asNamespace("metacheck"))
.match_query <- get("trove_match",       envir = asNamespace("metacheck"))

message("trove: indexing corpus at ", .root, " ...")
INDEX <- .trove_index_fun(.root, quiet = TRUE)

PAPERS    <- INDEX$papers
STUDIES   <- INDEX$studies
VARIABLES <- INDEX$variables
SCALES    <- INDEX$scales

# ── Derived tables ───────────────────────────────────────────────────────────
paper_counts <- local({
  ns <- as.data.frame(table(STUDIES$paper_id), stringsAsFactors = FALSE)
  names(ns) <- c("paper_id", "n_studies")
  nv <- aggregate(n_variables ~ paper_id, data = STUDIES, FUN = sum)
  nsc <- as.data.frame(table(unique(SCALES[, c("paper_id", "code")])$paper_id),
                       stringsAsFactors = FALSE)
  names(nsc) <- c("paper_id", "n_scales")
  out <- merge(PAPERS, ns, by = "paper_id", all.x = TRUE)
  out <- merge(out, nv, by = "paper_id", all.x = TRUE)
  out <- merge(out, nsc, by = "paper_id", all.x = TRUE)
  out$n_studies[is.na(out$n_studies)]     <- 0
  out$n_variables[is.na(out$n_variables)] <- 0
  out$n_scales[is.na(out$n_scales)]       <- 0
  out
})

scale_summary <- local({
  s <- SCALES[!is.na(SCALES$scale), ]
  if (!nrow(s)) {
    return(data.frame(scale = character(), code = character(),
                      n_papers = integer(), median_items = numeric(),
                      source = character(), confidence = character()))
  }
  agg <- do.call(rbind, lapply(split(s, s$code), function(g) {
    data.frame(
      scale        = g$scale[1],
      code         = g$code[1],
      n_papers     = length(unique(g$paper_id)),
      median_items = suppressWarnings(stats::median(g$n_items, na.rm = TRUE)),
      source       = paste(sort(unique(stats::na.omit(g$source))), collapse = ", "),
      confidence   = paste(sort(unique(stats::na.omit(g$confidence))), collapse = ", ")
    )
  }))
  agg[order(-agg$n_papers, agg$scale), ]
})

# De-duplicated variable table for searching: one row per
# (paper, study, variable name, source file, scale).
var_search <- unique(VARIABLES[, c(
  "paper_id", "study_group", "name", "label", "concept", "level", "role",
  "representation", "scale", "scale_code", "technique", "value_pattern",
  "source_file", "n", "mean", "sd", "min_value", "max_value"
)])

# The JSON categories a user can prefix-search on each table.
VAR_FIELDS <- c("name", "label", "concept", "level", "role", "representation",
                "scale", "technique", "value_pattern", "source_file",
                "paper_id", "study_group")
VAR_FREE_COLS <- c("name", "label", "concept", "level", "role",
                   "representation", "scale", "technique", "value_pattern",
                   "source_file")
SCALE_FIELDS   <- c("scale", "code", "source", "confidence")
SCALE_FREE_COLS <- c("scale", "code", "source", "confidence")

# ── Small UI helpers ─────────────────────────────────────────────────────────
# An animated "count up" statistic tile.
stat_tile <- function(value, label, icon_name, accent) {
  div(
    class = "trove-stat",
    style = sprintf("--accent: %s;", accent),
    div(class = "trove-stat-icon", bs_icon(icon_name)),
    div(
      div(class = "trove-stat-value", `data-target` = value, "0"),
      div(class = "trove-stat-label", label)
    )
  )
}

# Icons via Font Awesome (shipped with shiny/bslib); avoids the optional
# {bsicons} dependency, which is not installed.
bs_icon <- function(name) {
  shiny::icon(switch(name,
    book = "book", table = "layer-group", ruler = "ruler-combined",
    search = "magnifying-glass", flask = "flask", "circle"))
}

# ── Theme ────────────────────────────────────────────────────────────────────
theme <- bs_theme(
  version = 5,
  bg = "#f6f8fb", fg = "#1c2733",
  primary = "#4f46e5", secondary = "#0ea5e9", success = "#10b981",
  base_font = font_google("Inter", local = FALSE),
  heading_font = font_google("Space Grotesk", local = FALSE),
  "border-radius" = "0.8rem"
)

trove_css <- HTML("
  :root { --grad-a:#4f46e5; --grad-b:#0ea5e9; --grad-c:#8b5cf6; }
  body { background: #f6f8fb; }

  .trove-hero {
    position: relative; overflow: hidden;
    background: linear-gradient(120deg,#4f46e5,#7c3aed 45%,#0ea5e9);
    background-size: 200% 200%;
    animation: heroShift 14s ease infinite;
    color:#fff; border-radius:1.1rem; padding:1.6rem 1.9rem;
    margin-bottom:1.2rem; box-shadow:0 18px 40px -18px rgba(79,70,229,.6);
  }
  @keyframes heroShift {
    0%{background-position:0% 50%} 50%{background-position:100% 50%}
    100%{background-position:0% 50%}
  }
  .trove-hero h1 { font-weight:700; letter-spacing:-.02em; margin:0; font-size:1.7rem; }
  .trove-hero p  { margin:.35rem 0 0; opacity:.9; font-size:.95rem; }
  .trove-hero::after {
    content:''; position:absolute; right:-60px; top:-60px;
    width:240px; height:240px; border-radius:50%;
    background:radial-gradient(circle,rgba(255,255,255,.22),transparent 70%);
  }

  .trove-stats { display:flex; gap:.9rem; flex-wrap:wrap; margin:.9rem 0 0; }
  .trove-stat {
    flex:1 1 150px; display:flex; align-items:center; gap:.7rem;
    background:rgba(255,255,255,.14); backdrop-filter:blur(6px);
    border:1px solid rgba(255,255,255,.22);
    border-radius:.8rem; padding:.7rem .9rem; color:#fff;
    transition:transform .25s ease, background .25s ease;
  }
  .trove-stat:hover { transform:translateY(-3px); background:rgba(255,255,255,.22); }
  .trove-stat-icon { font-size:1.4rem; opacity:.95; }
  .trove-stat-value { font-size:1.5rem; font-weight:700; line-height:1;
    font-family:'Space Grotesk',sans-serif; }
  .trove-stat-label { font-size:.72rem; text-transform:uppercase;
    letter-spacing:.06em; opacity:.85; }

  .card { border:none !important;
    box-shadow:0 10px 30px -20px rgba(28,39,51,.4);
    transition:transform .28s ease, box-shadow .28s ease; }
  .card:hover { transform:translateY(-2px);
    box-shadow:0 18px 40px -22px rgba(79,70,229,.55); }

  .nav-tabs .nav-link, .nav-pills .nav-link { transition:all .2s ease; }
  .nav-pills .nav-link.active {
    background:linear-gradient(120deg,#4f46e5,#0ea5e9) !important;
    box-shadow:0 8px 20px -10px rgba(79,70,229,.7); }

  .tab-pane.active { animation:fadeUp .45s ease both; }
  @keyframes fadeUp { from{opacity:0; transform:translateY(10px)} to{opacity:1; transform:none} }

  .trove-search input {
    border-radius:.8rem; border:1.5px solid #e2e8f0;
    padding:.7rem 1rem; font-size:1rem; transition:all .2s ease; }
  .trove-search input:focus {
    border-color:#4f46e5; box-shadow:0 0 0 4px rgba(79,70,229,.12); }

  .field-chip {
    display:inline-block; font-size:.72rem; font-family:monospace;
    background:#eef2ff; color:#4338ca; border:1px solid #c7d2fe;
    border-radius:.5rem; padding:.12rem .45rem; margin:.15rem .2rem 0 0;
    cursor:pointer; transition:all .15s ease; }
  .field-chip:hover { background:#4f46e5; color:#fff; transform:translateY(-1px); }

  .count-note { color:#64748b; font-size:.85rem; margin:.4rem 0 .2rem; }

  table.dataTable tbody tr { transition:background .15s ease; }
  table.dataTable tbody tr:hover { background:#eef2ff !important; }
  .dataTables_wrapper { animation:fadeUp .4s ease both; }
")

# Clickable chips that append `field:` into a search box.
field_chips <- function(fields, input_id) {
  tagList(
    tags$span(class = "count-note", "Search a single field with a prefix: "),
    lapply(fields, function(f) {
      tags$span(class = "field-chip",
                onclick = sprintf(
                  "var el=document.getElementById('%s');
                   el.value=(el.value?el.value.trim()+' ':'')+'%s:';
                   el.focus();
                   el.dispatchEvent(new Event('input',{bubbles:true}));",
                  input_id, f),
                paste0(f, ":"))
    })
  )
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "metacheck trove",
  theme = theme,
  fillable = FALSE,
  header = tags$head(tags$style(trove_css),
                     tags$script(HTML("
    // Animate the stat counters from 0 to their data-target on load.
    function troveCountUp(){
      document.querySelectorAll('.trove-stat-value').forEach(function(el){
        var target=+el.getAttribute('data-target')||0, t0=null, dur=1100;
        function step(ts){ if(!t0)t0=ts; var p=Math.min((ts-t0)/dur,1);
          el.textContent=Math.floor((1-Math.pow(1-p,3))*target).toLocaleString();
          if(p<1) requestAnimationFrame(step); }
        requestAnimationFrame(step);
      });
    }
    document.addEventListener('DOMContentLoaded', function(){ setTimeout(troveCountUp,150); });
  "))),

  # Papers -------------------------------------------------------------------
  nav_panel(
    title = "Papers", icon = shiny::icon("book"),
    div(class = "trove-hero",
        h1("Explore the metacheck trove"),
        p("Browse and search a corpus of machine-readable psychology datasets —",
          " every variable and every identified scale or task, traceable to its folder."),
        div(class = "trove-stats",
          stat_tile(nrow(PAPERS),      "Papers",         "book",   "#a5b4fc"),
          stat_tile(nrow(STUDIES),     "Studies",        "flask",  "#7dd3fc"),
          stat_tile(nrow(var_search),  "Variables",      "table",  "#6ee7b7"),
          stat_tile(nrow(scale_summary), "Scales & tasks", "ruler", "#fca5a5")
        )),
    card(
      card_header("Browse papers"),
      card_body(
        div(class = "trove-search",
            textInput("papers_query", NULL, width = "100%",
                      placeholder = "Filter by title, author, keyword, DOI, or ID")),
        div(class = "count-note",
            "One row per collection folder found under the corpus root."),
        DT::dataTableOutput("papers_table")
      )
    ),
    card(
      card_header("Paper detail"),
      card_body(uiOutput("paper_detail"))
    )
  ),

  # Variables ----------------------------------------------------------------
  nav_panel(
    title = "Variables", icon = shiny::icon("layer-group"),
    card(
      card_header("Search variables across every JSON category"),
      card_body(
        div(class = "trove-search",
            textInput("vars_query", NULL, width = "100%",
                      placeholder = paste(
                        "e.g.  trust   ·   scale:panas   ·  ",
                        "concept:likert level:ordinal   ·   name:rt"))),
        field_chips(VAR_FIELDS, "vars_query"),
        div(class = "count-note", textOutput("vars_count", inline = TRUE)),
        DT::dataTableOutput("vars_table")
      )
    )
  ),

  # Scales & Tasks -----------------------------------------------------------
  nav_panel(
    title = "Scales & Tasks", icon = shiny::icon("ruler-combined"),
    card(
      card_header("Search identified scales & tasks"),
      card_body(
        div(class = "trove-search",
            textInput("scales_query", NULL, width = "100%",
                      placeholder = paste(
                        "e.g.  just world   ·   scale:iat   ·  ",
                        "source:dictionary   ·   confidence:high"))),
        field_chips(SCALE_FIELDS, "scales_query"),
        div(class = "count-note", textOutput("scales_count", inline = TRUE)),
        DT::dataTableOutput("scales_table")
      )
    ),
    card(
      card_header("Papers & variables using this scale/task"),
      card_body(uiOutput("scale_detail"))
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "count-note",
                     style = "padding-right:1rem;",
                     sprintf("%d papers indexed", nrow(PAPERS))))
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  ## Papers ------------------------------------------------------------------
  papers_filtered <- reactive({
    df <- paper_counts
    p <- .parse_query(input$papers_query,
                      c("title", "authors", "keywords", "doi", "paper_id"))
    keep <- .match_query(df, p,
                         c("title", "authors", "keywords", "doi", "paper_id"))
    df[keep, ]
  })

  output$papers_table <- DT::renderDataTable({
    df <- papers_filtered()[, c("paper_id", "title", "authors",
                                "n_studies", "n_variables", "n_scales")]
    names(df) <- c("Paper", "Title", "Authors", "Studies", "Variables", "Scales")
    DT::datatable(df, selection = "single", rownames = FALSE,
                  options = list(pageLength = 12, scrollX = TRUE,
                                 dom = "tip"))
  })

  output$paper_detail <- renderUI({
    sel <- input$papers_table_rows_selected
    if (is.null(sel) || !length(sel)) {
      return(tags$em("Select a paper above to see its studies, keywords, and scales."))
    }
    pid <- papers_filtered()$paper_id[sel]
    p <- PAPERS[PAPERS$paper_id == pid, ]
    st <- STUDIES[STUDIES$paper_id == pid, ]
    sc <- unique(SCALES[SCALES$paper_id == pid & !is.na(SCALES$scale),
                        c("scale", "n_items", "source", "confidence")])
    tagList(
      tags$h4(p$title),
      tags$p(tags$b("Authors: "), p$authors),
      if (!is.na(p$doi)) tags$p(tags$b("DOI: "),
                                tags$a(href = p$doi, p$doi, target = "_blank")),
      if (!is.na(p$keywords)) tags$p(tags$b("Keywords: "), p$keywords),
      tags$p(tags$b("Folder: "), tags$code(p$path)),
      tags$h5("Studies"),
      DT::renderDataTable(
        DT::datatable(st[, c("study_group", "title", "n_variables",
                             "schema_version")],
                      rownames = FALSE, options = list(dom = "t", pageLength = 25))),
      tags$h5(sprintf("Identified scales & tasks (%d)", nrow(sc))),
      if (nrow(sc)) DT::renderDataTable(
        DT::datatable(sc, rownames = FALSE,
                      options = list(dom = "t", pageLength = 25)))
      else tags$em("None identified in this paper.")
    )
  })

  ## Variables ---------------------------------------------------------------
  vars_filtered <- reactive({
    p <- .parse_query(input$vars_query, VAR_FIELDS)
    keep <- .match_query(var_search, p, VAR_FREE_COLS)
    var_search[keep, ]
  })

  output$vars_count <- renderText({
    n <- nrow(vars_filtered())
    sprintf("%d matching variable%s (of %d searched across %d studies).",
            n, if (n == 1) "" else "s", nrow(var_search), nrow(STUDIES))
  })

  output$vars_table <- DT::renderDataTable({
    df <- vars_filtered()[, c("paper_id", "study_group", "name", "label",
                              "concept", "level", "role", "scale",
                              "source_file", "n", "mean", "sd")]
    names(df) <- c("Paper", "Study", "Variable", "Label", "Concept", "Level",
                   "Role", "Scale", "Source file", "n", "Mean", "SD")
    DT::datatable(df, rownames = FALSE, filter = "top",
                  options = list(pageLength = 20, scrollX = TRUE)) |>
      DT::formatRound(c("Mean", "SD"), digits = 2)
  })

  ## Scales & Tasks ----------------------------------------------------------
  scales_filtered <- reactive({
    p <- .parse_query(input$scales_query, SCALE_FIELDS)
    keep <- .match_query(scale_summary, p, SCALE_FREE_COLS)
    scale_summary[keep, ]
  })

  output$scales_count <- renderText({
    n <- nrow(scales_filtered())
    sprintf("%d matching scale%s/task%s (of %d distinct in the corpus).",
            n, if (n == 1) "" else "s", if (n == 1) "" else "s",
            nrow(scale_summary))
  })

  output$scales_table <- DT::renderDataTable({
    df <- scales_filtered()[, c("scale", "n_papers", "median_items",
                                "source", "confidence", "code")]
    names(df) <- c("Scale / task", "Papers", "Median items", "Source",
                   "Confidence", "Code")
    DT::datatable(df, selection = "single", rownames = FALSE,
                  options = list(pageLength = 20, scrollX = TRUE, dom = "tip"))
  })

  output$scale_detail <- renderUI({
    sel <- input$scales_table_rows_selected
    if (is.null(sel) || !length(sel)) {
      return(tags$em("Select a scale/task above to see which papers and variables use it."))
    }
    code <- scales_filtered()$code[sel]
    nm   <- scales_filtered()$scale[sel]
    vars <- unique(VARIABLES[!is.na(VARIABLES$scale_code) &
                               VARIABLES$scale_code == code,
                             c("paper_id", "study_group", "name", "label",
                               "concept")])
    tagList(
      tags$h4(nm),
      tags$p(sprintf("Used in %d paper%s; %d variable instance%s below.",
                     length(unique(vars$paper_id)),
                     if (length(unique(vars$paper_id)) == 1) "" else "s",
                     nrow(vars), if (nrow(vars) == 1) "" else "s")),
      DT::renderDataTable(
        DT::datatable(vars, rownames = FALSE,
                      colnames = c("Paper", "Study", "Variable", "Label", "Concept"),
                      options = list(pageLength = 15, scrollX = TRUE)))
    )
  })
}

shinyApp(ui, server)
