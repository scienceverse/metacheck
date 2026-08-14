## osf_app.R — download OSF projects in two steps: list, then download ##
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
})

options(shiny.maxRequestSize = 100 * 1024^2,
        scipen = 10,
        digits = 4)

source("tabs/osf_list.R")
source("tabs/osf_download.R")


## UI ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "metacheck OSF"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("1. Find Projects", tabName = "list_tab",
               icon = icon("magnifying-glass"), selected = TRUE),
      menuItem("2. Download", tabName = "download_tab",
               icon = icon("download"))
    ),
    HTML("<img src='images/logo.png' alt='Logo' style='width: 85%; margin: 1em;' />")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js"),
      tags$script(src = "osf_direct.js")
    ),
    tabItems(
      list_tab,
      download_tab
    )
  )
)


## can we actually save here? ----
# Whether a folder can be written to. The only reliable test is to create a
# file and remove it again: file.access() is documented as unreliable on
# Windows, and a folder can exist and be readable while still refusing new
# files (the top of a drive, such as C:/, usually does without administrator
# rights). Used to tell "cannot save here" apart from "the download failed".
dir_writable <- function(path) {
  if (!nzchar(path) || !dir.exists(path)) return(FALSE)
  probe <- file.path(path, paste0(".metacheck_write_test_", Sys.getpid()))
  ok <- tryCatch(file.create(probe, showWarnings = FALSE),
                 error = function(e) FALSE, warning = function(w) FALSE)
  if (isTRUE(ok)) unlink(probe)
  isTRUE(ok)
}


## usage statistics ----
#
# ANONYMOUS USAGE STATISTICS. These numbers are kept so that continued
# development of metacheck can be justified in future grant applications,
# which is what pays for the work. Please keep this in place.
#
# What is recorded, and nothing else: the date, whether a session was started
# or a download finished, how many OSF projects were downloaded, and how many
# bytes they came to.
#
# There is NO identifier of any kind: no name, no OSF ID, no project ID, no
# token, no address of the person using the app, and no way to link two rows
# to the same visitor. A row records that something happened, never who did
# it. "Sessions" therefore counts visits rather than people, and overcounts
# anyone who comes back; it is reported that way rather than being dressed up
# as a user count. The file stays on the machine running the app and is never
# sent anywhere.
#
# Nothing is recorded when the app runs locally: only the hosted app counts,
# so development and testing do not inflate the figures.
# Where the file goes. On a Shiny Server the app runs as the `shiny` user,
# whose home directory may not be writable (or set at all), and
# rappdirs::user_data_dir() resolves to ~/.local/share/metacheck there. So the
# location can be set explicitly with the METACHECK_USAGE_DIR environment
# variable in the app's Renviron, and the default is only used when it can
# actually be written to. Returns "" when there is nowhere to write, and
# usage_record() then does nothing.
usage_file <- function(filename = "osf_app_usage.csv") {
  dir <- Sys.getenv("METACHECK_USAGE_DIR")
  if (!nzchar(dir)) {
    dir <- rappdirs::user_data_dir("metacheck", "scienceverse")
  }
  ok <- tryCatch({
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    dir.exists(dir) && file.access(dir, mode = 2) == 0
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok)) return("")
  file.path(dir, filename)
}

usage_record <- function(event, projects = 0L, bytes = 0) {
  # Only count real use of the hosted app. Running it locally is development
  # and testing, which would inflate the figures the grant application rests
  # on. SHINY_PORT is set by Shiny Server and empty everywhere else, which is
  # the same test the app uses to decide how downloading works.
  if (Sys.getenv("SHINY_PORT") == "") return(invisible(NULL))
  # Recording usage must never interfere with using the app, so any failure
  # here (an unwritable folder, a locked file) is swallowed deliberately.
  tryCatch({
    path <- usage_file()
    if (!nzchar(path)) return(invisible(NULL))   # nowhere writable
    row <- data.frame(
      date     = format(Sys.Date()),          # date only, never a timestamp
      event    = event,                       # "session" or "download"
      projects = as.integer(projects),
      bytes    = as.numeric(bytes)
    )
    utils::write.table(row, path, sep = ",", row.names = FALSE,
                       col.names = !file.exists(path), append = file.exists(path))
  }, error = function(e) NULL, warning = function(w) NULL)
  invisible(NULL)
}

#' Read the anonymous usage statistics this app has recorded
#'
#' Returns one row per day with the number of sessions, the number of OSF
#' projects downloaded, and the total size. See the note above for exactly
#' what is and is not recorded.
usage_summary <- function() {
  path <- usage_file()
  if (!nzchar(path) || !file.exists(path)) {
    return(data.frame(date = character(0), sessions = integer(0),
                      downloads = integer(0), projects = integer(0),
                      gb = numeric(0)))
  }
  x <- utils::read.csv(path)
  dates <- sort(unique(x$date))
  data.frame(
    date      = dates,
    sessions  = vapply(dates, function(d) sum(x$date == d & x$event == "session"), 0L),
    downloads = vapply(dates, function(d) sum(x$date == d & x$event == "download"), 0L),
    projects  = vapply(dates, function(d) sum(x$projects[x$date == d]), 0L),
    gb        = vapply(dates, function(d) sum(x$bytes[x$date == d]) / 1024^3, 0),
    row.names = NULL
  )
}


## server ----
server <- function(input, output, session) {
  # One row per visit. See the note above: this counts visits, not people.
  usage_record("session")

  ## reactiveVals ----
  projects        <- reactiveVal(NULL)   # the full listing from stage 1
  list_error      <- reactiveVal("")
  list_running    <- reactiveVal(FALSE)
  dl_result       <- reactiveVal(NULL)   # the table osf_file_download returns
  dl_error        <- reactiveVal("")
  dl_running      <- reactiveVal(FALSE)
  # What the download explained about itself in warnings, which otherwise go
  # only to the console. Shown so a failure gives its reason.
  dl_notes        <- reactiveVal(character(0))
  # The folder the last local download was written to, so the advice at the
  # end can name a real path even when the result table has none.
  dest_used       <- reactiveVal("")
  # Hosted only: what the browser was asked to fetch, and what it has
  # finished, since the server never sees the files themselves.
  direct_items    <- reactiveVal(list())
  direct_done     <- reactiveVal(character(0))
  direct_status   <- reactiveVal("")

  # Size limits only take effect in the modes that list files first. In
  # mode = "all" the whole node is taken as one archive with no file listing,
  # so there is nothing to filter and the inputs would promise a limit that is
  # never applied (see osf_file_download()'s `mode` documentation).
  sizes_apply <- reactive(input$dl_mode %in% c("select", "zip"))

  ## where the files end up ----
  # Running locally, the server IS the user's own computer, so files can be
  # saved straight onto it with osf_file_download() and a folder browser is
  # meaningful. Hosted, the server is elsewhere, so the browser fetches each
  # archive directly from files.osf.io and saves it itself: nothing passes
  # through the server, which would otherwise have to store the whole project
  # and send it a second time. SHINY_PORT is set when a Shiny Server runs the
  # app, which is how report_app.R already tells the two apart.
  is_local <- Sys.getenv("SHINY_PORT") == ""
  # The folder is typed or pasted rather than picked from a browser. A native
  # dialogue (utils::choose.dir) opens on the machine running R and blocks the
  # whole R process until it is answered, so hosted it would open on the
  # server where nobody can dismiss it, and locally it freezes the app while
  # it is open. The in-page browsers are worse to use than pasting a path.
  ### where_to_save_ui ----
  output$where_to_save_ui <- renderUI({
    if (!is_local) {
      return(tagList(
        tags$p(paste("Your browser downloads each project straight from the",
                     "OSF and saves it where your browser puts downloads, so",
                     "nothing is stored on this server. Your browser may ask",
                     "you to allow more than one file when several projects",
                     "are selected.")),
        tags$p(paste("Each project arrives as one zip file per component.",
                     "Files kept on a linked GitHub or Dropbox account are",
                     "not in the OSF archive, so they are not included; to",
                     "get those, run metacheck on your own computer as the",
                     "manual describes below."))
      ))
    }
    tagList(
      # Left empty on purpose. getwd() here is the folder the app itself is
      # in, which is never where someone wants their downloads, so the box
      # starts blank and asks for a path instead of proposing a bad one.
      textInput(
        "download_to",
        "Folder to download into (it must already exist)",
        value = "",
        placeholder = "C:/some_folder_on_your_computer",
        width = "100%"
      ),
      tags$span(paste("Copy the folder from the address bar in File Explorer",
                      "and paste it here. Both C:/folder and C:\\folder work.")),
      uiOutput("folder_check_ui")
    )
  })

  ### current_pat — the token as it stands in the box ----
  # Read from the box wherever it is needed rather than stored by an observer.
  # An observer with ignoreInit = TRUE swallowed the FIRST value typed, so the
  # token never reached the download and every private project failed with a
  # permission refusal; one that fires on every keystroke would store partial
  # tokens instead. Setting it here as well keeps osf_pat() correct for the
  # package functions the app calls, which read it themselves.
  current_pat <- reactive({
    typed <- trimws(input$osf_pat %||% "")
    if (nzchar(typed)) {
      osf_pat(typed)
      return(typed)
    }
    # Nothing typed: fall back to whatever is already set for the session,
    # which is the OSF_PAT environment variable when one is in .Renviron.
    osf_pat()
  })

  ### pat_check — say whether the token actually works ----
  observeEvent(input$pat_check, {
    pat <- trimws(input$osf_pat %||% "")
    if (!nzchar(pat)) {
      showNotification("Enter a token first.", type = "warning")
      return(NULL)
    }
    osf_pat(pat)
    ok <- tryCatch(.osf_pat_validate(pat), error = function(e) FALSE)
    if (isTRUE(ok)) {
      showNotification("The token works. Private projects you can read will be listed.",
                       type = "message")
    } else {
      showNotification(
        paste("The token could not be validated. Check that it was copied in",
              "full and has the osf.full_read scope."),
        type = "error", duration = NULL)
    }
  })

  ### find_projects — stage 1 ----
  observeEvent(input$find_projects, {
    id <- trimws(input$osf_id %||% "")
    # Apply the token before asking the OSF anything: a private project is not
    # listed at all without it.
    current_pat()
    projects(NULL)
    list_error("")
    # A new listing is a new set of projects, so anything ticked from the
    # previous one must not survive into it.
    DT::selectRows(DT::dataTableProxy("project_table"), integer(0))

    if (!nzchar(id)) {
      list_error("Enter an OSF user or project ID first.")
      return(NULL)
    }
    # osf_check_id() warns and returns NA for anything it cannot parse. The
    # warning would only reach the console, which the person using the app
    # never sees, and the NA is reported to them below instead.
    checked <- suppressWarnings(osf_check_id(id))
    if (is.na(checked)) {
      list_error(paste0("'", id, "' is not a valid OSF ID or URL."))
      return(NULL)
    }

    list_running(TRUE)
    on.exit(list_running(FALSE))

    tryCatch({
      withProgress(message = "Asking the OSF", value = 0.3, {
        type <- osf_type(checked)
        incProgress(0.3, detail = "Listing projects...")

        if (identical(type, "users")) {
          tbl <- osf_user_projects(checked)
        } else if (is.na(type)) {
          stop("The OSF did not recognise that ID.", call. = FALSE)
        } else if (identical(type, "inaccessible")) {
          stop(paste("That ID is a valid OSF ID, but the project could not be",
                     "read. If it is private, enter a token above."),
               call. = FALSE)
        } else {
          # A single project: show it as a one-row listing so that stage 2
          # works the same way whether the user started from a profile or a
          # project. osf_info() names the title column `name` (see
          # .osf_parse_response()) and can return more than one row, so take
          # the row for the ID that was asked about.
          info <- osf_info(checked)
          row <- if (is.data.frame(info) && nrow(info) > 0) {
            hit <- which(info$osf_id == checked)
            info[if (length(hit) > 0) hit[[1]] else 1, , drop = FALSE]
          } else NULL
          tbl <- data.frame(
            osf_id   = checked,
            name     = row$name %||% NA_character_,
            category = row$category %||% NA_character_,
            public   = row$public %||% NA,
            osf_url  = paste0("https://osf.io/", checked)
          )
        }
        incProgress(0.4, detail = "Done")
        projects(tbl)
      })
    }, error = function(e) {
      list_error(conditionMessage(e))
    })
  })

  ### filtered — search words and the private-only box ----
  filtered <- reactive({
    tbl <- projects()
    if (is.null(tbl) || nrow(tbl) == 0) return(tbl)

    term <- trimws(input$search_term %||% "")
    if (nzchar(term)) {
      # Search the title and the ID together, so pasting an ID also finds it.
      hay <- paste(tbl$name %||% "", tbl$osf_id)
      keep <- grepl(term, hay, ignore.case = TRUE, fixed = FALSE)
      # An invalid regular expression should narrow nothing rather than error
      keep <- tryCatch(keep, error = function(e) rep(TRUE, nrow(tbl)))
      tbl <- tbl[keep, , drop = FALSE]
    }
    if (isTRUE(input$private_only)) {
      # public is NA when the listing could not read the project at all, which
      # is not the same as known-private, so require an explicit FALSE.
      tbl <- tbl[!is.na(tbl$public) & !tbl$public, , drop = FALSE]
    }
    tbl
  })

  ### project_table ----
  output$project_table <- DT::renderDT({
    tbl <- filtered()
    # shiny:: qualified because metacheck exports a validate() of its own
    # (for checking modules), which otherwise masks Shiny's here.
    shiny::validate(shiny::need(!is.null(tbl) && nrow(tbl) > 0,
                                "No projects to show yet."))
    show <- data.frame(
      ID      = tbl$osf_id,
      Title   = tbl$name,
      Type    = tbl$category,
      Access  = ifelse(is.na(tbl$public), "unknown",
                       ifelse(tbl$public, "public", "private")),
      Link    = sprintf('<a href="%s" target="_blank">%s</a>',
                        tbl$osf_url, tbl$osf_url)
    )
    # Every project on one page: the list is what the user searches and ticks,
    # so paging through it would hide rows that "select all" then appears to
    # have selected. dom = "t" leaves just the table, with no paging controls.
    DT::datatable(show,
                  escape = FALSE,
                  selection = "multiple",
                  rownames = FALSE,
                  options = list(paging = FALSE, dom = "t",
                                 scrollY = "50vh", scrollCollapse = TRUE))
  })

  ### selected_projects — exactly the rows ticked in the table ----
  # The ticks in the table are the only record of what is selected. An earlier
  # version also kept a separate list of chosen IDs and pushed it back into the
  # table with a proxy; the table then reported those ticks back as a new
  # selection, so clicking one row could not clear the others. Reading the
  # table directly means what is ticked is what is downloaded, always.
  selected_projects <- reactive({
    tbl <- filtered()
    empty <- data.frame(osf_id = character(0), name = character(0),
                        category = character(0), public = logical(0),
                        osf_url = character(0))
    if (is.null(tbl) || nrow(tbl) == 0) return(empty)
    rows <- input$project_table_rows_selected
    if (length(rows) == 0) return(empty)
    # A stale row number can arrive from the browser just after the filter
    # narrows the table, so drop anything past the end rather than return NA
    # rows that would be downloaded as missing IDs.
    rows <- rows[rows >= 1 & rows <= nrow(tbl)]
    if (length(rows) == 0) return(empty)
    tbl[rows, , drop = FALSE]
  })

  ### select_all / clear_all ----
  # Both act on the table itself, so the ticks stay the single source of truth.
  observeEvent(input$select_all, {
    tbl <- filtered()
    if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
    # Selects every row the filter is showing, so "only private" followed by
    # "select all shown" selects exactly the private projects.
    DT::selectRows(DT::dataTableProxy("project_table"), seq_len(nrow(tbl)))
  })

  observeEvent(input$clear_all, {
    DT::selectRows(DT::dataTableProxy("project_table"), integer(0))
  })

  ### list_status_ui ----
  output$list_status_ui <- renderUI({
    if (list_running()) {
      return(shiny::p(icon("spinner", class = "fa-spin"),
                      " Asking the OSF, please wait..."))
    }
    err <- list_error()
    if (nzchar(err)) {
      return(shiny::p(style = "color: #c0392b;", icon("circle-xmark"),
                      " ", err))
    }
    tbl <- projects()
    if (is.null(tbl)) return(NULL)
    if (nrow(tbl) == 0) {
      return(shiny::p("No projects were found for that ID.",
                      " A private project needs a token."))
    }
    shown <- nrow(filtered())
    shiny::p(sprintf("Found %d project%s, showing %d. Tick the ones you want, then go to step 2.",
                     nrow(tbl), plural(nrow(tbl)), shown))
  })

  ### folder_check_ui — say whether the folder can be saved to ----
  # Checked as the path is typed, so "you cannot save there" appears before a
  # download is started rather than after everything has been fetched.
  output$folder_check_ui <- renderUI({
    if (!is_local) return(NULL)
    dest <- trimws(input$download_to %||% "")
    dest <- gsub('^"|"$', "", dest)
    if (!nzchar(dest)) return(NULL)

    if (!dir.exists(dest)) {
      return(shiny::p(style = "color: #c0392b;", icon("circle-xmark"),
                      " That folder does not exist."))
    }
    if (!dir_writable(dest)) {
      return(shiny::p(style = "color: #c0392b;", icon("circle-xmark"),
                      paste(" That folder cannot be written to. It probably",
                            "needs administrator rights; choose a folder",
                            "inside your own account instead.")))
    }
    shiny::p(style = "color: #27ae60;", icon("circle-check"),
             " Files can be saved here.")
  })

  ### selection_count_ui — how many are ticked, including any hidden ----
  output$selection_count_ui <- renderUI({
    n <- nrow(selected_projects())
    if (n == 0) return(shiny::p("Nothing selected yet."))
    # Only ticked rows count, and ticks only ever refer to rows the filter is
    # showing, so this is exactly what pressing Download would fetch.
    shiny::p(sprintf("%d project%s selected — only these will be downloaded.",
                     n, plural(n)))
  })

  ### selection_ui — what will be downloaded ----
  output$selection_ui <- renderUI({
    sel <- selected_projects()
    if (is.null(sel) || nrow(sel) == 0) {
      return(shiny::p(style = "color: #c0392b;",
                      "No projects are selected. Go back to step 1 and tick at least one."))
    }
    n_private <- sum(!is.na(sel$public) & !sel$public)
    tagList(
      shiny::p(sprintf("%d project%s selected:", nrow(sel), plural(nrow(sel)))),
      tags$ul(lapply(seq_len(nrow(sel)), function(i) {
        is_private <- !is.na(sel$public[i]) && !sel$public[i]
        tags$li(sel$osf_id[i], " — ", sel$name[i] %||% "(no title)",
                if (is_private) tags$strong(" (private)"))
      })),
      # A private project without a token fails with a permission refusal the
      # OSF reports as 403, so say it before the download rather than after.
      if (n_private > 0 && !nzchar(current_pat())) {
        shiny::p(style = "color: #c0392b;", icon("triangle-exclamation"),
                 sprintf(paste(" %d of these %s private and no token is set.",
                               "Downloading will fail until you enter one in",
                               "step 1."),
                         n_private, if (n_private == 1) "is" else "are"))
      }
    )
  })

  ### size limits only bite in the listing modes ----
  observe({
    # Hosted, the browser takes each component's archive whole from the OSF,
    # so none of these settings has anything to act on.
    shinyjs::toggleState("max_file_size",
                         condition = is_local && sizes_apply())
    shinyjs::toggleState("max_download_size",
                         condition = is_local && sizes_apply())
    shinyjs::toggleState("unzip",
                         condition = is_local && identical(input$dl_mode, "zip"))
    shinyjs::toggleState("dl_mode", condition = is_local)
    shinyjs::toggleState("metadata", condition = is_local)
    shinyjs::toggleState("ignore_folder_structure", condition = is_local)
  })

  output$mode_note_ui <- renderUI({
    if (!is_local) {
      return(shiny::p(style = "color: #8a6d3b;", icon("circle-info"),
                      paste(" These settings apply when metacheck downloads the",
                            "files itself, which happens when you run the app on",
                            "your own computer. Here your browser takes each",
                            "component's archive from the OSF whole, so they are",
                            "switched off.")))
    }
    if (sizes_apply()) return(NULL)
    shiny::p(style = "color: #8a6d3b;",
             icon("circle-info"),
             paste(" In 'all' mode the whole project is taken as one archive per",
                   "component, so the size limits do not apply and a repeated run",
                   "downloads everything again. Choose 'select' to filter by size",
                   "or to resume an interrupted download."))
  })

  ### direct_progress — what the browser reports back ----
  # Sent by osf_direct.js as each archive starts, finishes, or fails. The
  # server learns only what the browser tells it; the files never come here.
  observeEvent(input$direct_progress, {
    msg <- input$direct_progress
    if (is.null(msg)) return(NULL)
    state <- msg$state %||% ""
    id    <- msg$id %||% ""

    if (identical(state, "error")) {
      dl_error(sprintf("%s: %s", id, msg$message %||% "download failed"))
      dl_running(FALSE)
      return(NULL)
    }

    if (identical(state, "done")) {
      already <- id %in% direct_done()
      done <- union(direct_done(), id)
      direct_done(done)
      # Anonymous usage statistics: one archive finished, and how big it was.
      # The browser reports only a byte count; the server never sees the file.
      if (!already) {
        usage_record("download", 1L, as.numeric(msg$received %||% 0))
      }
      if (length(done) >= length(direct_items())) {
        dl_running(FALSE)
        direct_status("")
      }
      return(NULL)
    }

    # "start": a byte count while the archive is arriving. The OSF builds the
    # archive before sending it and reports no length, so show what has come
    # rather than a percentage that cannot be calculated.
    got   <- as.numeric(msg$received %||% 0)
    total <- as.numeric(msg$total %||% 0)
    direct_status(
      if (got > 0 && total > 0) {
        sprintf("%s: %.1f MB of %.1f MB", msg$message %||% id,
                got / 1024^2, total / 1024^2)
      } else if (got > 0) {
        sprintf("%s: %.1f MB so far", msg$message %||% id, got / 1024^2)
      } else {
        sprintf("%s: waiting for the OSF to build the archive...",
                msg$message %||% id)
      })
  })

  ### download — stage 2 ----
  observeEvent(input$start_download, {
    dl_result(NULL)
    dl_error("")
    dl_notes(character(0))
    # Apply the token before downloading anything, whether or not the private
    # check below looks at it.
    pat <- current_pat()

    sel <- selected_projects()
    if (is.null(sel) || nrow(sel) == 0) {
      dl_error("No projects are selected.")
      return(NULL)
    }

    # A private project cannot be read without a token, and the OSF answers
    # 403, which the download treats as a temporary refusal and retries with
    # backoff before giving up. Saying so now is more use than the retries.
    private_sel <- sel$osf_id[!is.na(sel$public) & !sel$public]
    if (length(private_sel) > 0 && !nzchar(pat)) {
      dl_error(sprintf(
        paste("%d of the selected project%s private (%s), and no OSF token is",
              "set. Enter a token in step 1 and press 'Check token', or select",
              "only public projects."),
        length(private_sel), if (length(private_sel) == 1) " is" else "s are",
        paste(private_sel, collapse = ", ")))
      return(NULL)
    }

    ## hosted: the browser fetches from the OSF itself ----
    if (!is_local) {
      # Each component is one archive at its own address. osf_info(recursive)
      # finds the components; a project with none is just itself.
      items <- tryCatch({
        withProgress(message = "Asking the OSF what to fetch", value = 0, {
          nodes <- lapply(seq_len(nrow(sel)), function(i) {
            incProgress(1 / nrow(sel),
                        detail = sprintf("Project %d of %d", i, nrow(sel)))
            info <- tryCatch(osf_info(sel$osf_id[i], recursive = TRUE),
                             error = function(e) NULL)
            ids <- if (!is.null(info) && "osf_type" %in% names(info)) {
              unique(info$osf_id[info$osf_type %in% c("nodes", "registrations")])
            } else character(0)
            ids <- ids[!is.na(ids)]
            if (length(ids) == 0) ids <- sel$osf_id[i]
            title <- sel$name[i] %||% sel$osf_id[i]
            if (is.na(title)) title <- sel$osf_id[i]
            lapply(ids, function(id) {
              list(id = id,
                   name = paste0(title, " (", id, ")"),
                   # The archive the OSF builds for one component's storage,
                   # verified to be served with cross-origin access allowed.
                   url = sprintf(paste0("https://files.osf.io/v1/resources/%s",
                                        "/providers/osfstorage/?zip="), id),
                   filename = paste0(gsub("[^A-Za-z0-9._-]+", "_", id), ".zip"))
            })
          })
          unlist(nodes, recursive = FALSE)
        })
      }, error = function(e) e)

      if (inherits(items, "error")) {
        dl_error(conditionMessage(items))
        return(NULL)
      }
      direct_items(items)
      direct_done(character(0))
      dl_running(TRUE)
      # The token goes from here to the browser, which sends it only to
      # files.osf.io. It is needed there for a private project, because a
      # download started by the browser cannot borrow this session's token.
      session$sendCustomMessage("osfDirectDownload",
                                list(items = items, token = current_pat()))
      return(NULL)
    }

    ## local: write straight onto this computer ----
    dest <- trimws(input$download_to %||% "")
    # Windows Explorer's "Copy as path" wraps the path in quotation marks, and
    # dir.exists() does not recognise the quoted form, so take them off.
    dest <- gsub('^"|"$', "", dest)
    if (!nzchar(dest)) {
      dl_error("Enter a folder to download into.")
      return(NULL)
    }
    if (!dir.exists(dest)) {
      dl_error(paste0("The folder '", dest, "' does not exist. Create it first, ",
                      "or enter a path that does."))
      return(NULL)
    }
    # Saving and downloading fail for different reasons, and the message
    # should say which. A folder such as C:/ exists but cannot be written to
    # without administrator rights, and without this check the download runs,
    # fetches everything, then fails while writing -- reported as a download
    # failure, which sends people looking in the wrong place.
    if (!dir_writable(dest)) {
      dl_error(paste0(
        "The folder '", dest, "' exists but cannot be written to, so nothing ",
        "was downloaded. This usually means it needs administrator rights ",
        "(the top of a drive, such as C:/, normally does). Choose a folder ",
        "inside your own account, such as your Desktop or Documents."))
      return(NULL)
    }

    dest_used(dest)
    dl_running(TRUE)
    on.exit(dl_running(FALSE))

    tryCatch({
      n <- nrow(sel)
      withProgress(message = "Downloading from the OSF", value = 0, {
        # The OSF download writes its own progress to the console, which the
        # browser cannot see, so report per project instead: the bar advances
        # as each project finishes rather than as bytes arrive.
        # An empty numericInput is NA, but the function documents NULL for
        # "no limit", so convert rather than relying on NA behaving the same.
        lim <- function(x) if (isTRUE(sizes_apply()) && !is.null(x) &&
                               length(x) == 1 && !is.na(x)) x else NULL
        results <- vector("list", n)
        # osf_file_download() explains a failure in a warning — a private
        # project refused without a token, or files that did not arrive — and
        # a warning only reaches the console, which nobody using the app can
        # see. Collect them so the reason can be shown instead of a bare
        # "nothing was downloaded".
        notes <- character(0)
        for (i in seq_len(n)) {
          incProgress(0, detail = sprintf("Project %d of %d: %s",
                                          i, n, sel$osf_id[i]))
          results[[i]] <- withCallingHandlers(
            osf_file_download(
              osf_id                  = sel$osf_id[i],
              download_to             = dest,
              max_file_size           = lim(input$max_file_size),
              max_download_size       = lim(input$max_download_size),
              ignore_folder_structure = isTRUE(input$ignore_folder_structure),
              mode                    = input$dl_mode,
              unzip                   = isTRUE(input$unzip),
              metadata                = isTRUE(input$metadata)
            ),
            warning = function(w) {
              notes <<- c(notes, conditionMessage(w))
              invokeRestart("muffleWarning")
            })
          incProgress(1 / n)
        }
        dl_notes(notes)
        res <- do.call(rbind, results)
        dl_result(res)

        # Anonymous usage statistics: how much was downloaded, never by whom.
        # mode = "all" reports one row per node with `bytes`; the other modes
        # report one row per file with `size_on_disk`.
        if (!is.null(res) && nrow(res) > 0) {
          ok <- if ("downloaded" %in% names(res)) res$downloaded %in% TRUE else TRUE
          size_col <- intersect(c("bytes", "size_on_disk"), names(res))
          total <- if (length(size_col) > 0) {
            sum(as.numeric(res[[size_col[[1]]]][ok]), na.rm = TRUE)
          } else 0
          # Count PROJECTS, not rows: a row is a node in mode = "all" but a
          # single file in the other modes, so rows would not be comparable.
          n_proj <- if ("osf_project" %in% names(res)) {
            length(unique(res$osf_project[ok]))
          } else sum(ok)
          if (any(ok)) usage_record("download", n_proj, total)
        }
      })
    }, error = function(e) {
      dl_error(conditionMessage(e))
    })
  })

  ### download_status_ui ----
  output$download_status_ui <- renderUI({
    if (dl_running()) {
      detail <- if (!is_local) direct_status() else ""
      return(tagList(
        shiny::p(icon("spinner", class = "fa-spin"),
                 " Downloading, please wait. Large projects can take many minutes."),
        if (nzchar(detail)) shiny::p(detail),
        if (!is_local) {
          shiny::p(sprintf("%d of %d finished.",
                           length(direct_done()), length(direct_items())))
        }
      ))
    }
    err <- dl_error()
    if (nzchar(err)) {
      return(shiny::p(style = "color: #c0392b;", icon("circle-xmark"),
                      " Error: ", err))
    }
    # Hosted, the files went straight to the browser, so there is no table of
    # downloaded files to report — only what the browser said it finished.
    if (!is_local) {
      done <- direct_done()
      if (length(done) == 0) return(NULL)
      return(tagList(
        shiny::p(style = "color: #27ae60;", icon("circle-check"),
                 sprintf(" %d archive%s downloaded by your browser.",
                         length(done), plural(length(done)))),
        # The browser saves what the OSF sends, which is a zip per component;
        # it cannot write a folder tree, so unzipping is left to the person.
        # report_repository() needs the unzipped folder, hence the order here.
        shiny::p(
          "Each file is a zip archive. Unzip it, then check the data and code ",
          "in it by running ",
          tags$code("report_repository(\"the-unzipped-folder\")"),
          " in R on your own computer — see ",
          tags$a("the manual",
                 href = paste0("https://www.scienceverse.org/metacheck_book/",
                               "chapters/archiving-osf-to-zenodo.html"),
                 target = "_blank"),
          "."
        )
      ))
    }
    res <- dl_result()
    if (is.null(res)) return(NULL)
    # Whatever the download said about itself, so a failure gives its reason
    # rather than leaving people to guess.
    notes <- dl_notes()
    notes_ui <- if (length(notes) > 0) {
      tags$ul(lapply(unique(notes), function(m) tags$li(m)))
    }
    if (nrow(res) == 0) {
      return(tagList(
        shiny::p(style = "color: #c0392b;", icon("circle-xmark"),
                 " Nothing was downloaded."),
        notes_ui))
    }
    # osf_file_download() reports a refusal in the `downloaded` column rather
    # than by raising an error, so a run that fetched nothing would otherwise
    # be announced as a success.
    ok <- if ("downloaded" %in% names(res)) {
      res$downloaded %in% TRUE
    } else rep(TRUE, nrow(res))
    if (!any(ok)) {
      return(tagList(
        shiny::p(style = "color: #c0392b;", icon("circle-xmark"),
                 sprintf(" None of the %d selected project%s could be downloaded.",
                         nrow(res), plural(nrow(res)))),
        # The folder was checked before downloading, so this is a problem at
        # the OSF end rather than a problem saving the files.
        shiny::p(paste("The files could be saved to your folder, so this is",
                       "the OSF refusing to send them rather than a problem",
                       "on your computer. A private project needs a token.")),
        notes_ui
      ))
    }
    where <- unique(res$download_path[ok] %||% "")
    tagList(
      shiny::p(style = "color: #27ae60;", icon("circle-check"),
               sprintf(" Downloaded %d item%s into %s",
                       sum(ok), plural(sum(ok)),
                       paste(where, collapse = ", "))),
      if (any(!ok)) {
        shiny::p(style = "color: #c0392b;",
                 sprintf("%d could not be downloaded.", sum(!ok)))
      },
      if (any(!ok)) notes_ui,
      # Downloaded locally the files are already unzipped, so this can name
      # the folder they are in and skip the unzipping step the hosted app
      # has to describe. download_path can be missing, so fall back to the
      # folder that was typed rather than showing an empty path.
      shiny::p(
        "To check the data and code in what you just downloaded, run ",
        tags$code(sprintf('report_repository("%s")',
                          gsub("\\", "/",
                               if (length(where) > 0 && !is.na(where[[1]]) &&
                                   nzchar(where[[1]])) where[[1]] else dest_used(),
                               fixed = TRUE))),
        " in R — see ",
        tags$a("the manual",
               href = paste0("https://www.scienceverse.org/metacheck_book/",
                             "chapters/archiving-osf-to-zenodo.html"),
               target = "_blank"),
        "."
      )
    )
  })

  ### result_table ----
  output$result_table <- DT::renderDT({
    res <- dl_result()
    shiny::validate(shiny::need(
      !is.null(res) && nrow(res) > 0,
      if (is_local) "Nothing downloaded yet."
      else paste("Your browser saves the files directly, so this server has no",
                 "list of them. Look in your browser's downloads.")))
    DT::datatable(res, rownames = FALSE,
                  options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
  })

  ### moving between the two steps ----
  observeEvent(input$to_download, {
    updateTabItems(session, "tabs", "download_tab")
  })
  observeEvent(input$back_to_list, {
    updateTabItems(session, "tabs", "list_tab")
  })

} # end server()

shinyApp(ui, server)
