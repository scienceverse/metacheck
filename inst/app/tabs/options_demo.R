### options_demo_tab ----
# Privacy / external-server settings for the demo app. These toggles gate which
# modules are available in the Advanced tab: turning one off disables the
# modules that would send content to, or retrieve information from, an external
# server. See `demo_gating` below for the toggle -> module map.

# toggle id -> modules it controls (kept here so the server can reuse it).
# Checkbox toggles are TRUE/FALSE; create_llm_backend is a radio whose value is
# "none" (disabled), "ollama", or "groq" — see demo_toggle_on() for the test.
demo_gating <- list(
  create_crossref    = c("ref_accuracy"),
  create_pubpeer     = c("ref_pubpeer"),
  create_repos       = c("repo_check", "code_check", "prereg_check", "open_practices"),
  create_llm_backend = c("power"),
  create_causal      = c("causal_claims")
)

# is a given gating input currently "on"? checkboxes -> TRUE; the LLM radio ->
# any value other than "none"
demo_toggle_on <- function(value) {
  if (is.null(value)) return(FALSE)
  if (is.logical(value)) return(isTRUE(value))
  !identical(value, "none")
}

options_demo_tab <- tabItem(
  tabName = "options_tab",

  fluidRow(
    column(
      width = 10, offset = 1,
      box(
        width = 12,
        h3("Options"),
        tags$p(
          "These settings control what metacheck sends to, or retrieves from, ",
          "external servers. Turning an option off disables the modules that ",
          "rely on it in the Advanced tab. By default, only modules that send ",
          "no identifiable paper content are enabled."
        ),

        # ---- where information is sent ----
        tags$div(
          class = "gdpr-privacy-box",
          h4("Where information is sent"),
          tags$p(tags$strong("Sends paper content to an external server:")),
          tags$ul(
            tags$li(
              tags$strong("Randomization & Causal Claims"),
              " — sends the title and abstract to a machine-learning ",
              "classifier hosted on HuggingFace."
            ),
            tags$li(
              tags$strong("Power (LLM mode)"),
              " — only if an LLM backend is selected: sends candidate sentences ",
              "about sample size and power to a local Ollama model or, if you ",
              "choose Groq, to the Groq API."
            )
          ),
          tags$p(tags$strong(
            "Retrieves information via DOIs or URLs (no paper content sent):"
          )),
          tags$ul(
            tags$li(tags$strong("PubPeer Comments"),
                    " — sends reference DOIs to the PubPeer API."),
            tags$li(tags$strong("Reference Accuracy"),
                    " — verifies reference titles/authors via CrossRef."),
            tags$li(tags$strong("Repository Check / Code Check"),
                    " — query OSF, GitHub, and Zenodo via URLs in the paper."),
            tags$li(tags$strong("Preregistration Check / Open Practices"),
                    " — follow AsPredicted / OSF links in the paper.")
          ),
          tags$p(tags$strong("Local only (nothing leaves your machine):")),
          tags$ul(
            tags$li(tags$strong("RetractionWatch / Replication Check"),
                    " — query a bundled internal copy of those databases."),
            tags$li("All other modules process text locally.")
          )
        ),

        radioButtons(
          "grobid_server_choice",
          "PDF Conversion Server:",
          choiceNames = list(
            "Use GDPR compliant GROBID server at Eindhoven University of Technology",
            "Use HuggingFace server in the USA",
            tagList(
              "Use local GROBID server through Docker — see ",
              tags$a("metacheck docs",
                href = paste0("https://www.scienceverse.org/metacheck/",
                              "articles/metacheck.html#load-from-pdf"),
                target = "_blank"),
              " and ",
              tags$a("GROBID docs",
                href = "https://grobid.readthedocs.io/en/latest/",
                target = "_blank")
            )
          ),
          choiceValues = list("metacheck", "huggingface", "local"),
          selected = "metacheck"
        ),

        tags$div(
          class = "create-report-checks",
          checkboxInput("create_crossref", "Query CrossRef", value = FALSE),
          tags$span("Reference Accuracy: verify references via the CrossRef API"),
          checkboxInput("create_pubpeer", "Query PubPeer", value = FALSE),
          tags$span("PubPeer Comments: send reference DOIs to the PubPeer API"),
          checkboxInput("create_repos", "Query Data Repositories", value = FALSE),
          tags$span("Repo/Code/Prereg/Open Practices: query GitHub, Zenodo, OSF, AsPredicted via links"),
          checkboxInput("create_causal", "Causal Claims classifier", value = FALSE),
          tags$span("Randomization & Causal Claims: send title and abstract to a HuggingFace classifier")
        ),

        # ---- LLM backend (Power module) ----
        radioButtons(
          "create_llm_backend",
          "LLM backend (Power module):",
          choiceNames = list(
            "None — do not use an LLM",
            tagList(
              "Ollama — run a model locally (GDPR compliant). See ",
              tags$a("setup help",
                href = "https://www.scienceverse.org/metacheck/articles/ollama.html",
                target = "_blank")
            ),
            tagList(
              "Groq — use the Groq API (requires a GROQ_API_KEY). See ",
              tags$a("setup help",
                href = "https://www.scienceverse.org/metacheck/reference/llm.html",
                target = "_blank")
            )
          ),
          choiceValues = list("none", "ollama", "groq"),
          selected = "none"
        ),

        tags$br(),
        actionButton(
          "options_done", "Done — back to Metacheck",
          icon = icon("arrow-left"),
          class = "btn-options-done"
        )
      )
    )
  )
)
