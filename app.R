# app.R — LTBI Screening CEA Shiny Application
# Main entry point

source("global.R")

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
app_ui <- page_navbar(
  title = "LTBI-CEA",
  id = "main_nav",
  fillable = FALSE,
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2E5090",
    success = "#70AD47",
    warning = "#ED7D31",
    danger = "#FF4444",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  bg = "#2E5090",

  # ── Tab 1: Overview ──
  nav_panel(
    title = "Overview",
    icon = icon("info-circle"),
    mod_overview_ui("overview")
  ),

  # ── Tab 2: Parameters ──
  nav_panel(
    title = "Parameters",
    icon = icon("sliders-h"),
    mod_parameters_ui("parameters")
  ),

  # ── Tab 3: Screening Results (Decision Tree) ──
  nav_panel(
    title = "Screening",
    icon = icon("sitemap"),
    mod_decision_tree_ui("dt")
  ),

  # ── Tab 4: PRIMARY CEA (Decision Tree Based) ──
  nav_panel(
    title = "CEA",
    icon = icon("star"),
    mod_dt_cea_ui("dt_cea")
  ),

  # ── Tab 5: Decision Framework (MCDA) ──
  nav_panel(
    title = "Decision Framework",
    icon = icon("th-list"),
    mod_mcda_ui("mcda")
  ),

  # ── Tab 6: Sensitivity Analysis ──
  nav_menu(
    title = "Sensitivity",
    icon = icon("tornado"),
    nav_panel(
      title = "DSA (Deterministic)",
      icon = icon("tornado"),
      mod_dsa_ui("dsa")
    ),
    nav_panel(
      title = "PSA (Probabilistic)",
      icon = icon("dice"),
      mod_psa_ui("psa")
    )
  ),

  # ── Tab 7: Reports ──
  nav_panel(
    title = "Reports",
    icon = icon("download"),
    mod_report_ui("report")
  ),

  # ── Help ──
  nav_panel(
    title = "Help",
    icon = icon("question-circle"),
    card(
      card_header("User Guide", class = "bg-primary text-white"),
      card_body(
        h4("How to Use This Application"),
        h5("1. Overview"),
        p("Review the study design, comparator strategies, and model structure diagrams."),
        h5("2. Parameters"),
        p("Adjust model parameters using sliders or detailed tables.",
          "Parameters loaded from the Excel workbook; modifiable in real-time."),
        h5("3. Screening"),
        p("Decision tree outcomes: true positives, false negatives (LTBI missed),",
          "false positives (unnecessary treatment), and per-person screening costs."),
        h5("4. CEA (Primary Analysis)"),
        p("Short-term cost-effectiveness based on the decision tree: cost per LTBI detected,",
          "cost per TB case averted, NNT/NNS. This is the primary analysis because",
          "screening decisions are fundamentally short-term and long-term QALY differences",
          "across strategies are marginal."),
        h5("5. Decision Framework"),
        p("Multi-criteria analysis with adjustable weights. When ICER cannot distinguish",
          "strategies, the decision should incorporate TB prevention priority, test availability,",
          "equity, and public health impact. Adjust weights to reflect your setting."),
        h5("6. Sensitivity Analysis"),
        p("DSA: one-way and two-way deterministic sensitivity analysis on DT outcomes.",
          "PSA: Monte Carlo simulation showing parameter uncertainty impact on TB burden and cost."),
        hr(),
        h5("Key Concepts"),
        tags$dl(
          tags$dt("NNT"), tags$dd("Number Needed to Treat: patients given prophylaxis to prevent one TB case."),
          tags$dt("NNS"), tags$dd("Number Needed to Screen: patients screened to detect one true LTBI case."),
          tags$dt("ICER"), tags$dd("Incremental Cost-Effectiveness Ratio = \u0394Cost / \u0394QALY (supplementary)."),
          tags$dt("MCDA"), tags$dd("Multi-Criteria Decision Analysis: weighted scoring across multiple criteria."),
          tags$dt("WTP"), tags$dd("Willingness-to-Pay: Indian threshold 1-3\u00D7 GDP per capita (\u20B92.35L-\u20B97.05L).")
        ),
        hr(),
        p(class = "text-muted small",
          "LTBI screening strategies before biologic therapy in Indian rheumatology patients.",
          "Parameters from systematic literature review. Model follows ISPOR guidelines.",
          br(), br(),
          "Version 2.0 | March 2026")
      )
    )
  ),

  nav_spacer(),
  nav_item(
    tags$a(class = "nav-link", href = "#",
           onclick = "window.print()", icon("print"), "Print")
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════
app_server <- function(input, output, session) {
  # Shared reactive: editable parameters
  params_rv <- reactiveVal(params_default)

  # Module servers
  mod_overview_server("overview")
  mod_parameters_server("parameters", params_rv)

  # Model results (reactive on parameter changes) — DT-only for speed
  model_results <- reactive({
    p <- get_param_values(params_rv())
    tryCatch(
      run_dt_only_model(p, MODEL_SETTINGS),
      error = function(e) {
        showNotification(paste("Model error:", e$message), type = "error")
        NULL
      }
    )
  })

  # Shared reactive for PSA results (populated by PSA module)
  psa_results_rv <- reactiveVal(NULL)

  mod_decision_tree_server("dt", params_rv, MODEL_SETTINGS)
  mod_dt_cea_server("dt_cea", model_results, params_rv, MODEL_SETTINGS)
  mod_mcda_server("mcda", model_results, MODEL_SETTINGS)
  mod_dsa_server("dsa", params_rv, MODEL_SETTINGS)
  mod_psa_server("psa", params_rv, MODEL_SETTINGS, psa_results_rv)
  mod_report_server("report", params_rv, model_results, psa_results_rv, MODEL_SETTINGS)
}

# ══════════════════════════════════════════════════════════════════════════════
# LAUNCH
# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui = app_ui, server = app_server)
