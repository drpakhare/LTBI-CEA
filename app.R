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

  # Tab 1: Overview
  nav_panel(
    title = "Overview",
    icon = icon("info-circle"),
    mod_overview_ui("overview")
  ),

  # Tab 2: Parameters
  nav_panel(
    title = "Parameters",
    icon = icon("sliders-h"),
    mod_parameters_ui("parameters")
  ),

  # Tab 3: Decision Tree
  nav_panel(
    title = "Screening Results",
    icon = icon("sitemap"),
    mod_decision_tree_ui("dt")
  ),

  # Tab 4: Markov
  nav_panel(
    title = "Long-Term Outcomes",
    icon = icon("chart-line"),
    mod_markov_ui("markov")
  ),

  # Tab 5: CEA
  nav_panel(
    title = "Cost-Effectiveness",
    icon = icon("balance-scale"),
    mod_cea_ui("cea")
  ),

  # Tab 6: DSA
  nav_panel(
    title = "Sensitivity (DSA)",
    icon = icon("tornado"),
    mod_dsa_ui("dsa")
  ),

  # Tab 7: PSA
  nav_panel(
    title = "Probabilistic SA",
    icon = icon("dice"),
    mod_psa_ui("psa")
  ),

  # Tab 8: Reports
  nav_panel(
    title = "Download Reports",
    icon = icon("download"),
    mod_report_ui("report")
  ),

  # Help
  nav_panel(
    title = "Help",
    icon = icon("question-circle"),
    card(
      card_header("User Guide", class = "bg-primary text-white"),
      card_body(
        h4("How to Use This Application"),
        h5("1. Overview Tab"),
        p("Review the study design, comparator strategies, and model structure diagrams."),
        h5("2. Parameters Tab"),
        p("Adjust model parameters using sliders or by editing the detailed tables.
           Parameters are loaded from the Excel workbook and can be modified in real-time.
           Use 'Reset All to Default' to restore original values."),
        h5("3. Screening Results Tab"),
        p("View the decision tree outcomes: how many patients are correctly identified (true positives),
           missed (false negatives), or unnecessarily treated (false positives) under each strategy."),
        h5("4. Long-Term Outcomes Tab"),
        p("Examine the Markov model trace showing how the cohort moves through health states
           over 40 years. Compare TB burden, cumulative costs, and QALYs across strategies."),
        h5("5. Cost-Effectiveness Tab"),
        p("The ICER table shows which strategies are dominated, and the incremental cost per QALY
           for strategies on the efficiency frontier. The CE plane and NMB chart help visualise
           trade-offs. The interpretation panel provides a plain-language summary."),
        h5("6. Sensitivity Analysis (DSA) Tab"),
        p("Run one-way sensitivity analysis to see which parameters most influence the results
           (tornado diagram). Two-way analysis shows how combinations of parameters affect the
           optimal strategy. Threshold analysis identifies critical parameter values."),
        h5("7. Probabilistic SA Tab"),
        p("Monte Carlo simulation drawing from parameter distributions. The CEAC shows the
           probability each strategy is cost-effective across WTP thresholds. EVPI quantifies
           the value of reducing parameter uncertainty."),
        hr(),
        h5("Key Concepts"),
        tags$dl(
          tags$dt("ICER"), tags$dd("Incremental Cost-Effectiveness Ratio = (Cost_A - Cost_B) / (QALY_A - QALY_B)"),
          tags$dt("NMB"), tags$dd("Net Monetary Benefit = QALY x WTP - Cost. Higher NMB = better value."),
          tags$dt("WTP"), tags$dd("Willingness-to-Pay: the maximum a healthcare system will pay per QALY gained.
                                   Indian threshold: 1-3x GDP per capita (\u20B92.35L - \u20B97.05L)."),
          tags$dt("Dominated"), tags$dd("A strategy that costs more AND provides fewer QALYs than another."),
          tags$dt("CEAC"), tags$dd("Cost-Effectiveness Acceptability Curve: probability of being optimal at each WTP."),
          tags$dt("EVPI"), tags$dd("Expected Value of Perfect Information: max value of eliminating all uncertainty.")
        ),
        hr(),
        p(class = "text-muted small",
          "Application built for decision analytic modelling of LTBI screening strategies
           before biologic therapy in Indian rheumatology patients. Parameters sourced from
           systematic literature review. Model follows DARTH/ISPOR guidelines.",
          br(), br(),
          "Version 1.0 | March 2026")
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

  # Model results (reactive on parameter changes)
  model_results <- reactive({
    p <- get_param_values(params_rv())
    tryCatch(
      run_full_model(p, MODEL_SETTINGS),
      error = function(e) {
        showNotification(paste("Model error:", e$message), type = "error")
        NULL
      }
    )
  })

  # Shared reactive for PSA results (populated by PSA module)
  psa_results_rv <- reactiveVal(NULL)

  mod_decision_tree_server("dt", params_rv, MODEL_SETTINGS)
  mod_markov_server("markov", model_results, MODEL_SETTINGS)
  mod_cea_server("cea", model_results, MODEL_SETTINGS)
  mod_dsa_server("dsa", params_rv, MODEL_SETTINGS)
  mod_psa_server("psa", params_rv, MODEL_SETTINGS, psa_results_rv)
  mod_report_server("report", params_rv, model_results, psa_results_rv, MODEL_SETTINGS)
}

# ══════════════════════════════════════════════════════════════════════════════
# LAUNCH
# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui = app_ui, server = app_server)
