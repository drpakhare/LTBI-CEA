# mod_overview.R — Module 1: Model Overview, Diagrams, Study Design

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Study Design Summary", class = "bg-primary text-white"),
        card_body(
          h5("Decision Problem"),
          p("In adult patients with rheumatological disorders who are candidates for
             biologic therapy in India, what is the most cost-effective strategy for
             latent tuberculosis screening and management?"),
          tags$table(class = "table table-sm table-bordered",
            tags$tr(tags$th("PICO Element"), tags$th("Specification")),
            tags$tr(tags$td("Population"), tags$td("Adults with RA/SpA/PsA, biologic-naive, India")),
            tags$tr(tags$td("Interventions"), tags$td("5 screening/management strategies")),
            tags$tr(tags$td("Comparator"), tags$td("TST alone (reference)")),
            tags$tr(tags$td("Outcomes"), tags$td("Cost per TB case averted, LTBI detection, NNT/NNS"))
          ),
          br(),
          h5("Model Structure"),
          tags$table(class = "table table-sm table-bordered",
            tags$tr(tags$th("Setting"), tags$th("Value")),
            tags$tr(tags$td("Primary model"), tags$td("Decision tree")),
            tags$tr(tags$td("TB projection"), tags$td("5-year expected TB from epidemiological reactivation rates")),
            tags$tr(tags$td("Perspective"), tags$td("Indian healthcare system")),
            tags$tr(tags$td("Decision framework"), tags$td("Multi-criteria (MCDA) with adjustable weights")),
            tags$tr(tags$td("Sensitivity analysis"), tags$td("DSA (tornado/threshold) + PSA (Monte Carlo)"))
          ),
          br(),
          h5("Analytical Rationale"),
          p(class = "text-muted",
            "The primary analysis uses a decision tree model because LTBI screening is a ",
            "short-term clinical decision. Long-term Markov modelling (40-year QALY projection) ",
            "shows marginal differences across strategies (< 0.08 QALYs \u2248 27.5 days), ",
            "making ICER unable to meaningfully distinguish strategies. Therefore, the decision ",
            "is better informed by short-term TB prevention effectiveness, screening cost, ",
            "test availability, equity, and public health impact \u2014 captured through the ",
            "multi-criteria decision framework (MCDA).")
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Comparator Strategies"),
        card_body(
          DT::dataTableOutput(ns("strategy_table"))
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Decision Tree Structure"),
        card_body(
          DiagrammeR::grVizOutput(ns("dt_diagram"), height = "600px")
        )
      )
    )
  )
}

mod_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$strategy_table <- DT::renderDataTable({
      si <- strategy_info()
      DT::datatable(
        si %>% select(name, test, rule) %>%
          rename(Strategy = name, `Screening Test(s)` = test, `Treatment Rule` = rule),
        options = list(dom = "t", pageLength = 5, ordering = FALSE),
        rownames = FALSE
      )
    })

    output$dt_diagram <- DiagrammeR::renderGrViz({
      decision_tree_diagram()
    })
  })
}
