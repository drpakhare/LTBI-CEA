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
            tags$tr(tags$td("Outcomes"), tags$td("Cost/QALY, TB cases averted, NMB"))
          ),
          br(),
          h5("Model Structure"),
          tags$table(class = "table table-sm table-bordered",
            tags$tr(tags$th("Setting"), tags$th("Value")),
            tags$tr(tags$td("Model type"), tags$td("Decision tree + Markov cohort")),
            tags$tr(tags$td("Perspective"), tags$td("Indian healthcare system")),
            tags$tr(tags$td("Time horizon"), tags$td("40 years (lifetime)")),
            tags$tr(tags$td("Cycle length"), tags$td("3 months")),
            tags$tr(tags$td("Discount rate"), tags$td("3% (costs & outcomes)")),
            tags$tr(tags$td("Cohort size"), tags$td("100,000")),
            tags$tr(tags$td("WTP threshold"), tags$td("\u20B92,34,859/QALY (1\u00D7 GDP p.c.)"))
          )
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
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Markov State Transition Diagram"),
        card_body(
          DiagrammeR::grVizOutput(ns("markov_diagram"), height = "600px")
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

    output$markov_diagram <- DiagrammeR::renderGrViz({
      markov_state_diagram()
    })
  })
}
