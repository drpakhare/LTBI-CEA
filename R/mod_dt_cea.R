# mod_dt_cea.R — Module: Short-Term Cost-Effectiveness (Decision Tree Based)
# PRIMARY ANALYSIS — no long-term projection, directly actionable

mod_dt_cea_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          tagList(
            icon("star", class = "me-1"),
            "Primary Analysis: Screening Cost-Effectiveness"
          ),
          class = "bg-primary text-white"
        ),
        card_body(
          p(class = "text-muted",
            "This is the primary cost-effectiveness analysis based on the decision tree model.",
            "It compares strategies on screening costs, LTBI detection, and expected TB burden",
            "using published reactivation rates — without requiring long-term Markov projections.",
            br(), br(),
            strong("Rationale: "),
            "The screening decision is fundamentally short-term: which test to use and who to treat.",
            "Long-term modelling (40 years) introduces speculative assumptions about biologic",
            "adherence, cost stability, and mortality. When lifetime QALY differences are marginal",
            "(< 0.08 QALYs across all strategies), the decision should be driven by",
            "directly observable screening performance and a multi-criteria framework."
          )
        )
      )
    ),

    # ── Key metrics table ──
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Screening Performance & Short-Term CEA"),
        card_body(
          DT::dataTableOutput(ns("dt_cea_table")),
          br(),
          div(class = "text-muted small",
            "TB rates calculated using annual reactivation rate on biologics (RR × base rate).",
            "NNT = number needed to treat with prophylaxis to prevent one TB case (5-year horizon).",
            "Cost per TB averted is compared to a no-screening baseline.")
        )
      )
    ),

    # ── TB burden comparison ──
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Expected TB Cases per 1000 Screened"),
        card_body(plotlyOutput(ns("plot_tb_burden"), height = "450px"))
      ),
      card(
        card_header("Cost per TB Case Averted"),
        card_body(plotlyOutput(ns("plot_cost_per_tb"), height = "450px"))
      )
    ),

    # ── Efficiency trade-off ──
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Screening Efficiency Frontier: TB Averted vs. Cost"),
        card_body(plotlyOutput(ns("plot_frontier"), height = "500px"))
      )
    ),

    # ── NNT/NNS table ──
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Number Needed to Screen (NNS) & Number Needed to Treat (NNT)"),
        card_body(
          DT::dataTableOutput(ns("nnt_table")),
          br(),
          div(class = "text-muted small",
            "NNS = patients screened to detect one true LTBI case.",
            "NNT = patients treated with prophylaxis to prevent one active TB case (5-year).",
            "Lower NNT indicates more efficient use of prophylaxis.")
        )
      )
    ),

    # ── Interpretation ──
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Short-Term CEA Interpretation", class = "bg-success text-white"),
        card_body(uiOutput(ns("interpretation")))
      )
    ),

    # ── Download ──
    layout_columns(
      col_widths = c(12),
      card(
        card_body(
          downloadButton(ns("dl_dt_cea_csv"), "Download Short-Term CEA (.csv)",
                        class = "btn-outline-primary btn-sm",
                        icon = icon("download"))
        )
      )
    )
  )
}


mod_dt_cea_server <- function(id, model_results_rv, params_rv, settings) {
  moduleServer(id, function(input, output, session) {

    dt_cea <- reactive({
      res <- model_results_rv()
      if (is.null(res)) return(NULL)
      res$dt_cea
    })

    output$dt_cea_table <- DT::renderDataTable({
      df <- dt_cea()
      if (is.null(df)) return(datatable(data.frame()))

      display <- df %>%
        transmute(
          Strategy = strategy_name,
          `Sensitivity` = fmt_pct(sensitivity),
          `Specificity` = fmt_pct(specificity),
          `LTBI Detected /1000` = round(ltbi_detected_per_1000, 1),
          `LTBI Missed /1000` = round(ltbi_missed_per_1000, 1),
          `Unnecessary Rx /1000` = round(unnecessarily_treated_per_1000, 1),
          `Cost/Person` = fmt_inr(cost_total_dt),
          `Expected TB (5y) /1000` = round(expected_tb_5y_per_1000, 2),
          `TB Averted (5y) /1000` = round(tb_averted_5y_per_1000, 2),
          `Cost per TB Averted` = ifelse(is.na(cost_per_tb_averted), "-",
                                          fmt_inr(cost_per_tb_averted))
        )

      datatable(display,
        options = list(dom = "t", pageLength = 5, ordering = FALSE),
        rownames = FALSE
      ) %>%
        formatStyle("LTBI Missed /1000",
          backgroundColor = styleInterval(c(50, 100, 200),
            c("#E0FFE0", "#FFF0E0", "#FFE0E0", "#FF8080")),
          fontWeight = "bold"
        ) %>%
        formatStyle("Expected TB (5y) /1000",
          backgroundColor = styleInterval(c(3, 5, 8),
            c("#E0FFE0", "#FFF0E0", "#FFE0E0", "#FF8080")),
          fontWeight = "bold"
        )
    })

    output$plot_tb_burden <- renderPlotly({
      df <- dt_cea()
      if (is.null(df)) return(plotly_empty())
      si <- strategy_info()

      plot_df <- df %>%
        left_join(si %>% select(id, color), by = c("strategy" = "id")) %>%
        select(strategy_name, expected_tb_1y_per_1000, expected_tb_5y_per_1000, color) %>%
        pivot_longer(
          cols = c(expected_tb_1y_per_1000, expected_tb_5y_per_1000),
          names_to = "horizon", values_to = "tb_rate"
        ) %>%
        mutate(horizon = ifelse(grepl("1y", horizon), "1 Year", "5 Years"))

      p <- ggplot(plot_df, aes(x = strategy_name, y = tb_rate, fill = horizon)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("1 Year" = "#4472C4", "5 Years" = "#ED7D31")) +
        labs(x = NULL, y = "Expected TB Cases per 1000", fill = "Time Horizon") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

      ggplotly(p, tooltip = c("x", "fill", "y")) %>%
        layout(legend = list(orientation = "h", y = -0.2))
    })

    output$plot_cost_per_tb <- renderPlotly({
      df <- dt_cea()
      if (is.null(df)) return(plotly_empty())
      si <- strategy_info()

      plot_df <- df %>%
        left_join(si %>% select(id, color), by = c("strategy" = "id")) %>%
        filter(!is.na(cost_per_tb_averted))

      p <- ggplot(plot_df, aes(x = reorder(strategy_name, cost_per_tb_averted),
                                y = cost_per_tb_averted, fill = strategy_name)) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = setNames(plot_df$color, plot_df$strategy_name)) +
        scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
        labs(x = NULL, y = "Cost per TB Case Averted (INR)", fill = NULL) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

      ggplotly(p, tooltip = c("x", "y"))
    })

    output$plot_frontier <- renderPlotly({
      df <- dt_cea()
      if (is.null(df)) return(plotly_empty())
      si <- strategy_info()

      plot_df <- df %>%
        left_join(si %>% select(id, color, name), by = c("strategy" = "id"))

      # No-screening reference point
      no_screen_tb <- df$tb_no_screening_5y[1]

      p <- ggplot(plot_df,
                   aes(x = tb_averted_5y_per_1000, y = cost_total_dt,
                       color = strategy_name)) +
        geom_point(size = 5, alpha = 0.9) +
        geom_text(aes(label = strategy_name), vjust = -1.2, size = 3.5) +
        scale_color_manual(values = setNames(plot_df$color, plot_df$strategy_name)) +
        scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
        labs(x = "TB Cases Averted per 1000 (5-year, vs no screening)",
             y = "Screening Cost per Person (INR)",
             color = "Strategy") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")

      ggplotly(p, tooltip = c("x", "y", "color"))
    })

    output$nnt_table <- DT::renderDataTable({
      df <- dt_cea()
      if (is.null(df)) return(datatable(data.frame()))

      display <- df %>%
        transmute(
          Strategy = strategy_name,
          `% Treated` = fmt_pct(p_treated),
          `NNS (detect 1 LTBI)` = ifelse(is.na(nns_detect_ltbi), "-",
                                           round(nns_detect_ltbi, 1)),
          `NNT (prevent 1 TB, 5y)` = ifelse(is.na(nnt_prevent_tb), "-",
                                              round(nnt_prevent_tb, 0)),
          `TB Risk (untreated LTBI, 5y)` = fmt_pct(tb_risk_untreated_5y),
          `TB Risk (treated LTBI, 5y)` = fmt_pct(tb_risk_treated_5y),
          `Absolute Risk Reduction (5y)` = fmt_pct(tb_risk_untreated_5y - tb_risk_treated_5y)
        )

      datatable(display,
        options = list(dom = "t", pageLength = 5, ordering = FALSE),
        rownames = FALSE
      )
    })

    output$interpretation <- renderUI({
      df <- dt_cea()
      if (is.null(df)) return(p("Run the model to see results."))

      cheapest <- df %>% slice_min(cost_total_dt, n = 1)
      best_tb <- df %>% slice_min(expected_tb_5y_per_1000, n = 1)
      best_detect <- df %>% slice_max(ltbi_detected_per_1000, n = 1)
      lowest_unnecessary <- df %>%
        filter(strategy != "TreatAll") %>%
        slice_min(unnecessarily_treated_per_1000, n = 1)

      # Cost range
      cost_range <- max(df$cost_total_dt) - min(df$cost_total_dt)

      tagList(
        tags$p(tags$strong("Key findings from screening cost-effectiveness analysis:")),
        tags$ul(
          tags$li(HTML(paste0(
            "<strong>", best_detect$strategy_name[1], "</strong> detects the most LTBI (",
            round(best_detect$ltbi_detected_per_1000[1], 0), "/1000) and has the ",
            "lowest 5-year TB burden (", round(best_tb$expected_tb_5y_per_1000[1], 1), "/1000)."))),
          tags$li(HTML(paste0(
            "<strong>", cheapest$strategy_name[1], "</strong> has the lowest screening cost (",
            fmt_inr(cheapest$cost_total_dt[1]), "/person)."))),
          tags$li(HTML(paste0(
            "<strong>", lowest_unnecessary$strategy_name[1], "</strong> has the fewest unnecessary ",
            "treatments (", round(lowest_unnecessary$unnecessarily_treated_per_1000[1], 0),
            "/1000 screened), reflecting highest specificity."))),
          tags$li(HTML(paste0(
            "Cost difference between cheapest and most expensive strategy: <strong>",
            fmt_inr(cost_range), "/person</strong> — a one-time screening cost difference that is ",
            "small relative to annual biologic cost (\u20B91.68 lakh/year).")))
        ),
        hr(),
        tags$p(class = "text-muted",
          "Note: These metrics capture the screening decision directly without 40-year projections.",
          "Long-term QALY differences across all strategies are marginal (< 0.08 QALYs over 40 years),",
          "confirming that the choice should be guided by screening performance, public health priorities,",
          "and local resource availability rather than ICER alone.",
          "See the Decision Framework tab for multi-criteria analysis."
        )
      )
    })

    output$dl_dt_cea_csv <- downloadHandler(
      filename = function() "LTBI_ShortTerm_CEA.csv",
      content = function(file) {
        df <- dt_cea()
        if (is.null(df)) return()
        export <- df %>%
          transmute(
            Strategy = strategy_name,
            Sensitivity = round(sensitivity, 3),
            Specificity = round(specificity, 3),
            LTBI_Detected_per_1000 = round(ltbi_detected_per_1000, 1),
            LTBI_Missed_per_1000 = round(ltbi_missed_per_1000, 1),
            Unnecessarily_Treated_per_1000 = round(unnecessarily_treated_per_1000, 1),
            Cost_per_Person_INR = round(cost_total_dt, 0),
            Expected_TB_1y_per_1000 = round(expected_tb_1y_per_1000, 2),
            Expected_TB_5y_per_1000 = round(expected_tb_5y_per_1000, 2),
            TB_Averted_5y_per_1000 = round(tb_averted_5y_per_1000, 2),
            Cost_per_TB_Averted = round(cost_per_tb_averted, 0),
            NNS_detect_LTBI = round(nns_detect_ltbi, 1),
            NNT_prevent_TB_5y = round(nnt_prevent_tb, 0)
          )
        write.csv(export, file, row.names = FALSE)
      }
    )
  })
}
