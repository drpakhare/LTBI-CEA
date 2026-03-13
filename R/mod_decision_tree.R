# mod_decision_tree.R — Module 3: Screening Phase Results

mod_decision_tree_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Screening Outcomes by Strategy", class = "bg-primary text-white"),
        card_body(
          DT::dataTableOutput(ns("dt_table")),
          br(),
          div(class = "text-muted small",
            "TP = True Positive (LTBI correctly identified); FP = False Positive;
             FN = False Negative (LTBI missed); TN = True Negative.
             Cost is per-person screening + prophylaxis + adverse event cost.")
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Test Classification Distribution"),
        card_body(plotlyOutput(ns("plot_classification"), height = "550px"))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Per-Person Screening Cost Breakdown"),
        card_body(plotlyOutput(ns("plot_costs"), height = "550px"))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("LTBI Detection Rate vs. Unnecessary Treatment"),
        card_body(plotlyOutput(ns("plot_tradeoff"), height = "550px"))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Interpretation"),
        card_body(
          uiOutput(ns("interpretation"))
        )
      )
    )
  )
}

mod_decision_tree_server <- function(id, params_rv, settings) {
  moduleServer(id, function(input, output, session) {

    dt_results <- reactive({
      p <- get_param_values(params_rv())
      run_decision_tree(p)
    })

    output$dt_table <- DT::renderDataTable({
      dt <- dt_results()
      si <- strategy_info()
      display <- dt %>%
        left_join(si %>% select(id, name), by = c("strategy" = "id")) %>%
        transmute(
          Strategy = name,
          `LTBI Prevalence` = fmt_pct(p_ltbi),
          Sensitivity = fmt_pct(sensitivity),
          Specificity = fmt_pct(specificity),
          `True Positive` = fmt_pct(p_true_positive),
          `False Positive` = fmt_pct(p_false_positive),
          `False Negative` = fmt_pct(p_false_negative),
          `% Treated` = fmt_pct(p_treated),
          `% LTBI Missed` = fmt_pct(p_ltbi_missed),
          `Hepatotoxicity Risk` = fmt_pct(p_hepatox),
          `Cost/Person (INR)` = fmt_inr(cost_total_dt)
        )

      datatable(display, options = list(dom = "t", pageLength = 5, ordering = FALSE),
                rownames = FALSE) %>%
        formatStyle("Strategy", fontWeight = "bold")
    })

    output$plot_classification <- renderPlotly({
      dt <- dt_results()
      si <- strategy_info()

      plot_data <- dt %>%
        left_join(si %>% select(id, name), by = c("strategy" = "id")) %>%
        select(name, p_true_positive, p_false_positive, p_false_negative, p_true_negative) %>%
        pivot_longer(-name, names_to = "category", values_to = "proportion") %>%
        mutate(category = case_when(
          category == "p_true_positive" ~ "True Positive",
          category == "p_false_positive" ~ "False Positive",
          category == "p_false_negative" ~ "False Negative (LTBI Missed)",
          category == "p_true_negative" ~ "True Negative"
        ))

      colors <- c("True Positive" = "#70AD47", "False Positive" = "#FFC000",
                   "False Negative (LTBI Missed)" = "#FF4444", "True Negative" = "#4472C4")

      p <- ggplot(plot_data, aes(x = name, y = proportion, fill = category)) +
        geom_col(position = "stack") +
        scale_fill_manual(values = colors) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = NULL, y = "Proportion of Cohort", fill = NULL) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

      ggplotly(p, tooltip = c("x", "fill", "y")) %>%
        layout(legend = list(orientation = "h", y = -0.2))
    })

    output$plot_costs <- renderPlotly({
      dt <- dt_results()
      si <- strategy_info()

      cost_data <- dt %>%
        left_join(si %>% select(id, name), by = c("strategy" = "id")) %>%
        select(name, cost_screening, cost_prophylaxis, cost_adverse_events) %>%
        pivot_longer(-name, names_to = "component", values_to = "cost") %>%
        mutate(component = case_when(
          component == "cost_screening" ~ "Screening Tests",
          component == "cost_prophylaxis" ~ "TB Prophylaxis",
          component == "cost_adverse_events" ~ "Adverse Events"
        ))

      colors <- c("Screening Tests" = "#2E5090", "TB Prophylaxis" = "#4472C4",
                   "Adverse Events" = "#FF4444")

      p <- ggplot(cost_data, aes(x = name, y = cost, fill = component)) +
        geom_col(position = "stack") +
        scale_fill_manual(values = colors) +
        scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
        labs(x = NULL, y = "Cost per Person (INR)", fill = NULL) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

      ggplotly(p, tooltip = c("x", "fill", "y")) %>%
        layout(legend = list(orientation = "h", y = -0.2))
    })

    output$plot_tradeoff <- renderPlotly({
      dt <- dt_results()
      si <- strategy_info()

      df <- dt %>%
        left_join(si %>% select(id, name, color), by = c("strategy" = "id"))

      p <- ggplot(df, aes(x = p_false_positive * 100, y = (1 - p_ltbi_missed / p_ltbi) * 100,
                          color = name, size = cost_total_dt)) +
        geom_point(alpha = 0.8) +
        scale_color_manual(values = setNames(df$color, df$name)) +
        labs(x = "False Positive Rate (%)", y = "LTBI Detection Rate (%)",
             color = "Strategy", size = "Cost/Person") +
        theme_minimal(base_size = 12)

      ggplotly(p, tooltip = c("x", "y", "color", "size"))
    })

    output$interpretation <- renderUI({
      dt <- dt_results()
      si <- strategy_info()
      dt <- dt %>% left_join(si %>% select(id, name), by = c("strategy" = "id"))

      best_detect <- dt %>% slice_max(1 - p_ltbi_missed / p_ltbi, n = 1)
      cheapest <- dt %>% slice_min(cost_total_dt, n = 1)
      most_specific <- dt %>% filter(strategy != "TreatAll") %>% slice_min(p_false_positive, n = 1)

      tagList(
        tags$p(tags$strong("Key findings from the screening phase:")),
        tags$ul(
          tags$li(paste0(best_detect$name[1], " achieves the highest LTBI detection rate (",
                        fmt_pct(1 - best_detect$p_ltbi_missed[1] / best_detect$p_ltbi[1]), ").")),
          tags$li(paste0(cheapest$name[1], " has the lowest per-person screening cost (",
                        fmt_inr(cheapest$cost_total_dt[1]), ").")),
          tags$li(paste0(most_specific$name[1], " has the lowest false positive rate (",
                        fmt_pct(most_specific$p_false_positive[1]),
                        "), minimising unnecessary prophylaxis.")),
          tags$li(paste0("Treat All ensures zero missed LTBI cases but treats ",
                        fmt_pct(dt$p_false_positive[dt$strategy == "TreatAll"]),
                        " of patients unnecessarily."))
        )
      )
    })
  })
}
