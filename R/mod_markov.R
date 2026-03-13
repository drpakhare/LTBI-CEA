# mod_markov.R — Module 4: Markov Model Long-Term Outcomes

mod_markov_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Long-Term Outcomes Summary", class = "bg-primary text-white"),
        card_body(DT::dataTableOutput(ns("summary_table")))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Markov Trace — Select Strategy"),
        card_body(
          selectInput(ns("sel_strategy"), NULL,
                     choices = setNames(MODEL_SETTINGS$strategies, MODEL_SETTINGS$strategy_names),
                     selected = "TST"),
          plotlyOutput(ns("plot_trace"), height = "550px")
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Cumulative TB Cases Over Time"),
        card_body(plotlyOutput(ns("plot_tb_incidence"), height = "550px"))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Cumulative Discounted Costs"),
        card_body(plotlyOutput(ns("plot_cum_costs"), height = "550px"))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Cumulative Discounted QALYs"),
        card_body(plotlyOutput(ns("plot_cum_qalys"), height = "550px"))
      )
    )
  )
}

mod_markov_server <- function(id, model_results_rv, settings) {
  moduleServer(id, function(input, output, session) {

    output$summary_table <- DT::renderDataTable({
      res <- model_results_rv()
      if (is.null(res)) return(datatable(data.frame()))

      display <- res$summary %>%
        transmute(
          Strategy = strategy_name,
          `Cost/Person (INR)` = fmt_inr(cost_total_per_person),
          `QALYs/Person` = round(qaly_per_person, 3),
          `TB Cases/1000` = round(tb_cases_per_1000, 2),
          `% Treated` = fmt_pct(p_treated),
          `% LTBI Missed` = fmt_pct(p_ltbi_missed)
        )

      datatable(display, options = list(dom = "t", pageLength = 5), rownames = FALSE) %>%
        formatStyle("Strategy", fontWeight = "bold")
    })

    output$plot_trace <- renderPlotly({
      res <- model_results_rv()
      if (is.null(res)) return(plotly_empty())

      strat <- input$sel_strategy
      mk <- res$markov_results[[strat]]
      if (is.null(mk)) return(plotly_empty())

      trace_df <- as.data.frame(mk$trace)
      trace_df$year <- (0:settings$n_cycles) * settings$cycle_length
      trace_df <- trace_df %>%
        select(-Dead) %>%
        pivot_longer(-year, names_to = "State", values_to = "Population")

      state_colors <- c(
        "Well_on_biologic" = "#70AD47", "LTBI_untreated" = "#ED7D31",
        "LTBI_prophylaxis" = "#4472C4", "Active_TB" = "#FF4444",
        "TB_treatment" = "#FFC000", "Post_TB" = "#BDD7EE"
      )

      p <- ggplot(trace_df, aes(x = year, y = Population, fill = State)) +
        geom_area(alpha = 0.8) +
        scale_fill_manual(values = state_colors) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Years", y = "Cohort Population", fill = "Health State") +
        theme_minimal(base_size = 12)

      ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
    })

    output$plot_tb_incidence <- renderPlotly({
      res <- model_results_rv()
      if (is.null(res)) return(plotly_empty())

      si <- strategy_info()
      tb_data <- lapply(names(res$markov_results), function(s) {
        mk <- res$markov_results[[s]]
        trace <- mk$trace
        # Cumulative TB = initial non-TB minus current non-TB-non-Dead
        cum_tb <- cumsum(c(0, diff(trace[, "Active_TB"])))
        # Better: track inflow
        tibble::tibble(
          year = (0:settings$n_cycles) * settings$cycle_length,
          active_tb = trace[, "Active_TB"] + trace[, "TB_treatment"] + trace[, "Post_TB"],
          strategy = s,
          strategy_name = si$name[si$id == s]
        )
      })
      tb_df <- bind_rows(tb_data)

      p <- ggplot(tb_df, aes(x = year, y = active_tb, color = strategy_name)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = setNames(si$color, si$name)) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Years", y = "Persons with TB History", color = "Strategy") +
        theme_minimal(base_size = 12)

      ggplotly(p)
    })

    output$plot_cum_costs <- renderPlotly({
      res <- model_results_rv()
      if (is.null(res)) return(plotly_empty())

      si <- strategy_info()
      cost_data <- lapply(names(res$markov_results), function(s) {
        mk <- res$markov_results[[s]]
        tibble::tibble(
          year = (0:settings$n_cycles) * settings$cycle_length,
          cum_cost = cumsum(mk$disc_cycle_costs) / settings$n_cohort,
          strategy_name = si$name[si$id == s]
        )
      })
      cost_df <- bind_rows(cost_data)

      p <- ggplot(cost_df, aes(x = year, y = cum_cost, color = strategy_name)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = setNames(si$color, si$name)) +
        scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
        labs(x = "Years", y = "Cumulative Discounted Cost/Person", color = NULL) +
        theme_minimal(base_size = 12)

      ggplotly(p)
    })

    output$plot_cum_qalys <- renderPlotly({
      res <- model_results_rv()
      if (is.null(res)) return(plotly_empty())

      si <- strategy_info()
      qaly_data <- lapply(names(res$markov_results), function(s) {
        mk <- res$markov_results[[s]]
        tibble::tibble(
          year = (0:settings$n_cycles) * settings$cycle_length,
          cum_qaly = cumsum(mk$disc_cycle_qalys) / settings$n_cohort,
          strategy_name = si$name[si$id == s]
        )
      })
      qaly_df <- bind_rows(qaly_data)

      p <- ggplot(qaly_df, aes(x = year, y = cum_qaly, color = strategy_name)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = setNames(si$color, si$name)) +
        labs(x = "Years", y = "Cumulative Discounted QALYs/Person", color = NULL) +
        theme_minimal(base_size = 12)

      ggplotly(p)
    })
  })
}
