# mod_dsa.R — Module: Deterministic Sensitivity Analysis (DT-Based)

mod_dsa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(12),
      card(
        card_header("One-Way Sensitivity Analysis (Tornado Diagram)", class = "bg-primary text-white"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            pickerInput(ns("sel_owsa_params"), "Select Parameters (up to 15)",
              choices = NULL, multiple = TRUE,
              options = list(`actions-box` = TRUE, `live-search` = TRUE,
                            `selected-text-format` = "count > 3")),
            selectInput(ns("sel_owsa_ref"), "Reference Strategy",
              choices = setNames(MODEL_SETTINGS$strategies, MODEL_SETTINGS$strategy_names),
              selected = "TreatAll")
          ),
          layout_columns(
            col_widths = c(6, 6),
            selectInput(ns("sel_owsa_outcome"), "Outcome Metric",
              choices = c(
                "Expected TB (5y) per 1000" = "expected_tb_5y",
                "Screening Cost per Person" = "cost",
                "LTBI Missed per 1000" = "ltbi_missed"
              ),
              selected = "expected_tb_5y"),
            actionButton(ns("btn_run_owsa"), "Run One-Way SA",
                        class = "btn-primary mt-4", width = "100%")
          ),
          helpText("The tornado diagram shows how each parameter (varied across its plausible range) ",
                   "affects the selected outcome for the chosen strategy. Wider bars = more influential parameters."),
          conditionalPanel(
            condition = paste0("input['", ns("btn_run_owsa"), "'] > 0"),
            br(),
            plotlyOutput(ns("plot_tornado"), height = "600px")
          )
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Two-Way Sensitivity Analysis (Heatmap)"),
        card_body(
          helpText("Varies two parameters simultaneously on a grid and shows which strategy is optimal (lowest value) ",
                   "at each combination. The heatmap reveals interaction effects and regions where different strategies are preferred."),
          layout_columns(
            col_widths = c(4, 4, 4),
            selectInput(ns("sel_twsa_p1"), "Parameter 1 (X-axis)",
              choices = NULL, selected = NULL),
            selectInput(ns("sel_twsa_p2"), "Parameter 2 (Y-axis)",
              choices = NULL, selected = NULL),
            selectInput(ns("sel_twsa_outcome"), "Outcome Metric",
              choices = c(
                "Expected TB (5y) per 1000" = "expected_tb_5y",
                "Screening Cost per Person" = "cost",
                "LTBI Missed per 1000" = "ltbi_missed"
              ),
              selected = "expected_tb_5y")
          ),
          layout_columns(
            col_widths = c(6, 6),
            selectInput(ns("sel_twsa_ref"), "Strategy to Track (contour)",
              choices = setNames(MODEL_SETTINGS$strategies, MODEL_SETTINGS$strategy_names),
              selected = "CyTb"),
            actionButton(ns("btn_run_twsa"), "Run Two-Way SA",
                        class = "btn-warning mt-4", width = "100%")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("btn_run_twsa"), "'] > 0"),
            br(),
            plotlyOutput(ns("plot_heatmap"), height = "550px")
          )
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Threshold Analysis (Parameter Sweep)"),
        card_body(
          helpText("Sweeps a single parameter across its range and plots the selected outcome for all strategies. ",
                   "Shows where (if anywhere) the best strategy changes, helping identify critical parameter thresholds."),
          layout_columns(
            col_widths = c(4, 4, 4),
            selectInput(ns("sel_thresh_param"), "Parameter to Vary",
              choices = NULL, selected = NULL),
            selectInput(ns("sel_thresh_outcome"), "Outcome Metric",
              choices = c(
                "Expected TB (5y) per 1000" = "expected_tb_5y",
                "Screening Cost per Person" = "cost",
                "LTBI Missed per 1000" = "ltbi_missed"
              ),
              selected = "expected_tb_5y"),
            actionButton(ns("btn_run_thresh"), "Run Threshold Analysis",
                        class = "btn-success mt-4", width = "100%")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("btn_run_thresh"), "'] > 0"),
            br(),
            plotlyOutput(ns("plot_threshold"), height = "450px"),
            br(),
            verbatimTextOutput(ns("thresh_result"))
          )
        )
      )
    )
  )
}

mod_dsa_server <- function(id, params_rv, settings) {
  moduleServer(id, function(input, output, session) {

    # Key parameters for DSA (DT-relevant only, no Markov params)
    key_params <- c(
      "p_LTBI_RA_combined", "Se_TST_5mm_IS", "Sp_TST_5mm_BCG",
      "Se_IGRA_rheum", "Sp_IGRA_rheum", "Se_CyTb", "Sp_CyTb_BCG",
      "RR_adalimumab", "r_LTBI_react_natural", "OR_INH_alone",
      "p_hepatox_INH_TNFi", "c_TST", "c_IGRA", "c_CyTb", "c_INH_6mo",
      "c_LFT_monitoring", "c_hepatox_mild", "c_hepatox_severe",
      "c_CXR", "c_TB_treat_total"
    )

    observe({
      p <- params_rv()
      available <- intersect(key_params, names(p))
      updatePickerInput(session, "sel_owsa_params", choices = available,
                       selected = available[1:min(10, length(available))])
      updateSelectInput(session, "sel_twsa_p1", choices = available,
                       selected = "p_LTBI_RA_combined")
      updateSelectInput(session, "sel_twsa_p2", choices = available,
                       selected = "Se_CyTb")
      updateSelectInput(session, "sel_thresh_param", choices = available,
                       selected = "p_LTBI_RA_combined")
    })

    # ── Column name mapping: outcome selector → model summary column ──
    col_for_outcome <- function(outcome) {
      map <- c(expected_tb_5y = "expected_tb_5y", cost = "cost_per_person",
               ltbi_missed = "ltbi_missed")
      if (outcome %in% names(map)) map[[outcome]] else outcome
    }

    outcome_label <- function(outcome) {
      labs <- c(expected_tb_5y = "Expected TB Cases/1000 (5y)",
                cost = "Screening Cost/Person (\u20B9)",
                ltbi_missed = "LTBI Missed/1000")
      if (outcome %in% names(labs)) labs[[outcome]] else outcome
    }

    # ═══════════════════════════════════════════════════════════
    # ONE-WAY SA (TORNADO)
    # ═══════════════════════════════════════════════════════════
    owsa_results <- eventReactive(input$btn_run_owsa, {
      withProgress(message = "Running one-way sensitivity analysis...", {
        run_owsa(params_rv(), input$sel_owsa_params, settings, n_steps = 8)
      })
    })

    output$plot_tornado <- renderPlotly({
      res <- owsa_results()
      if (is.null(res) || nrow(res) == 0) return(plotly_empty())

      outcome_var <- input$sel_owsa_outcome
      td <- tornado_data(res, input$sel_owsa_ref, outcome = outcome_var)
      if (nrow(td) == 0) return(plotly_empty())

      td <- td %>% head(15)

      # Base case value — use the correct column name from summary
      p_base <- get_param_values(params_rv())
      base_model <- run_dt_only_model(p_base, settings)
      ref_idx <- which(base_model$summary$strategy == input$sel_owsa_ref)
      sum_col <- col_for_outcome(outcome_var)
      base_val <- base_model$summary[[sum_col]][ref_idx]

      td <- td %>%
        mutate(
          low_dev = value_at_low - base_val,
          high_dev = value_at_high - base_val,
          param_label = factor(param_name, levels = rev(param_name))
        )

      p <- ggplot(td) +
        geom_segment(aes(x = low_dev, xend = high_dev, y = param_label, yend = param_label,
                        text = paste0(param_name, "\nLow: ", round(value_at_low, 2),
                                     "\nHigh: ", round(value_at_high, 2),
                                     "\nRange: [", round(param_low, 4), " \u2013 ", round(param_high, 4), "]")),
                     linewidth = 8, color = "#4472C4", alpha = 0.7) +
        geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
        labs(x = paste0("Change in ", outcome_label(outcome_var),
                        " vs Base Case (", input$sel_owsa_ref, ": ",
                        round(base_val, 1), ")"),
             y = NULL,
             title = paste0("Tornado Diagram \u2014 ", outcome_label(outcome_var))) +
        theme_minimal(base_size = 11)

      ggplotly(p, tooltip = "text")
    })

    # ═══════════════════════════════════════════════════════════
    # TWO-WAY SA (HEATMAP)
    # ═══════════════════════════════════════════════════════════
    twsa_results <- eventReactive(input$btn_run_twsa, {
      req(input$sel_twsa_p1 != input$sel_twsa_p2)
      withProgress(message = "Running two-way sensitivity analysis...", {
        run_twsa(params_rv(), input$sel_twsa_p1, input$sel_twsa_p2,
                 input$sel_twsa_ref,
                 outcome = input$sel_twsa_outcome,
                 settings = settings, n_steps = 8)
      })
    })

    output$plot_heatmap <- renderPlotly({
      grid <- twsa_results()
      if (is.null(grid) || nrow(grid) == 0) return(plotly_empty())

      out_var <- grid$outcome[1]
      out_lab <- outcome_label(out_var)

      p <- ggplot(grid, aes(x = v1, y = v2, fill = optimal,
                             text = paste0(grid$param1_name[1], ": ", round(v1, 4),
                                          "\n", grid$param2_name[1], ": ", round(v2, 4),
                                          "\nBest Strategy: ", optimal,
                                          "\n", input$sel_twsa_ref, " value: ", round(ref_value, 1)))) +
        geom_tile(alpha = 0.85) +
        scale_fill_brewer(palette = "Set2") +
        labs(x = grid$param1_name[1], y = grid$param2_name[1],
             fill = paste0("Best Strategy\n(lowest ", out_lab, ")"),
             title = paste0("Two-Way SA: Strategy with Lowest ", out_lab)) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom")

      ggplotly(p, tooltip = "text")
    })

    # ═══════════════════════════════════════════════════════════
    # THRESHOLD ANALYSIS (LINE PLOT)
    # ═══════════════════════════════════════════════════════════
    thresh_result <- eventReactive(input$btn_run_thresh, {
      withProgress(message = "Running threshold analysis...", {
        find_threshold(params_rv(), input$sel_thresh_param,
                       outcome = input$sel_thresh_outcome,
                       settings = settings, n_points = 30)
      })
    })

    output$plot_threshold <- renderPlotly({
      res <- thresh_result()
      if (is.null(res) || is.null(res$sweep) || nrow(res$sweep) == 0) return(plotly_empty())

      sweep <- res$sweep

      # Strategy colours
      strat_colors <- c("TST Alone" = "#E41A1C", "IGRA Alone" = "#377EB8",
                         "Cy-Tb Alone" = "#4DAF4A", "TST \u2192 IGRA" = "#984EA3",
                         "Treat All" = "#FF7F00")

      p <- ggplot(sweep, aes(x = param_value, y = outcome_value,
                              color = strategy_name, group = strategy_name,
                              text = paste0(strategy_name,
                                           "\n", res$param_name, ": ", round(param_value, 4),
                                           "\n", res$outcome_label, ": ", round(outcome_value, 2)))) +
        geom_line(linewidth = 1.2) +
        scale_color_manual(values = strat_colors, name = "Strategy") +
        geom_vline(xintercept = res$base_value, linetype = "dashed", color = "grey40") +
        annotate("text", x = res$base_value, y = max(sweep$outcome_value, na.rm = TRUE),
                 label = "Base case", hjust = -0.1, vjust = 1, size = 3.5, color = "grey40") +
        labs(x = res$param_name,
             y = res$outcome_label,
             title = paste0("Threshold Analysis: ", res$param_name, " vs ", res$outcome_label)) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom")

      # Add threshold vertical lines
      for (th in res$thresholds) {
        p <- p + geom_vline(xintercept = th$value, linetype = "dotted",
                            color = "red", linewidth = 0.8)
      }

      ggplotly(p, tooltip = "text")
    })

    output$thresh_result <- renderText({
      res <- thresh_result()
      if (is.null(res)) return("")
      res$message
    })
  })
}
