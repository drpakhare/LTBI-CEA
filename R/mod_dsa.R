# mod_dsa.R — Module 6: Deterministic Sensitivity Analysis

mod_dsa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(12),
      card(
        card_header("One-Way Sensitivity Analysis", class = "bg-primary text-white"),
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
            numericInput(ns("n_owsa_wtp"), "WTP (INR/QALY)", value = 234859,
                        min = 0, max = 2000000, step = 50000),
            actionButton(ns("btn_run_owsa"), "Run One-Way SA",
                        class = "btn-primary mt-4", width = "100%")
          ),
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
        card_header("Two-Way Sensitivity Analysis"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            selectInput(ns("sel_twsa_p1"), "Parameter 1 (X-axis)",
              choices = NULL, selected = NULL),
            selectInput(ns("sel_twsa_p2"), "Parameter 2 (Y-axis)",
              choices = NULL, selected = NULL)
          ),
          layout_columns(
            col_widths = c(6, 6),
            selectInput(ns("sel_twsa_ref"), "Strategy of Interest",
              choices = setNames(MODEL_SETTINGS$strategies, MODEL_SETTINGS$strategy_names),
              selected = "TreatAll"),
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
        card_header("Threshold Analysis"),
        card_body(
          layout_columns(
            col_widths = c(12),
            selectInput(ns("sel_thresh_param"), "Parameter to Vary",
              choices = NULL, selected = NULL)
          ),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(ns("n_thresh_wtp"), "WTP (INR/QALY)", value = 234859),
            actionButton(ns("btn_run_thresh"), "Find Threshold",
                        class = "btn-success mt-4", width = "100%")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("btn_run_thresh"), "'] > 0"),
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

    # Key parameters for DSA
    key_params <- c(
      "p_LTBI_RA_combined", "Se_TST_5mm_IS", "Sp_TST_5mm_BCG",
      "Se_IGRA_rheum", "Sp_IGRA_rheum", "Se_CyTb", "Sp_CyTb_BCG",
      "RR_adalimumab", "r_LTBI_react_natural", "OR_INH_alone",
      "p_hepatox_INH_TNFi", "c_TST", "c_IGRA", "c_CyTb", "c_INH_6mo",
      "c_TB_total", "c_adalimumab_mo", "u_RA_controlled", "u_active_PTB",
      "CFR_DS_TB", "p_treatment_success", "discount_rate"
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

    # One-way SA
    owsa_results <- eventReactive(input$btn_run_owsa, {
      withProgress(message = "Running one-way sensitivity analysis...", {
        run_owsa(params_rv(), input$sel_owsa_params, settings, n_steps = 8)
      })
    })

    output$plot_tornado <- renderPlotly({
      res <- owsa_results()
      if (is.null(res) || nrow(res) == 0) return(plotly_empty())

      td <- tornado_data(res, input$sel_owsa_ref, input$n_owsa_wtp)
      if (nrow(td) == 0) return(plotly_empty())

      td <- td %>% head(15)  # Top 15

      # Base NMB
      p_base <- get_param_values(params_rv())
      base_model <- run_full_model(p_base, settings)
      ref_idx <- which(base_model$summary$strategy == input$sel_owsa_ref)
      base_nmb <- base_model$summary$qaly_per_person[ref_idx] * input$n_owsa_wtp -
                   base_model$summary$cost_total_per_person[ref_idx]

      td <- td %>%
        mutate(
          low_dev = nmb_at_low - base_nmb,
          high_dev = nmb_at_high - base_nmb,
          param_label = factor(param_name, levels = rev(param_name))
        )

      p <- ggplot(td) +
        geom_segment(aes(x = low_dev, xend = high_dev, y = param_label, yend = param_label),
                     linewidth = 8, color = "#4472C4", alpha = 0.7) +
        geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
        scale_x_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
        labs(x = paste0("Change in NMB vs Base Case (", input$sel_owsa_ref, ")"),
             y = NULL,
             title = "Tornado Diagram — One-Way Sensitivity Analysis") +
        theme_minimal(base_size = 11)

      ggplotly(p, tooltip = c("x", "y"))
    })

    # Two-way SA
    twsa_results <- eventReactive(input$btn_run_twsa, {
      withProgress(message = "Running two-way sensitivity analysis...", {
        run_twsa(params_rv(), input$sel_twsa_p1, input$sel_twsa_p2,
                 input$sel_twsa_ref, settings$wtp_thresholds[2], settings, n_steps = 7)
      })
    })

    output$plot_heatmap <- renderPlotly({
      grid <- twsa_results()
      if (is.null(grid)) return(plotly_empty())

      p <- ggplot(grid, aes(x = v1, y = v2, fill = optimal)) +
        geom_tile(alpha = 0.8) +
        scale_fill_brewer(palette = "Set2") +
        labs(x = grid$param1_name[1], y = grid$param2_name[1],
             fill = "Optimal Strategy",
             title = "Two-Way SA: Optimal Strategy by Parameter Combination") +
        theme_minimal(base_size = 12)

      ggplotly(p, tooltip = c("x", "y", "fill"))
    })

    # Threshold analysis
    thresh_result <- eventReactive(input$btn_run_thresh, {
      withProgress(message = "Finding threshold...", {
        find_threshold(params_rv(), input$sel_thresh_param,
                      input$n_thresh_wtp, settings)
      })
    })

    output$thresh_result <- renderText({
      res <- thresh_result()
      if (is.null(res)) return("")
      res$message
    })
  })
}
