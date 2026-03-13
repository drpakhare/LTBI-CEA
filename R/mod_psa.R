# mod_psa.R — Module: Probabilistic Sensitivity Analysis (DT-Based)

mod_psa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(12),
      card(
        card_header("PSA Configuration", class = "bg-primary text-white"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            sliderInput(ns("n_sim"), "Number of Simulations",
                       min = 100, max = 10000, value = 1000, step = 100),
            numericInput(ns("psa_seed"), "Random Seed", value = 42,
                        min = 1, max = 99999)
          ),
          actionButton(ns("btn_run_psa"), "Run PSA",
                      class = "btn-danger btn-lg mt-3", width = "100%",
                      icon = icon("play"))
        )
      )
    ),

    # PSA Summary Table
    layout_columns(
      col_widths = c(12),
      card(
        card_header("PSA Summary Statistics"),
        card_body(DT::dataTableOutput(ns("psa_summary_table")))
      )
    ),

    # Cost vs Expected TB Scatter
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Cost vs Expected TB Burden (PSA Scatter)"),
        card_body(plotlyOutput(ns("plot_cost_tb_scatter"), height = "550px"))
      )
    ),

    # Probability of Lowest TB / Cheapest
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Probability of Lowest TB Burden"),
        card_body(plotlyOutput(ns("plot_prob_lowest_tb"), height = "400px"))
      ),
      card(
        card_header("Probability of Lowest Cost"),
        card_body(plotlyOutput(ns("plot_prob_cheapest"), height = "400px"))
      )
    ),

    # TB Distribution Density
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Expected TB (5y) Distribution by Strategy"),
        card_body(plotlyOutput(ns("plot_tb_density"), height = "500px"))
      )
    ),

    # Interpretation
    layout_columns(
      col_widths = c(12),
      card(
        card_header("PSA Interpretation"),
        card_body(uiOutput(ns("psa_interpretation")))
      )
    ),

    # Downloads
    layout_columns(
      col_widths = c(12),
      card(
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            downloadButton(ns("dl_psa_xlsx"), "PSA Data (.xlsx)",
                          class = "btn-outline-danger btn-sm w-100",
                          icon = icon("download")),
            downloadButton(ns("dl_scatter_png"), "Scatter (.png)",
                          class = "btn-outline-danger btn-sm w-100",
                          icon = icon("download")),
            downloadButton(ns("dl_density_png"), "TB Density (.png)",
                          class = "btn-outline-danger btn-sm w-100",
                          icon = icon("download"))
          )
        )
      )
    )
  )
}

mod_psa_server <- function(id, params_rv, settings, psa_results_shared = NULL) {
  moduleServer(id, function(input, output, session) {

    psa_results <- eventReactive(input$btn_run_psa, {
      set.seed(input$psa_seed)
      res <- withProgress(message = "Running probabilistic sensitivity analysis...", value = 0, {
        run_psa(params_rv(), input$n_sim, settings,
               progress_fn = function(i, n) {
                 if (i %% max(1, n %/% 20) == 0) {
                   setProgress(i / n, detail = paste0("Iteration ", i, " / ", n))
                 }
               })
      })
      # Share PSA results with the report module
      if (!is.null(psa_results_shared)) {
        psa_results_shared(res)
      }
      res
    })

    # PSA Summary Table
    output$psa_summary_table <- DT::renderDataTable({
      psa <- psa_results()
      if (is.null(psa)) return(datatable(data.frame()))

      summ <- psa_summary(psa)

      display <- summ %>%
        transmute(
          Strategy = strategy_name,
          `Mean Cost/Person` = fmt_inr(mean_cost),
          `95% CI Cost` = paste0("[", fmt_inr(ci_cost_low), ", ", fmt_inr(ci_cost_high), "]"),
          `Mean TB (5y)/1000` = round(mean_tb_5y, 2),
          `95% CI TB` = paste0("[", round(ci_tb_low, 2), ", ", round(ci_tb_high, 2), "]"),
          `Mean TB Averted/1000` = round(mean_tb_averted, 2),
          `Mean LTBI Missed/1000` = round(mean_ltbi_missed, 1),
          `P(Lowest TB)` = fmt_pct(prob_lowest_tb),
          `P(Cheapest)` = fmt_pct(prob_cheapest)
        )

      datatable(display, options = list(dom = "t", pageLength = 5), rownames = FALSE) %>%
        formatStyle("P(Lowest TB)", fontWeight = "bold",
          backgroundColor = styleInterval(c(0.5), c("white", "#E0FFE0")))
    })

    # Cost vs Expected TB Scatter
    output$plot_cost_tb_scatter <- renderPlotly({
      psa <- psa_results()
      if (is.null(psa)) return(plotly_empty())

      si <- strategy_info()
      n_strat <- ncol(psa$costs)

      scatter_data <- list()
      for (j in 1:n_strat) {
        df_j <- tibble::tibble(
          cost = psa$costs[, j],
          tb_5y = psa$tb_5y[, j],
          strategy = psa$strategy_names[j]
        )
        # Subsample for performance
        if (nrow(df_j) > 1000) {
          df_j <- df_j %>% sample_n(1000)
        }
        scatter_data[[j]] <- df_j
      }
      scatter_df <- bind_rows(scatter_data)

      p <- ggplot(scatter_df, aes(x = tb_5y, y = cost, color = strategy)) +
        geom_point(alpha = 0.15, size = 1) +
        stat_ellipse(level = 0.95, linewidth = 1) +
        scale_color_manual(values = setNames(si$color, si$name)) +
        scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
        labs(x = "Expected TB Cases per 1000 (5-Year)",
             y = "Screening Cost per Person (INR)",
             color = "Strategy",
             title = "Cost vs TB Burden Under Parameter Uncertainty") +
        theme_minimal(base_size = 12)

      ggplotly(p, tooltip = c("x", "y", "color"))
    })

    # Probability of Lowest TB
    output$plot_prob_lowest_tb <- renderPlotly({
      psa <- psa_results()
      if (is.null(psa)) return(plotly_empty())

      si <- strategy_info()
      summ <- psa_summary(psa)

      p <- ggplot(summ, aes(x = reorder(strategy_name, -prob_lowest_tb),
                            y = prob_lowest_tb, fill = strategy_name)) +
        geom_col(alpha = 0.85) +
        scale_fill_manual(values = setNames(si$color, si$name)) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        labs(x = NULL, y = "Probability", title = "P(Lowest 5y TB Burden)") +
        theme_minimal(base_size = 11) +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 30, hjust = 1))

      ggplotly(p, tooltip = c("y"))
    })

    # Probability of Cheapest
    output$plot_prob_cheapest <- renderPlotly({
      psa <- psa_results()
      if (is.null(psa)) return(plotly_empty())

      si <- strategy_info()
      summ <- psa_summary(psa)

      p <- ggplot(summ, aes(x = reorder(strategy_name, -prob_cheapest),
                            y = prob_cheapest, fill = strategy_name)) +
        geom_col(alpha = 0.85) +
        scale_fill_manual(values = setNames(si$color, si$name)) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        labs(x = NULL, y = "Probability", title = "P(Lowest Cost)") +
        theme_minimal(base_size = 11) +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 30, hjust = 1))

      ggplotly(p, tooltip = c("y"))
    })

    # TB Distribution Density
    output$plot_tb_density <- renderPlotly({
      psa <- psa_results()
      if (is.null(psa)) return(plotly_empty())

      si <- strategy_info()
      n_strat <- ncol(psa$tb_5y)

      density_data <- list()
      for (j in 1:n_strat) {
        density_data[[j]] <- tibble::tibble(
          tb_5y = psa$tb_5y[, j],
          strategy = psa$strategy_names[j]
        )
      }
      density_df <- bind_rows(density_data)

      p <- ggplot(density_df, aes(x = tb_5y, fill = strategy, color = strategy)) +
        geom_density(alpha = 0.3, linewidth = 0.8) +
        scale_fill_manual(values = setNames(si$color, si$name)) +
        scale_color_manual(values = setNames(si$color, si$name)) +
        labs(x = "Expected TB Cases per 1000 (5-Year)",
             y = "Density",
             fill = "Strategy", color = "Strategy",
             title = "Distribution of Expected TB Burden Across Simulations") +
        theme_minimal(base_size = 12)

      ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
    })

    # Interpretation
    output$psa_interpretation <- renderUI({
      psa <- psa_results()
      if (is.null(psa)) return(tags$p("Run PSA to see interpretation."))

      summ <- psa_summary(psa)
      best_tb <- summ %>% slice_max(prob_lowest_tb, n = 1)
      best_cost <- summ %>% slice_max(prob_cheapest, n = 1)

      tagList(
        tags$h5("PSA Results Summary"),
        tags$p(paste0("Based on ", psa$n_valid, " valid simulations (of ",
                     psa$n_total, " attempted):")),
        tags$ul(
          tags$li(HTML(paste0(
            tags$strong(best_tb$strategy_name[1]),
            " has the highest probability of achieving the lowest 5-year TB burden (",
            fmt_pct(best_tb$prob_lowest_tb[1]), ")."))),
          tags$li(HTML(paste0(
            tags$strong(best_cost$strategy_name[1]),
            " has the highest probability of being the cheapest strategy (",
            fmt_pct(best_cost$prob_cheapest[1]), ")."))),
          tags$li(paste0("Mean screening costs range from ",
                        fmt_inr(min(summ$mean_cost)), " to ",
                        fmt_inr(max(summ$mean_cost)), " per person.")),
          tags$li(paste0("Mean expected TB (5y) ranges from ",
                        round(min(summ$mean_tb_5y), 2), " to ",
                        round(max(summ$mean_tb_5y), 2), " per 1000 patients."))
        ),
        tags$h6("How to Interpret:"),
        tags$ul(
          tags$li(tags$strong("Cost vs TB Scatter: "),
                 "Each dot = one simulation. Strategies in the bottom-left corner ",
                 "(low cost, low TB) are preferred. The 95% ellipse captures uncertainty."),
          tags$li(tags$strong("P(Lowest TB): "),
                 "Probability each strategy yields the fewest TB cases across simulations. ",
                 "Higher = more robust TB prevention under uncertainty."),
          tags$li(tags$strong("P(Cheapest): "),
                 "Probability each strategy is least costly. Trade-off with TB prevention ",
                 "informs the final recommendation."),
          tags$li(tags$strong("TB Density: "),
                 "Narrower distributions indicate less sensitivity to parameter uncertainty. ",
                 "Strategies with distributions shifted left have lower TB burden.")
        )
      )
    })

    # ── Download handlers ──
    output$dl_psa_xlsx <- downloadHandler(
      filename = function() paste0("PSA_Results_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        psa <- psa_results()
        if (is.null(psa)) {
          showNotification("Run PSA first.", type = "warning")
          return()
        }
        tmp <- generate_psa_excel(psa)
        file.copy(tmp, file)
      }
    )

    output$dl_scatter_png <- downloadHandler(
      filename = function() "PSA_Cost_TB_Scatter.png",
      content = function(file) {
        psa <- psa_results()
        if (is.null(psa)) return()
        p <- build_psa_scatter_plot(psa)
        if (!is.null(p)) ggsave(file, plot = p, width = 10, height = 7, dpi = 200)
      }
    )

    output$dl_density_png <- downloadHandler(
      filename = function() "PSA_TB_Density.png",
      content = function(file) {
        psa <- psa_results()
        if (is.null(psa)) return()
        p <- build_psa_tb_density_plot(psa)
        if (!is.null(p)) ggsave(file, plot = p, width = 10, height = 7, dpi = 200)
      }
    )
  })
}
