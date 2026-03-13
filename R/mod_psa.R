# mod_psa.R — Module 7: Probabilistic Sensitivity Analysis

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
          layout_columns(
            col_widths = c(6, 6),
            numericInput(ns("psa_wtp"), "WTP (INR/QALY)", value = 234859,
                        min = 0, max = 2000000, step = 50000),
            actionButton(ns("btn_run_psa"), "Run PSA",
                        class = "btn-danger btn-lg mt-3", width = "100%",
                        icon = icon("play"))
          )
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("PSA Summary Statistics"),
        card_body(DT::dataTableOutput(ns("psa_summary_table")))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Cost-Effectiveness Scatter Plot"),
        card_body(plotlyOutput(ns("plot_ce_scatter"), height = "550px"))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Cost-Effectiveness Acceptability Curve (CEAC)"),
        card_body(plotlyOutput(ns("plot_ceac"), height = "550px"))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Expected Value of Perfect Information (EVPI)"),
        card_body(
          plotlyOutput(ns("plot_evpi"), height = "550px"),
          br(),
          uiOutput(ns("evpi_text"))
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("PSA Interpretation"),
        card_body(uiOutput(ns("psa_interpretation")))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            downloadButton(ns("dl_psa_xlsx"), "PSA Data (.xlsx)",
                          class = "btn-outline-danger btn-sm w-100",
                          icon = icon("download")),
            downloadButton(ns("dl_scatter_png"), "Scatter (.png)",
                          class = "btn-outline-danger btn-sm w-100",
                          icon = icon("download")),
            downloadButton(ns("dl_ceac_png"), "CEAC (.png)",
                          class = "btn-outline-danger btn-sm w-100",
                          icon = icon("download")),
            downloadButton(ns("dl_evpi_png"), "EVPI (.png)",
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

    output$psa_summary_table <- DT::renderDataTable({
      psa <- psa_results()
      if (is.null(psa)) return(datatable(data.frame()))

      summ <- psa_summary(psa, input$psa_wtp)

      display <- summ %>%
        transmute(
          Strategy = strategy_name,
          `Mean Cost` = fmt_inr(mean_cost),
          `95% CI Cost` = paste0("[", fmt_inr(ci_cost_low), ", ", fmt_inr(ci_cost_high), "]"),
          `Mean QALY` = round(mean_qaly, 4),
          `95% CI QALY` = paste0("[", round(ci_qaly_low, 4), ", ", round(ci_qaly_high, 4), "]"),
          `Mean NMB` = fmt_inr(mean_nmb),
          `P(CE)` = fmt_pct(prob_ce)
        )

      datatable(display, options = list(dom = "t", pageLength = 5), rownames = FALSE) %>%
        formatStyle("P(CE)", fontWeight = "bold",
          backgroundColor = styleInterval(c(0.5), c("white", "#E0FFE0")))
    })

    output$plot_ce_scatter <- renderPlotly({
      psa <- psa_results()
      if (is.null(psa)) return(plotly_empty())

      si <- strategy_info()
      n_strat <- ncol(psa$costs)

      # Reference = first strategy (TST)
      ref_cost <- psa$costs[, 1]
      ref_qaly <- psa$qalys[, 1]

      scatter_data <- list()
      for (j in 2:n_strat) {
        scatter_data[[j-1]] <- tibble::tibble(
          inc_cost = psa$costs[, j] - ref_cost,
          inc_qaly = psa$qalys[, j] - ref_qaly,
          strategy = psa$strategy_names[j]
        )
      }
      scatter_df <- bind_rows(scatter_data)

      # Subsample for performance
      if (nrow(scatter_df) > 5000) {
        scatter_df <- scatter_df %>% sample_n(5000)
      }

      wtp <- input$psa_wtp

      p <- ggplot(scatter_df, aes(x = inc_qaly, y = inc_cost, color = strategy)) +
        geom_point(alpha = 0.15, size = 1) +
        stat_ellipse(level = 0.95, linewidth = 1) +
        geom_abline(slope = wtp, intercept = 0, linetype = "dashed", color = "gray40") +
        geom_hline(yintercept = 0, color = "gray80") +
        geom_vline(xintercept = 0, color = "gray80") +
        scale_color_manual(values = setNames(si$color[2:n_strat], psa$strategy_names[2:n_strat])) +
        scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
        labs(x = paste0("Incremental QALYs (vs ", psa$strategy_names[1], ")"),
             y = "Incremental Cost (INR)", color = "Strategy") +
        theme_minimal(base_size = 12)

      ggplotly(p, tooltip = c("x", "y", "color"))
    })

    output$plot_ceac <- renderPlotly({
      psa <- psa_results()
      if (is.null(psa)) return(plotly_empty())

      si <- strategy_info()
      ceac <- calculate_ceac(psa, wtp_range = seq(0, 1000000, by = 10000))

      p <- ggplot(ceac, aes(x = wtp, y = prob_ce, color = strategy_name)) +
        geom_line(linewidth = 1) +
        geom_vline(xintercept = 234859, linetype = "dashed", color = "gray50") +
        annotate("text", x = 250000, y = 0.05, label = "1\u00D7 GDP p.c.",
                 color = "gray50", size = 3, hjust = 0) +
        geom_vline(xintercept = 704577, linetype = "dotted", color = "gray60") +
        annotate("text", x = 720000, y = 0.05, label = "3\u00D7 GDP p.c.",
                 color = "gray60", size = 3, hjust = 0) +
        scale_color_manual(values = setNames(si$color, si$name)) +
        scale_x_continuous(labels = function(x) paste0("\u20B9", scales::comma(x / 1000), "k")) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        labs(x = "Willingness-to-Pay (INR/QALY)",
             y = "Probability of Being Cost-Effective",
             color = "Strategy") +
        theme_minimal(base_size = 12)

      ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
    })

    output$plot_evpi <- renderPlotly({
      psa <- psa_results()
      if (is.null(psa)) return(plotly_empty())

      wtp_range <- seq(0, 1000000, by = 20000)
      evpi_vals <- sapply(wtp_range, function(w) calculate_evpi(psa, w))

      evpi_df <- tibble::tibble(wtp = wtp_range, evpi = evpi_vals)

      p <- ggplot(evpi_df, aes(x = wtp, y = evpi)) +
        geom_line(color = "#FF4444", linewidth = 1.2) +
        geom_area(fill = "#FF4444", alpha = 0.1) +
        geom_vline(xintercept = 234859, linetype = "dashed", color = "gray50") +
        scale_x_continuous(labels = function(x) paste0("\u20B9", scales::comma(x / 1000), "k")) +
        scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
        labs(x = "WTP (INR/QALY)", y = "EVPI (INR/Person)") +
        theme_minimal(base_size = 12)

      ggplotly(p)
    })

    output$evpi_text <- renderUI({
      psa <- psa_results()
      if (is.null(psa)) return(NULL)

      evpi <- calculate_evpi(psa, input$psa_wtp)
      tagList(
        tags$p(
          "At WTP = ", fmt_inr(input$psa_wtp), "/QALY, the ",
          tags$strong("EVPI = ", fmt_inr(evpi), " per person"), "."
        ),
        tags$p(class = "text-muted small",
          "EVPI represents the maximum value of eliminating all parameter uncertainty.
           A high EVPI suggests further research to reduce uncertainty would be worthwhile.")
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
        tmp <- generate_psa_excel(psa, input$psa_wtp)
        file.copy(tmp, file)
      }
    )

    output$dl_scatter_png <- downloadHandler(
      filename = function() "PSA_CE_Scatter.png",
      content = function(file) {
        psa <- psa_results()
        if (is.null(psa)) return()
        p <- build_psa_scatter_plot(psa, input$psa_wtp)
        if (!is.null(p)) ggsave(file, plot = p, width = 10, height = 7, dpi = 200)
      }
    )

    output$dl_ceac_png <- downloadHandler(
      filename = function() "CEAC.png",
      content = function(file) {
        psa <- psa_results()
        if (is.null(psa)) return()
        p <- build_ceac_plot(psa)
        if (!is.null(p)) ggsave(file, plot = p, width = 10, height = 7, dpi = 200)
      }
    )

    output$dl_evpi_png <- downloadHandler(
      filename = function() "EVPI.png",
      content = function(file) {
        psa <- psa_results()
        if (is.null(psa)) return()

        wtp_range <- seq(0, 1000000, by = 20000)
        evpi_vals <- sapply(wtp_range, function(w) calculate_evpi(psa, w))
        evpi_df <- tibble::tibble(wtp = wtp_range, evpi = evpi_vals)

        p <- ggplot(evpi_df, aes(x = wtp, y = evpi)) +
          geom_line(color = "#FF4444", linewidth = 1.2) +
          geom_area(fill = "#FF4444", alpha = 0.1) +
          geom_vline(xintercept = 234859, linetype = "dashed", color = "gray50") +
          scale_x_continuous(labels = function(x) paste0("\u20B9", scales::comma(x / 1000), "k")) +
          scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
          labs(x = "WTP (INR/QALY)", y = "EVPI (INR/Person)",
               title = "Expected Value of Perfect Information") +
          theme_minimal(base_size = 12)
        ggsave(file, plot = p, width = 10, height = 7, dpi = 200)
      }
    )

    output$psa_interpretation <- renderUI({
      psa <- psa_results()
      if (is.null(psa)) return(tags$p("Run PSA to see interpretation."))

      summ <- psa_summary(psa, input$psa_wtp)
      best <- summ %>% slice_max(prob_ce, n = 1)

      tagList(
        tags$h5("PSA Results Summary"),
        tags$p(paste0("Based on ", psa$n_valid, " valid simulations (of ",
                     psa$n_total, " attempted):")),
        tags$ul(
          tags$li(paste0("At WTP = ", fmt_inr(input$psa_wtp), "/QALY, ",
                        tags$strong(best$strategy_name[1]),
                        " has the highest probability of being cost-effective (",
                        fmt_pct(best$prob_ce[1]), ").")),
          tags$li(paste0("Mean costs range from ",
                        fmt_inr(min(summ$mean_cost)), " to ",
                        fmt_inr(max(summ$mean_cost)), " per person.")),
          tags$li(paste0("Mean QALYs range from ",
                        round(min(summ$mean_qaly), 3), " to ",
                        round(max(summ$mean_qaly), 3), " per person."))
        ),
        tags$h6("How to Interpret:"),
        tags$ul(
          tags$li(tags$strong("CE Scatter Plot: "),
                 "Each dot represents one simulation. Points below the WTP line
                  are cost-effective. The 95% ellipse shows parameter uncertainty."),
          tags$li(tags$strong("CEAC: "),
                 "Shows the probability each strategy is optimal across different
                  WTP thresholds. The strategy with the highest probability at your
                  chosen WTP is the recommended choice under uncertainty."),
          tags$li(tags$strong("EVPI: "),
                 "If EVPI is high, it means more research to reduce parameter
                  uncertainty could change the decision and would be valuable.")
        )
      )
    })
  })
}
