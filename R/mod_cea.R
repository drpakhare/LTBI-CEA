# mod_cea.R — Module 5: Cost-Effectiveness Analysis Results

mod_cea_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Incremental Cost-Effectiveness Analysis", class = "bg-primary text-white"),
        card_body(
          DT::dataTableOutput(ns("icer_table")),
          br(),
          div(class = "small text-muted",
            "Strategies sorted by cost. Dominated = higher cost & lower QALYs vs another strategy.
             ICER = Incremental Cost / Incremental QALY vs next less costly non-dominated strategy.")
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Cost-Effectiveness Plane"),
        card_body(
          sliderInput(ns("sl_wtp"), "WTP Threshold (INR/QALY)",
                     min = 0, max = 1000000, value = 234859, step = 10000,
                     pre = "\u20B9"),
          plotlyOutput(ns("plot_ceplane"), height = "550px")
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Net Monetary Benefit"),
        card_body(plotlyOutput(ns("plot_nmb"), height = "550px"))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Interpretation & Decision Guidance", class = "bg-success text-white"),
        card_body(
          uiOutput(ns("interpretation_panel"))
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            downloadButton(ns("dl_icer_csv"), "ICER Table (.csv)",
                          class = "btn-outline-primary btn-sm w-100",
                          icon = icon("download")),
            downloadButton(ns("dl_ceplane_png"), "CE Plane (.png)",
                          class = "btn-outline-primary btn-sm w-100",
                          icon = icon("download")),
            downloadButton(ns("dl_nmb_png"), "NMB Chart (.png)",
                          class = "btn-outline-primary btn-sm w-100",
                          icon = icon("download"))
          )
        )
      )
    )
  )
}

mod_cea_server <- function(id, model_results_rv, settings) {
  moduleServer(id, function(input, output, session) {

    icer_df <- reactive({
      res <- model_results_rv()
      if (is.null(res)) return(NULL)
      calculate_icer(res$summary)
    })

    output$icer_table <- DT::renderDataTable({
      df <- icer_df()
      if (is.null(df)) return(datatable(data.frame()))

      display <- df %>%
        transmute(
          Strategy = strategy_name,
          `Total Cost/Person` = fmt_inr(cost_total_per_person),
          `Total QALYs/Person` = round(qaly_per_person, 4),
          `TB Cases/1000` = round(tb_cases_per_1000, 2),
          `Inc. Cost` = ifelse(is.na(inc_cost), "-", fmt_inr(inc_cost)),
          `Inc. QALY` = ifelse(is.na(inc_qaly), "-", round(inc_qaly, 4)),
          ICER = fmt_icer(icer),
          Status = status
        )

      datatable(display, options = list(dom = "t", pageLength = 5), rownames = FALSE) %>%
        formatStyle("Status",
          backgroundColor = styleEqual(
            c("Dominated", "Ext. Dominated", "On Frontier", "Dominant"),
            c("#FFE0E0", "#FFF0E0", "#E0F0FF", "#E0FFE0")
          ),
          fontWeight = "bold"
        ) %>%
        formatStyle("ICER", fontWeight = "bold")
    })

    output$plot_ceplane <- renderPlotly({
      df <- icer_df()
      if (is.null(df)) return(plotly_empty())

      si <- strategy_info()
      wtp <- input$sl_wtp

      # Reference = cheapest strategy
      ref_cost <- min(df$cost_total_per_person)
      ref_qaly <- df$qaly_per_person[which.min(df$cost_total_per_person)]

      plot_df <- df %>%
        left_join(si %>% select(id, color), by = c("strategy" = "id")) %>%
        mutate(
          inc_c = cost_total_per_person - ref_cost,
          inc_q = qaly_per_person - ref_qaly
        )

      # WTP line
      q_range <- range(plot_df$inc_q)
      wtp_df <- tibble::tibble(
        x = q_range,
        y = q_range * wtp
      )

      p <- ggplot(plot_df, aes(x = inc_q, y = inc_c, color = strategy_name)) +
        geom_point(size = 4, alpha = 0.9) +
        geom_text(aes(label = strategy_name), vjust = -1, size = 3) +
        geom_line(data = wtp_df, aes(x = x, y = y), inherit.aes = FALSE,
                  linetype = "dashed", color = "gray50") +
        geom_hline(yintercept = 0, color = "gray80") +
        geom_vline(xintercept = 0, color = "gray80") +
        scale_color_manual(values = setNames(si$color, si$name)) +
        scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
        labs(x = "Incremental QALYs", y = "Incremental Cost (INR)",
             color = "Strategy") +
        annotate("text", x = max(plot_df$inc_q) * 0.8,
                 y = max(plot_df$inc_q) * 0.8 * wtp,
                 label = paste0("WTP = ", fmt_inr(wtp), "/QALY"),
                 color = "gray50", size = 3, hjust = 1) +
        theme_minimal(base_size = 12)

      ggplotly(p, tooltip = c("x", "y", "color"))
    })

    output$plot_nmb <- renderPlotly({
      df <- icer_df()
      if (is.null(df)) return(plotly_empty())

      si <- strategy_info()
      wtp_vec <- c(100000, 234859, 500000, 704577)
      wtp_labels <- c("1L", "2.35L (1x GDP)", "5L", "7L (3x GDP)")

      nmb_data <- lapply(seq_along(wtp_vec), function(j) {
        nmb <- df$qaly_per_person * wtp_vec[j] - df$cost_total_per_person
        tibble::tibble(
          strategy_name = df$strategy_name,
          wtp_label = wtp_labels[j],
          nmb = nmb
        )
      })
      nmb_df <- bind_rows(nmb_data)
      nmb_df$wtp_label <- factor(nmb_df$wtp_label, levels = wtp_labels)

      p <- ggplot(nmb_df, aes(x = strategy_name, y = nmb, fill = wtp_label)) +
        geom_col(position = "dodge") +
        scale_fill_brewer(palette = "Blues") +
        scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
        labs(x = NULL, y = "Net Monetary Benefit (INR/Person)", fill = "WTP Threshold") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

      ggplotly(p, tooltip = c("x", "fill", "y")) %>%
        layout(legend = list(orientation = "h", y = -0.2))
    })

    # ── Download handlers ──
    output$dl_icer_csv <- downloadHandler(
      filename = function() "ICER_Table.csv",
      content = function(file) {
        df <- icer_df()
        if (is.null(df)) return()
        export <- df %>%
          dplyr::transmute(
            Strategy = strategy_name,
            Cost_Per_Person = round(cost_total_per_person, 2),
            QALYs_Per_Person = round(qaly_per_person, 6),
            TB_Cases_Per_1000 = round(tb_cases_per_1000, 3),
            Incremental_Cost = round(inc_cost, 2),
            Incremental_QALY = round(inc_qaly, 6),
            ICER = round(icer, 2),
            Status = status
          )
        write.csv(export, file, row.names = FALSE)
      }
    )

    output$dl_ceplane_png <- downloadHandler(
      filename = function() "CE_Plane.png",
      content = function(file) {
        res <- model_results_rv()
        if (is.null(res)) return()
        p <- build_ceplane_plot(res, input$sl_wtp)
        ggsave(file, plot = p, width = 10, height = 7, dpi = 200)
      }
    )

    output$dl_nmb_png <- downloadHandler(
      filename = function() "NMB_Chart.png",
      content = function(file) {
        res <- model_results_rv()
        if (is.null(res)) return()
        p <- build_nmb_plot(res)
        ggsave(file, plot = p, width = 10, height = 7, dpi = 200)
      }
    )

    output$interpretation_panel <- renderUI({
      df <- icer_df()
      if (is.null(df)) return(p("Run the model to see results."))

      wtp <- input$sl_wtp
      text <- interpret_cea(df, wtp)

      # Parse markdown-like bold
      html_text <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", text)
      paragraphs <- strsplit(html_text, "\n\n")[[1]]

      tagList(
        lapply(paragraphs, function(para) tags$p(HTML(para))),
        hr(),
        tags$p(class = "text-muted small",
          "Note: This analysis uses base-case parameter estimates. Results should be
           interpreted alongside sensitivity analyses (DSA and PSA tabs) which explore
           parameter uncertainty. The Indian WTP threshold of \u20B92,34,859/QALY
           (1\u00D7 GDP per capita) is used as the primary decision threshold per
           Indian HTA guidelines.")
      )
    })
  })
}
