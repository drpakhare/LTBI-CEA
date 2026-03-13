# mod_mcda.R — Module: Multi-Criteria Decision Framework
# When QALY differences are marginal, the decision should be multi-criteria.

mod_mcda_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          tagList(icon("th-list", class = "me-1"), "Multi-Criteria Decision Framework"),
          class = "bg-primary text-white"
        ),
        card_body(
          p("When cost-effectiveness analysis cannot distinguish between strategies",
            "(QALY differences < 0.08 over 40 years), the decision should incorporate",
            "multiple criteria beyond ICER. Adjust the criterion weights below to reflect",
            "your setting's priorities."),
          p(class = "text-muted small",
            "Scoring: Each strategy is scored 0-100 on each criterion based on model outputs.",
            "Final score = weighted average. Adjust weights to explore how priorities",
            "affect the optimal choice.")
        )
      )
    ),

    # ── Weight sliders ──
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Criterion Weights (adjust to your priorities)"),
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            sliderInput(ns("w_tb_prevention"), "TB Prevention",
                       min = 0, max = 10, value = 8, step = 1),
            sliderInput(ns("w_cost"), "Screening Cost",
                       min = 0, max = 10, value = 3, step = 1),
            sliderInput(ns("w_unnecessary_rx"), "Minimise Unnecessary Treatment",
                       min = 0, max = 10, value = 5, step = 1)
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            sliderInput(ns("w_test_availability"), "Test Availability & Feasibility",
                       min = 0, max = 10, value = 6, step = 1),
            sliderInput(ns("w_equity"), "Equity (don't miss vulnerable patients)",
                       min = 0, max = 10, value = 7, step = 1),
            sliderInput(ns("w_public_health"), "Public Health / TB Transmission",
                       min = 0, max = 10, value = 7, step = 1)
          )
        )
      )
    ),

    # ── Scored table ──
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Multi-Criteria Scoring Matrix"),
        card_body(
          DT::dataTableOutput(ns("mcda_table")),
          br(),
          div(class = "text-muted small",
            "Scores are derived from model outputs: TB Prevention = fewer missed LTBI cases,",
            "Cost = lower screening cost, Unnecessary Rx = fewer false positives,",
            "Availability = TST widely available / IGRA needs lab / Cy-Tb limited,",
            "Equity = proportion of true LTBI detected,",
            "Public Health = fewer expected TB index cases (reduces transmission).")
        )
      )
    ),

    # ── Radar chart ──
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Weighted Score Comparison"),
        card_body(plotlyOutput(ns("plot_scores"), height = "500px"))
      )
    ),

    # ── Context-specific recommendations ──
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Context-Specific Recommendations", class = "bg-success text-white"),
        card_body(uiOutput(ns("recommendations")))
      )
    )
  )
}


mod_mcda_server <- function(id, model_results_rv, settings) {
  moduleServer(id, function(input, output, session) {

    # ── Score each strategy on each criterion (0-100) ──
    scores_df <- reactive({
      res <- model_results_rv()
      if (is.null(res)) return(NULL)

      dt_cea <- res$dt_cea
      if (is.null(dt_cea)) return(NULL)

      si <- strategy_info()

      # Raw values for scoring
      df <- dt_cea %>%
        left_join(si %>% select(id, name), by = c("strategy" = "id")) %>%
        select(strategy, strategy_name = name,
               ltbi_missed_per_1000, expected_tb_5y_per_1000,
               cost_total_dt, unnecessarily_treated_per_1000,
               ltbi_detected_per_1000, p_true_positive, p_ltbi)

      # Score functions: linear rescale to 0-100
      # Higher is better for all scores
      rescale_lower_better <- function(x) {
        if (max(x) == min(x)) return(rep(50, length(x)))
        100 * (max(x) - x) / (max(x) - min(x))
      }
      rescale_higher_better <- function(x) {
        if (max(x) == min(x)) return(rep(50, length(x)))
        100 * (x - min(x)) / (max(x) - min(x))
      }

      df <- df %>%
        mutate(
          # TB Prevention: fewer expected TB cases → higher score
          score_tb_prevention = rescale_lower_better(expected_tb_5y_per_1000),

          # Cost: lower cost → higher score
          score_cost = rescale_lower_better(cost_total_dt),

          # Unnecessary treatment: fewer FP → higher score
          score_unnecessary_rx = rescale_lower_better(unnecessarily_treated_per_1000),

          # Test availability/feasibility (expert-assigned based on test type)
          score_test_availability = case_when(
            strategy == "TST" ~ 95,       # universally available, single visit
            strategy == "Sequential" ~ 60, # TST + conditional IGRA (2 visits, needs lab)
            strategy == "CyTb" ~ 40,       # limited availability in India
            strategy == "IGRA" ~ 50,       # needs lab infrastructure
            strategy == "TreatAll" ~ 100,  # no test needed
            TRUE ~ 50
          ),

          # Equity: higher LTBI detection rate → higher score
          detection_rate = ifelse(p_ltbi > 0, p_true_positive / p_ltbi, 0),
          score_equity = rescale_higher_better(detection_rate),

          # Public health / transmission: fewer TB cases → higher score
          # Same direction as TB prevention but distinct rationale (societal)
          score_public_health = rescale_lower_better(expected_tb_5y_per_1000)
        )

      df
    })

    # ── Weighted scores ──
    weighted_df <- reactive({
      df <- scores_df()
      if (is.null(df)) return(NULL)

      # Get weights
      w <- c(
        tb_prevention = input$w_tb_prevention,
        cost = input$w_cost,
        unnecessary_rx = input$w_unnecessary_rx,
        test_availability = input$w_test_availability,
        equity = input$w_equity,
        public_health = input$w_public_health
      )
      w_total <- sum(w)
      if (w_total == 0) w_total <- 1  # avoid division by zero
      w_norm <- w / w_total

      df %>%
        mutate(
          weighted_total = score_tb_prevention * w_norm["tb_prevention"] +
                           score_cost * w_norm["cost"] +
                           score_unnecessary_rx * w_norm["unnecessary_rx"] +
                           score_test_availability * w_norm["test_availability"] +
                           score_equity * w_norm["equity"] +
                           score_public_health * w_norm["public_health"]
        ) %>%
        arrange(desc(weighted_total))
    })

    output$mcda_table <- DT::renderDataTable({
      df <- weighted_df()
      if (is.null(df)) return(datatable(data.frame()))

      display <- df %>%
        transmute(
          Strategy = strategy_name,
          `TB Prevention` = round(score_tb_prevention, 0),
          `Screening Cost` = round(score_cost, 0),
          `Avoid Unnecessary Rx` = round(score_unnecessary_rx, 0),
          `Test Availability` = round(score_test_availability, 0),
          `Equity` = round(score_equity, 0),
          `Public Health` = round(score_public_health, 0),
          `Weighted Score` = round(weighted_total, 1)
        )

      datatable(display,
        options = list(dom = "t", pageLength = 5, order = list(list(7, "desc"))),
        rownames = FALSE
      ) %>%
        formatStyle("Weighted Score",
          background = styleColorBar(c(0, 100), "#4472C4"),
          backgroundSize = "98% 80%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center",
          color = "white",
          fontWeight = "bold"
        ) %>%
        formatStyle(
          columns = c("TB Prevention", "Screening Cost", "Avoid Unnecessary Rx",
                       "Test Availability", "Equity", "Public Health"),
          backgroundColor = styleInterval(c(25, 50, 75),
            c("#FFE0E0", "#FFF5E0", "#E8F5E0", "#C0E8C0"))
        )
    })

    output$plot_scores <- renderPlotly({
      df <- weighted_df()
      if (is.null(df)) return(plotly_empty())
      si <- strategy_info()

      plot_df <- df %>%
        left_join(si %>% select(id, color), by = c("strategy" = "id")) %>%
        transmute(
          strategy_name,
          color,
          `TB Prevention` = score_tb_prevention,
          `Cost` = score_cost,
          `Avoid Unnecessary Rx` = score_unnecessary_rx,
          `Test Availability` = score_test_availability,
          `Equity` = score_equity,
          `Public Health` = score_public_health,
          `WEIGHTED TOTAL` = weighted_total
        ) %>%
        pivot_longer(-c(strategy_name, color),
                     names_to = "criterion", values_to = "score")

      plot_df$criterion <- factor(plot_df$criterion,
        levels = c("TB Prevention", "Cost", "Avoid Unnecessary Rx",
                   "Test Availability", "Equity", "Public Health", "WEIGHTED TOTAL"))

      p <- ggplot(plot_df, aes(x = criterion, y = score, fill = strategy_name)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = setNames(
          df %>% left_join(si %>% select(id, color), by = c("strategy" = "id")) %>% pull(color),
          df$strategy_name)) +
        labs(x = NULL, y = "Score (0-100)", fill = "Strategy") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 35, hjust = 1))

      ggplotly(p, tooltip = c("x", "fill", "y")) %>%
        layout(legend = list(orientation = "h", y = -0.3))
    })

    output$recommendations <- renderUI({
      df <- weighted_df()
      if (is.null(df)) return(p("Run the model to see results."))

      best <- df[1, ]
      runner_up <- if (nrow(df) > 1) df[2, ] else NULL

      tagList(
        tags$p(HTML(paste0(
          "With current weights, <strong>", best$strategy_name,
          "</strong> scores highest (", round(best$weighted_total, 1), "/100)."))),

        tags$h5("Context-Specific Guidance:"),

        tags$div(
          class = "border rounded p-3 mb-3",
          tags$p(tags$strong("A. Budget-constrained setting (district hospital)")),
          tags$p("Recommended: TST Alone or TST \u2192 IGRA"),
          tags$p(class = "text-muted small",
            "TST is universally available, low cost, no lab needed.",
            "Adding IGRA as confirmatory improves specificity (reduces unnecessary treatment).",
            "Acceptable TB burden if resources are limited.")
        ),

        tags$div(
          class = "border rounded p-3 mb-3",
          tags$p(tags$strong("B. Tertiary care center (lab infrastructure available)")),
          tags$p("Recommended: Cy-Tb Alone or IGRA Alone"),
          tags$p(class = "text-muted small",
            "Single-test high accuracy (Cy-Tb: Sp=99%, Se=74.5%).",
            "Best balance of TB prevention and avoiding unnecessary treatment.",
            "Cost difference vs cheaper strategies is clinically negligible.")
        ),

        tags$div(
          class = "border rounded p-3 mb-3",
          tags$p(tags$strong("C. Public health priority (TB elimination mandate)")),
          tags$p("Recommended: Treat All"),
          tags$p(class = "text-muted small",
            "Zero missed LTBI, lowest TB burden. Cost increment is minimal.",
            "Aligns with India's End TB Strategy and WHO guidelines for high-risk groups.",
            "No test infrastructure needed.")
        ),

        tags$div(
          class = "border rounded p-3 mb-3",
          tags$p(tags$strong("D. Patient-centred shared decision-making")),
          tags$p("Strategy depends on individual risk tolerance"),
          tags$p(class = "text-muted small",
            "Risk-averse patient: Treat All (zero chance of missed TB).",
            "Concerned about side effects: Cy-Tb (fewest false positives, excellent specificity).",
            "Cost-conscious: TST Alone (cheapest, widely available).")
        ),

        hr(),
        tags$p(class = "text-muted",
          "This framework reflects that all strategies produce nearly identical long-term",
          "health outcomes (QALY difference < 0.08 over 40 years). The decision should be",
          "driven by clinical priorities, local infrastructure, and public health policy,",
          "not by ICER values that cannot meaningfully distinguish between strategies.")
      )
    })
  })
}
