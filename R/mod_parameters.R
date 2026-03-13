# mod_parameters.R — Module 2: Editable Parameter Tables

mod_parameters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Key Parameter Sliders", class = "bg-primary text-white"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            sliderInput(ns("sl_prev"), "LTBI Prevalence",
                       min = 0.10, max = 0.70, value = 0.43, step = 0.01),
            sliderInput(ns("sl_se_tst"), "TST Sensitivity (immunosuppressed)",
                       min = 0.10, max = 0.90, value = 0.571, step = 0.01)
          ),
          layout_columns(
            col_widths = c(6, 6),
            sliderInput(ns("sl_se_igra"), "IGRA Sensitivity (rheumatic)",
                       min = 0.40, max = 0.95, value = 0.739, step = 0.01),
            sliderInput(ns("sl_se_cytb"), "Cy-Tb Sensitivity",
                       min = 0.50, max = 0.95, value = 0.745, step = 0.01)
          ),
          layout_columns(
            col_widths = c(6, 6),
            sliderInput(ns("sl_proph_eff"), "Prophylaxis Efficacy (OR)",
                       min = 0.20, max = 0.80, value = 0.46, step = 0.01),
            sliderInput(ns("sl_c_igra"), "IGRA Cost (INR)",
                       min = 500, max = 5000, value = 2500, step = 100)
          )
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
            "Detailed Parameter Tables",
            div(
              actionButton(ns("btn_reset"), "Reset All to Default",
                          class = "btn btn-outline-warning btn-sm me-2"),
              downloadButton(ns("btn_download"), "Export Parameters",
                            class = "btn btn-outline-success btn-sm")
            )
          )
        ),
        card_body(
          navset_pill(
            nav_panel("Prevalence",
              DT::dataTableOutput(ns("tbl_prevalence"))),
            nav_panel("Test Accuracy",
              DT::dataTableOutput(ns("tbl_test_accuracy"))),
            nav_panel("TB Reactivation",
              DT::dataTableOutput(ns("tbl_reactivation"))),
            nav_panel("Prophylaxis",
              DT::dataTableOutput(ns("tbl_prophylaxis"))),
            nav_panel("Costs (INR)",
              DT::dataTableOutput(ns("tbl_costs"))),
            nav_panel("Utilities",
              DT::dataTableOutput(ns("tbl_utilities"))),
            nav_panel("Mortality",
              DT::dataTableOutput(ns("tbl_mortality")))
          )
        )
      )
    )
  )
}

mod_parameters_server <- function(id, params_rv) {
  moduleServer(id, function(input, output, session) {

    # Update params from sliders
    observeEvent(input$sl_prev, {
      p <- params_rv()
      p[["p_LTBI_RA_combined"]]$value <- input$sl_prev
      params_rv(p)
    })
    observeEvent(input$sl_se_tst, {
      p <- params_rv()
      p[["Se_TST_5mm_IS"]]$value <- input$sl_se_tst
      params_rv(p)
    })
    observeEvent(input$sl_se_igra, {
      p <- params_rv()
      p[["Se_IGRA_rheum"]]$value <- input$sl_se_igra
      params_rv(p)
    })
    observeEvent(input$sl_se_cytb, {
      p <- params_rv()
      p[["Se_CyTb"]]$value <- input$sl_se_cytb
      params_rv(p)
    })
    observeEvent(input$sl_proph_eff, {
      p <- params_rv()
      p[["OR_INH_alone"]]$value <- input$sl_proph_eff
      params_rv(p)
    })
    observeEvent(input$sl_c_igra, {
      p <- params_rv()
      p[["c_IGRA"]]$value <- input$sl_c_igra
      params_rv(p)
    })

    # Reset
    observeEvent(input$btn_reset, {
      params_rv(params_default)
      updateSliderInput(session, "sl_prev", value = 0.43)
      updateSliderInput(session, "sl_se_tst", value = 0.571)
      updateSliderInput(session, "sl_se_igra", value = 0.739)
      updateSliderInput(session, "sl_se_cytb", value = 0.745)
      updateSliderInput(session, "sl_proph_eff", value = 0.46)
      updateSliderInput(session, "sl_c_igra", value = 2500)
    })

    # Render parameter tables
    render_param_table <- function(domain) {
      renderDT({
        p <- params_rv()
        raw <- attr(params_default, "raw_sheets")[[domain]]
        if (is.null(raw)) return(datatable(data.frame()))

        # Update current values from params_rv
        display_df <- raw
        col1 <- names(display_df)[1]
        col_base <- names(display_df)[3]
        for (i in seq_len(nrow(display_df))) {
          pname <- as.character(display_df[[col1]][i])
          if (!is.na(pname) && pname %in% names(p)) {
            display_df[[col_base]][i] <- p[[pname]]$value
          }
        }

        datatable(
          display_df,
          editable = list(target = "cell", disable = list(columns = c(0, 1, 3:7))),
          options = list(
            pageLength = 20,
            scrollX = TRUE,
            dom = "ftp",
            columnDefs = list(list(width = "200px", targets = c(1)))
          ),
          rownames = FALSE
        ) %>%
          formatRound(columns = 3, digits = 4)
      })
    }

    output$tbl_prevalence <- render_param_table("prevalence")
    output$tbl_test_accuracy <- render_param_table("test_accuracy")
    output$tbl_reactivation <- render_param_table("reactivation")
    output$tbl_prophylaxis <- render_param_table("prophylaxis")
    output$tbl_costs <- render_param_table("costs")
    output$tbl_utilities <- render_param_table("utilities")
    output$tbl_mortality <- render_param_table("mortality")

    # Download (exports current modified values)
    output$btn_download <- downloadHandler(
      filename = function() paste0("LTBI_CEA_Params_", Sys.Date(), ".xlsx"),
      content = function(file) {
        tmp <- generate_params_excel(params_rv())
        file.copy(tmp, file)
      }
    )
  })
}
