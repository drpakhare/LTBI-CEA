# mod_report.R — Module: Download Reports & Exports (DT-Based)
# LTBI Screening CEA Shiny Application

mod_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Download Reports & Exports", class = "bg-primary text-white"),
        card_body(
          tags$p(
            "Download comprehensive reports and data exports from your analysis. ",
            "All reports use your current parameter settings and model results."
          ),
          tags$p(class = "text-muted small",
            "Note: Reports with embedded plots may take a few seconds to generate. ",
            "PSA-related downloads are only available after running the PSA."
          )
        )
      )
    ),

    # ── Full CEA Report ──
    layout_columns(
      col_widths = c(12),
      card(
        card_header(icon("file-word"), " Full CEA Report (Word)", class = "bg-success text-white"),
        card_body(
          tags$p(
            "Comprehensive cost-effectiveness analysis report including: executive summary, ",
            "strategy comparison, decision tree outcomes, short-term CEA results, ",
            "interpretation, model assumptions, and embedded figures."
          ),
          layout_columns(
            col_widths = c(6, 6),
            checkboxInput(ns("include_plots"), "Include Embedded Plots", value = TRUE),
            checkboxInput(ns("include_psa_in_report"), "Include PSA Results (if available)",
                         value = TRUE)
          ),
          downloadButton(ns("dl_full_report"), "Download Full Report (.docx)",
                        class = "btn-success btn-lg", icon = icon("download"))
        )
      )
    ),

    # ── Parameter Export ──
    layout_columns(
      col_widths = c(12),
      card(
        card_header(icon("table"), " Parameter Workbook (Excel)"),
        card_body(
          tags$p(
            "Export all current model parameters as an Excel workbook. ",
            "Parameters are organized by domain (prevalence, test accuracy, costs, etc.). ",
            "Includes current values, low/high bounds, and distribution types."
          ),
          downloadButton(ns("dl_params_excel"), "Download Parameters (.xlsx)",
                        class = "btn-primary", icon = icon("download"))
        )
      )
    ),

    # ── PSA Exports ──
    layout_columns(
      col_widths = c(12),
      card(
        card_header(icon("dice"), " PSA Exports"),
        card_body(
          tags$p(
            "Export raw PSA simulation results for external analysis. ",
            "Available only after running the PSA in the Probabilistic SA tab."
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            downloadButton(ns("dl_psa_excel"), "PSA Results (.xlsx)",
                          class = "btn-outline-warning btn-sm w-100"),
            downloadButton(ns("dl_psa_scatter_png"), "Cost vs TB Scatter",
                          class = "btn-outline-warning btn-sm w-100"),
            downloadButton(ns("dl_tb_density_png"), "TB Density Plot",
                          class = "btn-outline-warning btn-sm w-100")
          )
        )
      )
    ),

    # ── Quick CSV Exports ──
    layout_columns(
      col_widths = c(12),
      card(
        card_header(icon("file-csv"), " Quick Summary Exports"),
        card_body(
          tags$p("Download summary tables as CSV for easy import into other tools."),
          layout_columns(
            col_widths = c(6, 6),
            downloadButton(ns("dl_dt_csv"), "Decision Tree (.csv)",
                          class = "btn-outline-secondary btn-sm w-100"),
            downloadButton(ns("dl_summary_csv"), "CEA Summary (.csv)",
                          class = "btn-outline-secondary btn-sm w-100")
          )
        )
      )
    )
  )
}


mod_report_server <- function(id, params_rv, model_results_rv, psa_results_rv, settings) {
  moduleServer(id, function(input, output, session) {

    # ══════════════════════════════════════════════════════════════════
    # Full CEA Report (Word)
    # ══════════════════════════════════════════════════════════════════
    output$dl_full_report <- downloadHandler(
      filename = function() {
        paste0("LTBI_CEA_Report_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        withProgress(message = "Generating report...", value = 0.2, {
          res <- model_results_rv()
          if (is.null(res)) {
            showNotification("No model results available. Please check parameters.", type = "error")
            return()
          }

          psa_res <- NULL
          if (input$include_psa_in_report) {
            psa_res <- tryCatch(psa_results_rv(), error = function(e) NULL)
          }

          setProgress(0.5, detail = "Building document...")

          if (input$include_plots) {
            tmp <- generate_full_report_with_plots(
              res, params_rv(), settings,
              psa_results = psa_res
            )
          } else {
            tmp <- generate_cea_report(
              res, params_rv(), settings,
              psa_results = psa_res
            )
          }

          setProgress(0.9, detail = "Saving...")
          file.copy(tmp, file)
        })
      }
    )

    # ══════════════════════════════════════════════════════════════════
    # Parameter Excel Export
    # ══════════════════════════════════════════════════════════════════
    output$dl_params_excel <- downloadHandler(
      filename = function() {
        paste0("LTBI_Parameters_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        tmp <- generate_params_excel(params_rv())
        file.copy(tmp, file)
      }
    )

    # ══════════════════════════════════════════════════════════════════
    # PSA Exports
    # ══════════════════════════════════════════════════════════════════
    output$dl_psa_excel <- downloadHandler(
      filename = function() {
        paste0("LTBI_PSA_Results_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        psa_res <- tryCatch(psa_results_rv(), error = function(e) NULL)
        if (is.null(psa_res) || psa_res$n_valid == 0) {
          showNotification("PSA has not been run yet. Go to the PSA tab first.", type = "warning")
          return()
        }
        tmp <- generate_psa_excel(psa_res)
        file.copy(tmp, file)
      }
    )

    output$dl_psa_scatter_png <- downloadHandler(
      filename = function() "PSA_Cost_TB_Scatter.png",
      content = function(file) {
        psa_res <- tryCatch(psa_results_rv(), error = function(e) NULL)
        if (is.null(psa_res) || psa_res$n_valid == 0) {
          showNotification("PSA has not been run yet.", type = "warning")
          return()
        }
        p <- build_psa_scatter_plot(psa_res)
        if (!is.null(p)) {
          ggsave(file, plot = p, width = 10, height = 7, dpi = 200)
        }
      }
    )

    output$dl_tb_density_png <- downloadHandler(
      filename = function() "PSA_TB_Density.png",
      content = function(file) {
        psa_res <- tryCatch(psa_results_rv(), error = function(e) NULL)
        if (is.null(psa_res) || psa_res$n_valid == 0) {
          showNotification("PSA has not been run yet.", type = "warning")
          return()
        }
        p <- build_psa_tb_density_plot(psa_res)
        if (!is.null(p)) {
          ggsave(file, plot = p, width = 10, height = 7, dpi = 200)
        }
      }
    )

    # ══════════════════════════════════════════════════════════════════
    # Quick CSV Exports
    # ══════════════════════════════════════════════════════════════════
    output$dl_dt_csv <- downloadHandler(
      filename = function() "Decision_Tree_Results.csv",
      content = function(file) {
        res <- model_results_rv()
        if (is.null(res)) {
          showNotification("No model results.", type = "error")
          return()
        }
        dt <- res$dt_results
        dt$strategy_name <- settings$strategy_names
        write.csv(dt, file, row.names = FALSE)
      }
    )

    output$dl_summary_csv <- downloadHandler(
      filename = function() "CEA_Summary.csv",
      content = function(file) {
        res <- model_results_rv()
        if (is.null(res)) {
          showNotification("No model results.", type = "error")
          return()
        }
        write.csv(res$summary, file, row.names = FALSE)
      }
    )
  })
}
