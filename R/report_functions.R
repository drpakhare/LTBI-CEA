# report_functions.R — Report Generation Functions (DT-Based)
# LTBI Screening CEA Shiny Application
# Uses officer + flextable for Word document generation

#' Generate a comprehensive CEA report as a Word document (DT-based)
#' @param model_results Output from run_full_model() or run_dt_only_model()
#' @param params_rv Current parameter list (with value/low/high/distribution)
#' @param settings MODEL_SETTINGS list
#' @param psa_results Optional PSA results (NULL if PSA not run)
#' @return Path to generated .docx file
generate_cea_report <- function(model_results, params_rv, settings,
                                 wtp = 234859, psa_results = NULL,
                                 dsa_results = NULL) {

  # Create officer document
  doc <- officer::read_docx()

  # ── Page setup ──
  doc <- doc %>%
    officer::body_set_default_section(
      officer::prop_section(
        page_size = officer::page_size(orient = "portrait"),
        page_margins = officer::page_mar(
          top = 1, bottom = 1, left = 1.2, right = 1.2,
          header = 0.5, footer = 0.5
        )
      )
    )

  # ── Styles ──
  title_style <- officer::fp_text(
    font.size = 24, bold = TRUE, color = "#2E5090",
    font.family = "Calibri"
  )
  subtitle_style <- officer::fp_text(
    font.size = 14, italic = TRUE, color = "#444444",
    font.family = "Calibri"
  )
  normal_style <- officer::fp_text(
    font.size = 11, color = "#333333", font.family = "Calibri"
  )

  doc <- doc %>%
    officer::body_add_fpar(
      officer::fpar(officer::ftext("LTBI Screening in Rheumatology", title_style)),
      style = "heading 1"
    ) %>%
    officer::body_add_fpar(
      officer::fpar(officer::ftext("Cost-Effectiveness Analysis Report", subtitle_style))
    ) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(paste0("Generated: ", format(Sys.time(), "%d %B %Y, %H:%M")), normal_style)
      )
    ) %>%
    officer::body_add_par("")

  # ── 1. Executive Summary ──
  doc <- doc %>%
    officer::body_add_par("1. Executive Summary", style = "heading 1")

  summ <- model_results$summary
  best_tb <- summ %>% dplyr::slice_min(expected_tb_5y, n = 1)
  cheapest <- summ %>% dplyr::slice_min(cost_per_person, n = 1)

  summary_text <- paste0(
    "This report presents the results of a cost-effectiveness analysis comparing ",
    "five LTBI screening strategies before biologic therapy in Indian rheumatology patients. ",
    "The analysis uses a decision tree model with 5-year TB projections based on ",
    "epidemiological reactivation rates. ",
    best_tb$strategy_name[1], " achieves the lowest expected TB burden (",
    round(best_tb$expected_tb_5y[1], 2), " cases/1000 at 5 years), while ",
    cheapest$strategy_name[1], " is the least costly strategy (",
    fmt_inr(cheapest$cost_per_person[1]), " per person)."
  )

  doc <- doc %>%
    officer::body_add_par(summary_text)

  # ── 2. Strategies ──
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("2. Screening Strategies", style = "heading 1")

  si <- strategy_info()
  strat_df <- data.frame(
    Strategy = si$name,
    Test = si$test,
    `Treatment Rule` = si$rule,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  ft_strat <- flextable::flextable(strat_df) %>%
    flextable::set_header_labels(Strategy = "Strategy", Test = "Test(s)",
                                  `Treatment Rule` = "Treatment Rule") %>%
    flextable::theme_booktabs() %>%
    flextable::autofit() %>%
    flextable::bg(part = "header", bg = "#2E5090") %>%
    flextable::color(part = "header", color = "white") %>%
    flextable::fontsize(size = 10, part = "all") %>%
    flextable::font(fontname = "Calibri", part = "all")

  doc <- doc %>%
    flextable::body_add_flextable(ft_strat)

  # ── 3. Decision Tree Screening Outcomes ──
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("3. Decision Tree Screening Outcomes", style = "heading 1")

  dt <- model_results$dt_results
  dt_display <- data.frame(
    Strategy = settings$strategy_names,
    `Sensitivity` = paste0(round(dt$sensitivity * 100, 1), "%"),
    `Specificity` = paste0(round(dt$specificity * 100, 1), "%"),
    `True Positives` = paste0(round(dt$p_true_positive * 100, 2), "%"),
    `False Positives` = paste0(round(dt$p_false_positive * 100, 2), "%"),
    `False Negatives` = paste0(round(dt$p_false_negative * 100, 2), "%"),
    `Treated (%)` = paste0(round(dt$p_treated * 100, 1), "%"),
    `Screening Cost` = fmt_inr(dt$cost_screening),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  ft_dt <- flextable::flextable(dt_display) %>%
    flextable::theme_booktabs() %>%
    flextable::autofit() %>%
    flextable::bg(part = "header", bg = "#2E5090") %>%
    flextable::color(part = "header", color = "white") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::font(fontname = "Calibri", part = "all")

  doc <- doc %>%
    flextable::body_add_flextable(ft_dt)

  # ── 4. Short-Term CEA (Decision Tree) ──
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("4. Cost-Effectiveness Analysis (Decision Tree)", style = "heading 1")

  cea_display <- data.frame(
    Strategy = summ$strategy_name,
    `Cost/Person` = fmt_inr(summ$cost_per_person),
    `Expected TB (5y)/1000` = round(summ$expected_tb_5y, 2),
    `TB Averted (5y)/1000` = round(summ$tb_averted_5y, 2),
    `Cost/TB Averted` = ifelse(is.finite(summ$cost_per_tb_averted),
                                fmt_inr(summ$cost_per_tb_averted), "N/A"),
    `LTBI Detected/1000` = round(summ$ltbi_detected, 1),
    `LTBI Missed/1000` = round(summ$ltbi_missed, 1),
    `Unnecessary Tx/1000` = round(summ$unnecessarily_treated, 1),
    `NNT (Prevent 1 TB)` = ifelse(is.finite(summ$nnt_prevent_tb),
                                   round(summ$nnt_prevent_tb, 0), "N/A"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  ft_cea <- flextable::flextable(cea_display) %>%
    flextable::theme_booktabs() %>%
    flextable::autofit() %>%
    flextable::bg(part = "header", bg = "#2E5090") %>%
    flextable::color(part = "header", color = "white") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::font(fontname = "Calibri", part = "all")

  doc <- doc %>%
    flextable::body_add_flextable(ft_cea)

  # Interpretation
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Interpretation:", style = "heading 2")

  best_tb_name <- summ$strategy_name[which.min(summ$expected_tb_5y)]
  cheapest_name <- summ$strategy_name[which.min(summ$cost_per_person)]

  interp_text <- paste0(
    best_tb_name, " achieves the lowest expected TB burden at 5 years. ",
    cheapest_name, " is the least costly strategy. ",
    "Since screening decisions are fundamentally short-term and the differences in ",
    "long-term QALYs across strategies are marginal (< 0.08 over 40 years), ",
    "the primary analysis focuses on TB prevention effectiveness and screening cost. ",
    "A multi-criteria decision framework (MCDA) with adjustable weights is recommended ",
    "for final decision-making, incorporating TB prevention priority, cost, ",
    "test availability, equity, and public health considerations."
  )

  doc <- doc %>%
    officer::body_add_par(interp_text)

  # ── 5. PSA Summary (if available) ──
  if (!is.null(psa_results) && psa_results$n_valid > 0) {
    doc <- doc %>%
      officer::body_add_par("") %>%
      officer::body_add_par("5. Probabilistic Sensitivity Analysis", style = "heading 1") %>%
      officer::body_add_par(paste0(
        "Based on ", psa_results$n_valid, " valid Monte Carlo simulations ",
        "(of ", psa_results$n_total, " attempted)."
      ))

    psa_summ <- psa_summary(psa_results)
    psa_display <- data.frame(
      Strategy = psa_summ$strategy_name,
      `Mean Cost` = fmt_inr(psa_summ$mean_cost),
      `95% CI Cost` = paste0("[", fmt_inr(psa_summ$ci_cost_low), ", ",
                             fmt_inr(psa_summ$ci_cost_high), "]"),
      `Mean TB (5y)/1000` = round(psa_summ$mean_tb_5y, 2),
      `95% CI TB` = paste0("[", round(psa_summ$ci_tb_low, 2), ", ",
                           round(psa_summ$ci_tb_high, 2), "]"),
      `P(Lowest TB)` = fmt_pct(psa_summ$prob_lowest_tb),
      `P(Cheapest)` = fmt_pct(psa_summ$prob_cheapest),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    ft_psa <- flextable::flextable(psa_display) %>%
      flextable::theme_booktabs() %>%
      flextable::autofit() %>%
      flextable::bg(part = "header", bg = "#2E5090") %>%
      flextable::color(part = "header", color = "white") %>%
      flextable::fontsize(size = 9, part = "all") %>%
      flextable::font(fontname = "Calibri", part = "all")

    doc <- doc %>%
      flextable::body_add_flextable(ft_psa)

    best_psa <- psa_summ %>% dplyr::slice_max(prob_lowest_tb, n = 1)
    doc <- doc %>%
      officer::body_add_par("") %>%
      officer::body_add_par(paste0(
        best_psa$strategy_name[1],
        " has the highest probability of achieving the lowest TB burden (",
        fmt_pct(best_psa$prob_lowest_tb[1]), ")."
      ))
  }

  # ── 6. Model Assumptions ──
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Model Assumptions & Settings", style = "heading 1") %>%
    officer::body_add_par(paste0(
      "Analysis type: Decision tree with 5-year TB projection. ",
      "5 screening strategies compared: TST alone, IGRA alone, Cy-Tb alone, ",
      "TST then IGRA (sequential), and Treat All. ",
      "TB reactivation calculated from epidemiological rates adjusted for biologic use. ",
      "Prophylaxis efficacy based on INH 6-month regimen (OR = ",
      round(params_rv[["OR_INH_alone"]]$value, 2), "). ",
      "Parameters sourced from systematic literature review. ",
      "Model follows ISPOR guidelines for decision-analytic modelling."
    ))

  # ── Footer ──
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(officer::ftext(
        "Report generated by LTBI Screening CEA Shiny Application v2.0 | March 2026",
        officer::fp_text(font.size = 9, italic = TRUE, color = "#888888",
                         font.family = "Calibri")
      ))
    )

  # Save to temp file
  tmp <- tempfile(fileext = ".docx")
  print(doc, target = tmp)
  tmp
}


#' Generate a parameter export as Excel workbook
#' @param params_rv Current parameter list
#' @return Path to generated .xlsx file
generate_params_excel <- function(params_rv) {
  params <- params_rv
  rows <- lapply(names(params), function(pname) {
    x <- params[[pname]]
    data.frame(
      Parameter = pname,
      Value = x$value,
      Low = x$low,
      High = x$high,
      Distribution = x$distribution,
      Domain = x$domain,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)

  # Split by domain for multi-sheet workbook
  sheets <- split(df, df$Domain)

  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sheets, path = tmp)
  tmp
}


#' Save a ggplot as PNG and return path
save_plot_png <- function(plot_obj, width = 10, height = 6, dpi = 150) {
  tmp <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmp, plot = plot_obj, width = width, height = height, dpi = dpi)
  tmp
}


#' Generate Cost vs TB scatter plot (static version for report/download)
#' @param psa_results PSA output (DT-based)
#' @return ggplot object or NULL
build_psa_scatter_plot <- function(psa_results, wtp = NULL) {
  if (is.null(psa_results) || psa_results$n_valid == 0) return(NULL)

  si <- strategy_info()
  n_strat <- ncol(psa_results$costs)

  scatter_data <- list()
  for (j in 1:n_strat) {
    df_j <- tibble::tibble(
      cost = psa_results$costs[, j],
      tb_5y = psa_results$tb_5y[, j],
      strategy = psa_results$strategy_names[j]
    )
    if (nrow(df_j) > 1000) {
      df_j <- df_j %>% dplyr::sample_n(1000)
    }
    scatter_data[[j]] <- df_j
  }
  scatter_df <- dplyr::bind_rows(scatter_data)

  ggplot(scatter_df, aes(x = tb_5y, y = cost, color = strategy)) +
    geom_point(alpha = 0.1, size = 0.8) +
    stat_ellipse(level = 0.95, linewidth = 1) +
    scale_color_manual(values = setNames(si$color, si$name)) +
    scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
    labs(x = "Expected TB Cases per 1000 (5-Year)",
         y = "Screening Cost per Person (INR)",
         color = "Strategy",
         title = "PSA: Cost vs Expected TB Burden") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}


#' Generate TB density plot (static version for report/download)
#' @param psa_results PSA output (DT-based)
#' @return ggplot object or NULL
build_psa_tb_density_plot <- function(psa_results) {
  if (is.null(psa_results) || psa_results$n_valid == 0) return(NULL)

  si <- strategy_info()
  n_strat <- ncol(psa_results$tb_5y)

  density_data <- list()
  for (j in 1:n_strat) {
    density_data[[j]] <- tibble::tibble(
      tb_5y = psa_results$tb_5y[, j],
      strategy = psa_results$strategy_names[j]
    )
  }
  density_df <- dplyr::bind_rows(density_data)

  ggplot(density_df, aes(x = tb_5y, fill = strategy, color = strategy)) +
    geom_density(alpha = 0.3, linewidth = 0.8) +
    scale_fill_manual(values = setNames(si$color, si$name)) +
    scale_color_manual(values = setNames(si$color, si$name)) +
    labs(x = "Expected TB Cases per 1000 (5-Year)",
         y = "Density",
         fill = "Strategy", color = "Strategy",
         title = "Distribution of Expected TB Burden Across Simulations") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}


#' Generate a full report with embedded plots (DT-based)
generate_full_report_with_plots <- function(model_results, params_rv, settings,
                                             wtp = 234859, psa_results = NULL) {

  # First generate the base report
  doc_path <- generate_cea_report(model_results, params_rv, settings, wtp, psa_results)
  doc <- officer::read_docx(doc_path)

  # Add plots section
  doc <- doc %>%
    officer::body_add_break() %>%
    officer::body_add_par("Appendix: Figures", style = "heading 1")

  # TB Burden Comparison
  tryCatch({
    summ <- model_results$summary
    p <- ggplot(summ, aes(x = reorder(strategy_name, expected_tb_5y),
                          y = expected_tb_5y, fill = strategy_name)) +
      geom_col(alpha = 0.85) +
      coord_flip() +
      labs(x = NULL, y = "Expected TB Cases per 1000 (5-Year)",
           title = "Expected TB Burden by Strategy") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
    tb_path <- save_plot_png(p, width = 8, height = 5)
    doc <- doc %>%
      officer::body_add_par("Figure 1: Expected TB Burden by Strategy", style = "heading 2") %>%
      officer::body_add_img(src = tb_path, width = 6.5, height = 4)
  }, error = function(e) {})

  # Cost Comparison
  tryCatch({
    p <- ggplot(summ, aes(x = reorder(strategy_name, cost_per_person),
                          y = cost_per_person, fill = strategy_name)) +
      geom_col(alpha = 0.85) +
      coord_flip() +
      scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
      labs(x = NULL, y = "Screening Cost per Person (INR)",
           title = "Screening Cost by Strategy") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
    cost_path <- save_plot_png(p, width = 8, height = 5)
    doc <- doc %>%
      officer::body_add_par("") %>%
      officer::body_add_par("Figure 2: Screening Cost by Strategy", style = "heading 2") %>%
      officer::body_add_img(src = cost_path, width = 6.5, height = 4)
  }, error = function(e) {})

  # PSA plots (if available)
  if (!is.null(psa_results) && psa_results$n_valid > 0) {
    tryCatch({
      psa_scatter <- build_psa_scatter_plot(psa_results)
      if (!is.null(psa_scatter)) {
        scatter_path <- save_plot_png(psa_scatter, width = 8, height = 6)
        doc <- doc %>%
          officer::body_add_par("") %>%
          officer::body_add_par("Figure 3: PSA Cost vs TB Scatter", style = "heading 2") %>%
          officer::body_add_img(src = scatter_path, width = 6.5, height = 4.5)
      }
    }, error = function(e) {})

    tryCatch({
      tb_density <- build_psa_tb_density_plot(psa_results)
      if (!is.null(tb_density)) {
        density_path <- save_plot_png(tb_density, width = 8, height = 6)
        doc <- doc %>%
          officer::body_add_par("") %>%
          officer::body_add_par("Figure 4: TB Burden Distribution (PSA)", style = "heading 2") %>%
          officer::body_add_img(src = density_path, width = 6.5, height = 4.5)
      }
    }, error = function(e) {})
  }

  # Save final document
  tmp <- tempfile(fileext = ".docx")
  print(doc, target = tmp)
  tmp
}


#' Export PSA raw results as Excel (DT-based)
generate_psa_excel <- function(psa_results, wtp = NULL) {
  if (is.null(psa_results) || psa_results$n_valid == 0) return(NULL)

  costs_df <- as.data.frame(psa_results$costs)
  colnames(costs_df) <- paste0("Cost_", psa_results$strategy_names)

  tb_df <- as.data.frame(psa_results$tb_5y)
  colnames(tb_df) <- paste0("TB_5y_", psa_results$strategy_names)

  tb_averted_df <- as.data.frame(psa_results$tb_averted)
  colnames(tb_averted_df) <- paste0("TB_Averted_", psa_results$strategy_names)

  ltbi_missed_df <- as.data.frame(psa_results$ltbi_missed)
  colnames(ltbi_missed_df) <- paste0("LTBI_Missed_", psa_results$strategy_names)

  results <- cbind(
    Iteration = 1:nrow(costs_df),
    costs_df, tb_df, tb_averted_df, ltbi_missed_df
  )

  # Summary sheet
  summ <- psa_summary(psa_results)
  summ_df <- as.data.frame(summ)

  sheets <- list(
    "PSA Iterations" = results,
    "Summary Statistics" = summ_df
  )

  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sheets, path = tmp)
  tmp
}
