# report_functions.R — Report Generation Functions
# LTBI Screening CEA Shiny Application
# Uses officer + flextable for Word document generation

#' Generate a comprehensive CEA report as a Word document
#' @param model_results Output from run_full_model()
#' @param params_rv Current parameter list (with value/low/high/distribution)
#' @param settings MODEL_SETTINGS list
#' @param wtp WTP threshold used
#' @param psa_results Optional PSA results (NULL if PSA not run)
#' @param dsa_results Optional DSA tornado data (NULL if DSA not run)
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

  # ── Title ──
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
  bold_style <- officer::fp_text(
    font.size = 11, bold = TRUE, color = "#333333", font.family = "Calibri"
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
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(paste0("WTP Threshold: ", fmt_inr(wtp), "/QALY"), normal_style)
      )
    ) %>%
    officer::body_add_par("")

  # ── 1. Executive Summary ──
  doc <- doc %>%
    officer::body_add_par("1. Executive Summary", style = "heading 1")

  icer_df <- calculate_icer(model_results$summary)
  nmb_df <- calculate_nmb(icer_df, wtp)
  best <- nmb_df %>% dplyr::slice_max(nmb, n = 1)

  summary_text <- paste0(
    "This report presents the results of a cost-effectiveness analysis comparing ",
    "five LTBI screening strategies before biologic therapy in Indian rheumatology patients. ",
    "At a willingness-to-pay threshold of ", fmt_inr(wtp), "/QALY ",
    "(", round(wtp / 234859, 1), "\u00D7 India GDP per capita), the optimal strategy is ",
    best$strategy_name[1], " with a net monetary benefit of ",
    fmt_inr(best$nmb[1]), " per person."
  )

  doc <- doc %>%
    officer::body_add_par(summary_text)

  # TB burden summary
  tb_min <- icer_df %>% dplyr::slice_min(tb_cases_per_1000, n = 1)
  tb_max <- icer_df %>% dplyr::slice_max(tb_cases_per_1000, n = 1)
  doc <- doc %>%
    officer::body_add_par(paste0(
      "TB burden ranges from ", round(tb_min$tb_cases_per_1000[1], 2),
      " cases/1000 (", tb_min$strategy_name[1], ") to ",
      round(tb_max$tb_cases_per_1000[1], 2),
      " cases/1000 (", tb_max$strategy_name[1], ")."
    ))

  # ── 2. Strategies ──
  doc <- doc %>%
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

  # ── 3. Decision Tree Results ──
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

  # ── 4. Long-Term Outcomes (Markov) ──
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("4. Long-Term Outcomes (Markov Model)", style = "heading 1")

  markov_summary <- data.frame(
    Strategy = settings$strategy_names,
    `Total Cost/Person` = fmt_inr(model_results$summary$cost_total_per_person),
    `QALYs/Person` = round(model_results$summary$qaly_per_person, 4),
    `TB Cases/1000` = round(model_results$summary$tb_cases_per_1000, 2),
    `LTBI Missed (%)` = paste0(round(model_results$summary$p_ltbi_missed * 100, 2), "%"),
    `Treated (%)` = paste0(round(model_results$summary$p_treated * 100, 1), "%"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  ft_markov <- flextable::flextable(markov_summary) %>%
    flextable::theme_booktabs() %>%
    flextable::autofit() %>%
    flextable::bg(part = "header", bg = "#2E5090") %>%
    flextable::color(part = "header", color = "white") %>%
    flextable::fontsize(size = 10, part = "all") %>%
    flextable::font(fontname = "Calibri", part = "all")

  doc <- doc %>%
    flextable::body_add_flextable(ft_markov)

  # ── 5. Cost-Effectiveness Analysis ──
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("5. Cost-Effectiveness Analysis", style = "heading 1")

  # ICER table
  icer_display <- data.frame(
    Strategy = icer_df$strategy_name,
    `Cost/Person` = fmt_inr(icer_df$cost_total_per_person),
    `QALYs/Person` = round(icer_df$qaly_per_person, 4),
    `Inc. Cost` = ifelse(is.na(icer_df$inc_cost), "-", fmt_inr(icer_df$inc_cost)),
    `Inc. QALY` = ifelse(is.na(icer_df$inc_qaly), "-", round(icer_df$inc_qaly, 4)),
    ICER = fmt_icer(icer_df$icer),
    Status = icer_df$status,
    `NMB` = fmt_inr(nmb_df$nmb),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  ft_icer <- flextable::flextable(icer_display) %>%
    flextable::theme_booktabs() %>%
    flextable::autofit() %>%
    flextable::bg(part = "header", bg = "#2E5090") %>%
    flextable::color(part = "header", color = "white") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::font(fontname = "Calibri", part = "all")

  # Highlight dominated rows
  dominated_rows <- which(icer_df$status == "Dominated")
  if (length(dominated_rows) > 0) {
    ft_icer <- ft_icer %>%
      flextable::bg(i = dominated_rows, bg = "#FFE0E0")
  }

  frontier_rows <- which(icer_df$status == "On Frontier")
  if (length(frontier_rows) > 0) {
    ft_icer <- ft_icer %>%
      flextable::bg(i = frontier_rows, bg = "#E0F0FF")
  }

  doc <- doc %>%
    flextable::body_add_flextable(ft_icer)

  # Interpretation
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Interpretation:", style = "heading 2")

  interp_text <- interpret_cea(icer_df, wtp)
  # Clean markdown bold markers
  interp_clean <- gsub("\\*\\*", "", interp_text)
  paragraphs <- strsplit(interp_clean, "\n\n")[[1]]
  for (para in paragraphs) {
    if (nchar(trimws(para)) > 0) {
      doc <- doc %>% officer::body_add_par(trimws(para))
    }
  }

  # ── 6. NMB at Multiple WTP Thresholds ──
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("6. Net Monetary Benefit at Multiple WTP Thresholds", style = "heading 1")

  wtp_vec <- c(100000, 234859, 500000, 704577)
  wtp_labels <- c("\u20B91L", "\u20B92.35L (1\u00D7GDP)", "\u20B95L", "\u20B97.05L (3\u00D7GDP)")
  nmb_multi <- data.frame(Strategy = icer_df$strategy_name, stringsAsFactors = FALSE)
  for (j in seq_along(wtp_vec)) {
    nmb_j <- icer_df$qaly_per_person * wtp_vec[j] - icer_df$cost_total_per_person
    nmb_multi[[wtp_labels[j]]] <- fmt_inr(nmb_j)
  }

  ft_nmb <- flextable::flextable(nmb_multi) %>%
    flextable::theme_booktabs() %>%
    flextable::autofit() %>%
    flextable::bg(part = "header", bg = "#2E5090") %>%
    flextable::color(part = "header", color = "white") %>%
    flextable::fontsize(size = 10, part = "all") %>%
    flextable::font(fontname = "Calibri", part = "all")

  doc <- doc %>%
    flextable::body_add_flextable(ft_nmb)

  # ── 7. PSA Summary (if available) ──
  if (!is.null(psa_results) && psa_results$n_valid > 0) {
    doc <- doc %>%
      officer::body_add_par("") %>%
      officer::body_add_par("7. Probabilistic Sensitivity Analysis", style = "heading 1") %>%
      officer::body_add_par(paste0(
        "Based on ", psa_results$n_valid, " valid Monte Carlo simulations ",
        "(of ", psa_results$n_total, " attempted)."
      ))

    summ <- psa_summary(psa_results, wtp)
    psa_display <- data.frame(
      Strategy = summ$strategy_name,
      `Mean Cost` = fmt_inr(summ$mean_cost),
      `95% CI Cost` = paste0("[", fmt_inr(summ$ci_cost_low), ", ", fmt_inr(summ$ci_cost_high), "]"),
      `Mean QALY` = round(summ$mean_qaly, 4),
      `95% CI QALY` = paste0("[", round(summ$ci_qaly_low, 4), ", ", round(summ$ci_qaly_high, 4), "]"),
      `Mean NMB` = fmt_inr(summ$mean_nmb),
      `P(CE)` = fmt_pct(summ$prob_ce),
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

    # EVPI
    evpi_val <- calculate_evpi(psa_results, wtp)
    doc <- doc %>%
      officer::body_add_par("") %>%
      officer::body_add_par(paste0(
        "EVPI at WTP = ", fmt_inr(wtp), "/QALY: ",
        fmt_inr(evpi_val), " per person."
      ))

    # Best strategy
    best_psa <- summ %>% dplyr::slice_max(prob_ce, n = 1)
    doc <- doc %>%
      officer::body_add_par(paste0(
        "At WTP = ", fmt_inr(wtp), "/QALY, ", best_psa$strategy_name[1],
        " has the highest probability of being cost-effective (",
        fmt_pct(best_psa$prob_ce[1]), ")."
      ))
  }

  # ── 8. Model Assumptions ──
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Model Assumptions & Settings", style = "heading 1") %>%
    officer::body_add_par(paste0(
      "Cohort size: ", scales::comma(settings$n_cohort), " patients. ",
      "Time horizon: ", settings$time_horizon, " years (",
      settings$n_cycles, " cycles of ", settings$cycle_length * 12, " months). ",
      "Discount rate: ", settings$discount_rate * 100, "% per annum. ",
      "Half-cycle correction applied. ",
      "Health states: Well on biologic, LTBI untreated, LTBI on prophylaxis, ",
      "Active TB, TB treatment, Post-TB, Dead. ",
      "Indian WTP threshold: 1-3\u00D7 GDP per capita (",
      fmt_inr(234859), " - ", fmt_inr(704577), "/QALY)."
    ))

  # ── Footer ──
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(officer::ftext(
        "Report generated by LTBI Screening CEA Shiny Application v1.0 | March 2026",
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
#' @param plot_obj A ggplot object
#' @param width Width in inches
#' @param height Height in inches
#' @param dpi Resolution
#' @return Path to saved PNG
save_plot_png <- function(plot_obj, width = 10, height = 6, dpi = 150) {
  tmp <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmp, plot = plot_obj, width = width, height = height, dpi = dpi)
  tmp
}


#' Generate CE plane ggplot (static version for report)
#' @param model_results Output from run_full_model()
#' @param wtp WTP threshold
#' @return ggplot object
build_ceplane_plot <- function(model_results, wtp = 234859) {
  icer_df <- calculate_icer(model_results$summary)
  si <- strategy_info()

  ref_cost <- min(icer_df$cost_total_per_person)
  ref_qaly <- icer_df$qaly_per_person[which.min(icer_df$cost_total_per_person)]

  plot_df <- icer_df %>%
    dplyr::left_join(si %>% dplyr::select(id, color), by = c("strategy" = "id")) %>%
    dplyr::mutate(
      inc_c = cost_total_per_person - ref_cost,
      inc_q = qaly_per_person - ref_qaly
    )

  q_range <- range(plot_df$inc_q)
  wtp_df <- tibble::tibble(x = q_range, y = q_range * wtp)

  ggplot(plot_df, aes(x = inc_q, y = inc_c, color = strategy_name)) +
    geom_point(size = 4, alpha = 0.9) +
    ggrepel::geom_text_repel(aes(label = strategy_name), size = 3) +
    geom_line(data = wtp_df, aes(x = x, y = y), inherit.aes = FALSE,
              linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = 0, color = "gray80") +
    geom_vline(xintercept = 0, color = "gray80") +
    scale_color_manual(values = setNames(si$color, si$name)) +
    scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
    labs(x = "Incremental QALYs",
         y = "Incremental Cost (INR)",
         color = "Strategy",
         title = "Cost-Effectiveness Plane") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}


#' Generate NMB bar chart (static version)
#' @param model_results Output from run_full_model()
#' @return ggplot object
build_nmb_plot <- function(model_results) {
  icer_df <- calculate_icer(model_results$summary)

  wtp_vec <- c(100000, 234859, 500000, 704577)
  wtp_labels <- c("\u20B91L", "\u20B92.35L (1\u00D7GDP)", "\u20B95L", "\u20B97.05L (3\u00D7GDP)")

  nmb_data <- lapply(seq_along(wtp_vec), function(j) {
    nmb <- icer_df$qaly_per_person * wtp_vec[j] - icer_df$cost_total_per_person
    tibble::tibble(
      strategy_name = icer_df$strategy_name,
      wtp_label = wtp_labels[j],
      nmb = nmb
    )
  })
  nmb_df <- dplyr::bind_rows(nmb_data)
  nmb_df$wtp_label <- factor(nmb_df$wtp_label, levels = wtp_labels)

  ggplot(nmb_df, aes(x = strategy_name, y = nmb, fill = wtp_label)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = "Blues") +
    scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
    labs(x = NULL, y = "Net Monetary Benefit (INR/Person)",
         fill = "WTP Threshold",
         title = "Net Monetary Benefit by Strategy") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "bottom")
}


#' Generate Markov trace plot (static version)
#' @param model_results Output from run_full_model()
#' @param strategy Strategy ID (e.g., "Sequential")
#' @param settings MODEL_SETTINGS
#' @return ggplot object
build_markov_trace_plot <- function(model_results, strategy = "Sequential", settings = MODEL_SETTINGS) {
  markov <- model_results$markov_results[[strategy]]
  if (is.null(markov)) return(NULL)

  trace_df <- as.data.frame(markov$trace)
  trace_df$cycle <- 0:settings$n_cycles
  trace_df$year <- trace_df$cycle * settings$cycle_length

  # Normalize to proportions
  total <- rowSums(trace_df[, HEALTH_STATES])
  for (s in HEALTH_STATES) {
    trace_df[[s]] <- trace_df[[s]] / total
  }

  trace_long <- tidyr::pivot_longer(
    trace_df, cols = all_of(HEALTH_STATES),
    names_to = "state", values_to = "proportion"
  )

  state_colors <- c(
    "Well_on_biologic" = "#70AD47", "LTBI_untreated" = "#ED7D31",
    "LTBI_prophylaxis" = "#4472C4", "Active_TB" = "#FF4444",
    "TB_treatment" = "#FFC000", "Post_TB" = "#9DC3E6", "Dead" = "#808080"
  )

  strat_name <- settings$strategy_names[match(strategy, settings$strategies)]

  ggplot(trace_long, aes(x = year, y = proportion, fill = state)) +
    geom_area(alpha = 0.8) +
    scale_fill_manual(values = state_colors) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Years", y = "Cohort Proportion",
         fill = "Health State",
         title = paste0("Markov Trace: ", strat_name)) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}


#' Generate PSA scatter plot (static version)
#' @param psa_results PSA output
#' @param wtp WTP threshold
#' @return ggplot object or NULL
build_psa_scatter_plot <- function(psa_results, wtp = 234859) {
  if (is.null(psa_results) || psa_results$n_valid == 0) return(NULL)

  si <- strategy_info()
  n_strat <- ncol(psa_results$costs)

  ref_cost <- psa_results$costs[, 1]
  ref_qaly <- psa_results$qalys[, 1]

  scatter_data <- list()
  for (j in 2:n_strat) {
    scatter_data[[j-1]] <- tibble::tibble(
      inc_cost = psa_results$costs[, j] - ref_cost,
      inc_qaly = psa_results$qalys[, j] - ref_qaly,
      strategy = psa_results$strategy_names[j]
    )
  }
  scatter_df <- dplyr::bind_rows(scatter_data)

  if (nrow(scatter_df) > 3000) {
    scatter_df <- scatter_df %>% dplyr::sample_n(3000)
  }

  ggplot(scatter_df, aes(x = inc_qaly, y = inc_cost, color = strategy)) +
    geom_point(alpha = 0.1, size = 0.8) +
    stat_ellipse(level = 0.95, linewidth = 1) +
    geom_abline(slope = wtp, intercept = 0, linetype = "dashed", color = "gray40") +
    geom_hline(yintercept = 0, color = "gray80") +
    geom_vline(xintercept = 0, color = "gray80") +
    scale_color_manual(values = setNames(
      si$color[2:n_strat], psa_results$strategy_names[2:n_strat]
    )) +
    scale_y_continuous(labels = function(x) paste0("\u20B9", scales::comma(x))) +
    labs(x = paste0("Incremental QALYs (vs ", psa_results$strategy_names[1], ")"),
         y = "Incremental Cost (INR)",
         color = "Strategy",
         title = "PSA Cost-Effectiveness Scatter Plot") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}


#' Generate CEAC plot (static version)
#' @param psa_results PSA output
#' @return ggplot object or NULL
build_ceac_plot <- function(psa_results) {
  if (is.null(psa_results) || psa_results$n_valid == 0) return(NULL)

  si <- strategy_info()
  ceac <- calculate_ceac(psa_results, wtp_range = seq(0, 1000000, by = 10000))

  ggplot(ceac, aes(x = wtp, y = prob_ce, color = strategy_name)) +
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
         color = "Strategy",
         title = "Cost-Effectiveness Acceptability Curve (CEAC)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}


#' Generate a full report with embedded plots
#' @param model_results Output from run_full_model()
#' @param params_rv Current parameter list
#' @param settings MODEL_SETTINGS list
#' @param wtp WTP threshold
#' @param psa_results Optional PSA results
#' @return Path to generated .docx file
generate_full_report_with_plots <- function(model_results, params_rv, settings,
                                             wtp = 234859, psa_results = NULL) {

  # First generate the base report
  doc_path <- generate_cea_report(model_results, params_rv, settings, wtp, psa_results)
  doc <- officer::read_docx(doc_path)

  # Add plots section
  doc <- doc %>%
    officer::body_add_break() %>%
    officer::body_add_par("Appendix: Figures", style = "heading 1")

  # CE Plane
  tryCatch({
    ce_plot <- build_ceplane_plot(model_results, wtp)
    ce_path <- save_plot_png(ce_plot, width = 8, height = 6)
    doc <- doc %>%
      officer::body_add_par("Figure 1: Cost-Effectiveness Plane", style = "heading 2") %>%
      officer::body_add_img(src = ce_path, width = 6.5, height = 4.5)
  }, error = function(e) {})

  # NMB
  tryCatch({
    nmb_plot <- build_nmb_plot(model_results)
    nmb_path <- save_plot_png(nmb_plot, width = 8, height = 6)
    doc <- doc %>%
      officer::body_add_par("") %>%
      officer::body_add_par("Figure 2: Net Monetary Benefit", style = "heading 2") %>%
      officer::body_add_img(src = nmb_path, width = 6.5, height = 4.5)
  }, error = function(e) {})

  # Markov Trace (for optimal strategy)
  tryCatch({
    best_strat <- "Sequential"  # default; could be dynamic
    trace_plot <- build_markov_trace_plot(model_results, best_strat, settings)
    if (!is.null(trace_plot)) {
      trace_path <- save_plot_png(trace_plot, width = 8, height = 6)
      doc <- doc %>%
        officer::body_add_par("") %>%
        officer::body_add_par("Figure 3: Markov Trace (Sequential Strategy)", style = "heading 2") %>%
        officer::body_add_img(src = trace_path, width = 6.5, height = 4.5)
    }
  }, error = function(e) {})

  # PSA plots (if available)
  if (!is.null(psa_results) && psa_results$n_valid > 0) {
    tryCatch({
      psa_scatter <- build_psa_scatter_plot(psa_results, wtp)
      if (!is.null(psa_scatter)) {
        scatter_path <- save_plot_png(psa_scatter, width = 8, height = 6)
        doc <- doc %>%
          officer::body_add_par("") %>%
          officer::body_add_par("Figure 4: PSA Scatter Plot", style = "heading 2") %>%
          officer::body_add_img(src = scatter_path, width = 6.5, height = 4.5)
      }
    }, error = function(e) {})

    tryCatch({
      ceac_plot <- build_ceac_plot(psa_results)
      if (!is.null(ceac_plot)) {
        ceac_path <- save_plot_png(ceac_plot, width = 8, height = 6)
        doc <- doc %>%
          officer::body_add_par("") %>%
          officer::body_add_par("Figure 5: CEAC", style = "heading 2") %>%
          officer::body_add_img(src = ceac_path, width = 6.5, height = 4.5)
      }
    }, error = function(e) {})
  }

  # Save final document
  tmp <- tempfile(fileext = ".docx")
  print(doc, target = tmp)
  tmp
}


#' Export PSA raw results as Excel
#' @param psa_results PSA output
#' @param wtp WTP threshold
#' @return Path to generated .xlsx file
generate_psa_excel <- function(psa_results, wtp = 234859) {
  if (is.null(psa_results) || psa_results$n_valid == 0) return(NULL)

  costs_df <- as.data.frame(psa_results$costs)
  colnames(costs_df) <- paste0("Cost_", psa_results$strategy_names)

  qalys_df <- as.data.frame(psa_results$qalys)
  colnames(qalys_df) <- paste0("QALY_", psa_results$strategy_names)

  nmb_mat <- psa_results$qalys * wtp - psa_results$costs
  nmb_df <- as.data.frame(nmb_mat)
  colnames(nmb_df) <- paste0("NMB_", psa_results$strategy_names)

  results <- cbind(
    Iteration = 1:nrow(costs_df),
    costs_df, qalys_df, nmb_df
  )

  # Summary sheet
  summ <- psa_summary(psa_results, wtp)
  summ_df <- as.data.frame(summ)

  sheets <- list(
    "PSA Iterations" = results,
    "Summary Statistics" = summ_df
  )

  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sheets, path = tmp)
  tmp
}
