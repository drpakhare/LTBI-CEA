# sa_functions.R — Deterministic & Probabilistic Sensitivity Analysis
# LTBI Screening CEA — Decision Tree Based (no Markov dependency)

# ══════════════════════════════════════════════════════════════════════════════
# DETERMINISTIC SENSITIVITY ANALYSIS (DSA)
# ══════════════════════════════════════════════════════════════════════════════

#' Run one-way sensitivity analysis (DT-based)
#' @param base_params Full parameter list (with value/low/high/distribution)
#' @param param_names Character vector of parameter names to vary
#' @param settings MODEL_SETTINGS
#' @param n_steps Number of steps between low and high
#' @return tibble with DSA results
run_owsa <- function(base_params, param_names, settings = MODEL_SETTINGS, n_steps = 10) {
  p_base <- sapply(base_params, function(x) x$value, USE.NAMES = TRUE)

  results <- list()

  for (pname in param_names) {
    if (!(pname %in% names(base_params))) next
    param_info <- base_params[[pname]]
    low <- param_info$low
    high <- param_info$high
    values <- seq(low, high, length.out = n_steps)

    for (v in values) {
      p_mod <- p_base
      p_mod[[pname]] <- v

      tryCatch({
        model_out <- run_dt_only_model(p_mod, settings)

        for (i in 1:nrow(model_out$summary)) {
          results[[length(results) + 1]] <- tibble::tibble(
            param_name = pname,
            param_value = v,
            param_low = low,
            param_high = high,
            strategy = model_out$summary$strategy[i],
            strategy_name = model_out$summary$strategy_name[i],
            cost = model_out$summary$cost_per_person[i],
            expected_tb_5y = model_out$summary$expected_tb_5y[i],
            tb_averted_5y = model_out$summary$tb_averted_5y[i],
            cost_per_tb_averted = model_out$summary$cost_per_tb_averted[i],
            ltbi_missed = model_out$summary$ltbi_missed[i]
          )
        }
      }, error = function(e) {
        # Skip problematic parameter values
      })
    }
  }

  bind_rows(results)
}

#' Create tornado data from OWSA results
#' @param owsa_results Output from run_owsa
#' @param ref_strategy Reference strategy
#' @param outcome Which outcome to use: "expected_tb_5y", "cost", "cost_per_tb_averted"
#' @return tibble formatted for tornado plot
tornado_data <- function(owsa_results, ref_strategy = "TreatAll",
                          outcome = "expected_tb_5y") {
  owsa_results %>%
    filter(strategy == ref_strategy) %>%
    group_by(param_name) %>%
    summarise(
      value_at_low = .data[[outcome]][which.min(param_value)],
      value_at_high = .data[[outcome]][which.max(param_value)],
      param_low = first(param_low),
      param_high = first(param_high),
      value_range = abs(max(.data[[outcome]], na.rm = TRUE) -
                         min(.data[[outcome]], na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(desc(value_range))
}

#' Run two-way sensitivity analysis (DT-based)
#' @param base_params Full parameter list
#' @param param1 First parameter name
#' @param param2 Second parameter name
#' @param ref_strategy Strategy of interest
#' @param settings MODEL_SETTINGS
#' @param n_steps Grid resolution per axis
run_twsa <- function(base_params, param1, param2, ref_strategy = "TreatAll",
                     outcome = "expected_tb_5y", settings = MODEL_SETTINGS, n_steps = 7) {
  p_base <- sapply(base_params, function(x) x$value, USE.NAMES = TRUE)

  # Column name mapping: outcome selector -> summary column name
  col_map <- c(expected_tb_5y = "expected_tb_5y", cost = "cost_per_person",
               ltbi_missed = "ltbi_missed", cost_per_tb_averted = "cost_per_tb_averted")
  col_name <- if (outcome %in% names(col_map)) col_map[[outcome]] else outcome

  vals1 <- seq(base_params[[param1]]$low, base_params[[param1]]$high, length.out = n_steps)
  vals2 <- seq(base_params[[param2]]$low, base_params[[param2]]$high, length.out = n_steps)

  grid <- expand.grid(v1 = vals1, v2 = vals2)
  grid$optimal <- NA_character_
  grid$ref_value <- NA_real_

  for (i in 1:nrow(grid)) {
    p_mod <- p_base
    p_mod[[param1]] <- grid$v1[i]
    p_mod[[param2]] <- grid$v2[i]

    tryCatch({
      model_out <- run_dt_only_model(p_mod, settings)
      sm <- model_out$summary
      # Optimal = strategy with best (lowest) value on selected outcome
      best_idx <- which.min(sm[[col_name]])
      grid$optimal[i] <- sm$strategy_name[best_idx]
      # Value for the selected reference strategy
      ref_idx <- which(sm$strategy == ref_strategy)
      if (length(ref_idx) > 0) grid$ref_value[i] <- sm[[col_name]][ref_idx]
    }, error = function(e) {})
  }

  grid$param1_name <- param1
  grid$param2_name <- param2
  grid$outcome <- outcome
  grid
}

#' Find threshold value for a parameter where optimal strategy changes
#' @param base_params Full parameter list
#' @param param_name Parameter to vary
#' @param settings MODEL_SETTINGS
find_threshold <- function(base_params, param_name, outcome = "expected_tb_5y",
                           settings = MODEL_SETTINGS, n_points = 30) {
  p_base <- sapply(base_params, function(x) x$value, USE.NAMES = TRUE)
  low <- base_params[[param_name]]$low
  high <- base_params[[param_name]]$high
  base_val <- base_params[[param_name]]$value

  # Column name mapping
  col_map <- c(expected_tb_5y = "expected_tb_5y", cost = "cost_per_person",
               ltbi_missed = "ltbi_missed", cost_per_tb_averted = "cost_per_tb_averted")
  col_name <- if (outcome %in% names(col_map)) col_map[[outcome]] else outcome

  outcome_labels <- c(expected_tb_5y = "Expected TB/1000 (5y)", cost = "Cost/Person (INR)",
                      ltbi_missed = "LTBI Missed/1000", cost_per_tb_averted = "Cost/TB Averted")
  outcome_label <- if (outcome %in% names(outcome_labels)) outcome_labels[[outcome]] else outcome

  # Sweep parameter across range and record outcome for all strategies
  vals <- seq(low, high, length.out = n_points)
  sweep_results <- list()

  for (v in vals) {
    p_mod <- p_base
    p_mod[[param_name]] <- v
    tryCatch({
      model_out <- run_dt_only_model(p_mod, settings)
      sm <- model_out$summary
      for (j in 1:nrow(sm)) {
        sweep_results[[length(sweep_results) + 1]] <- tibble::tibble(
          param_value = v,
          strategy = sm$strategy[j],
          strategy_name = sm$strategy_name[j],
          outcome_value = sm[[col_name]][j]
        )
      }
    }, error = function(e) {})
  }

  sweep_df <- bind_rows(sweep_results)

  # Find thresholds: where the optimal (lowest) strategy changes
  thresholds <- list()
  if (nrow(sweep_df) > 0) {
    optimal_per_val <- sweep_df %>%
      group_by(param_value) %>%
      slice_min(outcome_value, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(param_value)

    # Detect switch points
    for (i in 2:nrow(optimal_per_val)) {
      if (optimal_per_val$strategy[i] != optimal_per_val$strategy[i-1]) {
        thresholds[[length(thresholds) + 1]] <- list(
          value = (optimal_per_val$param_value[i] + optimal_per_val$param_value[i-1]) / 2,
          from = optimal_per_val$strategy_name[i-1],
          to = optimal_per_val$strategy_name[i]
        )
      }
    }
  }

  # Build message
  if (length(thresholds) == 0) {
    best_at_base <- sweep_df %>%
      filter(abs(param_value - base_val) == min(abs(param_value - base_val))) %>%
      slice_min(outcome_value, n = 1, with_ties = FALSE)
    msg <- paste0("No threshold found for ", param_name, ": ",
                  best_at_base$strategy_name[1], " has the lowest ",
                  outcome_label, " across the entire range [",
                  round(low, 4), " \u2013 ", round(high, 4), "]")
  } else {
    msg_parts <- sapply(thresholds, function(th) {
      paste0("At ", param_name, " \u2248 ", round(th$value, 4),
             ": best strategy switches from ", th$from, " to ", th$to)
    })
    msg <- paste(msg_parts, collapse = "\n")
  }

  list(
    sweep = sweep_df,
    thresholds = thresholds,
    param_name = param_name,
    base_value = base_val,
    outcome = outcome,
    outcome_label = outcome_label,
    message = msg
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# PROBABILISTIC SENSITIVITY ANALYSIS (PSA) — DT-Based
# ══════════════════════════════════════════════════════════════════════════════

#' Sample one set of parameters from distributions
#' @param base_params Full parameter list with distribution info
sample_params_once <- function(base_params) {
  p <- sapply(base_params, function(x) {
    v <- x$value
    lo <- x$low
    hi <- x$high
    dist <- tolower(x$distribution)

    if (is.na(dist) || dist %in% c("-", "fixed", "")) return(v)

    tryCatch({
      switch(dist,
        beta = sample_beta(1, v, lo, hi),
        gamma = sample_gamma(1, v, lo, hi),
        `log-normal` = sample_lnorm(1, v, lo, hi),
        lognormal = sample_lnorm(1, v, lo, hi),
        normal = {
          se <- (hi - lo) / (2 * 1.96)
          rnorm(1, v, max(se, 0.001))
        },
        uniform = runif(1, lo, hi),
        v  # default: return base case
      )
    }, error = function(e) v)
  }, USE.NAMES = TRUE)
  p
}

#' Run PSA (DT-based, no Markov)
#' @param base_params Full parameter list
#' @param n_sim Number of simulations
#' @param settings MODEL_SETTINGS
#' @param progress_fn Optional function(i, n) for progress updates
#' @return list with per-strategy cost and TB metrics
run_psa <- function(base_params, n_sim = 1000, settings = MODEL_SETTINGS,
                    progress_fn = NULL) {

  n_strat <- length(settings$strategies)
  costs_mat <- matrix(NA, nrow = n_sim, ncol = n_strat)
  tb_mat <- matrix(NA, nrow = n_sim, ncol = n_strat)
  tb_averted_mat <- matrix(NA, nrow = n_sim, ncol = n_strat)
  ltbi_missed_mat <- matrix(NA, nrow = n_sim, ncol = n_strat)
  colnames(costs_mat) <- colnames(tb_mat) <- settings$strategies
  colnames(tb_averted_mat) <- colnames(ltbi_missed_mat) <- settings$strategies

  for (i in 1:n_sim) {
    if (!is.null(progress_fn)) progress_fn(i, n_sim)

    p_sampled <- sample_params_once(base_params)

    tryCatch({
      model_out <- run_dt_only_model(p_sampled, settings)
      costs_mat[i, ] <- model_out$summary$cost_per_person
      tb_mat[i, ] <- model_out$summary$expected_tb_5y
      tb_averted_mat[i, ] <- model_out$summary$tb_averted_5y
      ltbi_missed_mat[i, ] <- model_out$summary$ltbi_missed
    }, error = function(e) {
      # Keep as NA for failed iterations
    })
  }

  # Remove failed iterations
  valid <- complete.cases(costs_mat) & complete.cases(tb_mat)
  costs_mat <- costs_mat[valid, , drop = FALSE]
  tb_mat <- tb_mat[valid, , drop = FALSE]
  tb_averted_mat <- tb_averted_mat[valid, , drop = FALSE]
  ltbi_missed_mat <- ltbi_missed_mat[valid, , drop = FALSE]

  list(
    costs = costs_mat,
    tb_5y = tb_mat,
    tb_averted = tb_averted_mat,
    ltbi_missed = ltbi_missed_mat,
    n_valid = sum(valid),
    n_total = n_sim,
    strategies = settings$strategies,
    strategy_names = settings$strategy_names
  )
}

#' Calculate acceptability curve: probability each strategy has lowest TB
#' @param psa PSA output
#' @return tibble with probability at each threshold
calculate_ceac <- function(psa, wtp_range = NULL) {
  # For DT-based: "acceptability" = probability of having lowest expected TB
  n_sim <- nrow(psa$tb_5y)
  n_strat <- ncol(psa$tb_5y)

  # Simple: which strategy has lowest TB at 5y?
  best <- apply(psa$tb_5y, 1, which.min)
  probs <- tabulate(best, nbins = n_strat) / n_sim

  tibble::tibble(
    strategy = psa$strategies,
    strategy_name = psa$strategy_names,
    prob_lowest_tb = probs
  )
}

#' PSA summary statistics (DT-based)
psa_summary <- function(psa, wtp = NULL) {
  n_strat <- ncol(psa$costs)

  # Which strategy has lowest TB in each simulation?
  best_tb <- apply(psa$tb_5y, 1, which.min)
  prob_best_tb <- tabulate(best_tb, nbins = n_strat) / nrow(psa$costs)

  # Which strategy has lowest cost?
  best_cost <- apply(psa$costs, 1, which.min)
  prob_cheapest <- tabulate(best_cost, nbins = n_strat) / nrow(psa$costs)

  tibble::tibble(
    strategy = psa$strategies,
    strategy_name = psa$strategy_names,
    mean_cost = colMeans(psa$costs),
    sd_cost = apply(psa$costs, 2, sd),
    ci_cost_low = apply(psa$costs, 2, quantile, 0.025),
    ci_cost_high = apply(psa$costs, 2, quantile, 0.975),
    mean_tb_5y = colMeans(psa$tb_5y),
    sd_tb_5y = apply(psa$tb_5y, 2, sd),
    ci_tb_low = apply(psa$tb_5y, 2, quantile, 0.025),
    ci_tb_high = apply(psa$tb_5y, 2, quantile, 0.975),
    mean_tb_averted = colMeans(psa$tb_averted),
    mean_ltbi_missed = colMeans(psa$ltbi_missed),
    prob_lowest_tb = prob_best_tb,
    prob_cheapest = prob_cheapest
  )
}
