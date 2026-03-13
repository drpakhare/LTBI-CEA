# sa_functions.R — Deterministic & Probabilistic Sensitivity Analysis
# LTBI Screening CEA Shiny Application

# ══════════════════════════════════════════════════════════════════════════════
# DETERMINISTIC SENSITIVITY ANALYSIS (DSA)
# ══════════════════════════════════════════════════════════════════════════════

#' Run one-way sensitivity analysis
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
        model_out <- run_full_model(p_mod, settings)
        icer_df <- calculate_icer(model_out$summary)

        for (i in 1:nrow(icer_df)) {
          results[[length(results) + 1]] <- tibble::tibble(
            param_name = pname,
            param_value = v,
            param_low = low,
            param_high = high,
            strategy = icer_df$strategy[i],
            strategy_name = icer_df$strategy_name[i],
            cost = icer_df$cost_total_per_person[i],
            qaly = icer_df$qaly_per_person[i],
            nmb_235k = icer_df$qaly_per_person[i] * 234859 - icer_df$cost_total_per_person[i]
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
#' @param ref_strategy Reference strategy for NMB comparison
#' @param wtp WTP threshold
#' @return tibble formatted for tornado plot
tornado_data <- function(owsa_results, ref_strategy = "TreatAll", wtp = 234859) {
  # For each parameter, find NMB range of the reference strategy
  owsa_results %>%
    filter(strategy == ref_strategy) %>%
    mutate(nmb = qaly * wtp - cost) %>%
    group_by(param_name) %>%
    summarise(
      nmb_at_low = nmb[which.min(param_value)],
      nmb_at_high = nmb[which.max(param_value)],
      param_low = first(param_low),
      param_high = first(param_high),
      nmb_range = abs(max(nmb) - min(nmb)),
      .groups = "drop"
    ) %>%
    arrange(desc(nmb_range))
}

#' Run two-way sensitivity analysis
#' @param base_params Full parameter list
#' @param param1 First parameter name
#' @param param2 Second parameter name
#' @param ref_strategy Strategy of interest
#' @param wtp WTP threshold
#' @param n_steps Grid resolution per axis
run_twsa <- function(base_params, param1, param2, ref_strategy = "TreatAll",
                     wtp = 234859, settings = MODEL_SETTINGS, n_steps = 7) {
  p_base <- sapply(base_params, function(x) x$value, USE.NAMES = TRUE)

  vals1 <- seq(base_params[[param1]]$low, base_params[[param1]]$high, length.out = n_steps)
  vals2 <- seq(base_params[[param2]]$low, base_params[[param2]]$high, length.out = n_steps)

  grid <- expand.grid(v1 = vals1, v2 = vals2)
  grid$nmb <- NA_real_
  grid$optimal <- NA_character_

  for (i in 1:nrow(grid)) {
    p_mod <- p_base
    p_mod[[param1]] <- grid$v1[i]
    p_mod[[param2]] <- grid$v2[i]

    tryCatch({
      model_out <- run_full_model(p_mod, settings)
      nmb_vec <- model_out$summary$qaly_per_person * wtp - model_out$summary$cost_total_per_person
      best_idx <- which.max(nmb_vec)
      grid$nmb[i] <- nmb_vec[model_out$summary$strategy == ref_strategy]
      grid$optimal[i] <- model_out$summary$strategy_name[best_idx]
    }, error = function(e) {})
  }

  grid$param1_name <- param1
  grid$param2_name <- param2
  grid
}

#' Find threshold value for a parameter where optimal strategy changes
#' @param base_params Full parameter list
#' @param param_name Parameter to vary
#' @param wtp WTP threshold
#' @param tol Tolerance for convergence
find_threshold <- function(base_params, param_name, wtp = 234859,
                           settings = MODEL_SETTINGS, tol = 0.001) {
  p_base <- sapply(base_params, function(x) x$value, USE.NAMES = TRUE)
  low <- base_params[[param_name]]$low
  high <- base_params[[param_name]]$high

  # Find optimal at low and high
  get_optimal <- function(val) {
    p_mod <- p_base
    p_mod[[param_name]] <- val
    model_out <- run_full_model(p_mod, settings)
    nmb <- model_out$summary$qaly_per_person * wtp - model_out$summary$cost_total_per_person
    model_out$summary$strategy[which.max(nmb)]
  }

  opt_low <- get_optimal(low)
  opt_high <- get_optimal(high)

  if (opt_low == opt_high) {
    return(list(
      threshold = NA,
      message = paste0("No threshold found: ", opt_low, " is optimal across entire range [",
                       round(low, 4), ", ", round(high, 4), "]")
    ))
  }

  # Binary search
  for (iter in 1:50) {
    mid <- (low + high) / 2
    opt_mid <- get_optimal(mid)
    if (opt_mid == opt_low) {
      low <- mid
    } else {
      high <- mid
    }
    if (high - low < tol) break
  }

  list(
    threshold = (low + high) / 2,
    strategy_below = opt_low,
    strategy_above = get_optimal(high),
    message = paste0("At ", param_name, " = ", round((low + high) / 2, 4),
                     ", optimal strategy switches from ", opt_low, " to ", get_optimal(high))
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# PROBABILISTIC SENSITIVITY ANALYSIS (PSA)
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

#' Run PSA
#' @param base_params Full parameter list
#' @param n_sim Number of simulations
#' @param settings MODEL_SETTINGS
#' @param progress_fn Optional function(i, n) for progress updates
#' @return list with psa_results matrix and summary
run_psa <- function(base_params, n_sim = 1000, settings = MODEL_SETTINGS,
                    progress_fn = NULL) {

  n_strat <- length(settings$strategies)
  costs_mat <- matrix(NA, nrow = n_sim, ncol = n_strat)
  qalys_mat <- matrix(NA, nrow = n_sim, ncol = n_strat)
  colnames(costs_mat) <- colnames(qalys_mat) <- settings$strategies

  for (i in 1:n_sim) {
    if (!is.null(progress_fn)) progress_fn(i, n_sim)

    p_sampled <- sample_params_once(base_params)

    tryCatch({
      model_out <- run_full_model(p_sampled, settings)
      costs_mat[i, ] <- model_out$summary$cost_total_per_person
      qalys_mat[i, ] <- model_out$summary$qaly_per_person
    }, error = function(e) {
      # Keep as NA for failed iterations
    })
  }

  # Remove failed iterations
  valid <- complete.cases(costs_mat) & complete.cases(qalys_mat)
  costs_mat <- costs_mat[valid, , drop = FALSE]
  qalys_mat <- qalys_mat[valid, , drop = FALSE]

  list(
    costs = costs_mat,
    qalys = qalys_mat,
    n_valid = sum(valid),
    n_total = n_sim,
    strategies = settings$strategies,
    strategy_names = settings$strategy_names
  )
}

#' Calculate CEAC from PSA results
#' @param psa PSA output
#' @param wtp_range Vector of WTP thresholds
#' @return tibble with probability of cost-effectiveness at each WTP
calculate_ceac <- function(psa, wtp_range = seq(0, 1000000, by = 5000)) {
  n_sim <- nrow(psa$costs)
  n_strat <- ncol(psa$costs)

  results <- list()
  for (w in wtp_range) {
    nmb <- psa$qalys * w - psa$costs
    best <- apply(nmb, 1, which.max)
    probs <- tabulate(best, nbins = n_strat) / n_sim

    for (j in 1:n_strat) {
      results[[length(results) + 1]] <- tibble::tibble(
        wtp = w,
        strategy = psa$strategies[j],
        strategy_name = psa$strategy_names[j],
        prob_ce = probs[j]
      )
    }
  }
  bind_rows(results)
}

#' Calculate EVPI from PSA results
#' @param psa PSA output
#' @param wtp WTP threshold
#' @return EVPI value (per person)
calculate_evpi <- function(psa, wtp = 234859) {
  nmb <- psa$qalys * wtp - psa$costs

  # E[max NMB]: average of per-iteration maximum
  e_max_nmb <- mean(apply(nmb, 1, max))

  # max E[NMB]: maximum of strategy averages
  max_e_nmb <- max(colMeans(nmb))

  # EVPI = E[max NMB] - max E[NMB]
  evpi <- e_max_nmb - max_e_nmb
  evpi
}

#' PSA summary statistics
psa_summary <- function(psa, wtp = 234859) {
  n_strat <- ncol(psa$costs)

  tibble::tibble(
    strategy = psa$strategies,
    strategy_name = psa$strategy_names,
    mean_cost = colMeans(psa$costs),
    sd_cost = apply(psa$costs, 2, sd),
    ci_cost_low = apply(psa$costs, 2, quantile, 0.025),
    ci_cost_high = apply(psa$costs, 2, quantile, 0.975),
    mean_qaly = colMeans(psa$qalys),
    sd_qaly = apply(psa$qalys, 2, sd),
    ci_qaly_low = apply(psa$qalys, 2, quantile, 0.025),
    ci_qaly_high = apply(psa$qalys, 2, quantile, 0.975),
    mean_nmb = colMeans(psa$qalys * wtp - psa$costs),
    prob_ce = {
      nmb <- psa$qalys * wtp - psa$costs
      best <- apply(nmb, 1, which.max)
      tabulate(best, nbins = n_strat) / nrow(psa$costs)
    }
  )
}
