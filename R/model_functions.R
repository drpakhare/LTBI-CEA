# model_functions.R — Decision Tree + Markov Model Engine
# LTBI Screening CEA Shiny Application

# ══════════════════════════════════════════════════════════════════════════════
# DECISION TREE ENGINE
# ══════════════════════════════════════════════════════════════════════════════

#' Get test parameters for a given strategy
get_test_params <- function(p, strategy) {
  switch(strategy,
    TST = list(
      se = p[["Se_TST_5mm_IS"]],
      sp = p[["Sp_TST_5mm_BCG"]],
      cost = p[["c_TST"]],
      name = "TST"
    ),
    IGRA = list(
      se = p[["Se_IGRA_rheum"]],
      sp = p[["Sp_IGRA_rheum"]],
      cost = p[["c_IGRA"]],
      name = "IGRA"
    ),
    CyTb = list(
      se = p[["Se_CyTb"]],
      sp = p[["Sp_CyTb_BCG"]],
      cost = p[["c_CyTb"]],
      name = "Cy-Tb"
    ),
    Sequential = list(
      se_tst = p[["Se_TST_5mm_IS"]],
      sp_tst = p[["Sp_TST_5mm_BCG"]],
      se_igra = p[["Se_IGRA_rheum"]],
      sp_igra = p[["Sp_IGRA_rheum"]],
      cost_tst = p[["c_TST"]],
      cost_igra = p[["c_IGRA"]],
      name = "TST->IGRA"
    ),
    TreatAll = list(
      se = 1, sp = 0, cost = 0, name = "Treat All"
    )
  )
}

#' Run decision tree for a single strategy
#' @param p Named vector of parameter values
#' @param strategy One of: "TST", "IGRA", "CyTb", "Sequential", "TreatAll"
#' @return tibble with screening outcomes
run_dt_single <- function(p, strategy) {
  prev <- p[["p_LTBI_RA_combined"]]
  c_proph <- p[["c_INH_6mo"]]
  c_lft <- p[["c_LFT_monitoring"]]
  c_cxr <- p[["c_CXR"]]
  p_hepatox <- p[["p_hepatox_INH_TNFi"]]
  c_hepatox <- p[["c_hepatox_mild"]]
  p_severe_hepatox <- p[["p_hepatox_fatal"]]  # proportion severe among hepatox
  c_severe <- p[["c_hepatox_severe"]]

  if (strategy == "Sequential") {
    tp <- get_test_params(p, strategy)
    # Sequential: TST first, then IGRA for TST+ only
    # P(TST+) = prev*se_tst + (1-prev)*(1-sp_tst)
    p_tst_pos <- prev * tp$se_tst + (1 - prev) * (1 - tp$sp_tst)

    # Among TST+, do IGRA
    # P(LTBI | TST+) = prev*se_tst / p_tst_pos
    p_ltbi_given_tst_pos <- (prev * tp$se_tst) / p_tst_pos

    # P(IGRA+ | TST+) = P(LTBI|TST+)*se_igra + (1-P(LTBI|TST+))*(1-sp_igra)
    p_igra_pos_given_tst_pos <- p_ltbi_given_tst_pos * tp$se_igra +
      (1 - p_ltbi_given_tst_pos) * (1 - tp$sp_igra)

    # Final treated = P(TST+) * P(IGRA+|TST+)
    p_treated <- p_tst_pos * p_igra_pos_given_tst_pos

    # True positives = P(LTBI) * P(TST+ | LTBI) * P(IGRA+ | LTBI)
    p_tp <- prev * tp$se_tst * tp$se_igra
    # False positives = P(no LTBI) * P(TST+ | no LTBI) * P(IGRA+ | no LTBI)
    p_fp <- (1 - prev) * (1 - tp$sp_tst) * (1 - tp$sp_igra)
    p_fn <- prev - p_tp
    p_tn <- (1 - prev) - p_fp

    # Cost: everyone gets TST + CXR; TST+ also get IGRA
    cost_screen <- tp$cost_tst + c_cxr + p_tst_pos * tp$cost_igra

    se_eff <- tp$se_tst * tp$se_igra
    sp_eff <- 1 - (1 - tp$sp_tst) * (1 - tp$sp_igra)

  } else if (strategy == "TreatAll") {
    p_tp <- prev
    p_fp <- 1 - prev
    p_fn <- 0
    p_tn <- 0
    p_treated <- 1
    cost_screen <- c_cxr  # still do CXR to rule out active TB
    se_eff <- 1
    sp_eff <- 0

  } else {
    tp <- get_test_params(p, strategy)
    p_tp <- prev * tp$se
    p_fp <- (1 - prev) * (1 - tp$sp)
    p_fn <- prev * (1 - tp$se)
    p_tn <- (1 - prev) * tp$sp
    p_treated <- p_tp + p_fp
    cost_screen <- tp$cost + c_cxr
    se_eff <- tp$se
    sp_eff <- tp$sp
  }

  # Prophylaxis costs (only for those treated)
  cost_proph_per_treated <- c_proph + c_lft
  cost_ae_per_treated <- p_hepatox * (c_hepatox + p_severe_hepatox * c_severe)
  cost_total <- cost_screen + p_treated * (cost_proph_per_treated + cost_ae_per_treated)

  tibble::tibble(
    strategy = strategy,
    p_ltbi = prev,
    sensitivity = se_eff,
    specificity = sp_eff,
    p_true_positive = p_tp,
    p_false_positive = p_fp,
    p_false_negative = p_fn,
    p_true_negative = p_tn,
    p_treated = p_treated,
    p_ltbi_missed = p_fn,  # LTBI patients not treated
    p_hepatox = p_treated * p_hepatox,
    cost_screening = cost_screen,
    cost_prophylaxis = p_treated * cost_proph_per_treated,
    cost_adverse_events = p_treated * cost_ae_per_treated,
    cost_total_dt = cost_total
  )
}

#' Run decision tree for all strategies
#' @param p Named vector of parameter values
#' @return tibble with one row per strategy
run_decision_tree <- function(p) {
  strategies <- c("TST", "IGRA", "CyTb", "Sequential", "TreatAll")
  bind_rows(lapply(strategies, function(s) run_dt_single(p, s)))
}


# ══════════════════════════════════════════════════════════════════════════════
# MARKOV MODEL ENGINE
# ══════════════════════════════════════════════════════════════════════════════

#' Health state names
HEALTH_STATES <- c(
  "Well_on_biologic",       # RA controlled, on biologic, no active TB
  "LTBI_untreated",         # LTBI not identified, on biologic
  "LTBI_prophylaxis",       # On TB prophylaxis + biologic
  "Active_TB",              # Active TB (pulmonary or extrapulmonary)
  "TB_treatment",           # On TB treatment (biologic may be held)
  "Post_TB",                # Completed TB treatment, recovering
  "Dead"
)
N_STATES <- length(HEALTH_STATES)

#' Build initial cohort distribution based on decision tree results
#' @param dt_row One row from decision tree output (for a single strategy)
#' @param n_cohort Cohort size
build_initial_cohort <- function(dt_row, n_cohort = 100000) {
  cohort <- rep(0, N_STATES)
  names(cohort) <- HEALTH_STATES

  # True negatives + false positives → Well on biologic (no LTBI, correctly/incorrectly treated)
  cohort["Well_on_biologic"] <- n_cohort * (dt_row$p_true_negative + dt_row$p_false_positive)

  # True positives → LTBI on prophylaxis (correctly identified and treated)
  cohort["LTBI_prophylaxis"] <- n_cohort * dt_row$p_true_positive

  # False negatives → LTBI untreated (missed by screening)
  cohort["LTBI_untreated"] <- n_cohort * dt_row$p_false_negative

  cohort
}

#' Build transition probability matrix for one cycle
#' @param p Named parameter vector
#' @param on_prophylaxis_effective Effective prophylaxis coverage for this strategy
#' @param cycle Current cycle number (for age-dependent mortality)
build_transition_matrix <- function(p, on_prophylaxis_effective = FALSE, cycle = 1) {
  # Base rates (convert annual to 3-month cycle)
  cl <- 0.25  # cycle length in years

  # TB reactivation rate (annual) — use adalimumab as reference TNFi
  # From parameters: cumulative incidence 9.62/1000 exposed ≈ 0.96% annual
  rr_bio <- p[["RR_adalimumab"]]  # relative risk vs general population
  r_react_base <- p[["r_LTBI_react_natural"]]  # annual base rate
  r_react_bio <- r_react_base * rr_bio

  # Prophylaxis reduces reactivation
  proph_eff <- p[["OR_INH_alone"]]  # OR ≈ 0.46 (protective)
  r_react_treated <- r_react_bio * proph_eff

  # Convert to cycle probabilities
  p_react_untreated <- rate_to_prob(r_react_bio, cl)
  p_react_treated <- rate_to_prob(r_react_treated, cl)
  p_react_well <- rate_to_prob(p[["r_LTBI_react_natural"]], cl)  # very low for non-LTBI

  # Proportion of active TB that is extrapulmonary
  p_eptb <- p[["p_EPTB_biologics"]]

  # TB diagnosis probability (per cycle, active TB → treatment)
  p_dx <- 0.90  # most active TB diagnosed within one cycle (3 months)

  # TB treatment outcomes (per cycle)
  p_tx_success <- rate_to_prob(-log(1 - p[["p_treatment_success"]]) / 8, cl)  # 6-month treatment over ~8 cycles
  p_tb_death <- rate_to_prob(-log(1 - p[["CFR_DS_TB"]]) / 8, cl)

  # Background mortality (annual, adjusted for RA)
  age_start <- 45  # typical RA patient starting biologics
  age_current <- age_start + cycle * cl
  # Simplified age-specific mortality (India life table approximation)
  r_mort_base <- ifelse(age_current < 50, 0.005,
                  ifelse(age_current < 60, 0.010,
                  ifelse(age_current < 70, 0.025,
                  ifelse(age_current < 80, 0.060, 0.150))))
  smr_ra <- p[["SMR_RA"]]
  p_mort <- rate_to_prob(r_mort_base * smr_ra, cl)

  # Prophylaxis completion: after ~6 months (8 cycles), move to Well
  p_proph_complete <- ifelse(cycle <= 8, 0, 1)  # simplified: complete at 6 months

  # Build 7×7 transition matrix
  # States: Well, LTBI_untreated, LTBI_prophylaxis, Active_TB, TB_treatment, Post_TB, Dead
  m <- matrix(0, nrow = N_STATES, ncol = N_STATES)
  rownames(m) <- colnames(m) <- HEALTH_STATES

  # 1. Well_on_biologic
  m["Well_on_biologic", "Dead"] <- p_mort
  m["Well_on_biologic", "Well_on_biologic"] <- 1 - p_mort

  # 2. LTBI_untreated (missed by screening, on biologic)
  p_react <- p_react_untreated
  m["LTBI_untreated", "Active_TB"] <- p_react * (1 - p_mort)
  m["LTBI_untreated", "Dead"] <- p_mort
  m["LTBI_untreated", "LTBI_untreated"] <- 1 - p_react * (1 - p_mort) - p_mort

  # 3. LTBI_prophylaxis (correctly identified, on treatment)
  p_react_p <- p_react_treated
  if (cycle <= 8) {
    # Still on prophylaxis
    m["LTBI_prophylaxis", "Active_TB"] <- p_react_p * (1 - p_mort)
    m["LTBI_prophylaxis", "Dead"] <- p_mort
    m["LTBI_prophylaxis", "LTBI_prophylaxis"] <- 1 - p_react_p * (1 - p_mort) - p_mort
  } else {
    # Prophylaxis completed → move to Well (with residual protection)
    m["LTBI_prophylaxis", "Active_TB"] <- p_react_p * (1 - p_mort)
    m["LTBI_prophylaxis", "Dead"] <- p_mort
    m["LTBI_prophylaxis", "Well_on_biologic"] <- (1 - p_react_p) * (1 - p_mort)
    m["LTBI_prophylaxis", "LTBI_prophylaxis"] <- 0
  }

  # 4. Active_TB (diagnosed within cycle → treatment)
  m["Active_TB", "TB_treatment"] <- p_dx * (1 - p_mort * 1.5)
  m["Active_TB", "Dead"] <- p_mort * 1.5  # elevated mortality
  m["Active_TB", "Active_TB"] <- 1 - p_dx * (1 - p_mort * 1.5) - p_mort * 1.5

  # 5. TB_treatment
  m["TB_treatment", "Post_TB"] <- p_tx_success
  m["TB_treatment", "Dead"] <- p_tb_death
  m["TB_treatment", "TB_treatment"] <- 1 - p_tx_success - p_tb_death

  # 6. Post_TB (recovered, may resume biologic)
  m["Post_TB", "Dead"] <- p_mort
  m["Post_TB", "Post_TB"] <- 1 - p_mort

  # 7. Dead (absorbing)
  m["Dead", "Dead"] <- 1

  # Ensure rows sum to 1 (correct rounding)
  for (i in 1:N_STATES) {
    row_sum <- sum(m[i, ])
    if (row_sum > 0 && abs(row_sum - 1) > 1e-10) {
      # Adjust the diagonal (staying in same state)
      m[i, i] <- m[i, i] + (1 - row_sum)
    }
    # Ensure no negative probabilities
    m[i, ] <- pmax(m[i, ], 0)
    m[i, ] <- m[i, ] / sum(m[i, ])
  }

  m
}

#' Cost and utility vectors for each health state (per cycle)
state_costs <- function(p) {
  cl <- 0.25
  c(
    Well_on_biologic = p[["c_adalimumab_mo"]] * 3,  # 3 months biologic
    LTBI_untreated = p[["c_adalimumab_mo"]] * 3,
    LTBI_prophylaxis = p[["c_adalimumab_mo"]] * 3 + p[["c_INH_6mo"]] / 8,  # amortized prophylaxis
    Active_TB = p[["c_TB_total"]] / 4,  # treatment costs spread
    TB_treatment = p[["c_TB_total"]] / 8,  # during 6-month treatment
    Post_TB = p[["c_adalimumab_mo"]] * 3 * 0.8,  # reduced biologic use post-TB
    Dead = 0
  )
}

state_utilities <- function(p) {
  cl <- 0.25
  c(
    Well_on_biologic = p[["u_RA_controlled"]] * cl,
    LTBI_untreated = p[["u_RA_controlled"]] * cl,  # asymptomatic LTBI
    LTBI_prophylaxis = (p[["u_RA_controlled"]] + p[["du_prophylaxis"]]) * cl,
    Active_TB = p[["u_active_PTB"]] * cl,
    TB_treatment = p[["u_TB_treatment"]] * cl,
    Post_TB = p[["u_post_TB"]] * cl,
    Dead = 0
  )
}

#' Run Markov model for a single strategy
#' @param p Named parameter vector
#' @param dt_row One row from decision tree (for initial cohort)
#' @param settings MODEL_SETTINGS list
#' @return list with trace, costs, qalys
run_markov_single <- function(p, dt_row, settings) {
  n_cycles <- settings$n_cycles
  n_cohort <- settings$n_cohort
  discount_rate <- settings$discount_rate
  cl <- settings$cycle_length

  # Initial cohort
  cohort <- build_initial_cohort(dt_row, n_cohort)

  # State costs and utilities
  s_costs <- state_costs(p)
  s_utils <- state_utilities(p)

  # Trace matrix (rows = cycles, cols = states)
  trace <- matrix(0, nrow = n_cycles + 1, ncol = N_STATES)
  colnames(trace) <- HEALTH_STATES
  trace[1, ] <- cohort

  # Per-cycle costs and QALYs
  cycle_costs <- numeric(n_cycles + 1)
  cycle_qalys <- numeric(n_cycles + 1)

  # Cycle 0 costs (from decision tree)
  cycle_costs[1] <- dt_row$cost_total_dt * n_cohort
  cycle_qalys[1] <- sum(cohort * s_utils)

  # Run Markov cycles
  for (t in 1:n_cycles) {
    tm <- build_transition_matrix(p, cycle = t)
    trace[t + 1, ] <- trace[t, ] %*% tm

    # Costs and QALYs for this cycle
    cycle_costs[t + 1] <- sum(trace[t + 1, ] * s_costs)
    cycle_qalys[t + 1] <- sum(trace[t + 1, ] * s_utils)
  }

  # Discount factors
  disc <- discount_vector(n_cycles + 1, discount_rate, cl)

  # Apply half-cycle correction
  hcc_costs <- cycle_costs
  hcc_costs[1] <- hcc_costs[1] / 2
  hcc_costs[n_cycles + 1] <- hcc_costs[n_cycles + 1] / 2

  hcc_qalys <- cycle_qalys
  hcc_qalys[1] <- hcc_qalys[1] / 2
  hcc_qalys[n_cycles + 1] <- hcc_qalys[n_cycles + 1] / 2

  # Total discounted
  total_cost <- sum(hcc_costs * disc)
  total_qaly <- sum(hcc_qalys * disc)

  # TB cases
  tb_cases <- sum(diff(trace[, "Active_TB"]) > 0) # rough count of new TB entries
  # More precise: count inflows to Active_TB
  tb_inflow <- numeric(n_cycles)
  for (t in 1:n_cycles) {
    tm <- build_transition_matrix(p, cycle = t)
    # Inflow to Active_TB from LTBI states
    tb_inflow[t] <- trace[t, "LTBI_untreated"] * tm["LTBI_untreated", "Active_TB"] +
                     trace[t, "LTBI_prophylaxis"] * tm["LTBI_prophylaxis", "Active_TB"]
  }
  total_tb_cases <- sum(tb_inflow)

  list(
    trace = trace,
    cycle_costs = cycle_costs,
    cycle_qalys = cycle_qalys,
    disc_cycle_costs = hcc_costs * disc,
    disc_cycle_qalys = hcc_qalys * disc,
    total_cost = total_cost,
    total_qaly = total_qaly,
    total_cost_per_person = total_cost / n_cohort,
    total_qaly_per_person = total_qaly / n_cohort,
    total_tb_cases = total_tb_cases,
    tb_cases_per_1000 = total_tb_cases / n_cohort * 1000,
    strategy = dt_row$strategy
  )
}

#' Run full model (decision tree + Markov) for all strategies
#' @param p Named parameter vector
#' @param settings MODEL_SETTINGS list
#' @return list with dt_results, markov_results, summary
run_full_model <- function(p, settings = MODEL_SETTINGS) {
  # Decision tree
  dt <- run_decision_tree(p)

  # Markov for each strategy
  markov <- lapply(1:nrow(dt), function(i) {
    run_markov_single(p, dt[i, ], settings)
  })
  names(markov) <- dt$strategy

  # Summary table
  summary_df <- tibble::tibble(
    strategy = dt$strategy,
    strategy_name = settings$strategy_names,
    cost_screening = sapply(1:nrow(dt), function(i) dt$cost_screening[i]),
    cost_total_per_person = sapply(markov, function(m) m$total_cost_per_person),
    qaly_per_person = sapply(markov, function(m) m$total_qaly_per_person),
    tb_cases_per_1000 = sapply(markov, function(m) m$tb_cases_per_1000),
    p_treated = dt$p_treated,
    p_ltbi_missed = dt$p_ltbi_missed
  )

  list(
    dt_results = dt,
    markov_results = markov,
    summary = summary_df
  )
}
