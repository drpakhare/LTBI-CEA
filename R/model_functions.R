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
  rr_bio <- p[["RR_adalimumab"]]  # relative risk vs general population
  r_react_base <- p[["r_LTBI_react_natural"]]  # annual base rate
  r_react_bio <- r_react_base * rr_bio

  # Prophylaxis reduces reactivation (OR ≈ 0.46, protective)
  proph_eff <- p[["OR_INH_alone"]]
  r_react_treated <- r_react_bio * proph_eff

  # Post-prophylaxis residual risk: prophylaxis protection wanes over time

  # Literature: INH protection wanes ~50% after completion
  # Residual rate = midpoint between treated and untreated rates
  r_react_post_proph <- r_react_bio * (1 - (1 - proph_eff) * 0.5)

  # Convert to cycle probabilities
  p_react_untreated <- rate_to_prob(r_react_bio, cl)
  p_react_treated <- rate_to_prob(r_react_treated, cl)
  p_react_post_proph <- rate_to_prob(r_react_post_proph, cl)

  # TB diagnosis probability (per cycle, active TB → treatment)
  p_dx <- 0.90  # most active TB diagnosed within one cycle (3 months)

  # TB treatment outcomes (per cycle)
  p_tx_success <- rate_to_prob(-log(1 - p[["p_treatment_success"]]) / 8, cl)
  p_tb_death <- rate_to_prob(-log(1 - p[["CFR_DS_TB"]]) / 8, cl)

  # TB relapse rate: ~5% within 2 years post-treatment (annual ~2.5%)
  r_tb_relapse <- 0.025
  p_tb_relapse <- rate_to_prob(r_tb_relapse, cl)

  # Post-TB recovery: resume biologic after ~1 year stabilisation
  # ~25% per cycle after first year post-treatment = gradual return to Well
  p_post_tb_recovery <- 0.0

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

  # Build 7×7 transition matrix
  # States: Well, LTBI_untreated, LTBI_prophylaxis, Active_TB, TB_treatment, Post_TB, Dead
  m <- matrix(0, nrow = N_STATES, ncol = N_STATES)
  rownames(m) <- colnames(m) <- HEALTH_STATES

  # 1. Well_on_biologic — no LTBI, stable on biologic
  m["Well_on_biologic", "Dead"] <- p_mort
  m["Well_on_biologic", "Well_on_biologic"] <- 1 - p_mort

  # 2. LTBI_untreated (missed by screening, on biologic)
  #    Full reactivation risk every cycle — this is the primary penalty for missed LTBI
  m["LTBI_untreated", "Active_TB"] <- p_react_untreated * (1 - p_mort)
  m["LTBI_untreated", "Dead"] <- p_mort
  m["LTBI_untreated", "LTBI_untreated"] <- 1 - p_react_untreated * (1 - p_mort) - p_mort

  # 3. LTBI_prophylaxis (correctly identified, on/completed treatment)
  #    FIX: After prophylaxis completion, patients REMAIN in this state with
  #    RESIDUAL reactivation risk (waning protection), NOT moved to Well.
  #    This corrects the prior "prophylaxis = cure" assumption.
  if (cycle <= 8) {
    # During active prophylaxis: reduced reactivation (OR × base rate)
    p_react_p <- p_react_treated
    m["LTBI_prophylaxis", "Active_TB"] <- p_react_p * (1 - p_mort)
    m["LTBI_prophylaxis", "Dead"] <- p_mort
    m["LTBI_prophylaxis", "LTBI_prophylaxis"] <- 1 - p_react_p * (1 - p_mort) - p_mort
  } else {
    # Post-prophylaxis: residual risk (waned protection)
    # Patients stay in LTBI_prophylaxis with ongoing but reduced TB risk
    p_react_resid <- p_react_post_proph
    m["LTBI_prophylaxis", "Active_TB"] <- p_react_resid * (1 - p_mort)
    m["LTBI_prophylaxis", "Dead"] <- p_mort
    m["LTBI_prophylaxis", "LTBI_prophylaxis"] <- 1 - p_react_resid * (1 - p_mort) - p_mort
  }

  # 4. Active_TB (diagnosed within cycle → treatment)
  #    Biologic is HELD during active TB → RA flare (captured in utility/cost)
  #    Elevated mortality (1.5× background)
  p_mort_tb <- min(p_mort * 1.5, 0.95)  # cap to avoid impossible probabilities
  m["Active_TB", "TB_treatment"] <- p_dx * (1 - p_mort_tb)
  m["Active_TB", "Dead"] <- p_mort_tb
  m["Active_TB", "Active_TB"] <- 1 - p_dx * (1 - p_mort_tb) - p_mort_tb

  # 5. TB_treatment — biologic still held, RA not well controlled
  m["TB_treatment", "Post_TB"] <- p_tx_success
  m["TB_treatment", "Dead"] <- p_tb_death
  m["TB_treatment", "TB_treatment"] <- 1 - p_tx_success - p_tb_death

  # 6. Post_TB (completed TB treatment, recovering)
  #    FIX: Can relapse to Active_TB (~5% within 2 years)
  #    Can gradually recover to Well_on_biologic (resume biologics)
  m["Post_TB", "Active_TB"] <- p_tb_relapse * (1 - p_mort)
  m["Post_TB", "Well_on_biologic"] <- p_post_tb_recovery * (1 - p_mort - p_tb_relapse * (1 - p_mort))
  m["Post_TB", "Dead"] <- p_mort
  m["Post_TB", "Post_TB"] <- 1 - m["Post_TB", "Active_TB"] - m["Post_TB", "Well_on_biologic"] - p_mort

  # 7. Dead (absorbing)
  m["Dead", "Dead"] <- 1

  # Ensure rows sum to 1 (correct rounding)
  for (i in 1:N_STATES) {
    row_sum <- sum(m[i, ])
    if (row_sum > 0 && abs(row_sum - 1) > 1e-10) {
      m[i, i] <- m[i, i] + (1 - row_sum)
    }
    # Ensure no negative probabilities
    m[i, ] <- pmax(m[i, ], 0)
    m[i, ] <- m[i, ] / sum(m[i, ])
  }

  m
}

#' Cost and utility vectors for each health state (per cycle)
#' KEY FIX: During Active_TB and TB_treatment, biologic is HELD (withheld).
#' This means: (1) No biologic cost, (2) RA flare management cost added,
#' (3) Utility reflects BOTH TB burden AND uncontrolled RA from biologic interruption.
state_costs <- function(p) {
  cl <- 0.25
  c_bio_quarter <- p[["c_adalimumab_mo"]] * 3  # quarterly biologic cost

  # RA flare management during biologic hold: conventional DMARDs + steroids

  # + extra clinic visits (~INR 5,000/quarter; conservative Indian estimate)
  c_ra_flare_mgmt <- 5000

  c(
    Well_on_biologic = c_bio_quarter,                                  # stable on biologic
    LTBI_untreated = c_bio_quarter,                                    # asymptomatic LTBI, on biologic
    LTBI_prophylaxis = c_bio_quarter + p[["c_INH_6mo"]] / 8,         # biologic + amortized INH
    Active_TB = c_ra_flare_mgmt + p[["c_TB_total"]] / 4,             # NO biologic (held) + TB dx + RA flare mgmt
    TB_treatment = c_ra_flare_mgmt + p[["c_TB_total"]] / 8,          # NO biologic (held) + TB Rx + RA flare mgmt
    Post_TB = c_bio_quarter * 0.8,                                     # biologic restarted (cautious dose)
    Dead = 0
  )
}

state_utilities <- function(p) {
  cl <- 0.25

  # RA flare disutility when biologic is held during TB:
  # Literature: RA flare reduces utility by 0.15-0.20 (Stevenson 2016, Wailoo 2006)
  # Applied as additive decrement to TB-state utility, reflecting compound burden
  du_ra_flare <- -0.15

  c(
    Well_on_biologic = p[["u_RA_controlled"]] * cl,
    LTBI_untreated = p[["u_RA_controlled"]] * cl,                              # asymptomatic LTBI
    LTBI_prophylaxis = (p[["u_RA_controlled"]] + p[["du_prophylaxis"]]) * cl,
    Active_TB = (p[["u_active_PTB"]] + du_ra_flare) * cl,                      # TB + RA flare (biologic held)
    TB_treatment = (p[["u_TB_treatment"]] + du_ra_flare) * cl,                 # TB Rx + RA flare (biologic held)
    Post_TB = p[["u_post_TB"]] * cl,                                           # recovering, biologic restarting
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
    # Inflow to Active_TB from LTBI states + Post_TB relapses
    tb_inflow[t] <- trace[t, "LTBI_untreated"] * tm["LTBI_untreated", "Active_TB"] +
                     trace[t, "LTBI_prophylaxis"] * tm["LTBI_prophylaxis", "Active_TB"] +
                     trace[t, "Post_TB"] * tm["Post_TB", "Active_TB"]
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

# ══════════════════════════════════════════════════════════════════════════════
# SHORT-TERM (DECISION TREE) COST-EFFECTIVENESS
# ══════════════════════════════════════════════════════════════════════════════

#' Compute short-term CEA metrics from decision tree outputs
#' These are the PRIMARY analysis: no long-term projection needed.
#' @param dt Decision tree results (from run_decision_tree)
#' @param p Parameter vector
#' @param settings MODEL_SETTINGS
#' @return tibble with per-strategy short-term CEA metrics
compute_dt_cea <- function(dt, p, settings) {
  # Annual TB reactivation rate on biologics (from parameters)
  r_react_bio <- p[["r_LTBI_react_natural"]] * p[["RR_adalimumab"]]
  proph_eff <- p[["OR_INH_alone"]]

  # Expected TB cases per 1000 over different horizons (no Markov needed)
  # Using simple probability calculation: P(TB in t years) = 1 - exp(-rate * t)
  horizons <- c(1, 2, 5)  # years
  n_cohort <- settings$n_cohort

  dt_cea <- dt %>%
    mutate(
      strategy_name = settings$strategy_names[match(strategy, settings$strategies)],

      # Key screening performance metrics
      ltbi_detected_per_1000 = p_true_positive * 1000,
      ltbi_missed_per_1000 = p_false_negative * 1000,
      unnecessarily_treated_per_1000 = p_false_positive * 1000,

      # Cost per LTBI detected (among those correctly identified)
      cost_per_ltbi_detected = ifelse(p_true_positive > 0,
                                       cost_total_dt / p_true_positive, NA),

      # Expected TB without any screening (all LTBI untreated)
      # Annual rate on biologics
      tb_risk_untreated_1y = 1 - exp(-r_react_bio * 1),
      tb_risk_untreated_5y = 1 - exp(-r_react_bio * 5),

      # TB risk for treated LTBI (prophylaxis reduces rate)
      tb_risk_treated_1y = 1 - exp(-r_react_bio * proph_eff * 1),
      tb_risk_treated_5y = 1 - exp(-r_react_bio * proph_eff * 5),

      # Expected TB cases per 1000 at 5 years:
      # Missed LTBI (FN) reactivate at full rate
      # Detected LTBI (TP) reactivate at treated rate
      # FP and TN: no LTBI, no TB risk from reactivation
      expected_tb_5y_per_1000 = (p_false_negative * tb_risk_untreated_5y +
                                  p_true_positive * tb_risk_treated_5y) * 1000,
      expected_tb_1y_per_1000 = (p_false_negative * tb_risk_untreated_1y +
                                  p_true_positive * tb_risk_treated_1y) * 1000,

      # TB cases averted vs no-screening baseline
      # No screening = all LTBI reactivate at untreated rate
      tb_no_screening_5y = p[["p_LTBI_RA_combined"]] * tb_risk_untreated_5y * 1000,
      tb_averted_5y_per_1000 = tb_no_screening_5y - expected_tb_5y_per_1000,

      # Cost per TB case averted (vs no screening) over 5 years
      cost_per_tb_averted = ifelse(tb_averted_5y_per_1000 > 0,
                                    cost_total_dt / (tb_averted_5y_per_1000 / 1000), NA),

      # NNS to detect one LTBI
      nns_detect_ltbi = ifelse(p_true_positive > 0,
                                1 / p_true_positive, NA),

      # NNT to prevent one TB case (5-year horizon)
      nnt_prevent_tb = ifelse(tb_averted_5y_per_1000 > 0,
                               p_treated / (tb_averted_5y_per_1000 / 1000), NA)
    )

  dt_cea
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

  # Short-term CEA from decision tree (PRIMARY analysis)
  dt_cea <- compute_dt_cea(dt, p, settings)

  list(
    dt_results = dt,
    dt_cea = dt_cea,
    markov_results = markov,
    summary = summary_df
  )
}
