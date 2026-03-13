# cea_functions.R — ICER, dominance, NMB calculations
# LTBI Screening CEA Shiny Application

#' Calculate ICERs with dominance checks
#' @param summary_df tibble with strategy, cost_total_per_person, qaly_per_person
#' @return tibble with incremental analysis
calculate_icer <- function(summary_df) {
  df <- summary_df %>%
    arrange(cost_total_per_person) %>%
    mutate(
      dominated = FALSE,
      ext_dominated = FALSE,
      inc_cost = NA_real_,
      inc_qaly = NA_real_,
      icer = NA_real_
    )

  # Strong dominance: higher cost AND lower/equal QALY
  for (i in 2:nrow(df)) {
    if (any(df$cost_total_per_person[1:(i-1)] <= df$cost_total_per_person[i] &
            df$qaly_per_person[1:(i-1)] >= df$qaly_per_person[i])) {
      df$dominated[i] <- TRUE
    }
  }

  # Remove dominated, calculate incremental on frontier
  frontier <- df %>% filter(!dominated) %>% arrange(qaly_per_person)

  if (nrow(frontier) > 1) {
    # Extended dominance check
    repeat {
      changed <- FALSE
      for (i in 2:(nrow(frontier) - 1)) {
        if (nrow(frontier) < 3) break
        # Check if strategy i is extended dominated
        icer_to_prev <- (frontier$cost_total_per_person[i] - frontier$cost_total_per_person[i-1]) /
                         (frontier$qaly_per_person[i] - frontier$qaly_per_person[i-1])
        icer_next_to_prev <- (frontier$cost_total_per_person[i+1] - frontier$cost_total_per_person[i-1]) /
                              (frontier$qaly_per_person[i+1] - frontier$qaly_per_person[i-1])
        if (!is.na(icer_to_prev) && !is.na(icer_next_to_prev) &&
            icer_to_prev > icer_next_to_prev) {
          df$ext_dominated[df$strategy == frontier$strategy[i]] <- TRUE
          frontier <- frontier[-i, ]
          changed <- TRUE
          break
        }
      }
      if (!changed) break
    }
  }

  # Recalculate frontier
  frontier <- df %>% filter(!dominated & !ext_dominated) %>% arrange(qaly_per_person)

  # Incremental analysis on frontier
  if (nrow(frontier) >= 1) {
    frontier$inc_cost[1] <- NA
    frontier$inc_qaly[1] <- NA
    frontier$icer[1] <- NA  # reference
  }
  if (nrow(frontier) >= 2) {
    for (i in 2:nrow(frontier)) {
      frontier$inc_cost[i] <- frontier$cost_total_per_person[i] - frontier$cost_total_per_person[i-1]
      frontier$inc_qaly[i] <- frontier$qaly_per_person[i] - frontier$qaly_per_person[i-1]
      if (frontier$inc_qaly[i] > 0) {
        frontier$icer[i] <- frontier$inc_cost[i] / frontier$inc_qaly[i]
      } else {
        frontier$icer[i] <- Inf
      }
    }
  }

  # Merge back
  df <- df %>%
    select(-inc_cost, -inc_qaly, -icer) %>%
    left_join(
      frontier %>% select(strategy, inc_cost, inc_qaly, icer),
      by = "strategy"
    ) %>%
    mutate(
      status = case_when(
        dominated ~ "Dominated",
        ext_dominated ~ "Ext. Dominated",
        !is.na(icer) & icer < 0 ~ "Dominant",
        TRUE ~ "On Frontier"
      )
    )

  df
}

#' Calculate Net Monetary Benefit
#' @param icer_df Output from calculate_icer
#' @param wtp Willingness-to-pay threshold (INR/QALY)
#' @return tibble with NMB added
calculate_nmb <- function(icer_df, wtp = 234859) {
  icer_df %>%
    mutate(nmb = qaly_per_person * wtp - cost_total_per_person)
}

#' Calculate NMB at multiple WTP thresholds
calculate_nmb_multiple <- function(icer_df, wtp_vec = c(100000, 234859, 500000, 704577)) {
  results <- lapply(wtp_vec, function(w) {
    icer_df %>%
      mutate(
        wtp = w,
        nmb = qaly_per_person * w - cost_total_per_person
      )
  })
  bind_rows(results)
}

#' Identify optimal strategy at each WTP
optimal_at_wtp <- function(icer_df, wtp_range = seq(0, 1000000, by = 10000)) {
  results <- tibble(
    wtp = wtp_range,
    optimal = NA_character_,
    max_nmb = NA_real_
  )
  for (i in seq_along(wtp_range)) {
    w <- wtp_range[i]
    nmb_vec <- icer_df$qaly_per_person * w - icer_df$cost_total_per_person
    best_idx <- which.max(nmb_vec)
    results$optimal[i] <- icer_df$strategy_name[best_idx]
    results$max_nmb[i] <- nmb_vec[best_idx]
  }
  results
}

#' Generate interpretation text
interpret_cea <- function(icer_df, wtp = 234859) {
  nmb_df <- calculate_nmb(icer_df, wtp)
  best <- nmb_df %>% slice_max(nmb, n = 1)
  dominated <- icer_df %>% filter(status == "Dominated")
  frontier_df <- icer_df %>% filter(status %in% c("On Frontier", "Dominant"))

  text <- paste0(
    "At a willingness-to-pay threshold of ", fmt_inr(wtp), "/QALY ",
    "(", round(wtp / 234859, 1), "x India GDP per capita), ",
    "the optimal strategy is **", best$strategy_name[1], "** ",
    "with a net monetary benefit of ", fmt_inr(best$nmb[1]), " per person.\n\n"
  )

  if (nrow(dominated) > 0) {
    text <- paste0(text,
      "**Dominated strategies** (higher cost, same or lower QALYs): ",
      paste(dominated$strategy_name, collapse = ", "), ".\n\n"
    )
  }

  if (nrow(frontier_df) > 1) {
    text <- paste0(text,
      "**Cost-effectiveness frontier** includes: ",
      paste(frontier_df$strategy_name, collapse = " → "), ".\n\n"
    )
  }

  # TB cases comparison
  tb_min <- icer_df %>% slice_min(tb_cases_per_1000, n = 1)
  tb_max <- icer_df %>% slice_max(tb_cases_per_1000, n = 1)
  text <- paste0(text,
    "TB burden ranges from ", round(tb_min$tb_cases_per_1000[1], 1),
    " cases/1000 (", tb_min$strategy_name[1], ") to ",
    round(tb_max$tb_cases_per_1000[1], 1),
    " cases/1000 (", tb_max$strategy_name[1], ")."
  )

  text
}
