# helpers.R — Utility functions for LTBI CEA Shiny App

#' Format INR currency
fmt_inr <- function(x, digits = 0) {
  ifelse(is.na(x), "-",
         paste0("\u20B9", formatC(round(x, digits), format = "f",
                                  digits = digits, big.mark = ",")))
}

#' Format percentage
fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "-", paste0(round(x * 100, digits), "%"))
}

#' Format ICER
fmt_icer <- function(x) {
  ifelse(is.na(x), "-",
         ifelse(x < 0, "Dominant",
                ifelse(is.infinite(x), "Dominated",
                       fmt_inr(x))))
}

#' Validate parameter is within bounds
validate_param <- function(value, low, high, name = "") {
  if (is.na(value)) return(list(valid = FALSE, msg = paste(name, "is missing")))
  if (value < low) return(list(valid = FALSE, msg = paste(name, "below lower bound")))
  if (value > high) return(list(valid = FALSE, msg = paste(name, "above upper bound")))
  list(valid = TRUE, msg = "")
}

#' Convert annual rate to cycle probability (3-month cycles)
rate_to_prob <- function(annual_rate, cycle_length = 0.25) {
  1 - exp(-annual_rate * cycle_length)
}

#' Convert probability to rate
prob_to_rate <- function(p, t = 1) {
  -log(1 - p) / t
}

#' Apply half-cycle correction
apply_hcc <- function(trace_matrix) {
  n_cycles <- nrow(trace_matrix)
  corrected <- trace_matrix
  corrected[1, ] <- trace_matrix[1, ] / 2
  corrected[n_cycles, ] <- trace_matrix[n_cycles, ] / 2
  corrected
}

#' Discount vector
discount_vector <- function(n_cycles, rate = 0.03, cycle_length = 0.25) {
  years <- (0:(n_cycles - 1)) * cycle_length
  1 / (1 + rate)^years
}

#' Sample from Beta distribution (method of moments)
sample_beta <- function(n, mean_val, low, high) {
  if (mean_val <= 0) return(rep(0, n))
  if (mean_val >= 1) return(rep(1, n))
  se <- (high - low) / (2 * 1.96)
  se <- max(se, 0.001)
  var_val <- se^2
  var_val <- min(var_val, mean_val * (1 - mean_val) * 0.99)
  alpha <- mean_val * (mean_val * (1 - mean_val) / var_val - 1)
  beta_par <- (1 - mean_val) * (mean_val * (1 - mean_val) / var_val - 1)
  alpha <- max(alpha, 0.1)
  beta_par <- max(beta_par, 0.1)
  pmin(pmax(rbeta(n, alpha, beta_par), 0), 1)
}

#' Sample from Gamma distribution (method of moments)
sample_gamma <- function(n, mean_val, low, high) {
  if (mean_val <= 0) return(rep(0, n))
  se <- (high - low) / (2 * 1.96)
  se <- max(se, mean_val * 0.01)
  var_val <- se^2
  shape <- mean_val^2 / var_val
  rate <- mean_val / var_val
  rgamma(n, shape = max(shape, 0.1), rate = max(rate, 0.001))
}

#' Sample from Log-normal distribution
sample_lnorm <- function(n, mean_val, low, high) {
  if (mean_val <= 0) return(rep(0.001, n))
  se <- (high - low) / (2 * 1.96)
  se <- max(se, mean_val * 0.01)
  mu_ln <- log(mean_val^2 / sqrt(se^2 + mean_val^2))
  sigma_ln <- sqrt(log(1 + se^2 / mean_val^2))
  rlnorm(n, meanlog = mu_ln, sdlog = max(sigma_ln, 0.01))
}

#' Strategy names and descriptions
strategy_info <- function() {
  tibble::tibble(
    id = c("TST", "IGRA", "CyTb", "Sequential", "TreatAll"),
    name = c("TST Alone", "IGRA Alone", "Cy-Tb Alone",
             "TST \u2192 IGRA", "Treat All"),
    test = c("TST (1 TU)", "QFT-Plus", "Cy-Tb",
             "TST then IGRA", "None"),
    rule = c("Treat if TST \u2265 5mm",
             "Treat if IGRA +",
             "Treat if Cy-Tb \u2265 5mm",
             "Treat if TST+ confirmed by IGRA+",
             "Treat everyone"),
    color = c("#2E5090", "#4472C4", "#70AD47", "#ED7D31", "#FF4444")
  )
}
