# global.R — Load packages, read parameters, set up defaults
# LTBI Screening CEA Shiny Application

# ── Packages ──────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyWidgets)
  library(DT)
  library(plotly)
  library(ggplot2)
  library(DiagrammeR)
  library(readxl)
  library(writexl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(scales)
  library(future)
  library(furrr)
  library(htmltools)
  library(officer)
  library(flextable)
  library(ggrepel)
})

# ── Source R files ────────────────────────────────────────────────────────────
source("R/helpers.R")
source("R/model_functions.R")
source("R/cea_functions.R")
source("R/sa_functions.R")
source("R/diagram_functions.R")
source("R/mod_overview.R")
source("R/mod_parameters.R")
source("R/mod_decision_tree.R")
source("R/mod_markov.R")
source("R/mod_cea.R")
source("R/mod_dsa.R")
source("R/mod_psa.R")
source("R/report_functions.R")
source("R/mod_report.R")

# ── Parallel processing setup ─────────────────────────────────────────────────
plan(multisession, workers = max(1, availableCores() - 1))

# ── Read parameter Excel workbook ─────────────────────────────────────────────
EXCEL_PATH <- "data/LTBI_Model_Parameters.xlsx"

read_params_from_excel <- function(path = EXCEL_PATH) {
  sheets <- c("1. Prevalence", "2. Test Accuracy", "3. TB Reactivation",
              "4. Prophylaxis", "5. Costs (INR)", "6. Utilities",
              "7. Mortality & Natural Hx", "8. Data Gaps & Notes")

  raw <- lapply(sheets[1:7], function(s) {
    tryCatch(read_excel(path, sheet = s), error = function(e) NULL)
  })
  names(raw) <- c("prevalence", "test_accuracy", "reactivation",
                   "prophylaxis", "costs", "utilities", "mortality")

  # Build flat parameter list from all sheets
  params <- list()
  for (sheet_name in names(raw)) {
    df <- raw[[sheet_name]]
    if (is.null(df)) next
    col1 <- names(df)[1]
    col_base <- names(df)[3]
    col_low <- names(df)[4]
    col_high <- names(df)[5]
    col_dist <- names(df)[6]

    for (i in seq_len(nrow(df))) {
      pname <- as.character(df[[col1]][i])
      if (is.na(pname) || startsWith(pname, "##") || pname == "") next
      base_val <- suppressWarnings(as.numeric(df[[col_base]][i]))
      low_val <- suppressWarnings(as.numeric(df[[col_low]][i]))
      high_val <- suppressWarnings(as.numeric(df[[col_high]][i]))
      dist_val <- as.character(df[[col_dist]][i])
      if (is.na(base_val)) next

      params[[pname]] <- list(
        value = base_val,
        low = ifelse(is.na(low_val), base_val * 0.5, low_val),
        high = ifelse(is.na(high_val), base_val * 1.5, high_val),
        distribution = ifelse(is.na(dist_val), "Beta", dist_val),
        domain = sheet_name
      )
    }
  }

  # Store raw data frames for display in parameter module
  attr(params, "raw_sheets") <- raw
  params
}

# ── Load default parameters ───────────────────────────────────────────────────
params_default <- read_params_from_excel()

# ── Extract a flat named vector of base-case values ───────────────────────────
get_param_values <- function(params) {
  sapply(params, function(x) x$value, USE.NAMES = TRUE)
}

# ── Key model settings ────────────────────────────────────────────────────────
MODEL_SETTINGS <- list(
  n_cohort = 100000,
  n_cycles = 160,          # 40 years × 4 cycles/year

cycle_length = 0.25,     # 3 months
  time_horizon = 40,
  discount_rate = 0.03,
  strategies = c("TST", "IGRA", "CyTb", "Sequential", "TreatAll"),
  strategy_names = c("TST Alone", "IGRA Alone", "Cy-Tb Alone",
                     "TST \u2192 IGRA", "Treat All"),
  wtp_thresholds = c(100000, 234859, 500000, 704577)
)
