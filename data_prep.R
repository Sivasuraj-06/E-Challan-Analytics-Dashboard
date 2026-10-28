# ============================================================
# data_prep.R — Data Loading & Feature Engineering
# E-Challan System Analysis (2015 – Dec 2025)
# ============================================================

library(dplyr)
library(lubridate)

load_and_prepare <- function(filepath = "echallan_daily_data.csv") {
  df <- read.csv(filepath, stringsAsFactors = FALSE)

  df$date <- as.Date(df$date,
    tryFormats = c("%Y-%m-%d", "%d-%m-%Y", "%d/%m/%Y", "%Y/%m/%d"))

  # Filter to Dec 2025 only
  df <- df %>% filter(!is.na(date), date <= as.Date("2025-12-31"))

  df$year  <- year(df$date)
  df$month <- month(df$date)
  df$ym    <- format(df$date, "%Y-%m")

  # Derived metrics (safe division)
  df$compliance_rate     <- ifelse(df$totalChallan > 0,
                                   df$disposedChallan / df$totalChallan * 100, NA)
  df$backlog_rate        <- ifelse(df$totalChallan > 0,
                                   df$pendingChallan  / df$totalChallan * 100, NA)
  df$court_burden_rate   <- ifelse(df$totalCourt > 0,
                                   df$pendingCourt    / df$totalCourt   * 100, NA)
  df$collection_rate     <- ifelse(df$totalAmount > 0,
                                   df$disposedAmount  / df$totalAmount  * 100, NA)
  df$revenue_per_challan <- ifelse(df$totalChallan > 0,
                                   df$totalAmount     / df$totalChallan,       NA)

  # Clamp rates to [0, 100]
  clamp <- function(x) pmin(pmax(x, 0, na.rm = FALSE), 100)
  df$compliance_rate   <- clamp(df$compliance_rate)
  df$backlog_rate      <- clamp(df$backlog_rate)
  df$court_burden_rate <- clamp(df$court_burden_rate)
  df$collection_rate   <- clamp(df$collection_rate)

  df %>% arrange(date)
}

monthly_summary <- function(df) {
  df %>%
    group_by(year, month, ym) %>%
    summarise(
      totalChallan    = sum(totalChallan,    na.rm = TRUE),
      disposedChallan = sum(disposedChallan, na.rm = TRUE),
      pendingChallan  = sum(pendingChallan,  na.rm = TRUE),
      totalAmount     = sum(totalAmount,     na.rm = TRUE),
      disposedAmount  = sum(disposedAmount,  na.rm = TRUE),
      pendingAmount   = sum(pendingAmount,   na.rm = TRUE),
      totalCourt      = sum(totalCourt,      na.rm = TRUE),
      pendingCourt    = sum(pendingCourt,    na.rm = TRUE),
      disposedCourt   = sum(disposedCourt,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      compliance_rate     = ifelse(totalChallan > 0, disposedChallan / totalChallan * 100, NA),
      backlog_rate        = ifelse(totalChallan > 0, pendingChallan  / totalChallan * 100, NA),
      court_burden_rate   = ifelse(totalCourt   > 0, pendingCourt    / totalCourt   * 100, NA),
      collection_rate     = ifelse(totalAmount  > 0, disposedAmount  / totalAmount  * 100, NA),
      revenue_per_challan = ifelse(totalChallan > 0, totalAmount     / totalChallan,       NA),
      date = as.Date(paste0(ym, "-01"))
    ) %>%
    arrange(date)
}

yearly_summary <- function(df) {
  df %>%
    group_by(year) %>%
    summarise(
      totalChallan    = sum(totalChallan,    na.rm = TRUE),
      disposedChallan = sum(disposedChallan, na.rm = TRUE),
      pendingChallan  = sum(pendingChallan,  na.rm = TRUE),
      totalAmount     = sum(totalAmount,     na.rm = TRUE),
      disposedAmount  = sum(disposedAmount,  na.rm = TRUE),
      pendingAmount   = sum(pendingAmount,   na.rm = TRUE),
      totalCourt      = sum(totalCourt,      na.rm = TRUE),
      pendingCourt    = sum(pendingCourt,    na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      compliance_rate   = ifelse(totalChallan > 0, disposedChallan / totalChallan * 100, NA),
      collection_rate   = ifelse(totalAmount  > 0, disposedAmount  / totalAmount  * 100, NA),
      court_burden_rate = ifelse(totalCourt   > 0, pendingCourt    / totalCourt   * 100, NA),
      backlog_rate      = ifelse(totalChallan > 0, pendingChallan  / totalChallan * 100, NA),
      growth_challan    = (totalChallan / lag(totalChallan) - 1) * 100,
      growth_revenue    = (totalAmount  / lag(totalAmount)  - 1) * 100
    )
}


# library(dplyr)
# library(lubridate)
# 
# load_and_prepare <- function(filepath = "echallan_daily_data.csv") {
#   df <- read.csv(filepath, stringsAsFactors = FALSE)
# 
#   df$date <- as.Date(df$date,
#                      tryFormats = c("%Y-%m-%d", "%d-%m-%Y", "%d/%m/%Y", "%Y/%m/%d"))
# 
#   df <- df %>% filter(!is.na(date), date <= as.Date("2025-12-31"))
# 
#   df$year  <- year(df$date)
#   df$month <- month(df$date)
#   df$ym    <- format(df$date, "%Y-%m")
# 
#   df$compliance_rate     <- ifelse(df$totalChallan > 0,
#                                    df$disposedChallan / df$totalChallan * 100, NA)
#   df$backlog_rate        <- ifelse(df$totalChallan > 0,
#                                    df$pendingChallan  / df$totalChallan * 100, NA)
#   df$court_burden_rate   <- ifelse(df$totalCourt > 0,
#                                    df$pendingCourt    / df$totalCourt   * 100, NA)
#   df$collection_rate     <- ifelse(df$totalAmount > 0,
#                                    df$disposedAmount  / df$totalAmount  * 100, NA)
#   df$revenue_per_challan <- ifelse(df$totalChallan > 0,
#                                    df$totalAmount     / df$totalChallan,       NA)
# 
#   clamp <- function(x) pmin(pmax(x, 0), 100)
# 
#   df$compliance_rate   <- clamp(df$compliance_rate)
#   df$backlog_rate      <- clamp(df$backlog_rate)
#   df$court_burden_rate <- clamp(df$court_burden_rate)
#   df$collection_rate   <- clamp(df$collection_rate)
# 
#   invalid_rows <- df$totalChallan < (df$disposedChallan + df$pendingChallan)
#   if (any(invalid_rows, na.rm = TRUE)) {
#     warning("Data inconsistency detected: totalChallan < disposed + pending")
#   }
# 
#   df %>% arrange(date)
# }
# 
# monthly_summary <- function(df) {
#   df %>%
#     group_by(year, month, ym) %>%
#     summarise(
#       totalChallan    = sum(totalChallan,    na.rm = TRUE),
#       disposedChallan = sum(disposedChallan, na.rm = TRUE),
#       pendingChallan  = sum(pendingChallan,  na.rm = TRUE),
#       totalAmount     = sum(totalAmount,     na.rm = TRUE),
#       disposedAmount  = sum(disposedAmount,  na.rm = TRUE),
#       pendingAmount   = sum(pendingAmount,   na.rm = TRUE),
#       totalCourt      = sum(totalCourt,      na.rm = TRUE),
#       pendingCourt    = sum(pendingCourt,    na.rm = TRUE),
#       disposedCourt   = sum(disposedCourt,   na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       compliance_rate     = ifelse(totalChallan > 0, disposedChallan / totalChallan * 100, NA),
#       backlog_rate        = ifelse(totalChallan > 0, pendingChallan  / totalChallan * 100, NA),
#       court_burden_rate   = ifelse(totalCourt   > 0, pendingCourt    / totalCourt   * 100, NA),
#       collection_rate     = ifelse(totalAmount  > 0, disposedAmount  / totalAmount  * 100, NA),
#       revenue_per_challan = ifelse(totalChallan > 0, totalAmount     / totalChallan,       NA),
#       compliance_rate     = pmin(pmax(compliance_rate, 0), 100),
#       backlog_rate        = pmin(pmax(backlog_rate, 0), 100),
#       court_burden_rate   = pmin(pmax(court_burden_rate, 0), 100),
#       collection_rate     = pmin(pmax(collection_rate, 0), 100),
#       date = as.Date(paste0(ym, "-01"))
#     ) %>%
#     arrange(date)
# }
# 
# yearly_summary <- function(df) {
#   df %>%
#     group_by(year) %>%
#     summarise(
#       totalChallan    = sum(totalChallan,    na.rm = TRUE),
#       disposedChallan = sum(disposedChallan, na.rm = TRUE),
#       pendingChallan  = sum(pendingChallan,  na.rm = TRUE),
#       totalAmount     = sum(totalAmount,     na.rm = TRUE),
#       disposedAmount  = sum(disposedAmount,  na.rm = TRUE),
#       pendingAmount   = sum(pendingAmount,   na.rm = TRUE),
#       totalCourt      = sum(totalCourt,      na.rm = TRUE),
#       pendingCourt    = sum(pendingCourt,    na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     mutate(
#       compliance_rate   = ifelse(totalChallan > 0, disposedChallan / totalChallan * 100, NA),
#       collection_rate   = ifelse(totalAmount  > 0, disposedAmount  / totalAmount  * 100, NA),
#       court_burden_rate = ifelse(totalCourt   > 0, pendingCourt    / totalCourt   * 100, NA),
#       backlog_rate      = ifelse(totalChallan > 0, pendingChallan  / totalChallan * 100, NA),
#       compliance_rate   = pmin(pmax(compliance_rate, 0), 100),
#       collection_rate   = pmin(pmax(collection_rate, 0), 100),
#       court_burden_rate = pmin(pmax(court_burden_rate, 0), 100),
#       backlog_rate      = pmin(pmax(backlog_rate, 0), 100),
#       growth_challan    = ifelse(lag(totalChallan) > 0,
#                                  (totalChallan / lag(totalChallan) - 1) * 100, NA),
#       growth_revenue    = ifelse(lag(totalAmount) > 0,
#                                  (totalAmount / lag(totalAmount) - 1) * 100, NA)
#     )
# }

