# ============================================================
# analysis_trend.R
# Trend Analysis — Yearly & Monthly Aggregations
# ============================================================

library(dplyr)

get_trend_kpis <- function(df, yearly) {
  total_challans  <- sum(df$totalChallan,    na.rm = TRUE)
  total_revenue   <- sum(df$totalAmount,     na.rm = TRUE)
  compliance_rate <- sum(df$disposedChallan, na.rm = TRUE) /
                     sum(df$totalChallan,    na.rm = TRUE) * 100
  backlog_rate    <- sum(df$pendingChallan,  na.rm = TRUE) /
                     sum(df$totalChallan,    na.rm = TRUE) * 100
  peak_year       <- yearly$year[which.max(yearly$totalChallan)]

  list(
    total_challans  = total_challans,
    total_revenue   = total_revenue,
    compliance_rate = round(compliance_rate, 1),
    backlog_rate    = round(backlog_rate,    1),
    peak_year       = peak_year
  )
}
