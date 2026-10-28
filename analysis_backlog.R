
library(dplyr)

get_backlog_kpis <- function(df) {
  total_pending      <- sum(df$pendingChallan, na.rm = TRUE)
  total_disposed     <- sum(df$disposedChallan, na.rm = TRUE)
  total_court        <- sum(df$totalCourt,      na.rm = TRUE)
  pending_court      <- sum(df$pendingCourt,    na.rm = TRUE)
  backlog_rate       <- total_pending  / sum(df$totalChallan, na.rm = TRUE) * 100
  court_burden_rate  <- pending_court  / total_court * 100

  list(
    total_pending     = total_pending,
    total_disposed    = total_disposed,
    backlog_rate      = round(backlog_rate,      1),
    court_burden_rate = round(court_burden_rate, 1),
    pending_court     = pending_court,
    total_court       = total_court
  )
}

get_yearly_backlog_table <- function(yearly) {
  data.frame(
    Year             = yearly$year,
    `Total Challans` = formatC(yearly$totalChallan,    format = "d", big.mark = ","),
    `Disposed`       = formatC(yearly$disposedChallan, format = "d", big.mark = ","),
    `Pending`        = formatC(yearly$pendingChallan,  format = "d", big.mark = ","),
    `Revenue (Cr)`   = round(yearly$totalAmount   / 1e7, 1),
    `Collected (Cr)` = round(yearly$disposedAmount / 1e7, 1),
    `Pending (Cr)`   = round(yearly$pendingAmount  / 1e7, 1),
    `Compliance %`   = round(yearly$compliance_rate,   1),
    `Backlog %`      = round(yearly$backlog_rate,       1),
    `Court Burden %` = round(yearly$court_burden_rate,  1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
