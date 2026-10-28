# ============================================================
# analysis_revenue.R
# Revenue & Collection Efficiency Analysis
# ============================================================

library(dplyr)

get_revenue_kpis <- function(df) {
  total_revenue    <- sum(df$totalAmount,    na.rm = TRUE)
  collected        <- sum(df$disposedAmount, na.rm = TRUE)
  pending          <- sum(df$pendingAmount,  na.rm = TRUE)
  collection_rate  <- collected / total_revenue * 100
  avg_fine         <- total_revenue / sum(df$totalChallan, na.rm = TRUE)

  list(
    total_revenue   = total_revenue,
    collected       = collected,
    pending         = pending,
    collection_rate = round(collection_rate, 1),
    avg_fine        = round(avg_fine, 0)
  )
}
