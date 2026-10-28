# ============================================================
# analysis_correlation.R
# Correlation Analysis — Pearson Correlation Matrix
# ============================================================

library(dplyr)

METRIC_COLS <- c(
  "totalChallan", "disposedChallan", "pendingChallan",
  "totalAmount",  "disposedAmount",  "pendingAmount",
  "totalCourt",   "pendingCourt",
  "compliance_rate", "backlog_rate", "collection_rate"
)

METRIC_LABELS <- c(
  "Total Challans", "Disposed Challans", "Pending Challans",
  "Total Revenue",  "Collected Revenue", "Pending Revenue",
  "Court Cases",    "Pending Court",
  "Compliance %",   "Backlog %",         "Collection %"
)

run_correlation <- function(monthly_df) {
  d <- monthly_df[, METRIC_COLS]
  colnames(d) <- METRIC_LABELS
  cor(d, use = "complete.obs")
}

get_correlation_strength_table <- function(cor_mat, ref = "Total Challans") {
  vals <- cor_mat[, ref]
  df <- data.frame(
    Metric      = names(vals),
    Correlation = round(vals, 3),
    stringsAsFactors = FALSE
  )
  df <- df[df$Metric != ref, ]
  df$Strength <- cut(
    abs(df$Correlation),
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.01),
    labels = c("Negligible", "Weak", "Moderate", "Strong", "Very Strong"),
    include.lowest = TRUE
  )
  df$Direction <- ifelse(df$Correlation >= 0, "Positive \u2191", "Negative \u2193")
  df[order(-abs(df$Correlation)), ]
}

get_descriptive_stats <- function(monthly_df) {
  d <- monthly_df[, METRIC_COLS]
  data.frame(
    Metric  = METRIC_LABELS,
    Mean    = sapply(d, function(x) round(mean(x,   na.rm = TRUE), 1)),
    Median  = sapply(d, function(x) round(median(x, na.rm = TRUE), 1)),
    Std_Dev = sapply(d, function(x) round(sd(x,     na.rm = TRUE), 1)),
    Min     = sapply(d, function(x) round(min(x,    na.rm = TRUE), 1)),
    Max     = sapply(d, function(x) round(max(x,    na.rm = TRUE), 1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
