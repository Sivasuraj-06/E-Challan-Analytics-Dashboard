# ============================================================
# analysis_policy.R
# MV Act 2019 Impact Analysis — Welch t-test Before/After
# ============================================================

library(dplyr)

MV_CUTOFF      <- as.Date("2019-10-01")
COVID_START    <- as.Date("2020-03-01")
COVID_END      <- as.Date("2021-06-30")

run_policy_analysis <- function(monthly_df) {

  before         <- monthly_df[monthly_df$date <  MV_CUTOFF, ]
  after          <- monthly_df[monthly_df$date >= MV_CUTOFF, ]
  after_no_covid <- after[!(after$date >= COVID_START & after$date <= COVID_END), ]

  metrics <- list(
    list(col = "totalChallan",      label = "Total Challans"),
    list(col = "totalAmount",       label = "Total Revenue (Rs)"),
    list(col = "compliance_rate",   label = "Compliance Rate (%)"),
    list(col = "court_burden_rate", label = "Court Burden Rate (%)"),
    list(col = "backlog_rate",      label = "Backlog Rate (%)")
  )

  rows <- lapply(metrics, function(m) {
    b   <- before[[m$col]];        b   <- b[!is.na(b)]
    a   <- after[[m$col]];         a   <- a[!is.na(a)]
    anc <- after_no_covid[[m$col]]; anc <- anc[!is.na(anc)]

    mb  <- if (length(b)   > 0) mean(b)   else NA
    ma  <- if (length(a)   > 0) mean(a)   else NA
    mac <- if (length(anc) > 0) mean(anc) else NA

    tt  <- tryCatch(t.test(b, a), error = function(e) NULL)
    pv_raw <- if (!is.null(tt)) tt$p.value else NA

    # Format p-value properly — never show raw 0
    pv_fmt <- if (is.na(pv_raw)) {
      "N/A"
    } else if (pv_raw < 0.0001) {
      "< 0.0001"
    } else {
      formatC(pv_raw, format = "f", digits = 4)
    }

    pct_change <- if (!is.na(mb) && mb != 0) (ma - mb) / mb * 100 else NA

    data.frame(
      Metric        = m$label,
      Before        = round(mb,  2),
      After         = round(ma,  2),
      After_NoCovid = round(mac, 2),
      Pct_Change    = round(pct_change, 2),
      P_Value       = pv_fmt,
      Significant   = ifelse(!is.na(pv_raw) & pv_raw < 0.05, "Yes", "No"),
      stringsAsFactors = FALSE
    )
  })

  list(
    summary     = do.call(rbind, rows),
    cutoff      = MV_CUTOFF,
    covid_start = COVID_START,
    covid_end   = COVID_END,
    monthly     = monthly_df
  )
}
