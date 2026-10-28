# ============================================================
# analysis_forecast.R
# Time Series Forecasting — ARIMA & ETS
# ============================================================

library(forecast)
library(lubridate)

run_forecast <- function(monthly_df, col = "totalChallan", h = 12) {
  vals <- monthly_df[[col]]
  ts_data <- ts(vals,
                start     = c(monthly_df$year[1], monthly_df$month[1]),
                frequency = 12)

  arima_fit <- auto.arima(ts_data, seasonal = TRUE,
                           stepwise = FALSE, approximation = FALSE)
  ets_fit   <- ets(ts_data)

  list(
    arima     = forecast(arima_fit, h = h),
    ets       = forecast(ets_fit,   h = h),
    ts        = ts_data,
    h         = h,
    last_date = tail(monthly_df$date, 1),
    col       = col
  )
}

# Returns data frame with exact underscore column names — no renaming needed
build_forecast_table <- function(fc) {
  dates <- seq(fc$last_date %m+% months(1), by = "month", length.out = fc$h)

  df <- data.frame(
    Month          = format(dates, "%b %Y"),
    ARIMA_Forecast = round(pmax(as.numeric(fc$arima$mean),          0)),
    ARIMA_Lo95     = round(pmax(as.numeric(fc$arima$lower[, 2]),    0)),
    ARIMA_Hi95     = round(pmax(as.numeric(fc$arima$upper[, 2]),    0)),
    ETS_Forecast   = round(pmax(as.numeric(fc$ets$mean),            0)),
    ETS_Lo95       = round(pmax(as.numeric(fc$ets$lower[, 2]),      0)),
    ETS_Hi95       = round(pmax(as.numeric(fc$ets$upper[, 2]),      0)),
    stringsAsFactors = FALSE
  )
  df
}

# get_accuracy_text <- function(fc) {
#   a1 <- accuracy(fc$arima)
#   a2 <- accuracy(fc$ets)
#   paste0(
#     "ARIMA\n",
#     "  RMSE : ", round(a1[1, "RMSE"], 2), "\n",
#     "  MAPE : ", round(a1[1, "MAPE"], 2), "%\n\n",
#     "ETS\n",
#     "  RMSE : ", round(a2[1, "RMSE"], 2), "\n",
#     "  MAPE : ", round(a2[1, "MAPE"], 2), "%\n\n",
#     "Lower = Better fit"
#   )
# }
get_accuracy_text <- function(fc, monthly_df, col) {
  
  vals <- monthly_df[[col]]
  
  n <- length(vals)
  n_train <- floor(n * 0.8)
  
  train <- vals[1:n_train]
  test  <- vals[(n_train + 1):n]
  
  start_year  <- monthly_df$year[1]
  start_month <- monthly_df$month[1]
  
  train_ts <- ts(train,
                 start = c(start_year, start_month),
                 frequency = 12)
  
  test_ts <- ts(test,
                start = c(
                  monthly_df$year[n_train + 1],
                  monthly_df$month[n_train + 1]
                ),
                frequency = 12)
  
  arima_fit <- auto.arima(train_ts)
  ets_fit   <- ets(train_ts)
  
  arima_fc <- forecast(arima_fit, h = length(test))
  ets_fc   <- forecast(ets_fit,   h = length(test))
  
  arima_rmse <- sqrt(mean((test_ts - arima_fc$mean)^2))
  ets_rmse   <- sqrt(mean((test_ts - ets_fc$mean)^2))
  
  smape <- function(a, f) {
    mean(200 * abs(a - f) / (abs(a) + abs(f)), na.rm = TRUE)
  }
  
  arima_smape <- smape(test_ts, arima_fc$mean)
  ets_smape   <- smape(test_ts, ets_fc$mean)
  
  paste0(
    "ARIMA\n",
    "  RMSE : ", round(arima_rmse, 2), "\n",
    "  SMAPE: ", round(arima_smape, 2), "%\n\n",
    "ETS\n",
    "  RMSE : ", round(ets_rmse, 2), "\n",
    "  SMAPE: ", round(ets_smape, 2), "%\n\n",
    "Evaluation based on hold-out test set"
  )
}

