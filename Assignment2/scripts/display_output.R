# ==============================================================================
# display_output.R
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# Author: Onuegbo Nnaemeka Philip
#
# Purpose:
#   Customized display functions for the imported stock data and its
#   calculated statistics. Demonstrates several of the common ways stock
#   market data is presented (as researched via Yahoo Finance and standard
#   technical-analysis dashboards): a plain tabular quote-style summary, a
#   "latest quote" single-row snapshot, a full statistics table across the
#   whole portfolio, and price/moving-average charts (line chart with
#   overlaid SMAs, plus an OHLC candlestick-style chart via quantmod's own
#   chartSeries()).
# ==============================================================================

library(quantmod)

#' Display a Yahoo-Finance-style tabular snapshot of a stock's data
#' (most recent N rows, most-recent-first, like a price history table)
#'
#' @param stock_df Data frame from load_stock_data()
#' @param symbol    Ticker symbol for the header
#' @param n         Number of most recent rows to show (default 10)
display_price_table <- function(stock_df, symbol = "UNKNOWN", n = 10) {
  ordered <- stock_df[order(stock_df$Date, decreasing = TRUE), ]
  recent <- head(ordered, n)
  recent$Change <- c(NA, diff(rev(recent$Close))) * -1
  recent$PctChange <- round(100 * recent$Change / (recent$Close - recent$Change), 2)

  cat(sprintf("\n===== %s : Most Recent %d Trading Days =====\n", symbol, n))
  print(recent[, c("Date", "Open", "High", "Low", "Close", "Volume")], row.names = FALSE)
  invisible(recent)
}

#' Display a single "current quote" style snapshot (like the header box on
#' a Yahoo Finance stock page): latest close, daily change, and day's range.
#'
#' @param stock_df Data frame from load_stock_data()
#' @param symbol    Ticker symbol for the header
display_latest_quote <- function(stock_df, symbol = "UNKNOWN") {
  ordered <- stock_df[order(stock_df$Date, decreasing = TRUE), ]
  latest <- ordered[1, ]
  previous <- ordered[2, ]

  change <- latest$Close - previous$Close
  pct_change <- round(100 * change / previous$Close, 2)
  arrow <- ifelse(change >= 0, "UP", "DOWN")

  cat(sprintf(
    "\n%s  |  %s  |  Close: $%.2f  %s %.2f (%.2f%%)  |  Day Range: $%.2f - $%.2f  |  Volume: %s\n",
    symbol, format(latest$Date, "%b %d, %Y"), latest$Close, arrow,
    abs(change), abs(pct_change), latest$Low, latest$High,
    format(latest$Volume, big.mark = ",")
  ))
  invisible(latest)
}

#' Display the full statistics summary table for the whole portfolio
#'
#' @param stats_summary Combined data frame from calculate_statistics() runs
display_statistics_table <- function(stats_summary) {
  cat("\n================ PORTFOLIO STATISTICS SUMMARY ================\n")
  printable <- stats_summary
  printable[, c("Mean", "Median", "Mode", "SD", "Min", "Max")] <-
    round(printable[, c("Mean", "Median", "Mode", "SD", "Min", "Max")], 2)
  print(printable, row.names = FALSE)
  cat("=================================================================\n")
  invisible(printable)
}

#' Plot closing price with overlaid moving averages (line chart)
#'
#' @param ma_df   Data frame with Date, Close, and SMA_* columns
#' @param symbol   Ticker symbol for the chart title
#' @param filepath Output PNG file path
plot_price_with_ma <- function(ma_df, symbol = "UNKNOWN", filepath = NULL) {
  if (is.null(filepath)) filepath <- sprintf("output/%s_price_ma.png", symbol)

  png(filepath, width = 1000, height = 600, res = 120)
  ma_cols <- grep("^SMA_", names(ma_df), value = TRUE)
  y_range <- range(ma_df$Close, na.rm = TRUE)

  plot(ma_df$Date, ma_df$Close, type = "l", col = "black", lwd = 1.2,
       xlab = "Date", ylab = "Price (USD)",
       main = sprintf("%s -- Closing Price with Moving Averages", symbol),
       ylim = y_range)

  colors <- c("blue", "red", "darkgreen", "purple")
  for (i in seq_along(ma_cols)) {
    lines(ma_df$Date, ma_df[[ma_cols[i]]], col = colors[((i - 1) %% length(colors)) + 1], lwd = 1.5)
  }
  legend("topleft", legend = c("Close", ma_cols),
         col = c("black", colors[seq_along(ma_cols)]), lty = 1, lwd = 1.5, cex = 0.8)
  dev.off()
  cat(sprintf("Saved chart: %s\n", filepath))
  invisible(filepath)
}

#' Plot an OHLC candlestick-style chart using quantmod's chartSeries()
#'
#' @param stock_df Data frame from load_stock_data()
#' @param symbol    Ticker symbol
#' @param filepath  Output PNG file path
plot_candlestick <- function(stock_df, symbol = "UNKNOWN", filepath = NULL) {
  if (is.null(filepath)) filepath <- sprintf("output/%s_candlestick.png", symbol)

  xts_obj <- xts::xts(
    stock_df[, c("Open", "High", "Low", "Close", "Volume")],
    order.by = stock_df$Date
  )
  colnames(xts_obj) <- paste0(symbol, c(".Open", ".High", ".Low", ".Close", ".Volume"))

  # Last 180 trading days for readability
  recent <- tail(xts_obj, 180)

  png(filepath, width = 1000, height = 700, res = 120)
  chartSeries(recent, name = symbol, theme = chartTheme("white"),
              TA = "addSMA(n=20, col='blue'); addSMA(n=50, col='red'); addVo()")
  dev.off()
  cat(sprintf("Saved chart: %s\n", filepath))
  invisible(filepath)
}

# ------------------------------------------------------------------------
# Standalone execution / smoke test -- runs the full pipeline end to end
# ------------------------------------------------------------------------
if (sys.nframe() == 0) {

  source("scripts/load_stock_data.R")
  source("scripts/calculate_statistics.R")

  if (file.exists("output/stocks_data.rds")) {
    stocks <- readRDS("output/stocks_data.rds")
  } else {
    stocks <- load_stock_data(portfolio_file = "portfolio.txt", data_dir = "data", src = "csv")
  }

  all_summaries <- data.frame()

  for (sym in names(stocks)) {
    display_price_table(stocks[[sym]], symbol = sym, n = 10)
    display_latest_quote(stocks[[sym]], symbol = sym)

    result <- calculate_statistics(stocks[[sym]], symbol = sym, ma_periods = c(20, 50))
    all_summaries <- rbind(all_summaries, result$summary)

    plot_price_with_ma(result$moving_averages, symbol = sym)
    plot_candlestick(stocks[[sym]], symbol = sym)
  }

  display_statistics_table(all_summaries)
}
