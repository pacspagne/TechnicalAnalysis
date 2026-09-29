# ==============================================================================
# chart_helpers.R -- Steps 2, 3 and 4 logic, pulled out of the Shiny
# server function into plain, independently-testable functions.
#
# Keeping this logic outside of renderPlot() means it can be run and verified
# directly with Rscript (see test_chart_helpers.R), the same
# calculate-then-verify pattern used throughout this project, without needing
# a live browser session to check that the numbers are right.
#
# BDA400 Assignment 6: Technical Analysis using R, Visualization Phase
# Author: Onuegbo Nnaemeka Philip
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(TTR)
})

source("scripts/sma.R")
source("scripts/crossover.R")

# ------------------------------------------------------------------------
# Step 2 (data prep): filter an xts stock object to a date range and an
# aggregation time frame (Daily / Weekly / Monthly)
# ------------------------------------------------------------------------
filter_stock_data <- function(stock_data, date_range, time_frame = "Daily") {
  filtered <- stock_data[paste0(date_range[1], "/", date_range[2])]

  filtered <- switch(time_frame,
    "Daily"   = filtered,
    "Weekly"  = to.weekly(filtered, OHLC = TRUE, drop.time = TRUE),
    "Monthly" = to.monthly(filtered, OHLC = TRUE, drop.time = TRUE, indexAt = "yearmon"),
    filtered
  )
  # to.weekly()/to.monthly() prefix each column with the input's variable name
  # (e.g. "filtered.Open") but keep the same 6-column OHLCVA shape as the
  # daily data, since the input already carried an Adjusted column -- just
  # normalize the names either way.
  colnames(filtered) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  filtered
}

# ------------------------------------------------------------------------
# Step 2: build a data.frame ggplot can use (xts objects need converting)
# ------------------------------------------------------------------------
stock_to_df <- function(filtered_data) {
  df <- data.frame(
    Date = zoo::index(filtered_data),
    Open = as.numeric(filtered_data$Open),
    High = as.numeric(filtered_data$High),
    Low = as.numeric(filtered_data$Low),
    Close = as.numeric(filtered_data$Close),
    Volume = as.numeric(filtered_data$Volume)
  )
  df
}

# ------------------------------------------------------------------------
# Step 4: Moving Average crossover trading rule, using the exact crossover()
# function verified in Assignment 5. Returns Buy / Sell / Hold per row of df,
# aligned to df's length (leading rows where a moving average isn't yet
# defined, or no cross has happened, are "Hold").
# ------------------------------------------------------------------------
compute_trading_signals <- function(df, short_period = 20, long_period = 50) {
  n <- nrow(df)
  short_ma <- sma(df$Close, min(short_period, n))
  long_ma <- sma(df$Close, min(long_period, n))

  # Align both MA vectors to the same trailing window (they start at
  # different offsets because sma() drops (period-1) leading points each).
  common_len <- min(length(short_ma), length(long_ma))
  short_aligned <- tail(short_ma, common_len)
  long_aligned <- tail(long_ma, common_len)

  cross <- crossover(short_aligned, long_aligned)
  # Map crossover()'s "Up"/"Down"/"None" onto trading language, then pad the
  # front of the vector (rows before long_ma is defined) with "Hold".
  signal_tail <- ifelse(cross == "Up", "Buy", ifelse(cross == "Down", "Sell", "Hold"))
  signals <- c(rep("Hold", n - common_len), signal_tail)
  signals
}

# ------------------------------------------------------------------------
# Step 2/3: build the main price chart (line, area, or candlestick), with
# optional Moving Average overlay and optional Buy/Sell annotations.
# ------------------------------------------------------------------------
build_price_chart <- function(df, chart_type = "Line", show_ma = FALSE,
                               short_period = 20, long_period = 50,
                               show_signals = FALSE) {
  if (chart_type == "Candlestick") {
    df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")
  }
  p <- ggplot(df, aes(x = Date))

  if (chart_type == "Candlestick") {
    p <- p +
      geom_segment(aes(xend = Date, y = Low, yend = High), color = "grey40") +
      geom_rect(aes(xmin = Date - 0.3, xmax = Date + 0.3,
                    ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                    fill = Direction)) +
      scale_fill_manual(values = c(Up = "#2E7D32", Down = "#C62828")) +
      ylab("Price (USD)")
  } else if (chart_type == "Area") {
    p <- p + geom_area(aes(y = Close), fill = "#90CAF9", alpha = 0.5) +
      geom_line(aes(y = Close), color = "#1565C0") +
      ylab("Close Price (USD)")
  } else {
    p <- p + geom_line(aes(y = Close), color = "#1565C0") +
      ylab("Close Price (USD)")
  }

  if (show_ma) {
    n <- nrow(df)
    short_ma <- sma(df$Close, min(short_period, n))
    long_ma <- sma(df$Close, min(long_period, n))
    ma_df <- data.frame(
      Date = tail(df$Date, length(short_ma)),
      ShortMA = short_ma
    )
    ma_df2 <- data.frame(
      Date = tail(df$Date, length(long_ma)),
      LongMA = long_ma
    )
    p <- p +
      geom_line(data = ma_df, aes(x = Date, y = ShortMA, color = paste0("SMA(", short_period, ")"))) +
      geom_line(data = ma_df2, aes(x = Date, y = LongMA, color = paste0("SMA(", long_period, ")"))) +
      scale_color_manual(name = "Moving Average",
                          values = setNames(c("#F9A825", "#6A1B9A"),
                                             c(paste0("SMA(", short_period, ")"), paste0("SMA(", long_period, ")"))))
  }

  if (show_signals) {
    df$Signal <- compute_trading_signals(df, short_period, long_period)
    signal_points <- df[df$Signal %in% c("Buy", "Sell"), ]
    if (nrow(signal_points) > 0) {
      p <- p +
        geom_point(data = signal_points,
                   aes(x = Date, y = Close, shape = Signal),
                   size = 3, inherit.aes = FALSE,
                   color = ifelse(signal_points$Signal == "Buy", "#2E7D32", "#C62828")) +
        geom_text(data = signal_points,
                  aes(x = Date, y = Close, label = Signal),
                  vjust = -1, size = 3, fontface = "bold", inherit.aes = FALSE,
                  color = ifelse(signal_points$Signal == "Buy", "#2E7D32", "#C62828")) +
        scale_shape_manual(name = "Signal", values = c(Buy = 24, Sell = 25))
    }
  }

  p + theme_minimal() + xlab("Date") +
    ggtitle("Stock Price") +
    theme(legend.position = "bottom")
}

# ------------------------------------------------------------------------
# Step 3: RSI sub-chart, with overbought (70) / oversold (30) reference lines
# ------------------------------------------------------------------------
build_rsi_chart <- function(df, period = 14) {
  rsi_values <- TTR::RSI(df$Close, n = period)
  rsi_df <- data.frame(Date = df$Date, RSI = as.numeric(rsi_values))
  ggplot(rsi_df, aes(x = Date, y = RSI)) +
    geom_line(color = "#6A1B9A") +
    geom_hline(yintercept = 70, linetype = "dashed", color = "#C62828") +
    geom_hline(yintercept = 30, linetype = "dashed", color = "#2E7D32") +
    ylim(0, 100) +
    theme_minimal() + ylab("RSI") + xlab("Date") +
    ggtitle(paste0("RSI (", period, ")"))
}

# ------------------------------------------------------------------------
# Step 3: MACD sub-chart (MACD line, signal line, histogram)
# ------------------------------------------------------------------------
build_macd_chart <- function(df, short_period = 12, long_period = 26, signal_period = 9) {
  macd_result <- TTR::MACD(df$Close, nFast = short_period, nSlow = long_period,
                            nSig = signal_period, maType = "EMA")
  macd_df <- data.frame(
    Date = df$Date,
    MACD = as.numeric(macd_result[, "macd"]),
    Signal = as.numeric(macd_result[, "signal"])
  )
  macd_df$Histogram <- macd_df$MACD - macd_df$Signal

  ggplot(macd_df, aes(x = Date)) +
    geom_col(aes(y = Histogram), fill = "grey70") +
    geom_line(aes(y = MACD, color = "MACD")) +
    geom_line(aes(y = Signal, color = "Signal")) +
    scale_color_manual(name = NULL, values = c(MACD = "#1565C0", Signal = "#F9A825")) +
    theme_minimal() + ylab("MACD") + xlab("Date") +
    ggtitle(paste0("MACD (", short_period, ",", long_period, ",", signal_period, ")")) +
    theme(legend.position = "bottom")
}
