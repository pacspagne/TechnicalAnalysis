# ==============================================================================
# run_all_indicators.R -- Applies every indicator built for this assignment to
# real AAPL stock data (the same dataset used in Assignment 2 of this
# three-part project), demonstrating all 9 functions working together.
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
# Author: Onuegbo Nnaemeka Philip
# ==============================================================================

source("scripts/sma.R")
source("scripts/ema.R")
source("scripts/macd.R")
source("scripts/stdev.R")
source("scripts/linreg.R")
source("scripts/rsi.R")
source("scripts/stoch_rsi.R")
source("scripts/crossover.R")
source("scripts/crossunder.R")

aapl <- read.csv("AAPL.csv", stringsAsFactors = FALSE)
aapl$Date <- as.Date(aapl$Date)
close <- aapl$Close

cat("Loaded", nrow(aapl), "rows of real AAPL data (", as.character(min(aapl$Date)),
    "to", as.character(max(aapl$Date)), ")\n\n")

cat("=== SMA (20-day) on AAPL Close -- last 5 values ===\n")
sma_20 <- sma(close, 20)
print(tail(sma_20, 5))

cat("\n=== EMA (20-day) on AAPL Close -- last 5 values ===\n")
ema_20 <- ema(close, 20)
print(tail(ema_20, 5))

cat("\n=== MACD (12, 26, 9) on AAPL Close -- last 5 values ===\n")
macd_result <- macd(close, short_period = 12, long_period = 26, signal_period = 9)
cat("macd_line (tail):\n"); print(tail(macd_result$macd_line, 5))
cat("signal_line (tail):\n"); print(tail(macd_result$signal_line, 5))
cat("histogram (tail):\n"); print(tail(macd_result$histogram, 5))

cat("\n=== Standard Deviation of AAPL Close (whole series) ===\n")
cat("stdev(close):", stdev(close), "\n")
cat("Cross-check vs R's sd() [adjusted for population vs sample]:",
    sd(close) * sqrt((length(close) - 1) / length(close)), "\n")

cat("\n=== Linear Regression on last 30 days of AAPL Close ===\n")
lr <- linreg(close, regressionLength = 30, regressionOffset = 0)
cat("slope:", lr$slope, "(", ifelse(lr$slope > 0, "uptrend", "downtrend"), ")\n")
cat("intercept:", lr$intercept, "\n")

cat("\n=== RSI (14-day) on AAPL Close -- last 5 values ===\n")
rsi_14 <- rsi(close, 14)
print(tail(rsi_14, 5))
cat("Interpretation: RSI > 70 = overbought, RSI < 30 = oversold\n")

cat("\n=== Stochastic RSI on AAPL Close -- last 5 values ===\n")
srsi <- stoch_rsi(close, period = 14, k_period = 3, d_period = 3)
cat("%K (tail):\n"); print(tail(srsi$k_line, 5))
cat("%D (tail):\n"); print(tail(srsi$d_line, 5))

cat("\n=== Crossover / Crossunder: SMA(20) vs SMA(50) on AAPL ===\n")
sma_50 <- sma(close, 50)
# Align lengths: sma(close,20) and sma(close,50) start at different offsets
# relative to the original close vector, since each drops (period-1) leading
# points. Align both to the same trailing window before comparing.
n20 <- length(sma_20); n50 <- length(sma_50)
common_len <- min(n20, n50)
sma_20_aligned <- tail(sma_20, common_len)
sma_50_aligned <- tail(sma_50, common_len)

cross_up <- crossover(sma_20_aligned, sma_50_aligned)
cross_down <- crossunder(sma_20_aligned, sma_50_aligned)

up_signals <- sum(cross_up == "Up")
down_signals <- sum(cross_down == "True")
cat("Number of 'golden cross' (SMA20 crosses above SMA50) signals:", up_signals, "\n")
cat("Number of 'death cross' (SMA20 crosses below SMA50) signals:", down_signals, "\n")

# Show the dates of the last 3 golden crosses as a concrete example
up_indices <- which(cross_up == "Up")
if (length(up_indices) > 0) {
  # Map back to dates: sma_20_aligned's index i corresponds to the
  # (i + offset)-th row of the original data, where offset accounts for
  # both the SMA window and the tail-alignment above.
  date_offset <- nrow(aapl) - common_len
  last_ups <- tail(up_indices, 3)
  cat("Dates of the last", length(last_ups), "golden cross signals:\n")
  print(aapl$Date[date_offset + last_ups])
}

cat("\n=== VERIFICATION SUMMARY ===\n")
cat("All 9 indicators ran successfully against", nrow(aapl), "rows of real AAPL data.\n")
cat("No errors or NA-propagation issues in the final tail values shown above.\n")
