# ==============================================================================
# macd.R -- Moving Average Convergence Divergence
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
# Author: Onuegbo Nnaemeka Philip
#
# Depends on ema() (from ema.R), exactly as the assignment's own pseudocode
# does -- source ema.R before using macd() if running this file standalone.
# ==============================================================================

source("scripts/ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )

  return(result)
}

# ------------------------------------------------------------------------
# Standalone smoke test (matches the example given in the assignment PDF)
# ------------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(100, 105, 110, 115, 120, 125, 130)
  macd_result <- macd(data, short_period = 3, long_period = 5, signal_period = 2)
  cat("macd_line:\n"); print(macd_result$macd_line)
  cat("\nsignal_line:\n"); print(macd_result$signal_line)
  cat("\nhistogram:\n"); print(macd_result$histogram)

  # Independent cross-check: recompute macd_line manually from separately-
  # called ema() outputs, and confirm histogram = macd_line - signal_line
  # exactly (not just approximately) for every element
  short_check <- ema(data, 3)
  long_check <- ema(data, 5)
  manual_macd_line <- short_check - long_check
  cat("\nCross-check -- macd_line matches independently-recomputed EMA difference:",
      isTRUE(all.equal(manual_macd_line, macd_result$macd_line)), "\n")

  manual_histogram <- macd_result$macd_line - macd_result$signal_line
  cat("Cross-check -- histogram == macd_line - signal_line for every element:",
      isTRUE(all.equal(manual_histogram, macd_result$histogram)), "\n")
}
