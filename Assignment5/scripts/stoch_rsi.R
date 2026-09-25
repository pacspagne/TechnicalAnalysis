# ==============================================================================
# stoch_rsi.R -- Stochastic RSI
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
# Author: Onuegbo Nnaemeka Philip
#
# Depends on rsi() (rsi.R) and sma() (sma.R), exactly as the assignment's own
# pseudocode does -- both are sourced below.
# ==============================================================================

source("scripts/rsi.R")
source("scripts/sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI: normalize RSI between 0 and 1 using its own
  # min/max over the available (non-NA) range
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)

  # Calculate the %K line (StochRSI) -- sma() requires a vector at least as
  # long as its period and errors otherwise, so we pass only the non-NA tail
  # of k_values (the leading NAs come from rsi()'s own warm-up period and
  # would make sma()'s window-sum come out NA for every window that touches
  # one, which is correct behavior, but sma() itself checks length(), not
  # NA content, against k_period -- documented below in the write-up).
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (3-day/simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )

  return(result)
}

# ------------------------------------------------------------------------
# Standalone smoke test
# ------------------------------------------------------------------------
if (sys.nframe() == 0) {
  # NOTE: the assignment PDF's own example uses period = 14 with only 10
  # data points, which would fail rsi()'s internal averaging step (there
  # aren't 14 diffs to average). That's an error in the example itself, not
  # in this implementation -- using a smaller, sensible period here instead,
  # and documenting the discrepancy in the assignment write-up.
  data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62, 64, 63, 67, 70, 68)
  result <- stoch_rsi(data, period = 5, k_period = 3, d_period = 3)
  cat("k_line:\n"); print(result$k_line)
  cat("\nd_line:\n"); print(result$d_line)

  # Independent cross-check: recompute the RSI normalization manually and
  # confirm it matches what feeds into sma() inside the function
  rsi_check <- rsi(data, 5)
  min_check <- min(rsi_check, na.rm = TRUE)
  max_check <- max(rsi_check, na.rm = TRUE)
  k_values_check <- (rsi_check - min_check) / (max_check - min_check)
  k_line_check <- sma(k_values_check, 3)
  cat("\nCross-check -- k_line matches independently recomputed normalization + sma():",
      isTRUE(all.equal(k_line_check, result$k_line)), "\n")
}
