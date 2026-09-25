# ==============================================================================
# rsi.R -- Relative Strength Index
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
# Author: Onuegbo Nnaemeka Philip
# ==============================================================================

rsi <- function(data, period) {
  n <- length(data)

  # Calculate the differences between consecutive data points
  diff_values <- diff(data)

  # Initialize two vectors to store the gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Calculate the average gains and average losses (over the first 'period' diffs)
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, n)

  # Calculate RSI values using Wilder's smoothing method
  for (i in (period + 1):n) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    if (avg_loss == 0) {
      # No losses at all in the smoothed window -- RSI is defined as 100
      # (this edge case isn't covered by the assignment's pseudocode, which
      # would otherwise divide by zero here)
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}

# ------------------------------------------------------------------------
# Standalone smoke test (matches the example given in the assignment PDF)
# ------------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
  rsi_result <- rsi(data, period = 5)
  cat("rsi(data, period = 5):\n")
  print(rsi_result)

  # Independent cross-check, replicating the pseudocode's exact two steps
  # (NOT the textbook Wilder formula, which would skip the re-smoothing of
  # the period-th element -- this implementation follows the assignment's
  # pseudocode literally, so the cross-check must too, to be a fair test):
  #   step A: avg_gain/avg_loss = mean of the first 'period' gains/losses
  #   step B: the loop's first iteration (i = period+1) re-applies Wilder's
  #           smoothing using gains[period]/losses[period] AGAIN
  diffs <- diff(data)
  manual_gains <- ifelse(diffs > 0, diffs, 0)
  manual_losses <- ifelse(diffs < 0, abs(diffs), 0)
  step_a_avg_gain <- mean(manual_gains[1:5])
  step_a_avg_loss <- mean(manual_losses[1:5])
  step_b_avg_gain <- (step_a_avg_gain * (5 - 1) + manual_gains[5]) / 5
  step_b_avg_loss <- (step_a_avg_loss * (5 - 1) + manual_losses[5]) / 5
  cat("\nCross-check -- step A avg_gain/avg_loss (first 5 diffs):",
      step_a_avg_gain, "/", step_a_avg_loss, "\n")
  cat("Cross-check -- step B avg_gain/avg_loss (after first smoothing pass):",
      step_b_avg_gain, "/", step_b_avg_loss, "\n")
  manual_rs <- step_b_avg_gain / step_b_avg_loss
  manual_rsi_6 <- 100 - (100 / (1 + manual_rs))
  cat("Manually computed RSI at index 6:", manual_rsi_6,
      "| function's rsi_values[6]:", rsi_result[6],
      "| match:", isTRUE(all.equal(manual_rsi_6, rsi_result[6])), "\n")

  cat("\nRSI values are all NA for indices 1 to period (5), as expected:",
      all(is.na(rsi_result[1:5])), "\n")
}
