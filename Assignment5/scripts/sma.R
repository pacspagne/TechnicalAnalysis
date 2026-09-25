# ==============================================================================
# sma.R -- Simple Moving Average
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
# Author: Onuegbo Nnaemeka Philip
#
# Implements the sma() function exactly per the assignment's template/naming
# and pseudocode, using only R's core functions (no external libraries).
# ==============================================================================

sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values
  n_values <- length(data) - period + 1
  sma_values <- numeric(n_values)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:n_values) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }

  return(sma_values)
}

# ------------------------------------------------------------------------
# Standalone smoke test (matches the example given in the assignment PDF)
# ------------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  sma_result <- sma(data, period = 3)
  cat("sma(data, period = 3):\n")
  print(sma_result)

  # Manual cross-check of the first value: mean(10, 12, 15)
  cat("\nManual cross-check of sma_result[1]: mean(c(10,12,15)) =",
      mean(c(10, 12, 15)), "| sma_result[1] =", sma_result[1],
      "| match:", isTRUE(all.equal(mean(c(10, 12, 15)), sma_result[1])), "\n")

  # Edge case: period longer than data should error
  cat("\nEdge case -- period > length(data):\n")
  tryCatch(sma(c(1, 2), period = 5),
           error = function(e) cat("Correctly rejected:", conditionMessage(e), "\n"))
}
