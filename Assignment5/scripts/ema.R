# ==============================================================================
# ema.R -- Exponential Moving Average
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
# Author: Onuegbo Nnaemeka Philip
# ==============================================================================

ema <- function(data, period) {
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty array to store EMA values
  ema_values <- numeric(length(data))

  # Loop through the data array
  for (i in seq_along(data)) {
    if (i == 1) {
      # Calculate EMA for the first data point
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}

# ------------------------------------------------------------------------
# Standalone smoke test (matches the example given in the assignment PDF)
# ------------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  ema_result <- ema(data, period = 3)
  cat("ema(data, period = 3):\n")
  print(ema_result)

  # Manual cross-check: first value should equal data[1]; second value
  # should equal (data[2]-data[1])*multiplier + data[1] with multiplier=0.5
  multiplier <- 2 / (3 + 1)
  manual_second <- (data[2] - data[1]) * multiplier + data[1]
  cat("\nManual cross-check -- ema_result[1] == data[1]:",
      isTRUE(all.equal(ema_result[1], data[1])), "\n")
  cat("Manual cross-check -- ema_result[2] == manual calc:",
      isTRUE(all.equal(ema_result[2], manual_second)),
      "(manual =", manual_second, ", function =", ema_result[2], ")\n")
}
