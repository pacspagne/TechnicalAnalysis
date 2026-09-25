# ==============================================================================
# stdev.R -- Standard Deviation
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
# Author: Onuegbo Nnaemeka Philip
#
# NOTE: the assignment's pseudocode explicitly divides by n (the population
# formula), not n-1 (the sample formula R's built-in sd() uses). This
# implementation follows the assignment's pseudocode exactly, so its result
# will differ slightly from sd() -- documented and cross-checked below.
# ==============================================================================

stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- data - mean_value

  # Calculate the squared differences
  squared_diff <- diff_values * diff_values

  # Calculate the variance (mean of squared differences) -- population variance
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}

# ------------------------------------------------------------------------
# Standalone smoke test
# ------------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  stdev_result <- stdev(data)
  cat("stdev(data):", stdev_result, "\n")

  # Independent cross-check against R's built-in sd(), adjusting for the
  # population-vs-sample divisor difference (n vs n-1)
  n <- length(data)
  sd_builtin_sample <- sd(data)                       # divides by n-1
  sd_builtin_population <- sd(data) * sqrt((n - 1) / n)  # convert to population version
  cat("R's sd() [sample, /n-1]:", sd_builtin_sample, "\n")
  cat("R's sd() converted to population [/n]:", sd_builtin_population, "\n")
  cat("Match against population-adjusted sd()?",
      isTRUE(all.equal(stdev_result, sd_builtin_population)), "\n")
}
