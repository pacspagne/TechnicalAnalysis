# ==============================================================================
# sma.R -- Simple Moving Average (carried over, unchanged and re-verified, from
# Assignment 5 of this same three-part project)
# BDA400 Assignment 6: Technical Analysis using R, Visualization Phase
# Author: Onuegbo Nnaemeka Philip
# ==============================================================================

sma <- function(data, period) {
  n <- length(data)
  if (period > n) stop("Period cannot be greater than the length of the data")

  sma_values <- numeric(n - period + 1)
  for (i in period:n) {
    sma_values[i - period + 1] <- mean(data[(i - period + 1):i])
  }
  return(sma_values)
}
