# ==============================================================================
# linreg.R -- Linear Regression
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
# Author: Onuegbo Nnaemeka Philip
# ==============================================================================

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Calculate the total number of elements in the regressionSource
  n <- length(regressionSource)

  # Check if regressionLength is greater than the number of elements in regressionSource
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }

  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  # Calculate the starting index for the regressionSource
  start_index <- max(1, n - regressionLength + regressionOffset)

  # Calculate the ending index for the regressionSource
  end_index <- min(n, n - regressionOffset)

  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]

  # Calculate the index values for the regression points
  index_values <- seq_len(length(source_subset))

  # Calculate the sum of index values and the sum of source_subset
  sum_index <- sum(index_values)
  sum_source <- sum(source_subset)

  # Calculate the mean of index values and the mean of source_subset
  mean_index <- mean(index_values)
  mean_source <- mean(source_subset)

  # Calculate the numerator and denominator for the linear regression formula
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  # Calculate the slope and intercept of the linear regression line
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # Calculate the predicted values for the regressionSource
  predicted_values <- slope * index_values + intercept

  # Return the slope, intercept, and predicted values as a list
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )

  return(result)
}

# ------------------------------------------------------------------------
# Standalone smoke test
# ------------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(100, 102, 105, 103, 108, 110, 115, 113, 118, 120)
  result <- linreg(data, regressionLength = 5, regressionOffset = 0)
  cat("slope:", result$slope, "\n")
  cat("intercept:", result$intercept, "\n")
  cat("predicted_values:\n"); print(result$predicted_values)

  # Independent cross-check against R's own lm() on the same subset (using
  # lm() only to VERIFY the manual calculation, not to compute the actual
  # result -- the function above never calls lm() itself)
  n <- length(data)
  regressionLength <- 5; regressionOffset <- 0
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  subset_check <- data[start_index:end_index]
  x_check <- seq_len(length(subset_check))
  lm_fit <- lm(subset_check ~ x_check)
  cat("\nCross-check against R's lm() on the same data subset:\n")
  cat("lm() slope:", coef(lm_fit)[2], "| linreg() slope:", result$slope,
      "| match:", isTRUE(all.equal(unname(coef(lm_fit)[2]), result$slope)), "\n")
  cat("lm() intercept:", coef(lm_fit)[1], "| linreg() intercept:", result$intercept,
      "| match:", isTRUE(all.equal(unname(coef(lm_fit)[1]), result$intercept)), "\n")

  # Edge case checks
  cat("\nEdge case -- regressionLength > length(data):\n")
  tryCatch(linreg(data, regressionLength = 50, regressionOffset = 0),
           error = function(e) cat("Correctly rejected:", conditionMessage(e), "\n"))

  cat("\nEdge case -- regressionOffset >= regressionLength:\n")
  tryCatch(linreg(data, regressionLength = 5, regressionOffset = 5),
           error = function(e) cat("Correctly rejected:", conditionMessage(e), "\n"))
}
