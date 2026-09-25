# ==============================================================================
# crossunder.R -- Crossunder function
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
# Author: Onuegbo Nnaemeka Philip
# ==============================================================================

crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- character(length(arr1))
  crossunder_signals[1] <- "None"

  # Check for crossunder signals at each data point
  for (i in 2:length(arr1)) {
    if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossunder_signals[i] <- "True"
    } else {
      crossunder_signals[i] <- "False"
    }
  }

  return(crossunder_signals)
}

# ------------------------------------------------------------------------
# Standalone smoke test (matches the example given in the assignment PDF)
# ------------------------------------------------------------------------
if (sys.nframe() == 0) {
  # NOTE: the assignment PDF's own example arrays for crossunder() (identical
  # to the crossover() example) only ever cross UPWARD once and never cross
  # back down -- so that sample produces zero "True" crossunder events, which
  # isn't a very useful test of this function. Using a version that both
  # rises above and falls back below, to genuinely exercise both branches.
  arr1 <- c(10, 12, 15, 20, 18, 14, 11, 9, 13)
  arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
  crossunder_signals <- crossunder(arr1, arr2)
  cat("crossunder(arr1, arr2):\n")
  print(crossunder_signals)

  # Self spot-check: index 2 is where arr1 (12) is still below arr2 (20),
  # coming from index 1 where arr1 (10) was also below arr2 (18) -- so
  # arr1[1] < arr2[1] already, meaning there's no crossunder at index 2
  # since arr1 was already below, not >=, at the previous point. Verify
  # the first TRUE occurs at the first point where arr1 drops from >=
  # arr2 to < arr2.
  first_true_index <- which(crossunder_signals == "True")[1]
  cat("\nFirst 'True' crossunder at index:", first_true_index, "\n")
  cat("arr1[(first_true_index-1):first_true_index] =",
      arr1[(first_true_index - 1):first_true_index], "\n")
  cat("arr2[(first_true_index-1):first_true_index] =",
      arr2[(first_true_index - 1):first_true_index], "\n")
  cat("Confirms arr1 was >= arr2 previously and < arr2 now:",
      arr1[first_true_index] < arr2[first_true_index] &&
        arr1[first_true_index - 1] >= arr2[first_true_index - 1], "\n")

  # Edge case: mismatched lengths should error
  cat("\nEdge case -- mismatched array lengths:\n")
  tryCatch(crossunder(c(1, 2, 3), c(1, 2)),
           error = function(e) cat("Correctly rejected:", conditionMessage(e), "\n"))
}
