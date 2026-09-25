# ==============================================================================
# crossover.R -- Crossover function
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
# Author: Onuegbo Nnaemeka Philip
#
# NOTE on a discrepancy in the assignment brief: the narrative description
# defines Crossover as a plain TRUE/FALSE boolean array, but the assignment's
# OWN pseudocode for crossover() returns three-way string labels
# ("Up" / "Down" / "None"), matching the two-way "True"/"False" pattern used
# in the separate crossunder() pseudocode instead. Since the assignment says
# the provided pseudocode is what to implement ("adhering to the pseudocode
# algorithms"), this function follows the pseudocode literally (string
# labels), not the narrative boolean description -- documented here and in
# the assignment write-up so the discrepancy is explicit rather than silently
# picked one way.
# ==============================================================================

crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossover signals
  crossover_signals <- character(length(arr1))
  crossover_signals[1] <- "None"

  # Check for crossovers at each data point
  for (i in 2:length(arr1)) {
    if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
      crossover_signals[i] <- "Up"
    } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossover_signals[i] <- "Down"
    } else {
      crossover_signals[i] <- "None"
    }
  }

  return(crossover_signals)
}

# ------------------------------------------------------------------------
# Standalone smoke test (matches the example given in the assignment PDF)
# ------------------------------------------------------------------------
if (sys.nframe() == 0) {
  arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
  crossover_signals <- crossover(arr1, arr2)
  cat("crossover(arr1, arr2):\n")
  print(crossover_signals)

  # Self spot-check: index 4 is where arr1 (20) first exceeds arr2 (18)
  # after being below/equal at index 3 (15 <= 22) -- should be "Up"
  cat("\nSpot-check -- index 4 should be 'Up' (arr1 crosses above arr2):",
      crossover_signals[4] == "Up", "\n")
  cat("arr1[3:4] =", arr1[3:4], "| arr2[3:4] =", arr2[3:4], "\n")

  # Edge case: mismatched lengths should error
  cat("\nEdge case -- mismatched array lengths:\n")
  tryCatch(crossover(c(1, 2, 3), c(1, 2)),
           error = function(e) cat("Correctly rejected:", conditionMessage(e), "\n"))
}
