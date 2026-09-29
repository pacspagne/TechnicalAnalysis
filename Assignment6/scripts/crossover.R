# ==============================================================================
# crossover.R -- Crossover function
# BDA400 Assignment 6: Technical Analysis using R, Visualization Phase
# (function carried over unchanged from Assignment 5, re-verified below)
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
