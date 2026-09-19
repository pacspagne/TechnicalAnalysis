# ==============================================================================
# calculate_statistics.R
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# Author: Onuegbo Nnaemeka Philip
#
# Purpose:
#   Provides calculate_statistics(), which takes a single stock's data frame
#   (as produced by load_stock_data.R) and computes the basic technical /
#   descriptive statistics required by the assignment: moving average, mean,
#   mode, median, and standard deviation of the closing price. Also computes
#   a couple of extra, commonly-used technical indicators (20-day and 50-day
#   SMA via the TTR package) since the rubric explicitly calls out "moving
#   average" as its own line item and real technical-analysis workflows
#   almost always show more than one moving-average window.
# ==============================================================================

library(TTR)

#' Statistical mode (base R has no built-in mode() for this purpose --
#' mode() in base R returns the storage type of an object, not the
#' statistical mode, so we implement it explicitly).
#'
#' For continuous price data with many unique values, the raw mode is
#' usually just "the value that happens to repeat," so we mode the price
#' after rounding to whole dollars, which is what's typically reported as
#' the modal closing price.
#'
#' @param x Numeric vector
#' @param digits Number of decimal places to round to before finding the mode
#' @return The most frequently occurring rounded value (numeric)
calc_mode <- function(x, digits = 0) {
  x <- x[!is.na(x)]
  rounded <- round(x, digits)
  freq_table <- table(rounded)
  mode_val <- as.numeric(names(freq_table)[freq_table == max(freq_table)])
  # If multiple values tie for most frequent, return the smallest (documented choice)
  mode_val[1]
}

#' Calculate basic descriptive and technical statistics for one stock
#'
#' @param stock_df Data frame with at least a Date and Close column, as
#'                  returned by load_stock_data()
#' @param symbol    Ticker symbol, used only for labeling in the output
#' @param ma_periods Integer vector of moving-average window lengths (days)
#'
#' @return A named list:
#'   $summary  -- one-row data frame of symbol, mean, median, mode, sd, n
#'   $moving_averages -- data frame of Date, Close, and one SMA column per
#'                        period requested (NA for the first period-1 rows)
calculate_statistics <- function(stock_df, symbol = "UNKNOWN", ma_periods = c(20, 50)) {

  if (!all(c("Date", "Close") %in% names(stock_df))) {
    stop("stock_df must contain 'Date' and 'Close' columns")
  }

  close <- stock_df$Close

  stats_summary <- data.frame(
    Symbol   = symbol,
    N        = length(close),
    Mean     = mean(close, na.rm = TRUE),
    Median   = median(close, na.rm = TRUE),
    Mode     = calc_mode(close, digits = 0),
    SD       = sd(close, na.rm = TRUE),
    Min      = min(close, na.rm = TRUE),
    Max      = max(close, na.rm = TRUE)
  )

  ma_df <- data.frame(Date = stock_df$Date, Close = close)
  for (p in ma_periods) {
    col_name <- paste0("SMA_", p)
    ma_df[[col_name]] <- as.numeric(TTR::SMA(close, n = p))
  }

  list(
    summary = stats_summary,
    moving_averages = ma_df
  )
}

# ------------------------------------------------------------------------
# Standalone execution / smoke test
# ------------------------------------------------------------------------
if (sys.nframe() == 0) {

  if (!file.exists("output/stocks_data.rds")) {
    source("scripts/load_stock_data.R")
    stocks <- load_stock_data(portfolio_file = "portfolio.txt", data_dir = "data", src = "csv")
  } else {
    stocks <- readRDS("output/stocks_data.rds")
  }

  all_summaries <- data.frame()
  all_ma <- list()

  for (sym in names(stocks)) {
    result <- calculate_statistics(stocks[[sym]], symbol = sym, ma_periods = c(20, 50))
    all_summaries <- rbind(all_summaries, result$summary)
    all_ma[[sym]] <- result$moving_averages

    # Independent cross-check: recompute mean and sd "by hand" (sum/n and
    # the textbook variance formula) and confirm they match R's built-ins
    # to within floating-point tolerance, per the course's required
    # verification pattern (primary calc + independent cross-check).
    close_vals <- stocks[[sym]]$Close
    n <- length(close_vals)
    manual_mean <- sum(close_vals) / n
    manual_sd <- sqrt(sum((close_vals - manual_mean)^2) / (n - 1))

    mean_ok <- isTRUE(all.equal(manual_mean, result$summary$Mean))
    sd_ok   <- isTRUE(all.equal(manual_sd, result$summary$SD))

    cat(sprintf("[%s] cross-check -- mean match: %s | sd match: %s\n",
                sym, mean_ok, sd_ok))

    if (!mean_ok || !sd_ok) {
      stop(sprintf("Cross-check FAILED for %s -- verification pattern violated", sym))
    }
  }

  cat("\n================ STATISTICS SUMMARY ================\n")
  print(all_summaries, row.names = FALSE)
  cat("======================================================\n")

  saveRDS(all_summaries, file = "output/statistics_summary.rds")
  saveRDS(all_ma, file = "output/moving_averages.rds")
  write.csv(all_summaries, file = "output/statistics_summary.csv", row.names = FALSE)
  cat("\nSaved statistics_summary.rds / .csv and moving_averages.rds to output/\n")
}
