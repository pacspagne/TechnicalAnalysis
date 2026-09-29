# ==============================================================================
# test_chart_helpers.R -- Verification for Steps 2-4 logic, run outside of the
# Shiny app itself so the calculations can be checked directly.
# Pattern: primary calculation -> independent cross-check -> self spot-check,
# same as Assignments 2, 4 and 5 of this project.
# BDA400 Assignment 6: Technical Analysis using R, Visualization Phase
# Author: Onuegbo Nnaemeka Philip
# ==============================================================================

source("scripts/load_data.R")
source("scripts/chart_helpers.R")

cat("======================================================================\n")
cat("STEP 1 CHECK -- real data loads correctly (see load_data.R output above)\n")
cat("======================================================================\n\n")

stock_data <- get_stock_data("AAPL", "2016-01-01", "2018-02-07", data_source = "csv")

cat("======================================================================\n")
cat("STEP 2 CHECK -- filtering + data.frame conversion\n")
cat("======================================================================\n")
filtered <- filter_stock_data(stock_data, c("2017-01-01", "2018-02-07"), "Daily")
df <- stock_to_df(filtered)
cat("Filtered to", nrow(df), "rows, from", as.character(min(df$Date)),
    "to", as.character(max(df$Date)), "\n")

# Cross-check: the filtered range should be a strict subset of the full data,
# and its row count should match a manual base-R subset of the original xts.
manual_check <- stock_data[paste0("2017-01-01/2018-02-07")]
cat("Cross-check -- row count via filter_stock_data():", nrow(df),
    "| via manual xts subset:", nrow(manual_check),
    "| match:", nrow(df) == nrow(manual_check), "\n\n")

# Weekly/monthly aggregation sanity check
weekly <- filter_stock_data(stock_data, c("2017-01-01", "2018-02-07"), "Weekly")
cat("Weekly aggregation:", nrow(weekly), "rows (should be far fewer than",
    nrow(df), "daily rows) --", nrow(weekly) < nrow(df), "\n\n")

cat("======================================================================\n")
cat("STEP 3 CHECK -- indicator overlays produce sane, non-degenerate values\n")
cat("======================================================================\n")
rsi_vals <- TTR::RSI(df$Close, n = 14)
cat("RSI(14): min =", round(min(rsi_vals, na.rm = TRUE), 2),
    "max =", round(max(rsi_vals, na.rm = TRUE), 2),
    "(should be within [0, 100]):",
    min(rsi_vals, na.rm = TRUE) >= 0 && max(rsi_vals, na.rm = TRUE) <= 100, "\n")

macd_result <- TTR::MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
cat("MACD line -- last 3 values:", paste(round(tail(na.omit(macd_result[, "macd"]), 3), 3), collapse = ", "), "\n")

# Spot-check RSI against Assignment 5's own from-scratch rsi() implementation,
# to confirm TTR::RSI (used here for the interactive dashboard) and the
# hand-built Wilder's-method function verified in Assignment 5 broadly agree
# in direction and magnitude on the same real data (TTR uses a slightly
# different warm-up convention, so exact equality isn't expected, but they
# should track closely once both have warmed up).
source("../Assignment5/scripts/rsi.R")
rsi_scratch <- rsi(df$Close, 14)
common <- !is.na(rsi_vals) & !is.na(rsi_scratch)
diffs <- abs(rsi_vals[common] - rsi_scratch[common])
cat("Cross-check -- mean absolute difference between TTR::RSI and Assignment 5's",
    "own rsi() on the same data (after warm-up):", round(mean(diffs), 2),
    "(expected to be small, both implement Wilder smoothing):",
    mean(diffs) < 5, "\n\n")

cat("======================================================================\n")
cat("STEP 4 CHECK -- trading rule signal generation\n")
cat("======================================================================\n")
signals <- compute_trading_signals(df, short_period = 20, long_period = 50)
cat("Signal counts:\n")
print(table(signals))

buy_count <- sum(signals == "Buy")
sell_count <- sum(signals == "Sell")
cat("\nBuy signals:", buy_count, "| Sell signals:", sell_count, "\n")

# Independent cross-check: manually recompute SMA(20)/SMA(50) with base R's
# own rolling mean (via filter()), find the first crossing point by hand, and
# confirm it lines up with what compute_trading_signals() reports.
n <- nrow(df)
short_ma_manual <- stats::filter(df$Close, rep(1/20, 20), sides = 1)
long_ma_manual <- stats::filter(df$Close, rep(1/50, 50), sides = 1)
above <- short_ma_manual > long_ma_manual
first_buy_manual <- which(above & !c(NA, head(above, -1)))[1]
first_buy_signal <- which(signals == "Buy")[1]
cat("First 'Buy' signal -- via compute_trading_signals():", first_buy_signal,
    "| via manual stats::filter() crossing check:", first_buy_manual,
    "| match:", isTRUE(all.equal(first_buy_signal, first_buy_manual, tolerance = 1)), "\n")

# Self spot-check: print the actual dates/prices around the first Buy signal
if (!is.na(first_buy_signal)) {
  window <- max(1, first_buy_signal - 1):min(n, first_buy_signal + 1)
  cat("\nSpot-check -- rows around the first Buy signal:\n")
  print(df[window, c("Date", "Close")])
  cat("Signal at that row:", signals[first_buy_signal], "\n")
}

cat("\n======================================================================\n")
cat("STEP 2/3/4 CHECK -- chart objects build without error\n")
cat("======================================================================\n")
p1 <- build_price_chart(df, chart_type = "Line", show_ma = TRUE, show_signals = TRUE)
p2 <- build_price_chart(df, chart_type = "Candlestick", show_ma = FALSE, show_signals = FALSE)
p3 <- build_price_chart(df, chart_type = "Area", show_ma = TRUE, show_signals = FALSE)
p4 <- build_rsi_chart(df)
p5 <- build_macd_chart(df)
cat("Line chart class:", paste(class(p1), collapse=", "), "\n")
cat("Candlestick chart class:", paste(class(p2), collapse=", "), "\n")
cat("Area chart class:", paste(class(p3), collapse=", "), "\n")
cat("RSI chart class:", paste(class(p4), collapse=", "), "\n")
cat("MACD chart class:", paste(class(p5), collapse=", "), "\n")

ggsave("output/test_line_chart.png", p1, width = 9, height = 5)
ggsave("output/test_candlestick_chart.png", p2, width = 9, height = 5)
ggsave("output/test_area_chart.png", p3, width = 9, height = 5)
ggsave("output/test_rsi_chart.png", p4, width = 9, height = 3)
ggsave("output/test_macd_chart.png", p5, width = 9, height = 3)
cat("\nSaved 5 sample charts to output/ for visual verification.\n")

cat("\n=== ALL CHECKS COMPLETE ===\n")
