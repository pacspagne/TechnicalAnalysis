# BDA400 Assignment 6 -- Technical Analysis using R, Visualization Phase

**Author:** Onuegbo Nnaemeka Philip
**Course:** BDA400 -- Data Science Tools and Techniques
**Assignment:** Assignment 6 -- Technical Analysis using R, Visualization Phase

Third and final stage of the three-part Technical Analysis project:
- **Assignment 2** -- data collection & summary statistics
- **Assignment 5** -- from-scratch, individually verified indicator functions (`sma()`, `rsi()`, `macd()`, `crossover()`, etc.)
- **Assignment 6** (this repo folder) -- an interactive R Shiny dashboard that visualizes the data, overlays technical indicators, and annotates trading signals

## Data source note

The assignment specifies Yahoo Finance (`src = "yahoo"`) as the data source, and `get_stock_data()` in `scripts/load_data.R` implements exactly that code path. This development environment's network access blocks live Yahoo Finance requests, so the Shiny app's sidebar defaults `data_source` to `"csv"`, which points `quantmod::getSymbols()` at the same real historical OHLCV data (AAPL, MSFT, GOOGL, AMZN, JPM, 2013-02-08 to 2018-02-07) already collected and used in Assignment 2. No figures are invented -- this is genuine market data, just read from a local file instead of the (blocked) Yahoo API. On a normal internet connection, selecting "Yahoo Finance (live)" in the sidebar uses the assignment's exact Step 1 code with no other changes needed.

## Repository structure

```
Assignment6/
+-- app.R                        # The Shiny app (UI + server), Steps 2-4
+-- data/                        # Real historical OHLCV CSVs (from Assignment 2)
+-- scripts/
|   +-- load_data.R              # Step 1: data collection + get_stock_data()
|   +-- sma.R                    # Simple Moving Average (carried over from Assignment 5)
|   +-- crossover.R              # Crossover signal function (carried over from Assignment 5)
|   +-- chart_helpers.R          # Steps 2-4 logic: filtering, chart builders, trading rule
|   +-- test_chart_helpers.R     # Standalone verification script (see AI declaration above)
+-- output/                      # Saved console logs + static chart PNGs from verification
+-- screenshots/                 # Real screenshots of the running, interactive app
+-- README.md
```

## How to run

```r
setwd("Assignment6")
shiny::runApp("app.R")
```

Required packages: `shiny`, `ggplot2`, `quantmod`, `TTR`, `xts`, `zoo` (all already installed for this project since Assignment 2).

## Dashboard walkthrough

**Step 1 -- Data Collection and Setup:** Choose a stock symbol (AAPL, MSFT, GOOGL, AMZN, JPM), a data source (local CSV or live Yahoo Finance), and a date range. `get_stock_data()` fetches the historical OHLCV series via `quantmod::getSymbols()`.

**Step 2 -- Visualizing Stock Data:** A `dateRangeInput` and a `selectInput` for time frame (Daily/Weekly/Monthly) filter and aggregate the data. A second `selectInput` picks the chart type -- Line, Area, or a genuine Candlestick built from `geom_segment()` (high-low wicks) and `geom_rect()` (open-close bodies), colored green/red for up/down days.

**Step 3 -- Overlay Technical Indicators:** A `checkboxGroupInput` lets the user turn Moving Averages, RSI, and MACD on or off independently. Moving Averages overlay directly on the price chart; RSI and MACD render as separate sub-panels below it (their own y-axis scale), each appearing/disappearing live as the checkboxes are toggled -- see `screenshots/02_rsi_macd_on.png` vs. `screenshots/01_default_view.png`.

**Step 4 -- Trading Rules and Annotations:** A simple Moving Average crossover rule (Buy when the short-term SMA crosses above the long-term SMA, Sell when it crosses below, otherwise Hold) is implemented in `compute_trading_signals()`, reusing the exact `sma()` and `crossover()` functions built and verified in Assignment 5. When "Show Buy/Sell signal annotations" is checked, Buy/Sell points are marked directly on the price chart with colored triangles and text labels at the price where the crossover occurred (see `screenshots/01_default_view.png`; unchecking it removes them, see `screenshots/04_indicators_off.png`).

## Repository link

https://github.com/pacspagne/TechnicalAnalysis/tree/main/Assignment6
