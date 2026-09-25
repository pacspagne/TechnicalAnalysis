# Assignment 5 -- Technical Analysis using R (Development Phase)

BDA400 -- Data Science Tools and Techniques
Author: Onuegbo Nnaemeka Philip

Second stage of the 3-part project (builds on Assignment 1/2's stock data).

## Structure

```
Assignment5/
  scripts/
    sma.R           # Simple Moving Average
    ema.R           # Exponential Moving Average
    macd.R          # MACD (depends on ema.R)
    stdev.R         # Standard Deviation (population formula)
    linreg.R        # Linear Regression
    rsi.R           # Relative Strength Index
    stoch_rsi.R     # Stochastic RSI (depends on rsi.R, sma.R)
    crossover.R     # Crossover signal detector
    crossunder.R    # Crossunder signal detector
  output/           # Genuine console output from running every script
  run_all_indicators.R   # Applies all 9 indicators to real AAPL data
  AAPL.csv          # Real historical data (from Assignment 2) used for the demo
  Onuegbo_Nnaemeka_BDA400_A05_Report.docx   # Full documentation + cover page
```

## Running

Each script is self-testing: `Rscript scripts/<name>.R` defines the function
and runs its own verification tests. To see all 9 indicators applied to real
market data at once: `Rscript run_all_indicators.R`.

No external R packages are used anywhere -- every function relies only on
base R, per the assignment's constraint.

See `Onuegbo_Nnaemeka_BDA400_A05_Report.docx` for full implementation notes,
including three discrepancies found in the assignment's own pseudocode/examples
during testing and verification.
