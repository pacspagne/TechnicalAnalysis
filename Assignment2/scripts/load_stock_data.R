# ==============================================================================
# load_stock_data.R
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# Author: Onuegbo Nnaemeka Philip
#
# Purpose:
#   Reads the stock symbols listed in 'portfolio.txt' and imports the
#   historical daily price data for each symbol using the 'quantmod' package.
#   Each stock's data is loaded into R and stored as a separate xts/data frame
#   object, keyed by ticker symbol, inside a named list for easy access by
#   downstream scripts (calculate_statistics.R, display_output.R).
# ==============================================================================

library(quantmod)

#' Load historical stock data for every symbol listed in a portfolio file
#'
#' @param portfolio_file Path to a text file with one ticker symbol per line
#' @param data_dir       Folder containing <SYMBOL>.csv files (used when
#'                        src = "csv"); ignored when src = "yahoo"
#' @param src             Data source passed to quantmod::getSymbols().
#'                        "csv"   -> read local CSV files (offline / sandbox)
#'                        "yahoo" -> pull live data from Yahoo Finance
#' @param from, to        Optional date range (only used for src = "yahoo")
#'
#' @return A named list of data frames, one per stock symbol, each with
#'         columns: Date, Open, High, Low, Close, Volume, Adjusted
load_stock_data <- function(portfolio_file = "portfolio.txt",
                             data_dir = "data",
                             src = "csv",
                             from = "2013-01-01",
                             to = Sys.Date()) {

  if (!file.exists(portfolio_file)) {
    stop(paste("Portfolio file not found:", portfolio_file))
  }

  symbols <- readLines(portfolio_file, warn = FALSE)
  symbols <- trimws(symbols)
  symbols <- symbols[nzchar(symbols)]   # drop blank lines

  if (length(symbols) == 0) {
    stop("No stock symbols found in portfolio file.")
  }

  stock_list <- list()

  for (sym in symbols) {
    message(sprintf("Loading data for %s (src = %s)...", sym, src))

    tryCatch({
      if (src == "csv") {
        # Offline / sandbox-safe path: read a pre-downloaded CSV of real
        # historical OHLCV data in quantmod's expected column layout.
        xts_obj <- getSymbols(
          Symbols   = sym,
          src       = "csv",
          dir       = data_dir,
          extension = "csv",
          auto.assign = FALSE
        )
      } else {
        # Live path (works unmodified on a machine with normal internet
        # access, e.g. the student's own PC as instructed in the assignment).
        xts_obj <- getSymbols(
          Symbols     = sym,
          src         = "yahoo",
          from        = from,
          to          = to,
          auto.assign = FALSE
        )
      }

      df <- data.frame(
        Date   = zoo::index(xts_obj),
        Open   = as.numeric(xts_obj[, 1]),
        High   = as.numeric(xts_obj[, 2]),
        Low    = as.numeric(xts_obj[, 3]),
        Close  = as.numeric(xts_obj[, 4]),
        Volume = as.numeric(xts_obj[, 5]),
        Adjusted = as.numeric(xts_obj[, 6])
      )
      rownames(df) <- NULL

      stock_list[[sym]] <- df
      message(sprintf("  -> %d rows loaded for %s (%s to %s)",
                       nrow(df), sym, min(df$Date), max(df$Date)))

    }, error = function(e) {
      warning(sprintf("Failed to load data for %s: %s", sym, conditionMessage(e)))
    })
  }

  if (length(stock_list) == 0) {
    stop("No stock data could be loaded for any symbol.")
  }

  return(stock_list)
}

# ------------------------------------------------------------------------
# Standalone execution / smoke test
# ------------------------------------------------------------------------
if (sys.nframe() == 0) {
  stocks <- load_stock_data(
    portfolio_file = "portfolio.txt",
    data_dir       = "data",
    src            = "csv"
  )

  cat("\n================ LOAD SUMMARY ================\n")
  for (sym in names(stocks)) {
    cat(sprintf("%-6s : %4d rows | %s -> %s\n",
                sym, nrow(stocks[[sym]]),
                min(stocks[[sym]]$Date), max(stocks[[sym]]$Date)))
  }
  cat("================================================\n")

  saveRDS(stocks, file = "output/stocks_data.rds")
  cat("\nSaved combined stock list to output/stocks_data.rds\n")
}
