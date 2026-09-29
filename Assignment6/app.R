# ==============================================================================
# app.R -- BDA400 Assignment 6: Technical Analysis using R, Visualization Phase
# Portfolio dashboard built with R Shiny, ggplot2 and quantmod.
# Author: Onuegbo Nnaemeka Philip
#
# Third stage of the three-part Technical Analysis project:
#   Assignment 2 -> data collection & summary statistics (this app's Step 1
#                   reuses that same real historical OHLCV dataset)
#   Assignment 5 -> from-scratch, individually verified indicator functions
#                   (sma() and crossover() are sourced directly from that work
#                   in scripts/sma.R and scripts/crossover.R)
#   Assignment 6 -> this file: an interactive Shiny dashboard that visualizes
#                   the data, overlays indicators, and annotates trading
#                   signals generated from those same verified functions
#
# See scripts/load_data.R for the data-source note (Yahoo Finance is what the
# assignment specifies; this app defaults to a local CSV of real historical
# data because live Yahoo access is blocked in the sandbox used to build and
# test this app -- flip data_source to "yahoo" in the sidebar to use the
# assignment's exact Step 1 code path on a normal internet connection).
# ==============================================================================

source("scripts/load_data.R")
source("scripts/chart_helpers.R")

# ------------------------------------------------------------------------
# Step 1: Data Collection and Setup (packages loaded, data-fetch function
# defined in scripts/load_data.R, sourced above)
# ------------------------------------------------------------------------
available_symbols <- c("AAPL", "MSFT", "GOOGL", "AMZN", "JPM")
data_min_date <- as.Date("2013-02-08")
data_max_date <- as.Date("2018-02-07")

# ==============================================================================
# Step 2: Visualizing Stock Data -- UI
# ==============================================================================
ui <- fluidPage(
  titlePanel("Portfolio Technical Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      h4("Step 1: Data"),
      selectInput("stock_symbol", "Stock Symbol:", choices = available_symbols, selected = "AAPL"),
      radioButtons("data_source", "Data Source:",
                   choices = c("Local CSV (sandbox-safe, real historical data)" = "csv",
                               "Yahoo Finance (live, per assignment Step 1)" = "yahoo"),
                   selected = "csv"),
      dateRangeInput("date_range", "Select Date Range:",
                      start = "2016-06-01", end = as.character(data_max_date),
                      min = data_min_date, max = data_max_date),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly"), selected = "Daily"),

      hr(),
      h4("Step 2: Chart Type"),
      selectInput("chart_type", "Price Chart Type:",
                  choices = c("Line", "Area", "Candlestick"), selected = "Line"),

      hr(),
      h4("Step 3: Technical Indicators"),
      checkboxGroupInput("technical_indicators", "Overlay / show indicators:",
                          choices = c("Moving Averages", "RSI", "MACD"),
                          selected = c("Moving Averages")),
      conditionalPanel(
        condition = "input.technical_indicators.includes('Moving Averages') || input.show_signals",
        numericInput("short_period", "Short MA period:", value = 20, min = 2, max = 100),
        numericInput("long_period", "Long MA period:", value = 50, min = 5, max = 200)
      ),

      hr(),
      h4("Step 4: Trading Rule & Annotations"),
      checkboxInput("show_signals", "Show Buy/Sell signal annotations (MA crossover rule)", value = TRUE),
      helpText("Rule: Buy when the short-term SMA crosses above the long-term SMA;",
               "Sell when it crosses below; otherwise Hold.",
               "Uses the same sma() and crossover() functions verified in Assignment 5."),

      hr(),
      textOutput("signal_summary")
    ),

    mainPanel(
      plotOutput("stock_chart", height = "420px"),
      conditionalPanel(
        condition = "input.technical_indicators.includes('RSI')",
        plotOutput("rsi_chart", height = "220px")
      ),
      conditionalPanel(
        condition = "input.technical_indicators.includes('MACD')",
        plotOutput("macd_chart", height = "220px")
      )
    )
  )
)

# ==============================================================================
# Server logic
# ==============================================================================
server <- function(input, output, session) {

  # --- Step 1: fetch the raw data for the selected symbol/source ---
  raw_stock_data <- reactive({
    get_stock_data(input$stock_symbol,
                    from = data_min_date, to = data_max_date,
                    data_source = input$data_source)
  })

  # --- Step 2: filter by date range and time frame, convert to data.frame ---
  filtered_df <- reactive({
    req(input$date_range)
    filtered <- filter_stock_data(raw_stock_data(), input$date_range, input$time_frame)
    stock_to_df(filtered)
  })

  # --- Step 2/3/4: main price chart with optional MA overlay + signals ---
  output$stock_chart <- renderPlot({
    df <- filtered_df()
    validate(need(nrow(df) > 1, "Not enough data in the selected range. Widen the date range."))

    build_price_chart(
      df,
      chart_type = input$chart_type,
      show_ma = "Moving Averages" %in% input$technical_indicators,
      short_period = input$short_period,
      long_period = input$long_period,
      show_signals = input$show_signals
    )
  })

  # --- Step 3: RSI sub-chart (toggle on/off via the checkbox group) ---
  output$rsi_chart <- renderPlot({
    df <- filtered_df()
    validate(need(nrow(df) > 14, "Not enough data to compute RSI in this range."))
    build_rsi_chart(df, period = 14)
  })

  # --- Step 3: MACD sub-chart (toggle on/off via the checkbox group) ---
  output$macd_chart <- renderPlot({
    df <- filtered_df()
    validate(need(nrow(df) > 26, "Not enough data to compute MACD in this range."))
    build_macd_chart(df)
  })

  # --- Step 4: text summary of the trading rule's signal counts ---
  output$signal_summary <- renderText({
    df <- filtered_df()
    if (nrow(df) <= input$long_period) return("Not enough data to generate signals yet.")
    signals <- compute_trading_signals(df, input$short_period, input$long_period)
    paste0("Signals in range -- Buy: ", sum(signals == "Buy"),
           " | Sell: ", sum(signals == "Sell"),
           " | Hold: ", sum(signals == "Hold"))
  })
}

shinyApp(ui = ui, server = server)
