library(shiny)
library(DT)
library(data.table)
library(quantmod)
library(ggplot2)
library(plotly)

# Grabs data for up to 5 stocks, and plots their closing price histories
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stock Ticker"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      h4('Select a date range.'),
      h4('Enter up to 5 stock symbols in the fields below, 
        then press "LOCK IN SELECTIONS".'),
      h4('Press "PLOT IT" to, you know, plot it.'),
      
      dateRangeInput("thedates", "Select your date range.", 
                     start = "2020-01-01"),
      textInput("stock1", "Enter a stock ticker.", "AAPL"),
      textInput("stock2", "Enter a stock ticker.", "AMZN"),
      textInput("stock3", "Enter a stock ticker.", "MSFT"),
      textInput("stock4", "Enter a stock ticker.", "GOOG"),
      textInput("stock5", "Enter a stock ticker.", "FB"),
      actionButton("go", "LOCK IN SELECTIONS."),
      actionButton("plotit", "PLOT IT."),
      h3("Will Kramlinger, 10/13/2020")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("dankplot"),
      DT::dataTableOutput("danktable")
    )
  )
))
