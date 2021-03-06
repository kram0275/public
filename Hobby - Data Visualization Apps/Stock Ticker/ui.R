library(shiny)
library(DT)
library(data.table)
library(quantmod)
library(ggplot2)
library(plotly)

# Grabs data for up to 5 stocks, and plots their closing price histories
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stock Performance"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      h4('Select a date range.'),
      h4('Enter up to 5 stock symbols in the fields below, and press "1. Select 
         Symbols".'),
      h4('Press "2. Update Plot and Table" to, you know, plot it.'),
      
      dateRangeInput("thedates", "Select your date range.", 
                     start = "2020-01-01"),
      textInput("stock1", "Enter a stock symbol.", "AAPL"),
      textInput("stock2", "Enter a stock symbol.", "AMZN"),
      textInput("stock3", "Enter a stock symbol.", "MSFT"),
      textInput("stock4", "Enter a stock symbol.", "GOOG"),
      textInput("stock5", "Enter a stock symbol.", "FB"),
      actionButton("go", "1. Select Symbols"),
      actionButton("plotit", "2. Update Plot and Table"),
      h4("Will Kramlinger, 10/13/2020")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("dankplot"),
      DT::dataTableOutput("danktable")
    )
  )
))
