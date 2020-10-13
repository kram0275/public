library(shiny)
library(DT)
library(data.table)
library(quantmod)
library(ggplot2)
library(plotly)

# Define server logic required to do the dang thang
shinyServer(function(input, output, session) {
  
  list.of.dfs <<- list() 
  react.val <- reactiveValues()
  
  observeEvent(input$go,
               stocklist <<- c(
                 if(input$stock1 != "") {as.character(input$stock1)} else {NULL},
                 if(input$stock2 != "") {as.character(input$stock2)} else {NULL},
                 if(input$stock3 != "") {as.character(input$stock3)} else {NULL},
                 if(input$stock4 != "") {as.character(input$stock4)} else {NULL},
                 if(input$stock5 != "") {as.character(input$stock5)} else {NULL}))
  
  observeEvent(
    input$plotit,
    output$dankplot <- renderPlotly({  
      for(i in 1:length(stocklist)) {
        temp.df <- as.data.frame(loadSymbols(stocklist[i], 
                                             auto.assign = FALSE))
        temp.df$date <- row.names(temp.df)
        temp.df <- temp.df[ , c(7, 1:6)]
        temp.df$Stock <- rep(stocklist[i], length(temp.df[ , 1]))
        colnames(temp.df) <- c("Date", "Open", "High", "Low", "Close",
                               "Volume", "Adjusted", "Stock")
        list.of.dfs[[i]] <- temp.df
        rm(temp.df)
      }
      
      # Aggregate stocklist data
      df <- data.frame(rbindlist(list.of.dfs))
      df$Date <- as.Date(df$Date)
      df$Stock <- as.factor(df$Stock)
      rownames(df) <- seq(1:length(df[ , 1]))
      df.sub <- as.data.frame(df[df$Date >= input$thedates[1] & 
                                   df$Date <= input$thedates[2], ])
      row.names(df.sub) <- NULL
      react.val$df <- df.sub
      # Start graphing
      the.plot <- ggplot(
        aes(x = Date, y = Close, color = Stock), data = df.sub) +
        geom_line(aes(group = Stock), data = df.sub) +
        labs() +
        ggtitle("Stock Performance History. Interactive plot (Plotly.com).") +
        scale_x_date(limits <- input$thedates) +
        xlab("Date") +
        ylab("Closing Price [$]") +
        theme_bw()
      ggplotly(the.plot, tooltip = c("y", "x", "group"))
    })
  )
  output$danktable <- DT::renderDataTable(react.val$df)
})