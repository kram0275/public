### See https://deanattali.com/blog/building-shiny-apps-tutorial/

# App originally conceptualized in July 2018

load(file = "war_shiny.Rda")
load(file ="yearPlot.Rda")
library(shiny)
library(ggplot2)
library(DT)

app_title <- "Twins WAR by Year (1986 - 2017)"

ui <- fluidPage(
  titlePanel(app_title, windowTitle = app_title),
  
  # Begin sub-header
  "Data from ", 
  tags$a(href = 
           "https://www.fangraphs.com/warleaders.aspx?season=2017&team=8&type=", 
         "Fangraphs. "),
  "WAR calculated using FIP (Fielding Independent Pitching).",
  # End sub-header
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1986, max = 2017, value = 1991,
                  sep = ""),
      radioButtons("fullview", "Select table view", 
                   choices = c("By selected year", "Full")),
      width = 12),
    
    mainPanel(
      plotOutput("coolplot"), 
      DT::dataTableOutput("contents"),
      width = 12)
  )
)


server <- function(input, output) {
  grab_data <- function(df, calYear) {
    simple <- data.frame(Year = df$year, Player = df$Player, Pos = df$Pos, 
                         PA = df$PA, IP = df$IP, Total.WAR = df$Total.WAR, 
                         Pct.WAR = df$pct_war)
    if(input$fullview == "By selected year") {
      simple <- simple[simple$Year == calYear, ]
    }
    if(input$fullview == "Full") {
      simple <- simple
    }
    return(simple)
  }
  
  output$coolplot <- renderPlot({
    yearPlot(full_data, input$yearInput[1])
  })
  
  output$contents <- DT::renderDataTable (
    DT::datatable(grab_data(full_data, input$yearInput), rownames = FALSE,
                  options = list(lengthMenu = c(10, 25, 50), pageLength = 25))
  )
}

shinyApp(ui = ui, server = server)
