library(shiny)
library(Quandl)
library(plotly)
#source("Utility.R")

shinyServer(function(input, output){

  ###Render welcome page
  output$welcomeUI <- renderUI(welcomePageUI())

  ###Render plot using input from Calendar
  # output$plot <- renderPlot({
  #   data <- getTicker()
  #   data <- data[data$Date<=input$date1,]
  #   plot(data$High)
  # })
  
  ### Use plotly
  output$plot <- renderPlotly({
    data <- getTicker()
    data <- data[data$Date<=input$date1,]
    f <- list(
      family = "Arial",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Date",
      titlefont = f
    )
    y <- list(
      title = "Price",
      titlefont = f
    )
    plot_ly(data, x = ~Date, y = ~High, mode='lines') %>%
      layout(xaxis=x, yaxis=y)
  })
  
  getTicker <- eventReactive(input$submit, {
    ticker <- paste0("GOOG", "/NASDAQ_", input$ticker)
    data <- Quandl(ticker)
  })

  ###Render Graph page
  uiList <- eventReactive(input$submit, {
    graphPageUI(input, output)
  })
  
  output$graphUI <- renderUI(uiList())
  }
)

welcomePageUI <- function(){
  return(list(
    h3("Welcome"),
    br(),
    textInput("username", label="Name:"),
    textInput("ticker", label="Enter Stock Ticker:"),
    actionButton("submit", "Submit"),
    textOutput("loginError")
  ))
}


graphPageUI <- function(input, output){
  output$welcomeUI <- renderUI(list())
  list(
    sidebarLayout(
      sidebarPanel(
        dateInput('date1', label="Select a date", value = '2014-01-20', min = '2005-01-20', max = '2017-01-25',
                  format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                  language = "en", width = NULL)    ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotlyOutput("plot"))
        )
      )
    )
  )
}