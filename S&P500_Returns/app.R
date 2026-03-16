# Title: S&P 500 Returns
# Description: Data of S&P 500 Closing Values and multi-year returns
# Author: Christy Yau
# Date: 1/1/2024


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)  # for data manipulation and graphics
library(lubridate)  # for working with dates
library(tidyquant)  # financial & quant functions
library(plotly)     # for web interactive graphics


# ======================================================
# Auxiliary objects/functions 
# (that don't depend on input widgets)
# ======================================================
# You may want to add one or more auxiliary objects for your analysis
# (by "auxiliary" we mean anything that doesn't depend on input widgets)

sp500 = tq_get("^GSPC", from = "1928-01-01", to = "2023-12-31")
sp500 = mutate(sp500, year = year(sp500$date))

# =======================================================
# Define UI for application
# =======================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("S&P 500 Historical Data"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets
  # (adapt code with widgets of your choice!!!)
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # inputs
      sliderInput(inputId = "widget1",
                  label = "Time period:",
                  min = 1928,
                  max = 2023,
                  value = c(1990,2020),
                  sep = ""),
      radioButtons(inputId = "widget2",
                   label = "Scale of Timeline:",
                   choices = c("Linear", "Log (log-10)")),
      numericInput(inputId = "widget3",
                   label = "Number-of-Year Returns:",
                   value = 5),
      checkboxGroupInput(inputId = "widget4",
                   label = "Summary Statistics:",
                   choices = list("Mean", "Median", "Std Dev")),
    ),  # closes sidebarPanel of inputs
    
    # -------------------------------------------------------
    # Main Panel with outputs: plots and table
    # -------------------------------------------------------
    mainPanel(
      h3("S&P 500 Daily Closing Values"),
      plotlyOutput(outputId = "plot1"),
      hr(),
      h3("S&P 500 Returns"),
      plotlyOutput(outputId = "plot2"),
      hr(),
      h3("S&P 500 Summary Statistics"),
      tableOutput(outputId = "table"),
    ) # closes mainPanel of outputs
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)


# ======================================================
# Define server logic
# ======================================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Auxiliary table (note that this is a reactive conductor)
  # (adapt code to your own analysis)
  # ------------------------------------------------------------
  #filtered sp500 table with selected years
  sp500_filtered = reactive({
    sp500|>
      filter(year >= input$widget1[1] & year <= input$widget1[2])
  })
  
  #Table of multi-year returns
  multiyear_returns = reactive({
  #first and last closing data per year
  annual_close = sp500|>
    filter(year >= input$widget1[1] & year <= input$widget1[2])|>
    group_by(year)|>
    summarise(first = first(close), last = last(close))
  
  first = annual_close$first
  last = annual_close$last
  
  #calculate multi-year returns
  years = nrow(annual_close)-(input$widget3-1)
  returns = rep(0, years) 
  for (i in 1:years) {
    returns[i] = ((last[i+(input$widget3-1)]-first[i])/first[i]) * 100 
  }
  
  #table 
  start = seq(input$widget1[1],input$widget1[2]-(input$widget3-1))
  end = seq(input$widget1[1]+(input$widget3-1),input$widget1[2])
  data.frame(year = paste(start,"-", end), return = returns)
  })
  
  # ------------------------------------------------------------
  # Plot (timeline of daily closing values)
  # (adapt code to make a timeline according to your analysis!!!)
  # ------------------------------------------------------------
  
  output$plot1 <- renderPlotly({
   
  if (input$widget2 == "Linear") {
    ggplot(data = sp500_filtered(), aes(x = date, y = close)) +
      geom_line(color = "#2F8BF5") +
      labs(title = paste(input$widget1[1],"-",input$widget1[2]),
           x = "Date",
           y = "Closing value") +
      theme_minimal()
  } else {
    ggplot(data = sp500_filtered(), aes(x = date, y = close)) +
      geom_line(color = "#2F8BF5") +
      scale_y_continuous(trans = "log10") +
      labs(title = paste(input$widget1[1],"-",input$widget1[2]),
           x = "Date",
           y = "Closing value (log-scale)")+
      theme_minimal()
    }
  })

  # ------------------------------------------------------------
  # Plot (bar-chart of multi-year returns)
  # (adapt code to make a timeline according to your analysis!!!)
  # ------------------------------------------------------------
  output$plot2 <- renderPlotly({
  #bar chart of returns 
  bar = multiyear_returns() |>
    ggplot(aes(x = year, y = return, fill = return>=0)) + 
    geom_col(color = "black") +
    theme_minimal() +
    labs(title = paste(input$widget3,"- Year Returns"), 
         x = "Year", 
         y = "Annual Percent Return") +
    theme(axis.text.x = element_text(angle = 90, size = 5), legend.position = "none") 
  
  #Statistics
  r = multiyear_returns()$return
  if ("Mean" %in% input$widget4) {
    bar = bar + geom_hline(yintercept =  mean(r), col = "red") 
  }
  
  if ("Median" %in% input$widget4) {
    bar = bar + geom_hline(yintercept =  median(r), col = "green") 
  } 
  
  if ("Std Dev" %in% input$widget4) {
    bar = bar + geom_hline(yintercept = mean(r)+sd(r), col = "blue") +
      geom_hline(yintercept = mean(r)-sd(r), col = "blue")
  } 
  bar
  })
    
  # ------------------------------------------------------------
  # Table
  # (adapt code to display appropriate table!!!)
  # ------------------------------------------------------------
  output$table <- renderTable({
    # values
    p10 = quantile(multiyear_returns()$return, .1)
    p25 = quantile(multiyear_returns()$return, .25)
    med = median(multiyear_returns()$return)
    mean = mean(multiyear_returns()$return)
    p75 = quantile(multiyear_returns()$return, .75)
    p90 = quantile(multiyear_returns()$return, .9)
    std = sd(multiyear_returns()$return)
    IQR = IQR(multiyear_returns()$return) 
    # stats table
    sp500_stat = data.frame(
      statistic = c("p10", "p25", "median", "mean","p75","p90","std dev", "IQR"),
      value = c(p10, p25, med, mean, p75, p90, std, IQR)
    )
  })
 
} # closes server


# Run the application 
shinyApp(ui = ui, server = server)
