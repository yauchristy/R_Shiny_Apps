# Title: S&P 500 Returns
# Description: Data of S&P 500 Closing Values and multi-year returns
# Author: Christy Yau
# Date: 1/1/2024


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)   
library(tidyquant)
library(dplyr)     
library(plotly)
library(readxl)


# ======================================================
# Auxiliary objects/functions 
# (that don't depend on input widgets)
# ======================================================
# You may want to add one or more auxiliary objects for your analysis
# (by "auxiliary" we mean anything that doesn't depend on input widgets)

income = read_excel("claim_denial_financial_resp.xlsx", sheet = 1)
ethnicity = read_excel("claim_denial_financial_resp.xlsx", sheet = 2)

# =======================================================
# Define UI for application
# =======================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Financial Responsibility for Denied Preventive Claims"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets
  # (adapt code with widgets of your choice!!!)
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # inputs
      selectInput(inputId = "widget1",
                  label = "Socioeconomic Group",
                  choices = c("Income level", "Race & Ethnicity")),
      tagList(replicate(20, br(), simplify = FALSE)),
      radioButtons(inputId = "widget2",
                   label = "Statistic for OOP Expenses",
                   choices = c("Mean", "Median", "Max"))
    ),  # closes sidebarPanel of inputs
    
    # -------------------------------------------------------
    # Main Panel with outputs: plots and table
    # -------------------------------------------------------
    mainPanel(
      h3("% Denied Claims Requiring OOP Expenses"),
      plotlyOutput(outputId = "plot1"),
      hr(),
      h3("OOP Expenses of Denied Claims ($)"),
      plotlyOutput(outputId = "plot2"),
      hr(),
      h3("Summary Statistics"),
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
  
  #income table 
  income_tbl = reactive({data.frame(income) |>
    rename(Income = Household.Income..thousands., Claims = N.Claims) |>
    mutate(Income = factor(Income, levels = c("0-29", "30-49", "50-74", "75-99", ">=100"))) |>
    slice (-6)}) 
  #income$Income = factor(income$Income, levels = c("0-29", "30-49", "50-74", "75-99", ">=100"))
  
  #ethnicity/race table
  ethnicity_tbl = reactive({data.frame(ethnicity) |>
    rename(Race = Ethnic.group, Claims = N.Claims) |>
    slice (-6)})
  
  # ------------------------------------------------------------
  # Plot (timeline of daily closing values)
  # (adapt code to make a timeline according to your analysis!!!)
  # ------------------------------------------------------------
  
  output$plot1 <- renderPlotly({
    if (input$widget1 == "Income level"){
          ggplot(data = income_tbl(), aes(x = Income, y = Percentage)) + 
          geom_col(fill = "#F55246") + 
          theme_classic() + 
          labs(x = "Income level ($1000)", y = "% of Denied Claims") + 
          coord_cartesian(ylim = c(92, 94))
    }
    else if (input$widget1 == "Race & Ethnicity"){
        ggplot(data = ethnicity_tbl(),aes(x = Race, y = Percentage)) + 
        geom_col(fill = "#F55246") + 
        theme_classic() + 
        labs(x = "Race and Ethnicity", y = "% of Denied Claims") + 
        coord_cartesian(ylim = c(92, 94.5))
    }
  })
  
  # ------------------------------------------------------------
  # Plot (bar-chart of multi-year returns)
  # (adapt code to make a timeline according to your analysis!!!)
  # ------------------------------------------------------------
  output$plot2 <- renderPlotly({
    if (input$widget1 == "Income level"){
      if (input$widget2 == "Median"){
        p2 = ggplot(data = income_tbl(), aes(x = Income, y = Med))+
          scale_y_continuous(limits = c(0,500), breaks = seq(0, 500, by = 50))
        }
      else if (input$widget2 == "Mean"){
        p2 = ggplot(data = income_tbl(), aes(x = Income, y = Mean)) +
          scale_y_continuous(limits = c(0,1700), breaks = seq(0, 1800, by = 200))
      }
      else {
        p2 = ggplot(data = income_tbl(), aes(x = Income, y = Max))+
          scale_y_continuous(limits = c(0,250000), breaks = seq(0, 250000, by = 25000))
      }
        p2+
        geom_col(fill = "#4E917A") + 
        theme_classic() + 
        labs(x = "Income level ($1000)", y =paste0(input$widget2," Amount ($)"))
    }
    else {
      if (input$widget2 == "Median"){
        p2 = ggplot(data = ethnicity_tbl(), aes(x = Race, y = Med))+
          scale_y_continuous(limits = c(0,600), breaks = seq(0, 600, by = 50))
      }
      else if (input$widget2 == "Mean"){
        p2 = ggplot(data = ethnicity_tbl(), aes(x = Race, y = Mean))+
          scale_y_continuous(limits = c(0,1700), breaks = seq(0, 1800, by = 200))
      }
      else {
        p2 = ggplot(data = ethnicity_tbl(), aes(x = Race, y = Max))+
          scale_y_continuous(limits = c(0,250000), breaks = seq(0, 250000, by = 25000))
      }
        p2 +
        geom_col(fill = "#4E917A") + 
        theme_classic() + 
        labs(x = "Race and Ethnicity", y = paste0(input$widget2," Amount ($)")) + 
        theme(axis.text.x = element_text(size = 8.5)) 
    }
  })
  
  # ------------------------------------------------------------
  # Table
  # (adapt code to display appropriate table!!!)
  # ------------------------------------------------------------
  output$table <- renderTable({
    if (input$widget1 == "Income level"){
      data.frame(income)|>
        rename(Income = Household.Income..thousands., Claims = N.Claims)
    }
    else {
      data.frame(ethnicity)|>
        rename(Race_Ethnicity= Ethnic.group, Claims = N.Claims)
    }
    })
  
} # closes server


# Run the application 
shinyApp(ui = ui, server = server)
