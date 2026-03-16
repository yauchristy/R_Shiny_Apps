# ===============================================
# Fill in the following fields
# ===============================================
# Title: Harry Potter Text Analysis
# Description: App that performs
# 1) word trend analysis for each chapter
# 2) sentiment score analysis for each chapter
# Details: 
# 1) Four widgets in a column at top of app for customizing text analysis
# 2) Two tabs, each containing bar plots of a text analysis with a corresponding data table
# Author: Christy Yau
# Date: 11/15/24


# ===============================================
# Required packages
# (you can use other packages if you want to)
# ===============================================
library(shiny)
library(tidyverse)  # for data manipulation and graphics
library(tidytext)   # for text mining
library(DT)         # to render Data Tables nicely
library(plotly)     # if you are interested in using ggplotly()
library(stringr)

# ===============================================
# Import data
# ===============================================

#harry potter data table  
hp = read_csv(file = "harry_potter_books.csv", col_types = "ccc") 

# vector with names of books
book_names = unique(hp$book)

# tokenization of hp 
hp_tokens = hp |> 
  unnest_tokens(word, text)

#sentiment lexicons
bing = read.csv("bing.csv")
afinn = read.csv("afinn.csv")
nrc = read.csv("nrc.csv")
loughran = read.csv("loughran.csv")
# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  # Application title
  titlePanel("Harry Potter Text Analysis"),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  fluidRow(
    # widget 1 column
    column(3,
           p(em("Analysis 1 & 2")),
           selectInput(inputId = "widget1", 
                       label = "Select Book", 
                       choices = c(book_names, "All books"),
                       selected = book_names[1])
    ), # closes column 1
    
    #  widget 2 column
    column(3,
           p(em("Analysis 1 & 2")),
           sliderInput(inputId = "widget2", 
                       label = "Choose chapter range", 
                       min = 1,
                       max = 38,
                       value = c(0,38))
    ), # closes column 2
    
    # widget 3 column
    column(3,
           p(em("Analysis 1")),
           textInput(inputId = "widget3", 
                        label = "Choose word (in lowercase)", 
                        value = "harry")
    ), # closes column 3
    
    # widget 4 column
    column(3,
           p(em("Analysis 2")),
           radioButtons(inputId = "widget4", 
                        label = "Choose sentiment lexicon", 
                        choices = c("bing", "afinn", "nrc","loughran"),
                        selected = "bing")
    ) # closes column 4
    
  ), # closes fluidRow
  
  hr(), # horizontal rule
  
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # Customize the following output elements with your own outputs
  # -------------------------------------------------------
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("Word Trend Analysis"),
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("Sentiment Score Analysis"),
                       plotOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  ) # closes tabsetPanel
  
) # closes ui



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # reactive conductor
  # tokens in all books or single book
  book_tokens = reactive({
    if (input$widget1 != "All books") {
      book_tokens = hp_tokens |>
        filter(book == input$widget1) 
    }
    else {
      book_tokens = hp_tokens
    }
    book_tokens |>
      mutate(chapter_num = as.integer(str_extract(book_tokens$chapter, pattern = "\\d+"))) |>
      filter(chapter_num >= input$widget2[1] & chapter_num <= input$widget2[2])
  })
  
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # reactive conductor
  #table of frequency of chosen word in each chapter
  chapter_tokens = reactive({
    book_tokens() |>
      group_by(chapter_num, chapter) |>
      summarise(count = sum(word == input$widget3))  
  })
  
  # plot1: bar plot of frequency that chosen word appears in each chapter
  output$plot1 = renderPlot({
    plot1 = chapter_tokens() |>
      ggplot(aes(x=reorder(chapter, chapter_num), y=count)) + 
      geom_col(color = "black", fill = "#EAE64E") +
      theme_minimal() +
      labs(title = paste("Appearance of", str_to_upper(input$widget3),"in",input$widget1),
           x = "", 
           y = "Count") + 
      theme(axis.text.x = element_text(angle = 90), 
            plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15)) 
   
     #subtitle describing chapter range
    if (input$widget2[2] > max(chapter_tokens()$chapter_num)) {
      plot1 = plot1 + labs(subtitle = paste("Ch",input$widget2[1],"- Ch", max(chapter_tokens()$chapter_num)))
    } else {
      plot1 = plot1 + labs(subtitle = paste("Ch",input$widget2[1],"- Ch", input$widget2[2]))
    }
    plot1
  })
  
  # table of frequency of word in each chapter, based on plot1
  output$table1 <- renderDataTable({
    chapter_tokens() |>
      arrange(chapter_num) |>
      ungroup()|>
      select(chapter, count)
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  #reactive conductor
  #table sentiment score per chapter using different sentiment lexicons
  sentiment_score = reactive({
    if(input$widget4 != "afinn"){
    #sentiment score from bing, nrc, and loughran lexicon
      if(input$widget4 == "bing") {
        sentiment = book_tokens()|>
          inner_join(bing, by = "word", relationship = "many-to-many")
      } else if (input$widget4 == "nrc"){
        sentiment = book_tokens()|>
          inner_join(nrc, by = "word", relationship = "many-to-many")
      } else if (input$widget4 == "loughran"){
        sentiment = book_tokens()|>
          inner_join(loughran, by = "word", relationship = "many-to-many")
      } 
    sentiment = sentiment |>
        count(chapter_num, chapter, word, sentiment, name = "count") |>
        pivot_wider(names_from = sentiment, #names of new columns: neg & pos
                    values_from = count, #values to fill new columns
                    values_fill = 0) |>
        mutate(score = positive - negative) |> 
        group_by(chapter_num, chapter)|>
        summarize(score=sum(score)) 
    } else {
    #sentiment score from afinn lexicon
      sentiment = book_tokens()|>
        inner_join(afinn, by = "word", relationship = "many-to-many")|>
        group_by(chapter_num, chapter)|>
        summarize(score=sum(value))  
      }
  })
  
  # plot2: bar plot of sentiment score per chapter 
  output$plot2 = renderPlot({
    plot2 = sentiment_score() |>
      ggplot(aes(x=reorder(chapter,chapter_num), y=score, fill = score>0)) +
      geom_col(color="black") + theme_minimal() +
      labs(title = paste("Sentiment Score in",input$widget1),
                                                      x = "", 
                                                      y = "Score") + 
      theme(axis.text.x = element_text(angle = 90), 
            plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            legend.text = element_text(size = 12), 
            legend.title = element_text(size = 15)) +
      scale_fill_discrete(name = "Score", labels = c("Negative", "Positive"))
    
    #subtitle describing chapter range
    if (input$widget2[2] > max(chapter_tokens()$chapter_num)) {
      plot2 = plot2 + labs(subtitle = paste("Ch",input$widget2[1],"- Ch", max(chapter_tokens()$chapter_num)))
    } else {
      plot2 = plot2 + labs(subtitle = paste("Ch",input$widget2[1],"- Ch", input$widget2[2]))
    }
    plot2
  })
  
  # table displaying sentiment score per chapter, based on plot2
  output$table2 <- renderDataTable({
    sentiment_score()|>
      arrange(chapter_num) |>
      ungroup()|>
      select(chapter, score)
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

