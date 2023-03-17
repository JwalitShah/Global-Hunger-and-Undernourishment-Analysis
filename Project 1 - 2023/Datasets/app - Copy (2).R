#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### Install the shiny package if not already installed
### install.packages("shiny")

# load the shiny package
library(shiny) 

## 1: Define UI for application
ui <-
  
  ## 2: Creating a page using fluidpage()
  fluidPage(
    
    ## 3: Creating title panel using titlepanel()
    titlePanel( h1('Childhood Malnutrition and its Psychological Measures', align = "center")),
    
    ## 4: Creating Sidebar panel using sidebarPanel()
    sidebarPanel(
      
    
    
    ## 5: Creating Input fields -- TypeofInput(InputID, InputLabel, Choices, DefaultChoice)
    
    selectInput("var1", label = "1. Select the Variables to compare", 
                choices = c("Underweight" = 2, "Wasting" = 3, "Stunting" = 4),
                selected = 2), 
    
    selectInput("var2", label = "1. Select the Variables to compare", 
                choices = c("Underweight" = 2, "Wasting" = 3, "Stunting" = 4),
                selected = 3),
    
    
    checkboxGroupInput("checkbox", 
                       label = "2. Select any of the following", 
                       choices = lit$Country,
                       selected = 'Bangladesh'),
    
    
    
    
  ),
  
  ## 6: Creating Main panel -- TypeofOutput(OutputID)
  ## Output elements will appear by the order they are called in Main panel.
  mainPanel(
    textOutput("text1"), 
    tags$head(tags$style("#text1{color: blue;font-size: 20px;font-style: bold;}")),
    
    textOutput("text2"),
    tags$head(tags$style("#text2{color: blue;font-size: 20px;font-style: bold;}")),
    
    
    plotOutput("scatter"),
    tableOutput("my_table"),
    plotOutput("bar")),
  tableOutput("mytable")
 )

### Install the shiny package if not already installed
### install.packages("shiny")
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)


## 1: Define Server for application
server <-
  
  ## 2: Create R code that builds the object in the server function. 
  ## The server function builds a list-like object named "output" that contains all of the code 
  ## Each R object needs to have its own entry in the list.
  
  function(input, output) {
    
    ## Use Reactive() to evaluate expressions eg. Reading/Processing/Wrangling Data
    ipfile <- reactive({
      read.csv("R Shiny.csv")
    })
    
    colm1 <- reactive({
      as.numeric(input$var1)
    })
    
    colm2 <- reactive({
      as.numeric(input$var2)
    })
    
    colr <- reactive({
      as.numeric(input$colour)
    })
    
    lit <- read.csv("C:\Users\jwali\OneDrive\Desktop\RShinyApp\R Shiny.csv")
    
    
    
    
    databar <- reactive({
      data <- lit %>% filter(lit$Country %in% input$checkbox)})
    
    ## Use rendertext() to show text 
    output$text1 <- renderText({ 
      paste("Selected columns for analysis are -", names(lit[colm1()]), 'and ', names(lit[colm2()]))
    })
    
    output$text2 <- renderText({ 
      paste("Color legend is ", names(lit[colr()]))
    })
    
    
    
    
    ## Use renderTable() to show Dataframe
    output$my_table <- renderTable({
      databar() })
    
    
    
    
    ## Use renderPlot() to plot the chart 
    output$scatter <- renderPlot({
      lit %>% 
        ggplot(aes(x=.[[colm1()]], y=.[[colm2()]])) + 
        geom_point(size=6, aes(colour=Country)) + 
        labs(title = 'Bivariate Analysis', x=colnames(lit)[colm1()], y=colnames(lit)[colm2()], color =  colnames(lit)[colr()])+
        theme(title = element_text(size = 16, face = 'bold', colour = 'red'))
    })
    
    output$bar <- renderPlot({
      #using ggplot
      ggplot(databar(), aes(x=input$checkbox, y=TotalMalnutrition)) + 
        geom_bar(stat = "identity") + coord_flip() +  
        labs(title = 'Comparison of Total Malnutrition by country', y='Total Malnutrition', x='Country')+
        theme(title = element_text(size = 16, face = 'bold', colour = 'blue'))
    })
    
  }


# Create Shiny app ----
shinyApp(ui = ui, server=server)
