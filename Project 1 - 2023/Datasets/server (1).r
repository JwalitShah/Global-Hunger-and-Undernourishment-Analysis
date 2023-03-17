### Install the shiny package if not already installed
### install.packages("shiny")
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)


## 1: Define Server for application
shinyServer(
  
  ## 2: Create R code that builds the object in the server function. 
  ## The server function builds a list-like object named "output" that contains all of the code 
  ## Each R object needs to have its own entry in the list.
  
  function(input, output) {
    
    ## Use Reactive() to evaluate expressions eg. Reading/Processing/Wrangling Data
    ipfile <- reactive({
      read.csv('airlines_delay.csv')
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
    
    gear <- reactive({
      as.numeric(input$gear)
      })
    
    databar <- reactive({
      data <- mtcars %>% filter(rownames(mtcars) %in% input$checkbox)})
    
    ## Use rendertext() to show text 
    output$text1 <- renderText({ 
      paste("Selected columns for analysis are -", names(mtcars[colm1()]), 'and ', names(mtcars[colm2()]))
      })
    
    output$text2 <- renderText({ 
      paste("Color legend is ", names(mtcars[colr()]))
    })
    
    output$text3 <- renderText({ 
      paste("Selected gear", gear() )
    })
    
    
    ## Use renderTable() to show Dataframe
    output$my_table <- renderTable({
        databar() })
    
    output$mytable <- renderTable({
      head(ipfile()) })
    
    
    ## Use renderPlot() to plot the chart 
    output$scatter <- renderPlot({
      mtcars %>% 
        filter(mtcars[,10] == gear()  ) %>% 
        ggplot(aes(x=.[[colm1()]], y=.[[colm2()]], color=.[[colr()]])) + 
        geom_point(size=6) + 
        labs(title = 'Bivariate Analysis', x=colnames(mtcars)[colm1()], y=colnames(mtcars)[colm2()], color =  colnames(mtcars)[colr()])+
        theme(title = element_text(size = 16, face = 'bold', colour = 'red'))
    })
    
    output$bar <- renderPlot({
        #using ggplot
      ggplot(databar(), aes(x=input$checkbox, y=mpg)) + 
        geom_bar(stat = "identity") + coord_flip() +  
      labs(title = 'Comparison of mpg by model', y='Mileage per gallon', x='Model')+
      theme(title = element_text(size = 16, face = 'bold', colour = 'blue'))
    })
  
})
    
    
    