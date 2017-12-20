#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Observe click, update Visualize
  rawData <- eventReactive(input$click, {
    filename <- input$file1$datapath
    read.csv(filename)
  })
  rawEmeta <- eventReactive(input$click, {
    filename <- input$file_emeta$datapath
    read.csv(filename)
  })
  
  # Display head of file
  output$head_file1 <- renderDataTable({
    head(rawData())
  })
  

  # Display head of e_meta file
  output$head_emeta <- renderDataTable({
    head(rawEmeta())
  })
  
})
