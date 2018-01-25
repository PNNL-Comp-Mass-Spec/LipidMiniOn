#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# library(shiny)
# 
# # Define server logic required to draw a histogram
# shinyServer(function(input, output) {
#   
#   # Observe click, update Visualize
#   universeData <- eventReactive(input$click, {
#     filename <- input$universe$datapath
#     read.csv(filename)
#   })
#   
#   queryData <- eventReactive(input$click, {
#     filename <- input$query$datapath
#     read.csv(filename)
#   })
#   
#   
#   # Display head of file
#   output$head_universe <- renderDataTable({
#     head(universeData())
#   })
#   
# 
#   # Display head of e_meta file
#   output$head_query <- renderDataTable({
#     head(queryData())
#   })
#   
# })


#options(shiny.maxRequestSize=30*1024^2, ch.dir = TRUE) 
library(shiny)
library(rodin)
library(DT)

shinyServer(function(session, input, output){
  
  ######## Upload Tab ##############
  
  #### Sidebar Panel ####
  
  # Get data from Query File #
  queryData <- reactive({
    req(input$query$datapath)
    filename <- input$query$datapath
    read.csv(filename, stringsAsFactors = FALSE)
  })
  
  # Get data from Universe File #
  universeData <-reactive({
    req(input$universe$datapath)
    filename <- input$universe$datapath
    read.csv(filename, stringsAsFactors = FALSE)
  })
  
  
  #### Main Panel ####

  options(DT.options = list(pageLength = 5))
  
  # Preview the Query File #
  output$num_query <- renderUI({
    req(queryData())
    HTML(paste('<h4>Number of lipids in Query file: ', nrow(queryData()), '</h4>', sep=""))
  })

  output$head_query <- DT::renderDataTable({
    queryData()
    }
    #options = list(dom = 't', searching = FALSE)
  )
  
  # Preview the Universe File #
  output$num_universe <- renderUI({
    req(universeData())
    HTML(paste('<h4>Number of lipids in Universe file: ', nrow(universeData()), '</h4>', sep=""))
  })
  
  output$head_universe <- DT::renderDataTable({
    universeData()
  }
  #options = list(dom = 't', searching = FALSE)
  )
  
  
  
  
  
  #### Action Button Reactions ####
  # Clean the 2 datasets #
  queryDataClean <- eventReactive(input$process_click, {
    validate(
      need(nrow(queryData()) > 0, 
           'Please upload Query file with > 1 lipid')
    )
    
    clean.lipid.list(X=queryData())
  })
  
  universeDataClean <- eventReactive(input$process_click, {
    validate(
      need(nrow(universeData()) > 0, 
            'Please upload Universe file with > 1 lipid')
    )
    
    clean.lipid.list(X=universeData())
  })
  
  # Display success message if everything is loaded correctly
  output$process_success <- renderUI({
    req(universeDataClean()) 
    req(queryDataClean())
    test1 <- universeDataClean()
    test2 <- queryDataClean()
    
    if(all(test2 %in% test1)){
      HTML('<h4 style= "color:#1A5276">Your data has been successfully uploaded and cleaned. [Placeholder for disclaimer on cleaning function; need text from Geremy] 
         You may proceed to the subsequent tabs for analysis.</h4>')
    }else{
      #HTML('<h4 style= "color:#1A5276"></h4>')
      HTML(paste('<h4 style= "color:#cc3d16">', c('The following lipids are in the Query file but not in the Universe file: ', setdiff(test2, test1)),'</h4>', sep="", collapse=""))
    }
    
  })
  
  

  
  ####### Enrichment Analysis Tab #######
  output$tempplaceholder = renderText({"Summary of tests can go here"})
  
  
  ####### Visualize Tab #######
  #### Sidebar Panel ####
  
  # # Which groups should be displayed (if multiple)? #
  # output$whichGroups1 <- renderUI({
  #   selectInput('whichGroups1', 'Group 1', 
  #               choices = sample_names(), 
  #               multiple = TRUE)
  # })
  # output$whichGroups2 <- renderUI({
  #   selectInput('whichGroups2', 'Group 2', 
  #               choices = sample_names(), 
  #               multiple = TRUE)
  # })
  # 
  # #### Main Panel ####
  # # Display Kendrick or Van Krevelen plots
  # output$kendrick <- renderPlot({
  #   
  # })
  # output$vankrev <- renderPlot({
  #   
  # })
  
})