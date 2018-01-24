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
#library(rodin)

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
  
  # Preview the Query File #
  output$num_query <- renderText({
    c('Number of lipids in Query file: ', nrow(queryData()))
  })
  output$head_query <- renderDataTable({
    head(queryData())
    }, 
    options = list(dom = 't', searching = FALSE)
  )
  
  # Preview the Universe File #
  output$num_universe <- renderText({
    c('Number of lipids in Universe file: ', nrow(universeData()))
  })
  output$head_universe <- renderDataTable({
    head(universeData())
  }, 
  options = list(dom = 't', searching = FALSE)
  )
  
  
  
  
  
  #### Action Button Reactions ####
  # Clean the 2 datasets #
  clean <- eventReactive(input$process_click, {
    validate(
      #need(input$edata_id_col != 'Select one', 
       #    'Please select a unique identifier column')
      need(nrow(universeData()) > 0 & nrow(queryData()) > 0, 
            'Please upload files with > 1 lipid')
    )
    
    universeDataClean <- clean.lipid.list(universeData)
    queryDataClean <- clean.lipid.list(queryData)
    
    # Display success message if everything is loaded correctly
    output$process_success <- renderUI({
      req(universeDataClean() & queryDataClean())
      test1 <- head(universeDataClean())
      test2 <- head(queryDataCl)
      HTML('<h4 style= "color:#1A5276">Your data has been successfully uploaded. 
           You may proceed to the subsequent tabs for analysis.</h4>')
    })
  }
  )
  
  
  
  
  
  
  ####### Preprocess Tab #######
  output$tempplaceholder = renderText({"Summary of tests can go here"})
  
  
  ####### Visualize Tab #######
  #### Sidebar Panel ####
  
  # Which groups should be displayed (if multiple)? #
  output$whichGroups1 <- renderUI({
    selectInput('whichGroups1', 'Group 1', 
                choices = sample_names(), 
                multiple = TRUE)
  })
  output$whichGroups2 <- renderUI({
    selectInput('whichGroups2', 'Group 2', 
                choices = sample_names(), 
                multiple = TRUE)
  })
  
  #### Main Panel ####
  # Display Kendrick or Van Krevelen plots
  output$kendrick <- renderPlot({
    
  })
  output$vankrev <- renderPlot({
    
  })
  
})