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
#   enrichedData <- eventReactive(input$click, {
#     filename <- input$enriched$datapath
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
#   output$head_enriched <- renderDataTable({
#     head(enrichedData())
#   })
#   
# })


#options(shiny.maxRequestSize=30*1024^2, ch.dir = TRUE) 
library(shiny)
#library(rodin)

shinyServer(function(session, input, output){
  
  ######## Upload Tab ##############
  #### Sidebar Panel ####
  # Get data from files
  universeData <-reactive({
    req(input$universe$datapath)
    filename <- input$universe$datapath
    read.csv(filename, stringsAsFactors = FALSE)
  })
  enrichedData <- reactive({
    req(input$enriched$datapath)
    filename <- input$enriched$datapath
    read.csv(filename, stringsAsFactors = FALSE)
  })
  

  
  #### Main Panel ####
  # Display results
  output$head_universe <- renderDataTable({
    head(universeData())
    }, 
    options = list(dom = 't', searching = FALSE)
  )
  output$head_enriched <- renderDataTable({
    head(enrichedData())
    }, 
    options = list(dom = 't', searching = FALSE)
  )
  output$num_universe <- renderText({
    c('Number of lipids in Universe file: ', nrow(universeData()))
  })
  output$num_enriched <- renderText({
    c('Number of lipids in Enriched file: ', nrow(enrichedData()))
  })
  
  
  #### Action Button Reactions ####
  # Clean the 2 datasets #
  clean <- eventReactive(input$process_click, {
    validate(
      #need(input$edata_id_col != 'Select one', 
       #    'Please select a unique identifier column')
      need(nrow(universeData()) > 0 & nrow(enrichedData()) > 0, 
            'Please upload files with > 1 lipid')
    )
    
    universeDataClean <- clean.lipid.list(universeData)
    enrichedDataClean <- clean.lipid.list(enrichedData)
    
    # Display success message if everything is loaded correctly
    output$process_success <- renderUI({
      req(universeDataClean() & enrichedDataClean())
      test1 <- head(universeDataClean())
      test2 <- head(enrichedDataCl)
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