#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


#options(shiny.maxRequestSize=30*1024^2, ch.dir = TRUE) 
library(shiny)
library(rodin)
library(DT)
library(ggplot2)
library(gridExtra)

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
  universeData <- reactive({
    req(input$universe$datapath)
    filename <- input$universe$datapath
    read.csv(filename, stringsAsFactors = FALSE)
  })
  
  output$CleaningDescription = renderText({"Description of the data cleaning that happens when the user clicks the 'Check Data' button"})
  
  #### Main Panel ####
  
  options(DT.options = list(pageLength = 5))
  
  
  ## Display Table and populate with number of lipids in Original (un-cleaned) datasets ##
  # Depends on: file upload (to get the uploadResults numbers) and button click (to get the cleanResults numbers)
  
  # # Set default results: NA before data is uploaded
  # uploadResultsQuery <- NA
  # uploadResultsUniverse <- NA
  # 
  # # Set default results: NA before cleaning occurs
  # cleanResultsQuery <- NA
  # cleanResultsUniverse <- NA
  # 
  #   # query upload length
  #   uploadResultsquery <- reactive({
  #     # Make sure requirements are met
  #     req(queryData())
  # 
  #     nrow(queryData())
  #   })
  # 
  #   # Universe upload length
  #   uploadResultsUniverse <- reactive({
  #     # Make sure requirements are met
  #     req(universeData())
  # 
  #     nrow(universeData())
  #   })
  # 
  #   # query cleaned length
  #   cleanResultsQuery <- reactive({
  #     # Make sure requirements are met
  #     req(queryDataClean())
  # 
  #     length(queryDataClean())
  #   })
  # 
  #   # Universe cleaned length
  #   cleanResultsUniverse <- reactive({
  #     # Make sure requirements are met
  #     req(universeDataClean())
  # 
  #     length(universeDataClean())
  #   })
  
  ## Display table with number lipids in cleaned datasets ## 1. HAVING TROUBLE WITH THE REACTIVITY HERE - I WANT TO POPULATE THE TABLE WITH THE NUMBERS FROM THE FILE UPLOADS AS SOON AS THE FILE IS UPLOADED, AND THEN POPULATE THE REST OF THE TABLE WITH THE NUMBERS FROM THE CLEANED DATA AFTER THE BUTTON HAS BEEN CLICKED
  output$summary_data <- renderTable({
    input$query
    input$universe
    # req(universeDataClean())
    # req(queryDataClean())
    # req(cleanResultsUniverse())
    # req(cleanResultsQuery())
    
    # If query data uploaded
    if(!is.null(queryData())){
      uploadResultsQ <- nrow(queryData())
    } else {
      uploadResultsQ <- NA
    }
    
    # If universe data uploaded
    if(!is.null(universeData())){
      uploadResultsU <- nrow(universeData())
    } else {
      uploadResultsU <- NA
    }
    
    # If query data cleaned #
    if(!is.null(queryDataClean())){
      cleanResultsQ <- length(queryDataClean())
    } else {
      cleanResultsQ <- NA
    }
    
    # If universe data cleaned #
    if(!is.null(universeDataClean())){
      cleanResultsU <- length(universeDataClean())
    } else {
      cleanResultsU <- NA
    }
    
    #uploadResults <- unlist(uploadResults)
    #cleanResults <- unlist(cleanResultsQuery)
    
    # Create a dataframe out of Before and After results from summaryFilterDataFrame
    data.frame('Uploaded' = c(uploadResultsQ, uploadResultsU),
               'Cleaned' = c(cleanResultsQ, cleanResultsU),
               row.names = c('Query',
                             'Universe'))
    
  }, rownames = TRUE)
  
  
  #### Action Button Reactions ####
  
  # Clean the 2 datasets #
  # queryDataClean <- eventReactive(input$check_click, {
  #   validate(
  #     need(nrow(queryData()) > 0, 
  #          'Please upload Query file with > 1 lipid')
  #   )
  #   
  #   clean.lipid.list(X=queryData())
  # })
  queryDataClean <- reactive({
    validate(
      need(nrow(queryData()) > 0, 
           'Please upload Query file with > 1 lipid')
    )
    if (input$check_click > 0){
      return(clean.lipid.list(X = queryData()))
    } else {
      return(NULL)
    }
  })
  
  universeDataClean <- reactive({
    validate(
      need(nrow(universeData()) > 0, 
           'Please upload Universe file with > 1 lipid')
    )
    if (input$check_click > 0){
      return(clean.lipid.list(X = universeData()))
    } else {
      return(NULL)
    }
  })
  
  
  # Run lipid.miner on the 2 datasets #
  queryMined <- eventReactive(input$check_click, {
    validate(
      need(length(queryDataClean()) > 0, 
           'There are zero lipids in the cleaned query data')
    )
    
    lipid.miner(queryDataClean(), Name="Query", TGcollapse.rm = TRUE, output.list = TRUE)
  })
  
  universeMined <- eventReactive(input$check_click, {
    validate(
      need(length(universeDataClean()) > 0, 
           'There are zero lipids in the cleaned universe data')
    )
    
    lipid.miner(universeDataClean(), Name="Query", TGcollapse.rm = TRUE, output.list = TRUE)
  })
  
  
  ## Display success message if everything is loaded correctly ##
  output$process_success <- renderUI({
    req(universeDataClean()) 
    req(queryDataClean())
    req(universeMined())
    req(queryMined())
    
    test1 <- universeDataClean()
    test2 <- queryDataClean()
    test3 <- universeMined()
    test4 <- queryMined()
    
    if(all(test2 %in% test1)){
      HTML('<h4 style= "color:#1A5276">Your data has been successfully uploaded and cleaned.
         You may proceed to the subsequent tabs for analysis.</h4>')
    }else{
      #HTML('<h4 style= "color:#1A5276"></h4>')
      HTML(paste('<h4 style= "color:#cc3d16">', c('The following lipids are in the Query file but not in the Universe file: ', setdiff(test2, test1)),'</h4>', sep="", collapse=""))
    }
    
    
    
    
    # ## Download option for cleaned data ## 2. THESE BUTTONS SHOULD ONLY BE AVAILABLE ONCE THE DATA HAS BEEN SUCCESSFULLY CLEANED
    
  })
  
  output$downloadQueryClean <- downloadHandler(
    filename = function() {
      paste("Query_Data_Cleaned", ".txt", sep = "")
    },
    content = function(file) {
      query = queryDataClean()
      write.table(query, file, row.names = FALSE)
    }
  )
  
  output$downloadQueryCleanUI <- renderUI({
    if (is.null(queryDataClean())) {
      return(NULL)
    } else {
      downloadButton("downloadQueryClean", "Download Cleaned Query Data")
    }
  })
  

  
  output$downloadUniverseClean <- downloadHandler(
    filename = function() {
      paste("Universe_Data_Cleaned", ".txt", sep = "")
    },
    content = function(file) {
      universe = universeDataClean()
      write.table(universe, file, row.names = FALSE)
    }
  )
  
  output$downloadUniverseCleanUI <- renderUI({
    if (is.null(universeDataClean())) {
      return(NULL)
    } else {
      downloadButton("downloadUniverseClean", "Download Cleaned Universe Data")
    }
  })
  
  ####### Enrichment Analysis Tab #######
  output$tempplaceholder = renderText({"Summary of tests can go here"})
  output$pvalue_text = renderText({"P-value filter"})
  
  # initialize the user input values? 
  
  # Get user inputs #
  test_type <- reactive({
    req(input$dd_enrich_test)
    input$dd_enrich_test
  })
  
  general_select <- reactive({
    req(input$cb_test_params)
    input$cb_test_params
  })
  
  subset_by <- reactive({
    req(input$dd_subset_id)
    input$dd_subset_id
  })
  
  subset_select <- reactive({
    req(input$cb_params_subclass)
    cb_params_subclass
  })
  
  enrich <- reactive({
    req(input$cb_pval_filter)
    cb_pval_filter
  })
  
  p_type <- reactive({
    req(input$dd_pval_type)
    dd_pval_type
  })
  
  
  p_value <- reactive({
    req(input$ue_pval_thresh)
    ue_pval_thresh
  })
  
  # End of get user inputs #
  
  
  #### Action Button Reactions ####
  
  # Run the specified test(s) when Process Data button is clicked #
  global_results <- eventReactive(input$process_click, {
    validate(
      # need cleaned query data #
      need(length(querMined()) > 0, 
           'Please upload and clean Query data.'),
      # need cleaned universe data #
      need(length(universeMined()) > 0, 
           'Please upload and clean Universe data.'),
      # need (at the min) test type #
      need(!is.null(test_type),
           'Please select an enrichment test to use.')
    )
    
    run_the_test(Query.miner = queryMined(), Universe.miner = universeMined, test.type = test_type, general.select = general_select, subset.by = subset_by, subset.select = subset_select, enrich = enrich, pval = p_value, adjpval = p_type)
  })
  
  output$global_results_table <- renderTable({
    req(global_results())
    global_results()
  })
  
  # Check that the parameters have the values chosen by the user -- this will be removed once I know things are working properly -- NOTHING IS BEING DISPLAYED AFTER I CLICK THE BUTTON...NOT SURE WHY
  output$param_check <- renderUI({
    req(test_type()) 
    req(general_select())
    req(subset_by())
    req(subset_select())
    req(enrich())
    req(p_value())
    req(p_type())
    
    HTML(paste('<h4 style= "color:#cc3d16">', c('test_type: ', test_type(),'</h4>', sep="", collapse="")))
    HTML(paste('<h4 style= "color:#cc3d16">', c('general_select: ', general_select(),'</h4>', sep="", collapse="")))
    HTML(paste('<h4 style= "color:#cc3d16">', c('subset_by: ', subset_by(),'</h4>', sep="", collapse="")))
    HTML(paste('<h4 style= "color:#cc3d16">', c('subset_select: ', subset_select(),'</h4>', sep="", collapse="")))
    HTML(paste('<h4 style= "color:#cc3d16">', c('enrich: ', enrich(),'</h4>', sep="", collapse="")))
    HTML(paste('<h4 style= "color:#cc3d16">', c('p_value: ', p_value(),'</h4>', sep="", collapse="")))
    HTML(paste('<h4 style= "color:#cc3d16">', c('p_type: ', p_type(),'</h4>', sep="", collapse="")))
    
  })
  
  
  
  
  
  ####### Visualize Tab #######
  
  output$pie <- renderPlot({
    req(queryMined())
    chain.pieCat(Query.miner$chain)
  })
  
  
})