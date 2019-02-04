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
library(visNetwork)
library(Rodin)
library(DT)
library(ggplot2)
library(plotly)
library(gridExtra)
library(htmlwidgets)
source("./run_the_tests.R")
source("./pie_chart_functions.R")

shinyServer(function(session, input, output){
  #Sys.setenv(R_ZIPCMD="/usr/bin/zip")
  ######## Upload Tab ##############

  #### Sidebar Panel ####

  # Get data from Query File #
  queryData <- reactive({
    if(!is.null(input$query$datapath)){
      filename <- input$query$datapath
      return(read.csv(filename, stringsAsFactors = FALSE))
    } else if (input$query_text != ""){
      req(input$query_text)
      input$check_click1
      temp <- strsplit(isolate(input$query_text), split = " ")[[1]]
      return(data.frame(ID = temp, row.names = NULL))
    } else if (!is.null(input$rank_table$datapath)){
      filename <- input$rank_table$datapath
      return(read.csv(filename, stringsAsFactors = FALSE))
    }
    else (return(NULL))

    
  })

  # Get data from Universe File #
  universeData <- reactive({
    if(!is.null(input$universe$datapath)){
      req(input$universe$datapath)
      filename <- input$universe$datapath
      return(read.csv(filename, stringsAsFactors = FALSE) )
    } else if (input$universe_text != ""){
      req(input$universe_text)
      input$check_click1
      temp <- strsplit(isolate(input$universe_text), split = " ")[[1]]
      return(data.frame(ID = temp, row.names = NULL))
    }

  })

  #---------- Example Data Download --------#

  output$Query_Human_Lung_Endothelial_Cells.txt <- downloadHandler(
    filename = function() {
      paste("Query_Human_Lung_Endothelial_Cells", ".txt", sep = "")
    },
    content = function(file) {
      query_human_lung <- read.table("20M_END_signif.csv", header = T)
      write.table(query_human_lung, file, row.names = FALSE, sep = "\t")
    }
  )

  output$Universe_Human_Lung_Endothelial_Cells.txt <- downloadHandler(
    filename = function() {
      paste("Universe_Human_Lung_Endothelial_Cells", ".txt", sep = "")
    },
    content = function(file) {
      universe_human_lung <- read.table("LungMap_universe.csv", header = T)
      write.table(universe_human_lung, file, row.names = FALSE, sep = "\t")
    }
  )

  output$Query_Soil_Surface.txt <- downloadHandler(
    filename = function() {
      paste("Query_Soil_Surface", ".txt", sep = "")
    },
    content = function(file) {
      query_soil_surface <- read.table("Soil_surface_signif.csv", header = T)
      write.table(query_soil_surface, file, row.names = FALSE, sep = "\t")
    }
  )

  output$Universe_Soil_Surface.txt <- downloadHandler(
    filename = function() {
      paste("Universe_Soil_Surface", ".txt", sep = "")
    },
    content = function(file) {
      universe_soil_surface <- read.table("Soil_universe.csv", header = T)
      write.table(universe_soil_surface, file, row.names = FALSE, sep = "\t")
    }
  )

  #---------- Depending on the type of data upload (text or csv) display a success message if successful
  output$CleaningDescription = renderText({"Verify annotations match between query and universe by clicking 'Check Data'"})
  output$CleaningDescription1 = renderText({"Verify annotations match between query and universe by clicking 'Check Data'"})
  output$CleaningDescription2 = renderText({"Verify annotations match between query and universe by clicking 'Check Data'"})
  
  #### Main Panel ####

  options(DT.options = list(pageLength = 15))

  ## Display table with number lipids in cleaned datasets ## 1. HAVING TROUBLE WITH THE REACTIVITY HERE - I WANT TO POPULATE THE TABLE WITH THE NUMBERS FROM THE FILE UPLOADS AS SOON AS THE FILE IS UPLOADED, AND THEN POPULATE THE REST OF THE TABLE WITH THE NUMBERS FROM THE CLEANED DATA AFTER THE BUTTON HAS BEEN CLICKED
  output$summary_data <- renderTable({
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

  }, rownames = TRUE, align = 'c')

#----- Clean the uploaded data --------#
  queryDataClean <- reactive({
    if(is.null(input$rank_table$datapath)){
      validate(
        need(nrow(queryData()) > 0,
             'Please upload Query file with > 1 lipid')
      )
    }
    if (input$check_click > 0 | input$check_click1 > 0){
      return(clean.lipid.list(X = queryData()))
    } else if (input$check_click2 > 0) {
      return(clean.rankingTable(queryData()))
    } else{
      return(NULL)
    }
  })

  universeDataClean <- reactive({
    if(is.null(input$rank_table$datapath)){
      validate(
        need(nrow(universeData()) > 0 ,
             'Please upload Universe file with > 1 lipid')
      )
    }

    if (input$check_click > 0 | input$check_click1 > 0){
      return(clean.lipid.list(X = universeData()))
    } else {
      return(NULL)
    }
  })

output$rankplot <- renderPlot({
  plot_ranking(queryDataClean())
})
#----- Run lipid.miner on the 2 datasets ------#
  mine_the_data <- reactiveValues(go = FALSE)
  observeEvent(input$check_click, {
    mine_the_data$go <- TRUE
  }, priority = 10)

  observeEvent(input$check_click1, {
    mine_the_data$go <- TRUE
  }, priority = 10)

  queryMined <- reactive({
    validate(
      need(length(queryDataClean()) > 0,
           'There are zero lipids in the cleaned query data')
    )
    if (mine_the_data$go) {
      return(lipid.miner(queryDataClean(), name="Query", TGcollapse.rm = TRUE, output.list = TRUE))
    } else {return(NULL)}
  })

  universeMined <- reactive({
    validate(
      need(length(universeDataClean()) > 0,
           'There are zero lipids in the cleaned universe data')
    )
    if (mine_the_data$go) {
      return(lipid.miner(universeDataClean(), name="Query", TGcollapse.rm = TRUE, output.list = TRUE))
    } else {return(NULL)}
  })
  
  ## Display success message if everything is loaded correctly ##
  output$process_success <- renderUI({

    req(universeDataClean())
    req(queryDataClean())
    req(universeMined())
    req(queryMined())
    test1 <- universeDataClean()
    test2 <- queryDataClean()

    if (all(test2 %in% test1)){
      HTML('<h4 style= "color:#1A5276">Your data has been successfully uploaded and cleaned.
         You may proceed to the subsequent tabs for analysis.</h4>')
    }else{
      HTML(paste('<h4 style= "color:#cc3d16">', c('The following lipids are in the Query but not in the Universe: ', setdiff(test2, test1)),'</h4>', sep = "", collapse=""))
    }
  })

  output$QueryClean.txt <- downloadHandler(
    filename = function() {
      paste("Query_Data_Cleaned", ".txt", sep = "")
    },
    content = function(file) {
      query = queryDataClean()
      write.table(query, file, row.names = FALSE, sep = "\t")
    }
  )

  output$downloadQueryCleanUI <- renderUI({
    if (is.null(queryDataClean())) {
      return(NULL)
    } else {
      downloadButton(outputId = "QueryClean.txt", "Download Passed Query Data")
    }
  })



  output$UniverseClean.txt <- downloadHandler(
    filename = function() {
      paste("Universe_Data_Cleaned", ".txt", sep = "")
    },
    content = function(file) {
      universe = universeDataClean()
      write.table(universe, file, row.names = FALSE, sep = "\t")
    }
  )

  output$downloadUniverseCleanUI <- renderUI({
    if (is.null(universeDataClean())) {
      return(NULL)
    } else {
      downloadButton(outputId = "UniverseClean.txt", "Download Passed Universe Data")
    }
  })

  ####### Enrichment Analysis Tab #######
  output$tempplaceholder = renderText({
    if(input$precheck_click == 0) {
      return("Proceed with analysis by clicking the 'Process Data' button")
    } else {
      validate(
        need(!is.null(global_results()), message = "Something went wrong calculating the results"))
      if (is.null(input$ue_pval_thresh)) {
        p <- 0.05
      } else {
        p <- input$ue_pval_thresh
      }
      # Construct a status message of the form
      # Fisher output table (9 pvals < 0.05)
      return(paste(input$dd_enrich_test, " output table (",
            sum(global_results()$Pvalue < p),
            " pvals < ", p,
            ")", sep = ""))
    }


    })
  output$pvalue_text = renderText({"P-value filter"})
  output$pval_ui <- renderUI({
    if (input$cb_pval_filter) {
      tagList(
        selectInput("dd_pval_type", "",
                    choices = c("Unadjusted p-value (default)",
                                "Adjusted p-value"
                    )
        ),
        ### Actual p-value to use - user entry ### 4. THIS SHOULD ONLY BE VISIBLE OR BECOME ACTIVE IF THE P-VALUE FILTER CHECKBOX IS CHECKED
        textInput("ue_pval_thresh", "of", "0.05")
      )
    } else {
      return(NULL)
    }

  })

  #
  # End of get user inputs #


  #### Action Button Reactions ####

  # Run the specified test(s) when Process Data button is clicked #
  unfiltered_results <- eventReactive(input$precheck_click, {
    validate(
      # need cleaned query data #
      need(length(queryMined()) > 0,
           'Please upload and clean Query data.'),
      # need cleaned universe data #
      need(length(universeMined()) > 0,
           'Please upload and clean Universe data.'),
      # need (at the min) test type #
      need(input$dd_enrich_test != "none",
           'Please select an enrichment test to use.')
    )
    temp <- run_the_tests(Query.miner = queryMined(), Universe.miner = universeMined(),
                          test.type = input$dd_enrich_test, general.select = input$cb_test_params,
                          subset.by = input$dd_subset_id, subset.select = input$cb_params_subclass, enrich = FALSE)#, enrich = input$cb_pval_filter, pval = input$ue_pval_thresh, adjpval = input$ue_pval_thresh)

    return(temp)
    })
  #------ Visualization Object ------#
  global_results <- reactive({
    req(unfiltered_results())
    temp <- unfiltered_results()
    if (input$cb_pval_filter) {
      #figure out which filter to use
      if (input$dd_pval_type == "Adjusted p-value"){
        temp <- subset(temp, BHadjustPvalue <= input$ue_pval_thresh)
      } else if ( input$dd_pval_type == "Unadjusted p-value (default)"){
        temp <- subset(temp, Pvalue <= input$ue_pval_thresh)
      }
      temp <- subset(temp, fold.change > 1)
    }
    return(temp)
  })




  output$global_results_table <- DT::renderDataTable({
    req(global_results())
    display_table <- isolate(global_results())
    # display_table$Test.performed <- unlist(lapply(global_results()$Test.performed, function(x)stringr::str_split(x, pattern = "[(]")[[1]][1]))
    display_table$Test.performed <- unlist(lapply(global_results()$Test.performed, function(x)gsub(x, pattern = "\\s*\\([^\\)]+\\)", replacement="" )))
    brks <- c(0.01, 0.01001)
    clrs <- c("bold","weight", "weight")
    p <- datatable(display_table,
                   filter = 'top',
                   options = list(pageLength = 100, autoWidth = TRUE),
                   rownames = FALSE) %>%
      formatStyle("p-value", color = JS("value <= 0.05 ? 'red' : value > 0.05 ? 'black' : 'blue'"),
                  fontWeight = styleInterval(brks, clrs)) %>%
      formatStyle("FDR.q-value", color = JS("value <= 0.05 ? 'red' : value > 0.05 ? 'black' : 'blue'"),
                  fontWeight = styleInterval(brks, clrs)) %>%
      formatRound(columns=c('%.query', '%.universe','Fold.change'), digits = 2) %>%
      formatRound(columns=c('p-value', 'FDR.q-value','Fold.change'), digits = 4) %>%
      formatStyle("Fold.change", color = JS("value <= 0 ? 'black' : value > 0 ? 'green' : 'green'"),
                  fontWeight = styleInterval(0, c("weight", "bold")))
    #backgroundColor = styleInterval(brks, clrs))

    return(p)

  })

  output$downloadGlobalResultsUI <- renderUI({
    if (is.null(global_results())) {
      return(NULL)
    } else {
      downloadButton("downloadGlobalResults.txt", "Download Results Table")
    }
  })

  table_name <- reactive({
    validate(
      need(!is.null(global_results()), message = "Something went wrong calculating the results"))
    if (is.null(input$ue_pval_thresh)) {
      p <- 0.05
    } else {
      p <- input$ue_pval_thresh
    }
    return(paste(input$dd_enrich_test, " output table (",
                 sum(global_results()$Pvalue < p),
                 " pvals < ", p,
                 ")", sep = ""))
  })

  output$downloadGlobalResults.txt <- downloadHandler(
    filename = function() {
      paste(table_name(), ".txt", sep = "")
    },
    content = function(file) {
      display_table <- isolate(global_results())
      #test_display_name <- stringr::str_split(global_results()$Test.performed, pattern = "[(]")
      display_table$Test.performed <- unlist(lapply(global_results()$Test.performed, function(x)gsub(x, pattern = "\\s*\\([^\\)]+\\)", replacement="" )))
      
      write.table(display_table, file, row.names = FALSE)
    }
  )

  ####### Visualize Tab #######
  output$vizUI <- renderUI({
    #-------- Classification Charts --------#
    if (input$chooseplots == 1) {
      return(tagList(
        radioButtons(inputId = "type" ,label = tags$b("Type"),
                     choices = c("Category", "Main Class", "Subclass"),
                     selected = "Category"),

        checkboxInput(inputId = "pie1", label = "View as Pie Chart")
      )
      )
    }
    #-------- Chain Charts --------#
    if (input$chooseplots == 2) {
      return(checkboxInput(inputId = "pie2", label = "View as Pie Chart"))
    }
    #-------- Subset Charts --------#
    if (input$chooseplots == 3) {
      radioButtons(inputId = "classification_type" ,label = tags$b("Select a Classification Type"),
                   choices = c("All Chains", "Category", "Main Class", "Subclass"),
                   selected = "All Chains")
    }
  })

  output$chain_subset <- renderUI({
    req(input$classification_type)
    if (input$classification_type == "All Chains") {
      return(NULL)
    }
    else if (input$classification_type == "Category") {
      selectInput(inputId = "subset_name", tags$b("Select a Chain"),
                  choices = queryMined()$intact$Category,
                  selected = queryMined()$intact$Category[1])
    } else if (input$classification_type == "Main Class") {
      selectInput(inputId = "subset_name", tags$b("Select a Chain"),
                  choices = queryMined()$intact$`Main class`,
                  selected = queryMined()$intact$`Main class`[1])
    } else if (input$classification_type == "Subclass") {
      selectInput(inputId = "subset_name", tags$b("Select a Chain"),
                  choices = queryMined()$intact$`Sub class`,
                  selected = queryMined()$intact$`Sub class`[1])
    }
  })

  output$vizPlot <- renderPlotly({
    req(queryMined())
    # validate(need(!is.null(input$chooseplots), message = "Please select a plot to view"))
    #
    if (input$chooseplots == 1) {
      validate(need(!is.null(input$type), message = "Please select a plot type"))
      if (input$type == "Category") {
        if (input$pie1) {
          return(createCatPieCharts(pie_data1 = universeMined()$intact, pie_data2 =  queryMined()$intact,
                                    left_title = "Universe (Category)", right_title = "Query (Category)"))
        } else {
          p1 <- ggplotly(intact.cat.stack(universeMined()$intact), tooltip = 'tag')
          p2 <- ggplotly(intact.cat.stack(queryMined()$intact), tooltip = 'tag')
          return(subplot(p1,p2) %>% layout(annotations = list(
            list(
              x = 0.225,
              y = 1.0,
              font = list(size = 16),
              showarrow = FALSE,
              text = "Universe (Category)",
              xanchor = "center",
              xref = "paper",
              yanchor = "bottom",
              yref = "paper"
            ),
            list(
              x = 0.775,
              y = 1.0,
              font = list(size = 16),
              showarrow = FALSE,
              text = "Query (Category)",
              xanchor = "center",
              xref = "paper",
              yanchor = "bottom",
              yref = "paper"
            )
          ))
          )
        }
      }
      if (input$type == "Main Class") {
        if (input$pie1) {
          return(createMainPieCharts(pie_data1 = universeMined()$intact, pie_data2 =  queryMined()$intact,
                                     left_title = "Universe (Main Class)", right_title = "Query (Main Class)"))

        } else {
          p1 <- ggplotly(intact.main.stack(universeMined()$intact), tooltip = 'tag')
          p2 <- ggplotly(intact.main.stack(queryMined()$intact), tooltip = 'tag')
          return(subplot(p1,p2) %>% layout(showlegend = FALSE, annotations = list(
            list(
              x = 0.225,
              y = 1.0,
              font = list(size = 16),
              showarrow = FALSE,
              text = "Universe (Main Class)",
              xanchor = "center",
              xref = "paper",
              yanchor = "bottom",
              yref = "paper"
            ),
            list(
              x = 0.775,
              y = 1.0,
              font = list(size = 16),
              showarrow = FALSE,
              text = "Query (Main Class)",
              xanchor = "center",
              xref = "paper",
              yanchor = "bottom",
              yref = "paper"
            )
          ))
          )
        }
      }
      if (input$type == "Subclass") {
        if (input$pie1) {
          return(createSubPieCharts(pie_data1 = universeMined()$intact, pie_data2 =  queryMined()$intact,
                                    left_title = "Universe (Sub Class)", right_title = "Query (Sub Class)"))
        } else {
          p1 <- ggplotly(intact.sub.stack(universeMined()$intact), tooltip = 'tag')
          p2 <- ggplotly(intact.sub.stack(queryMined()$intact), tooltip = 'tag')
          return(subplot(p1,p2) %>% layout(showlegend = FALSE, annotations = list(
            list(
              x = 0.225,
              y = 0.97,
              font = list(size = 16),
              showarrow = FALSE,
              text = "Universe (Subclass)",
              xanchor = "center",
              xref = "paper",
              yanchor = "bottom",
              yref = "paper"
            ),
            list(
              x = 0.775,
              y = 0.97,
              font = list(size = 16),
              showarrow = FALSE,
              text = "Query (Subclass)",
              xanchor = "center",
              xref = "paper",
              yanchor = "bottom",
              yref = "paper"
            )
          ))
          )
        }
      }
    }
    else if (input$chooseplots == 2) {
      # chains
      validate(need(!is.null(input$pie2), message = ""))
      p1 <- ggplotly(chain.length.stack(universeMined()$chain), tooltip = 'tag')
      p2 <- ggplotly(chain.length.stack(queryMined()$chain), tooltip = 'tag')
      p3 <- ggplotly(chain.unsat.stack(universeMined()$chain), tooltip = 'tag')
      p4 <- ggplotly(chain.unsat.stack(queryMined()$chain), tooltip = 'tag')

      if (input$pie2) {
        unsat_colors <- c("#ff281d", "#ff6840", "#ff9750", "#ffc950")
        length_colors <- c("#17aeae","#6bbfb9", "#a7dbd9","#d7f4f0" )
        p1 <- plotly_data(p1) %>% mutate(Color = rev(length_colors))
        p2 <- plotly_data(p2) %>% mutate(Color = rev(length_colors))
        p3 <- plotly_data(p3) %>% mutate(Color = rev(unsat_colors))
        p4 <- plotly_data(p4) %>% mutate(Color = rev(unsat_colors))


        pp <- plot_ly() %>%
          add_pie(data = p1, labels = ~tag, values = ~percentage,
                  textposition = 'inside',
                  textinfo = 'label',
                  domain = list(x = c(0, 0.45), y = c(0.5, 0.9)),
                  marker = list(colors = p1$Color)) %>%
          add_pie(data = p2, labels = ~tag, values = ~percentage,
                  textposition = 'inside',
                  textinfo = 'label',
                  domain = list(x = c(0.55, 1), y = c(0.5, 0.9)),
                  marker = list(colors = p2$Color)) %>%
          add_pie(data = p3, labels = ~tag, values = ~percentage,
                  textposition = 'inside',
                  textinfo = 'label',
                  domain = list(x = c(0, 0.45), y = c(0, 0.4)),
                  marker = list(colors = p3$Color)) %>%
          add_pie(data = p4, labels = ~tag, values = ~percentage,
                  textposition = 'inside',
                  textinfo = 'label',
                  domain = list(x = c(0.55, 1), y = c(0, 0.4)),
                  marker = list(colors = p4$Color)) %>%
          layout(showlegend = FALSE, annotations = list(
            list(
              x = 0.225,
              y = 0.93,
              font = list(size = 16),
              showarrow = FALSE,
              text = "Universe Chain Length",
              xanchor = "center",
              xref = "paper",
              yanchor = "bottom",
              yref = "paper"
            ),
            list(
              x = 0.775,
              y = 0.93,
              font = list(size = 16),
              showarrow = FALSE,
              text = "Query Chain Length",
              xanchor = "center",
              xref = "paper",
              yanchor = "bottom",
              yref = "paper"
            ),
            list(
              x = 0.225,
              y = 0.43,
              font = list(size = 16),
              showarrow = FALSE,
              text = "Universe Chain Saturation",
              xanchor = "center",
              xref = "paper",
              yanchor = "bottom",
              yref = "paper"
            ),
            list(
              x = 0.775,
              y = 0.43,
              font = list(size = 16),
              showarrow = FALSE,
              text = "Query Chain Saturation",
              xanchor = "center",
              xref = "paper",
              yanchor = "bottom",
              yref = "paper"
            )
          ))
        return(pp)
      } else {
        subplot(p1,p2,p3,p4, nrows = 2) %>% layout(showlegend = FALSE, annotations = list(
          list(
            x = 0.225,
            y = 0.99,
            font = list(size = 16),
            showarrow = FALSE,
            text = "Universe Chain Length",
            xanchor = "center",
            xref = "paper",
            yanchor = "bottom",
            yref = "paper"
          ),
          list(
            x = 0.775,
            y = 0.99,
            font = list(size = 16),
            showarrow = FALSE,
            text = "Query Chain Length",
            xanchor = "center",
            xref = "paper",
            yanchor = "bottom",
            yref = "paper"
          ),
          list(
            x = 0.225,
            y = 0.47,
            font = list(size = 16),
            showarrow = FALSE,
            text = "Universe Chain Saturation",
            xanchor = "center",
            xref = "paper",
            yanchor = "bottom",
            yref = "paper"
          ),
          list(
            x = 0.775,
            y = 0.47,
            font = list(size = 16),
            showarrow = FALSE,
            text = "Query Chain Saturation",
            xanchor = "center",
            xref = "paper",
            yanchor = "bottom",
            yref = "paper"
          )
        ))
        #   grid.arrange(intact.main.stack(universeMined()$intact)+ggtitle("Main Universe"),intact.main.stack(queryMined()$intact)+ggtitle(" Main Query"),ncol=2)
        # plotly::ggplotly(grid.arrange(p1,p2,ncol=1))
      }
    }
    else if (input$chooseplots == 3) {
      req(input$classification_type)
      if (input$classification_type == "All Chains") {
        ggplotly(allchains.barplot(Y = universeMined()$allchains,
                                   X = queryMined()$allchains)+
                   ggtitle(paste("All chains"))+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle = 90), legend.title=element_blank())+
                   scale_fill_manual(values =c("grey","#336699")))
      }
      else if (input$classification_type == "Category") {
        validate(need(input$subset_name != "", message = "Please select a subset chain"))
        ggplotly(allchains.barplot(Y = subsetcat(universeMined()$allchains, cat = input$subset_name),
                                   X = subsetcat(queryMined()$allchains, cat = input$subset_name))+
                   ggtitle(paste("All chains of the category", input$subset_name))+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle = 90), legend.title=element_blank())+
                   scale_fill_manual(values =c("grey","#336699")))
      } else if(input$classification_type == "Main Class") {
        validate(need(input$subset_name != "", message = "Please select a subset chain"))
        ggplotly(allchains.barplot(Y = subsetmainclass(universeMined()$allchains, mainclass = input$subset_name),
                                   X = subsetmainclass(queryMined()$allchains, mainclass = input$subset_name))+
                   ggtitle(paste("All chains of the main class", input$subset_name))+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle = 90), legend.title=element_blank())+
                   scale_fill_manual(values = c("grey","#336699")))
      } else if(input$classification_type == "Subclass") {
        validate(need(input$subset_name != "", message = "Please select a subset chain"))
        ggplotly(allchains.barplot(Y = subsetsubclass(universeMined()$allchains, subclass = input$subset_name),
                                   X =subsetsubclass(queryMined()$allchains, subclass = input$subset_name))+
                   ggtitle(paste("All chains of the subclass", input$subset_name))+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle = 90), legend.title=element_blank())+
                   scale_fill_manual(values =c("grey","#336699")))
      }
    }
  })
  #------------------ Results Network ---------------#
  output$graph_pval_ui <- renderUI({
    if (!input$cb_pval_filter) {
      pv <- 1
    } else {
      pv <- as.numeric(input$ue_pval_thresh)
    }
    if (input$graph_pval_filter) {
      tagList(
        selectInput(inputId = "graph_pval_type", "",
                    choices = c("Unadjusted p-value (default)",
                                "Adjusted p-value"
                    ),
                    selected = "Unadjusted p-value (default)"
        ),
        ### Actual p-value to use - user entry ### 4. THIS SHOULD ONLY BE VISIBLE OR BECOME ACTIVE IF THE P-VALUE FILTER CHECKBOX IS CHECKED
        textInput(inputId = "graph_pval", label = tags$b("Select a value"), value = pv)
      )
    } else {
      return(NULL)
    }

  })

  #------- Network Object ------#
  global_results_network <- reactive({
    req(unfiltered_results())
    temp <- unfiltered_results()
    if (input$graph_pval_filter) {
      #figure out which filter to use
      if (input$graph_pval_type == "Adjusted p-value"){
        temp <- subset(temp, BHadjustPvalue <= input$graph_pval)
      } else if ( input$graph_pval_type == "Unadjusted p-value (default)"){
        temp <- subset(temp, Pvalue <= input$graph_pval)
      }
      temp <- subset(temp, fold.change > 1)
    }
    return(temp)
  })

  network_components <- reactive({
    if (input$graph_pval_filter) {
      req(input$graph_pval_type)
      if (input$graph_pval_type == "Unadjusted p-value (default)"){
        lipid_network_maker(queryMined()$intact$Lipid, global_results_network(),
                            pval = as.numeric(input$graph_pval))
      } else if (input$graph_pval_type == "Adjusted p-value"){
        lipid_network_maker(queryMined()$intact$Lipid, global_results_network(),
                            adjpval = as.numeric(input$graph_pval))
      }
    } else {
      lipid_network_maker(queryMined()$intact$Lipid, global_results_network())
    }

    colnames(network.nodes_attributes) <- c("label","title","color.background")
    network.nodes_attributes2 <- cbind(id=paste0("s",1:nrow(network.nodes_attributes)),network.nodes_attributes,shape=c("dot", "diamond")[as.numeric(network.nodes_attributes$title)],size=c(5, 50)[as.numeric(network.nodes_attributes$title)],borderWidth=0)
    network.nodes_attributes2$title <- network.nodes_attributes2$label
    network.edges_attributes2 <- data.frame(from=network.nodes_attributes2$id[match(network.edges_attributes$Lipid.name,network.nodes_attributes2$label)],to=network.nodes_attributes2$id[match(network.edges_attributes$Class,network.nodes_attributes2$label)],color=network.edges_attributes$Color,width=1)
    return(list(Nodes = network.nodes_attributes2, Edges = network.edges_attributes2))
  })

  output$graphplaceholder = renderText({
    if (input$precheck_click == 0) {
      return("Please Process Data on the Enruchment Analysis tab to continue")
    } else {
      return(NULL)
    }
  })
  output$network <- renderVisNetwork({
    if (input$precheck_click == 0) {
      return(NULL)
    } else{
       return(visNetwork(network_components()$Nodes, network_components()$Edges, width = "100%", height = "800px") %>%
                visOptions(highlightNearest = TRUE, selectedBy = "type.label",manipulation=T)) %>%
                visEvents(stabilized = "function() { this.setOptions({nodes : {physics : false}})}") %>%
                visNodes(font = '18px arial #343434')

    }

  })

  #------ Make graph available for download -----#
  output$NetworkNodes.txt <- downloadHandler(
    filename = function() {
      paste("Network_nodes", ".txt", sep = "")
    },
    content = function(file) {
      temp <- network_components()$Nodes
      write.table(temp, file, sep = "\t")
    }
  )

  output$downloadNetworkNodesUI <- renderUI({
    if (is.null(queryMined())) {
      return(NULL)
    } else {
      downloadButton(outputId = "NetworkNodes.txt", "Download Network Nodes")
    }
  })

  output$NetworkEdges.txt <- downloadHandler(
    filename = function() {
      paste("Network_edges", ".txt", sep = "")
    },
    content = function(file) {
      temp <- network_components()$Edges
      write.table(temp, file, sep = "\t")
    }
  )

  output$downloadNetworkEdgesUI <- renderUI({
    if (is.null(queryMined())) {
      return(NULL)
    } else {
      downloadButton(outputId = "NetworkEdges.txt", "Download Network Edges")
    }
  })

  output$NetworkEdgeAttributes.txt <- downloadHandler(
    filename = function() {
      paste("Network_edge_attributes", ".txt", sep = "")
    },
    content = function(file) {
      temp <- network.edges_attributes
      write.table(temp, file, sep = "\t")
    }
  )

  output$downloadNetworkEdgeAttributesUI <- renderUI({
    if (is.null(queryMined())) {
      return(NULL)
    } else {
      downloadButton(outputId = "NetworkEdgeAttributes.txt", "Download Network Edge Attributes")
    }
  })
})
