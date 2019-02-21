#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(visNetwork)
library(shinycssloaders)

# Define UI for application that draws a histogram
tagList(


  
  # App title ----
  # titlePanel("Lipid Mini-On: MINIng and ONtology"),
  
  # Main panel for displaying outputs ----
  #mainPanel(
  
  # Output: Tabset w/ Upload and Visualize (graph) tabs

  navbarPage(title = div(img(src = "logoteal.png", height = 85, width = 120), "Lipid Mini-On"),
             windowTitle = "Lipid Mini-On",
             theme = "yeti.css",
             ################## Upload Panel ##################
             tabPanel("Upload", 
                      sidebarLayout(
                        
                        sidebarPanel(
                          shiny::tabsetPanel(
                            tabPanel(title = "Query/Universe",
                                     ## Load Query file ##
                                     br(),
                                     fluidRow(
                                       column(width = 12,
                                              fileInput("query", tags$b("Upload 'Query' Lipid Names (.csv)"),
                                                        multiple = TRUE,
                                                        accept = c("text/csv",
                                                                   "text/comma-separated-values,text/plain",
                                                                   ".csv"))
                                       )
                                     ), 
                                     fluidRow(
                                       column(width = 12,
                                              ## Load Universe file ##
                                              fileInput("universe", tags$b("Upload 'Universe' Lipid Names (.csv)"),
                                                        multiple = TRUE,
                                                        accept = c("text/csv",
                                                                   "text/comma-separated-values,text/plain",
                                                                   ".csv"))
                                              #textInput("universe", "Upload 'Universe' Lipid Names")
                                       )
                                     ),
                                     ## Description of what the "Check Data" button does #
                                     textOutput("CleaningDescription"),
                                     br(),
                                     
                                     ## Process data button (clean it) ##
                                     actionButton('check_click', 'Check Data')
                                     ),
                            # tabPanel(title = "Text Upload",
                            #          br(),
                            #          ## Load Query file ##
                            #          fluidRow(
                            #            column(width = 12,
                            #                   textAreaInput("query_text", tags$b("Enter 'Query' Lipid Names")
                            #                                 )
                            #            )
                            #          ), 
                            #          fluidRow(
                            #            column(width = 12,
                            #                   ## Load Universe file ##
                            #                   textAreaInput("universe_text", tags$b("Enter 'Universe' Lipid Names"))
                            #                   #textInput("universe", "Upload 'Universe' Lipid Names")
                            #            )
                            #          ),
                            #          ## Description of what the "Check Data" button does #
                            #          textOutput("CleaningDescription1"),
                            #          br(),
                            #          
                            #          ## Process data button (clean it) ##
                            #          actionButton('check_click1', 'Check Data')
                            # ),
                            tabPanel(title = "Ranked Table",
                                     br(),
                                     column(width = 12,
                                            ## Load Universe file ##
                                            fileInput("rank_table", tags$b("Upload 'Ranking' Table (.csv)"),
                                                      multiple = TRUE,
                                                      accept = c("text/csv",
                                                                 "text/comma-separated-values,text/plain",
                                                                 ".csv"))
                                            #textInput("universe", "Upload 'Universe' Lipid Names")
                                     ),
                                     radioButtons(inputId = "asc_desc_button", label = tags$b("Rank Order"),
                                                  choices = c("Ascending" = "ascending", "Descending" = "descending")),
                                     ## Description of what the "Check Data" button does #
                                     textOutput("CleaningDescription2"),
                                     br(),
                                     
                                     ## Process data button (clean it) ##
                                     actionButton('check_click2', 'Check Data')
                            ),
                            tabPanel(title = "Examples",
                                     br(),
                                     tags$h4(tags$b("Download example files for analysis")),
                                     br(),
                                     fluidRow(
                                       column(width = 6, tags$p("Query - Human Lung Endothelial Cells"),
                                              downloadButton("Query_Human_Lung_Endothelial_Cells.txt", "Download Query")),
                                       column(width = 6,tags$p("Universe - Human Lung Endothelial Cells"),
                                              downloadButton("Universe_Human_Lung_Endothelial_Cells.txt", "Download Universe"))
                                     ),
                                     
                                     br(),
                                     fluidRow(
                                       column(width = 6, tags$p("Query - Peat Soil"),
                                              downloadButton("Query_Soil_Surface.txt", "Download Query")),
                                       column(width = 6,tags$p("Universe - Peat Soil"),
                                              downloadButton("Universe_Soil_Surface.txt", "Download Universe"))
                                     ),
                                     br(),
                                     
                                     fluidRow(
                                       column(width = 10, tags$p("Rank Table - Endothelial"),
                                              downloadButton("Rank_Table_Endothelial_vs_Whole_Lysate.csv", "Download Table"),offset = 3)
                                     )
                                     
                                     )
                        )), 
                        
                        ## Main Panel ##
                        mainPanel(
                          width = 8,

                          # Summary table giving number lipids # 1. HAVING TROUBLE WITH THE REACTIVITY HERE (SEE THE SERVER FILE FOR A NOTE ON WHAT I'M TRYING TO DO), ALSO GETTING ERROR ABOUT SUBSETTING...
                          
                            conditionalPanel(condition = 'input.check_click == 0 & input.check_click2 == 0',{
                              img(src="displaylogo.png", height = "60%", width = "60%")
                            }),
                            uiOutput("process_success"),
                        wellPanel(
                          width = 6,
                            tableOutput('summary_data')
                        ),

                        # conditionalPanel(condition = 'input.check_click2 > 0',{
                        #   uiOutput("process_success_rank_table")
                        # }),
                        # conditionalPanel(condition = 'input.check_click2 > 0',{
                        #   wellPanel(
                        #     width = 6,
                        #     tableOutput('summary_data_rank_table')
                        #   )}),
                          # Download the cleaned data (.txt files) # 2. THESE BUTTONS SHOULD ONLY APPEAR OR BECOME ACTIVE ONCE THE DATA HAS BEEN PROCESSED SUCCESSFULLY VIA THE "CHECK DATA" BUTTON
                          
                        fluidRow(
                            column(width = 4,
                                   uiOutput("downloadQueryCleanUI")),
                            column(width = 4,
                                   uiOutput("downloadUniverseCleanUI"))
                          ),
                        plotOutput("rankplot")
                          #downloadButton("downloadQueryClean", "Download Cleaned Query Data"),
                          #downloadButton("downloadUniverseClean", "Download Cleaned Universe Data")
                        )
                      )),
             
             
             ################## Enrichment Analysis Panel ##################
             tabPanel("Enrichment Analysis",
                      #conditionalPanel(condition = 'input.check_click > 0 ',{
                        uiOutput("EnrichmentUI")
                      # sidebarLayout(
                      #       sidebarPanel(
                      #         ### Enrichment Test - dropdown ###
                      #         selectInput('dd_enrich_test', tags$b('Enrichment test to use:'),
                      #                     choices = c("Fisher's exact (default)" = "Fisher",
                      #                                 "EASE score (DAVID)" = "EASE",
                      #                                 "Binomial" = "Binom",
                      #                                 "Hypergeometric" = "Hyper"),
                      #                     selected = "Fisher's exact (default)"),
                      #         
                      #         
                      #         
                      #         ### General Parameters to Test - checkbox group ###
                      #         checkboxGroupInput("cb_test_params", tags$b("General parameters to test"),
                      #                            choices = c("Category" = "cat",
                      #                                        "Main class" = "main",
                      #                                        "Subclass" = "sub",
                      #                                        "Individual chains (e.g. fatty acids)" = "chains",
                      #                                        "Individual chain length and number of double bonds" = "length"),
                      #                            selected = c("cat", "main", "sub", "chains", "length")
                      #         ),
                      #         
                      #         hr(),
                      #         
                      #         
                      #         ### Subset-specific Test - drop down ###
                      #         # (should be "none" by default but can be also "category", "mainclass", "subclass")
                      #         selectInput('dd_subset_id', tags$b('Subset to test:'),
                      #                     choices = c("None (default)", 
                      #                                 "All" = "all",
                      #                                 "Category" = "category",
                      #                                 "Main Class" = "mainclass",
                      #                                 "Subclass" = "subclass"
                      #                     )
                      #         ),
                      #         
                      #         
                      #         ### What to look at in the subset - checkbox group ###
                      #         checkboxGroupInput("cb_params_subclass", tags$b("Parameters to test within each subset"),
                      #                            choices = c("Total number of chain carbon" = "total_carbon",
                      #                                        "Total number of double bonds" = "total_insaturation",
                      #                                        "Individual chains (e.g. fatty acids)" = "specific_chains")
                      #         ),
                      #         
                      #         
                      #         hr(),
                      #         
                      #         ### P-value Filter - checkbox ###
                      #         tags$b(p(textOutput("pvalue_text"))), 
                      #         checkboxInput("cb_pval_filter", "Enrichment analysis with a p-value filter",
                      #                       value = FALSE),
                      #         ### Unadjusted or Adjusted? - dropdown ### 3. THIS SHOULD ONLY BE VISIBLE OR BECOME ACTIVE IF THE P-VALUE FILTER CHECKBOX IS CHECKED
                      #         uiOutput("pval_ui"),
                      #         hr(),
                      #         
                      #         ### Process Data - button ###
                      #         actionButton("precheck_click", "Process Data")
                      #         
                      #       ),
                      #       
                      #       
                      #       mainPanel(# Display globaloutput upon successful click of the "Process Data" button
                      #         width = 7, 
                      #         
                      #         fluidRow(
                      #           column(width = 5, textOutput("tempplaceholder")),
                      #           column(width = 3, uiOutput("downloadGlobalResultsUI"))),
                      #         hr(),
                      #         
                      #         uiOutput("param_check"),
                      #         #tableOutput("global_results_table")
                      #         DT::dataTableOutput("global_results_table")
                      #         
                      #       )
                      #       )
                     #     }),
                      ######## KS ######
                      # conditionalPanel(condition = 'input.check_click2 > 0',{
                      #   sidebarLayout(
                      #     sidebarPanel(
                      #       ### Enrichment Test - dropdown ###
                      #       selectInput('dd_enrich_test1', tags$b('Enrichment test to use:'),
                      #                   choices = c("Weighted Kolmogorov–Smirnov Test" = "KS"),
                      #                   selected = "Weighted Kolmogorov–Smirnov Test"),
                      # 
                      # 
                      # 
                      #        ### General Parameters to Test - checkbox group ###
                      #        checkboxGroupInput("cb_test_params_rank", tags$b("General parameters to test"),
                      #                           choices = c("Category" = "cat",
                      #                                       "Main class" = "main",
                      #                                       "Subclass" = "sub",
                      #                                       "Individual chains (e.g. fatty acids)" = "chains",
                      #                                       "Individual chain length and number of double bonds" = "length"),
                      #                           selected = c("cat", "main", "sub", "chains", "length")
                      #        ),
                      #     
                      #        hr(),
                      #     
                      #     
                      #        ### Subset-specific Test - drop down ###
                      #        # (should be "none" by default but can be also "category", "mainclass", "subclass")
                      #        selectInput('dd_subset_id_rank_table', tags$b('Subset to test:'),
                      #                    choices = c("None (default)",
                      #                                "All" = "all",
                      #                                "Category" = "category",
                      #                                "Main Class" = "mainclass",
                      #                                "Subclass" = "subclass"
                      #                    )
                      #        ),
                      #     
                      #     #
                      #     #   ### What to look at in the subset - checkbox group ###
                      #        checkboxGroupInput("cb_params_subclass_rank_table", tags$b("Parameters to test within each subset"),
                      #                           choices = c("Total number of chain carbon" = "total_carbon",
                      #                                       "Total number of double bonds" = "total_insaturation",
                      #                                       "Individual chains (e.g. fatty acids)" = "specific_chains")
                      #        ),
                      #    
                      #        hr(),
                      #     
                      #        ### P-value Filter - checkbox ###
                      #        tags$b(p(textOutput("pvalue_text_rank_table"))),
                      #        checkboxInput("cb_pval_filter_rank_table", "Enrichment analysis with a p-value filter",
                      #                      value = FALSE),
                      #        ### Unadjusted or Adjusted? - dropdown ### 3. THIS SHOULD ONLY BE VISIBLE OR BECOME ACTIVE IF THE P-VALUE FILTER CHECKBOX IS CHECKED
                      #        uiOutput("pval_ui_rank_table"),
                      #        hr(),
                      #     
                      #        ### Process Data - button ###
                      #        actionButton("precheck_click_rank_table", "Process Data")
                      #     
                      #      ),
                      #      
                      #     
                      #     mainPanel(# Display globaloutput upon successful click of the "Process Data" button
                      #       width = 7, 
                      #       
                      #       fluidRow(
                      #         column(width = 5, textOutput("tempplaceholder_rank_table")),
                      #         column(width = 3, uiOutput("downloadGlobalResultsUIRankTable"))),
                      #       hr(),
                      #       
                      #       uiOutput("param_check_rank_table"),
                      #       #tableOutput("global_results_table")
                      #       DT::dataTableOutput("global_results_rank_table")
                      #       
                      #     )
                      #   )
                      # })
                      ),
             ################## Visualize Panel ##################
             tabPanel("Visualize",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput('chooseplots', tags$b('I want to view'),
                                      choices = c('Classifications' = 1,
                                                  'Fatty Acid Characteristics' = 2,
                                                  'Specific Chains (e.g. fatty acids)' = 3)
                          ),
                          uiOutput("vizUI"),
                          conditionalPanel(condition = 'input.chooseplots == 3', {
                            uiOutput("chain_subset")
                          })
                        ),
                        
                        mainPanel(
                          plotlyOutput("vizPlot", width = "700px", height = "700px")
                        )
                      )
             ),
             tabPanel("Results Network",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxInput("graph_pval_filter", label = tags$b("Check Box to Filter by P-Value"), value = TRUE),
                          uiOutput("graph_pval_ui"),
                          fluidRow(
                            column(width = 6,
                              uiOutput("downloadNetworkNodesUI")
                            )),
                          br(),
                          fluidRow(
                            column(width = 6,
                                   uiOutput("downloadNetworkEdgesUI")
                                   )
                          ),
                          br(),
                          fluidRow(
                            column(width = 6,
                                   uiOutput("downloadNetworkEdgeAttributesUI")
                            )
                          )
                        ),
                        mainPanel( 
                          textOutput("graphplaceholder"),
                      withSpinner(visNetworkOutput("network", width = "100%", height = "800px"))
                      )
                      )
                      ),
             tabPanel("About Mini-On",
                      h4("Links"),
                      p(HTML("<a href='https://scholar.google.com/schhp?hl=en'>Cite Us</a><br/><br/><a href='https://github.com/'>Github</a><br></br>")),
                      h4("About Us"),
                      includeMarkdown("About.md")
                      ),
             tabPanel("Help",
                      h4("Contact Us"),
                      img(src = "logoteal.png", height = 340, width = 480),
                      br(),
                      h4(HTML("<a href='https://youtube.com'>YouTube</a>"))
                      )
  )
)



