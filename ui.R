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
                            tabPanel(title = "File Upload",
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
                            tabPanel(title = "Text Upload",
                                     br(),
                                     ## Load Query file ##
                                     fluidRow(
                                       column(width = 12,
                                              textAreaInput("query_text", tags$b("Enter 'Query' Lipid Names")
                                                            )
                                       )
                                     ), 
                                     fluidRow(
                                       column(width = 12,
                                              ## Load Universe file ##
                                              textAreaInput("universe_text", tags$b("Enter 'Universe' Lipid Names"))
                                              #textInput("universe", "Upload 'Universe' Lipid Names")
                                       )
                                     ),
                                     ## Description of what the "Check Data" button does #
                                     textOutput("CleaningDescription1"),
                                     br(),
                                     
                                     ## Process data button (clean it) ##
                                     actionButton('check_click1', 'Check Data')
                            ),
                            tabPanel(title = "Examples",
                                     br(),
                                     tags$p("Download example files for analysis"),
                                     br(),
                                     fluidRow(
                                       column(width = 6, tags$p("Example 1 Query"),
                                              downloadButton("downloadExample1.1", "Download Query")),
                                       column(width = 6,tags$p("Example 1 Universe"),
                                              downloadButton("downloadExample1.2", "Download Universe"))
                                     ),
                                     
                                     br(),
                                     fluidRow(
                                       column(width = 6, tags$p("Example 2 Query"),
                                              downloadButton("downloadExample2.1", "Download Query")),
                                       column(width = 6,tags$p("Example 2 Universe"),
                                              downloadButton("downloadExample2.2", "Download Universe"))
                                     )
                                     
                                     )
                        )), 
                        
                        ## Main Panel ##
                        mainPanel(
                          width = 8,

                          # Summary table giving number lipids # 1. HAVING TROUBLE WITH THE REACTIVITY HERE (SEE THE SERVER FILE FOR A NOTE ON WHAT I'M TRYING TO DO), ALSO GETTING ERROR ABOUT SUBSETTING...
                          
                            conditionalPanel(condition = 'input.check_click == 0 & input.check_click1 == 0',{
                              img(src="displaylogo.png", height = "60%", width = "60%")
                            }),
                          conditionalPanel(condition = 'input.check_click > 0 | input.check_click1 > 0',{
                            uiOutput("process_success")
                          }),
                        conditionalPanel(condition = 'input.check_click > 0 | input.check_click1 > 0',{
                        wellPanel(
                          width = 6,
                            tableOutput('summary_data')
                        )
                        }),
                          # Download the cleaned data (.txt files) # 2. THESE BUTTONS SHOULD ONLY APPEAR OR BECOME ACTIVE ONCE THE DATA HAS BEEN PROCESSED SUCCESSFULLY VIA THE "CHECK DATA" BUTTON
                          fluidRow(
                            column(width = 4,
                                   uiOutput("downloadQueryCleanUI")),
                            column(width = 4,
                                   uiOutput("downloadUniverseCleanUI"))
                          )
                          #downloadButton("downloadQueryClean", "Download Cleaned Query Data"),
                          #downloadButton("downloadUniverseClean", "Download Cleaned Universe Data")
                        )
                      )),
             
             
             ################## Enrichment Analysis Panel ##################
             tabPanel("Enrichment Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          
                          ### Enrichment Test - dropdown ###
                          selectInput('dd_enrich_test', tags$b('Enrichment test to use:'),
                                      choices = c("Fisher's exact (default)" = "Fisher",
                                                  "EASE score (DAVID)" = "EASE",
                                                  "Binomial" = "Binom",
                                                  "Hypergeometric" = "Hyper"),
                                                  selected = "Fisher's exact (default)"),
                          
                          
                          
                          ### General Parameters to Test - checkbox group ###
                          checkboxGroupInput("cb_test_params", tags$b("General parameters to test"),
                                             choices = c("Category" = "cat",
                                                         "Main class" = "main",
                                                         "Subclass" = "sub",
                                                         "Individual chains (e.g. fatty acids)" = "chains",
                                                         "Individual chain length and number of double bonds" = "length"),
                                             selected = c("cat", "main", "sub", "chains", "length")
                          ),
                          
                          hr(),
                          
                          
                          ### Subset-specific Test - drop down ###
                          # (should be "none" by default but can be also "category", "mainclass", "subclass")
                          selectInput('dd_subset_id', tags$b('Subset to test:'),
                                      choices = c("None (default)", 
                                                  "All" = "all",
                                                  "Category" = "category",
                                                  "Main Class" = "mainclass",
                                                  "Subclass" = "subclass"
                                      )
                          ),
                          
                          
                          ### What to look at in the subset - checkbox group ###
                          checkboxGroupInput("cb_params_subclass", tags$b("Parameters to test within each subset"),
                                             choices = c("Total number of chain carbon" = "total_carbon",
                                                         "Total number of double bonds" = "total_insaturation",
                                                         "Individual chains (e.g. fatty acids)" = "specific_chains")
                          ),
                          
                          
                          hr(),
                          
                          ### P-value Filter - checkbox ###
                          tags$b(p(textOutput("pvalue_text"))), 
                          checkboxInput("cb_pval_filter", "Enrichment analysis with a p-value filter",
                                        value = FALSE),
                          ### Unadjusted or Adjusted? - dropdown ### 3. THIS SHOULD ONLY BE VISIBLE OR BECOME ACTIVE IF THE P-VALUE FILTER CHECKBOX IS CHECKED
                          uiOutput("pval_ui"),
                          hr(),
                          
                          ### Process Data - button ###
                          actionButton("precheck_click", "Process Data")

                        ),
                        
                        
                        mainPanel(# Display globaloutput upon successful click of the "Process Data" button
                          width = 7, 
                          
                          fluidRow(
                            column(width = 5, textOutput("tempplaceholder")),
                            column(width = 3, uiOutput("downloadGlobalResultsUI"))),
                          hr(),
                          
                          uiOutput("param_check"),
                          
                          DT::dataTableOutput("global_results_table")
                          
                        )
                        
                        
                        
                        
                      )
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
                      p("Lipidomics is...")
                      
                      ),
             tabPanel("Help",
                      h4("Contact Us"),
                      img(src="logoteal.pdf"),
                      br(),
                      h4(HTML("<a href='https://youtube.com'>YouTube</a>"))
                      )
  )
)



