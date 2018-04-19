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

# Define UI for application that draws a histogram
shinyUI(#fluidPage(

  
  # App title ----
  # titlePanel("Lipid Mini-On: MINIng and ONtology"),
  
  # Main panel for displaying outputs ----
  #mainPanel(
  
  # Output: Tabset w/ Upload and Visualize (graph) tabs
  navbarPage(title = div(img(src = "logoteal.png", height = 60, width = 145)),
             windowTitle = "Lipid Mini-On: MINIng and ONtology",
             theme = "yeti.css",

             
             ################## Upload Panel ##################
             tabPanel("Upload", 
                      sidebarLayout(
                        
                        sidebarPanel(
                          ## Load Query file ##
                          fluidRow(
                            column(width = 12,
                                   fileInput("query", "Upload 'Query' Lipid Names (.csv)",
                                             multiple = TRUE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv"))
                            )
                          ), 
                          fluidRow(
                            column(width = 12,
                                   ## Load Universe file ##
                                   fileInput("universe", "Upload 'Universe' Lipid Names (.csv)",
                                             multiple = TRUE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv"))
                            )
                          ),
                          
                          ## Description of what the "Check Data" button does #
                          textOutput("CleaningDescription"),
                          
                          ## Process data button (clean it) ##
                          actionButton('check_click', 'Check Data')
                        ), 
                        
                        ## Main Panel ##
                        mainPanel(
                          width = 8,
                          uiOutput("process_success"),
                          
                          # Summary table giving number lipids # 1. HAVING TROUBLE WITH THE REACTIVITY HERE (SEE THE SERVER FILE FOR A NOTE ON WHAT I'M TRYING TO DO), ALSO GETTING ERROR ABOUT SUBSETTING...
                          wellPanel(
                            width = 6,
                            tableOutput('summary_data')
                          ),
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
                          selectInput('dd_enrich_test', 'Enrichment test to use:',
                                      choices = c("Please select a test" = "none", 
                                                  "Fisher's exact test" = "Fisher",
                                                  "EASE score (DAVID)" = "EASE",
                                                  "Binomial test" = "Binom",
                                                  "Hypergeometric test" = "Hyper")
                          ),
                          
                          
                          
                          ### General Parameters to Test - checkbox group ###
                          checkboxGroupInput("cb_test_params", "General parameters to test",
                                             choices = c("Category" = "cat",
                                                         "Main class" = "main",
                                                         "Sub-class" = "sub",
                                                         "Individual chains (e.g. Fatty acids)" = "chains",
                                                         "Individual chain length and number of double bonds" = "length"),
                                             selected = c("cat", "main", "sub", "chains", "length")
                          ),
                          
                          hr(),
                          
                          
                          ### Subset-specific Test - drop down ###
                          # (should be "none" by default but can be also "category", "mainclass", "subclass")
                          selectInput('dd_subset_id', 'Subset to test:',
                                      choices = c("None", 
                                                  "Category",
                                                  "Main Class",
                                                  "Sub Class"
                                      )
                          ),
                          
                          
                          ### What to look at in the subset - checkbox group ###
                          checkboxGroupInput("cb_params_subclass", "Main class specific parameters",
                                             choices = c("Total number of chain carbon within each class" = "total_carbon",
                                                         "Total number of chain insaturations" = "total_insaturation",
                                                         "Specific chains" = "specific_chains")
                          ),
                          
                          
                          hr(),
                          
                          ### P-value Filter - checkbox ###
                          textOutput("pvalue_text"),
                          checkboxInput("cb_pval_filter", "Implement a p-value filter by subsetting to lipids with a(n)",
                                        value = FALSE),
                          
                          ### Unadjusted or Adjusted? - dropdown ### 3. THIS SHOULD ONLY BE VISIBLE OR BECOME ACTIVE IF THE P-VALUE FILTER CHECKBOX IS CHECKED
                          selectInput("dd_pval_type", "",
                                      choices = c("Unadjusted p-value", 
                                                  "Adjusted p-value"
                                      )
                          ),
                          
                          ### Actual p-value to use - user entry ### 4. THIS SHOULD ONLY BE VISIBLE OR BECOME ACTIVE IF THE P-VALUE FILTER CHECKBOX IS CHECKED
                          textInput("ue_pval_thresh", "of", "0.05"),
                          
                          
                          
                          hr(),
                          
                          ### Process Data - button ###
                          actionButton("precheck_click", "Process Data")
                          
                          
                        ),
                        
                        
                        mainPanel(# Display globaloutput upon successful click of the "Process Data" button
                          width = 7, 
                          
                          textOutput("tempplaceholder"),
                          
                          uiOutput("param_check"),
                          
                          DT::dataTableOutput("global_results_table")
                          
                        )
                        
                        
                        
                        
                      )
             ),
             ################## Visualize Panel ##################
             tabPanel("Visualize",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput('chooseplots', 'I want to view',
                                      choices = c('Classifications' = 1,
                                                  'Fatty Acid Characteristics' = 2,
                                                  'Specific Chains' = 3)
                          ),
                          uiOutput("vizUI")
                        ),
                        
                        mainPanel(
                          plotlyOutput("vizPlot", width = "700px", height = "700px")
                        )
                      )
             )
  )
)


