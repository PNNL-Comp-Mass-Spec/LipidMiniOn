#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)
# 
# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
# 
#   # App title ----
#   titlePanel("Lipid Mini-On: MINIng and ONtology"),
# 
#     # Main panel for displaying outputs ----
#     mainPanel(
# 
#       # Output: Tabset w/ Upload and Visualize (graph) tabs
#       tabsetPanel(type = "tabs",
#                   
#                   ### Upload Tab ###
#                   tabPanel("Data Upload",
#                            
#                            # Universe File - character vector #
#                            fileInput("universe", "Upload Universe File (.csv)",
#                                      multiple = TRUE,
#                                      accept = c("text/csv",
#                                                 "text/comma-separated-values,text/plain",
#                                                 ".csv")),
#                            # tags$hr(),
#                            
#                            # Query File - character vector #
#                            fileInput("query", "Upload Query File (.csv)",
#                                      multiple = TRUE,
#                                      accept = c("text/csv",
#                                                 "text/comma-separated-values,text/plain",
#                                                 ".csv")),
#                            
#                            # Action Button - process the files #
#                            actionButton("click", "Process Data")
#                            ),
#                   
#                   ### Statistical Tests Tab ###
#                   tabPanel("Statistical Test(s)",
#                            dataTableOutput("head_universe"),
#                            dataTableOutput("head_query")
#                            ),
#                   
#                   ### Visualize Tab ###
#                   tabPanel("Visualize",
#                            dataTableOutput("head_universe"),
#                            dataTableOutput("head_query")
#                            
#                            )
#                   )
#                   )
#   )
# )



#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # App title ----
  titlePanel("Lipid Mini-On: MINIng and ONtology"),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    # Output: Tabset w/ Upload and Visualize (graph) tabs
    tabsetPanel(type = "tabs",
                
                ################## Upload Panel ##################
                tabPanel("Upload",
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             ## Load Query file ##
                             fileInput("query", "Upload 'Query' Lipid Names (.csv)",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")), 
                             
                             ## Load Universe file ##
                             fileInput("universe", "Upload 'Universe' Lipid Names (.csv)",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             
                             ## Process data button (clean it) ##
                             actionButton('process_click', 'Process Data')
                           ), 
                           
                           ## Main Panel ##
                           mainPanel(
                             uiOutput("process_success"),

                             fluidRow(
                               column(
                                 6, h2("Query"), hr(),
                                 htmlOutput("num_query"), 
                                 DT::dataTableOutput("head_query")
                               ),

                               column(
                                 6, h2("Universe"), hr(),
                                 htmlOutput("num_universe"), 
                                 DT::dataTableOutput("head_universe")
                               )
                             )
                              
                              
                           )
                          )),


                ################## Preprocess Panel ##################
                tabPanel("Enrichment Analysis",
                         sidebarLayout(
                           sidebarPanel(
                             
                             ### Enrichment Test - dropdown ###
                             selectInput('dd_tests', 'Enrichment test to use:',
                                                choices = c("Please select a test", 
                                                            "Fisher's exact test",
                                                            "EASE score (DAVID)",
                                                            "Binomial test",
                                                            "Hypergeometric test")
                                         ),
                             
                             hr(),
                             
                             ### General Parameters to Test - checkbox group ###
                             checkboxGroupInput("cb_params", "General parameters to test",
                                         choices = c("Category" = "cat",
                                                     "Main class" = "main",
                                                     "Sub-class" = "sub",
                                                     "Individual chains (e.g. Fatty acids)" = "chains",
                                                     "Individual chain length and number of double bonds" = "length"),
                                         selected = c("cat", "main", "sub", "chains", "length")
                                         ),
                             
                             ### Main Class Specific Parameters - checkbox group ###
                             checkboxGroupInput("cb_params_mainclass", "Main class specific parameters",
                                                choices = c("Total number of chain carbon within each class" = 1,
                                                            "Total number of chain double bond within each class" = 2)
                                                ),
                             
                             hr(),
                             
                             ### P-value Threshold - user entry ###
                             
                             
                             ### Test P-values - dropdown ###
                             
                             
                             ### Output Filter - checkbox ###
                             checkboxInput("cb_nofilter", "Don't filter the output data", 
                                           value = FALSE
                                           ),
                             
                             ### Process Data - button ###
                             actionButton("preprocess_click", "Process Data")


                           ),
                           
                           
                           mainPanel(# Add plots and visualization stuff here
                             textOutput("tempplaceholder"),
                             width = 7)


                         )
                ),
                ################## Visualize Panel ##################
                tabPanel("Visualize",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput('chooseplots', 'I want to plot a',
                                         choices = c('Pie Chart' = 1,
                                                     'Other?' = 2)
                             )#,
                             # selectInput('choose_single', 'Using a(n)',
                             #             choices = c('Single sample' = 1, 'Overlay of multiple samples' = 2)),
                             # conditionalPanel(
                             #   condition = 'input.choose_single == 2',
                             #   {fluidRow(
                             #     column(6,
                             #            uiOutput('whichGroups1')
                             #     ),
                             #     column(6,
                             #            uiOutput('whichGroups2')
                             #     )
                             #   )})
                           ),
                           mainPanel(
                             # conditionalPanel(
                             #   condition = "input.chooseplots == 1",
                             #   plotOutput('vankrev'),
                             #   selectInput('placeholder_1', 'Use boundary:',
                             #               choices = c('None' = 0, 'A' = 1, 'B' = 2, 'C' = 3))
                             # ),
                             conditionalPanel(
                               condition = "input.chooseplots == 2",
                               plotOutput('kendrick')
                             ),
                             selectInput('placeholder_1', 'Color by:',
                                         choices = c('Placeholder 1' = 1, 'Placeholder 2' = 2)),

                             width = 7)
                         )
                )
    ))
)
)

