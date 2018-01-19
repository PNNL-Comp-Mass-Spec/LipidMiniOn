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
#                            # Enriched File - character vector #
#                            fileInput("enriched", "Upload Enriched File (.csv)",
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
#                            dataTableOutput("head_enriched")
#                            ),
#                   
#                   ### Visualize Tab ###
#                   tabPanel("Visualize",
#                            dataTableOutput("head_universe"),
#                            dataTableOutput("head_enriched")
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
                             
                             ## Load Universe file ##
                             fileInput("universe", "Upload 'Universe' Lipid Names (.csv)",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             
                             # Load Enriched file
                             fileInput("enriched", "Upload 'Enriched' Lipid Names (.csv)",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")), 
                             
                             # Process data (clean it) #
                             actionButton('process_click', 'Process Data')
                           ), 
                           
                           ## Main Panel ##
                           mainPanel(
      
                              textOutput("num_universe"), 
                              dataTableOutput("head_universe"),
                               
                              textOutput("num_enriched"), 
                              dataTableOutput("head_enriched"), 
                             
                              textOutput("process_success")
                           )
                          )),


                ################## Preprocess Panel ##################
                tabPanel("Enrichment Analysis",
                         sidebarLayout(
                           sidebarPanel(
                             checkboxGroupInput('tests', 'I want to use the following:',
                                                c("Fisher's Exact Test",
                                                  "Binomial Test",
                                                  "EASE score"),
                                                selected = ''),
                             

                             selectInput("chooseplots", "To test",
                                         choices = c("Intact" = 1,
                                                     "Chain" = 2,
                                                     "All Chains" = 3)
                             ),
                             
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

