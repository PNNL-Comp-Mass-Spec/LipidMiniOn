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
                  tabPanel("Upload",
                           fileInput("file1", "Upload Universe File (.csv)",
                                     multiple = TRUE,
                                     accept = c("text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv")),
                           # tags$hr(),
                           fileInput("file_emeta", "Upload Enriched File (.csv)",
                                     multiple = TRUE,
                                     accept = c("text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv")),
                           actionButton('click', 'Load Files')),
                  tabPanel("Visualize",
                           dataTableOutput("head_file1"),
                           dataTableOutput('head_emeta'))
                  )
                  )
  )
)

