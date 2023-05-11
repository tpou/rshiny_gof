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
fluidPage(

    title = 'Goodness of Fit',
    
    h1('Test'),
    
    sidebarLayout(
      
      sidebarPanel(
        fileInput("upload", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ), #end_fileInput
        numericInput("n", "Rows", value=5, min=1, step=1),
        DTOutput("mytable"),
        
        tags$hr(),
        checkboxInput("header", "Header", TRUE),
        actionButton("myButton","Plot"),
        actionButton('myButtongo', 'Update')
      ), #end_sidebarPanel
      
      mainPanel(
      DT::dataTableOutput("head"),
      plotOutput('myplot'),
      plotOutput('myplot2')
      )
      #mainPanel(
      #  fluidRow(
      #          column(6, dt_output('Test','contents')),
      #          column(6, plotOutput('x1', height = 500))
      #  )
      ) #end_mainPanel
)
