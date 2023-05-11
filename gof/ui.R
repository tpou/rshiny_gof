#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)
library(tibble)

# Define UI for application that draws a histogram
fluidPage(

    title = 'Goodness of Fit',
    
    h1('Test'),
    
    sidebarLayout(
      
      sidebarPanel(
        fileInput("file", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ), #end_fileInput
        
        DTOutput("mytable"),
        
        tags$hr(),
        
        checkboxInput("header", "Header", TRUE),
        actionButton("myButton","Plot"),
        actionButton('myButtongo', 'Update')
      ), #end_sidebarPanel
      
      mainPanel(
        tabsetPanel(
          tabPanel("Table",DT::dataTableOutput("head")),
          tabPanel("Plot",plotOutput('myplot')),
          tabPanel("Plot Update", plotOutput('myplot2'))
        )
      )
      #mainPanel(
      #  fluidRow(
      #          column(6, dt_output('Test','contents')),
      #          column(6, plotOutput('x1', height = 500))
      #  )
      ), #end_mainPanel
    # Define JavaScript code to handle cell clicks ----
    tags$script('
    $(document).on("click", "#DataTables_Table_1 td", function() {
      var col_idx = $(this).index();
      var row_idx = $(this).parent().index();
      Shiny.setInputValue("cell_edit", [row_idx, col_idx]);
    });
  ')

)
