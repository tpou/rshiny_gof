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
  titlePanel("CSV Upload and Table/Plot Display"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
    ),
      checkboxInput("header","Header", TRUE),),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DT::dataTableOutput("table")),
        tabPanel("Plot", plotOutput("plot"))
      )
    )
  )

)
