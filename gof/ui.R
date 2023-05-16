#
library(shiny)
library(ggplot2)
library(DT)
library(tidyverse)
library(shinydashboard)
library(plotly)

# Define UI 
ui <- dashboardPage(skin="black",
                    dashboardHeader(titleWidth=200),
                    dashboardSidebar(width = 250,
                                     sidebarMenu(
                                       menuItem("Statistical Test", tabName= "stattest")
                                     ) # end sidebarMenu
                                     ), # end dashboardSidebar
                    dashboardBody(
                      tabItems(
                        tabItem(tabName ="stattest",
                                tabBox(
                                       id = "tabbox1",
                                       width = 12,
                                       tabPanel("Data Load",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    fileInput("file", "Choose Excel/CSV File", accept = c(".csv",".xlsx",".xls"))
                                                  ),
                                                  mainPanel(
                                                  checkboxInput("header","Header", TRUE),
                                                  selectInput("filetype", "File Type", choices=list("EXCEL", "CSV"))))
                                                ),
                                       tabPanel("Data Description",
                                                h4("Data Summary"),
                                                verbatimTextOutput("datadescript"),
                                                h4("Data Report"),
                                                actionButton("report_maid","Generate Data Maid Report", class="btn-success"),
                                                actionButton("report_explorer","Generate Data Explorer", class="btn-primary", style="color:white;margin-right:5px;"),
                                                actionButton("report_smart","Generate Data Smart", class="btn-warning", style="color:white;margin-right:5px;"),
                                                verbatimTextOutput("error"),
                                                htmlOutput("report")
                                
                                                ),
                                       tabPanel("Data Table",
                                                h4("This Table is editable."),
                                                
                                                h5("How: Double-click on individual data cell to make change."),
                                                h6("Plots and analysis in the subsequent tabs are updated interactively !!!"),
                                                tags$style("h6{color: red; font-style:italic;}","h5{color:blue}"),
                                
                                                DT::dataTableOutput("table")),
                                       tabPanel("Data Plot",
                                                h3("Scatter Plot"),
                                                h5("X and Y are selected from dropdown menus below."),
                                                fluidRow(
                                                column(width = 3,selectInput("column1_tabplot", "Select X axis", NULL)),
                                                column(width = 3,selectInput("column2_tabplot", "Select Y axis", NULL)),
                                                column(width = 3,selectInput("column3_tabplot", "Size", NULL)), 
                                                column(width = 3,selectInput("column4_tabplot", "Fill", NULL))),# end fluidRow
                                                plotlyOutput("plot", height=600)),
                                       tabPanel("CDF Composite",
                                                h3("Cumulative Density Function"),
                                                fluidRow(
                                                column(width = 3,selectInput("column1_tabcdf", "Univariate Parameter", NULL))  
                                                ), # end fluidRow
                                                h5("Plot shows different Cumulative Density Function (CDF) curves."),
                                                h5("The empirical CDF showed in black dotted-line indicating the actual data, in comparision with other theoretical CDFs which computed from normal and lognormal parameters.",style="color:black;"),
                                                h5("The plot can be used to quickly interpret which distribution type could be matched the data.",style="color:green;"),
                                                plotOutput("plot1",width=800, height=600)),
                                       tabPanel("Goodness of Fit",
                                                h4("Goodness of Fit Test", style="color:green;"),
                                                h5("Data is selected from the tab CDF Comp > Univariate Paramter"),
                           
                                                fluidRow(
                                                  column(width = 3,selectInput("column1_tabgof","Distribution",c("Normal"="norm","LogNormal"="lnorm","Gamma"="gamma","Exponential"="exp"))),
                                                  column(width = 3,selectInput("column2_tabgof", "Statistical Method",c("Chi-square"="chisq")))
                                              
                                                  ),
                                                h4("Visual Check:",style="color:red;"),
                                                h5("From left-right and top-bottom are histogram, quantile-quantile (Q-Q), cumulative density function, and probability-probability plots."),
                                                plotOutput("plot2", height=800),
                                                h4("Statistical Test:",style="color:red;"),
                                                h5("Goodness-of-Fit Test is interpreted using Null Hypothesis."),
                                                h5("p-value is computed from statistical test method and to compare with significance level [alpha], hereby is set to 0.05 to make inferrence on hythothesis.",style="font-style:italic;"),
                                                h5("If p-value > 0.05, it can be inferred that the sample data is generated from a selected theoretical distribution.",style="color:green;"),
                                                verbatimTextOutput("fitdist")),
                               
 
                                       ) # end tabBox
                                )
                      )
                    )
                    ) # end dashboardPage

# fluidPage(
#   title = 'Goodness of Fit Statistical Test',
#   h1('Goodness of Fitness'),
#   titlePanel("Upload CSV file and Table/Plot Display"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       fileInput("file", "Choose CSV File", accept = ".csv")
#     ),
#       checkboxInput("header","Header", TRUE)),
#       selectInput("column1", "Select X Column", NULL),
#       selectInput("column2", "Select Y Column", NULL),
#       
#       selectInput("column3", "Select data for GoF", NULL),
#       selectInput("column4", "Distribution:",c("Normal"="norm","LogNormal"="lnorm","Gamma"="gamma","Exponential"="exp")),
#       selectInput("column5", "Statistical Test Method:",c("Chi-squared"="chisq")),
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Data Table", DT::dataTableOutput("table")),
#         tabPanel("Data Plot", plotOutput("plot")),
#         tabPanel("CDF Comp", plotOutput("plot1")),
#         tabPanel("Goodness of Fitness", plotOutput("plot2"), verbatimTextOutput("fitdist")),
#         tabPanel("Markdown")
#         )
#     )
#   )
