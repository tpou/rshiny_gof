#
library(shiny)
library(ggplot2)
library(DT)
library(tidyverse)
library(shinydashboard)
library(plotly)

parameter1_tabs <- tabsetPanel( # define flexible params for distr.
  id = "params1",
  type = "hidden",
  tabPanel("normal",
           numericInput("mean1", "mean", value=0),
           numericInput("sd1","standard deviation", min=0, value=1)),
  tabPanel("uniform",
           numericInput("min1", "min", value=0),
           numericInput("max1", "max", value=1)),
  tabPanel("exponential",
           numericInput("rate1","rate",value=1,min=0))
)
parameter2_tabs <- tabsetPanel( # define flexible params for distr.
  id = "params2",
  type = "hidden",
  tabPanel("normal",
           numericInput("mean2", "mean", value=0),
           numericInput("sd2","standard deviation", min=0, value=1)),
  tabPanel("uniform",
           numericInput("min2", "min", value=0),
           numericInput("max2", "max", value=1)),
  tabPanel("exponential",
           numericInput("rate2","rate",value=1,min=0))
)
parameter3_tabs <- tabsetPanel( # define flexible params for distr.
  id = "params3",
  type = "hidden",
  tabPanel("normal",
           numericInput("mean3", "mean", value=0),
           numericInput("sd3","standard deviation", min=0, value=1)),
  tabPanel("uniform",
           numericInput("min3", "min", value=0),
           numericInput("max3", "max", value=1)),
  tabPanel("exponential",
           numericInput("rate3","rate",value=1,min=0))
)
parameter4_tabs <- tabsetPanel( # define flexible params for distr.
  id = "params4",
  type = "hidden",
  tabPanel("normal",
           numericInput("mean4", "mean", value=0),
           numericInput("sd4","standard deviation", min=0, value=1)),
  tabPanel("uniform",
           numericInput("min4", "min", value=0),
           numericInput("max4", "max", value=1)),
  tabPanel("exponential",
           numericInput("rate4","rate",value=1,min=0))
)
# Define UI 
ui <- dashboardPage(skin="black",
                    dashboardHeader(titleWidth=200),
                    dashboardSidebar(width = 200,
                                     sidebarMenu(
                                       menuItem("Data Load", tabName= "dataload"),
                                       menuItem("Data View", tabName ="dataview"),
                                       menuItem("Data Viz", tabName = "dataviz"),
                                       menuItem("Statiscal Test", tabName ="statisticaltest"),
                                       menuItem("Monte Carlo Simulation", tabName ="montecarlo")
                                     ) # end sidebarMenu
                                     ), # end dashboardSidebar
                    dashboardBody(
                      tabItems(
                        tabItem(tabName="dataload",
                                fluidRow(
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
                                    )
                            
                                  ) # end tabBox
                                )#endfluidRow
                                ),
                        
                        tabItem(tabName="dataview",
                                fluidRow(
                                  tabBox(
                                    id = "tabbox2",
                                    width = 12,
      
                                    tabPanel("Data View",
                                             h4("This Table is editable."),
                                             h5("How: Double-click on individual data cell to make change."),
                                             h6("Plots and analysis in the subsequent tabs are updated interactively !!!"),
                                             fluidRow(
                                               column(width = 4, selectInput('first_filter', "Filter by Column 1:", NULL)),
                                               column(width = 4, selectInput('first_filter_value', 'Value 1:',NULL))
                                             ),
                                             fluidRow(
                                               column(width = 4, selectInput('second_filter', "Filter by Column 2:", NULL)),
                                               column(width = 4, selectInput('second_filter_value', 'Value 2:',NULL))
                                             ),
                                             tags$style("h6{color: red; font-style:italic;}","h5{color:blue}"),
                                             
                                             DT::dataTableOutput("table")),
                                    tabPanel("Data Description",
                                             h4("Data Summary"),
                                             verbatimTextOutput("datadescript"),
                                             h4("Data Report"),
                                             fluidRow(
                                               column(width = 4, actionButton("report_maid","Generate Data Maid Report", class="btn-success")),
                                               column(width = 4, actionButton("report_explorer","Generate Data Explorer", class="btn-primary", style="color:white;")),
                                               column(width = 4, actionButton("report_smart","Generate Data Smart", class="btn-warning", style="color:white;"))
                                             ),
                                             #actionButton("report_maid","Generate Data Maid Report", class="btn-success"),
                                             #actionButton("report_explorer","Generate Data Explorer", class="btn-primary", style="color:white;margin-right:5px;"),
                                             #actionButton("report_smart","Generate Data Smart", class="btn-warning", style="color:white;margin-right:5px;"),
                                             verbatimTextOutput("error"),
                                             htmlOutput("report")
                                             
                                    )
                                    ) # end tabBox
                                ) #endfluidRow
                                ),
                        tabItem(tabName="dataviz",
                                fluidRow(
                                  tabBox(
                                    id = "tabbox3",
                                    width = 12,
                                   
                                      tabPanel("Univariate",
                                             fluidRow(
                                               h2("Univariate"),
                                               selectInput("column1_univ", "Select Variable:", NULL),
                                               box(
                                                 title ='Histogram', status='primary', solidHeader = TRUE,collapsible = TRUE,
                                                 sliderInput("number_of_bins", "Choose the bin size:",value=10,min=0,max=100,step=2),
                                                 plotlyOutput("plotviz1",height=250)
                                               ),
                                               box(
                                                 title ='Density', status='warning', solidHeader = TRUE,collapsible = TRUE,
                                                 selectInput("column2_univ", "Filtered By", NULL),
                                                 plotlyOutput("plotviz1.2",height=250)
                                               )
                                               
                                             ), # endFluidRow
                                             fluidRow(
                                               
                                             )
                                             
                                        ),
                                    tabPanel("Bivariates",
                                             fluidRow(
                                               box(
                                                 title ='Scatter Plot', status='info', solidHeader = TRUE, collapsible = TRUE,
                                                 h5("X and Y are selected from dropdown menus below."),
                                                 fluidRow(
                                                   column(width = 3,selectInput("column1_tabplot", "Select X axis", NULL)),
                                                   column(width = 3,selectInput("column2_tabplot", "Select Y axis", NULL)),
                                                   column(width = 3,selectInput("column3_tabplot", "Size", NULL))),# end fluidRow
                                                 plotlyOutput("plotviz2", height=250)
                                               )
                                             ) #endfluidRow
                                             )
                                    
                                  ) # end tabBox
                                ) #endFluidRow
                                ),
                        tabItem(tabName ="statisticaltest",
                                fluidRow(
                                tabBox(
                                       id = "tabbox4",
                                       width = 12,
                                       
                                       tabPanel("CDF Composite",
                                                h3("Cumulative Density Function"),
                                                fluidRow(
                                                column(width = 3,selectInput("column1_tabcdf", "Univariate Parameter", NULL))  
                                                ), # end fluidRow
                                                h5("Plot shows different Cumulative Density Function (CDF) curves."),
                                                h5("The empirical CDF showed in black dotted-line indicating the actual data, in comparision with other theoretical CDFs which computed from normal and lognormal parameters.",style="color:black;"),
                                                h5("The plot can be used to quickly interpret which distribution type could be matched the data.",style="color:green;"),
                                                plotOutput("plot1",width=800, height=600),
                                                verbatimTextOutput("error2")),
                                       
                                       tabPanel("Goodness of Fit",
                                                h4("Goodness of Fit Test", style="color:green;"),
                                                h5("Data is selected from the tab CDF Comp > Univariate Paramter"),
                           
                                                fluidRow(
                                                  column(width = 3,selectInput("column1_tabgof","Distribution",c("Normal"="norm","LogNormal"="lnorm","Gamma"="gamma","Exponential"="exp","Binominal"="bninorm","Uniform"="unif"))),
                                              
                                                  ),
                                                h4("Visual Check:",style="color:red;"),
                                                h5("From left-right and top-bottom are histogram, quantile-quantile (Q-Q), cumulative density function, and probability-probability plots."),
                                                plotOutput("plot2", height=600),
                                                verbatimTextOutput("error3"),
                                                
                                                h4("Statistical Test:",style="color:red;"),
                                                selectInput("column2_tabgof", "Statistical Method",c("Chi-square"="chisq","Shapiro-Wilk"="sw","Kolmogorov-Smirnov"="ks")),
                                                h5("Goodness-of-Fit Test is interpreted using Null Hypothesis."),
                                                h5("p-value is computed from statistical test method and to compare with significance level [alpha], hereby is set to 0.05 to make inferrence on hythothesis.",style="font-style:italic;"),
                                                h5("If p-value > 0.05, it can be inferred that the sample data is generated from a selected theoretical distribution.",style="color:green;"),
                                                verbatimTextOutput("fitdist")),
                               
                                       tabPanel("Markdown",
                                                h4("Distribution"),
                                                h5("Normal Distribution"),
                                                h5("Binominal Distribution"),
                                                h5("Uniform Distribution"),
                                                h5("Gamma Distribution"),
                                                h5("Null Hypothesis"),
                                                h4("Statistical Test"),
                                                h5("Chi-square"),
                                                h5("Shapiro-Wilk"),
                                                h5("Kolmogorov-Smirnov"),
                                                uiOutput("markdown"))
 
                                       ) # end tabBox
                                       )
                                ),
                        tabItem(tabName="montecarlo",
                                fluidRow(
                                  tabBox(
                                    id = "tabbox5",
                                    width = 12,
                                    
                                    tabPanel("Input",
                                             fluidRow(
                                               column(width=2,selectInput("column1_tabinput", "Var 1", NULL)),
                                               column(width=2,selectInput("column2_tabinput", "Var 2", NULL)),
                                               column(width=2,selectInput("column3_tabinput", "Var 3", NULL)),
                                               column(width=2,selectInput("column4_tabinput", "Var 4", NULL))
                                             ), #endfluidRow
                                             ),
                                    tabPanel("Define Distribution",
                                             fluidRow(
                                             sidebarPanel(
                                               width = 4,
                                               numericInput("n","Number of samples", value=100))
                                             ),
                                             fluidRow(
                                               
                                               box(
                                                 title ='Var 1 Distribution', solidHeader = TRUE,collapsible = TRUE,
                                                 selectInput("dist_var1", "Distribution",
                                                             choices=c("normal","uniform","exponential")),
                                               
                                                 parameter1_tabs,
                                                 plotOutput("plot_distr_var1")
                                               ),
                                               box(
                                                 title ='Var 2 Distribution', solidHeader = TRUE,collapsible = TRUE,
                                                 selectInput("dist_var2", "Distribution",
                                                             choices=c("normal","uniform","exponential")),
                                                 
                                                 parameter2_tabs,
                                                 plotOutput("plot_distr_var2")
                                               ), 
                                               box(
                                                 title ='Var 3 Distribution', solidHeader = TRUE,collapsible = TRUE,
                                                 selectInput("dist_var3", "Distribution",
                                                             choices=c("normal","uniform","exponential")),
                                                 
                                                 parameter3_tabs,
                                                 plotOutput("plot_distr_var3")
                                               ),
                                               box(
                                                 title ='Var 4 Distribution', solidHeader = TRUE,collapsible = TRUE,
                                                 selectInput("dist_var4", "Distribution",
                                                             choices=c("normal","uniform","exponential")),
                                                 
                                                 parameter4_tabs,
                                                 plotOutput("plot_distr_var4")
                                               ), 
                                             ) #endfluidRow
                                             ),
                                    tabPanel("Monte Carlo Sim"),
                                    tabPanel("Result",
                                             fluidRow(
                                               box(
                                                 title='Result',solidHeader=TRUE,collapsible = TRUE,
                                                 plotOutput("plot_result_mc")
                                               )
                                             ))
                                    
                                  )
                                ))
                      )
                    )
                    ) # end dashboardPage


