
load(url("https://aiprediction.s3.us-east-2.amazonaws.com/Dashboard.RData"))
load(url("https://aiprediction.s3.us-east-2.amazonaws.com/Summary.RData"))
load(url("https://aiprediction.s3.us-east-2.amazonaws.com/DrillingSchedule.RData"))

#Summary <-datatable(Summary,colnames=c("Block","BTU_Prod","GasVol_Prod","Condensate_Prod","GHV","WobbleIndex","CH4",
#                                       "CO2","N2","Inert","P10Water_Prod","P50Water_Prod","No.WHP_Hubs_Instl","No_Delin",
#                                       "No_Wells","NoRigMoves","AvgDepth","No_InjWells","No_DepletedWells","No_DepletedHub",
#                                       "No_HubInst","NoWHP-HubsInst","NoWHPonline","BTUSale","GasSale","GasResAdd"))
#rstudio.github.io/DT/options.html

blocks <- sort(unique(Summary$Block))

# Deploy app:
#library(rsconnect)
#rsconnect::deployApp('path/to/your/app')

library(ggplot2)
library(reshape2)
library(shiny)
library(shinydashboard)
library(plotly)

Dashboard2<- melt(Dashboard,id.vars="Date",measure.vars=c("N","W","S","OT"))
#----------------------------------------------------------------------------------------------------------------------------
dbHeader <- dashboardHeader(titleWidth=250)
anchor <-tags$a(tags$img(src='header1.png',height='50',width='100'),'iG Forecast')
dbHeader$children[[2]]$children <- tags$div(align="left",tags$head(tags$style(HTML(".name {
                                                                                            font-family:Palatino Header;
                                                                                            font-size:1em;
                                                                                            font-weight:bold;
                                                                                            color:red}"))),
                                                         class = 'name',anchor)


#----------------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(skin="black",
        dbHeader,
        #--------------------------------------------------------------------------------------------------------------------
        dashboardSidebar(width=250,
           sidebarMenu(
             menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
             menuItem("Schedule", tabName = "schedule",icon = icon("calendar")),
             menuItem("Model", tabName = "model", icon = icon("cogs")),
             menuItem("Production Allocation", tabName ="prodallocation", icon = icon("")),
             menuItem("Decline Curve", tabName ="declinecurve", icon = icon("chart-line")),
             menuItem("Statistical Regression",tabName="statistic", icon=icon("brain")),
             menuItem("Machine Learning", tabName = "machinelearning", icon = icon("brain"))
                      ) # end sidebarMenu              
                         
                         
                         
                         
                         ), # end dashboardSidebar
        #--------------------------------------------------------------------------------------------------------------------
        dashboardBody(
          tabItems(
              tabItem( tabName ="dashboard",

                    tabBox(title="Dashboard",
                           id = "tabbox1",
                           width = 12,
                           
                           #height = "750px",
                           tabPanel("Plot",
                                    fluidRow(
                                        column(width = 6,
                                               plotlyOutput("plot1"),
                                               verbatimTextOutput("event1")),
                                        column(width = 6,
                                               plotlyOutput("plot2")),
                                               verbatimTextOutput(("event2"))
                                            ),
                                    fluidRow(
                                      column(width = 6,
                                             plotlyOutput("plot3"),
                                             verbatimTextOutput("event3")),
                                      column(width = 6,
                                             plotlyOutput("plot4"),
                                             verbatimTextOutput("event4"))                                      
                                            )
                                    ),
                           tabPanel("Summary",
                                    fluidRow(
                                      selectInput(inputId="block",
                                                label="Select Block:",
                                                choices=blocks,
                                                selected="B",
                                                multiple=TRUE)
                                            ),
                                    fluidRow(
                                      DT::dataTableOutput(outputId="summary")
                                            )),
                           tabPanel("Table",
                                    sliderInput("slider","Slider Input:",1,100,5),
                                    selectInput(inputId="x",
                                                label = "X-axis",
                                                choices = c("Date","N"),
                                                selected="Date"),
                                    selectInput(inputId="y",
                                                label = "Y-axis",
                                                choices = c("Capacity","Condensate"),
                                                selected="Capacity")))
                          
                      ),
              #--------------------------------------------------------------------------------------------------------------
              tabItem( tabName ="schedule",
                  h2("Drilling Schedule"),
                    plotlyOutput("plot5"),
                    verbatimTextOutput("event5") 
                      ),
              #--------------------------------------------------------------------------------------------------------------
              tabItem( tabName ="model",
                  h2("Model")
                      ),
              tabItem( tabName ="prodallocation",
                  h2("Production Allocation")  
                      ),
              tabItem( tabName ="declinecurve",
                  h2("Machine Learning")
                      ),
              tabItem( tabName ="statistic",
                  h2("Statistical Regression")     
                       ),
              tabItem( tabName ="machinelearning",
                  h2(" Machine Learning")
                      )
            
                  )
                      ) # end dashboardBody
                    ) # end ui dashboardPage

#----------------------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  #Dashboard/Plot
  output$plot1 <- renderPlotly({
    g1<-ggplot(data=Dashboard,aes(x=Date)) + geom_line(aes(y=Capacity),size=0.5) +
      geom_line(aes(y=Gas.Production),color="red",size=1.5)+
      geom_line(aes(y=MDQ),color="turquoise2",size=1)
    ggplotly(g1)                             })
  
  output$event1<-renderPrint({
    d1<- event_data("plotly_hover")
    if (is.null(d1)) "hover on a point!" else d1
  })

  output$plot2 <- renderPlotly({
    g2<-ggplot(data=Dashboard,aes(x=Date)) + geom_line(aes(y=Capacity.Volume)) +
      geom_area(data=Dashboard2,aes(y=value,color=variable,fill=variable))
    ggplotly(g2)                             })
  
  output$event2<-renderPrint({
    d2<- event_data("plotly_hover")
    if (is.null(d2)) "hover on a point!" else d2
  })
  
  output$plot3 <- renderPlotly({
    g3<-ggplot(data=Dashboard,aes(x=Date)) + geom_line(aes(y=Condensate,color=Condensate),color="seagreen",size=1.5) +
      geom_line(aes(y=Water),color="blue",size=1.5)
    ggplotly(g3)                         })
  
  output$event3<-renderPrint({
    d3<- event_data("plotly_hover")
    if (is.null(d3)) "hover on a point!" else d3
  })
  
  output$plot4 <- renderPlotly({
  plot_ly() %>%
      add_lines(x=Dashboard$Date,y=Dashboard$CO2,name="CO2") %>%
      add_lines(x=Dashboard$Date,y=Dashboard$N2,name="N2") %>%
      add_lines(x=Dashboard$Date,y=Dashboard$BTU,name="BTU",yaxis="y2")%>%
      layout(yaxis2=list(overlaying="y",side="right"))
  })
  output$event4 <- renderPrint({
    d4<- event_data("plotly_hover")
    if (is.null(d4)) "hover on a point!" else d4
  })
  
  output$plot5 <- renderPlotly({
    drlsch$Start <- as.Date(drlsch$Start,format = "%m/%d/%Y")
    client = "BaseCase Development"
    cols <- RColorBrewer::brewer.pal(length(unique(drlsch$Rig)), name = "Set1")
    drlsch$color <- factor(drlsch$Rig, labels = cols)
    p <- plot_ly()
    
    # Each task is a separate trace
    # Each trace is essentially a thick line plot
    # x-axis ticks are dates and handled automatically
    
    for(i in 1:(nrow(drlsch) - 1)){
      p <- add_trace(p,
                     x = c(drlsch$Start[i], drlsch$Start[i] + drlsch$Duration[i]),  # x0, x1
                     y = c(i, i),  # y0, y1
                     mode = "lines",
                     line = list(color = drlsch$color[i], width = 20),
                     showlegend = F,
                     hoverinfo = "text",
                     
                     # Create custom hover text
                     
                     text = paste("Project: ", drlsch$Project[i], "
                                  ",
                                  "Duration: ", drlsch$Duration[i], "days
                                  ",
                                  "Rig: ", drlsch$Rig[i]),
                     
                     evaluate = T  # needed to avoid lazy loading
      )
    }
    
    # add title name
    a <- list(xref = "paper",
              yref = "paper",
              x = 0.1,
              y = 1,
              xanchor = "left",
              text = paste0("Drilling Schedule: ", client),
              font = list(color = 'chocolate', size = 20, family = "Helvetica"),
              ax = 0,
              ay = 0,
              align = "center",
              showarrow = FALSE)
    
    
    p <- p %>% layout(annotations = a) 
    
    return(p)
  })
  output$event5 <- renderPrint({
    d4<- event_data("plotly_hover")
    #if (is.null(d5)) "hover on a point!" else d5
  })
  
  #Dashboard/Summary
  output$summary <-DT::renderDataTable({
    summary_from_selected_block <- Summary %>%
      filter(Block %in% input$block) %>%
      select(Block:P90.Water)
    DT::datatable(data = summary_from_selected_block,
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
}

#----------------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)