library(ggplot2)
library(reshape2)
library(shiny)
library(shinydashboard)
library(plotly)

load(url("https://rshiny.s3.ap-southeast-1.amazonaws.com/Dashboard.RData"))
load(url("https://rshiny.s3.ap-southeast-1.amazonaws.com/Summary.RData"))
load(url("https://rshiny.s3.ap-southeast-1.amazonaws.com/DrillingSchedule.RData"))

Dashboard2<- melt(Dashboard,id.vars="Date",measure.vars=c("N","W","S","OT"))

server <- function(input, output) {
  
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
  
  #interactive CO2/N2/BTU plot + hover
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
  #Drilling Schedule:
  output$plot5 <- renderPlotly({
    drlsch$Start <- as.Date(drlsch$Start,format = "%m/%d/%Y")
    client = "BaseCase Development"
    cols <- RColorBrewer::brewer.pal(length(unique(drlsch$Rig)), name = "Set1")
    drlsch$color <- factor(drlsch$Rig, labels = cols)
    p <- plot_ly()
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

