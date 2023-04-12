
# reference ggplot N2, CO2 and GHV:

output$plot4 <- renderPlot({
  ggplot(data=Dashboard,aes(x=Date)) + geom_line(aes(y=CO2),color="purple",size=1) +
    geom_line(aes(y=N2),color="orange", size=1)+ 
    geom_line(aes(y=BTU/50),color="red1",size=1)+
    scale_y_continuous(sec.axis=sec_axis(~.*50,name="GHV"))+
    scale_colour_manual(values=c("purple","orange"), name ="Fluid",labels=c("CO2","N2","GHV"))+
    labs(y="CO2 / N2",color="Parameters")+
    theme(plot.title = element_text(hjust=0.5),legend.position=c(0.8,0.9))+
    ggtitle("CO2/ N2 Forecast")
})

# Drilling Schedule:

library(plotly)
 #Load data:
 drlsch <- read.csv("DrillingChart.csv")
 drlsch$Start <- as.Date(drlsch$Start,format = "%m/%d/%Y")
 
 # Sample client name
 client = "BaseCase Development"
 
 # Choose colors based on number of resources
 cols <- RColorBrewer::brewer.pal(length(unique(drlsch$Rig)), name = "Set1")
 drlsch$color <- factor(drlsch$Rig, labels = cols)
 
 # Initialize empty plot
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
 
 p
 