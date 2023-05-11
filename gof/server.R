#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

   myplot = reactiveVal()
    mydata = reactiveVal()
    
    # client-side processing

    output$head <- DT::renderDataTable({
      inFile <- input$upload
      if (is.null(inFile))
        return (NULL)
      df <- read.csv(inFile$datapath, header=input$header)
    })
    
    
    observeEvent(input$myButton, {
      inFile2 <- input$upload
      if(is.null(inFile2))
        return(NULL)
      data1 = read.csv(inFile2$datapath, header=input$header)
      
      
      mydata(data1)
      
      output$mytable <- renderDT({
        DT::datatable(data1, editable = TRUE)
      })
      
      myplot(ggplot(data1, aes(x=x, y=y)) + geom_point(size=5, shape=10, color='blue', fill='cyan'))
    }, ignoreNULL = F)
    
    output$myplot <- renderPlot({
      myplot()
    })
    
    v<- reactiveValues(data = {data.frame(x=numeric(0), y=numeric(0)) %>% add_row(x=rep(0,100),y=rep(0,100))})

  
    
    #output$mytable <- renderDT({
    #  DT::datatable(v$data, editable = TRUE)
    #})
    
    #when there is any edit to a cell, write that edit to the initial dataframe
    #check to make sure it's positive, if not convert
    observeEvent(input$mytable_cell_edit, {
      #get values
      info = input$mytable_cell_edit
      i = as.numeric(info$row)
      j = as.numeric(info$col)
      k = as.numeric(info$value)
      
      #write values to reactive
      v$data[i,j] <- k
      
    })
    
    #render plot
    output$myplot2 <- renderPlot({
      req(input$myButtongo) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
      v$data %>%  #don't react to any changes in the data
        ggplot(aes(x,y)) +
        geom_point(size=5, shape=11, color='red')
    })

}
