#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)
library(tibble)

# Define server logic required to draw a histogram
function(input, output, session) {

   myplot = reactiveVal()
    mydata = reactiveVal()
    myplot2 = reactiveVal()
    
    # client-side processing

    output$head <- DT::renderDataTable({
      inFile <- input$file
      if (is.null(inFile))
        return (NULL)
      df <- read.csv(inFile$datapath, header=input$header)
    })
    
    
    observeEvent(input$myButton, {
      inFile2 <- input$file
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
    
    #v<- reactiveValues(data = {data.frame(x=numeric(0), y=numeric(0)) %>% add_row(x=rep(0,100),y=rep(0,100))})
    v<- reactive({req(input$file)
                  read.csv(input$file$datapath, header=input$header)})
    
    
    #output$mytable <- renderDT({
    #  DT::datatable(v$data, editable = TRUE)
    #})
    
    #when there is any edit to a cell, write that edit to the initial dataframe
    #check to make sure it's positive, if not convert
    #observeEvent(input$mytable_cell_edit, {
      #get values
    #  info = input$mytable_cell_edit
    #  i = as.numeric(info$row)
    #  j = as.numeric(info$col)
    #  k = as.numeric(info$value)
    #  print(i)
    #  print(j)
    #  print(k)
      #print(v())
      #write values to reactive
    #  v()[i+1,j+1]<- k
      
    #})
    
    observeEvent(input$mytable_cell_edit, {
      info <- input$mytable_cell_edit
      row_idx <- info[1] + 1  # add 1 to adjust for row header
      col_idx <- info[2] + 1  # add 1 to adjust for column header
      new_value <- as.character(input[["mytable_cell_edit"]][["value"]])
      print(row_idx)
      print(col_idx)
      print(new_value)
      viva <- as.data.frame(v())
      print(class(viva))
      viva[2,2] <- 4000
      print(viva)
      
      # render plot
      myplot2(ggplot(viva, aes(x=x, y=y)) + geom_point(size=5, shape=10, color='red', fill='cyan'))
    }, ignoreNULL = F)
    
    output$myplot2 <- renderPlot({
      myplot2()
    })


    
    #render plot
    #output$myplot2 <- renderPlot({
    #  print(viva)
    #  req(input$myButtongo) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
    #  viva %>%  #don't react to any changes in the data
    #    ggplot(aes(x,y)) +
    #    geom_point(size=5, shape=11, color='red')
    #})

}
