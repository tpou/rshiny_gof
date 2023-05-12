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

   # Initialize reactiveValues object to store data
  values <- reactiveValues(data = NULL)
  
  # Load CSV file into the reactive data object
  observeEvent(input$file, {
    req(input$file)
    values$data <- read.csv(input$file$datapath, header=input$header)
  })
  
  # Display the contents of the uploaded CSV file in an editable DT table
  output$table <- DT::renderDataTable({
    req(values$data)
    DT::datatable(values$data, editable = TRUE, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
  # Create a plot of the uploaded CSV data using ggplot2 and dynamically update the plot
  output$plot <- renderPlot({
    req(values$data)
    ggplot(values$data, aes(x = x, y = y)) + 
      geom_point() +
      labs(x = "X", y = "Y", title = "Uploaded CSV Data Plot")
  })
  
  # Update the uploaded data when the user makes changes to the table
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    row_idx <- info$row 
    col_idx <- info$col + 1
    value <- info$value
    
    # Update the data object using reactiveValues
    values$data[row_idx, col_idx] <- value

    # Update the plot when the table is edited
    output$plot <- renderPlot({
      req(values$data)
      ggplot(values$data, aes(x = x, y = y)) + 
        geom_point() +
        labs(x = "X", y = "Y", title = "Uploaded CSV Data Plot")
    })
  })
  
}
