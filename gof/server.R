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
library(fitdistrplus)
library(EnvStats)
library(plotly)
library(viridis)
# define goodness of fitness function
gof_fit <- function(data, dist_type, test_type){ 
  
  # dist_type can be 'norm','lnorm','pois','exp','gamma','bninom','geom','beta','unif','logis'
  # test_type: sw (Shapiro-Wilk), ad, cmv, lillie, skew, chisq, ks
  
  fit_func <- fitdist(data, dist_type)
  gof = gofTest(data,distribution = dist_type, test = test_type)
  
  plot(fit_func)
  print(gof)
}

# Define server logic required to draw a histogram
function(input, output, session) {

   # Initialize reactiveValues object to store data
  values <- reactiveValues(data = NULL)
  
  # Load CSV file into the reactive data object
  observeEvent(input$file, {
    req(input$file)
    values$data <- read.csv(input$file$datapath, header=input$header, sep=",")
    updateSelectInput(session, "column1_tabcdf", choices=names(values$data))
    updateSelectInput(session, "column1_tabplot", choices=names(values$data))
    updateSelectInput(session, "column2_tabplot", choices=names(values$data))
    updateSelectInput(session, "column3_tabplot", choices=names(values$data))
    updateSelectInput(session, "column4_tabplot", choices=names(values$data))
    })
  
  # Display the contents of the uploaded CSV file in an editable DT table
  output$table <- DT::renderDataTable({
    req(values$data)
    DT::datatable(values$data, editable = TRUE, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
  # Create a plot of the uploaded CSV data using ggplot2 and dynamically update the plot
  output$plot <- renderPlotly({
    req(values$data)
      x_col <- grep(input$column1_tabplot,colnames(values$data))
      y_col <- grep(input$column2_tabplot,colnames(values$data))
      size <- grep(input$column3_tabplot,colnames(values$data))
      fill <- grep(input$column4_tabplot,colnames(values$data))
    g1<-ggplot(values$data, aes(x=values$data[[x_col]],y=values$data[[y_col]],
                                size=values$data[[size]],
                                fill=values$data[[fill]],
                                text=paste(input$column1_tabplot,":",values$data[[x_col]],br(),input$column2_tabplot,":",values$data[[y_col]]))) + 
      geom_point(shape=21,color="black",fill="pink", stroke=0.5, alpha=0.8) +
      scale_fill_viridis(discrete=TRUE, guide=FALSE, option="B") +
      labs(x = input$column1_tabplot, y = input$column2_tabplot, title = "Data Plot")
    ggplotly(g1, tooltip="text")
  })
  
  # Create CDF composite plot
  output$plot1 <- renderPlot({
    req(values$data)
    fit_data <- grep(input$column1_tabcdf, colnames(values$data))
    fit_norm <- fitdist(values$data[[fit_data]], "norm")
    fit_logn <- fitdist(values$data[[fit_data]], "lnorm")
    #fit_gama <- fitdist(values$data[[fit_data]], "gamma")
    #fit_exp <- fitdist(values$data[[fit_data]], "exp")
    cdfcomp(list(fit_norm, fit_logn),fitlty=c(2,6),fitcol=c("blue","green"),fitlwd=c(4,4),legendtext = c("Normal","LogNormal"))
  })
  
  # Create the plot2-GoF when the data is selected
  output$plot2 <- renderPlot({
    req(values$data)
    fit_data <- grep(input$column1_tabcdf,colnames(values$data))
    fit_dist <- fitdist(values$data[[fit_data]],input$column1_tabgof)
    
    plot(fit_dist)
  })
  
  # render Print data of GOF
  output$fitdist <- renderPrint({
    req(values$data)
    fit_data <- grep(input$column1_tabcdf,colnames(values$data))
    gof = gofTest(values$data[[fit_data]],distribution = input$column1_tabgof, test = "chisq")
    print(gof)
    #tagList('Test',"B", gof_prt)
    #print(gof);
  })
  
  # Update the uploaded data when the user makes changes to the table
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    row_idx <- info$row 
    col_idx <- info$col + 1
    value <- info$value
    
  # Update the data object using reactiveValues
  values$data[row_idx, col_idx] <- value

  
    
  })
  
}
