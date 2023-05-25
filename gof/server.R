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
library(readxl)
library(dataMaid)
library(DataExplorer)
library(SmartEDA)
library(dplyr)

options(shiny.reactlog = TRUE)

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

  # Sample data
  data <- data.frame(
    Name = c("John", "Jane", "Tom", "Alice"),
    Age = c(25, 30, 35, 40),
    Gender = c("Male", "Female", "Male", "Female")
  )
  
   # Initialize reactiveValues object to store data
  values <- reactiveValues(data = NULL)
  
  # Load CSV file into the reactive data object
  observeEvent(input$file, {
    req(input$file)
    if (input$filetype == 'CSV') {
        infilename = input$file$name
        print(infilename)
        values$data <- read.csv(input$file$datapath, header=input$header, sep=",")
      } else {
        values$data <- read_excel(input$file$datapath,1)
      }

    updateSelectInput(session, "column1_tabcdf", choices=names(values$data))
    updateSelectInput(session, "column1_univ", choices=names(values$data))
    updateSelectInput(session, "column2_univ", choices=names(values$data))
    updateSelectInput(session, "column1_tabplot", choices=names(values$data))
    updateSelectInput(session, "column2_tabplot", choices=names(values$data))
    updateSelectInput(session, "column3_tabplot", choices=names(values$data))
    updateSelectInput(session, "column4_tabplot", choices=names(values$data))
    updateSelectInput(session, "first_filter", choices=c("All",names(values$data)))
    updateSelectInput(session, "second_filter", choices=c("All",names(values$data)))
    #updateSelectInput(session, "first_filter", choices=c("All",unique(values$data[,2]))) # test: CWN
    })
  
  # update value filter of first column
  observeEvent(input$first_filter, {
    colidx = grep(input$first_filter,colnames(values$data))
    updateSelectInput(session, "first_filter_value", choices=c("All",unique(values$data[,colidx])))
  })
  # update value filter of second column
  observeEvent(input$second_filter, {
    colidx = grep(input$second_filter,colnames(values$data))
    updateSelectInput(session, "second_filter_value", choices=c("All",unique(values$data[,colidx])))
  })
  
  # filter data
  filtered_data <- reactive({
    filtered <- values$data
    if (input$first_filter !="All") {
      if (input$first_filter_value !="All") {
        colidx = grep(input$first_filter,colnames(values$data))
        filtered <- filtered[filtered[,colidx] == input$first_filter_value, ]
        if (input$second_filter !="All") {
          if (input$second_filter_value !="All") {
            colidx2 = grep(input$second_filter, colnames(values$data))
            filtered <- filtered[filtered[,colidx2] == input$second_filter_value,]
          }
        }
      }
    }
    filtered 
  })
  
  # render Print data summary
  output$datadescript <- renderPrint({
    #req(values$data)
    #datasummary <- summary(values$data)
    datasummary <- summary(filtered_data())
    print(datasummary)
  })
  
  # generate data report - maid
  observeEvent(input$report_maid, { 
    req(values$data)
    withProgress(message="PROCESSING...", value=0, {
      incProgress(1/2)
      tryCatch({
        makeDataReport(values$data, output='html', file="report",reportTitle="", openResult=FALSE, replace=TRUE)
        print("succesful creating report")  
      }, error = function(err) {
        output$error <- renderPrint({
          print(paste("MY_ERROR: ", err))
        })
      })
    
    })
    output$report <- renderUI({
      includeHTML("report.html")
    })
  })
  
  # generate data report - explorer
  observeEvent(input$report_explorer, {
    req(values$data)
    withProgress(message="PROCESSING...", value=0, {
      incProgress(1/2)
      tryCatch({
        create_report(values$data)
        print("succesful creating report") 
      }, error=function(err) {
        output$error <- renderPrint({
          print(paste("MY_ERROR: ", err))
        })
      })
    })
    output$report <- renderUI({
      
      includeHTML("C:/Users/tienpn/rshiny/gof/report.html")
    })
  })  

  # generate data report - smart report
  observeEvent(input$report_smart, {
    req(values$data)
    withProgress(message="PROCESSING....", value=0, {
      incProgress(3/4)
      tryCatch({
        ExpReport(values$data, op_file='report.html', label="Smart Data Report", theme=theme_update())
        print("succesful creating report")
      }, error=function(err) {
        output$error <- renderPrint({
          print(paste("MY_ERROR: ", err))
        })
      })

    })
    output$report <- renderUI({
      includeHTML("C:/Users/tienpn/rshiny/gof/report.html")
    })
  })  
  
  # Display the contents of the uploaded CSV file in an editable DT table
  output$table <- DT::renderDataTable({
    df <-DT::datatable(filtered_data(), editable = TRUE, rownames = FALSE, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 pageLength=20,info=TRUE, lengthMenu=list(c(20,-1),c("20","All"))),
                                filter=list(position='top')
                  )
  })
  
  
  # Create a plot of the uploaded CSV data using ggplot2 and dynamically update the plot
  output$plotviz1 <- renderPlotly({
  req(filtered_data())
    selected_col <- grep(input$column1_univ, colnames(values$data))
    selected_data <- filtered_data()[c(selected_col)]

    selected_data <- as.data.frame(na.omit(selected_data))

    # drop na and 0 values
    selected_data <- as.data.frame(selected_data[apply(selected_data,1, function(x) all(x!=0)),])
    g <- ggplot(selected_data, aes(x=selected_data[[1]])) + 
      geom_histogram(aes(y=..density..),bins=input$number_of_bins, color='lightblue',fill='green',alpha=0.1) +
      geom_density(data=NULL,stat='density',color="red",bw="nrd0") 
      
    ggplotly(g)
  })
  
  output$plotviz1.2 <- renderPlotly({
    req(filtered_data())
    selected_col1 <- grep(input$column1_univ, colnames(values$data))
    selected_col2 <- grep(input$column2_univ, colnames(values$data))
    selected_data <- filtered_data()[c(selected_col1,selected_col2)]
    
    selected_data <- as.data.frame(na.omit(selected_data))
    
    # drop na and 0 values
    selected_data <- as.data.frame(selected_data[apply(selected_data,1, function(x) all(x!=0)),])
    
    g <- ggplot(selected_data, aes(x=selected_data[[1]], fill=selected_data[[2]])) + 
         geom_density(data=NULL,stat='density',alpha=0.3) 
    g <- g + scale_color_brewer(palette="Accent") + theme_minimal()
    
    ggplotly(g)
  })
  
  output$plotviz2 <- renderPlotly({
    req(filtered_data())
      x_col <- grep(input$column1_tabplot,colnames(filtered_data()))
      y_col <- grep(input$column2_tabplot,colnames(filtered_data()))
      size <- grep(input$column3_tabplot,colnames(filtered_data()))
    g1<-ggplot(filtered_data(), aes(x=filtered_data()[[x_col]],y=filtered_data()[[y_col]],
                                size=filtered_data()[[size]],
                                text=paste(input$column1_tabplot,":",filtered_data()[[x_col]],input$column2_tabplot,":",filtered_data()[[y_col]]))) + 
      geom_point(shape=21,color="black",fill="pink", stroke=0.5, alpha=0.8) +
      scale_fill_viridis(discrete=FALSE, guide=FALSE, option="A") +
      labs(x = input$column1_tabplot, y = input$column2_tabplot, title = "Data Plot") +
      theme_bw()
    ggplotly(g1, tooltip="text")
  })
  
  # Create CDF composite plot
  output$plot1 <- renderPlot({
    req(filtered_data())
    tryCatch({
      selected_col <- grep(input$column1_tabcdf, colnames(values$data))
      selected_data <- filtered_data()[[selected_col]]
      # omit na
      selected_data <- as.data.frame(na.omit(selected_data))
      # drop na and 0 values
      selected_data <- selected_data[apply(selected_data,1, function(x) all(x!=0)),]

      fit_norm <- fitdist(selected_data, "norm")
      fit_logn <- fitdist(selected_data, "lnorm")
      fit_gama <- fitdist(selected_data, "gamma")
      fit_exp <- fitdist(selected_data, "exp")
      cdfcomp(list(fit_norm, fit_logn, fit_gama, fit_exp),fitlty=c(2,6,4,8),fitcol=c("blue","green","red","cyan"),fitlwd=c(4,4,4,4),legendtext = c("Normal","LogNormal","Gamma","Exp"))
    }, error=function(err){
      output$error2 <- renderPrint({
        print(paste("MY_ERROR: ", err))
      })
    })
  })
  
  # Create the plot2-GoF when the data is selected
  output$plot2 <- renderPlot({
    req(filtered_data())
    tryCatch({
      selected_col <- grep(input$column1_tabcdf,colnames(values$data))
      selected_data <- filtered_data()[[selected_col]]
      # omit na
      selected_data <- as.data.frame(na.omit(selected_data))
      # drop na and 0 values
      selected_data <- selected_data[apply(selected_data,1, function(x) all(x!=0)),]
      
      fit_dist <- fitdist(selected_data,input$column1_tabgof)
      plot(fit_dist)
      
    }, error=function(err) {
      output$error3 < renderPrint({
        print(paste("MY_ERROR: ", err))
      })
    })
  })
  
  # render Print data of GOF
  output$fitdist <- renderPrint({
    req(filtered_data())
    selected_col <- grep(input$column1_tabcdf,colnames(values$data))
    selected_data <- filtered_data()[[selected_col]]
    # omit na
    selected_data <- as.data.frame(na.omit(selected_data))
    # drop na and 0 values
    selected_data <- selected_data[apply(selected_data,1, function(x) all(x!=0)),]
    
    gof = gofTest(selected_data,distribution = input$column1_tabgof, test = input$column2_tabgof)
    print(gof)
    #tagList('Test',"B", gof_prt)
    #print(gof);
  })
  
  # render markdown
  output$markdown <- renderUI({
    tagList("URL Link: ", a("Google page", href="https://www.google.com"))
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
  
  # Update the data frame with filtering option
  
}
