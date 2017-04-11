library(shiny)
function(input, output) {
  #预警提交
  output$dateText2 <- renderText({
    paste("input$date2 is", as.character(input$date2))
  })
  cityInput<-reactive({
    switch(input$city,
           "福州"=c("城网","农网"),
           "厦门"=c("思明湖里","同安","翔安"))
  })
    
  output$area<-renderText({cityInput()})
    
    datasetInput <- reactive({
      # Fetch the appropriate data object, depending on the value
      # of input$dataset.
    #  library(RODBC)
     # channel<-odbcConnect("DSN_DL",uid="postgres",pwd="postgres",believeNRows=F)
    #  tglist_predict<-sqlQuery(channel,"select * from tglist_predict limit 100")
    #  loess_num<-sqlQuery(channel,"select * from loess_num limit 100")
    #  tglist_extend<-sqlQuery(channel,"select * from tglist_extend limit 100")
      switch(input$dataset,
             "predict" = rock,
             "loess" = pressure,
             "extend" = cars)
    })
    
    output$table <- renderTable({
      datasetInput()
    })
    
    # downloadHandler() takes two arguments, both functions.
    # The content function is passed a filename as an argument, and
    #   it should write out data to that filename.
    output$downloadData <- downloadHandler(
      
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        paste(input$dataset, input$filetype, sep = ".")
      },
      
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
        sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
        
        # Write to a file specified by the 'file' argument
        write.table(datasetInput(), file, sep = sep,
                    row.names = FALSE)
      }
    )
  }
  
  
