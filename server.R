library(shiny)
library(RODBC)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]


function(input, output, session) {

  cityInput<-reactive({
    switch(input$city,
           "福州"=c("城网","农网"),
           "厦门"=c("思明湖里","同安","翔安"))
  })
    
  output$area<-renderDataTable({cityInput()})
  
  datasetInput <- reactive({
      # Fetch the appropriate data object, depending on the value
      # of input$dataset.
    channel<-odbcConnect("DSN_DL",uid="postgres",pwd="postgres",believeNRows=F)
    predict<-sqlQuery(channel,"select * from tglist_predict limit 100")
    loess<-sqlQuery(channel,"select * from loess_num limit 100")
    extend<-sqlQuery(channel,"select * from tglist_extend limit 100")
      switch(input$dataset,
             "predict" = predict,
             "loess" = loess,
             "extend" = extend)
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
    
    
    
    ## Interactive Map ###########################################
    
    # Create the map
    # output$map <- renderLeaflet({
    #   leaflet() %>%
    #     addTiles(
    #       urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    #       attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    #     ) %>%
    #     setView(lng = 110, lat = 30.45, zoom = 4)
    # })
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = 118.3, lat = 26.4, zoom = 4) %>% 
        addWMSTiles("http://localhost:6080/arcgis/services/Baidu_China/MapServer/WMSServer",
                    layers = "Layers",
                    options = WMSTileOptions(format = "png", transparent = FALSE),
                    attribution = NULL)
    })
    
    
    # A reactive expression that returns the set of zips that are
    # in bounds right now
    zipsInBounds <- reactive({
      if (is.null(input$map_bounds))
        return(zipdata[FALSE,])
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(zipdata,
             latitude >= latRng[1] & latitude <= latRng[2] &
               longitude >= lngRng[1] & longitude <= lngRng[2])
    })
    
    # Precalculate the breaks we'll need for the two histograms
    centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
    
    output$histCentile <- renderPlot({
      # If no zipcodes are in view, don't plot
      if (nrow(zipsInBounds()) == 0)
        return(NULL)
      
      hist(zipsInBounds()$centile,
           breaks = centileBreaks,
           main = "SuperZIP score (visible zips)",
           xlab = "Percentile",
           xlim = range(allzips$centile),
           col = '#00DD00',
           border = 'white')
    })
    
    output$scatterCollegeIncome <- renderPlot({
      # If no zipcodes are in view, don't plot
      if (nrow(zipsInBounds()) == 0)
        return(NULL)
      
      print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
    })
    
    # This observer is responsible for maintaining the circles and legend,
    # according to the variables the user has chosen to map to color and size.
    observe({
      colorBy <- input$color
      sizeBy <- input$size
      
      if (colorBy == "superzip") {
        # Color and palette are treated specially in the "superzip" case, because
        # the values are categorical instead of continuous.
        colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
        pal <- colorFactor("Spectral", colorData)
      } else {
        colorData <- zipdata[[colorBy]]
        pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)
      }
      
      if (sizeBy == "superzip") {
        # Radius is treated specially in the "superzip" case.
        radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
      } else {
        radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
      }
      
      leafletProxy("map", data = zipdata) %>%
        clearShapes() %>%
        addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
                   stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    })
    
    # Show a popup at the given location
    showZipcodePopup <- function(zipcode, lat, lng) {
      selectedZip <- allzips[allzips$zipcode == zipcode,]
      content <- as.character(tagList(
        tags$h4("Score:", as.integer(selectedZip$centile)),
        tags$strong(HTML(sprintf("%s, %s %s",
                                 selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
        ))), tags$br(),
        sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
        sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
        sprintf("Adult population: %s", selectedZip$adultpop)
      ))
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
    }
    
    # When map is clicked, show a popup with city info
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      if (is.null(event))
        return()
      
      isolate({
        showZipcodePopup(event$id, event$lat, event$lng)
      })
    })
    
    
    ## Data Explorer ###########################################
    
    observe({
      cities <- if (is.null(input$states)) character(0) else {
        filter(cleantable, State %in% input$states) %>%
          `$`('City') %>%
          unique() %>%
          sort()
      }
      stillSelected <- isolate(input$cities[input$cities %in% cities])
      updateSelectInput(session, "cities", choices = cities,
                        selected = stillSelected)
    })
    
    observe({
      zipcodes <- if (is.null(input$states)) character(0) else {
        cleantable %>%
          filter(State %in% input$states,
                 is.null(input$cities) | City %in% input$cities) %>%
          `$`('Zipcode') %>%
          unique() %>%
          sort()
      }
      stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
      updateSelectInput(session, "zipcodes", choices = zipcodes,
                        selected = stillSelected)
    })
    
    observe({
      if (is.null(input$goto))
        return()
      isolate({
        map <- leafletProxy("map")
        map %>% clearPopups()
        dist <- 0.5
        zip <- input$goto$zip
        lat <- input$goto$lat
        lng <- input$goto$lng
        showZipcodePopup(zip, lat, lng)
        map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
      })
    })
    
    output$ziptable <- DT::renderDataTable({
      df <- cleantable %>%
        filter(
          Score >= input$minScore,
          Score <= input$maxScore,
          is.null(input$states) | State %in% input$states,
          is.null(input$cities) | City %in% input$cities,
          is.null(input$zipcodes) | Zipcode %in% input$zipcodes
        ) %>%
        mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
      action <- DT::dataTableAjax(session, df)
      
      DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    })
  }
  
  
