# input <- list(color = "T", value = "max")

function(input, output, session) {

  # Interactive Map ---------------------------------------------------------
  # Create the map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      htmlwidgets::onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }") %>%
      setView(lng =  -70.64827, lat = -33.45694, zoom = 6)

  })

  output$chart <- renderHighchart({

    highchart() %>%
      hc_add_series(data = ts(1:10), id = "data") %>%
      hc_xAxis(type = "datetime")

  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  # zipsInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #     return(zipdata[FALSE,])
  #   bounds <- input$map_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  #
  #   subset(zipdata,
  #     latitude >= latRng[1] & latitude <= latRng[2] &
  #       longitude >= lngRng[1] & longitude <= lngRng[2])
  # })

  # Precalculate the breaks we'll need for the two histograms
  # centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #
  #   hist(zipsInBounds()$centile,
  #     breaks = centileBreaks,
  #     main = "SuperZIP score (visible zips)",
  #     xlab = "Percentile",
  #     xlim = range(allzips$centile),
  #     col = '#00DD00',
  #     border = 'white')
  # })

  # output$scatterCollegeIncome <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #
  #   print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  # })

  data_markers <- reactive({

    data_markers <- dtiempo %>%
      filter(var == input$color) %>%
      group_by(station) %>%
      filter(tiempo == max(tiempo)) %>%
      ungroup() %>%
      left_join(
        destaciones %>% select(latitud, longitud, station = nombre), by = "station"
        ) %>%
      arrange(latitud, longitud)

    data_markers

  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({

    # message(input$color)
    colorBy <- input$color
    # sizeBy <- input$size

    data_markers <- data_markers()

    data_markers

    colorData <- data_markers[["valor"]]
    pal <- colorBin("viridis", colorData, 7, pretty = FALSE)

    radius <- scales::rescale(data_markers[["valor"]], to = c(1000, 20000))

    leafletProxy("map", data = data_markers) %>%
      clearShapes() %>%
      addCircles(
        ~longitud, ~latitud , radius = radius,
        layerId = ~station,
        stroke=FALSE, fillOpacity=0.6,
        fillColor=pal(colorData)
        ) %>%
      addLegend(
        # "bottomleft",
        "topright",
        pal = pal, values = colorData, title = colorBy,
        layerId="colorLegend"
        )
  })

  observeEvent(input$map_shape_click, {
    message("hello!!")
    p <- input$map_shape_click    # typo was on this line
    print(p)
  })

  # Show a popup at the given location
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedZip <- allzips[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(selectedZip$centile)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #       selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }

  # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  #
  #   isolate({
  #     showZipcodePopup(event$id, event$lat, event$lng)
  #   })
  # })


  ## Data Explorer ###########################################
#
#   observe({
#     cities <- if (is.null(input$states)) character(0) else {
#       filter(cleantable, State %in% input$states) %>%
#         `$`('City') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$cities[input$cities %in% cities])
#     updateSelectizeInput(session, "cities", choices = cities,
#       selected = stillSelected, server = TRUE)
#   })
#
#   observe({
#     zipcodes <- if (is.null(input$states)) character(0) else {
#       cleantable %>%
#         filter(State %in% input$states,
#           is.null(input$cities) | City %in% input$cities) %>%
#         `$`('Zipcode') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
#     updateSelectizeInput(session, "zipcodes", choices = zipcodes,
#       selected = stillSelected, server = TRUE)
#   })
#
#   observe({
#     if (is.null(input$goto))
#       return()
#     isolate({
#       map <- leafletProxy("map")
#       map %>% clearPopups()
#       dist <- 0.5
#       zip <- input$goto$zip
#       lat <- input$goto$lat
#       lng <- input$goto$lng
#       showZipcodePopup(zip, lat, lng)
#       map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
#     })
#   })
#
#   output$ziptable <- DT::renderDataTable({
#     df <- cleantable %>%
#       filter(
#         Score >= input$minScore,
#         Score <= input$maxScore,
#         is.null(input$states) | State %in% input$states,
#         is.null(input$cities) | City %in% input$cities,
#         is.null(input$zipcodes) | Zipcode %in% input$zipcodes
#       ) %>%
#       mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
#     action <- DT::dataTableAjax(session, df, outputId = "ziptable")
#
#     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
#   })
}
