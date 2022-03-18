# input <- list(color = "T", value = "max", station = "INIA-83")

function(input, output, session) {

  # mapa principal
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      htmlwidgets::onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }") %>%
      setView(lng =  -70.64827, lat = -33.45694, zoom = 6)

  })

  # grafico detalle
  output$chart <- renderHighchart({

    highchart() %>%
      hc_add_series(data = ts(1:10), id = "data") %>%
      hc_xAxis(type = "datetime")

  })

  # data/valores para colorear las estaciones/circulos
  data_markers <- reactive({

    data_markers <- dtiempo %>%
      filter(var == input$color) %>%
      group_by(station) %>%
      filter(tiempo == max(tiempo)) %>%
      ungroup() %>%
      left_join(
        destaciones %>% select(latitud, longitud, station = nombre, identificador), by = "station"
        ) %>%
      arrange(latitud, longitud)

    data_markers

  })

  # observer que mira variable y valor para modificar estaciones
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
        layerId = ~identificador,
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

    # print(input$map_shape_click)

    updateSelectizeInput(
      session,
      inputId = "station",
      selected =  input$map_shape_click$id
      )

  })

  observeEvent(input$station, {

    # message(input$station)

    if(input$station == "") return(TRUE)

    coords <- destaciones %>%
      filter(identificador == input$station) %>%
      select(longitud, latitud) %>%
      gather() %>%
      deframe() %>%
      as.list()

    leafletProxy("map") %>%
      flyTo(lng = coords$longitud, lat = coords$latitud, zoom = 5)

  })
}
