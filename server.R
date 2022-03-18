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
      hc_add_series(data = ts(1), id = "data", showInLegend = FALSE) %>%
      hc_xAxis(type = "datetime") %>%
      hc_credits(enabled = TRUE, text = "", href = "")

  })

  data_variable <- reactive({

    data_variable <- ddefvars %>%
      filter(Name == input$color) %>%
      as.list()

    data_variable

  })

  # data que depende de la variable y valor
  data_transformada <- reactive({

    data_transformada <- dtiempo %>%
      filter(var == input$color) %>%
      # ACA FALTA LA TRANSFORMACION "VALOR"
      group_by(station) %>%
      ungroup() %>%
      left_join(
        destaciones %>% select(latitud, longitud, station = nombre, identificador), by = "station"
      ) %>%
      arrange(latitud, longitud)

    data_transformada

  })

  # filtra data_transformada por la estacion
  data_transformada_estacion <- reactive({

    data_transformada <- data_transformada()

    data_transformada_estacion <- data_transformada %>%
      filter(identificador == input$station)

    data_transformada_estacion

  })

  # filtra data_transformada por el ultimo registro
  data_markers <- reactive({

    data_transformada <- data_transformada()

    data_markers <- data_transformada %>%
      group_by(identificador) %>%
      filter(tiempo == max(tiempo))

    data_markers

  })

  # observer escucha variable y valor para modificar estaciones/markers
  observe({

    # message(input$color)
    colorBy <- input$color
    # sizeBy <- input$size

    data_markers <- data_markers()

    data_markers

    colorData <- data_markers[["valor"]]
    pal <- colorBin("viridis", colorData, 7, pretty = FALSE)

    radius <- scales::rescale(data_markers[["valor"]], to = c(1000, 20000))

    data_variable <- data_variable()

    leafletProxy("map", data = data_markers) %>%
      clearShapes() %>%
      addCircles(
        ~longitud,
        ~ latitud ,
        radius = radius,
        layerId = ~ identificador,
        stroke = FALSE,
        fillOpacity = 0.6,
        fillColor = pal(colorData)
        ) %>%
      addLegend(
        # "bottomleft",
        "topright",
        pal = pal,
        values = colorData,
        title = data_variable$Symbol,
        # labFormat = labelFormat(suffix = ),
        layerId = "colorLegend"
        )
  })

  # observer que ve en que estación se hace click y cambia
  # el selector de estaciones "station"
  observeEvent(input$map_shape_click, {

    # print(input$map_shape_click)

    updateSelectizeInput(
      session,
      inputId = "station",
      selected =  input$map_shape_click$id
      )

  })

  # dado el cambio de estacion centrar el mapa en la ubicación de la estacion
  observeEvent(input$station, {

    # message(input$station)

    if(input$station == "") return(TRUE)

    # leaflet
    coords <- destaciones %>%
      filter(identificador == input$station) %>%
      select(longitud, latitud) %>%
      gather() %>%
      deframe() %>%
      as.list()

    leafletProxy("map") %>%
      flyTo(lng = coords$longitud, lat = coords$latitud, zoom = 7)

  })

  # observa si cambia la data estacion para modificar chart
  observe({

    data_transformada_estacion <- data_transformada_estacion()

    data_variable <- data_variable()

    datos <- data_transformada_estacion %>%
      select(x = tiempo, y = valor) %>%
      mutate(x = datetime_to_timestamp(x), y = round(y, 2))

    highchartProxy("chart") %>%
      hcpxy_update_series(
        id = "data",
        data = list_parse2(datos),
        name = data_variable$Description
      ) %>%
      hcpxy_update(
        # tooltip = list(),
        yAxis = list(
          labels = list(
            format = str_glue("{{value}} { symbol }", symbol = data_variable$Symbol)
            )
          ),
        credits = list(
          text = data_variable$Description
          )
      )



  })

}
