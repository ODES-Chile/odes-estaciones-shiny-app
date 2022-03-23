# input <- list(variable = "T",  group = "Semanal", station = "INIA-83")

function(input, output, session) {

  # mapa principal
  output$map <- renderLeaflet({

    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      # addTiles() %>%

      addProviderTiles(providers$CartoDB.Positron) %>%

      # addTiles(
      #   urlTemplate = "https://{s}.tile-cyclosm.openstreetmap.fr/cyclosm/{z}/{x}/{y}.png",
      #   attribution = '<a href="https://github.com/cyclosm/cyclosm-cartocss-style/releases" title="CyclOSM - Open Bicycle render">CyclOSM</a> | Map data: &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
      # ) %>%

      htmlwidgets::onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }") %>%
      setView(lng =  -70.64827, lat = -33.45694, zoom = 6)
  })

  # mapa demo
  output$map_demo <- renderLeaflet({

    map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      setView(lng =  -70.64827, lat = -33.45694, zoom = 6) %>%
      addProviderTiles(input$leafletprov) %>%
      htmlwidgets::onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }")

  })

  # grafico detalle
  output$chart <- renderHighchart({

    highchart() %>%
      hc_add_series(data = NULL, id = "data", showInLegend = FALSE) %>%
      hc_xAxis(type = "datetime") %>%
      hc_credits(enabled = TRUE, text = "", href = "")

  })

  # reactivo de informacion de variable seleccionada
  data_variable <- reactive({

    data_variable <- ddefvars %>%
      filter(Name == input$variable) %>%
      as.list()

    data_variable

  })

  # data que depende de la variable/agrupacion/stats
  data_transformada <- reactive({

    fceil <- fun_group[[input$group]]
    fstat <- fun_stat[[input$stat]]

    data_transformada <- dtiempo %>%
      # filtramos datos que importan
      filter(var == input$variable) %>%

      # redondeamos fechas
      mutate(tiempo = fceil(tiempo)) %>%

      # agrupación
      group_by(tiempo, station) %>%

      # calculo, por defaul, de momento obtendremos media
      summarise(valor = fstat(valor), .groups = "drop") %>%

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

  # observer que mira si variable es precipitacion
  observeEvent(input$variable, {

    if(input$variable == "P") {
      opts <- "Acumulado (suma)"
    } else {
      opts <- setdiff(opt_stat, "Acumulado (suma)")
    }

    updateSelectInput(session = session, "stat", choices = opts)

  })

  # observer escucha variable y valor para modificar estaciones/markers
  observe({

    # message(input$variable)
    # colorBy <- input$variable
    # sizeBy <- input$size

    data_markers  <- data_markers()

    data_variable <- data_variable()

    # crear label
    data_markers <- data_markers %>%
      mutate(
        # spark = map_chr(valor, function(x){ as.character(htmltools::tagList(sparkline(sample(20), width = 100))) }),
        lbl = str_glue("{ station }:<br/> {round(valor, 2)} { data_variable$Symbol }")
        )

    colorData <- data_markers[["valor"]]

    pal <- colorBin("viridis", colorData, 5, pretty = TRUE)

    # radius <- scales::rescale(data_markers[["valor"]], to = c(1000, 20000))

    leafletProxy("map", data = data_markers) %>%
      clearShapes() %>%

      # provider
      addProviderTiles(input$leafletprov) %>%

      # addCircles(
      addCircleMarkers(
        ~longitud,
        ~ latitud,
        # radius = radius,
        # https://stackoverflow.com/a/43155126/829971
        label = ~lapply(data_markers$lbl, htmltools::HTML),
        # label = ~htmltools::htmlEscape(lbl),

        labelOptions = labelOptions(
          # offset = c(-20, -20),
          style = list(
            "font-family" = parametros$font_family,
            "box-shadow" = "2px 2px rgba(0,0,0,0.15)",
            "font-size" = "15px",
            "padding" = "15px",
            "border-color" = "rgba(0,0,0,0.15)"
            )
        ),
        layerId = ~identificador,
        stroke = FALSE,
        fillOpacity = 0.8,
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
  # observeEvent(input$map_shape_click, {
  observeEvent(input$map_marker_click, {

    # print(input$map_shape_click)
    # print(input$map_marker_click)

    updateSelectizeInput(
      session,
      inputId = "station",
      selected =  input$map_marker_click$id
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
        color = parametros$color,
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
