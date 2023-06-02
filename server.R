# input <- list(variable = "temp_promedio_aire",  group = "Semanal", stat = "Promedio", station = 20, opt_yrsdata = c(2021, 2022))

function(input, output, session) {

  # main --------------------------------------------------------------------
  # mapa principal
  output$map <- renderLeaflet({

    leaflet(
      options = leafletOptions(
        attributionControl = FALSE,
        zoomControl = FALSE
      )
    ) |>

      addProviderTiles(providers$CartoDB.Positron,  group = "CartoDB") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI WI") |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "ESRI WTM") |>

      addLayersControl(
        baseGroups = c("CartoDB", "ESRI WI", "ESRI WTM"),
        position   = "bottomright",
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      htmlwidgets::onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }") |>
      setView(lng =  -70.64827, lat = -33.45694, zoom = 5) |>
      leafem::addLogo(
        img = "https://odes-chile.org/img/logo.png",
        src= "remote",
        position = "bottomleft",
        offset.x = 5,
        offset.y = 5,
      ) |>
      leaflet.extras::addSearchOSM(
        options = leaflet.extras::searchOptions(
          textErr = "Ubicación no encontrada",
          textCancel = "Cancelar",
          textPlaceholder = "Buscar...",
          position = "bottomright"
        )
      ) |>
      addEasyButton(
        easyButton(
          position = "bottomright",
          icon = "fa-crosshairs",
          title = "Mi ubicación",
          onClick = JS("function(btn, map){ map.locate({setView: true}); }")
        )
      )

  })

  # mini grafico
  output$chart <- renderHighchart(hc_void)

  # grafico temperatura estracion
  # output$chart_temp <- renderHighchart(hc_void)

  # botton cambia a detalle de la estación
  observeEvent(input$detalle_estacion, {
    message("hola, borrame plz")
    nav_select(
      id = "nav",
      selected = "estacion",
      session
      )
  })

  # reactivo de informacion de variable seleccionada
  data_variable <- reactive({

    nmvar <- names(opt_variable[opt_variable == input$variable])
    nmvar <- nmvar |>
      str_split(" ") |>
      unlist() |>
      first()


    data_variable <- ddefvars |>
      filter(str_detect(Description, nmvar)) |>
      slice(1) |>
      # filter(Name == input$variable) |>
      gather(k, v) |>
      deframe()

    data_variable

  })

  # data que depende de la variable/agrupacion/stats
  # para todas las estaciones
  data_transformada <- reactive({

    cli::cli_h2("Start `data_transformada`")

    # # aca el proceso se demora por lo que debiera...
    if(input$showchart){
      highchartProxy("chart") |>
        hcpxy_loading(action = "show")
    }

    fceil <- fun_group[[input$group]]
    fstat <- fun_stat[[input$stat]]

    minyr <- min(input$opt_yrsdata)
    maxyr <- max(input$opt_yrsdata)

    data_transformada <- tbl(sql_con(), parametros$tabla_datos) |>
      filter(year(fecha_hora) <= maxyr) |>
      filter(minyr            <= year(fecha_hora)) |>
      select(
        fecha_hora,
        valor = .data[[input$variable]],
        station_id
      ) |>
      collect()

    # si no hay quea grupar, se retorna data sin procesar
    if(identical(fceil, identity)) return(data_transformada)

    data_transformada <- data_transformada |>
      # redondeamos fechas
      mutate(fecha_hora = fceil(fecha_hora)) |>
      # agrupación
      group_by(fecha_hora, station_id) |>
      # calculo, por defaul, de momento obtendremos media
      summarise(valor = fstat(valor), .groups = "drop") |>
      ungroup()

    cli::cli_h2("End `data_transformada`")

    data_transformada

  }) |>
    bindCache(input$group, input$stat, input$variable, input$opt_yrsdata)

  # filtra data_transformada por la estacion
  # para el mini chart
  data_transformada_estacion <- reactive({

    data_transformada <- data_transformada()

    data_transformada_estacion <- data_transformada |>
      filter(station_id  == input$station)

    data_transformada_estacion

  })

  # data de estacion
  # data_estacion <- reactive({
  #
  #   data_estacion <- data |>
  #     filter(estacion_id == input$station)
  #
  #   data_estacion
  #
  # })

  # filtra data_transformada por el ultimo registro
  data_markers <- reactive({

    data_transformada <- data_transformada()

    data_markers <- data_transformada |>
      group_by(station_id) |>
      filter(fecha_hora == max(fecha_hora)) |>
      ungroup() |>
      left_join(
        select(destaciones, latitud, longitud, station_id, nombre_estacion, red),
        by = "station_id"
        )

    data_markers

  })

  # observer que mira si variable es precipitacion
  observeEvent(input$variable, {

    if(input$variable == "precipitacion_horaria") {
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
    data_markers <- data_markers |>
      mutate(
        # spark = map_chr(valor, function(x){ as.character(htmltools::tagList(sparkline(sample(20), width = 100))) }),
        lbl = str_glue("{ nombre_estacion  }:<br/> {round(valor, 2)} { data_variable$Symbol }<br/><small>red: { red }</small>")
        )

    colorData <- data_markers[["valor"]]

    pal <- colorBin(data_variable$cols, colorData, 10, pretty = TRUE)

    # radius <- scales::rescale(data_markers[["valor"]], to = c(1000, 20000))

    leafletProxy("map", data = data_markers) |>
      clearShapes() |>

      # provider
      addProviderTiles(input$leafletprov) |>

      # addCircles(
      addCircleMarkers(
        ~longitud,
        ~latitud,

        stroke = TRUE,
        color = "white",
        weight = 1,

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
        layerId = ~station_id,
        fillOpacity = 0.95,
        fillColor = pal(colorData)
        ) |>
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

    print(input$map_shape_click)
    print(input$map_marker_click)

    updateSelectizeInput(
      session,
      inputId = "station",
      selected =  input$map_marker_click$id
      )

  })

  # dado el cambio de estacion:
  # 1. centrar el mapa en la ubicación de la estacion
  observeEvent(input$station, {

    # message(input$station)

    if(input$station == "") return(TRUE)

    # leaflet
    coords <- destaciones |>
      filter(station_id == input$station) |>
      select(longitud, latitud) |>
      gather() |>
      deframe() |>
      as.list()

    leafletProxy("map") |>
      flyTo(lng = coords$longitud, lat = coords$latitud, zoom = input$map_zoom)

  })

  # observa si cambia la data estacion para modificar el mini chart
  observe({

    if(!input$showchart) return(TRUE)

    data_transformada_estacion <- data_transformada_estacion()

    data_variable <- data_variable()

    datos <- data_transformada_estacion |>
      select(x = fecha_hora, y = valor) |>
      mutate(x = datetime_to_timestamp(x), y = round(y, 2)) |>
      arrange(x)

    highchartProxy("chart") |>
      hcpxy_update_series(
        id = "data",
        lineWidth = 1,
        states = list(hover = list(lineWidthPlus = 0)),
        data = list_parse2(datos),
        color = parametros$color,
        name = data_variable$Description
      ) |>
      hcpxy_update(
        # tooltip = list(),
        yAxis = list(
          labels = list(
            format = str_glue("{{value}} { symbol }", symbol = data_variable$Symbol)
            )
          ),
        credits = list(text = data_variable$Description)
      ) |>
      hcpxy_loading(action = "hide") |>
      hcpxy_update_series()

  })


  # salon -------------------------------------------------------------------
  output$chart_nyt <- renderHighchart({
    nyt_chart(input$station_nyt, input$salon_yrs)
  })

  output$chart_chi <- renderHighchart({
    hcmap("countries/cl/cl-all")
  })


  # datos -------------------------------------------------------------------
  station_data <- reactive({

    sttns <- isolate(input$station_data_stations)
    # sttns <- c(4, 10)
    dts <- isolate(input$station_data_date)
    # dts <- tail(opt_estaciones_meses, 2)

    dts <- lubridate::ymd(str_c(dts, "/01", sep = ""))

    dts_min <- lubridate::as_datetime(dts[1])
    dts_max <- lubridate::as_datetime(dts[2]) + months(1) - lubridate::seconds(1)

    tbl(sql_con(), parametros$tabla_datos) |>
      filter(station_id  %in% sttns) |>
      filter(fecha_hora <= dts_max) |>
      filter(dts_min >= dts_min) |>
      collect()
  })

  output$station_data_download <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(station_data(), file)
    }
  )


  # opciones ----------------------------------------------------------------
  # mapa demo
  output$map_demo <- renderLeaflet({

    map <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
      setView(lng =  -70.64827, lat = -33.45694, zoom = 6) |>
      addProviderTiles(input$leafletprov) |>
      htmlwidgets::onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }")

    map

  })




}
