# navbarPage(
page_navbar(
  title  = tags$span("ODES", class = "title"),
  id = "nav",
  theme = theme_odes,
  # mapa --------------------------------------------------------------------
  bslib::nav(
    "Mapa",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE, draggable = FALSE,
        width = "auto", height = "auto",
        top = 77 + 10, left = 10,
        right = "auto", bottom = "auto",

        # h6("Panel de control"),
        tags$br(),

        selectInput("variable", tags$small("Variable"), opt_variable),
        selectInput("group", tags$small("Agrupación temporal"), opt_group, selected = "Diaria"),
        selectInput("stat", tags$small("Estadístico (cálculo)"), opt_stat),

        selectizeInput(
          "station",
          tags$small("Estación"),
          opt_estaciones,
          options = list(
            placeholder = "Seleccionar estación",
            onInitialize = I('function() { this.setValue(""); }')
            )
          ),

        conditionalPanel(
          # "false",
          "input.station != ''",
          # checkboxInput("showchart", "Mostrar detalle estacion histórica"),
          checkboxInput("showchart", "Mostrar información histórica"),

          conditionalPanel(
            "input.showchart",
            # "hchart va en 2do contitaion panel",
            highchartOutput("chart", width = "100%", height = "250px"),
            actionButton("detalle_estacion", label = "Ver detalle estacion", class = "btn-sm")
          ),
        )
      ),

      tags$div(id="cite",
        "Informacion de Institución Importante ", tags$em("OBSSA, 2021-2022"), " by Equipo ODES."
      )
    )
  ),
  # detalle estacion --------------------------------------------------------
  # bslib::nav(
  #   "Estación",
  #   value = "estacion",
  #   fluidRow(
  #     column(
  #       width = 8,
  #       offset = 2,
  #       highchartOutput("chart_temp", width = "100%")
  #       )
  #     )
  # ),
  # datos -------------------------------------------------------------------
  bslib::nav(
    "Datos",
    fluidRow(
      column(
        width = 8,
        offset = 2,
        tabsetPanel(
          type = "pills",
          tabPanel(
            "Estaciones",
            tags$br(),

            selectizeInput(
              "station_data_stations",
              "Seleciones estaciones a descargar",
              opt_estaciones_datos,
              multiple = TRUE,
              width = "100%",
              options = list(
                # maxOptions = 10,
                placeholder = "Seleccionar estación(es)",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),

            conditionalPanel(
              # "false",
              "input.station_data_stations != ''",

              sliderTextInput(
                inputId = "station_data_date",
                label = "Seleccione rango de fechas a descargar",
                choices = opt_estaciones_meses,
                selected = tail(opt_estaciones_meses, 2),
                width = "100%"
              ),
              downloadButton("station_data_download", label = "Descargar", class = "btn")
              )
            ),
          tabPanel("Shapes", "shapes"),
          tabPanel("Rasters", "rasters")
          ),
        )
      )
    ),
  # opciones ----------------------------------------------------------------
  bslib::nav(
    "Opciones",
    fluidRow(
      column(
        width = 8,
        offset = 2,

        tabsetPanel(
          type = "pills",
          tabPanel(
            "Leaflet Providers",
            tags$br(),
            radioButtons(
              "leafletprov",
              label = NULL,
              inline = TRUE,
              choices = c("CartoDB.Positron", "Esri.WorldImagery", "Esri.WorldTopoMap")
            ),
            leafletOutput("map_demo")
          ),
          # tabPanel("Otra opción")
        )
      )
    )
  )
)
