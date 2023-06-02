# navbarPage(
page_navbar(
  title  = tags$span("ODES Estaciones", class = "title"),
  lang = "es",
  id = "nav",
  theme = theme_odes,
  # mapa --------------------------------------------------------------------
  bslib::nav_panel(
    "Mapa",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("www/css/styles.css"),
        includeScript("www/js/gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE, draggable = FALSE,
        width = "auto", height = "auto",
        top = 56 + 10, left = 10,
        right = "auto", bottom = "auto",

        tags$br(),

        conditionalPanel(
          "input.showpanel",


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

              # actionButton("detalle_estacion", label = "Ver detalle estacion", class = "btn-sm")

            ),
          )
        ),

        prettyToggle(
          inputId = "showpanel",
          value = TRUE,
          label_on = tags$small("Esconder controles"),
          label_off = tags$small("Mostrar controles"),
          status_on = "primary",
          status_off = "info",
          icon_on = icon("caret-up"),
          icon_off = icon("caret-up", class = "fa-rotate-180")
        )

      ),

      tags$div(id="cite",
        "", tags$span("ODES, 2021-2022"), "."
      )
    )
  ),

  # salon -------------------------------------------------------------------
  bslib::nav_panel(
    "Salón",
    value = "salon",
    fluidRow(
      column(
        width = 10,
        offset = 1,

        fluidRow(
          column(
            width = 8,
            selectizeInput(
              "station_nyt",
              "Selecione estación a visualizar",
              opt_estaciones_nyt,
              multiple = FALSE,
              width = "100%"
              )
            ),
          column(
            width = 4,
            selectInput(
              inputId = "salon_yrs",
              label = "Año",
              choices = rev(opt_salon_yrs),
              selected = max(opt_salon_yrs),
              width = "100%"
              )
            )
          ),

        fluidRow(
          column(width = 12, highchartOutput("chart_nyt", width = "100%", height = 600)),
          # column(width =  2, highchartOutput("chart_chi", width = "100%", height = 700))
          )
        )
      )
  ),
  # datos -------------------------------------------------------------------
  bslib::nav_panel(
    "Datos",
    fluidRow(
      column(
        width = 10,
        offset = 1,
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
  bslib::nav_panel(
    "Configuración",
    fluidRow(
      column(
        width = 10,
        offset = 1,

        tabsetPanel(
          type = "pills",
          tabPanel(
            "Leaflet Providers",
            tags$br(),
            radioButtons(
              "leafletprov",
              label = NULL,
              inline = TRUE,
              choices = opt_opts_leafletproviders
            ),
            leafletOutput("map_demo")
          ),
          tabPanel(
            "Historia datos",
            tags$br(),

            sliderTextInput(
              inputId = "opt_yrsdata",
              label = "Rango de fechas a mostrar",
              choices = opt_opts_yrsdata,
              selected  = tail(opt_opts_yrsdata, 2),
              grid = TRUE,
              # min = min(opt_opts_yrsdata),
              # max = max(opt_opts_yrsdata),
              # value = tail(opt_opts_yrsdata, 2),
              width = "100%"
              ),
            )
        )
      )
    )
  )
)
