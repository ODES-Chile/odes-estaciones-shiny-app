page_navbar(
  title  = tags$span(
    class = "title",
    tags$a(
      tags$img(src = "horizontal_SB_blanco.png", height = "30px", style = "margin-top: -5px"),
      href = "https://odes-chile.org/"
    ),
    "Estaciones"
  ),
  id = "nav",
  lang = "es",
  theme = theme_odes,
  fillable = TRUE,
  fillable_mobile = TRUE,
  # sidebar -----------------------------------------------------------------
  sidebar = sidebar(
    width = 400,
    selectInput("variable", tags$small("Variable"), opt_variable),
    selectInput("group", tags$small("Agrupación temporal"), opt_group, selected = "Diaria"),
    selectInput("stat", tags$small("Estadístico (cálculo)"), opt_stat),
    selectizeInput(
      "station",
      tags$small("Estación"),
      opt_estaciones,
      options = list(
        placeholder = "",
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),

    conditionalPanel(
      # "false",
      "input.station != ''",
      # checkboxInput("showchart", "Mostrar detalle estacion histórica"),
      checkboxInput("showchart", tags$small("Mostrar información histórica")),

      conditionalPanel(
        "input.showchart",
        # "hchart va en 2do contitaion panel",
        sliderTextInput(
          inputId = "opt_yrsdata",
          label =  tags$small("Fecha"),
          choices = opt_opts_yrsdata,
          selected  = tail(opt_opts_yrsdata, 2),
          grid = TRUE,
          # min = min(opt_opts_yrsdata),
          # max = max(opt_opts_yrsdata),
          # value = tail(opt_opts_yrsdata, 2),
          width = "100%"
        ),
        highchartOutput("chart", width = "100%", height = "250px"),

        # actionButton("detalle_estacion", label = "Ver detalle estacion", class = "btn-sm")

      ),
    )
  ),
  # mapa --------------------------------------------------------------------
  bslib::nav_panel(
    title = "Mapa",
    icon  = icon("map-location-dot"),
    tags$head(
      tags$link(href = "Isotip_gradiente_azul.png", rel = "icon"),
      tags$script(src = "https://www.googletagmanager.com/gtag/js?id=G-CYG993XQRT", async = ""),
      tags$script(src = "js/ga.js"),
      includeCSS("www/css/styles.css"),
    ),
    leafletOutput("map", width="100%", height="100%")
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
  # bslib::nav_panel(
  #   "Datos",
  #   fluidRow(
  #     column(
  #       width = 10,
  #       offset = 1,
  #       tabsetPanel(
  #         type = "pills",
  #         tabPanel(
  #           "Estaciones",
  #           tags$br(),
  #
  #           selectizeInput(
  #             "station_data_stations",
  #             "Seleciones estaciones a descargar",
  #             opt_estaciones_datos,
  #             multiple = TRUE,
  #             width = "100%",
  #             options = list(
  #               # maxOptions = 10,
  #               placeholder = "Seleccionar estación(es)",
  #               onInitialize = I('function() { this.setValue(""); }')
  #             )
  #           ),
  #
  #           conditionalPanel(
  #             # "false",
  #             "input.station_data_stations != ''",
  #
  #             sliderTextInput(
  #               inputId = "station_data_date",
  #               label = "Seleccione rango de fechas a descargar",
  #               choices = opt_estaciones_meses,
  #               selected = tail(opt_estaciones_meses, 2),
  #               width = "100%"
  #             ),
  #             downloadButton("station_data_download", label = "Descargar", class = "btn")
  #             )
  #           ),
  #         tabPanel("Shapes", "shapes"),
  #         tabPanel("Rasters", "rasters")
  #         ),
  #       )
  #     )
  #   ),
  # opciones ----------------------------------------------------------------
  # bslib::nav_panel(
  #   "Configuración",
  #   fluidRow(
  #     column(
  #       width = 10,
  #       offset = 1,
  #
  #       tabsetPanel(
  #         type = "pills",
  #         tabPanel(
  #           "Leaflet Providers",
  #           tags$br(),
  #           radioButtons(
  #             "leafletprov",
  #             label = NULL,
  #             inline = TRUE,
  #             choices = opt_opts_leafletproviders
  #           ),
  #           leafletOutput("map_demo")
  #         ),
  #         tabPanel(
  #           "Historia datos",
  #           tags$br()
  #           )
  #       )
  #     )
  #   )
  # )
)
