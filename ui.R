# navbarPage(
page_navbar(
  title  = tags$h4("OBSSA", class = "title"),
  id = "nav",
  theme = theme_obbsa,
  bslib::nav(
  # tabPanel(
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

        h6("Panel de control"),

        selectInput("color", tags$small("Variable"), opt_variable),

        selectInput("value", tags$small("Valor"), opt_valores),

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
          checkboxInput("showchart", "Mostrar detalle estacion histórica"),
          conditionalPanel(
            "input.showchart",
            # "hchart va en 2do contitaion panel",
            highchartOutput("chart", width = "400px", height = "250px"),

          ),
        )

      ),

      tags$div(id="cite",
        'Informacion de Institución Importante ', tags$em('OBSSA, 2021-2022'), ' by Equipo OBSSA.'
      )
    )
  ),

  bslib::nav(
    "Datos"
  ),

  bslib::nav(
    "Acerca",
    "Auxiliares",
    sparkline(sample(10))
  )

)
