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
        top = 77 + 30, left = 20, right = "auto", bottom = "auto",

        h5("Panel de control"),

        selectInput("color", "Variable", vars),
        selectInput("value",   "Valor", values),

        checkboxInput("showchart", "Mostrar serie histórica"),

        conditionalPanel(
          "input.showchart",
          "Grafico (texto dummy)",
          highchartOutput("chart", width = "500px")
        ),

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
    "Acerca"
  )

)
