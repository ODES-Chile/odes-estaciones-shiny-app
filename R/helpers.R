# options highcharter -----------------------------------------------------
newlang_opts <- getOption("highcharter.lang")

newlang_opts$weekdays <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
newlang_opts$months <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
                         "agosto", "septiembre", "octubre", "noviembre", "diciembre")
newlang_opts$shortMonths <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep",
                              "oct", "nov", "dic")

newlang_opts$loading      <- "Cargando información"
newlang_opts$downloadCSV  <- "Descargar CSV"
newlang_opts$downloadJPEG <- "Descargar JPEG"
newlang_opts$downloadPDF  <- "Descargar PDF"
newlang_opts$downloadPNG  <- "Descargar PNG"
newlang_opts$downloadSVG  <- "Descargar SVG"
newlang_opts$downloadXLS  <- "Descargar XLS"
newlang_opts$printChart   <- "Imprimir gráfico"
newlang_opts$viewFullscreen <- "Ver pantalla completa"

newlang_opts$thousandsSep <- "."
newlang_opts$decimalPoint <- ","


options(
  highcharter.lang = newlang_opts,
  highcharter.theme = hc_theme_smpl(
    color = parametros$color,
    chart = list(style = list(fontFamily = parametros$font_family)),
    series = list(marker = list(symbol = "circle"))
    # colors = parametros$color
  )
)

hc_void <- highchart() |>
  hc_add_series(data = NULL, id = "data", showInLegend = FALSE) |>
  hc_xAxis(type = "datetime") |>
  hc_credits(enabled = TRUE, text = "", href = "")


# helpers -----------------------------------------------------------------
dt_2_tstmp <- highcharter::datetime_to_timestamp

sql_con <- function() {
  pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = "shiny",
    host = Sys.getenv("HOST"),
    user = "shiny",
    password = Sys.getenv("SHINY_PSQL_PWD")
  )
}

nyt_chart <- function(id = 49, year = lubridate::year(Sys.Date())){

  cli::cli_h3(str_glue("nyt_chart: station_id {id}, year {year}"))

  year <- as.numeric(year)

  d <- tbl(sql_con(), "estaciones_datos") |>
    filter(station_id == id, year(fecha_hora) == year) |>
    select(fecha_hora, temp_promedio_aire, temp_minima, temp_maxima, precipitacion_horaria) |>
    collect() |>
    arrange(fecha_hora)

  d |>
    count(fecha_hora, sort = TRUE)

  # esto es raro!!
  # al parecer queda un registro del mes anterior dando vuelta
  d <- d |>
    mutate(diff = abs(temp_maxima - temp_minima)) |>
    arrange(fecha_hora, desc(diff)) |>
    distinct(fecha_hora, .keep_all = TRUE) |>
    select(-diff)

  d <- d |>
    mutate(mes = month(fecha_hora)) |>
    group_by(mes) |>
    mutate(precipitacion_mensual_acumulada = cumsum(precipitacion_horaria)) |>
    ungroup()

  d <- d |>
    mutate(xdt = dt_2_tstmp(fecha_hora))

  nmeses <- d |> count(mes) |> nrow()

  dt <- readRDS("data/nyt_temp_historicos.rds") |>
    filter(station_id == id) |>
    mutate(across(where(is.numeric), round, 2)) |>
    mutate(xdt = dt_2_tstmp(ymd(str_c(year, m, d, sep = "/"))))

  dp <- readRDS("data/nyt_prec_historicos.rds") |>
    filter(station_id == id) |>
    mutate(across(where(is.numeric), round, 2)) |>
    mutate(xdt = dt_2_tstmp(ymd(str_c(year, m, d, sep = "/"))))

  axis <- create_axis(
    naxis = 2,
    heights = c(3, 1),
    sep = 0.05,
    turnopposite = FALSE,
    showLastLabel = FALSE,
    startOnTick = FALSE
  )

  axis[[1]]$title <- list(text = "Temperatura")
  axis[[1]]$labels <- list(format = "{value} °C")

  axis[[2]]$title <- list(text = "Precipitación")
  axis[[2]]$labels <- list(format = "{value} mm")
  axis[[2]]$min <- 0

  hc <- highchart() |>

    hc_yAxis_multiples(axis) |>

    hc_xAxis(
      type = "datetime",
      showLastLabel = FALSE,
      dateTimeLabelFormats = list(month = "%B")
      ) |>

    hc_tooltip(
      shared = TRUE,
      useHTML = TRUE,
      headerFormat = as.character(tags$small("{point.x: %b %d}", tags$br()))
    ) |>

    hc_plotOptions(
      series = list(
        marker = list(symbol = "circle"),
        pointWidth = 4
        )
      ) |>

    hc_add_series(
      data = transmute(dt, x = xdt, low = tmin_hist_smooth, high = tmax_hist_smooth),
      color = "#D3D3D388",
      # color = rgb(156, 156, 156, maxColorValue = 256),
      type = "arearange",
      name = "Temp. histórica"
    ) |>

    hc_add_series(
      data = transmute(dt, x = xdt, low = tmin_norm_smooth , high = tmax_norm_smooth),
      color = "#6C686820",
      type = "arearange",
      name = "Temperatura Normal"
    ) |>

    hc_add_series(
      data = transmute(d, x = xdt, low = temp_minima, high = temp_maxima),
      type = "columnrange",
      color = parametros$color,
      borderColor = "#6592a0",
      # color = "#6592a0",
      # boderColor = "red",
      borderWidth = 1,
      name = str_c("Temperatura ", year)
    )  |>

    hc_add_series(
      data = transmute(d, x = xdt, y = precipitacion_mensual_acumulada, m = mes),
      type = "area",
      hcaes(x, y, group = m),
      name = "Precipitación acumulada mensual",
      color = "#008ED0",
      lineWidth = 1.2,
      yAxis = 1,
      fillColor = "#EBEAE2",
      id = c("p", rep(NA, nmeses - 1)),
      linkedTo = c(NA, rep("p", nmeses - 1))
    ) |>

    hc_add_series(
      data = transmute(dp, x = xdt, y = prec_norm, m = m),
      "line",
      hcaes(x = x, y = y, group = m),
      name = "Precipitación Normal",
      color = "#008ED0",
      yAxis = 1,
      id = c("np", rep(NA, 11)),
      linkedTo = c(NA, rep("np", 11)),
      lineWidth = 1
    )

  hc

  cli::cli_h3(str_glue("nyt_chart: ready!"))

  hc

}
