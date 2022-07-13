# packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(bslib)
library(leaflet)
library(leaflet.providers)
library(highcharter)
library(shinyWidgets)
# remotes::install_github("jbkunst/highcharter")

# options -----------------------------------------------------------------
parametros <- list(
  color = "#236478",
  font_family = "IBM Plex Sans"
)

theme_obbsa <-  bs_theme(
  # bg = "white",
  # fg = "#236478",
  # primary = "black",
  # bootswatch = "yeti",
  base_font = font_google(parametros$font_family)
)

langs <- getOption("highcharter.lang")

# langs$loading <- "<i class='fas fa-circle-notch fa-spin fa-4x'></i>"
langs$loading <- "Cargando información"

options()

options(
  highcharter.lang = langs,
  highcharter.theme = hc_theme_smpl(
    color = parametros$color,
    chart = list(style = list(fontFamily = parametros$font_family))
    # colors = parametros$color
  )
)

# theme <- theme_obbsa |>
#   bs_add_rules("")


# bslib::bs_theme_preview(theme_obbsa)

# data --------------------------------------------------------------------
# dtiempo     <- readRDS("data/dummy/dtiempo.rds")
# dtiempo     <- readRDS("data/data_inia_chile.rds")
# dtiempo     <- mutate(dtiempo, tiempo = lubridate::ymd_hm(tiempo))
# dtiempo     <- rename(dtiempo, identificador = cod)
# destaciones <- readRDS("data/dummy/estaciones.rds")

# data        <- readRDS("data/data_diaria.rds")
# data        <- readRDS("data/data_diaria_202X.rds")
data        <- readRDS("data/data_diaria_2022.rds")
destaciones <- readRDS("data/estaciones.rds")
ddefvars    <- readRDS("data/definicion_variables.rds")

data        |> count(red)
destaciones |> count(red)

# inputs options ----------------------------------------------------------
# glimpse(data)
opt_variable <- data |>
  slice(0) |>
  select(temp_promedio_aire,
         temp_minima,
         temp_maxima,
         precipitacion_horaria,
         humed_rel_promedio,
         presion_atmosferica,
         radiacion_solar_max,
         veloc_max_viento
         ) |>
  names() |>
  as_tibble() |>
  mutate(desc = snakecase::to_sentence_case(value)) |>
  select(desc, value) |>
  deframe()

opt_variable

# opt_variable <- ddefvars |>
#   dplyr::filter(Name %in% opt_variable) |>
#   select(Description, Name) |>
#   deframe()

opt_variable

fun_group <- list(
  # "Horaria" = partial(lubridate::ceiling_date, unit = "hour"),
  "Diaria"  = partial(lubridate::ceiling_date, unit = "day"),
  "Semanal" = partial(lubridate::ceiling_date, unit = "week"),
  "Mensual" = partial(lubridate::ceiling_date, unit = "month"),
  "Sin agrupar" = identity
  )

opt_group <- names(fun_group)

opt_group

fun_stat <- list(
  "Promedio"  = partial(mean, na.rm = TRUE),
  "Mínimo"    = partial(min, na.rm = TRUE),
  "Máximo"    = partial(max, na.rm = TRUE),
  "Acumulado (suma)" = partial(sum, na.rm = TRUE)
)

opt_stat <- names(fun_stat)

opt_stat

opt_estaciones <- destaciones |>
  arrange(latitud) |>
  select(nombre_estacion, estacion_id) |>
  deframe()

# highcharter vacio
hc_void <- highchart() |>
  hc_add_series(data = NULL, id = "data", showInLegend = FALSE) |>
  hc_xAxis(type = "datetime") |>
  hc_credits(enabled = TRUE, text = "", href = "")



# opciones descarga de datos ----------------------------------------------
opt_estaciones_datos <- destaciones |>
  arrange(desc(latitud)) |>
  mutate(
    latitud  = round(latitud, 3),
    longitud = round(longitud, 3),
    nombre_estacion = str_glue(" {region} - {nombre_estacion} - ({latitud}, {longitud}) ")
    ) |>
  select(nombre_estacion, estacion_id) |>
  deframe()

fechas_min_max <- data |>
  summarise(
    fecha_hora_min = min(fecha_hora),
    fecha_hora_max=  max(fecha_hora)
  ) |>
  gather() |>
  mutate(value = lubridate::ceiling_date(value, unit = "month")) |>
  pull(value) |>
  as.Date()

opt_estaciones_meses <- seq.Date(fechas_min_max[1], fechas_min_max[2], by = "month") |>
  format("%Y/%m")

opt_estaciones_meses

